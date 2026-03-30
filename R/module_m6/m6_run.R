# ============================================================================
# m6_run.R - M6 Topic Evolution Analysis
# ============================================================================

#' Run M6 module (Topic Evolution Analysis)
#'
#' @param input Bibliographic data frame with keywords
#' @param config Configuration list
#' @param export Logical. If TRUE, exports artifacts to disk.
#' @return Module result object
#' @export
run_m6 <- function(input, config = biblio_config(), export = TRUE) {
  config <- merge_biblio_config(config)
  
  log_message("INFO", "Starting M6: Topic Evolution Analysis")
  
  # Validate input
  kw_col <- if ("DE" %in% names(input)) "DE" else if ("ID" %in% names(input)) "ID" else NULL
  if (is.null(kw_col)) {
    return(create_error_result("m6", "Topic Evolution", "Missing keyword columns (DE or ID)"))
  }
  
  # Dynamic topic modeling
  log_message("INFO", "Computing dynamic topic model...")
  dtm <- m6_dynamic_topic_model(input, kw_col, config)
  
  if (dtm$status != "success") {
    return(create_error_result("m6", "Topic Evolution", dtm$status))
  }
  
  # Detect trends
  trends <- m6_detect_topic_trends(dtm, config)
  
  # Build result
  data <- list(
    dtm = dtm,
    trends = trends,
    evolution = m6_track_topic_evolution(dtm, config),
    emerging = m6_identify_emerging_topics(dtm, trends, config)
  )
  
  result <- new_module_result(
    module_id = "m6",
    module_name = "Topic Evolution Analysis",
    status = "success",
    inputs = list(n_records = nrow(input)),
    data = data,
    diagnostics = list()
  )
  
  # Render
  result <- m6_render_all(result, data, config)
  
  # Export
  if (export) {
    exported <- export_m6(result, config)
    manifest <- build_m6_manifest(result, exported, config)
    result <- attach_manifest_to_result(result, manifest)
  }
  
  log_message("INFO", "M6: Topic Evolution Analysis completed")
  result
}

#' Dynamic Topic Modeling
#' @export
m6_dynamic_topic_model <- function(input, kw_col, config = biblio_config()) {
  # Extract keywords by year
  yearly_keywords <- input %>%
    dplyr::filter(!is.na(.data[[kw_col]]), !is.na(PY)) %>%
    dplyr::group_by(PY) %>%
    dplyr::summarise(
      keywords = paste(.data[[kw_col]], collapse = "; "),
      n_docs = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(PY)
  
  if (nrow(yearly_keywords) < 3) {
    return(list(status = "error: insufficient time periods (need 3+ years)"))
  }
  
  # For each year, compute top keywords
  yearly_top_kw <- lapply(seq_len(nrow(yearly_keywords)), function(i) {
    kw_list <- strsplit(yearly_keywords$keywords[i], ";")[[1]]
    kw_list <- trimws(kw_list)
    kw_list <- tolower(kw_list)
    kw_list <- kw_list[nchar(kw_list) > 2]
    
    kw_freq <- sort(table(kw_list), decreasing = TRUE)
    head(kw_freq, 50)  # Top 50 per year
  })
  
  names(yearly_top_kw) <- yearly_keywords$PY
  
  list(
    status = "success",
    yearly_keywords = yearly_top_kw,
    years = yearly_keywords$PY,
    n_years = length(yearly_top_kw)
  )
}

#' Detect topic trends
#' @export
m6_detect_topic_trends <- function(dtm, config = biblio_config()) {
  # Get all unique keywords across all years
  all_keywords <- unique(unlist(lapply(dtm$yearly_keywords, names)))
  
  # Track each keyword over time
  keyword_trends <- lapply(all_keywords, function(kw) {
    freqs <- sapply(dtm$yearly_keywords, function(yearly) {
      ifelse(kw %in% names(yearly), yearly[kw], 0)
    })
    
    # Mann-Kendall trend test (simplified)
    years <- as.numeric(names(freqs))
    if (length(unique(freqs)) > 1) {
      trend <- ifelse(freqs[length(freqs)] > freqs[1], "increasing", "decreasing")
      strength <- abs(freqs[length(freqs)] - freqs[1]) / max(freqs, 1)
    } else {
      trend <- "stable"
      strength <- 0
    }
    
    list(
      keyword = kw,
      frequencies = freqs,
      trend = trend,
      strength = strength,
      total_freq = sum(freqs)
    )
  })
  
  names(keyword_trends) <- all_keywords
  
  # Categorize
  emerging <- names(which(sapply(keyword_trends, function(x) 
    x$trend == "increasing" && x$strength > 0.5)))
  
  declining <- names(which(sapply(keyword_trends, function(x) 
    x$trend == "decreasing" && x$strength > 0.5)))
  
  stable <- names(which(sapply(keyword_trends, function(x) 
    x$trend == "stable")))
  
  list(
    status = "success",
    trends = keyword_trends,
    emerging = emerging,
    declining = declining,
    stable = stable,
    n_emerging = length(emerging),
    n_declining = length(declining)
  )
}

#' Track topic evolution
#' @export
m6_track_topic_evolution <- function(dtm, config = biblio_config()) {
  # Compute keyword persistence across years
  years <- dtm$years
  n_years <- length(years)
  
  evolution <- lapply(seq_along(years)[-1], function(i) {
    prev_year <- years[i-1]
    curr_year <- years[i]
    
    prev_kw <- names(dtm$yearly_keywords[[i-1]])
    curr_kw <- names(dtm$yearly_keywords[[i]])
    
    list(
      from_year = prev_year,
      to_year = curr_year,
      retained = length(intersect(prev_kw, curr_kw)),
      new = length(setdiff(curr_kw, prev_kw)),
      dropped = length(setdiff(prev_kw, curr_kw)),
      retention_rate = length(intersect(prev_kw, curr_kw)) / length(prev_kw)
    )
  })
  
  list(
    status = "success",
    transitions = evolution,
    avg_retention = mean(sapply(evolution, function(x) x$retention_rate), na.rm = TRUE)
  )
}

#' Identify emerging topics
#' @export
m6_identify_emerging_topics <- function(dtm, trends, config = biblio_config()) {
  # Get top emerging keywords from last few years
  recent_years <- tail(dtm$years, 3)
  recent_keywords <- unique(unlist(lapply(recent_years, function(y) {
    names(dtm$yearly_keywords[[as.character(y)]])
  })))
  
  # Filter to those marked as emerging
  emerging_recent <- intersect(trends$emerging, recent_keywords)
  
  # Get trend details
  emerging_details <- lapply(emerging_recent, function(kw) {
    tr <- trends$trends[[kw]]
    list(
      keyword = kw,
      first_appearance = min(which(tr$frequencies > 0)),
      total_frequency = tr$total_freq,
      growth_rate = tr$strength
    )
  })
  
  # Sort by growth rate
  emerging_details <- emerging_details[order(-sapply(emerging_details, function(x) x$growth_rate))]
  
  list(
    status = "success",
    emerging_topics = head(emerging_details, 20),
    n_emerging = length(emerging_details)
  )
}