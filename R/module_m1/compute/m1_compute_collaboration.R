# ============================================================================
# m1_compute_collaboration.R - Collaboration Metrics
# ============================================================================
# Computes collaboration indices for bibliometric analysis:
# - Collaboration Index (CI): Average authors per paper
# - Collaboration Coefficient (CC): Weighted measure of multi-authorship
# - Degree of Collaboration (DC): Proportion of multi-authored papers
# - Modified Collaboration Coefficient (CC')
#
# IEEE Q1 Enhancement: Includes comprehensive collaboration analysis
# following established bibliometric methodologies.

#' Compute collaboration metrics
#'
#' @param input Bibliographic data frame
#' @param config Configuration list
#' @return List with collaboration metrics
#' @export
compute_m1_collaboration <- function(input, config = biblio_config()) {
  validate_is_data_frame(input)
  
  au_col <- "AU"
  if (!au_col %in% names(input)) au_col <- "AF"
  
  if (!au_col %in% names(input)) {
    return(list(
      metrics = list(),
      by_year = data.frame(),
      by_document_type = data.frame(),
      summary = list(),
      status = "error: no author column found"
    ))
  }
  
  authors_per_paper <- compute_authors_per_paper(input, au_col)
  
  if (length(authors_per_paper) == 0) {
    return(list(
      metrics = list(),
      by_year = data.frame(),
      by_document_type = data.frame(),
      summary = list(),
      status = "error: could not compute authors per paper"
    ))
  }
  
  metrics <- compute_collaboration_indices(authors_per_paper)
  
  by_year <- if ("PY" %in% names(input)) {
    compute_collaboration_by_year(input, au_col)
  } else {
    data.frame()
  }
  
  by_doc_type <- if ("DT" %in% names(input)) {
    compute_collaboration_by_doc_type(input, au_col)
  } else {
    data.frame()
  }
  
  summary_stats <- list(
    total_papers = length(authors_per_paper),
    single_authored = sum(authors_per_paper == 1),
    multi_authored = sum(authors_per_paper > 1),
    single_author_pct = mean(authors_per_paper == 1) * 100,
    collaboration_index = metrics$CI,
    collaboration_coefficient = metrics$CC,
    degree_of_collaboration = metrics$DC,
    mean_authors_per_paper = metrics$mean_authors,
    median_authors_per_paper = metrics$median_authors,
    max_authors = metrics$max_authors
  )
  
  list(
    metrics = metrics,
    by_year = by_year,
    by_document_type = by_doc_type,
    summary = summary_stats,
    authors_per_paper = authors_per_paper,
    status = "success"
  )
}

#' Compute authors per paper
#' @keywords internal
compute_authors_per_paper <- function(input, au_col) {
  authors_col <- input[[au_col]]
  
  sapply(authors_col, function(au_str) {
    if (is.na(au_str) || au_str == "") return(1)
    authors <- strsplit(as.character(au_str), ";")[[1]]
    authors <- trimws(authors)
    authors <- authors[authors != ""]
    max(1, length(authors))
  })
}

#' Compute collaboration indices
#' @keywords internal
compute_collaboration_indices <- function(authors_per_paper) {
  n <- length(authors_per_paper)
  if (n == 0) {
    return(list(
      CI = NA_real_,
      CC = NA_real_,
      DC = NA_real_,
      CC_modified = NA_real_,
      mean_authors = NA_real_,
      median_authors = NA_real_,
      max_authors = NA_real_
    ))
  }
  
  freq_table <- table(authors_per_paper)
  k_values <- as.integer(names(freq_table))
  f_k <- as.integer(freq_table)
  
  # Collaboration Index (CI): mean number of authors per paper
  CI <- mean(authors_per_paper, na.rm = TRUE)
  
  # Degree of Collaboration (DC): proportion of multi-authored papers
  DC <- mean(authors_per_paper > 1, na.rm = TRUE)
  
  # Collaboration Coefficient (CC): Ajiferuke's formula
  # CC = sum(f_k * (1 - 1/k)) / n
  # where f_k = number of papers with k authors
  # Handle k = 0 case (should not occur) and ensure valid calculation
  CC <- sum(f_k * (1 - 1/k_values), na.rm = TRUE) / n
  
  list(
    CI = CI,
    CC = CC,
    DC = DC,
    mean_authors = mean(authors_per_paper, na.rm = TRUE),
    median_authors = median(authors_per_paper, na.rm = TRUE),
    max_authors = max(authors_per_paper, na.rm = TRUE),
    min_authors = min(authors_per_paper, na.rm = TRUE),
    sd_authors = sd(authors_per_paper, na.rm = TRUE),
    distribution = data.frame(
      n_authors = k_values,
      n_papers = f_k,
      proportion = f_k / n
    )
  )
}

#' Compute collaboration by year
#' @keywords internal
compute_collaboration_by_year <- function(input, au_col) {
  py_col <- "PY"
  if (!py_col %in% names(input)) {
    return(data.frame())
  }
  
  authors_per_paper <- compute_authors_per_paper(input, au_col)
  years <- input[[py_col]]
  
  valid_idx <- !is.na(years) & years > 1900 & years <= as.numeric(format(Sys.Date(), "%Y"))
  
  if (sum(valid_idx) == 0) {
    return(data.frame())
  }
  
  years_valid <- years[valid_idx]
  authors_valid <- authors_per_paper[valid_idx]
  
  # Use safer aggregation approach
  year_groups <- split(authors_valid, years_valid)
  result <- do.call(rbind, lapply(names(year_groups), function(yr) {
    x <- year_groups[[yr]]
    data.frame(
      year = as.numeric(yr),
      mean_authors = mean(x, na.rm = TRUE),
      median_authors = median(x, na.rm = TRUE),
      n_papers = length(x),
      single_author = sum(x == 1),
      multi_author = sum(x > 1),
      stringsAsFactors = FALSE
    )
  }))
  
  result$collaboration_rate <- result$multi_author / result$n_papers
  
  result[order(result$year), ]
}

#' Compute collaboration by document type
#' @keywords internal
compute_collaboration_by_doc_type <- function(input, au_col) {
  dt_col <- "DT"
  if (!dt_col %in% names(input)) {
    return(data.frame())
  }
  
  authors_per_paper <- compute_authors_per_paper(input, au_col)
  doc_types <- input[[dt_col]]
  
  valid_idx <- !is.na(doc_types) & doc_types != ""
  
  if (sum(valid_idx) == 0) {
    return(data.frame())
  }
  
  types_valid <- doc_types[valid_idx]
  authors_valid <- authors_per_paper[valid_idx]
  
  # Use safer aggregation approach
  type_groups <- split(authors_valid, types_valid)
  result <- do.call(rbind, lapply(names(type_groups), function(tp) {
    x <- type_groups[[tp]]
    data.frame(
      document_type = tp,
      mean_authors = mean(x, na.rm = TRUE),
      median_authors = median(x, na.rm = TRUE),
      n_papers = length(x),
      single_author = sum(x == 1),
      multi_author = sum(x > 1),
      stringsAsFactors = FALSE
    )
  }))
  
  result$collaboration_rate <- result$multi_author / result$n_papers
  
  result[order(-result$n_papers), ]
}