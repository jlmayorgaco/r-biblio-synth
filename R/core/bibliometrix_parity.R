# ============================================================================
# bibliometrix_parity.R - Fail-soft parity checks against bibliometrix
# ============================================================================

#' Compare key RBiblioSynth outputs against bibliometrix
#'
#' @param bib_data Canonical bibliometrix-compatible data frame.
#' @param module_results Optional module result list.
#' @param config Configuration list.
#' @return List with status, summary, divergence table, and raw notes.
#' @export
run_bibliometrix_parity <- function(bib_data,
                                    module_results = list(),
                                    config = biblio_config()) {
  config <- merge_biblio_config(config)

  if (!is.data.frame(bib_data) || nrow(bib_data) == 0) {
    return(bibliometrix_parity_empty("skipped", "No bibliographic data frame was supplied."))
  }
  if (!requireNamespace("bibliometrix", quietly = TRUE)) {
    return(bibliometrix_parity_empty("skipped", "Package 'bibliometrix' is not installed."))
  }

  tryCatch({
    bm <- suppressWarnings(suppressMessages(bibliometrix::biblioAnalysis(bib_data, sep = ";")))
    bm_summary <- suppressWarnings(suppressMessages(bibliometrix::summary(object = bm, k = 50, pause = FALSE)))
    references <- biblio_reference_metrics(bib_data)
    candidates <- biblio_extract_bibliometrix_metrics(bm_summary)
    divergence <- biblio_compare_metric_sets(references, candidates)
    summary <- data.frame(
      status = if (any(divergence$severity == "major", na.rm = TRUE)) "warning" else "success",
      metrics_checked = nrow(divergence),
      major_divergences = sum(divergence$severity == "major", na.rm = TRUE),
      minor_divergences = sum(divergence$severity == "minor", na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    list(
      status = summary$status[1],
      summary = summary,
      divergences = divergence,
      bibliometrix_available = TRUE,
      notes = if (summary$status[1] == "warning") "Major parity divergences require inspection before submission." else "No major parity divergence detected in checked metrics."
    )
  }, error = function(e) {
    bibliometrix_parity_empty("warning", paste("bibliometrix parity failed:", e$message))
  })
}

bibliometrix_parity_empty <- function(status, reason) {
  list(
    status = status,
    reason = reason,
    summary = data.frame(
      status = status,
      metrics_checked = 0L,
      major_divergences = 0L,
      minor_divergences = 0L,
      stringsAsFactors = FALSE
    ),
    divergences = data.frame(
      metric = character(),
      rbibliosynth_value = numeric(),
      bibliometrix_value = numeric(),
      absolute_difference = numeric(),
      relative_difference = numeric(),
      severity = character(),
      notes = character(),
      stringsAsFactors = FALSE
    ),
    bibliometrix_available = FALSE,
    notes = reason
  )
}

biblio_reference_metrics <- function(bib_data) {
  year <- if ("PY" %in% names(bib_data)) suppressWarnings(as.numeric(bib_data$PY)) else rep(NA_real_, nrow(bib_data))
  au <- if ("AU" %in% names(bib_data)) as.character(bib_data$AU) else rep("", nrow(bib_data))
  so <- if ("SO" %in% names(bib_data)) as.character(bib_data$SO) else rep("", nrow(bib_data))
  tc <- if ("TC" %in% names(bib_data)) suppressWarnings(as.numeric(bib_data$TC)) else rep(NA_real_, nrow(bib_data))

  authors <- unlist(strsplit(paste(au, collapse = ";"), ";", fixed = TRUE), use.names = FALSE)
  authors <- trimws(authors)
  authors <- authors[nzchar(authors)]
  sources <- trimws(so)
  sources <- sources[nzchar(sources)]

  list(
    documents = nrow(bib_data),
    timespan_start = suppressWarnings(min(year, na.rm = TRUE)),
    timespan_end = suppressWarnings(max(year, na.rm = TRUE)),
    annual_peak = if (any(is.finite(year))) max(tabulate(match(year[is.finite(year)], sort(unique(year[is.finite(year)])))), na.rm = TRUE) else NA_real_,
    authors = length(unique(authors)),
    top_author_count = if (length(authors) > 0) max(table(authors), na.rm = TRUE) else NA_real_,
    sources = length(unique(sources)),
    top_source_count = if (length(sources) > 0) max(table(sources), na.rm = TRUE) else NA_real_,
    total_citations = sum(tc, na.rm = TRUE)
  )
}

biblio_extract_bibliometrix_metrics <- function(bm_summary) {
  flat <- biblio_flatten_named_values(bm_summary)
  value_for <- function(patterns) {
    idx <- Reduce(`|`, lapply(patterns, function(p) grepl(p, names(flat), ignore.case = TRUE)))
    vals <- suppressWarnings(as.numeric(flat[idx]))
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0) NA_real_ else vals[1]
  }

  list(
    documents = value_for(c("documents", "articles")),
    timespan_start = value_for(c("timespan.*start", "timespan.*from", "year.*start")),
    timespan_end = value_for(c("timespan.*end", "timespan.*to", "year.*end")),
    annual_peak = value_for(c("annual.*peak", "max.*annual", "articles.*year")),
    authors = value_for(c("^authors$", "authors.*number", "authors.*total")),
    top_author_count = value_for(c("most.*productive.*author", "top.*author")),
    sources = value_for(c("sources", "journals")),
    top_source_count = value_for(c("most.*relevant.*source", "top.*source")),
    total_citations = value_for(c("total.*citations", "tc"))
  )
}

biblio_flatten_named_values <- function(x, prefix = "") {
  out <- numeric()
  if (is.null(x)) return(out)
  if (is.data.frame(x)) {
    for (nm in names(x)) {
      vals <- suppressWarnings(as.numeric(x[[nm]]))
      vals <- vals[is.finite(vals)]
      if (length(vals) > 0) {
        out[paste(prefix, nm, "first", sep = ".")] <- vals[1]
        out[paste(prefix, nm, "max", sep = ".")] <- max(vals, na.rm = TRUE)
      }
    }
    return(out)
  }
  if (is.atomic(x)) {
    vals <- suppressWarnings(as.numeric(x))
    vals <- vals[is.finite(vals)]
    if (length(vals) > 0) {
      out[prefix] <- vals[1]
    }
    return(out)
  }
  if (is.list(x)) {
    nms <- names(x)
    if (is.null(nms)) nms <- paste0("item", seq_along(x))
    for (i in seq_along(x)) {
      out <- c(out, biblio_flatten_named_values(x[[i]], paste(prefix, nms[i], sep = ".")))
    }
  }
  out
}

biblio_compare_metric_sets <- function(reference, candidate) {
  metrics <- intersect(names(reference), names(candidate))
  rows <- lapply(metrics, function(metric) {
    a <- suppressWarnings(as.numeric(reference[[metric]]))
    b <- suppressWarnings(as.numeric(candidate[[metric]]))
    diff <- if (is.finite(a) && is.finite(b)) abs(a - b) else NA_real_
    rel <- if (is.finite(diff) && is.finite(a) && abs(a) > 0) diff / abs(a) else NA_real_
    severity <- dplyr::case_when(
      !is.finite(a) | !is.finite(b) ~ "not_comparable",
      rel <= 0.01 | diff <= 1 ~ "none",
      rel <= 0.05 ~ "minor",
      TRUE ~ "major"
    )
    data.frame(
      metric = metric,
      rbibliosynth_value = a,
      bibliometrix_value = b,
      absolute_difference = diff,
      relative_difference = rel,
      severity = severity,
      notes = if (severity == "major") "Inspect harmonization, counting mode, and parsing differences." else "Within parity tolerance or not comparable.",
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}
