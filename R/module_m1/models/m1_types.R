# ============================================================================
# m1_types.R - S3 constructors for M1 sub-results
# ============================================================================

#' Create a new M1 metric result
#'
#' @param metric_id Character. Metric identifier.
#' @param data List. Metric-specific data.
#' @param diagnostics List. Diagnostics.
#' @param status Character. "stub", "ok", "error".
#' @return An object of class \code{c("m1_metric_result", "list")}.
#' @export
new_m1_metric_result <- function(metric_id   = "unknown",
                                 data         = list(),
                                 diagnostics  = list(),
                                 status       = "stub") {
  structure(
    list(
      metric_id   = metric_id,
      data        = data,
      diagnostics = diagnostics,
      status      = status
    ),
    class = c("m1_metric_result", "list")
  )
}

#' @export
new_m1_overview_result <- function(...) {
  new_m1_metric_result(metric_id = "overview", ...)
}

#' @export
new_m1_doc_types_result <- function(...) {
  new_m1_metric_result(metric_id = "doc_types", ...)
}

#' @export
new_m1_authors_result <- function(...) {
  new_m1_metric_result(metric_id = "authors", ...)
}

#' @export
new_m1_citations_result <- function(...) {
  new_m1_metric_result(metric_id = "citations", ...)
}

#' @export
new_m1_countries_result <- function(...) {
  new_m1_metric_result(metric_id = "countries", ...)
}

#' @export
new_m1_sources_result <- function(...) {
  new_m1_metric_result(metric_id = "sources", ...)
}

#' @export
new_m1_keywords_result <- function(...) {
  new_m1_metric_result(metric_id = "keywords", ...)
}

#' @export
new_m1_bradford_result <- function(...) {
  new_m1_metric_result(metric_id = "bradford", ...)
}
