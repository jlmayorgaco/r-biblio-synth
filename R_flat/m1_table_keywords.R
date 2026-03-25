# ============================================================================
# m1_table_keywords.R - Table builder for keywords
# ============================================================================

#' @export
build_m1_keywords_table <- function(result, config = biblio_config()) {
  list(
    status = if (inherits(result, "list") && "top_keywords" %in% names(result)) "success" else "stub",
    table  = if (inherits(result, "list") && "top_keywords" %in% names(result)) result$top_keywords else tibble::tibble()
  )
}
