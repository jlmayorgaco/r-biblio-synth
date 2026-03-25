# ============================================================================
# m1_table_authors.R - Table builder for authors
# ============================================================================

#' @export
build_m1_authors_table <- function(result, config = biblio_config()) {
  list(
    status = if (inherits(result, "list") && "top_authors" %in% names(result)) "success" else "stub",
    table  = if (inherits(result, "list") && "top_authors" %in% names(result)) result$top_authors else tibble::tibble()
  )
}
