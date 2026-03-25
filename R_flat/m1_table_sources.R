# ============================================================================
# m1_table_sources.R - Table builder for sources
# ============================================================================

#' @export
build_m1_sources_table <- function(result, config = biblio_config()) {
  list(
    status = if (inherits(result, "list") && "top_sources" %in% names(result)) "success" else "stub",
    table  = if (inherits(result, "list") && "top_sources" %in% names(result)) result$top_sources else tibble::tibble()
  )
}
