# ============================================================================
# m1_table_overview.R - Table builder for overview
# ============================================================================

#' @export
build_m1_overview_table <- function(result, config = biblio_config()) {
  list(
    status = if (inherits(result, "list") && "summary_table" %in% names(result)) "success" else "stub",
    table  = if (inherits(result, "list") && "summary_table" %in% names(result)) result$summary_table else tibble::tibble()
  )
}
