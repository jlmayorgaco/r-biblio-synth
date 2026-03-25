# ============================================================================
# m1_table_overview.R - Table builder for overview
# ============================================================================

#' Build M1 overview table
#'
#' @param result The compute result for overview.
#' @param config A configuration list.
#' @return A list with \code{status} and \code{table}.
#' @export
build_m1_overview_table <- function(result, config = biblio_config()) {
  list(
    status = "stub",
    table  = tibble::tibble()
  )
}
