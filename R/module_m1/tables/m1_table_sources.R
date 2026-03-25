# ============================================================================
# m1_table_sources.R - Table builder for sources
# ============================================================================

#' Build M1 sources table
#'
#' @param result The compute result for sources.
#' @param config A configuration list.
#' @return A list with \code{status} and \code{table}.
#' @export
build_m1_sources_table <- function(result, config = biblio_config()) {
  list(
    status = "stub",
    table  = tibble::tibble()
  )
}
