# ============================================================================
# m1_table_countries.R - Table builder for countries
# ============================================================================

#' Build M1 countries table
#'
#' @param result The compute result for countries.
#' @param config A configuration list.
#' @return A list with \code{status} and \code{table}.
#' @export
build_m1_countries_table <- function(result, config = biblio_config()) {
  list(
    status = "stub",
    table  = tibble::tibble()
  )
}
