# ============================================================================
# m1_compute_countries.R - Countries metric for M1
# ============================================================================

#' Compute M1 countries
#'
#' @param input A data frame.
#' @param config A configuration list.
#' @return A list with \code{top_countries}, \code{country_summary}, \code{status}.
#' @export
compute_m1_countries <- function(input, config = biblio_config()) {
  list(
    top_countries   = m1_empty_rank_table(),
    country_summary = list(),
    status          = "stub"
  )
}
