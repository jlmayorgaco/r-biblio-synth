# ============================================================================
# m1_compute_overview.R - Overview metric for M1
# ============================================================================

#' Compute M1 overview
#'
#' Returns summary indicators for the bibliographic data.
#'
#' @param input A data frame.
#' @param config A configuration list.
#' @return A list with \code{summary_table}, \code{main_indicators}, \code{status}.
#' @export
compute_m1_overview <- function(input, config = biblio_config()) {
  list(
    summary_table    = m1_empty_summary_table(),
    main_indicators  = list(),
    status           = "stub"
  )
}
