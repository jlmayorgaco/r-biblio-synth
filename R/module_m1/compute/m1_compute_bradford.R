# ============================================================================
# m1_compute_bradford.R - Bradford metric for M1
# ============================================================================

#' Compute M1 Bradford analysis
#'
#' @param input A data frame.
#' @param config A configuration list.
#' @return A list with \code{bradford_summary}, \code{core_sources}, \code{status}.
#' @export
compute_m1_bradford <- function(input, config = biblio_config()) {
  list(
    bradford_summary = list(),
    core_sources     = m1_empty_rank_table(),
    status           = "stub"
  )
}
