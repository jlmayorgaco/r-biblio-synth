# ============================================================================
# m1_compute_sources.R - Sources metric for M1
# ============================================================================

#' Compute M1 sources
#'
#' @param input A data frame.
#' @param config A configuration list.
#' @return A list with \code{top_sources}, \code{source_summary}, \code{status}.
#' @export
compute_m1_sources <- function(input, config = biblio_config()) {
  list(
    top_sources    = m1_empty_rank_table(),
    source_summary = list(),
    status         = "stub"
  )
}
