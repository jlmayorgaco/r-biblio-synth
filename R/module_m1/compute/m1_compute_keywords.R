# ============================================================================
# m1_compute_keywords.R - Keywords metric for M1
# ============================================================================

#' Compute M1 keywords
#'
#' @param input A data frame.
#' @param config A configuration list.
#' @return A list with \code{keywords_summary}, \code{top_keywords}, \code{status}.
#' @export
compute_m1_keywords <- function(input, config = biblio_config()) {
  list(
    keywords_summary = list(),
    top_keywords     = m1_empty_rank_table(),
    status           = "stub"
  )
}
