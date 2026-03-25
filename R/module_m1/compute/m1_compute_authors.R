# ============================================================================
# m1_compute_authors.R - Authors metric for M1
# ============================================================================

#' Compute M1 authors
#'
#' @param input A data frame.
#' @param config A configuration list.
#' @return A list with \code{top_authors}, \code{author_productivity}, \code{status}.
#' @export
compute_m1_authors <- function(input, config = biblio_config()) {
  list(
    top_authors          = m1_empty_rank_table(),
    author_productivity  = list(),
    status               = "stub"
  )
}
