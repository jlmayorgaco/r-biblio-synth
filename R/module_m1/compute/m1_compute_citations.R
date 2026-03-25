# ============================================================================
# m1_compute_citations.R - Citations metric for M1
# ============================================================================

#' Compute M1 citations
#'
#' @param input A data frame.
#' @param config A configuration list.
#' @return A list with \code{top_cited_documents}, \code{citation_summary}, \code{status}.
#' @export
compute_m1_citations <- function(input, config = biblio_config()) {
  list(
    top_cited_documents = m1_empty_rank_table(),
    citation_summary    = list(),
    status              = "stub"
  )
}
