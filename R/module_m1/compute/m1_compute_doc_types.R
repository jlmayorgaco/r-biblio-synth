# ============================================================================
# m1_compute_doc_types.R - Document types metric for M1
# ============================================================================

#' Compute M1 document types
#'
#' @param input A data frame.
#' @param config A configuration list.
#' @return A list with \code{doc_type_table}, \code{doc_type_counts}, \code{status}.
#' @export
compute_m1_doc_types <- function(input, config = biblio_config()) {
  list(
    doc_type_table  = m1_empty_rank_table(),
    doc_type_counts = list(),
    status          = "stub"
  )
}
