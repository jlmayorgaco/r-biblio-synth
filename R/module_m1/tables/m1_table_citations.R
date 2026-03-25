# ============================================================================
# m1_table_citations.R - Table builder for citations
# ============================================================================

#' Build M1 citations table
#'
#' @param result The compute result for citations.
#' @param config A configuration list.
#' @return A list with \code{status} and \code{table}.
#' @export
build_m1_citations_table <- function(result, config = biblio_config()) {
  list(
    status = "stub",
    table  = tibble::tibble()
  )
}
