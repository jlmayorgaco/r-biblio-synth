# ============================================================================
# m1_table_doc_types.R - Table builder for doc types
# ============================================================================

#' Build M1 doc types table
#'
#' @param result The compute result for doc_types.
#' @param config A configuration list.
#' @return A list with \code{status} and \code{table}.
#' @export
build_m1_doc_types_table <- function(result, config = biblio_config()) {
  list(
    status = "stub",
    table  = tibble::tibble()
  )
}
