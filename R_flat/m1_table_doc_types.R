# ============================================================================
# m1_table_doc_types.R - Table builder for doc types
# ============================================================================

#' @export
build_m1_doc_types_table <- function(result, config = biblio_config()) {
  list(
    status = if (inherits(result, "list") && "doc_type_table" %in% names(result)) "success" else "stub",
    table  = if (inherits(result, "list") && "doc_type_table" %in% names(result)) result$doc_type_table else tibble::tibble()
  )
}
