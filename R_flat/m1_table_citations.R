# ============================================================================
# m1_table_citations.R - Table builder for citations
# ============================================================================

#' @export
build_m1_citations_table <- function(result, config = biblio_config()) {
  list(
    status = if (inherits(result, "list") && "top_cited_documents" %in% names(result)) "success" else "stub",
    table  = if (inherits(result, "list") && "top_cited_documents" %in% names(result)) result$top_cited_documents else tibble::tibble()
  )
}
