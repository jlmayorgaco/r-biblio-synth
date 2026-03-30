# ============================================================================
# m1_table_collaboration.R - Table builder for Collaboration Index
# ============================================================================

#' @export
build_m1_collaboration_table <- function(result, config = biblio_config()) {
  status <- "stub"
  collab_table <- tibble::tibble()
  summary_list <- list()
  
  if (!is.null(result) && is.list(result) && result$status == "success") {
    status <- "success"
    
    summary_list <- list(
      collaboration_index = result$summary$collaboration_index %||% NA_real_,
      collaboration_coefficient = result$summary$collaboration_coefficient %||% NA_real_,
      documents_collaboration_rate = result$summary$degree_of_collaboration %||% NA_real_,
      mean_authors_per_paper = result$summary$mean_authors_per_paper %||% NA_real_,
      median_authors_per_paper = result$summary$median_authors_per_paper %||% NA_real_
    )
    
    if (!is.null(result$by_year) && nrow(result$by_year) > 0) {
      collab_table <- tibble::as_tibble(result$by_year)
    }
  }
  
  list(
    status = status,
    table = collab_table,
    summary = summary_list
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b