# ============================================================================
# m1_table_author_indices.R - Table builder for Author Impact Indices
# ============================================================================

#' @export
build_m1_author_indices_table <- function(result, config = biblio_config()) {
  status <- "stub"
  indices_table <- tibble::tibble()
  summary_list <- list()
  
  if (!is.null(result) && is.list(result) && result$status == "success") {
    status <- "success"
    
    if (!is.null(result$author_indices) && nrow(result$author_indices) > 0) {
      indices_table <- tibble::as_tibble(result$author_indices)
      indices_table$rank <- seq_len(nrow(indices_table))
      
      cols <- c("rank", "author")
      if ("h_index" %in% names(indices_table)) cols <- c(cols, "h_index")
      if ("g_index" %in% names(indices_table)) cols <- c(cols, "g_index")
      if ("m_index" %in% names(indices_table)) cols <- c(cols, "m_index")
      if ("i10_index" %in% names(indices_table)) cols <- c(cols, "i10_index")
      if ("total_citations" %in% names(indices_table)) cols <- c(cols, "total_citations")
      if ("n_papers" %in% names(indices_table)) cols <- c(cols, "n_papers")
      
      indices_table <- indices_table[, cols[cols %in% names(indices_table)]]
    }
    
    summary_list <- list(
      n_authors_analyzed = nrow(result$author_indices) %||% 0,
      mean_h_index = if (!is.null(result$author_indices$h_index)) mean(result$author_indices$h_index, na.rm = TRUE) else NA_real_,
      median_h_index = if (!is.null(result$author_indices$h_index)) median(result$author_indices$h_index, na.rm = TRUE) else NA_real_,
      max_h_index = if (!is.null(result$author_indices$h_index)) max(result$author_indices$h_index, na.rm = TRUE) else NA_real_,
      mean_g_index = if (!is.null(result$author_indices$g_index)) mean(result$author_indices$g_index, na.rm = TRUE) else NA_real_
    )
  }
  
  list(
    status = status,
    table = indices_table,
    summary = summary_list
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b