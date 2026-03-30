# ============================================================================
# m3_table_collaboration_indices.R - Table builder for collaboration indices
# ============================================================================

#' @export
m3_table_collaboration_indices <- function(result, config = biblio_config()) {
  status <- "stub"
  indices_table <- tibble::tibble()
  bilateral_table <- tibble::tibble()
  summary_list <- list()
  
  if (!is.null(result) && is.list(result) && result$status == "success") {
    status <- "success"
    
    if (!is.null(result$indices) && nrow(result$indices) > 0) {
      indices_table <- tibble::as_tibble(result$indices)
      indices_table$rank <- seq_len(nrow(indices_table))
      indices_table <- indices_table[, c("rank", "country", "n_documents", 
                                          "avg_salton_index", "max_salton_index", "top_salton_partner",
                                          "avg_jaccard_index", "max_jaccard_index", "top_jaccard_partner",
                                          "avg_affinity_index", "max_affinity_index", "collaboration_centrality")]
    }
    
    if (!is.null(result$bilateral) && nrow(result$bilateral) > 0) {
      bilateral_table <- tibble::as_tibble(result$bilateral)
      bilateral_table$rank <- seq_len(nrow(bilateral_table))
      bilateral_table <- bilateral_table[, c("rank", "country_1", "country_2", 
                                              "salton", "jaccard", 
                                              "affinity_1to2", "affinity_2to1", "avg_affinity")]
    }
    
    summary_list <- list(
      total_countries = result$summary$total_countries %||% NA_integer_,
      total_collaborations = result$summary$total_collaborations %||% NA_integer_,
      mean_salton = result$summary$mean_salton %||% NA_real_,
      mean_jaccard = result$summary$mean_jaccard %||% NA_real_,
      mean_affinity = result$summary$mean_affinity %||% NA_real_,
      collaboration_density = result$summary$collaboration_density %||% NA_real_
    )
  }
  
  list(
    status = status,
    indices = indices_table,
    bilateral = bilateral_table,
    summary = summary_list
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b