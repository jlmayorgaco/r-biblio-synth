# ============================================================================
# m1_table_keyword_cooccurrence.R - Table builder for keyword co-occurrence
# ============================================================================

#' @export
build_m1_keyword_cooccurrence_table <- function(result, config = biblio_config()) {
  status <- "stub"
  table_top_keywords <- tibble::tibble()
  table_top_pairs <- tibble::tibble()
  table_metrics <- tibble::tibble()
  summary_list <- list()
  
  if (!is.null(result) && is.list(result) && result$status == "success") {
    status <- "success"
    
    if (!is.null(result$keyword_freq) && length(result$keyword_freq) > 0) {
      top_n <- min(50, length(result$keyword_freq))
      table_top_keywords <- tibble::tibble(
        rank = seq_len(top_n),
        keyword = names(result$keyword_freq)[1:top_n],
        frequency = as.numeric(result$keyword_freq)[1:top_n]
      )
    }
    
    if (!is.null(result$summary$top_pairs) && nrow(result$summary$top_pairs) > 0) {
      table_top_pairs <- tibble::as_tibble(result$summary$top_pairs)
      table_top_pairs$rank <- seq_len(nrow(table_top_pairs))
      table_top_pairs <- table_top_pairs[, c("rank", "keyword1", "keyword2", "weight")]
    }
    
    if (!is.null(result$metrics$summary) && nrow(result$metrics$summary) > 0) {
      table_metrics <- tibble::as_tibble(result$metrics$summary)
      table_metrics <- table_metrics[order(-table_metrics$degree), ]
      table_metrics$rank <- seq_len(nrow(table_metrics))
      table_metrics <- table_metrics[, c("rank", "keyword", "degree", "strength", "betweenness", "closeness", "eigenvector")]
    }
    
    summary_list <- list(
      total_keywords = result$summary$total_keywords %||% NA_integer_,
      total_pairs = result$summary$total_pairs %||% NA_integer_,
      unique_pairs = result$summary$unique_pairs %||% NA_integer_,
      avg_cooccurrence = result$summary$avg_cooccurrence %||% NA_real_,
      max_cooccurrence = result$summary$max_cooccurrence %||% NA_integer_,
      density = result$summary$density %||% NA_real_,
      n_communities = result$communities$n_communities %||% NA_integer_,
      modularity = result$communities$modularity %||% NA_real_,
      clustering_coefficient = result$metrics$clustering_coefficient %||% NA_real_,
      is_small_world = result$metrics$is_small_world %||% NA
    )
  }
  
  list(
    status = status,
    top_keywords = table_top_keywords,
    top_pairs = table_top_pairs,
    metrics = table_metrics,
    summary = summary_list
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b