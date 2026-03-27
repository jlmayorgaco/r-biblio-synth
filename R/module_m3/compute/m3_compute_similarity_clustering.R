# ============================================================================
# m3_compute_similarity_clustering.R - Country similarity and clustering for M3
# ============================================================================
# NOTE: This computes distance matrix and hierarchical clustering.
# PCA/k-means clustering is handled separately in m3_compute_country_profiles.R

#' Compute country similarity matrix and hierarchical clustering
#'
#' Builds a feature matrix from country metrics, computes distance matrix,
#' and performs hierarchical clustering.
#' All results are marked exploratory and require careful interpretation.
#'
#' @param prepared_data Output from \code{prepare_m3_country_data}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list containing similarity matrix, distance matrix, and clustering results
#' @export
m3_compute_similarity_clustering <- function(prepared_data, config = biblio_config()) {
  country_summary <- prepared_data$country_summary
  
  empty_result <- list(
    feature_matrix = matrix(nrow = 0, ncol = 0),
    distance_matrix = matrix(nrow = 0, ncol = 0),
    hierarchical_clustering = NULL,
    cluster_assignments = tibble::tibble(),
    similarity_summary = list(
      n_countries = 0,
      n_features = 0,
      clustering_performed = FALSE,
      method = "none",
      notes = character()
    ),
    status = "success: no country data",
    exploratory = TRUE
  )
  
  if (nrow(country_summary) == 0) {
    empty_result$status <- "error: no country data"
    return(empty_result)
  }
  
  notes <- character()
  
  # Ensure we have the necessary columns
  required_cols <- c("article_count", "total_citations")
  if (!all(required_cols %in% names(country_summary))) {
    notes <- c(notes, paste("Missing required columns:", 
                            paste(setdiff(required_cols, names(country_summary)), collapse = ", ")))
    empty_result$similarity_summary$notes <- notes
    return(empty_result)
  }
  
  # Build feature matrix
  # Features: article_count, total_citations, average_citations, scp, mcp
  # We'll compute SCP/MCP from country_doc_level if available
  
  country_doc_level <- prepared_data$country_doc_level
  
  # Compute SCP/MCP per country if we have country_doc_level
  if (nrow(country_doc_level) > 0) {
    doc_country_counts <- country_doc_level %>%
      dplyr::group_by(doc_id) %>%
      dplyr::summarise(
        n_countries = dplyr::n_distinct(country),
        .groups = "drop"
      )
    
    scp_documents <- doc_country_counts %>%
      dplyr::filter(n_countries == 1) %>%
      dplyr::inner_join(country_doc_level, by = "doc_id") %>%
      dplyr::group_by(country) %>%
      dplyr::summarise(scp = dplyr::n(), .groups = "drop")
    
    mcp_documents <- doc_country_counts %>%
      dplyr::filter(n_countries > 1) %>%
      dplyr::inner_join(country_doc_level, by = "doc_id") %>%
      dplyr::group_by(country) %>%
      dplyr::summarise(mcp = dplyr::n(), .groups = "drop")
    
    country_summary <- country_summary %>%
      dplyr::left_join(scp_documents, by = "country") %>%
      dplyr::left_join(mcp_documents, by = "country") %>%
      dplyr::mutate(
        scp = dplyr::coalesce(scp, 0),
        mcp = dplyr::coalesce(mcp, 0)
      )
  } else {
    country_summary <- country_summary %>%
      dplyr::mutate(scp = 0, mcp = 0)
  }
  
  # Compute average_citations if not present
  if (!"average_citations" %in% names(country_summary)) {
    country_summary <- country_summary %>%
      dplyr::mutate(
        average_citations = ifelse(article_count > 0, 
                                   total_citations / article_count, 0)
      )
  }
  
  # Create standardized feature matrix
  feature_cols <- c("article_count", "total_citations", "average_citations", "scp", "mcp")
  
  # Filter to countries with complete data
  country_features <- country_summary %>%
    dplyr::select(country, dplyr::all_of(feature_cols)) %>%
    dplyr::filter(complete.cases(.))
  
  n_countries <- nrow(country_features)
  if (n_countries < 2) {
    notes <- c(notes, "Similarity clustering requires at least 2 countries with complete data.")
    empty_result$similarity_summary$notes <- notes
    return(empty_result)
  }
  
  # Standardize features (z-score)
  feature_matrix <- as.matrix(country_features[, -1])
  
  # Handle zero variance columns
  feature_sds <- apply(feature_matrix, 2, sd, na.rm = TRUE)
  zero_var_cols <- which(feature_sds == 0 | is.na(feature_sds))
  
  if (length(zero_var_cols) > 0) {
    notes <- c(notes, paste("Removed", length(zero_var_cols), 
                           "constant feature(s) before clustering."))
    feature_matrix <- feature_matrix[, -zero_var_cols, drop = FALSE]
  }
  
  if (ncol(feature_matrix) < 2) {
    notes <- c(notes, "Need at least 2 features with variance for meaningful clustering.")
    empty_result$similarity_summary$notes <- notes
    return(empty_result)
  }
  
  # Standardize
  feature_means <- colMeans(feature_matrix)
  feature_sds <- apply(feature_matrix, 2, sd)
  feature_matrix_std <- scale(feature_matrix, center = feature_means, scale = feature_sds)
  
  # Replace NaN with 0 (happens when sd = 0)
  feature_matrix_std[is.na(feature_matrix_std)] <- 0
  
  # Compute distance matrix (Euclidean)
  distance_matrix <- tryCatch({
    dist(feature_matrix_std, method = "euclidean")
  }, error = function(e) {
    notes <<- c(notes, paste("Distance computation failed:", e$message))
    NULL
  })
  
  if (is.null(distance_matrix)) {
    empty_result$similarity_summary$notes <- notes
    return(empty_result)
  }
  
  # Hierarchical clustering (complete linkage)
  hc <- tryCatch({
    hclust(distance_matrix, method = "complete")
  }, error = function(e) {
    notes <<- c(notes, paste("Hierarchical clustering failed:", e$message))
    NULL
  })
  
  clustering_performed <- !is.null(hc)
  
  # Convert distance matrix to similarity matrix (for visualization)
  # Similarity = 1 / (1 + distance)
  sim_matrix <- tryCatch({
    as.matrix(1 / (1 + as.matrix(distance_matrix)))
  }, error = function(e) {
    notes <<- c(notes, paste("Similarity matrix computation failed:", e$message))
    matrix(nrow = 0, ncol = 0)
  })
  
  # Assign cluster labels if clustering was performed
  cluster_assignments <- tibble::tibble()
  if (clustering_performed && n_countries >= 3) {
    # Cut tree at k = min(3, n-1) clusters, but at least 2
    k_clusters <- min(3, n_countries - 1)
    k_clusters <- max(2, k_clusters)
    
    cluster_labels <- tryCatch({
      cutree(hc, k = k_clusters)
    }, error = function(e) {
      notes <<- c(notes, paste("Cluster assignment failed:", e$message))
      NULL
    })
    
    if (!is.null(cluster_labels)) {
      cluster_assignments <- tibble::tibble(
        country = country_features$country,
        cluster = factor(cluster_labels)
      )
    }
  }
  
  # Summary
  similarity_summary <- list(
    n_countries = n_countries,
    n_features = ncol(feature_matrix_std),
    clustering_performed = clustering_performed,
    method = if (clustering_performed) "hierarchical (complete linkage)" else "none",
    notes = notes
  )
  
  list(
    feature_matrix = feature_matrix_std,
    distance_matrix = distance_matrix,
    similarity_matrix = sim_matrix,
    hierarchical_clustering = hc,
    cluster_assignments = cluster_assignments,
    country_labels = country_features$country,
    similarity_summary = similarity_summary,
    status = "success",
    exploratory = TRUE
  )
}