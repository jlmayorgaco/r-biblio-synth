# ============================================================================
# m3_compute_country_profiles.R - Country profile modeling for M3
# ============================================================================

#' Compute country profiles using multivariate data science techniques
#'
#' @param prepared_data Output from \code{prepare_m3_country_data}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list containing country profile metrics
#' @export
m3_compute_country_profiles <- function(prepared_data, config = biblio_config()) {
  # We need to build a feature matrix for each country.
  # We'll start with the country summary and then add features from other computations if available.
  # However, to keep this function self-contained, we'll compute the base features from the prepared data.
  # We can also use the results from other compute functions if we want to avoid recomputation, but for now we'll compute from prepared_data.
  
  country_summary <- prepared_data$country_summary
  
  if (nrow(country_summary) == 0) {
    return(list(
      country_features = tibble::tibble(),
      country_profiles = tibble::tibble(),
      pca_info = list(),
      clustering_info = list(),
      profile_summary = list(
        n_countries = 0,
        n_features = 0,
        clustering_performed = FALSE
      ),
      status = "error: no country data"
    ))
  }
  
  # Ensure we have the necessary columns
  if (!"article_count" %in% names(country_summary)) {
    country_summary <- country_summary %>%
      dplyr::mutate(article_count = 0)
  }
  if (!"total_citations" %in% names(country_summary)) {
    country_summary <- country_summary %>%
      dplyr::mutate(total_citations = 0)
  }
  if (!"average_citations" %in% names(country_summary)) {
    # We can compute it if we have article_count and total_citations
    country_summary <- country_summary %>%
      dplyr::mutate(average_citations = ifelse(article_count > 0, total_citations / article_count, 0))
  }
  
  # We also want SCP and MCP if available. However, the prepared_data does not have SCP/MCP by country.
  # We have two options:
  #   1. Compute SCP/MCP in this function (by calling the SCP/MCP computation or reusing the logic).
  #   2. Change the preparation function to include SCP/MCP by country.
  #
  # Since we are building the module, we can change the preparation function to include SCP/MCP by country.
  # But note: we have already written the preparation function and we are in the middle of the task.
  #
  # Alternatively, we can compute SCP/MCP here by reusing the logic from m3_compute_scp_mcp, but that would require the country-doc level data.
  # We have the country-doc level data in prepared_data.
  #
  # Let's compute SCP and MCP by country from the country-doc level data (which we have in prepared_data$country_doc_level).
  # We'll do a similar computation as in m3_compute_scp_mcp but just to get the SCP and MCP counts per country.
  #
  # We'll compute:
  #   scp: number of documents where the country is the only country
  #   mcp: number of documents where the country is one of multiple countries
  #
  # We'll use the country_doc_level data.
  
  country_doc_level <- prepared_data$country_doc_level
  
  if (nrow(country_doc_level) > 0) {
    # Compute the number of countries per document
    doc_country_counts <- country_doc_level %>%
      dplyr::group_by(doc_id) %>%
      dplyr::summarise(
        n_countries = dplyr::n_distinct(country),
        .groups = "drop"
      )
    
    # SCP documents: n_countries == 1
    scp_documents <- doc_country_counts %>%
      dplyr::filter(n_countries == 1) %>%
      dplyr::inner_join(country_doc_level, by = "doc_id") %>%
      dplyr::group_by(country) %>%
      dplyr::summarise(
        scp = dplyr::n(),
        .groups = "drop"
      )
    
    # MCP documents: n_countries > 1
    mcp_documents <- doc_country_counts %>%
      dplyr::filter(n_countries > 1) %>%
      dplyr::inner_join(country_doc_level, by = "doc_id") %>%
      dplyr::group_by(country) %>%
      dplyr::summarise(
        mcp = dplyr::n(),
        .groups = "drop"
      )
    
    # Now we want to join these with the country_summary
    country_summary <- country_summary %>%
      dplyr::left_join(scp_documents, by = "country") %>%
      dplyr::left_join(mcp_documents, by = "country") %>%
      dplyr::mutate(
        scp = dplyr::coalesce(scp, 0),
        mcp = dplyr::coalesce(mcp, 0)
      )
  } else {
    # If we don't have country-doc level data, set SCP and MCP to 0
    country_summary <- country_summary %>%
      dplyr::mutate(
        scp = 0,
        mcp = 0
      )
  }
  
  # Compute MCP ratio
  country_summary <- country_summary %>%
    dplyr::mutate(
      mcp_ratio = ifelse((scp + mcp) > 0, round(mcp / (scp + mcp) * 100, 1), NA_real_)
    )
  
  # Now we have the base features: article_count, total_citations, average_citations, scp, mcp, mcp_ratio
  # We can also add growth features if we have annual data and if the user wants to include them.
  # However, the task says we should implement growth dynamics where data supports it.
  # We can optionally include growth features (like CAGR) if annual data is available.
  # We'll check if we have annual data in prepared_data.
  
  # We'll prepare a feature matrix for the countries.
  # We'll select the features we want to include in the profiling.
  # We'll include:
  #   - article_count (productivity)
  #   - total_citations (impact)
  #   - average_citations (impact efficiency)
  #   - scp (collaboration: solo)
  #   - mcp (collaboration: multiple)
  #   - mcp_ratio (collaboration: proportion of multiple)
  #
  # We'll also consider adding growth features if annual data is available and if the number of years is sufficient.
  # But note: the task says we should not add fake complexity. We'll only add growth features if we have annual data and if we are computing growth dynamics.
  # However, to keep the profile stable, we might not include growth features in the profiling by default.
  # We'll leave it as an option in the config.
  
  # For now, we'll stick to the base features.
  # We'll create a feature tibble with these variables.
  
  country_features <- country_summary %>%
    dplyr::select(
      country,
      article_count,
      total_citations,
      average_citations,
      scp,
      mcp,
      mcp_ratio
    )
  
  # Now we want to standardize the features (z-score) for multivariate analysis.
  # We'll exclude the country column.
  feature_columns <- c("article_count", "total_citations", "average_citations", "scp", "mcp", "mcp_ratio")
  
  # Check if we have any non-zero variance in the features (if all zeros, we cannot standardize)
  # We'll compute the mean and sd for each feature, and if sd is 0, we set the standardized value to 0 (or NA?).
  # We'll set to 0 if sd is 0 (meaning all values are the same).
  
  # We'll create a standardized version of the features.
  country_features_z <- country_features %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(feature_columns),
        .fns = list(
          ~ {
            if (sd(.x, na.rm = TRUE) == 0) {
              return(0)
            } else {
              return((.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE))
            }
          },
          .names = "{.col}_z"
        )
      )
    )
  
  # Now we have the standardized features. We can optionally perform PCA and clustering.
  # We'll do PCA if we have at least 2 countries and 2 features.
  # We'll do clustering (k-means) if we have at least 2 countries and we want to get profiles.
  # However, note that the task says we should choose the simplest defensible method.
  # We'll do:
  #   - PCA for dimensionality reduction and to get a sense of the data structure.
  #   - Hierarchical clustering or k-means to get discrete profiles, but we will only do this if the number of countries is sufficient (e.g., > 5).
  #
  # We'll record in the diagnostics if we skip clustering due to insufficient data.
  
  # Prepare the PCA and clustering results.
  pca_info <- list()
  clustering_info <- list()
  
  # We'll only attempt PCA and clustering if we have at least 2 countries and 2 features with non-zero variance.
  # We'll check the number of countries and the number of features that have variance > 0.
  
  # Extract the standardized feature matrix (without country)
  feature_matrix <- country_features_z %>%
    dplyr::select(dplyr::all_of(paste0(feature_columns, "_z"))) %>%
    as.matrix()
  
  # Check if we have enough data for PCA and clustering
  n_countries <- nrow(country_features)
  n_features <- ncol(feature_matrix)
  
  # We'll consider that we need at least 2 countries and 2 features to do PCA.
  # For clustering, we'll need at least 2 countries and we'll set a minimum of 5 countries to attempt clustering (to avoid too many clusters).
  
  if (n_countries >= 2 && n_features >= 2) {
    # Perform PCA
    pca <- tryCatch({
      prcomp(feature_matrix, center = FALSE, scale. = FALSE) # Already centered and scaled
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(pca)) {
      pca_info <- list(
        sdev = pca$sdev,
        rotation = pca$rotation,
        center = pca$center,
        scale = pca$scale,
        n_components = ncol(pca$rotation)
      )
      
      # We can also add the PCA scores (coordinates) to the country features if we want.
      # We'll add the first two principal components as new columns.
      pca_scores <- predict(pca, newdata = feature_matrix)
      country_features_z <- country_features_z %>%
        dplyr::mutate(
          PC1 = pca_scores[, 1],
          PC2 = if (ncol(pca_scores) >= 2) pca_scores[, 2] else NA_real_
        )
    }
  }
  
  # Clustering: we'll do k-means if we have at least 5 countries and we have PCA scores (or the standardized features).
  # We'll choose the number of clusters using the elbow method or a fixed small number (e.g., 3) for simplicity.
  # But note: we should not invent fancy ML if the data size is tiny.
  # We'll do:
  #   - If n_countries >= 5, we'll attempt k-means with k = min(3, floor(n_countries/2)) but at least 2.
  #   - We'll set k to 2 if n_countries is between 2 and 4, but we'll note that clustering with 2 countries is trivial.
  #   - We'll only do clustering if n_countries >= 2 and we have at least 2 features.
  #   - We'll use the standardized features (or the PCA scores if we want to reduce dimension) for clustering.
  #
  # We'll use the first two principal components if PCA was successful and we have at least 2 PCs, otherwise we use the standardized features.
  #
  # We'll record the clustering only if we perform it and if we consider it meaningful.
  #
  # We'll set a flag for whether clustering was performed.
  
  clustering_performed <- FALSE
  k <- NA_integer_
  cluster_assignments <- NA_integer_
  
  if (n_countries >= 2 && n_features >= 2) {
    # Determine the data to use for clustering: if we have PCA and at least 2 PCs, use the first 2 PCs.
    # Otherwise, use the standardized features.
    if (!is.null(pca_info) && !is.null(pca_info$n_components) && pca_info$n_components >= 2) {
      # Use the first two principal components
      clustering_data <- country_features_z %>%
        dplyr::select(PC1, PC2) %>%
        as.matrix()
    } else {
      # Use the standardized features
      clustering_data <- feature_matrix
    }
    
    # We'll attempt k-means with k from 1 to min(5, n_countries-1) and choose the k that minimizes the within-cluster sum of squares?
    # But we want to keep it simple. We'll fix k to 2 if n_countries >= 2, but we'll note that we are doing this for demonstration.
    # Alternatively, we can use the elbow method, but that is more complex.
    #
    # We'll do: if n_countries >= 5, we'll try k=3 and k=4 and choose the one with the better silhouette score? 
    # But we don't want to add too much complexity.
    #
    # We'll keep it simple: we'll set k = 2 if n_countries >= 2, and we'll note that this is for getting two profiles (high and low).
    # However, we must be cautious: with two clusters, we are splitting the data into two groups.
    #
    # We'll do:
    #   k <- 2
    #   But we will only perform clustering if n_countries >= 5? Actually, we can do it for any n_countries >= 2, but we'll record a warning if n_countries is small.
    #
    # We'll set k = 2 and perform k-means.
    #
    # We'll also compute the within-cluster sum of squares to see if the clustering is meaningful.
    #
    # We'll do:
    k <- 2
    set.seed(123) # for reproducibility
    kmeans_result <- tryCatch({
      kmeans(clustering_data, centers = k, nstart = 10)
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(kmeans_result)) {
      clustering_performed <- TRUE
      cluster_assignments <- kmeans_result$cluster
      # We'll add the cluster assignments to the country features
      country_features_z <- country_features_z %>%
        dplyr::mutate(cluster = as.factor(cluster_assignments))
      
      clustering_info <- list(
        k = k,
        within_ss = kmeans_result$tot.withinss,
        size = kmeans_result$size,
        centers = kmeans_result$centers
      )
    }
  }
  
  # Now we have the country features with standardized values and optionally PCA scores and cluster assignments.
  # We want to return the country profiles. We'll define the country profiles as the country features with the cluster assignments (if performed) and the PCA scores (if available).
  #
  # We'll return:
  #   - country_features: the original features (with country)
  #   - country_profiles: the features with standardized values, PCA scores, and cluster assignments (if available)
  #   - pca_info: the PCA results
  #   - clustering_info: the clustering results
  #   - profile_summary: a summary of the profiling step
  #
  # We'll also note in the profile summary if clustering was performed and if PCA was performed.
  
  country_profiles <- country_features_z
  
  profile_summary <- list(
    n_countries = n_countries,
    n_features = n_features,
    pca_performed = !is.null(pca_info),
    clustering_performed = clustering_performed,
    k_for_clustering = ifelse(clustering_performed, k, NA_integer_)
  )
  
  return(list(
    country_features = country_features,
    country_profiles = country_profiles,
    pca_info = pca_info,
    clustering_info = clustering_info,
    profile_summary = profile_summary,
    status = "success"
  ))
}