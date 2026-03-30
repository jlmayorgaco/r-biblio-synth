# ============================================================================
# m3_compute_network_tests.R - Network Statistical Tests for M3
# ============================================================================
# Quadratic Assignment Procedure (QAP) and network comparison tests

#' Compute QAP test for network correlation
#'
#' Tests whether two networks are significantly correlated, accounting for
#' network structure. Used for testing collaboration-citation relationships.
#'
#' @param matrix1 First adjacency matrix
#' @param matrix2 Second adjacency matrix
#' @param n_permutations Number of permutations (default 1000)
#' @param method Correlation method: "pearson", "spearman"
#' @return List with QAP test results
#' @export
m3_compute_qap_test <- function(matrix1, matrix2, n_permutations = 1000, method = "pearson") {
  if (!is.matrix(matrix1) || !is.matrix(matrix2)) {
    return(list(status = "error: inputs must be matrices"))
  }
  
  if (nrow(matrix1) != nrow(matrix2) || ncol(matrix1) != ncol(matrix2)) {
    return(list(status = "error: matrices must have same dimensions"))
  }
  
  n <- nrow(matrix1)
  
  if (n < 5) {
    return(list(status = "error: insufficient nodes for QAP test (need >= 5)"))
  }
  
  # Observed correlation
  obs_cor <- compute_matrix_correlation(matrix1, matrix2, method)
  
  # Permutation distribution
  perm_correlations <- numeric(n_permutations)
  
  for (i in seq_len(n_permutations)) {
    # Perute rows and columns (maintains network structure)
    perm_order <- sample(n)
    perm_matrix1 <- matrix1[perm_order, perm_order]
    perm_correlations[i] <- compute_matrix_correlation(perm_matrix1, matrix2, method)
  }
  
  # Calculate p-values
  p_one_tailed <- mean(perm_correlations >= obs_cor)
  p_two_tailed <- 2 * min(mean(perm_correlations >= obs_cor), mean(perm_correlations <= obs_cor))
  p_two_tailed <- min(p_two_tailed, 1)  # Cap at 1
  
  # Effect size (correlation)
  effect_size <- abs(obs_cor)
  
  # Interpretation
  interpretation <- interpret_qap_result(obs_cor, p_two_tailed)
  
  list(
    observed_correlation = obs_cor,
    p_value_one_tailed = p_one_tailed,
    p_value_two_tailed = p_two_tailed,
    significant = p_two_tailed < 0.05,
    effect_size = effect_size,
    n_permutations = n_permutations,
    perm_mean = mean(perm_correlations),
    perm_sd = sd(perm_correlations),
    perm_distribution = perm_correlations,
    interpretation = interpretation,
    status = "success"
  )
}

#' Compute matrix correlation (lower triangle only)
#' @keywords internal
compute_matrix_correlation <- function(m1, m2, method = "pearson") {
  # Get lower triangle indices
  lower_idx <- lower.tri(m1)
  
  # Extract values
  v1 <- m1[lower_idx]
  v2 <- m2[lower_idx]
  
  # Remove diagonal
  if (method == "pearson") {
    cor(v1, v2, use = "complete.obs", method = "pearson")
  } else if (method == "spearman") {
    cor(v1, v2, use = "complete.obs", method = "spearman")
  } else {
    cor(v1, v2, use = "complete.obs")
  }
}

#' Interpret QAP test result
#' @keywords internal
interpret_qap_result <- function(correlation, p_value) {
  if (p_value < 0.001) {
    sig_text <- "highly significant"
  } else if (p_value < 0.01) {
    sig_text <- "very significant"
  } else if (p_value < 0.05) {
    sig_text <- "significant"
  } else {
    sig_text <- "not significant"
  }
  
  if (correlation > 0.5) {
    strength_text <- "strong positive"
  } else if (correlation > 0.3) {
    strength_text <- "moderate positive"
  } else if (correlation > 0.1) {
    strength_text <- "weak positive"
  } else if (correlation > -0.1) {
    strength_text <- "negligible"
  } else if (correlation > -0.3) {
    strength_text <- "weak negative"
  } else if (correlation > -0.5) {
    strength_text <- "moderate negative"
  } else {
    strength_text <- "strong negative"
  }
  
  sprintf("The networks show a %s correlation (r = %.3f), which is %s (p = %.4f).",
          strength_text, correlation, sig_text, p_value)
}

#' Compute MRQAP test (Multiple Regression QAP)
#'
#' Tests multiple network predictors simultaneously.
#'
#' @param dependent Dependent adjacency matrix
#' @param predictors List of predictor adjacency matrices
#' @param n_permutations Number of permutations
#' @return List with MRQAP results
#' @export
m3_compute_mrqap <- function(dependent, predictors, n_permutations = 1000) {
  if (!is.matrix(dependent)) {
    return(list(status = "error: dependent must be a matrix"))
  }
  
  if (!is.list(predictors)) {
    return(list(status = "error: predictors must be a list of matrices"))
  }
  
  n <- nrow(dependent)
  p <- length(predictors)
  
  if (n < 5) {
    return(list(status = "error: insufficient nodes (need >= 5)"))
  }
  
  # Flatten matrices to vectors (lower triangle only)
  dep_vec <- dependent[lower.tri(dependent)]
  
  pred_matrix <- matrix(NA, nrow = length(dep_vec), ncol = p)
  predictor_names <- names(predictors)
  if (is.null(predictor_names)) {
    predictor_names <- paste0("Predictor", seq_len(p))
  }
  
  for (i in seq_len(p)) {
    pred_matrix[, i] <- predictors[[i]][lower.tri(predictors[[i]])]
  }
  
  # Fit OLS regression
  data_df <- data.frame(y = dep_vec, pred_matrix)
  colnames(data_df) <- c("y", predictor_names)
  
  formula_str <- paste("y ~", paste(predictor_names, collapse = " + "))
  
  ols_model <- tryCatch({
    lm(as.formula(formula_str), data = data_df)
  }, error = function(e) NULL)
  
  if (is.null(ols_model)) {
    return(list(status = "error: regression failed"))
  }
  
  # Observed coefficients
  obs_coefs <- coef(ols_model)
  
  # Permutation distribution for each coefficient
  perm_coefs <- matrix(NA, nrow = n_permutations, ncol = length(obs_coefs))
  colnames(perm_coefs) <- names(obs_coefs)
  
  for (i in seq_len(n_permutations)) {
    perm_order <- sample(n)
    perm_dep <- dependent[perm_order, perm_order]
    dep_vec_perm <- perm_dep[lower.tri(perm_dep)]
    
    data_df_perm <- data.frame(y = dep_vec_perm, pred_matrix)
    colnames(data_df_perm) <- c("y", predictor_names)
    
    ols_perm <- tryCatch({
      lm(as.formula(formula_str), data = data_df_perm)
    }, error = function(e) NULL)
    
    if (!is.null(ols_perm)) {
      perm_coefs[i, ] <- coef(ols_perm)
    }
  }
  
  # Calculate p-values for each coefficient
  p_values <- numeric(length(obs_coefs))
  for (j in seq_along(obs_coefs)) {
    p_values[j] <- 2 * min(
      mean(perm_coefs[, j] >= obs_coefs[j], na.rm = TRUE),
      mean(perm_coefs[, j] <= obs_coefs[j], na.rm = TRUE)
    )
  }
  
  # R-squared
  r_squared <- summary(ols_model)$r.squared
  
  list(
    coefficients = obs_coefs,
    standard_errors = apply(perm_coefs, 2, sd, na.rm = TRUE),
    p_values = p_values,
    r_squared = r_squared,
    n_permutations = n_permutations,
    significant = p_values < 0.05,
    model = ols_model,
    status = "success"
  )
}

#' Compute network comparison test
#'
#' Tests whether two networks differ significantly in structure.
#'
#' @param matrix1 First adjacency matrix
#' @param matrix2 Second adjacency matrix
#' @param n_permutations Number of permutations
#' @return List with comparison results
#' @export
m3_compute_network_comparison <- function(matrix1, matrix2, n_permutations = 1000) {
  if (!is.matrix(matrix1) || !is.matrix(matrix2)) {
    return(list(status = "error: inputs must be matrices"))
  }
  
  # Network statistics to compare
  stats1 <- compute_network_statistics(matrix1)
  stats2 <- compute_network_statistics(matrix2)
  
  # Observed differences
  obs_diff <- stats1 - stats2
  
  # Permutation test
  n1 <- nrow(matrix1)
  n2 <- nrow(matrix2)
  
  perm_diffs <- matrix(NA, n_permutations, length(stats1))
  names(perm_diffs) <- names(stats1)
  
  for (i in seq_len(n_permutations)) {
    # Permute both networks
    perm1 <- matrix1[sample(n1), sample(n1)]
    perm2 <- matrix2[sample(n2), sample(n2)]
    
    stats1_perm <- compute_network_statistics(perm1)
    stats2_perm <- compute_network_statistics(perm2)
    
    perm_diffs[i, ] <- stats1_perm - stats2_perm
  }
  
  # Calculate p-values
  p_values <- numeric(length(stats1))
  for (j in seq_along(stats1)) {
    p_values[j] <- 2 * min(
      mean(perm_diffs[, j] >= obs_diff[j], na.rm = TRUE),
      mean(perm_diffs[, j] <= obs_diff[j], na.rm = TRUE)
    )
  }
  
  results <- data.frame(
    statistic = names(stats1),
    network1 = unname(stats1),
    network2 = unname(stats2),
    difference = unname(obs_diff),
    p_value = p_values,
    significant = p_values < 0.05,
    stringsAsFactors = FALSE
  )
  
  list(
    comparison = results,
    n_permutations = n_permutations,
    status = "success"
  )
}

#' Compute network statistics
#' @keywords internal
compute_network_statistics <- function(matrix) {
  n <- nrow(matrix)
  
  # Density
  density <- sum(matrix[lower.tri(matrix)] > 0) / (n * (n - 1) / 2)
  
  # Average degree
  avg_degree <- mean(rowSums(matrix > 0))
  
  # Clustering coefficient (simplified)
  clustering <- compute_clustering_coefficient(matrix)
  
  # Transitivity
  transitivity <- compute_transitivity(matrix)
  
  c(
    density = density,
    avg_degree = avg_degree,
    clustering = clustering,
    transitivity = transitivity
  )
}

#' Compute clustering coefficient
#' @keywords internal
compute_clustering_coefficient <- function(matrix) {
  n <- nrow(matrix)
  if (n < 3) return(NA)
  
  binary_matrix <- (matrix > 0) * 1
  
  triangles <- 0
  triples <- 0
  
  for (i in 1:(n - 2)) {
    for (j in (i + 1):(n - 1)) {
      for (k in (j + 1):n) {
        triple_count <- binary_matrix[i, j] + binary_matrix[i, k] + binary_matrix[j, k]
        if (triple_count >= 2) {
          triples <- triples + 1
          if (triple_count == 3) {
            triangles <- triangles + 1
          }
        }
      }
    }
  }
  
  if (triples > 0) {
    triangles / triples
  } else {
    NA
  }
}

#' Compute transitivity (global clustering)
#' @keywords internal
compute_transitivity <- function(matrix) {
  n <- nrow(matrix)
  binary_matrix <- (matrix > 0) * 1
  
  # Count closed triplets
  closed_triplets <- 0
  total_triplets <- 0
  
  for (i in 1:n) {
    neighbors <- which(binary_matrix[i, ] > 0)
    k <- length(neighbors)
    
    if (k >= 2) {
      # Potential triplets through node i
      total_triplets <- total_triplets + k * (k - 1) / 2
      
      # Count closed triplets
      for (j in neighbors) {
        for (l in neighbors) {
          if (j < l && binary_matrix[j, l] > 0) {
            closed_triplets <- closed_triplets + 1
          }
        }
      }
    }
  }
  
  if (total_triplets > 0) {
    closed_triplets / total_triplets
  } else {
    NA
  }
}

#' Build QAP test table
#' @export
build_m3_qap_table <- function(qap_result, config = biblio_config()) {
  if (is.null(qap_result) || qap_result$status != "success") {
    return(data.frame(
      test = character(),
      correlation = numeric(),
      p_value = numeric(),
      significant = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  data.frame(
    test = "QAP Correlation Test",
    correlation = round(qap_result$observed_correlation, 4),
    p_value = round(qap_result$p_value_two_tailed, 4),
    significant = ifelse(qap_result$significant, "Yes", "No"),
    interpretation = qap_result$interpretation,
    stringsAsFactors = FALSE
  )
}