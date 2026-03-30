# ============================================================================
# test_new_analyses.R - Test script for new analyses
# ============================================================================
# Run this script to verify all new analyses are working correctly.
# Usage: source("R/test_new_analyses.R")
# ============================================================================

cat("\n")
cat("===========================================================\n")
cat("  RBiblioSynth - New Analyses Test\n")
cat("===========================================================\n\n")

# Bootstrap
cat("Loading bootstrap...\n")
source("R/core/bootstrap.R")

# Helper function to print test results
test_pass <- function(name) cat("[PASS]", name, "\n")
test_fail <- function(name, msg) cat("[FAIL]", name, "-", msg, "\n")

# ---------------------------------------------------
# Test 1: M1 Keyword Co-occurrence
# ---------------------------------------------------
cat("\n--- Testing M1 Keyword Co-occurrence ---\n")

test_kw_data <- data.frame(
  KW = c(
    "machine learning; neural networks; deep learning",
    "machine learning; classification; supervised learning",
    "deep learning; neural networks; CNN",
    "classification; decision trees; random forest",
    "neural networks; RNN; LSTM"
  ),
  stringsAsFactors = FALSE
)

tryCatch({
  kw_result <- compute_m1_keyword_cooccurrence(test_kw_data)
  if (kw_result$status == "success" && length(kw_result$keyword_freq) > 0) {
    test_pass("M1 Keyword Co-occurrence computation")
    cat("   Keywords found:", length(kw_result$keyword_freq), "\n")
    cat("   Top pairs:", nrow(kw_result$summary$top_pairs), "\n")
    if (!is.null(kw_result$metrics$summary)) {
      cat("   Network metrics computed:", nrow(kw_result$metrics$summary), "\n")
    }
  } else {
    test_fail("M1 Keyword Co-occurrence computation", kw_result$status)
  }
}, error = function(e) {
  test_fail("M1 Keyword Co-occurrence computation", e$message)
})

# Test rendering
tryCatch({
  if (exists("kw_result") && kw_result$status == "success") {
    render_result <- render_m1_keyword_cooccurrence(kw_result)
    if (render_result$status == "success") {
      test_pass("M1 Keyword Co-occurrence rendering")
      cat("   Plots created:", length(render_result$plots), "\n")
    } else {
      test_fail("M1 Keyword Co-occurrence rendering", render_result$status)
    }
  }
}, error = function(e) {
  test_fail("M1 Keyword Co-occurrence rendering", e$message)
})

# Test table builder
tryCatch({
  if (exists("kw_result") && kw_result$status == "success") {
    table_result <- build_m1_keyword_cooccurrence_table(kw_result)
    if (table_result$status == "success") {
      test_pass("M1 Keyword Co-occurrence table")
      cat("   Top keywords table rows:", nrow(table_result$top_keywords), "\n")
      cat("   Top pairs table rows:", nrow(table_result$top_pairs), "\n")
    } else {
      test_fail("M1 Keyword Co-occurrence table", table_result$status)
    }
  }
}, error = function(e) {
  test_fail("M1 Keyword Co-occurrence table", e$message)
})

# ---------------------------------------------------
# Test 2: M1 Lotka's Law
# ---------------------------------------------------
cat("\n--- Testing M1 Lotka's Law ---\n")

test_lotka_data <- data.frame(
  AU = c("Smith J", "Johnson K", "Williams L", "Brown M", "Davis P",
         "Smith J", "Johnson K", "Williams L", "Evans R", "Garcia T",
         "Smith J", "Johnson K", "Miller H", "Wilson S", "Moore A"),
  stringsAsFactors = FALSE
)

tryCatch({
  lotka_result <- compute_m1_lotka(test_lotka_data)
  if (lotka_result$status == "success") {
    test_pass("M1 Lotka's Law computation")
    cat("   Inverse square exponent:", round(lotka_result$exponent, 4), "\n")
    cat("   K-S p-value:", round(lotka_result$ks_pvalue, 4), "\n")
  } else {
    test_fail("M1 Lotka's Law computation", lotka_result$status)
  }
}, error = function(e) {
  test_fail("M1 Lotka's Law computation", e$message)
})

# ---------------------------------------------------
# Test 3: M1 Collaboration Index
# ---------------------------------------------------
cat("\n--- Testing M1 Collaboration Index ---\n")

test_collab_data <- data.frame(
  AU = c("Smith J", "Johnson K; Williams L", "Brown M; Davis P; Garcia T",
         "Miller H", "Wilson S; Moore A", "Smith J; Johnson K",
         "Williams L", "Brown M", "Davis P", "Garcia T"),
  TC = c(10, 25, 5, 30, 15, 8, 12, 6, 45, 3),
  stringsAsFactors = FALSE
)

tryCatch({
  collab_result <- compute_m1_collaboration(test_collab_data)
  if (collab_result$status == "success") {
    test_pass("M1 Collaboration computation")
    if (!is.null(collab_result$collaboration_index)) {
      cat("   Collaboration Index:", round(collab_result$collaboration_index, 4), "\n")
    }
    if (!is.null(collab_result$collaboration_coefficient)) {
      cat("   Collaboration Coefficient:", round(collab_result$collaboration_coefficient, 4), "\n")
    }
    if (!is.null(collab_result$documents_collaboration_rate)) {
      cat("   Documents with collaboration:", round(collab_result$documents_collaboration_rate * 100, 1), "%\n")
    }
  } else {
    test_fail("M1 Collaboration computation", collab_result$status)
  }
}, error = function(e) {
  test_fail("M1 Collaboration computation", e$message)
})

# ---------------------------------------------------
# Test 4: M2 Ridge Regression
# ---------------------------------------------------
cat("\n--- Testing M2 Ridge Regression ---\n")

test_ts_data <- data.frame(
  Year = 2010:2024,
  Articles = c(10, 15, 18, 22, 28, 35, 45, 52, 63, 78,
              95, 110, 130, 155, 185, 220, 260, 310, 365, 430)
)

tryCatch({
  ridge_result <- compute_m2_ridge(test_ts_data)
  if (ridge_result$status == "success") {
    test_pass("M2 Ridge regression computation")
    cat("   Optimal lambda:", round(ridge_result$optimal_lambda, 4), "\n")
    cat("   R-squared:", round(ridge_result$r_squared, 4), "\n")
  } else {
    test_fail("M2 Ridge regression computation", ridge_result$status)
  }
}, error = function(e) {
  test_fail("M2 Ridge regression computation", e$message)
})

# ---------------------------------------------------
# Test 5: M2 Change-point Detection
# ---------------------------------------------------
cat("\n--- Testing M2 Change-point Detection ---\n")

tryCatch({
  changepoint_result <- compute_m2_changepoint(test_ts_data)
  if (changepoint_result$status == "success") {
    test_pass("M2 Change-point detection")
    if (!is.null(changepoint_result$changepoints) && length(changepoint_result$changepoints) > 0) {
      cat("   Changepoints detected:", changepoint_result$changepoints, "\n")
    } else {
      cat("   No changepoints detected\n")
    }
    if (!is.null(changepoint_result$n_changepoints)) {
      cat("   Number of changepoints:", changepoint_result$n_changepoints, "\n")
    }
  } else {
    test_fail("M2 Change-point detection", changepoint_result$status)
  }
}, error = function(e) {
  test_fail("M2 Change-point detection", e$message)
})

# ---------------------------------------------------
# Test 6: M2 STL Decomposition
# ---------------------------------------------------
cat("\n--- Testing M2 STL Decomposition ---\n")

tryCatch({
  stl_result <- compute_m2_stl(test_ts_data)
  if (stl_result$status == "success") {
    test_pass("M2 STL decomposition")
    if (!is.null(stl_result$trend)) {
      cat("   Trend length:", length(stl_result$trend), "\n")
    }
    if (!is.null(stl_result$seasonal)) {
      cat("   Seasonal components:", length(stl_result$seasonal), "\n")
    }
  } else {
    test_fail("M2 STL decomposition", stl_result$status)
  }
}, error = function(e) {
  test_fail("M2 STL decomposition", e$message)
})

# ---------------------------------------------------
# Test 7: M2 Growth Models (22+ models)
# ---------------------------------------------------
cat("\n--- Testing M2 Growth Models ---\n")

tryCatch({
  reg_result <- compute_m2_regression(test_ts_data)
  if (reg_result$status == "success") {
    test_pass("M2 Regression with 22+ models")
    if (!is.null(reg_result$best_model)) {
      cat("   Best model:", reg_result$best_model$name, "\n")
      cat("   Best R-squared:", round(reg_result$best_model$R2, 4), "\n")
    }
    if (!is.null(reg_result$all_models)) {
      cat("   Total models fitted:", length(reg_result$all_models), "\n")
    }
    # List top 5 models
    if (!is.null(reg_result$comparison)) {
      top5 <- head(reg_result$comparison[order(-reg_result$comparison$R2), ], 5)
      cat("   Top 5 models by R2:\n")
      for (i in seq_len(nrow(top5))) {
        cat(sprintf("     %d. %s (R2=%.4f)\n", i, top5$model[i], top5$R2[i]))
      }
    }
  } else {
    test_fail("M2 Regression with 22+ models", reg_result$status)
  }
}, error = function(e) {
  test_fail("M2 Regression with 22+ models", e$message)
})

# ---------------------------------------------------
# Test 8: M3 Collaboration Indices
# ---------------------------------------------------
cat("\n--- Testing M3 Collaboration Indices ---\n")

test_country_data <- data.frame(
  C1 = c(
    "Univ A, Germany",
    "Univ B, USA; Univ C, UK",
    "Univ D, China; Univ E, China",
    "Univ F, Germany; Univ G, USA",
    "Univ H, Japan",
    "Univ I, UK; Univ J, Germany; Univ K, USA",
    "Univ L, China",
    "Univ M, Japan; Univ N, USA"
  ),
  TC = c(10, 25, 15, 30, 5, 45, 12, 8),
  stringsAsFactors = FALSE
)

tryCatch({
  # Prepare country data first
  prepared <- prepare_m3_country_data(test_country_data)
  if (prepared$status == "success") {
    collab_indices <- compute_m3_collaboration_indices(prepared)
    if (collab_indices$status == "success") {
      test_pass("M3 Collaboration indices")
      if (!is.null(collab_indices$salton_matrix)) {
        cat("   Salton matrix:", nrow(collab_indices$salton_matrix), "x", ncol(collab_indices$salton_matrix), "\n")
      }
      if (!is.null(collab_indices$jaccard_matrix)) {
        cat("   Jaccard matrix:", nrow(collab_indices$jaccard_matrix), "x", ncol(collab_indices$jaccard_matrix), "\n")
      }
      if (!is.null(collab_indices$affinity_matrix)) {
        cat("   Affinity matrix:", nrow(collab_indices$affinity_matrix), "x", ncol(collab_indices$affinity_matrix), "\n")
      }
    } else {
      test_fail("M3 Collaboration indices", collab_indices$status)
    }
  } else {
    test_fail("M3 Data preparation", prepared$status)
  }
}, error = function(e) {
  test_fail("M3 Collaboration indices", e$message)
})

# ---------------------------------------------------
# Summary
# ---------------------------------------------------
cat("\n")
cat("===========================================================\n")
cat("  Test Complete\n")
cat("===========================================================\n\n")

cat("To run a full pipeline test with real data:\n")
cat("  source('examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/main.r')\n\n")