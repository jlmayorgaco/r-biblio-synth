# ============================================================================
# test-module-m2-advanced.R - Advanced tests for refactored M2
# ============================================================================

make_annual_fixture_advanced <- function() {
  data.frame(
    Year = 2000:2020,
    Articles = c(10, 15, 18, 22, 28, 35, 42, 50, 55, 60,
                 78, 95, 110, 125, 140, 155, 175, 190, 210, 230, 250),
    stringsAsFactors = FALSE
  )
}

test_that("compute_m2_regression returns canonical model cards and composite ranking", {
  data <- make_annual_fixture_advanced()

  result <- compute_m2_regression(data, biblio_config())
  skip_if(result$status != "success", "Regression did not converge")

  expect_true(is.list(result$models))
  expect_true(nrow(result$comparison_table) > 0)
  expect_true(all(c("CompositeScore", "Rank", "Stability", "SMAPE") %in% names(result$comparison_table)))
  expect_true(result$best_model$name %in% names(result$models))
  expect_true(is.list(result$models[[result$best_model$name]]$metrics))
  expect_true(is.list(result$models[[result$best_model$name]]$diagnostics))
  expect_true("selection_reason" %in% names(result$best_model))
})

test_that("model registry and diagnostics expose trend and changepoint summaries", {
  data <- make_annual_fixture_advanced()

  computed <- list(
    regression = compute_m2_regression(data, biblio_config()),
    forecasting = compute_m2_forecasting(data, biblio_config()),
    advanced_ts = compute_m2_advanced_ts(data, biblio_config()),
    growth_models = compute_m2_growth_models_wrapper(data, biblio_config()),
    changepoint = compute_m2_changepoint(data, biblio_config())
  )

  registry <- m2_build_model_registry(computed, data, biblio_config())
  diagnostics <- compute_m2_diagnostics(
    data,
    registry,
    biblio_config(),
    changepoint_result = computed$changepoint,
    forecasting_result = computed$forecasting
  )

  expect_equal(diagnostics$status, "success")
  expect_true("annual" %in% names(registry))
  expect_true("trend_statistics" %in% names(diagnostics))
  expect_true("changepoint_profile" %in% names(diagnostics))
  expect_true("mann_kendall" %in% names(diagnostics$trend_statistics))
  expect_true("hurst_exponent" %in% names(diagnostics$trend_statistics))
  expect_true(is.data.frame(diagnostics$comparison))
})

test_that("forecast model comparison uses composite ranking", {
  data <- make_annual_fixture_advanced()
  result <- compute_m2_forecasting(data, biblio_config())
  skip_if(result$status != "success", "Forecasting did not complete")

  comparison <- result$model_comparison$comparison
  expect_true(all(c("CompositeScore", "CompositeRank") %in% names(comparison)))
  expect_true(result$model_comparison$best_model %in% comparison$model)
})

test_that("changepoint detection includes Pettitt consensus metadata", {
  data <- make_annual_fixture_advanced()
  result <- compute_m2_changepoint(data, biblio_config())

  expect_equal(result$status, "success")
  expect_true("pettitt" %in% names(result$changepoints))
  expect_true("pettitt_changepoints" %in% names(result$summary))
  expect_true("phase" %in% names(result$segments))
})
