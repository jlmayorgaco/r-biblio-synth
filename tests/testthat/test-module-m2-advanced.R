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
  expect_true(all(c("CompositeScore", "CompositeRank", "RelativeImprovement_MAE", "DeltaVsNaive_MAE", "Coverage95", "IntervalWidth95") %in% names(comparison)))
  expect_true(result$model_comparison$best_model %in% comparison$model)
  expect_true("selection_basis" %in% names(result$model_comparison))
  expect_true("Ensemble" %in% comparison$model)
  expect_true(is.data.frame(result$cv_results$interval_coverage))
})

test_that("journal-grade M2 hypotheses expose a common editorial contract", {
  data <- make_annual_fixture_advanced()
  forecasting <- compute_m2_forecasting(data, biblio_config())
  regression <- compute_m2_regression(data, biblio_config())
  changepoint <- compute_m2_changepoint(data, biblio_config())
  diagnostics <- compute_m2_diagnostics(
    data,
    list(annual = regression$models),
    biblio_config(),
    changepoint_result = changepoint,
    forecasting_result = forecasting
  )

  result <- compute_m2_hypotheses(
    data,
    biblio_config(),
    context = list(
      forecasting = forecasting,
      regression = regression,
      changepoint = changepoint,
      diagnostics = diagnostics
    )
  )

  table <- build_m2_hypotheses_table(result)$table
  expect_true(nrow(table) >= 30)
  expect_true(all(c(
    "hypothesis_id",
    "question",
    "test",
    "effect_size",
    "confidence_interval",
    "p_value",
    "p_adjusted",
    "decision",
    "plain_language_interpretation"
  ) %in% names(table)))
  expect_true(all(c("H02_15", "H02_18", "H02_21", "H02_24", "H02_35", "H02_36") %in% table$hypothesis_id))
})

test_that("journal-grade regression tables separate core and exploratory candidates", {
  data <- make_annual_fixture_advanced()
  result <- compute_m2_regression(data, biblio_config())
  skip_if(result$status != "success", "Regression did not converge")

  tables <- build_m2_regression_table(result)$tables
  expect_true(all(c("core_ranking", "exploratory_models", "selected_model", "diagnostic_summary", "limitations") %in% names(tables)))
  expect_true(nrow(tables$core_ranking) > 0)
})

test_that("forecasting tables expose interval calibration and ensemble diagnostics", {
  data <- make_annual_fixture_advanced()
  result <- compute_m2_forecasting(data, biblio_config())
  skip_if(result$status != "success", "Forecasting did not complete")

  tables <- build_m2_forecasting_table(result)$tables
  expect_true(all(c("model_comparison", "selection_summary", "forecasts", "cv_results", "ensemble_weights", "interval_calibration") %in% names(tables)))
  expect_true(nrow(tables$interval_calibration) >= 1)
})

test_that("forecasting JSON payload exposes calibration and ensemble evidence", {
  data <- make_annual_fixture_advanced()
  result <- compute_m2_forecasting(data, biblio_config())
  skip_if(result$status != "success", "Forecasting did not complete")

  payload <- m2_prepare_json_export_payload("forecasting", result)
  expect_true(all(c("cv_results", "ensemble_weights", "interval_calibration", "calibration_summary") %in% names(payload)))
  expect_true(is.list(payload$calibration_summary))
  expect_true(isTRUE(payload$calibration_summary$status %in% c("success", "unavailable")))
})

test_that("M2 report recovers Kendall tau from curated hypotheses when needed", {
  data <- make_annual_fixture_advanced()
  result <- run_m2(data, biblio_config(), export = FALSE)
  report <- build_m2_report(result$data, biblio_config())

  expect_true(any(grepl("Mann-Kendall tau: 1\\.000", report$lines)))
})

test_that("changepoint detection includes Pettitt consensus metadata", {
  data <- make_annual_fixture_advanced()
  result <- compute_m2_changepoint(data, biblio_config())

  expect_equal(result$status, "success")
  expect_true("pettitt" %in% names(result$changepoints))
  expect_true("pettitt_changepoints" %in% names(result$summary))
  expect_true("phase" %in% names(result$segments))
})
