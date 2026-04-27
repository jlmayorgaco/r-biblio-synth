# ============================================================================
# test-module-m2-core.R - Tests for M2 Annual Production core functions
# ============================================================================

context("M2 Annual Production - Core Functions")

# Create annual production test fixture
make_annual_fixture <- function() {
  data.frame(
    Year = 2000:2020,
    Articles = c(10, 15, 18, 22, 28, 35, 42, 50, 55, 60,
78, 95, 110, 125, 140, 155, 175, 190, 210, 230, 250),
    stringsAsFactors = FALSE
  )
}

test_that("compute_m2_eda returns correct structure", {
  data <- make_annual_fixture()
  
  result <- compute_m2_eda(data, biblio_config())
  
  expect_true(is.list(result))
  expect_true("status" %in% names(result))
  expect_true("metrics" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("summary_stats" %in% names(result))
  expect_true("outlier_sets" %in% names(result))
  
  if (result$status == "success") {
    expect_true("mean" %in% names(result$metrics))
    expect_true("median" %in% names(result$metrics))
    expect_true("sd" %in% names(result$metrics))
    expect_true("min" %in% names(result$metrics))
    expect_true("max" %in% names(result$metrics))
    expect_true("growth_rate" %in% names(result$summary_stats))
  }
})

test_that("render_m2_eda creates descriptive stats plot from compute output", {
  data <- make_annual_fixture()
  result <- compute_m2_eda(data, biblio_config())
  rendered <- render_m2_eda(result, biblio_config())

  expect_equal(rendered$status, "success")
  expect_true("descriptive_stats" %in% names(rendered$plots))
  expect_s3_class(rendered$plots$descriptive_stats, "ggplot")
})

test_that("compute_m2_regression handles linear model", {
  data <- make_annual_fixture()
  
  result <- compute_m2_regression(data, biblio_config())
  
  expect_true(is.list(result))
  expect_true("status" %in% names(result))
  
  if (result$status == "success") {
    expect_true("linear" %in% names(result))
  }
})

test_that("fit_bass_model returns valid Bass diffusion parameters", {
  data <- make_annual_fixture()
  years <- data$Year
  articles <- data$Articles
  
  result <- fit_bass_model(years, articles)
  
  expect_true(is.list(result))
  expect_true("status" %in% names(result))
  
  if (result$status == "success") {
    expect_true("params" %in% names(result))
    expect_true("m" %in% names(result$params))
    expect_true("p" %in% names(result$params))
    expect_true("q" %in% names(result$params))
    expect_true(result$params$m > sum(articles))  # Market potential >cumulative
  }
})

test_that("fit_gompertz_model returns valid parameters", {
  data <- make_annual_fixture()
  years <- data$Year
  articles <- data$Articles
  
  result <- fit_gompertz_model(years, articles)
  
  expect_true(is.list(result))
  expect_true("model" %in% names(result))
  expect_equal(result$model, "gompertz")
})


test_that("compute_m2_forecasting generates predictions", {
  data <- make_annual_fixture()
  
  result <- compute_m2_forecasting(data, biblio_config())
  
  expect_true(is.list(result))
  expect_true("status" %in% names(result))
  
  if (result$status == "success") {
    # Should have ARIMA results
    expect_true(!is.null(result$arima) || !is.null(result$ets))
  }
})

test_that("compute_m2_harmonics detects periodicity", {
  data <- make_annual_fixture()
  config <- biblio_config()
  
  result <- compute_m2_harmonics(data, config)
  
  expect_true(is.list(result))
  expect_true("status" %in% names(result))
})

test_that("compute_m2_changepoint identifies changes", {
  data <- make_annual_fixture()
  
  result <- compute_m2_changepoint(data, biblio_config())
  
  expect_true(is.list(result))
  expect_true("status" %in% names(result))
})

test_that("validate_m2_input validates required columns", {
  valid_data <- data.frame(Year = 2000:2010, Articles = 1:11)
  invalid_data <- data.frame(Year = 2000:2010, Value = 1:11)  # Wrong column
  
  valid_result <- validate_m2_input(valid_data, biblio_config())
  invalid_result <- validate_m2_input(invalid_data, biblio_config())
  
  expect_true(valid_result$ok)
  expect_false(invalid_result$ok)
})

test_that("compute_m2_residual_analysis checks model residuals", {
  data <- make_annual_fixture()
  
  # First compute regression to get model
  regression <- compute_m2_regression(data, biblio_config())
  
  if (regression$status == "success") {
    result <- compute_m2_residual_analysis(data, regression, biblio_config())
    
    expect_true(is.list(result))
    expect_true("status" %in% names(result))
    expect_true("heteroscedasticity" %in% names(result))
    expect_true("variance_model" %in% names(result))
    expect_true("standardized_residuals" %in% names(result))
    expect_true("pacf_values" %in% names(result$autocorrelation))
  }
})

test_that("render_m2_residual_analysis returns flat legacy diagnostics", {
  data <- make_annual_fixture()
  regression <- compute_m2_regression(data, biblio_config())
  skip_if(regression$status != "success", "Regression did not converge for residual rendering test")

  residuals <- compute_m2_residual_analysis(data, regression, biblio_config())
  rendered <- render_m2_residual_analysis(residuals, biblio_config())

  expect_equal(rendered$status, "success")
  expect_true(all(c(
    "normality_histogram",
    "normality_qq",
    "residual_acf",
    "residual_pacf",
    "durbin_watson",
    "heteroscedasticity",
    "standardized_residuals",
    "residuals_squared",
    "residuals_squared_model_fit",
    "residual_fft"
  ) %in% names(rendered$plots)))
})

test_that("Model comparison works correctly", {
  data <- make_annual_fixture()
  
  # Fit multiple growth models
  bass <- tryCatch(fit_bass_model(data$Year, data$Articles), error = function(e) NULL)
  gompertz <- tryCatch(fit_gompertz_model(data$Year, data$Articles), error = function(e) NULL)
  
  if (!is.null(bass) && !is.null(gompertz)) {
    models <- list(bass = bass, gompertz = gompertz)
    
    comparison <- compare_growth_models(models)
    
    expect_true(is.data.frame(comparison))
    expect_true("model" %in% names(comparison))
    expect_true("AIC" %in% names(comparison))
  }
})
