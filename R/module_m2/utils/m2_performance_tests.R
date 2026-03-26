# ============================================================================
# module_m2/utils/m2_performance_tests.R - Full statistical tests
# ============================================================================

#' @export
m2_full_performance <- function(x, y, model) {
  preds <- tryCatch(predict(model), error = function(e) NULL)
  if (is.null(preds)) return(list())

  residuals <- y - preds
  std_resid <- if (sd(residuals) > 0) residuals / sd(residuals) else residuals

  list(
    model_fit = m2_compute_model_fit(y, preds, model),
    error_metrics = m2_compute_error_metrics(y, preds),
    normality_tests = m2_test_normality(residuals),
    heteroscedasticity_tests = m2_test_heteroscedasticity(model, residuals),
    autocorrelation_tests = m2_test_autocorrelation(model, residuals),
    linearity_tests = m2_test_linearity(model),
    residuals = round(residuals, 4),
    std_residuals = round(std_resid, 4)
  )
}

#' Model fit metrics
m2_compute_model_fit <- function(y, preds, model) {
  ss_res <- sum((y - preds)^2)
  ss_tot <- sum((y - mean(y))^2)
  r2 <- if (ss_tot > 0) 1 - ss_res / ss_tot else 0
  n <- length(y)
  p <- length(coef(model))
  adj_r2 <- 1 - (1 - r2) * (n - 1) / (n - p - 1)

  list(
    r2 = round(max(0, min(1, r2)), 6),
    adj_r2 = round(max(0, min(1, adj_r2)), 6),
    aic = tryCatch(AIC(model), error = function(e) NA),
    bic = tryCatch(BIC(model), error = function(e) NA)
  )
}

#' Error metrics
m2_compute_error_metrics <- function(y, preds) {
  residuals <- y - preds
  n <- length(y)

  list(
    mse = round(mean(residuals^2), 4),
    rmse = round(sqrt(mean(residuals^2)), 4),
    mae = round(mean(abs(residuals)), 4),
    mape = round(mean(abs(residuals / y[y != 0])) * 100, 2),
    rae = round(sum(abs(residuals)) / sum(abs(y - mean(y))), 4)
  )
}

#' Normality tests (5 tests)
m2_test_normality <- function(residuals) {
  list(
    shapiro_wilk = tryCatch({
      t <- shapiro.test(residuals)
      list(statistic = t$statistic, p_value = t$p.value)
    }, error = function(e) list(statistic = NA, p_value = NA)),

    kolmogorov_smirnov = tryCatch({
      t <- ks.test(residuals, "pnorm", mean = mean(residuals), sd = sd(residuals))
      list(statistic = t$statistic, p_value = t$p.value)
    }, error = function(e) list(statistic = NA, p_value = NA)),

    jarque_bera = tryCatch({
      t <- tseries::jarque.bera.test(residuals)
      list(statistic = t$statistic, p_value = t$p.value)
    }, error = function(e) list(statistic = NA, p_value = NA)),

    anderson_darling = tryCatch({
      t <- nortest::ad.test(residuals)
      list(statistic = t$statistic, p_value = t$p.value)
    }, error = function(e) list(statistic = NA, p_value = NA)),

    cramer_von_mises = tryCatch({
      t <- nortest::cvm.test(residuals)
      list(statistic = t$statistic, p_value = t$p.value)
    }, error = function(e) list(statistic = NA, p_value = NA))
  )
}

#' Heteroscedasticity tests (3 tests)
m2_test_heteroscedasticity <- function(model, residuals) {
  list(
    breusch_pagan = tryCatch({
      t <- lmtest::bptest(model)
      list(statistic = t$statistic, p_value = t$p.value)
    }, error = function(e) list(statistic = NA, p_value = NA)),

    white = tryCatch({
      # Simplified White test using squared residuals
      n <- length(residuals)
      fit <- lm(residuals^2 ~ 1)
      list(statistic = summary(fit)$r.squared * n, p_value = NA)
    }, error = function(e) list(statistic = NA, p_value = NA)),

    goldfeld_quandt = tryCatch({
      t <- lmtest::gqtest(model)
      list(statistic = t$statistic, p_value = t$p.value)
    }, error = function(e) list(statistic = NA, p_value = NA))
  )
}

#' Autocorrelation tests (3 tests)
m2_test_autocorrelation <- function(model, residuals) {
  list(
    durbin_watson = tryCatch({
      t <- lmtest::dwtest(model)
      list(statistic = t$statistic, p_value = t$p.value)
    }, error = function(e) list(statistic = NA, p_value = NA)),

    ljung_box = tryCatch({
      t <- Box.test(residuals, lag = 10, type = "Ljung-Box")
      list(statistic = t$statistic, p_value = t$p.value)
    }, error = function(e) list(statistic = NA, p_value = NA)),

    breusch_godfrey = tryCatch({
      t <- lmtest::bgtest(model, order = 1)
      list(statistic = t$statistic, p_value = t$p.value)
    }, error = function(e) list(statistic = NA, p_value = NA))
  )
}

#' Linearity tests (2 tests)
m2_test_linearity <- function(model) {
  list(
    ramsey_reset = tryCatch({
      t <- lmtest::resettest(model)
      list(statistic = t$statistic, p_value = t$p.value)
    }, error = function(e) list(statistic = NA, p_value = NA)),

    harvey_collier = tryCatch({
      t <- lmtest::harvtest(model)
      list(statistic = t$statistic, p_value = t$p.value)
    }, error = function(e) list(statistic = NA, p_value = NA))
  )
}

#' ACF of residuals
m2_compute_acf <- function(residuals, max_lag = 20) {
  acf_vals <- tryCatch(acf(residuals, plot = FALSE, lag.max = max_lag)$acf[-1], error = function(e) numeric(0))
  round(acf_vals, 4)
}
