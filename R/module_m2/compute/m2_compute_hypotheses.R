# ============================================================================
# m2_compute_hypotheses.R - Hypothesis Testing for M2 (Annual Production)
# ============================================================================
# Tests key hypotheses about publication trends and growth patterns

#' Compute hypothesis tests for M2
#'
#' @param input Time series data (Year, Articles)
#' @param config Configuration list  
#' @return List with hypothesis test results
#' @export
compute_m2_hypotheses <- function(input, config = biblio_config()) {
  if (!is.data.frame(input)) {
    return(list(hyphypotheses = list(), status = "error: invalid input"))
  }
  
  year_col <- if ("Year" %in% names(input)) "Year" else names(input)[1]
  articles_col <- if ("Articles" %in% names(input)) "Articles" else names(input)[2]
  
  years <- input[[year_col]]
  articles <- input[[articles_col]]
  
  hypotheses <- list()
  
  # H02.1: Growth is linear
  hypotheses$H02_1 <- test_linear_growth_hypothesis(years, articles)
  
  # H02.2: No structural breaks in trend
  hypotheses$H02_2 <- test_structural_break_hypothesis(years, articles)
  
  # H02.3: Production follows exponential pattern
  hypotheses$H02_3 <- test_exponential_growth_hypothesis(years, articles)
  
  # H02.4: Residuals are normally distributed
  hypotheses$H02_4 <- test_residual_normality_hypothesis(years, articles)
  
  # H02.5: Variance is homoscedastic
  hypotheses$H02_5 <- test_homoscedasticity_hypothesis(years, articles)
  
  # H02.6: Growth rate is constant
  hypotheses$H02_6 <- test_constant_growth_rate_hypothesis(years, articles)
  
  # H02.7: No significant outliers
  hypotheses$H02_7 <- test_outlier_hypothesis(years, articles)
  
  # H02.8: Production follows logistic S-curve
  hypotheses$H02_8 <- test_logistic_growth_hypothesis(years, articles)
  
  # H02.9: Year-over-year growth is stationary
  hypotheses$H02_9 <- test_stationarity_hypothesis(years, articles)
  
  # H02.10: Trend is accelerating/decelerating
  hypotheses$H02_10 <- test_acceleration_hypothesis(years, articles)
  
  summary_stats <- summarize_hypothesis_results(hyphypotheses)
  
  list(
    hyphypotheses = hyphypotheses,
    n_hypotheses = length(hypotheses),
    n_rejected = summary_stats$n_rejected,
    n_not_rejected = summary_stats$n_failed_to_reject,
    rejection_rate = summary_stats$rejection_rate,
    summary = summary_stats,
    status = "success"
  )
}

# Individual tests

test_linear_growth_hypothesis <- function(years, articles) {
  fit <- tryCatch(lm(articles ~ years), error = function(e) NULL)
  if (is.null(fit)) return(list(hyphypothesis = "Growth is linear", result = "inconclusive"))
  
  r2 <- summary(fit)$r.squared
  adj_r2 <- summary(fit)$adj.r.squared
  
  result <- if (r2 > 0.9) "fail_to_reject" else "reject"
  
  list(
    hyphypothesis = "Publication growth follows a linear trend",
    null = "Growth is non-linear",
    result = result,
    R_squared = r2,
    Adj_R_squared = adj_r2,
    interpretation = sprintf("R² = %.3f. %s",
                            r2, if (r2 > 0.9) "Strong linear fit." else "Non-linear pattern detected.")
  )
}

test_structural_break_hypothesis <- function(years, articles) {
  n <- length(years)
  if (n < 10) return(list(hyphypothesis = "No structural breaks in trend", result = "inconclusive"))
  
  mid <- floor(n / 2)
  
  fit1 <- tryCatch(lm(articles[1:mid] ~ years[1:mid]), error = function(e) NULL)
  fit2 <- tryCatch(lm(articles[(mid+1):n] ~ years[(mid+1):n]), error = function(e) NULL)
  fit_full <- tryCatch(lm(articles ~ years), error = function(e) NULL)
  
  if (is.null(fit1) || is.null(fit2) || is.null(fit_full)) {
    return(list(hyphypothesis = "No structural breaks in trend", result = "inconclusive"))
  }
  
  sse_full <- sum(residuals(fit_full)^2)
  sse_split <- sum(residuals(fit1)^2) + sum(residuals(fit2)^2)
  
  f_stat <- ((sse_full - sse_split) / 2) / (sse_split / (n - 4))
  p_value <- 1 - pf(f_stat, 2, n - 4)
  
  result <- if (p_value > 0.05) "fail_to_reject" else "reject"
  
  list(
    hyphypothesis = "No structural breaks in publication trend",
    null = "Structural break exists",
    result = result,
    F_statistic = f_stat,
    p_value = p_value,
    interpretation = sprintf("Chow test F = %.2f (p = %.4f). %s",
                            f_stat, p_value,
                            if (p_value > 0.05) "No break detected." else "Structural break likely.")
  )
}

test_exponential_growth_hypothesis <- function(years, articles) {
  articles_pos <- articles[articles > 0]
  years_sub <- years[articles > 0]
  
  if (length(articles_pos) < 5) return(list(hyphypothesis = "Exponential growth", result = "inconclusive"))
  
  fit_exp <- tryCatch(lm(log(articles_pos) ~ years_sub), error = function(e) NULL)
  fit_lin <- tryCatch(lm(articles ~ years), error = function(e) NULL)
  
  if (is.null(fit_exp) || is.null(fit_lin)) return(list(hyphypothesis = "Exponential growth", result = "inconclusive"))
  
  r2_exp <- summary(fit_exp)$r.squared
  r2_lin <- summary(fit_lin)$r.squared
  
  result <- if (r2_exp > r2_lin && r2_exp > 0.85) "fail_to_reject" else "reject"
  
  list(
    hyphypothesis = "Publication growth is exponential",
    null = "Growth is not exponential",
    result = result,
    R2_exponential = r2_exp,
    R2_linear = r2_lin,
    interpretation = sprintf("Exp R² = %.3f, Lin R² = %.3f. %s",
                            r2_exp, r2_lin,
                            if (r2_exp > r2_lin) "Exponential fits better." else "Linear fits better.")
  )
}

test_residual_normality_hypothesis <- function(years, articles) {
  fit <- tryCatch(lm(articles ~ years), error = function(e) NULL)
  if (is.null(fit)) return(list(hyphypothesis = "Residuals normality", result = "inconclusive"))
  
  res <- residuals(fit)
  
  if (length(res) < 8) return(list(hyphypothesis = "Residuals normality", result = "inconclusive"))
  
  sw_test <- tryCatch(shapiro.test(res), error = function(e) NULL)
  
  if (is.null(sw_test)) return(list(hyphypothesis = "Residuals normality", result = "inconclusive"))
  
  result <- if (sw_test$p.value > 0.05) "fail_to_reject" else "reject"
  
  list(
    hyphypothesis = "Linear model residuals are normally distributed",
    null = "Residuals deviate from normality",
    result = result,
    W_statistic = sw_test$statistic,
    p_value = sw_test$p.value,
    interpretation = sprintf("Shapiro-Wilk W = %.4f (p = %.4f). %s",
                            sw_test$statistic, sw_test$p.value,
                            if (sw_test$p.value > 0.05) "Normality assumed." else "Non-normal residuals.")
  )
}

test_homoscedasticity_hypothesis <- function(years, articles) {
  fit <- tryCatch(lm(articles ~ years), error = function(e) NULL)
  if (is.null(fit)) return(list(hyphypothesis = "Homoscedasticity", result = "inconclusive"))
  
  bp_stat <- tryCatch({
    fitted_vals <- fitted(fit)
    res_squared <- residuals(fit)^2
    aux_fit <- lm(res_squared ~ fitted_vals)
    n <- length(res_squared)
    n * summary(aux_fit)$r.squared
  }, error = function(e) NULL)
  
  if (is.null(bp_stat)) return(list(hyphypothesis = "Homoscedasticity", result = "inconclusive"))
  
  p_value <- 1 - pchisq(bp_stat, 1)
  result <- if (p_value > 0.05) "fail_to_reject" else "reject"
  
  list(
    hyphypothesis = "Variance is constant (homoscedastic)",
    null = "Variance is non-constant (heteroscedastic)",
    result = result,
    BP_statistic = bp_stat,
    p_value = p_value,
    interpretation = sprintf("Breusch-Pagan = %.2f (p = %.4f). %s",
                            bp_stat, p_value,
                            if (p_value > 0.05) "Homoscedasticity assumed." else "Heteroscedasticity detected.")
  )
}

test_constant_growth_rate_hypothesis <- function(years, articles) {
  if (length(years) < 10) return(list(hyphypothesis = "Constant growth rate", result = "inconclusive"))
  
  growth_rates <- diff(articles) / articles[-length(articles)]
  
  if (length(growth_rates) < 5) return(list(hyphypothesis = "Constant growth rate", result = "inconclusive"))
  
  mean_growth <- mean(growth_rates, na.rm = TRUE)
  sd_growth <- sd(growth_rates, na.rm = TRUE)
  
  # Handle division by zero when mean growth rate is zero or near-zero
  if (abs(mean_growth) < .Machine$double.eps) {
    # If growth rate is zero, check if variance is also zero
    cv <- if (sd_growth < .Machine$double.eps) 0 else Inf
  } else {
    cv <- sd_growth / abs(mean_growth)
  }
  
  result <- if (is.finite(cv) && cv < 0.5) "fail_to_reject" else "reject"
  
  list(
    hyphypothesis = "Year-over-year growth rate is constant",
    null = "Growth rate varies over time",
    result = result,
    mean_growth = mean_growth,
    sd_growth = sd_growth,
    CV = cv,
    interpretation = sprintf("Mean growth = %.1f%%, CV = %.2f. %s",
                            mean_growth * 100, cv,
                            if (!is.finite(cv)) "Growth rate undefined." 
                            else if (cv < 0.5) "Growth rate is relatively stable." 
                            else "Growth rate varies.")
  )
}

test_outlier_hypothesis <- function(years, articles) {
  fit <- tryCatch(lm(articles ~ years), error = function(e) NULL)
  if (is.null(fit)) return(list(hyphypothesis = "No outliers", result = "inconclusive"))
  
  res <- residuals(fit)
  std_res <- rstandard(fit)
  
  n_outliers <- sum(abs(std_res) > 2, na.rm = TRUE)
  
  result <- if (n_outliers == 0) "fail_to_reject" else "reject"
  
  list(
    hyphypothesis = "No significant outliers in production data",
    null = "Outliers present",
    result = result,
    n_outliers = n_outliers,
    outlier_years = years[abs(std_res) > 2],
    interpretation = sprintf("%d outlier(s) detected (|std res| > 2).", n_outliers)
  )
}

test_logistic_growth_hypothesis <- function(years, articles) {
  if (length(years) < 8) return(list(hyphypothesis = "Logistic growth", result = "inconclusive"))
  
  # Fit logistic: articles ~ K / (1 + exp(-r*(year-t0)))
  # Simplified test: check for inflection point
  n <- length(articles)
  first_third <- articles[1:floor(n/3)]
  last_third <- articles[(floor(2*n/3)+1):n]
  
  mean_first <- mean(first_third, na.rm = TRUE)
  mean_last <- mean(last_third, na.rm = TRUE)
  mean_mid <- mean(articles[(floor(n/3)+1):floor(2*n/3)], na.rm = TRUE)
  
  # Check for S-shape: growth slows at end
  growth_first <- (max(first_third) - min(first_third)) / length(first_third)
  growth_last <- (max(last_third) - min(last_third)) / length(last_third)
  
  result <- if (growth_first > growth_last * 1.5) "fail_to_reject" else "reject"
  
  list(
    hyphypothesis = "Production follows logistic S-curve (saturation)",
    null = "No saturation pattern observed",
    result = result,
    early_growth = growth_first,
    late_growth = growth_last,
    interpretation = sprintf("Early growth = %.1f, Late growth = %.1f. %s",
                            growth_first, growth_last,
                            if (result == "fail_to_reject") "Saturation pattern detected." else "No saturation detected.")
  )
}

test_stationarity_hypothesis <- function(years, articles) {
  if (length(articles) < 10) return(list(hyphypothesis = "Stationarity", result = "inconclusive"))
  
  yoy_growth <- diff(articles) / articles[-length(articles)]
  
  if (length(yoy_growth) < 5) return(list(hyphypothesis = "Stationarity", result = "inconclusive"))
  
  # Simple test: check if mean changes over time
  n <- length(yoy_growth)
  first_half <- yoy_growth[1:floor(n/2)]
  second_half <- yoy_growth[(floor(n/2)+1):n]
  
  t_test <- tryCatch(t.test(first_half, second_half), error = function(e) NULL)
  
  if (is.null(t_test)) return(list(hyphypothesis = "Stationarity", result = "inconclusive"))
  
  result <- if (t_test$p.value > 0.05) "fail_to_reject" else "reject"
  
  list(
    hyphypothesis = "YoY growth rate is stationary",
    null = "Growth rate has time-varying mean",
    result = result,
    t_statistic = t_test$statistic,
    p_value = t_test$p.value,
    interpretation = sprintf("T-test p = %.4f. %s",
                            t_test$p.value,
                            if (t_test$p.value > 0.05) "Stationarity assumed." else "Non-stationary.")
  )
}

test_acceleration_hypothesis <- function(years, articles) {
  if (length(years) < 8) return(list(hyphypothesis = "Acceleration", result = "inconclusive"))
  
  fit_quad <- tryCatch(lm(articles ~ years + I(years^2)), error = function(e) NULL)
  if (is.null(fit_quad)) return(list(hyphypothesis = "Acceleration", result = "inconclusive"))
  
  quad_coef <- coef(fit_quad)[3]
  p_value <- summary(fit_quad)$coefficients[3, 4]
  
  direction <- if (quad_coef > 0) "accelerating" else if (quad_coef < 0) "decelerating" else "constant"
  
  result <- if (p_value > 0.05) "fail_to_reject" else "reject"
  
  list(
    hyphypothesis = "Production trend is accelerating/decelerating",
    null = "Trend follows constant acceleration (quadratic = 0)",
    result = result,
    quadratic_coefficient = quad_coef,
    p_value = p_value,
    direction = direction,
    interpretation = sprintf("Quadratic coeff = %.4f (p = %.4f). Trend is %s.",
                            quad_coef, p_value, direction)
  )
}

summarize_hypothesis_results <- function(hyphypotheses) {
  n_total <- length(hyphypotheses)
  n_rejected <- sum(sapply(hyphypotheses, function(h) h$result == "reject"), na.rm = TRUE)
  n_failed <- sum(sapply(hyphypotheses, function(h) h$result == "fail_to_reject"), na.rm = TRUE)
  
  list(
    n_total = n_total,
    n_rejected = n_rejected,
    n_failed_to_reject = n_failed,
    rejection_rate = n_rejected / n_total
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b