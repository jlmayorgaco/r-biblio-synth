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
    return(list(hypotheses = list(), hyphypotheses = list(), status = "error: invalid input"))
  }

  year_col <- if ("Year" %in% names(input)) "Year" else names(input)[1]
  articles_col <- if ("Articles" %in% names(input)) "Articles" else names(input)[2]

  years <- suppressWarnings(as.numeric(input[[year_col]]))
  articles <- suppressWarnings(as.numeric(input[[articles_col]]))

  hypotheses <- list(
    H02_1 = test_linear_growth_hypothesis(years, articles),
    H02_2 = test_structural_break_hypothesis(years, articles),
    H02_3 = test_exponential_growth_hypothesis(years, articles),
    H02_4 = test_residual_normality_hypothesis(years, articles),
    H02_5 = test_homoscedasticity_hypothesis(years, articles),
    H02_6 = test_constant_growth_rate_hypothesis(years, articles),
    H02_7 = test_outlier_hypothesis(years, articles),
    H02_8 = test_logistic_growth_hypothesis(years, articles),
    H02_9 = test_stationarity_hypothesis(years, articles),
    H02_10 = test_acceleration_hypothesis(years, articles),
    H02_11 = test_monotonic_trend_hypothesis(years, articles),
    H02_12 = test_sen_slope_hypothesis(years, articles)
  )

  hypotheses <- m2_standardize_hypotheses(hypotheses)
  summary_stats <- summarize_hypothesis_results(hypotheses)

  list(
    hypotheses = hypotheses,
    hyphypotheses = hypotheses,
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
  fit <- m2_safe_lm(articles ~ years)
  if (is.null(fit)) {
    return(m2_inconclusive_hypothesis(
      label = "Publication growth follows a linear trend",
      null = "Growth is non-linear",
      interpretation = "Linear model could not be estimated"
    ))
  }

  fit_summary <- m2_safe_summary(fit)
  if (is.null(fit_summary)) {
    return(m2_inconclusive_hypothesis(
      label = "Publication growth follows a linear trend",
      null = "Growth is non-linear",
      interpretation = "Linear model summary could not be computed"
    ))
  }

  r2 <- fit_summary$r.squared
  adj_r2 <- fit_summary$adj.r.squared
  if (!m2_is_finite_scalar(r2) || !m2_is_finite_scalar(adj_r2)) {
    return(m2_inconclusive_hypothesis(
      label = "Publication growth follows a linear trend",
      null = "Growth is non-linear",
      interpretation = "Linear fit is degenerate for the available series"
    ))
  }

  result <- if (r2 > 0.9) "fail_to_reject" else "reject"

  list(
    hypothesis = "Publication growth follows a linear trend",
    null = "Growth is non-linear",
    result = result,
    R_squared = r2,
    Adj_R_squared = adj_r2,
    interpretation = sprintf(
      "R^2 = %.3f. %s",
      r2,
      if (r2 > 0.9) "Strong linear fit." else "Non-linear pattern detected."
    )
  )
}

test_structural_break_hypothesis <- function(years, articles) {
  n <- length(years)
  if (n < 10) {
    return(m2_inconclusive_hypothesis(
      label = "No structural breaks in publication trend",
      null = "Structural break exists",
      interpretation = "At least 10 annual observations are required"
    ))
  }

  pettitt <- m2_pettitt_test(articles)
  if (!m2_is_finite_scalar(pettitt$statistic) || !m2_is_finite_scalar(pettitt$p_value)) {
    return(m2_inconclusive_hypothesis(
      label = "No structural breaks in publication trend",
      null = "Structural break exists",
      interpretation = "Pettitt change-point test returned non-finite statistics"
    ))
  }

  result <- if (pettitt$p_value > 0.05) "fail_to_reject" else "reject"
  break_year <- if (m2_is_finite_scalar(pettitt$index) && pettitt$index >= 1 && pettitt$index <= length(years)) {
    years[as.integer(pettitt$index)]
  } else {
    NA_real_
  }

  list(
    hypothesis = "No structural breaks in publication trend",
    null = "Structural break exists",
    test = "Pettitt change-point test",
    evidence_class = "statistical",
    result = result,
    statistic = pettitt$statistic,
    p_value = pettitt$p_value,
    breakpoint_index = pettitt$index,
    breakpoint_year = break_year,
    interpretation = sprintf(
      "Pettitt K = %.1f (p = %.4f). %s",
      pettitt$statistic,
      pettitt$p_value,
      if (pettitt$p_value > 0.05) "No abrupt shift detected." else sprintf("Likely break around %s.", format(round(break_year, 0), trim = TRUE))
    )
  )
}

test_exponential_growth_hypothesis <- function(years, articles) {
  valid_positive <- articles > 0 & is.finite(articles) & is.finite(years)
  articles_pos <- articles[valid_positive]
  years_sub <- years[valid_positive]

  if (length(articles_pos) < 5) {
    return(m2_inconclusive_hypothesis(
      label = "Publication growth is exponential",
      null = "Growth is not exponential",
      interpretation = "At least 5 positive observations are required"
    ))
  }

  fit_exp <- m2_safe_lm(log(articles_pos) ~ years_sub)
  fit_lin <- m2_safe_lm(articles ~ years)

  if (is.null(fit_exp) || is.null(fit_lin)) {
    return(m2_inconclusive_hypothesis(
      label = "Publication growth is exponential",
      null = "Growth is not exponential",
      interpretation = "Linear or exponential model could not be estimated"
    ))
  }

  r2_exp <- tryCatch(m2_safe_summary(fit_exp)$r.squared, error = function(e) NA_real_)
  r2_lin <- tryCatch(m2_safe_summary(fit_lin)$r.squared, error = function(e) NA_real_)
  if (!m2_is_finite_scalar(r2_exp) || !m2_is_finite_scalar(r2_lin)) {
    return(m2_inconclusive_hypothesis(
      label = "Publication growth is exponential",
      null = "Growth is not exponential",
      interpretation = "Model comparison returned non-finite fit statistics"
    ))
  }

  result <- if (r2_exp > r2_lin && r2_exp > 0.85) "fail_to_reject" else "reject"

  list(
    hypothesis = "Publication growth is exponential",
    null = "Growth is not exponential",
    result = result,
    R2_exponential = r2_exp,
    R2_linear = r2_lin,
    interpretation = sprintf(
      "Exp R^2 = %.3f, Lin R^2 = %.3f. %s",
      r2_exp,
      r2_lin,
      if (r2_exp > r2_lin) "Exponential fits better." else "Linear fits better."
    )
  )
}

test_residual_normality_hypothesis <- function(years, articles) {
  fit <- m2_safe_lm(articles ~ years)
  if (is.null(fit)) {
    return(m2_inconclusive_hypothesis(
      label = "Linear model residuals are normally distributed",
      null = "Residuals deviate from normality",
      interpretation = "Linear model could not be estimated"
    ))
  }

  res <- residuals(fit)
  if (length(res) < 8) {
    return(m2_inconclusive_hypothesis(
      label = "Linear model residuals are normally distributed",
      null = "Residuals deviate from normality",
      interpretation = "At least 8 residuals are required for Shapiro-Wilk testing"
    ))
  }

  sw_test <- tryCatch(shapiro.test(res), error = function(e) NULL)
  if (is.null(sw_test) || !m2_is_finite_scalar(sw_test$p.value)) {
    return(m2_inconclusive_hypothesis(
      label = "Linear model residuals are normally distributed",
      null = "Residuals deviate from normality",
      interpretation = "Residual normality test could not be computed"
    ))
  }

  result <- if (sw_test$p.value > 0.05) "fail_to_reject" else "reject"

  list(
    hypothesis = "Linear model residuals are normally distributed",
    null = "Residuals deviate from normality",
    result = result,
    W_statistic = sw_test$statistic,
    p_value = sw_test$p.value,
    interpretation = sprintf(
      "Shapiro-Wilk W = %.4f (p = %.4f). %s",
      sw_test$statistic,
      sw_test$p.value,
      if (sw_test$p.value > 0.05) "Normality assumed." else "Non-normal residuals."
    )
  )
}

test_homoscedasticity_hypothesis <- function(years, articles) {
  fit <- m2_safe_lm(articles ~ years)
  if (is.null(fit)) {
    return(m2_inconclusive_hypothesis(
      label = "Variance is constant (homoscedastic)",
      null = "Variance is non-constant (heteroscedastic)",
      interpretation = "Linear model could not be estimated"
    ))
  }

  bp_stat <- tryCatch({
    fitted_vals <- fitted(fit)
    res_squared <- residuals(fit)^2
    aux_fit <- lm(res_squared ~ fitted_vals)
    n <- length(res_squared)
    n * suppressWarnings(summary(aux_fit)$r.squared)
  }, error = function(e) NULL)

  if (is.null(bp_stat) || !m2_is_finite_scalar(bp_stat)) {
    return(m2_inconclusive_hypothesis(
      label = "Variance is constant (homoscedastic)",
      null = "Variance is non-constant (heteroscedastic)",
      interpretation = "Breusch-Pagan statistic could not be computed"
    ))
  }

  p_value <- 1 - pchisq(bp_stat, 1)
  if (!m2_is_finite_scalar(p_value)) {
    return(m2_inconclusive_hypothesis(
      label = "Variance is constant (homoscedastic)",
      null = "Variance is non-constant (heteroscedastic)",
      interpretation = "Breusch-Pagan test returned a non-finite p-value"
    ))
  }

  result <- if (p_value > 0.05) "fail_to_reject" else "reject"

  list(
    hypothesis = "Variance is constant (homoscedastic)",
    null = "Variance is non-constant (heteroscedastic)",
    result = result,
    BP_statistic = bp_stat,
    p_value = p_value,
    interpretation = sprintf(
      "Breusch-Pagan = %.2f (p = %.4f). %s",
      bp_stat,
      p_value,
      if (p_value > 0.05) "Homoscedasticity assumed." else "Heteroscedasticity detected."
    )
  )
}

test_constant_growth_rate_hypothesis <- function(years, articles) {
  if (length(years) < 10) {
    return(m2_inconclusive_hypothesis(
      label = "Year-over-year growth rate is constant",
      null = "Growth rate varies over time",
      interpretation = "At least 10 annual observations are required"
    ))
  }

  growth_rates <- diff(articles) / articles[-length(articles)]
  growth_rates <- growth_rates[is.finite(growth_rates)]

  if (length(growth_rates) < 5) {
    return(m2_inconclusive_hypothesis(
      label = "Year-over-year growth rate is constant",
      null = "Growth rate varies over time",
      interpretation = "At least 5 finite year-over-year growth values are required"
    ))
  }

  mean_growth <- mean(growth_rates, na.rm = TRUE)
  sd_growth <- sd(growth_rates, na.rm = TRUE)

  if (abs(mean_growth) < .Machine$double.eps) {
    cv <- if (sd_growth < .Machine$double.eps) 0 else Inf
  } else {
    cv <- sd_growth / abs(mean_growth)
  }

  result <- if (is.finite(cv) && cv < 0.5) "fail_to_reject" else "reject"

  list(
    hypothesis = "Year-over-year growth rate is constant",
    null = "Growth rate varies over time",
    result = result,
    mean_growth = mean_growth,
    sd_growth = sd_growth,
    CV = cv,
    interpretation = sprintf(
      "Mean growth = %.1f%%, CV = %.2f. %s",
      mean_growth * 100,
      cv,
      if (!is.finite(cv)) {
        "Growth rate undefined."
      } else if (cv < 0.5) {
        "Growth rate is relatively stable."
      } else {
        "Growth rate varies."
      }
    )
  )
}

test_outlier_hypothesis <- function(years, articles) {
  fit <- m2_safe_lm(articles ~ years)
  if (is.null(fit)) {
    return(m2_inconclusive_hypothesis(
      label = "No significant outliers in production data",
      null = "Outliers present",
      interpretation = "Linear model could not be estimated"
    ))
  }

  std_res <- tryCatch(suppressWarnings(rstandard(fit)), error = function(e) NULL)
  if (is.null(std_res) || !any(is.finite(std_res))) {
    return(m2_inconclusive_hypothesis(
      label = "No significant outliers in production data",
      null = "Outliers present",
      interpretation = "Standardized residuals could not be computed"
    ))
  }

  n_outliers <- sum(abs(std_res) > 2, na.rm = TRUE)
  result <- if (n_outliers == 0) "fail_to_reject" else "reject"

  list(
    hypothesis = "No significant outliers in production data",
    null = "Outliers present",
    result = result,
    n_outliers = n_outliers,
    outlier_years = years[abs(std_res) > 2],
    interpretation = sprintf("%d outlier(s) detected (|std res| > 2).", n_outliers)
  )
}

test_logistic_growth_hypothesis <- function(years, articles) {
  if (length(years) < 8) {
    return(m2_inconclusive_hypothesis(
      label = "Production follows logistic S-curve (saturation)",
      null = "No saturation pattern observed",
      interpretation = "At least 8 annual observations are required"
    ))
  }

  n <- length(articles)
  first_third <- articles[1:floor(n / 3)]
  last_third <- articles[(floor(2 * n / 3) + 1):n]

  growth_first <- (max(first_third) - min(first_third)) / length(first_third)
  growth_last <- (max(last_third) - min(last_third)) / length(last_third)
  if (!m2_is_finite_scalar(growth_first) || !m2_is_finite_scalar(growth_last)) {
    return(m2_inconclusive_hypothesis(
      label = "Production follows logistic S-curve (saturation)",
      null = "No saturation pattern observed",
      interpretation = "Growth segments are not numerically stable"
    ))
  }

  result <- if (growth_first > growth_last * 1.5) "fail_to_reject" else "reject"

  list(
    hypothesis = "Production follows logistic S-curve (saturation)",
    null = "No saturation pattern observed",
    result = result,
    early_growth = growth_first,
    late_growth = growth_last,
    interpretation = sprintf(
      "Early growth = %.1f, Late growth = %.1f. %s",
      growth_first,
      growth_last,
      if (result == "fail_to_reject") "Saturation pattern detected." else "No saturation detected."
    )
  )
}

test_stationarity_hypothesis <- function(years, articles) {
  if (length(articles) < 10) {
    return(m2_inconclusive_hypothesis(
      label = "YoY growth rate is stationary",
      null = "Growth rate has time-varying mean",
      interpretation = "At least 10 annual observations are required"
    ))
  }

  yoy_growth <- diff(articles) / articles[-length(articles)]
  yoy_growth <- yoy_growth[is.finite(yoy_growth)]

  if (length(yoy_growth) < 5) {
    return(m2_inconclusive_hypothesis(
      label = "YoY growth rate is stationary",
      null = "Growth rate has time-varying mean",
      interpretation = "At least 5 finite year-over-year growth values are required"
    ))
  }

  n <- length(yoy_growth)
  first_half <- yoy_growth[1:floor(n / 2)]
  second_half <- yoy_growth[(floor(n / 2) + 1):n]

  t_test <- tryCatch(t.test(first_half, second_half), error = function(e) NULL)
  if (is.null(t_test) || !m2_is_finite_scalar(t_test$p.value)) {
    return(m2_inconclusive_hypothesis(
      label = "YoY growth rate is stationary",
      null = "Growth rate has time-varying mean",
      interpretation = "Stationarity comparison could not be computed"
    ))
  }

  result <- if (t_test$p.value > 0.05) "fail_to_reject" else "reject"

  list(
    hypothesis = "YoY growth rate is stationary",
    null = "Growth rate has time-varying mean",
    result = result,
    t_statistic = t_test$statistic,
    p_value = t_test$p.value,
    interpretation = sprintf(
      "T-test p = %.4f. %s",
      t_test$p.value,
      if (t_test$p.value > 0.05) "Stationarity assumed." else "Non-stationary."
    )
  )
}

test_monotonic_trend_hypothesis <- function(years, articles) {
  if (length(years) < 8) {
    return(m2_inconclusive_hypothesis(
      label = "Annual production shows a monotonic trend",
      null = "No monotonic trend is present",
      interpretation = "At least 8 annual observations are required"
    ))
  }

  kendall_test <- tryCatch(
    stats::cor.test(years, articles, method = "kendall", exact = FALSE),
    error = function(e) NULL
  )

  if (is.null(kendall_test) || !m2_is_finite_scalar(kendall_test$p.value)) {
    return(m2_inconclusive_hypothesis(
      label = "Annual production shows a monotonic trend",
      null = "No monotonic trend is present",
      interpretation = "Kendall trend test could not be computed"
    ))
  }

  tau <- suppressWarnings(as.numeric(kendall_test$estimate))
  result <- if (kendall_test$p.value < 0.05) "reject" else "fail_to_reject"

  list(
    hypothesis = "Annual production shows a monotonic trend",
    null = "No monotonic trend is present",
    test = "Kendall rank correlation",
    evidence_class = "statistical",
    result = result,
    statistic = tau,
    p_value = kendall_test$p.value,
    direction = if (tau > 0) "increasing" else if (tau < 0) "decreasing" else "flat",
    interpretation = sprintf(
      "Kendall tau = %.3f (p = %.4f). %s trend.",
      tau,
      kendall_test$p.value,
      if (abs(tau) < 1e-9) "Flat" else if (tau > 0) "Increasing" else "Decreasing"
    )
  )
}

test_sen_slope_hypothesis <- function(years, articles) {
  if (length(years) < 8) {
    return(m2_inconclusive_hypothesis(
      label = "The robust annual slope is positive",
      null = "The robust annual slope is not positive",
      interpretation = "At least 8 annual observations are required"
    ))
  }

  slopes <- m2_pairwise_slopes(years, articles)
  if (length(slopes) < 5) {
    return(m2_inconclusive_hypothesis(
      label = "The robust annual slope is positive",
      null = "The robust annual slope is not positive",
      interpretation = "Insufficient finite pairwise slopes for Sen slope estimation"
    ))
  }

  sen_slope <- stats::median(slopes, na.rm = TRUE)
  ci <- stats::quantile(slopes, probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE)
  result <- if (ci[1] > 0) "reject" else "fail_to_reject"

  list(
    hypothesis = "The robust annual slope is positive",
    null = "The robust annual slope is not positive",
    test = "Theil-Sen slope interval",
    evidence_class = "statistical",
    result = result,
    sen_slope = sen_slope,
    ci_lower = ci[1],
    ci_upper = ci[2],
    interpretation = sprintf(
      "Sen slope = %.3f articles/year with empirical 95%% interval [%.3f, %.3f].",
      sen_slope,
      ci[1],
      ci[2]
    )
  )
}

test_acceleration_hypothesis <- function(years, articles) {
  if (length(years) < 8) {
    return(m2_inconclusive_hypothesis(
      label = "Production trend is accelerating/decelerating",
      null = "Trend follows constant acceleration (quadratic = 0)",
      interpretation = "At least 8 annual observations are required"
    ))
  }

  fit_quad <- m2_safe_lm(articles ~ years + I(years^2))
  if (is.null(fit_quad)) {
    return(m2_inconclusive_hypothesis(
      label = "Production trend is accelerating/decelerating",
      null = "Trend follows constant acceleration (quadratic = 0)",
      interpretation = "Quadratic model could not be estimated"
    ))
  }

  fit_summary <- m2_safe_summary(fit_quad)
  quad_coef <- coef(fit_quad)[3]
  p_value <- if (!is.null(fit_summary) && nrow(fit_summary$coefficients) >= 3) fit_summary$coefficients[3, 4] else NA_real_
  if (!m2_is_finite_scalar(quad_coef) || !m2_is_finite_scalar(p_value)) {
    return(m2_inconclusive_hypothesis(
      label = "Production trend is accelerating/decelerating",
      null = "Trend follows constant acceleration (quadratic = 0)",
      interpretation = "Quadratic trend test returned non-finite statistics"
    ))
  }

  direction <- if (quad_coef > 0) "accelerating" else if (quad_coef < 0) "decelerating" else "constant"
  result <- if (p_value > 0.05) "fail_to_reject" else "reject"

  list(
    hypothesis = "Production trend is accelerating/decelerating",
    null = "Trend follows constant acceleration (quadratic = 0)",
    result = result,
    quadratic_coefficient = quad_coef,
    p_value = p_value,
    direction = direction,
    interpretation = sprintf(
      "Quadratic coeff = %.4f (p = %.4f). Trend is %s.",
      quad_coef,
      p_value,
      direction
    )
  )
}

summarize_hypothesis_results <- function(hypotheses) {
  n_total <- length(hypotheses)
  if (n_total == 0) {
    return(list(
      n_total = 0L,
      n_rejected = 0L,
      n_failed_to_reject = 0L,
      rejection_rate = NA_real_
    ))
  }

  n_rejected <- sum(vapply(hypotheses, function(h) identical(h$result, "reject"), logical(1)), na.rm = TRUE)
  n_failed <- sum(vapply(hypotheses, function(h) identical(h$result, "fail_to_reject"), logical(1)), na.rm = TRUE)

  list(
    n_total = n_total,
    n_rejected = n_rejected,
    n_failed_to_reject = n_failed,
    rejection_rate = n_rejected / n_total
  )
}

m2_inconclusive_hypothesis <- function(label, null = NULL, interpretation = "Insufficient data") {
  list(
    hypothesis = label,
    null = null,
    result = "inconclusive",
    interpretation = interpretation
  )
}

m2_standardize_hypotheses <- function(hypotheses) {
  standardized <- Map(
    f = function(result, id) m2_standardize_hypothesis_result(result, fallback_label = id),
    result = hypotheses,
    id = names(hypotheses)
  )
  names(standardized) <- names(hypotheses)
  standardized
}

m2_standardize_hypothesis_result <- function(result, fallback_label) {
  if (!is.list(result)) {
    return(m2_inconclusive_hypothesis(
      label = fallback_label,
      interpretation = "Invalid hypothesis result structure"
    ))
  }

  label <- result$hypothesis %||% result$hyphypothesis %||% fallback_label
  result$hypothesis <- label
  result$hyphypothesis <- label
  result$result <- result$result %||% "inconclusive"
  result$test <- result$test %||% if (!is.null(result$p_value)) "statistical test" else "heuristic diagnostic"
  result$evidence_class <- result$evidence_class %||% if (!is.null(result$p_value)) "statistical" else "heuristic"
  result
}

m2_safe_lm <- function(formula) {
  tryCatch(lm(formula), error = function(e) NULL)
}

m2_safe_summary <- function(fit) {
  tryCatch(suppressWarnings(summary(fit)), error = function(e) NULL)
}

m2_is_finite_scalar <- function(x) {
  is.numeric(x) && length(x) == 1 && is.finite(x)
}

m2_pairwise_slopes <- function(years, articles) {
  years <- suppressWarnings(as.numeric(years))
  articles <- suppressWarnings(as.numeric(articles))
  n <- min(length(years), length(articles))
  if (n < 2) {
    return(numeric())
  }

  slopes <- c()
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      delta_x <- years[j] - years[i]
      if (!is.finite(delta_x) || abs(delta_x) <= .Machine$double.eps) {
        next
      }
      slope <- (articles[j] - articles[i]) / delta_x
      if (is.finite(slope)) {
        slopes <- c(slopes, slope)
      }
    }
  }
  slopes
}

m2_pettitt_test <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3) {
    return(list(statistic = NA_real_, p_value = NA_real_, index = NA_integer_))
  }

  u_values <- numeric(n)
  for (t in seq_len(n)) {
    left <- x[seq_len(t)]
    right <- if (t < n) x[(t + 1L):n] else numeric()
    if (length(right) == 0) {
      u_values[t] <- 0
    } else {
      signs <- outer(left, right, FUN = function(a, b) sign(a - b))
      u_values[t] <- sum(signs, na.rm = TRUE)
    }
  }

  k_stat <- max(abs(u_values), na.rm = TRUE)
  idx <- which.max(abs(u_values))
  p_value <- 2 * exp((-6 * k_stat^2) / (n^3 + n^2))
  p_value <- min(1, max(0, p_value))

  list(statistic = k_stat, p_value = p_value, index = idx)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
