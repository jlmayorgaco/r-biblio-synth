# ============================================================================
# zz_m3_hypothesis_overrides.R - Runtime-safe overrides for M3 hypotheses
# ============================================================================

test_concentration_stability_hypothesis <- function(prepared_data, config) {
  gini_over_time <- prepared_data$gini_over_time

  if (is.null(gini_over_time) || length(gini_over_time) < 5) {
    return(list(
      hyphypothesis = "Production concentration is stable over time",
      null = "Concentration is changing",
      result = "inconclusive",
      interpretation = "Insufficient temporal data"
    ))
  }

  gini_values <- unlist(gini_over_time, use.names = FALSE)
  if (length(gini_values) < 5) {
    return(list(
      hyphypothesis = "Production concentration is stable over time",
      null = "Concentration is changing",
      result = "inconclusive",
      interpretation = "Insufficient temporal data"
    ))
  }

  years <- seq_along(gini_values)
  fit <- tryCatch(stats::lm(gini_values ~ years), error = function(e) NULL)
  if (is.null(fit) || length(stats::coef(fit)) < 2 || !is.finite(stats::coef(fit)[2])) {
    return(list(
      hyphypothesis = "Production concentration is stable over time",
      null = "Concentration is changing",
      result = "inconclusive",
      interpretation = "Concentration trend could not be estimated."
    ))
  }

  slope <- stats::coef(fit)[2]
  p_value <- suppressWarnings(summary(fit)$coefficients[2, 4])
  slope <- slope[1]
  p_value <- p_value[1]

  if (!is.finite(slope) || !is.finite(p_value)) {
    return(list(
      hyphypothesis = "Production concentration is stable over time",
      null = "Concentration is changing",
      result = "inconclusive",
      slope = slope,
      p_value = p_value,
      interpretation = "Concentration trend statistics were not estimable from the available yearly profile."
    ))
  }

  result <- if (abs(slope) < 0.01 && p_value > 0.05) "fail_to_reject" else "reject"

  list(
    hyphypothesis = "Production concentration is stable over time",
    null = "Concentration is changing",
    result = result,
    slope = slope,
    p_value = p_value,
    interpretation = sprintf(
      "Gini trend slope = %.4f/year (p = %.4f). %s",
      slope,
      p_value,
      if (result == "fail_to_reject") "Concentration is stable." else "Concentration is changing."
    )
  )
}
