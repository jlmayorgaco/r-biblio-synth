# ============================================================================
# zz_m1_hypothesis_overrides.R - Runtime-safe overrides for M1 hypotheses
# ============================================================================

test_collaboration_trend_hypothesis <- function(input, config) {
  collab_result <- tryCatch({
    compute_m1_collaboration(input, config)
  }, error = function(e) NULL)

  if (is.null(collab_result) || collab_result$status != "success") {
    return(list(
      hyphypothesis = "Collaboration rate has increased over time",
      null = "Collaboration rate is stable or decreasing",
      result = "inconclusive",
      interpretation = "Could not compute collaboration trends"
    ))
  }

  collaboration_by_year <- collab_result$by_year
  if (is.null(collaboration_by_year) || nrow(collaboration_by_year) < 5) {
    return(list(
      hyphypothesis = "Collaboration rate has increased over time",
      null = "Collaboration rate is stable or decreasing",
      result = "inconclusive",
      interpretation = "Insufficient temporal data"
    ))
  }

  if (!"collaboration_rate" %in% names(collaboration_by_year)) {
    if ("mcp_ratio" %in% names(collaboration_by_year)) {
      collaboration_by_year$collaboration_rate <- collaboration_by_year$mcp_ratio
    } else {
      return(list(
        hyphypothesis = "Collaboration rate has increased over time",
        null = "Collaboration rate is stable or decreasing",
        result = "inconclusive",
        interpretation = "Collaboration rate data not available"
      ))
    }
  }

  if (nrow(collaboration_by_year) < 3) {
    return(list(
      hyphypothesis = "Collaboration rate has increased over time",
      null = "Collaboration rate is stable or decreasing",
      result = "inconclusive",
      interpretation = "Insufficient data points for trend analysis (need at least 3 years)"
    ))
  }

  fit <- tryCatch(stats::lm(collaboration_rate ~ year, data = collaboration_by_year), error = function(e) NULL)
  if (is.null(fit) || length(stats::coef(fit)) < 2 || !is.finite(stats::coef(fit)[2])) {
    return(list(
      hyphypothesis = "Collaboration rate has increased over time",
      null = "Collaboration rate is stable or decreasing",
      result = "inconclusive",
      interpretation = "Could not fit regression model"
    ))
  }

  slope <- stats::coef(fit)[2]
  p_value <- suppressWarnings(summary(fit)$coefficients[2, 4])
  slope <- slope[1]
  p_value <- p_value[1]

  if (!is.finite(slope) || !is.finite(p_value)) {
    return(list(
      hyphypothesis = "Collaboration rate has increased over time",
      null = "Collaboration rate is stable or decreasing",
      result = "inconclusive",
      slope = slope,
      p_value = p_value,
      interpretation = "Collaboration trend statistics were not estimable from the available yearly profile."
    ))
  }

  result <- if (slope > 0 && p_value < 0.05) {
    "fail_to_reject"
  } else if (slope <= 0 && p_value < 0.05) {
    "reject"
  } else {
    "fail_to_reject"
  }

  interpretation <- sprintf(
    "Collaboration trend slope = %.4f articles/year (p = %.4f). %s",
    slope,
    p_value,
    if (slope > 0) "Increasing trend detected." else "No significant increasing trend."
  )

  list(
    hyphypothesis = "Collaboration rate has increased over time",
    null = "Collaboration rate is stable or decreasing",
    result = result,
    slope = slope,
    p_value = p_value,
    interpretation = interpretation
  )
}
