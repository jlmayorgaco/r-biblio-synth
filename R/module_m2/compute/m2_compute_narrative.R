# ============================================================================
# m2_compute_narrative.R - M2 narrative evidence metrics
# ============================================================================

compute_m2_narrative <- function(input, data, config = biblio_config()) {
  trend <- data$diagnostics$trend_statistics %||% list()
  mk <- trend$mann_kendall %||% list()
  forecast_comparison <- data$forecasting$model_comparison$comparison %||% data.frame()

  best_r2 <- ieee_pick_num(
    data$regression,
    list(c("best_model", "R2"), c("best_model", "r_squared"), c("best_model", "metrics", "R2"))
  )
  cagr <- ieee_safe_num(trend$cagr)
  recent_cagr <- ieee_safe_num(trend$recent_cagr)
  volatility <- ieee_safe_num(trend$volatility)
  hurst <- ieee_safe_num(trend$hurst_exponent)
  breakpoint_count <- ieee_safe_num(trend$n_breakpoints, 0)
  mk_p <- ieee_safe_num(mk$p_value)
  mk_tau <- ieee_safe_num(mk$tau)

  forecast_error <- NA_real_
  if (is.data.frame(forecast_comparison) && nrow(forecast_comparison) > 0) {
    err_col <- intersect(c("CV_RMSE", "RMSE", "CV_MAE", "MAE"), names(forecast_comparison))[1]
    if (!is.na(err_col)) {
      forecast_error <- min(suppressWarnings(as.numeric(forecast_comparison[[err_col]])), na.rm = TRUE)
    }
  }
  mean_articles <- if (is.data.frame(input) && "Articles" %in% names(input)) {
    mean(suppressWarnings(as.numeric(input$Articles)), na.rm = TRUE)
  } else {
    NA_real_
  }
  forecast_score <- if (is.finite(forecast_error) && is.finite(mean_articles) && mean_articles > 0) {
    1 / (1 + forecast_error / mean_articles)
  } else {
    NA_real_
  }

  metrics <- ieee_bind_metric_rows(
    ieee_metric_row(
      "M2", "Growth", "Overall CAGR",
      cagr * 100,
      score = min(abs(cagr) * 5, 1),
      units = "%",
      digits = 2,
      interpretation = "Quantifies the long-run annual expansion or contraction of publication volume."
    ),
    ieee_metric_row(
      "M2", "Growth", "Recent CAGR",
      recent_cagr * 100,
      score = min(abs(recent_cagr) * 5, 1),
      units = "%",
      digits = 2,
      interpretation = "Shows whether the most recent years are accelerating or cooling relative to the full series."
    ),
    ieee_metric_row(
      "M2", "Model Evidence", "Best interpretable R-squared",
      best_r2,
      score = best_r2,
      digits = 3,
      interpretation = "Measures how tightly the selected growth model explains annual production."
    ),
    ieee_metric_row(
      "M2", "Model Evidence", "Forecast reliability",
      forecast_score,
      score = forecast_score,
      digits = 3,
      interpretation = "Normalizes out-of-sample error against mean annual production; higher is more reliable."
    ),
    ieee_metric_row(
      "M2", "Trend Tests", "Mann-Kendall tau",
      mk_tau,
      score = abs(mk_tau),
      digits = 3,
      interpretation = if (is.finite(mk_p)) sprintf("Trend-test p-value: %.4f.", mk_p) else "Nonparametric trend evidence was not estimable."
    ),
    ieee_metric_row(
      "M2", "Dynamics", "Hurst persistence",
      hurst,
      score = abs(hurst - 0.5) * 2,
      digits = 3,
      interpretation = "Values far from 0.5 indicate persistence or anti-persistence in annual production."
    ),
    ieee_metric_row(
      "M2", "Dynamics", "Growth volatility",
      volatility * 100,
      score = min(abs(volatility) * 3, 1),
      units = "%",
      digits = 2,
      interpretation = "Shows how unstable year-to-year growth rates are."
    ),
    ieee_metric_row(
      "M2", "Structural Change", "Detected breakpoints",
      breakpoint_count,
      score = min(breakpoint_count / 3, 1),
      digits = 0,
      interpretation = "Counts retained structural changes in the annual production trajectory."
    )
  )

  list(
    status = if (nrow(metrics) > 0) "success" else "empty",
    metrics = metrics,
    narrative = ieee_narrative_lines(metrics, max_lines = 8)
  )
}
