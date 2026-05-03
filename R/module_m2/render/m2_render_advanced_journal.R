# ============================================================================
# m2_render_advanced_journal.R - Advanced M2 journal plots
# ============================================================================

render_m2_advanced_journal <- function(result, config = biblio_config()) {
  if (!is.list(result) || !identical(result$status %||% "", "success")) {
    return(list(status = result$status %||% "stub", plots = list(), reason = result$reason %||% "Advanced M2 analytics unavailable."))
  }

  plots <- list()

  uncertainty <- result$model_uncertainty$table %||% tibble::tibble()
  if (is.data.frame(uncertainty) && nrow(uncertainty) > 0) {
    plots$model_uncertainty_forest <- m2_plot_model_uncertainty(uncertainty, config)
  }

  regimes <- result$growth_regimes
  if (is.list(regimes) && is.data.frame(regimes$curve) && nrow(regimes$curve) > 0) {
    plots$growth_regime <- m2_plot_growth_regime(regimes, config)
    plots$residual_diagnostics_panel <- m2_plot_advanced_residuals(regimes, config)
  }

  leaderboard <- result$forecast_validation$leaderboard %||% tibble::tibble()
  if (is.data.frame(leaderboard) && nrow(leaderboard) > 0) {
    plots$forecast_backtesting_heatmap <- m2_plot_forecast_heatmap(leaderboard, config)
  }

  calibration <- result$forecast_validation$interval_calibration %||% tibble::tibble()
  if (is.data.frame(calibration) && nrow(calibration) > 0) {
    plots$prediction_interval_calibration <- m2_plot_interval_calibration(calibration, config)
  }

  consensus <- result$changepoint_consensus$table %||% tibble::tibble()
  if (is.data.frame(consensus) && nrow(consensus) > 0) {
    plots$changepoint_consensus_timeline <- m2_plot_changepoint_consensus(consensus, config)
  }

  intervals <- result$bootstrap_forecast_intervals$table %||% tibble::tibble()
  if (is.data.frame(intervals) && nrow(intervals) > 0) {
    plots$ensemble_forecast_intervals <- m2_plot_ensemble_intervals(intervals, config)
  }

  mcs <- result$model_confidence_set$table %||% tibble::tibble()
  if (is.data.frame(mcs) && nrow(mcs) > 0) {
    plots$model_confidence_set <- m2_plot_model_confidence_set(mcs, config)
  }

  segmented <- result$segmented_regression$table %||% tibble::tibble()
  if (is.data.frame(segmented) && nrow(segmented) > 0) {
    plots$segmented_regression_screen <- m2_plot_segmented_regression(segmented, config)
  }

  early <- result$early_warning$table %||% tibble::tibble()
  if (is.data.frame(early) && nrow(early) > 0) {
    plots$early_warning_acceleration <- m2_plot_early_warning(early, config)
  }

  list(
    status = if (length(plots) > 0) "success" else "stub",
    plots = plots
  )
}

m2_plot_model_uncertainty <- function(uncertainty, config) {
  df <- uncertainty |>
    dplyr::filter(is.finite(.data$estimate), is.finite(.data$ci_low), is.finite(.data$ci_high)) |>
    dplyr::mutate(label = paste(.data$model, .data$parameter, sep = ": "))
  if (nrow(df) == 0) {
    return(ieee_no_data_plot("Model uncertainty unavailable", "No finite parameter intervals were available.", layout = "full"))
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$estimate, y = stats::reorder(.data$label, .data$estimate))) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$ci_low, xmax = .data$ci_high), height = 0.22, linewidth = 0.35, color = "#333333") +
    ggplot2::geom_point(ggplot2::aes(color = .data$model), size = 1.9) +
    ggplot2::scale_color_manual(values = get_ieee_palette(length(unique(df$model)))) +
    ggplot2::labs(
      title = "Parametric Growth-Model Uncertainty",
      subtitle = "Bootstrap-style parameter intervals for interpretable growth curves.",
      x = "Parameter estimate with 95% interval",
      y = NULL,
      color = "Model",
      caption = "Intervals are fail-soft residual-bootstrap delta summaries; use larger bootstrap_n for final journal runs."
    ) +
    ieee_theme_wide(base_size = 8.5)
  ieee_mark_plot_layout(p, "full")
}

m2_plot_ensemble_intervals <- function(intervals, config) {
  df <- intervals |>
    dplyr::filter(is.finite(.data$forecast), is.finite(.data$pi95_low), is.finite(.data$pi95_high))
  if (nrow(df) == 0) {
    return(ieee_no_data_plot("Ensemble intervals unavailable", "No finite bootstrap forecast intervals were available.", layout = "full"))
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$target_year, y = .data$forecast)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$pi95_low, ymax = .data$pi95_high), fill = "#0072BD", alpha = 0.12) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$pi80_low, ymax = .data$pi80_high), fill = "#0072BD", alpha = 0.22) +
    ggplot2::geom_line(color = "#0072BD", linewidth = 0.8) +
    ggplot2::geom_point(color = "#111111", size = 1.8) +
    ggplot2::labs(
      title = "Ensemble Forecast with Bootstrap Intervals",
      subtitle = "Inverse-MASE ensemble with validation-residual uncertainty.",
      x = "Forecast year",
      y = "Articles",
      caption = "Dark band: 80% interval; light band: 95% interval."
    ) +
    ieee_theme_wide(base_size = 8.5)
  ieee_mark_plot_layout(p, "full")
}

m2_plot_model_confidence_set <- function(mcs, config) {
  df <- mcs |>
    dplyr::filter(is.finite(.data$mean_mase)) |>
    dplyr::arrange(.data$mean_mase)
  if (nrow(df) == 0) {
    return(ieee_no_data_plot("Model confidence set unavailable", "No finite model accuracy scores were available.", layout = "single"))
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$mean_mase, y = stats::reorder(.data$model, -.data$mean_mase), fill = .data$in_confidence_set)) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "#77AC30", `FALSE` = "#BDBDBD"), labels = c(`TRUE` = "Inside", `FALSE` = "Outside")) +
    ggplot2::labs(
      title = "Forecast Model Confidence Set",
      subtitle = "Models within tolerance of the best MASE remain defensible for forecasting claims.",
      x = "Mean MASE",
      y = NULL,
      fill = "Set"
    ) +
    ieee_theme(base_size = 8.5)
  ieee_mark_plot_layout(p, "single")
}

m2_plot_segmented_regression <- function(segmented, config) {
  df <- segmented |>
    dplyr::filter(is.finite(.data$breakpoint_year), is.finite(.data$aic)) |>
    dplyr::arrange(.data$aic) |>
    utils::head(15)
  if (nrow(df) == 0) {
    return(ieee_no_data_plot("Segmented regression unavailable", "No candidate breakpoint model converged.", layout = "full"))
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$breakpoint_year, y = .data$aic)) +
    ggplot2::geom_line(color = "#333333", linewidth = 0.45) +
    ggplot2::geom_point(ggplot2::aes(fill = .data$best), shape = 21, size = 2.4, color = "#111111") +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "#D95319", `FALSE` = "#BDBDBD"), guide = "none") +
    ggplot2::labs(
      title = "Formal Segmented-Regression Break Screen",
      subtitle = "Lower AIC identifies the most defensible single-break year.",
      x = "Candidate breakpoint year",
      y = "AIC"
    ) +
    ieee_theme_wide(base_size = 8.5)
  ieee_mark_plot_layout(p, "full")
}

m2_plot_early_warning <- function(early, config) {
  df <- early |>
    tidyr::pivot_longer(c("mean_acceleration", "velocity_slope"), names_to = "indicator", values_to = "value")
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$indicator, y = .data$value, fill = .data$value > 0)) +
    ggplot2::geom_hline(yintercept = 0, color = "#333333", linewidth = 0.35) +
    ggplot2::geom_col(width = 0.55) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "#77AC30", `FALSE` = "#BDBDBD"), guide = "none") +
    ggplot2::labs(
      title = "Early-Warning Acceleration Screen",
      subtitle = "Recent derivative indicators flag whether growth is still accelerating.",
      x = NULL,
      y = "Derivative indicator"
    ) +
    ieee_theme(base_size = 8.5)
  ieee_mark_plot_layout(p, "single")
}

m2_plot_growth_regime <- function(regimes, config) {
  curve <- regimes$curve
  summary <- regimes$summary
  inflection <- suppressWarnings(as.numeric(summary$inflection_year[1] %||% NA_real_))
  peak_growth <- suppressWarnings(as.numeric(summary$peak_growth_year[1] %||% NA_real_))
  capacity <- suppressWarnings(as.numeric(summary$capacity[1] %||% NA_real_))

  p <- ggplot2::ggplot(curve, ggplot2::aes(x = .data$year)) +
    ggplot2::geom_point(ggplot2::aes(y = .data$cumulative), size = 1.5, color = "#111111") +
    ggplot2::geom_line(ggplot2::aes(y = .data$fitted_cumulative), linewidth = 0.85, color = "#0072BD") +
    ggplot2::geom_line(ggplot2::aes(y = .data$fitted_increment), linewidth = 0.55, color = "#D95319", linetype = "dashed") +
    ggplot2::labs(
      title = "Growth Regimes and Saturation Narrative",
      subtitle = "Cumulative production, fitted carrying-capacity curve, and annual fitted growth rate.",
      x = "Year",
      y = "Articles / cumulative articles",
      caption = "Blue line: fitted cumulative trajectory. Orange dashed line: fitted annual growth increment."
    ) +
    ieee_theme_wide(base_size = 8.5)

  if (is.finite(capacity)) {
    p <- p + ggplot2::geom_hline(yintercept = capacity, linetype = "longdash", color = "#77AC30", linewidth = 0.45)
  }
  if (is.finite(inflection)) {
    p <- p + ggplot2::geom_vline(xintercept = inflection, linetype = "dashed", color = "#0072BD", linewidth = 0.45)
  }
  if (is.finite(peak_growth)) {
    p <- p + ggplot2::geom_vline(xintercept = peak_growth, linetype = "dotted", color = "#D95319", linewidth = 0.45)
  }

  ieee_mark_plot_layout(p, "full")
}

m2_plot_forecast_heatmap <- function(leaderboard, config) {
  df <- leaderboard |>
    dplyr::filter(is.finite(.data$mase)) |>
    dplyr::mutate(horizon = as.factor(.data$horizon))
  if (nrow(df) == 0) {
    return(ieee_no_data_plot("Forecast validation unavailable", "Rolling-origin validation did not produce finite MASE scores.", layout = "full"))
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$horizon, y = .data$model, fill = .data$mase)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$mase)), size = 2.6, color = "#111111") +
    ggplot2::scale_fill_gradient(low = "#F7FBFF", high = "#084594", name = "MASE") +
    ggplot2::labs(
      title = "Rolling-Origin Forecast Leaderboard",
      subtitle = "Lower MASE indicates better accuracy; MASE < 1 beats a naive baseline.",
      x = "Forecast horizon",
      y = NULL,
      caption = "Backtesting uses expanding-window rolling-origin validation."
    ) +
    ieee_theme_wide(base_size = 8.5)
  ieee_mark_plot_layout(p, "full")
}

m2_plot_interval_calibration <- function(calibration, config) {
  df <- calibration |>
    dplyr::filter(is.finite(.data$coverage), is.finite(.data$nominal)) |>
    dplyr::mutate(nominal_label = paste0(round(.data$nominal * 100), "%"))
  if (nrow(df) == 0) {
    return(ieee_no_data_plot("Interval calibration unavailable", "No finite coverage estimates were produced.", layout = "full"))
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$nominal_label)) +
    ggplot2::geom_col(ggplot2::aes(y = .data$coverage), fill = "#0072BD", width = 0.55) +
    ggplot2::geom_point(ggplot2::aes(y = .data$nominal), color = "#D95319", size = 2.2) +
    ggplot2::geom_text(ggplot2::aes(y = .data$coverage, label = sprintf("%.1f%%", 100 * .data$coverage)), vjust = -0.35, size = 2.8) +
    ggplot2::coord_cartesian(ylim = c(0, 1.05)) +
    ggplot2::labs(
      title = "Prediction Interval Calibration",
      subtitle = "Observed coverage should track the nominal interval level.",
      x = "Nominal interval",
      y = "Observed coverage",
      caption = "Bars show observed coverage; orange points show nominal targets."
    ) +
    ieee_theme_wide(base_size = 8.5)
  ieee_mark_plot_layout(p, "full")
}

m2_plot_changepoint_consensus <- function(consensus, config) {
  df <- consensus |>
    dplyr::filter(is.finite(.data$breakpoint_year), is.finite(.data$support_count))
  if (nrow(df) == 0) {
    return(ieee_no_data_plot("Changepoint consensus unavailable", "No finite consensus breakpoint was detected.", layout = "full"))
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$breakpoint_year, y = .data$support_count)) +
    ggplot2::geom_segment(ggplot2::aes(xend = .data$breakpoint_year, y = 0, yend = .data$support_count), linewidth = 0.45, color = "#333333") +
    ggplot2::geom_point(ggplot2::aes(fill = .data$decision), shape = 21, size = 3.2, color = "#111111") +
    ggplot2::scale_fill_manual(values = c(supported = "#77AC30", suggestive = "#EDB120", inconclusive = "#BDBDBD"), drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = sort(unique(df$breakpoint_year))) +
    ggplot2::labs(
      title = "Changepoint Consensus Timeline",
      subtitle = "Break years with support across independent detection methods.",
      x = "Candidate break year",
      y = "Methods supporting",
      fill = "Decision",
      caption = "Consensus is stronger when multiple independent methods support the same year."
    ) +
    ieee_theme_wide(base_size = 8.5)
  ieee_mark_plot_layout(p, "full")
}

m2_plot_advanced_residuals <- function(regimes, config) {
  curve <- regimes$curve |>
    dplyr::mutate(residual = .data$cumulative - .data$fitted_cumulative)
  if (nrow(curve) == 0 || all(!is.finite(curve$residual))) {
    return(ieee_no_data_plot("Residual diagnostics unavailable", "The headline growth curve did not expose finite residuals.", layout = "full"))
  }

  p <- ggplot2::ggplot(curve, ggplot2::aes(x = .data$year, y = .data$residual)) +
    ggplot2::geom_hline(yintercept = 0, color = "#333333", linewidth = 0.35) +
    ggplot2::geom_line(linewidth = 0.55, color = "#111111") +
    ggplot2::geom_point(size = 1.7, color = "#111111") +
    ggplot2::labs(
      title = "Compact Residual Diagnostics",
      subtitle = "Residual structure after the headline parametric growth model.",
      x = "Year",
      y = "Cumulative residual",
      caption = "A journal-ready model should avoid persistent residual regimes or late extreme drift."
    ) +
    ieee_theme_wide(base_size = 8.5)
  ieee_mark_plot_layout(p, "full")
}
