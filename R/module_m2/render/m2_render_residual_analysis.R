# ============================================================================
# m2_render_residual_analysis.R - Render Residual Diagnostics
# ============================================================================
# Creates plots for residual analysis including:
# - Residual time series
# - Normality tests
# - Breakpoint detection
# - Harmonic analysis on residuals

#' Render residual analysis plots
#'
#' @param data Output from compute_m2_residual_analysis
#' @param config Configuration list
#' @return List with plots
#' @export
render_m2_residual_analysis <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(list(plots = list(), status = "error: invalid data"))
  }
  
  plots <- list()
  
  # Residual time series
  plots$residual_series <- create_residual_series_plot(data, config)
  
  # Normality plots
  if (!is.null(data$normality)) {
    plots$normality_diagnostic <- create_normality_diagnostic_plot(data, config)
  }
  
  # Breakpoint visualization
  if (!is.null(data$breakpoints)) {
    plots$breakpoint_detection <- create_breakpoint_plot(data, config)
  }
  
  # Harmonic analysis on residuals
  if (!is.null(data$harmonics)) {
    plots$residual_spectrum <- create_residual_spectrum_plot(data, config)
  }
  
  # ACF/PACF
  if (!is.null(data$autocorrelation)) {
    plots$autocorrelation <- create_autocorrelation_plot(data, config)
  }
  
  # Model adequacy summary
  if (!is.null(data$model_adequacy)) {
    plots$model_adequacy <- create_model_adequacy_plot(data, config)
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create residual time series plot
#' @keywords internal
create_residual_series_plot <- function(data, config) {
  df <- data.frame(
    Year = data$years,
    Residual = data$residuals,
    Predicted = data$predicted,
    Observed = data$observed
  )
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Year)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
    ggplot2::geom_line(ggplot2::aes(y = Residual), color = "#0072BD", linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(y = Residual), color = "#0072BD", size = 1.5) +
    ggplot2::geom_smooth(ggplot2::aes(y = Residual), method = "loess", 
                        se = FALSE, color = "#D95319", linetype = "dashed", linewidth = 0.5) +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_y_continuous(name = "Residual") +
    ieee_theme_timeseries() +
    ggplot2::labs(
      title = "Residual Time Series",
      subtitle = sprintf("Mean: %.2f, SD: %.2f", mean(data$residuals), sd(data$residuals))
    )
  
  # Mark breakpoints if detected
  if (!is.null(data$breakpoints) && data$breakpoints$has_breakpoints) {
    for (bp_year in data$breakpoints$breakpoint_years) {
      p <- p + ggplot2::geom_vline(xintercept = bp_year, color = "#A2142F", 
                                   linetype = "dotted", linewidth = 0.5)
    }
  }
  
  p
}

#' Create normality diagnostic plot
#' @keywords internal
create_normality_diagnostic_plot <- function(data, config) {
  residuals <- data$residuals
  
  # Create a combined plot
  # Left: Histogram + density
  # Right: Q-Q plot
  
  df_hist <- data.frame(Residual = residuals)
  
  hist_plot <- ggplot2::ggplot(df_hist, ggplot2::aes(x = Residual)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                           bins = 20, fill = "#0072BD", color = "black", linewidth = 0.2, alpha = 0.7) +
    ggplot2::geom_density(color = "#D95319", linewidth = 0.8) +
    ggplot2::stat_function(fun = dnorm, args = list(mean = mean(residuals), sd = sd(residuals)),
                          color = "#77AC30", linetype = "dashed", linewidth = 0.6) +
    ggplot2::scale_x_continuous(name = "Residual") +
    ggplot2::scale_y_continuous(name = "Density") +
    ieee_theme() +
    ggplot2::labs(title = "Residual Distribution")
  
  # Q-Q plot
  qq_data <- data.frame(
    theoretical = qqnorm(residuals, plot.it = FALSE)$x,
    sample = qqnorm(residuals, plot.it = FALSE)$y
  )
  
  qq_plot <- ggplot2::ggplot(qq_data, ggplot2::aes(x = theoretical, y = sample)) +
    ggplot2::geom_point(color = "#0072BD", size = 1.5) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "#D95319", linetype = "dashed") +
    ggplot2::scale_x_continuous(name = "Theoretical Quantiles") +
    ggplot2::scale_y_continuous(name = "Sample Quantiles") +
    ieee_theme() +
    ggplot2::labs(title = "Q-Q Plot")
  
  # Normality annotation
  norm_data <- data$normality
  annotation <- sprintf(
    "Shapiro-Wilk: p = %.4f\nAnderson-Darling: p = %.4f\nNormal: %s",
    norm_data$shapiro_wilk$p_value %||% NA,
    norm_data$anderson_darling$p_value %||% NA,
    if (norm_data$overall_normal) "Yes" else "No"
  )
  
  hist_plot <- hist_plot + ggplot2::annotate(
    "text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
    label = annotation, size = 2.5, fontface = "italic"
  )
  
  list(
    histogram = hist_plot,
    qq_plot = qq_plot
  )
}

#' Create breakpoint detection plot
#' @keywords internal
create_breakpoint_plot <- function(data, config) {
  residuals <- data$residuals
  years <- data$years
  
  # CUSUM plot
  cumulative <- cumsum(residuals - mean(residuals))
  
  cusum_df <- data.frame(
    Year = years,
    CUSUM = cumulative
  )
  
  n <- length(residuals)
  threshold <- data$breakpoints$cusum$threshold %||% (1.143 * sqrt(n) * sd(residuals))
  
  p <- ggplot2::ggplot(cusum_df, ggplot2::aes(x = Year, y = CUSUM)) +
    ggplot2::geom_line(color = "#0072BD", linewidth = 0.8) +
    ggplot2::geom_point(color = "#0072BD", size = 1.5) +
    ggplot2::geom_hline(yintercept = c(threshold, -threshold), 
                        color = "#A2142F", linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = 0, color = "gray50", linewidth = 0.3) +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_y_continuous(name = "CUSUM") +
    ieee_theme_timeseries() +
    ggplot2::labs(
      title = "CUSUM Breakpoint Detection",
      subtitle = sprintf("Breakpoints detected: %s", 
                        if (data$breakpoints$has_breakpoints) 
                          paste(data$breakpoints$breakpoint_years, collapse = ", ")
                        else "None")
    )
  
  p
}

#' Create residual spectrum plot
#' @keywords internal
create_residual_spectrum_plot <- function(data, config) {
  if (is.null(data$harmonics) || is.null(data$harmonics$spectrum)) {
    return(NULL)
  }
  
  spec <- data$harmonics$spectrum
  
  # Filter to meaningful periods
  spec <- spec[spec$frequency > 0 & spec$frequency < 0.5 & spec$period < 50, ]
  
  p <- ggplot2::ggplot(spec, ggplot2::aes(x = period, y = normalized_power)) +
    ggplot2::geom_line(color = "#0072BD", linewidth = 0.8) +
    ggplot2::geom_point(color = "#0072BD", size = 1.5) +
    ggplot2::geom_vline(xintercept = data$harmonics$dominant_period, 
                        color = "#D95319", linetype = "dashed", linewidth = 0.5) +
    ggplot2::scale_x_log10(name = "Period (years, log scale)") +
    ggplot2::scale_y_continuous(name = "Normalized Power") +
    ieee_theme() +
    ggplot2::labs(
      title = "Residual Spectrum",
      subtitle = sprintf("Dominant period: %.1f years", data$harmonics$dominant_period)
    )
  
  p
}

#' Create autocorrelation plot
#' @keywords internal
create_autocorrelation_plot <- function(data, config) {
  residuals <- data$residuals
  n <- length(residuals)
  max_lag <- min(10, n - 1)
  
  # Calculate ACF
  acf_vals <- acf(residuals, lag.max = max_lag, plot = FALSE)$acf[-1]
  lags <- 1:max_lag
  
  acf_df <- data.frame(
    Lag = lags,
    ACF = as.numeric(acf_vals)
  )
  
  # Confidence bounds
  conf_bound <- 1.96 / sqrt(n)
  
  p <- ggplot2::ggplot(acf_df, ggplot2::aes(x = Lag, y = ACF)) +
    ggplot2::geom_hline(yintercept = 0, color = "gray50", linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = c(conf_bound, -conf_bound), 
                        color = "#D95319", linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_segment(ggplot2::aes(xend = Lag, yend = 0), color = "#0072BD", linewidth = 1) +
    ggplot2::geom_point(color = "#0072BD", size = 2) +
    ggplot2::scale_x_continuous(name = "Lag", breaks = lags) +
    ggplot2::scale_y_continuous(name = "Autocorrelation") +
    ieee_theme() +
    ggplot2::labs(
      title = "Residual Autocorrelation",
      subtitle = sprintf("DW statistic: %.3f", data$autocorrelation$dw_statistic)
    )
  
  p
}

#' Create model adequacy summary plot
#' @keywords internal
create_model_adequacy_plot <- function(data, config) {
  if (is.null(data$model_adequacy)) return(NULL)
  
  ma <- data$model_adequacy
  
  # Create a summary bar chart
  metrics <- data.frame(
    Metric = c("R²", "Adj R²", "CV", "RMSE/n"),
    Value = c(ma$r_squared, ma$adj_r_squared, ma$cv, ma$rmse / mean(abs(data$observed))),
    Threshold = c(0.7, 0.7, 0.5, 0.2)
  )
  
  metrics$Metric <- factor(metrics$Metric, levels = c("R²", "Adj R²", "CV", "RMSE/n"))
  
  metrics_long <- reshape2::melt(metrics[, c("Metric", "Value", "Threshold")],
                                 id.vars = "Metric",
                                 variable.name = "type",
                                 value.name = "value")
  
  colors <- c("Value" = "#0072BD", "Threshold" = "#A2142F")
  
  p <- ggplot2::ggplot(metrics_long, ggplot2::aes(x = Metric, y = value, fill = type)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), 
                       width = 0.6, color = "black", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = colors, name = NULL) +
    ggplot2::scale_y_continuous(name = "Value", expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Model Adequacy Metrics",
      subtitle = if (ma$model_adequate) "Model: ADEQUATE" else "Model: POTENTIALLY INADEQUATE"
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  p
}

`%||%` <- function(a, b) if (!is.null(a)) a else b