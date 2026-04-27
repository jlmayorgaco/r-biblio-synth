# ============================================================================
# m2_render_residual_analysis.R - Render Residual Diagnostics
# ============================================================================
# Creates legacy-inspired residual diagnostics with flat plot outputs:
# - Residual time series
# - Histogram and Q-Q normality diagnostics
# - Breakpoint detection
# - ACF/PACF and Durbin-Watson diagnostics
# - Heteroscedasticity and residual variance trend plots
# - Standardized residuals and residual FFT

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

  plots$residual_series <- create_residual_series_plot(data, config)
  plots$normality_histogram <- create_normality_histogram_plot(data, config)
  plots$normality_qq <- create_normality_qq_plot(data, config)
  plots$breakpoint_detection <- create_breakpoint_plot(data, config)
  plots$residual_spectrum <- create_residual_spectrum_plot(data, config)
  plots$residual_fft <- create_residual_fft_plot(data, config)
  plots$residual_acf <- create_residual_acf_plot(data, config)
  plots$residual_pacf <- create_residual_pacf_plot(data, config)
  plots$durbin_watson <- create_durbin_watson_plot(data, config)
  plots$heteroscedasticity <- create_heteroscedasticity_plot(data, config)
  plots$standardized_residuals <- create_standardized_residuals_plot(data, config)
  plots$residuals_squared <- create_residuals_squared_plot(data, config)
  plots$residuals_squared_model_fit <- create_residuals_squared_model_fit_plot(data, config)
  plots$model_adequacy <- create_model_adequacy_plot(data, config)

  plots <- Filter(Negate(is.null), plots)

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
    Residual = data$residuals
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = Year, y = Residual)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
    ggplot2::geom_line(color = "#0072BD", linewidth = 0.5) +
    ggplot2::geom_point(color = "#0072BD", size = 1.5) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#D95319", linetype = "dashed", linewidth = 0.5) +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_y_continuous(name = "Residual") +
    ieee_theme_timeseries() +
    ggplot2::labs(
      title = "Residual Time Series",
      subtitle = sprintf("Mean = %.2f | SD = %.2f", mean(data$residuals), stats::sd(data$residuals))
    )

  if (!is.null(data$breakpoints) && isTRUE(data$breakpoints$has_breakpoints)) {
    for (bp_year in data$breakpoints$breakpoint_years) {
      p <- p + ggplot2::geom_vline(
        xintercept = bp_year,
        color = "#A2142F",
        linetype = "dotted",
        linewidth = 0.5
      )
    }
  }

  p
}

#' Create normality histogram plot
#' @keywords internal
create_normality_histogram_plot <- function(data, config) {
  residuals <- data$residuals
  df <- data.frame(Residual = residuals)
  residual_mean <- mean(residuals, na.rm = TRUE)
  residual_sd <- stats::sd(residuals, na.rm = TRUE)

  annotation <- sprintf(
    "Shapiro p = %.4f\nAD p = %.4f\nJB p = %.4f\nSkew = %.2f\nKurtosis = %.2f",
    data$normality$shapiro_wilk$p_value %||% NA_real_,
    data$normality$anderson_darling$p_value %||% NA_real_,
    data$normality$jarque_bera$p_value %||% NA_real_,
    data$normality$skewness %||% NA_real_,
    data$normality$kurtosis %||% NA_real_
  )

  ggplot2::ggplot(df, ggplot2::aes(x = Residual)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = 20,
      fill = "#0072BD",
      color = "black",
      linewidth = 0.2,
      alpha = 0.75
    ) +
    ggplot2::geom_density(color = "#D95319", linewidth = 0.8) +
    ggplot2::stat_function(
      fun = stats::dnorm,
      args = list(mean = residual_mean, sd = residual_sd),
      color = "#77AC30",
      linetype = "dashed",
      linewidth = 0.6
    ) +
    ggplot2::annotate(
      "text",
      x = Inf, y = Inf,
      label = annotation,
      hjust = 1.05, vjust = 1.1,
      size = 2.7,
      fontface = "plain"
    ) +
    ggplot2::scale_x_continuous(name = "Residual") +
    ggplot2::scale_y_continuous(name = "Density") +
    ieee_theme() +
    ggplot2::labs(
      title = "Residual Distribution",
      subtitle = data$normality$interpretation %||% ""
    )
}

#' Create Q-Q plot
#' @keywords internal
create_normality_qq_plot <- function(data, config) {
  qq <- stats::qqnorm(data$residuals, plot.it = FALSE)
  df <- data.frame(theoretical = qq$x, sample = qq$y)

  ggplot2::ggplot(df, ggplot2::aes(x = theoretical, y = sample)) +
    ggplot2::geom_point(color = "#0072BD", size = 1.8, alpha = 0.8) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "#D95319", linetype = "dashed", linewidth = 0.6) +
    ggplot2::scale_x_continuous(name = "Theoretical Quantiles") +
    ggplot2::scale_y_continuous(name = "Sample Quantiles") +
    ieee_theme() +
    ggplot2::labs(title = "Q-Q Plot of Residuals")
}

#' Create breakpoint detection plot
#' @keywords internal
create_breakpoint_plot <- function(data, config) {
  if (is.null(data$breakpoints)) {
    return(NULL)
  }

  residuals <- data$residuals
  years <- data$years
  cumulative <- cumsum(residuals - mean(residuals, na.rm = TRUE))
  threshold <- data$breakpoints$cusum$threshold %||% (1.143 * sqrt(length(residuals)) * stats::sd(residuals))

  cusum_df <- data.frame(
    Year = years,
    CUSUM = cumulative
  )

  ggplot2::ggplot(cusum_df, ggplot2::aes(x = Year, y = CUSUM)) +
    ggplot2::geom_line(color = "#0072BD", linewidth = 0.8) +
    ggplot2::geom_point(color = "#0072BD", size = 1.4) +
    ggplot2::geom_hline(yintercept = c(-threshold, threshold), color = "#A2142F", linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = 0, color = "gray50", linewidth = 0.3) +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_y_continuous(name = "CUSUM") +
    ieee_theme_timeseries() +
    ggplot2::labs(
      title = "CUSUM Breakpoint Detection",
      subtitle = data$breakpoints$interpretation %||% ""
    )
}

#' Create residual spectrum plot
#' @keywords internal
create_residual_spectrum_plot <- function(data, config) {
  if (is.null(data$harmonics) || is.null(data$harmonics$spectrum)) {
    return(NULL)
  }

  spec <- data$harmonics$spectrum
  spec <- spec[spec$frequency > 0 & is.finite(spec$period) & spec$period < 50, , drop = FALSE]
  if (nrow(spec) == 0) {
    return(NULL)
  }

  ggplot2::ggplot(spec, ggplot2::aes(x = period, y = normalized_power)) +
    ggplot2::geom_line(color = "#0072BD", linewidth = 0.8) +
    ggplot2::geom_point(color = "#0072BD", size = 1.4) +
    ggplot2::geom_vline(
      xintercept = data$harmonics$dominant_period,
      color = "#D95319",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    ggplot2::scale_x_log10(name = "Period (years, log scale)") +
    ggplot2::scale_y_continuous(name = "Normalized Power") +
    ieee_theme() +
    ggplot2::labs(
      title = "Residual Harmonic Spectrum",
      subtitle = data$harmonics$interpretation %||% ""
    )
}

#' Create FFT residual plot
#' @keywords internal
create_residual_fft_plot <- function(data, config) {
  if (is.null(data$harmonics) || is.null(data$harmonics$spectrum)) {
    return(NULL)
  }

  fft_data <- data$harmonics$spectrum
  fft_data <- fft_data[fft_data$frequency > 0 & is.finite(fft_data$period) & fft_data$period < 100, , drop = FALSE]
  fft_data <- fft_data[order(fft_data$period), , drop = FALSE]
  if (nrow(fft_data) == 0) {
    return(NULL)
  }

  peak_indices <- m2_find_local_peaks(fft_data$normalized_power)
  if (length(peak_indices) > 0) {
    peak_power <- fft_data$normalized_power[peak_indices]
    top_n <- order(peak_power, decreasing = TRUE)[seq_len(min(5, length(peak_indices)))]
    peak_indices <- peak_indices[top_n]
  }

  dominant_period <- data$harmonics$dominant_period %||% fft_data$period[which.max(fft_data$normalized_power)]
  label_y <- max(fft_data$normalized_power, na.rm = TRUE) * 1.07

  p <- ggplot2::ggplot(fft_data, ggplot2::aes(x = period, y = normalized_power)) +
    ggplot2::geom_line(color = "black", linewidth = 0.45) +
    ggplot2::geom_point(color = "black", size = 1.2, shape = 21, fill = "white", stroke = 0.4) +
    ggplot2::geom_vline(xintercept = dominant_period, color = "#D95319", linetype = "dashed", linewidth = 0.4) +
    ggplot2::annotate(
      "text",
      x = dominant_period * 1.1,
      y = label_y,
      label = sprintf("Dominant: %.1f yr", dominant_period),
      hjust = 0,
      color = "#D95319",
      size = 2.8
    ) +
    ggplot2::scale_x_log10(name = "Period (years)", breaks = c(1, 2, 5, 10, 20, 50)) +
    ggplot2::scale_y_continuous(name = "Normalized Power", expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ieee_theme() +
    ggplot2::labs(title = "FFT Analysis of Residuals")

  if (length(peak_indices) > 0) {
    peaks_df <- fft_data[peak_indices, , drop = FALSE]
    p <- p +
      ggplot2::geom_point(data = peaks_df, color = "#A2142F", shape = 8, size = 2.5) +
      ggplot2::geom_text(
        data = peaks_df,
        ggplot2::aes(label = sprintf("%.1f", period)),
        color = "#A2142F",
        size = 2.4,
        vjust = -0.7
      )
  }

  p
}

#' Create residual ACF plot
#' @keywords internal
create_residual_acf_plot <- function(data, config) {
  acf_values <- data$autocorrelation$acf_values
  acf_lags <- data$autocorrelation$acf_lags
  if (length(acf_values) == 0) {
    return(NULL)
  }

  df <- data.frame(Lag = acf_lags, ACF = acf_values)
  conf_bound <- 1.96 / sqrt(length(data$residuals))

  ggplot2::ggplot(df, ggplot2::aes(x = Lag, y = ACF)) +
    ggplot2::geom_hline(yintercept = 0, color = "gray50", linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = c(-conf_bound, conf_bound), color = "#D95319", linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_segment(ggplot2::aes(xend = Lag, yend = 0), color = "#0072BD", linewidth = 0.9) +
    ggplot2::geom_point(color = "#0072BD", size = 1.8) +
    ggplot2::scale_x_continuous(name = "Lag") +
    ggplot2::scale_y_continuous(name = "Autocorrelation", limits = c(-1, 1)) +
    ieee_theme() +
    ggplot2::labs(title = "ACF of Residuals", subtitle = "Dashed lines show 95% confidence bounds")
}

#' Create residual PACF plot
#' @keywords internal
create_residual_pacf_plot <- function(data, config) {
  pacf_values <- data$autocorrelation$pacf_values
  pacf_lags <- data$autocorrelation$pacf_lags
  if (length(pacf_values) == 0) {
    return(NULL)
  }

  df <- data.frame(Lag = pacf_lags, PACF = pacf_values)
  conf_bound <- 1.96 / sqrt(length(data$residuals))

  ggplot2::ggplot(df, ggplot2::aes(x = Lag, y = PACF)) +
    ggplot2::geom_hline(yintercept = 0, color = "gray50", linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = c(-conf_bound, conf_bound), color = "#D95319", linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_segment(ggplot2::aes(xend = Lag, yend = 0), color = "#0072BD", linewidth = 0.9) +
    ggplot2::geom_point(color = "#0072BD", size = 1.8) +
    ggplot2::scale_x_continuous(name = "Lag") +
    ggplot2::scale_y_continuous(name = "Partial Autocorrelation", limits = c(-1, 1)) +
    ieee_theme() +
    ggplot2::labs(title = "PACF of Residuals", subtitle = "Legacy diagnostic recovered in flat exportable form")
}

#' Create Durbin-Watson diagnostic plot
#' @keywords internal
create_durbin_watson_plot <- function(data, config) {
  df <- data.frame(Year = data$years, Residual = data$residuals)
  max_abs <- max(abs(df$Residual), na.rm = TRUE)

  ggplot2::ggplot(df, ggplot2::aes(x = Year, y = Residual)) +
    ggplot2::geom_hline(yintercept = 0, color = "gray50", linewidth = 0.3) +
    ggplot2::geom_line(color = "black", linewidth = 0.45) +
    ggplot2::geom_point(color = "black", size = 1.4, shape = 21, fill = "white", stroke = 0.4) +
    ggplot2::coord_cartesian(ylim = c(-max_abs, max_abs)) +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_y_continuous(name = "Residual") +
    ieee_theme_timeseries() +
    ggplot2::labs(
      title = "Durbin-Watson Diagnostic",
      subtitle = sprintf(
        "DW = %.3f | Ljung-Box p = %.4f",
        data$autocorrelation$dw_statistic %||% NA_real_,
        data$autocorrelation$lb_p_value %||% NA_real_
      )
    )
}

#' Create heteroscedasticity plot
#' @keywords internal
create_heteroscedasticity_plot <- function(data, config) {
  if (is.null(data$heteroscedasticity)) {
    return(NULL)
  }

  df <- data.frame(
    Year = data$years,
    ResidualSq = data$squared_residuals
  )

  ggplot2::ggplot(df, ggplot2::aes(x = Year, y = ResidualSq)) +
    ggplot2::geom_point(color = "black", size = 1.4, shape = 21, fill = "white", stroke = 0.4) +
    ggplot2::geom_smooth(method = "lm", color = "#D95319", linetype = "dashed", linewidth = 0.5, se = FALSE) +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_y_continuous(name = "Residual Squared") +
    ieee_theme() +
    ggplot2::labs(
      title = "Breusch-Pagan Style Variance Diagnostic",
      subtitle = sprintf(
        "BP p = %.4f | Slope p = %.4f",
        data$heteroscedasticity$p_value %||% NA_real_,
        data$heteroscedasticity$slope_p_value %||% NA_real_
      )
    )
}

#' Create standardized residual plot
#' @keywords internal
create_standardized_residuals_plot <- function(data, config) {
  df <- data.frame(
    Year = data$years,
    StdResidual = data$standardized_residuals
  )

  ggplot2::ggplot(df, ggplot2::aes(x = Year, y = StdResidual)) +
    ggplot2::geom_hline(yintercept = 0, color = "gray50", linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = c(-2, 2), color = "#A2142F", linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_point(color = "black", size = 1.5, shape = 21, fill = "white", stroke = 0.4) +
    ggplot2::geom_line(color = "black", linewidth = 0.45) +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_y_continuous(name = "Standardized Residual") +
    ieee_theme_timeseries() +
    ggplot2::labs(title = "Standardized Residuals")
}

#' Create residuals squared plot
#' @keywords internal
create_residuals_squared_plot <- function(data, config) {
  df <- data.frame(
    Year = data$years,
    ResidualSq = data$squared_residuals
  )

  ggplot2::ggplot(df, ggplot2::aes(x = Year, y = ResidualSq)) +
    ggplot2::geom_point(color = "black", size = 1.4, shape = 21, fill = "white", stroke = 0.4) +
    ggplot2::geom_smooth(method = "loess", color = "#0072BD", linetype = "dashed", linewidth = 0.5, se = FALSE) +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_y_continuous(name = "Residual Squared") +
    ieee_theme_timeseries() +
    ggplot2::labs(title = "Residuals Squared Over Time")
}

#' Create residual variance model fit plot
#' @keywords internal
create_residuals_squared_model_fit_plot <- function(data, config) {
  variance_model <- data$variance_model
  if (is.null(variance_model) || variance_model$status != "success") {
    return(NULL)
  }

  observed_df <- data.frame(
    Year = data$years,
    ResidualSq = data$squared_residuals
  )

  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = observed_df,
      ggplot2::aes(x = Year, y = ResidualSq),
      color = "black",
      size = 1.5,
      shape = 21,
      fill = "white",
      stroke = 0.4
    ) +
    ggplot2::geom_line(
      data = variance_model$smooth_fit,
      ggplot2::aes(x = Year, y = Fitted),
      color = "#D95319",
      linewidth = 0.7,
      linetype = "dashed"
    ) +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_y_continuous(name = "Residual Squared") +
    ieee_theme_timeseries() +
    ggplot2::labs(
      title = "Residual Variance Model Fit",
      subtitle = sprintf(
        "Best model: %s | R2 = %.3f",
        variance_model$best_model %||% "NA",
        variance_model$best_r_squared %||% NA_real_
      )
    )
}

#' Create model adequacy summary plot
#' @keywords internal
create_model_adequacy_plot <- function(data, config) {
  if (is.null(data$model_adequacy)) {
    return(NULL)
  }

  ma <- data$model_adequacy
  metrics <- data.frame(
    Metric = c("R2", "Adj R2", "CV", "RMSE/Mean"),
    Value = c(
      ma$r_squared,
      ma$adj_r_squared,
      ma$cv,
      ma$rmse / max(mean(abs(data$observed), na.rm = TRUE), .Machine$double.eps)
    ),
    Threshold = c(0.7, 0.7, 0.5, 0.2),
    stringsAsFactors = FALSE
  )

  metrics_long <- data.frame(
    Metric = rep(metrics$Metric, 2),
    Type = rep(c("Observed", "Threshold"), each = nrow(metrics)),
    Value = c(metrics$Value, metrics$Threshold),
    stringsAsFactors = FALSE
  )
  metrics_long$Metric <- factor(metrics_long$Metric, levels = metrics$Metric)

  ggplot2::ggplot(metrics_long, ggplot2::aes(x = Metric, y = Value, fill = Type)) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 0.8),
      width = 0.65,
      color = "black",
      linewidth = 0.2
    ) +
    ggplot2::scale_fill_manual(values = c("Observed" = "#0072BD", "Threshold" = "#A2142F")) +
    ggplot2::scale_y_continuous(name = "Value", expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Model Adequacy Metrics",
      subtitle = if (isTRUE(ma$model_adequate)) "Model classified as adequate" else "Model may require refinement"
    ) +
    ggplot2::theme(legend.position = "bottom")
}

#' Detect local spectral peaks
#' @keywords internal
m2_find_local_peaks <- function(x) {
  if (length(x) < 3) {
    return(integer(0))
  }
  which(diff(sign(diff(x))) == -2) + 1L
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
