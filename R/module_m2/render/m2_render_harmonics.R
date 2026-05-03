# ============================================================================
# m2_render_harmonics.R - Render Harmonic Analysis (Enhanced)
# ============================================================================
# Creates plots for:
# - Data harmonics (underlying time series)
# - Residual harmonics (error between model and data)
# - Comparison of periodicities

#' Render harmonic analysis plots
#'
#' @param result Output from compute_m2_harmonics
#' @param config Configuration list
#' @return List with plots
#' @export
render_m2_harmonics <- function(result, config = biblio_config()) {
  if (is.null(result) || result$status != "success") {
    return(list(plots = list(), status = "error: invalid data"))
  }
  
  plots <- list()
  
  # Data spectrum plot
  if (!is.null(result$data_harmonics)) {
    plots$data_spectrum <- create_data_spectrum_plot(result$data_harmonics, config)
    plots$data_top_periods <- create_top_periods_plot(result$data_harmonics, config, "Data")
  }
  
  # Residual spectrum plot
  if (!is.null(result$residual_harmonics) && length(result$residual_harmonics) > 0) {
    plots$residual_spectrum <- create_residual_spectrum_plot(result$residual_harmonics, config)
    plots$residual_warning <- create_residual_warning_plot(result$residual_harmonics, config)
  }
  
  # Lomb-Scargle periodogram
  if (!is.null(result$lomb) && length(result$lomb) > 0) {
    plots$lomb_periodogram <- create_lomb_periodogram_plot(result$lomb, config)
  }
  
  # Wavelet analysis
  if (!is.null(result$wavelet) && length(result$wavelet) > 0 && !is.null(result$wavelet$power)) {
    plots$wavelet <- create_wavelet_plot(result$wavelet, config)
  }
  
  # Cross-analysis comparison
  if (!is.null(result$cross_analysis)) {
    plots$comparison <- create_harmonic_comparison_plot(result$cross_analysis, config)
  }

  # Harmonic regression fitness across candidate frequencies
  if (!is.null(result$r_squared_table) && nrow(result$r_squared_table) > 0) {
    plots$r2_vs_frequency <- create_r2_frequency_plot(result$r_squared_table, config)
  }
  
  # Summary plot
  if (!is.null(result$summary)) {
    plots$summary <- create_harmonic_summary_plot(result$summary, config)
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create data spectrum plot
#' @keywords internal
create_data_spectrum_plot <- function(data_harmonics, config) {
  if (is.null(data_harmonics$spectrum)) return(NULL)
  
  spec <- data_harmonics$spectrum
  spec <- spec[spec$frequency > 0 & spec$period < 50, ]
  
  p <- ggplot2::ggplot(spec, ggplot2::aes(x = period, y = normalized_power)) +
    ggplot2::geom_line(color = "black", linewidth = 0.55) +
    ggplot2::geom_point(color = "black", fill = "white", shape = 21, size = 1.1, stroke = 0.3) +
    ggplot2::geom_vline(xintercept = data_harmonics$dominant_period,
                       color = "#E15759", linetype = "22", linewidth = 0.55) +
    ggplot2::scale_x_log10(name = "Period (years)", breaks = c(1, 2, 5, 10, 20, 50)) +
    ggplot2::scale_y_continuous(name = "Normalized Power", expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ieee_theme() +
    ggplot2::labs(
      title = "Data Harmonic Spectrum",
      subtitle = sprintf("Dominant period: %.1f years (%.1f%% variance)", 
                        data_harmonics$dominant_period, 
                        data_harmonics$variance_explained * 100)
    )
  
  # Annotate top 3 periods
  if (!is.null(data_harmonics$top_periods) && nrow(data_harmonics$top_periods) > 0) {
    top3 <- head(data_harmonics$top_periods, 3)
    for (i in seq_len(nrow(top3))) {
      p <- p + ggplot2::annotate("text", x = top3$period[i], y = top3$normalized_power[i],
                                label = sprintf("%.1f", top3$period[i]), 
                                size = 2.2, vjust = -0.8, color = "#E15759")
    }
  }
  
  ieee_mark_plot_layout(p, "single")
}

#' Create top periods plot
#' @keywords internal
create_top_periods_plot <- function(harmonics, config, title_suffix = "") {
  if (is.null(harmonics$top_periods) || nrow(harmonics$top_periods) == 0) return(NULL)
  
  df <- harmonics$top_periods
  value_col <- if ("variance_explained" %in% names(df)) {
    "variance_explained"
  } else if ("normalized_power" %in% names(df)) {
    "normalized_power"
  } else {
    return(NULL)
  }
  df$period_label <- sprintf("%.1f yr", df$period)
  df$period_label <- factor(df$period_label, levels = df$period_label)
  df$share <- pmax(0, suppressWarnings(as.numeric(df[[value_col]])))
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = period_label, y = share * 100)) +
    ggplot2::geom_col(fill = "#0072BD", color = "black", linewidth = 0.2, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", share * 100)), 
                      vjust = -0.3, size = 2.5) +
    ggplot2::scale_y_continuous(name = "Variance Explained (%)", expand = ggplot2::expansion(mult = c(0, 0.2))) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = sprintf("Top Harmonic Periods - %s", title_suffix),
      x = "Period"
    )
  
  ieee_mark_plot_layout(p, "single")
}

#' Create R-squared versus frequency plot
#' @keywords internal
create_r2_frequency_plot <- function(r_squared_table, config) {
  if (is.null(r_squared_table) || nrow(r_squared_table) == 0) {
    return(NULL)
  }

  df <- r_squared_table[order(r_squared_table$Frequency), , drop = FALSE]
  best_idx <- which.max(df$R2)
  best_freq <- df$Frequency[best_idx]
  best_r2 <- df$R2[best_idx]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = Frequency, y = R2)) +
    ggplot2::geom_line(color = "black", linewidth = 0.7) +
    ggplot2::geom_point(color = "black", fill = "white", shape = 21, size = 1.7, stroke = 0.35) +
    ggplot2::geom_vline(xintercept = best_freq, color = "#E15759", linetype = "22", linewidth = 0.55) +
    ggplot2::annotate(
      "label",
      x = best_freq,
      y = min(1, best_r2 + 0.08),
      label = sprintf("Best: %.3f Hz\nR2 = %.3f", best_freq, best_r2),
      hjust = 0,
      size = 2.6,
      color = "#E15759",
      linewidth = 0.2,
      fill = scales::alpha("white", 0.9),
      label.padding = grid::unit(0.12, "lines")
    ) +
    ggplot2::scale_x_continuous(name = "Frequency") +
    ggplot2::scale_y_continuous(name = "R2", limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ieee_theme() +
    ggplot2::labs(
      title = "Harmonic Regression Fit by Frequency",
      subtitle = "Recovered from legacy M2 harmonic regression workflow"
    )

  ieee_mark_plot_layout(p, "single")
}

#' Create residual spectrum plot
#' @keywords internal
create_residual_spectrum_plot <- function(residual_harmonics, config) {
  if (is.null(residual_harmonics$spectrum)) return(NULL)
  
  spec <- residual_harmonics$spectrum
  spec <- spec[spec$frequency > 0 & spec$period < 50, ]
  
  color <- if (residual_harmonics$has_periodicity) "#A2142F" else "#77AC30"
  
  p <- ggplot2::ggplot(spec, ggplot2::aes(x = period, y = normalized_power)) +
    ggplot2::geom_line(color = "black", linewidth = 0.55) +
    ggplot2::geom_point(color = "black", fill = "white", shape = 21, size = 1.1, stroke = 0.3) +
    ggplot2::scale_x_log10(name = "Period (years)", breaks = c(1, 2, 5, 10, 20, 50)) +
    ggplot2::scale_y_continuous(name = "Normalized Power", expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ieee_theme() +
    ggplot2::labs(
      title = "Residual Harmonic Spectrum",
      subtitle = sprintf("Dominant: %.1f years | %s",
                        residual_harmonics$dominant_period %||% NA,
                        if (residual_harmonics$has_periodicity) "PERIODICITY DETECTED" else "No strong periodicity")
    )
  
  if (residual_harmonics$has_periodicity && !is.na(residual_harmonics$dominant_period)) {
    p <- p + ggplot2::geom_vline(xintercept = residual_harmonics$dominant_period,
                                 color = color, linetype = "22", linewidth = 0.55)
  }
  
  ieee_mark_plot_layout(p, "single")
}

#' Create residual warning plot
#' @keywords internal
create_residual_warning_plot <- function(residual_harmonics, config) {
  if (is.null(residual_harmonics$periodicity_test)) return(NULL)
  
  test <- residual_harmonics$periodicity_test
  
  status_color <- if (test$has_periodicity) "#A2142F" else "#77AC30"
  status_text <- if (test$has_periodicity) "PERIODICITY DETECTED" else "No Periodicity"
  
  df <- data.frame(
    x = 0.5,
    y = 0.5,
    label = sprintf("Fisher's g test\np-value: %.4f\n\n%s",
                    test$p_value,
                    test$interpretation)
  )
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_text(ggplot2::aes(label = label), hjust = 0.5, vjust = 0.5, 
                      size = 3, fontface = "bold", color = status_color) +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Residual Periodicity Test")
  
  ieee_mark_plot_layout(p, "single")
}

#' Create Lomb-Scargle periodogram
#' @keywords internal
create_lomb_periodogram_plot <- function(lomb, config) {
  if (is.null(lomb$frequency) || is.null(lomb$power)) return(NULL)
  
  df <- data.frame(
    frequency = lomb$frequency,
    power = lomb$power
  )
  
  # Convert to period
  df$period <- 1 / df$frequency
  df <- df[df$period > 0 & df$period < 50, ]
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = period, y = power)) +
    ggplot2::geom_line(color = "black", linewidth = 0.55) +
    ggplot2::geom_vline(xintercept = lomb$peak_period, 
                       color = "#E15759", linetype = "22", linewidth = 0.5) +
    ggplot2::scale_x_log10(name = "Period (years)", breaks = c(1, 2, 5, 10, 20, 50)) +
    ggplot2::scale_y_continuous(name = "Power") +
    ieee_theme() +
    ggplot2::labs(
      title = "Lomb-Scargle Periodogram",
      subtitle = sprintf("Peak period: %.1f years", lomb$peak_period %||% NA)
    )
  
  ieee_mark_plot_layout(p, "single")
}

#' Create wavelet plot
#' @keywords internal
create_wavelet_plot <- function(wavelet, config) {
  if (is.null(wavelet$time) || is.null(wavelet$period) || is.null(wavelet$power)) return(NULL)
  
  # Create contour-style plot
  df <- expand.grid(
    time = wavelet$time,
    period = wavelet$period
  )
  df$power <- as.vector(wavelet$power)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = time, y = period, z = power)) +
    ggplot2::geom_contour_filled(bins = 10) +
    ggplot2::scale_y_log10(name = "Period (years, log scale)") +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_fill_viridis_c(name = "Power") +
    ieee_theme() +
    ggplot2::labs(
      title = "Wavelet Power Spectrum"
    )
  
  ieee_mark_plot_layout(p, "full")
}

#' Create harmonic comparison plot
#' @keywords internal
create_harmonic_comparison_plot <- function(cross_analysis, config) {
  if (is.null(cross_analysis$data_dominant_period)) return(NULL)
  
  df <- data.frame(
    source = c("Data", "Residuals"),
    period = c(
      cross_analysis$data_dominant_period,
      cross_analysis$residual_dominant_period %||% NA
    ),
    variance = c(
      cross_analysis$data_variance_explained %||% NA,
      cross_analysis$residual_variance_explained %||% NA
    )
  )
  
  df <- df[!is.na(df$period), ]
  
  if (nrow(df) == 0) return(NULL)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = source, y = period, fill = source)) +
    ggplot2::geom_col(color = "black", linewidth = 0.2, width = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f yr\n(var: %.0f%%)", 
                                                     period, variance * 100)), 
                      vjust = -0.2, size = 2.5) +
    ggplot2::scale_fill_manual(values = c("Data" = "#0072BD", "Residuals" = "#A2142F"), guide = "none") +
    ggplot2::scale_y_continuous(name = "Dominant Period (years)", expand = ggplot2::expansion(mult = c(0, 0.3))) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Data vs Residual Harmonics",
      subtitle = cross_analysis$interpretation %||% ""
    )
  
  ieee_mark_plot_layout(p, "single")
}

#' Create harmonic summary plot
#' @keywords internal
create_harmonic_summary_plot <- function(summary, config) {
  periods <- c(
    summary$data_dominant_period,
    summary$residual_dominant_period,
    summary$wavelet_dominant_period,
    summary$lomb_peak_period
  )
  periods <- periods[!is.na(periods)]
  
  if (length(periods) == 0) return(NULL)
  
  sources <- c("FFT (Data)", "FFT (Residuals)", "Wavelet", "Lomb-Scargle")[1:length(periods)]
  
  df <- data.frame(
    method = factor(sources, levels = c("FFT (Data)", "FFT (Residuals)", "Wavelet", "Lomb-Scargle")),
    period = periods
  )
  
  colors <- c("FFT (Data)" = "#0072BD", "FFT (Residuals)" = "#A2142F", 
              "Wavelet" = "#77AC30", "Lomb-Scargle" = "#7E2F8E")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = method, y = period, fill = method)) +
    ggplot2::geom_col(color = "black", linewidth = 0.2, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", period)), vjust = -0.3, size = 2.5) +
    ggplot2::scale_fill_manual(values = colors, guide = "none") +
    ggplot2::scale_y_continuous(name = "Dominant Period (years)", expand = ggplot2::expansion(mult = c(0, 0.2))) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Dominant Periods by Method",
      x = NULL
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  ieee_mark_plot_layout(p, "single")
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
