# ============================================================================
# module_m2/render/m2_render_wavelet.R - Wavelet visualization
# ============================================================================

#' @export
render_m2_wavelet <- function(result, years, config = biblio_config()) {
  if (!inherits(result, "list") || !"power" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  plots <- list()

  if (!is.null(result$power) && is.matrix(result$power)) {
    # Create data for heatmap
    df_wavelet <- expand.grid(
      Time = result$time,
      Period = result$period
    )
    df_wavelet$Power <- c(result$power)

    # Map time indices to actual years
    if (length(years) >= 2) {
      time_range <- range(result$time)
      year_range <- range(years)
      df_wavelet$Year <- year_range[1] + (df_wavelet$Time - time_range[1]) /
                         diff(time_range) * diff(year_range)
    } else {
      df_wavelet$Year <- df_wavelet$Time
    }

    plots$wavelet_power <- ggplot2::ggplot(df_wavelet, ggplot2::aes(x = Year, y = Period, fill = Power)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_viridis_c(option = "viridis") +
      ggplot2::labs(
        title = "Wavelet Power Spectrum",
        x = "Year",
        y = "Period (Years)",
        fill = "Power"
      ) +
      ggplot2::scale_y_log10() +
      ieee_theme(base_size = 8) +
      ggplot2::theme(legend.position = "right")
  }

  list(status = "success", plots = plots, tables = list())
}
