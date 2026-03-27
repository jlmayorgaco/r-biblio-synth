# ============================================================================
# module_m2/render/m2_render_harmonics.R - Harmonics plots (FIXED)
# ============================================================================

#' @export
render_m2_harmonics <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"fft" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  plots <- list()

  # FFT
  fft <- result$fft
  if (length(fft$magnitude) > 0) {
    fft_df <- data.frame(frequency = fft$frequencies, magnitude = fft$magnitude)
    plots$fft <- ggplot2::ggplot(fft_df, ggplot2::aes(x = frequency, y = magnitude)) +
      ggplot2::geom_line(color = ieee_colors$blue, linewidth = 0.6) +
      ggplot2::labs(title = "FFT Magnitude Spectrum", x = "Frequency", y = "Magnitude") +
      ieee_theme(base_size = 8)
  }

  # R2 vs Frequency
  r2 <- result$r_squared_table
  if (!is.null(r2) && nrow(r2) > 0) {
    plots$r2_frequency <- ggplot2::ggplot(r2, ggplot2::aes(x = Frequency, y = R2)) +
      ggplot2::geom_line(color = ieee_colors$blue, linewidth = 0.6) +
      ggplot2::geom_point(color = ieee_colors$orange, size = 1.5) +
      ggplot2::labs(title = expression(R^2 ~ "vs Frequency"), x = "Frequency", y = expression(R^2)) +
      ieee_theme(base_size = 8)
  }

  list(status = "success", plots = plots, tables = list())
}
