# ============================================================================
# plot_export_service.R - Plot export service
# ============================================================================

#' Export a plot artifact to PNG and SVG
#'
#' @param plot A ggplot object.
#' @param path Character. Output file path (without extension).
#' @param width Numeric. Width in inches.
#' @param height Numeric. Height in inches.
#' @param dpi Numeric. Resolution in DPI.
#' @return Invisibly returns the character vector of paths.
#' @export
export_plot_artifact <- function(plot, path,
                                 width = 10,
                                 height = 6,
                                 dpi = 300) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  png_path <- paste0(path, ".png")
  svg_path <- paste0(path, ".svg")

  ggplot2::ggsave(
    filename = png_path,
    plot     = plot,
    width    = width,
    height   = height,
    dpi      = dpi
  )

  ggplot2::ggsave(
    filename = svg_path,
    plot     = plot,
    width    = width,
    height   = height,
    device   = "svg"
  )

  cli::cli_alert_info("Plot saved: {path}.{{png,svg}}")
  invisible(c(png = png_path, svg = svg_path))
}
