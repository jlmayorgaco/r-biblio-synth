# ============================================================================
# plot_export_service.R - Plot export service
# ============================================================================

#' Export a plot artifact to PNG, SVG, PDF, and EPS
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
  if (inherits(plot, "recordedplot")) {
    return(export_recorded_plot_artifact(plot, path, width = width, height = height, dpi = dpi))
  }

  # Suppress any display devices in non-interactive mode
  if (!interactive()) {
    old_dev <- grDevices::dev.cur()
    on.exit(if (grDevices::dev.cur() != old_dev) grDevices::dev.off(), add = TRUE)
  }
  
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  png_path <- paste0(path, ".png")
  svg_path <- paste0(path, ".svg")
  pdf_path <- paste0(path, ".pdf")
  eps_path <- paste0(path, ".eps")

  # Save PNG using Cairo if available (better for non-interactive)
  if (requireNamespace("Cairo", quietly = TRUE)) {
    suppressMessages(suppressWarnings(
      ggplot2::ggsave(
        filename = png_path,
        plot     = plot,
        width    = width,
        height   = height,
        dpi      = dpi,
        bg       = "white",
        type = "cairo"
      )
    ))
  } else {
    suppressMessages(suppressWarnings(
      ggplot2::ggsave(
        filename = png_path,
        plot     = plot,
        width    = width,
        height   = height,
        dpi      = dpi,
        bg       = "white"
      )
    ))
  }

  # Save SVG
  tryCatch({
    suppressMessages(suppressWarnings(
      ggplot2::ggsave(
        filename = svg_path,
        plot     = plot,
        width    = width,
        height   = height,
        device   = "svg",
        bg       = "white"
      )
    ))
  }, error = function(e) {
    # SVG might fail on some systems, that's ok
    NULL
  })

  tryCatch({
    suppressMessages(suppressWarnings(
      ggplot2::ggsave(
        filename = pdf_path,
        plot     = plot,
        width    = width,
        height   = height,
        device   = grDevices::pdf,
        bg       = "white"
      )
    ))
  }, error = function(e) {
    NULL
  })

  tryCatch({
    eps_device <- function(filename, width, height, ...) {
      grDevices::cairo_ps(
        filename = filename,
        width = width,
        height = height,
        onefile = FALSE,
        ...
      )
    }
    suppressMessages(suppressWarnings(
      ggplot2::ggsave(
        filename = eps_path,
        plot     = plot,
        width    = width,
        height   = height,
        device   = eps_device,
        bg       = "white"
      )
    ))
  }, error = function(e) {
    NULL
  })

  invisible(c(
    png = png_path,
    svg = if (file.exists(svg_path)) svg_path else NA_character_,
    pdf = if (file.exists(pdf_path)) pdf_path else NA_character_,
    eps = if (file.exists(eps_path)) eps_path else NA_character_
  ))
}

#' Export a recorded base-R plot artifact to PNG, SVG, PDF, and EPS
#'
#' @param plot A recordedplot object.
#' @param path Character. Output file path without extension.
#' @param width Numeric. Width in inches.
#' @param height Numeric. Height in inches.
#' @param dpi Numeric. Resolution in DPI.
#' @return Invisibly returns the character vector of paths.
#' @keywords internal
export_recorded_plot_artifact <- function(plot, path,
                                          width = 10,
                                          height = 6,
                                          dpi = 300) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  png_path <- paste0(path, ".png")
  svg_path <- paste0(path, ".svg")
  pdf_path <- paste0(path, ".pdf")
  eps_path <- paste0(path, ".eps")
  pixel_width <- max(1L, as.integer(round(width * dpi)))
  pixel_height <- max(1L, as.integer(round(height * dpi)))

  grDevices::png(filename = png_path, width = pixel_width, height = pixel_height, res = dpi)
  tryCatch({
    grDevices::replayPlot(plot)
  }, finally = {
    grDevices::dev.off()
  })

  tryCatch({
    grDevices::svg(filename = svg_path, width = width, height = height)
    tryCatch({
      grDevices::replayPlot(plot)
    }, finally = {
      grDevices::dev.off()
    })
  }, error = function(e) {
    NULL
  })

  tryCatch({
    grDevices::pdf(file = pdf_path, width = width, height = height)
    tryCatch({
      grDevices::replayPlot(plot)
    }, finally = {
      grDevices::dev.off()
    })
  }, error = function(e) {
    NULL
  })

  tryCatch({
    grDevices::postscript(
      file = eps_path,
      width = width,
      height = height,
      onefile = FALSE,
      paper = "special",
      horizontal = FALSE
    )
    tryCatch({
      grDevices::replayPlot(plot)
    }, finally = {
      grDevices::dev.off()
    })
  }, error = function(e) {
    NULL
  })

  invisible(c(
    png = png_path,
    svg = if (file.exists(svg_path)) svg_path else NA_character_,
    pdf = if (file.exists(pdf_path)) pdf_path else NA_character_,
    eps = if (file.exists(eps_path)) eps_path else NA_character_
  ))
}
