# ============================================================================
# ieee_theme.R - IEEE ggplot2 theme
# ============================================================================

#' IEEE-style ggplot2 theme
#'
#' Returns a minimal ggplot2 theme suitable for IEEE publications.
#'
#' @param base_size Numeric. Base font size.
#' @return A ggplot2 theme object.
#' @export
ieee_theme <- function(base_size = 10) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5, size = base_size + 1, face = "bold"
      ),
      axis.title = ggplot2::element_text(size = base_size - 1),
      axis.text  = ggplot2::element_text(size = base_size - 2),
      panel.grid.major = ggplot2::element_line(linewidth = 0.2, color = "#dddddd"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom",
      legend.text      = ggplot2::element_text(size = base_size - 2)
    )
}
