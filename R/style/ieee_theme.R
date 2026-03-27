# ============================================================================
# ieee_theme.R - IEEE Publication Theme for ggplot2
#
# IEEE 2-Column Paper Specifications:
# - Column width: 3.5 in (88 mm)
# - Page width (2-column): 7.16 in (181 mm)
# - Font: Times New Roman (or Computer Modern)
# - Caption font: 8 pt
# - Axis labels: 8-10 pt
# - Tick labels: 6-8 pt
# - Line width: 0.5-1.0 pt
# - Grid: subtle, gray
# ============================================================================

#' IEEE theme for single-column figures
#'
#' @param base_size Base font size (default 8pt for IEEE)
#' @param font_family Font family
#' @return A ggplot2 theme object
#' @export
ieee_theme <- function(base_size = 8, font_family = "sans") {
  ggplot2::theme_bw(base_size = base_size, base_family = font_family) +
    ggplot2::theme(
      # Title — mono for technical clarity
      plot.title = ggplot2::element_text(
        size = base_size + 1, face = "bold", hjust = 0.5,
        family = "mono",
        margin = ggplot2::margin(b = 6)
      ),
      # Subtitle — sans body
      plot.subtitle = ggplot2::element_text(
        size = base_size, hjust = 0.5, face = "italic",
        family = font_family,
        margin = ggplot2::margin(b = 4)
      ),
      # Axis titles — mono for technical labels
      axis.title = ggplot2::element_text(
        size = base_size, face = "bold",
        family = "mono",
        margin = ggplot2::margin(t = 4, r = 4)
      ),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 6)),
      # Axis text — mono for numeric alignment
      axis.text = ggplot2::element_text(size = base_size - 1, color = "black", family = "mono"),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      # Ticks
      axis.ticks = ggplot2::element_line(linewidth = 0.3, color = "black"),
      axis.ticks.length = ggplot2::unit(0.15, "cm"),
      # Lines
      axis.line = ggplot2::element_line(linewidth = 0.4, color = "black"),
      panel.border = ggplot2::element_rect(linewidth = 0.4, fill = NA, color = "black"),
      # Grid
      panel.grid.major = ggplot2::element_line(linewidth = 0.2, color = "#cccccc"),
      panel.grid.minor = ggplot2::element_blank(),
      # Background
      panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      # Legend
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(size = base_size - 1, face = "bold"),
      legend.text = ggplot2::element_text(size = base_size - 1),
      legend.key.size = ggplot2::unit(0.4, "cm"),
      legend.key = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      # Margins
      plot.margin = ggplot2::margin(t = 8, r = 8, b = 8, l = 8)
    )
}

#' IEEE theme for double-column (full width) figures
#'
#' @param base_size Base font size
#' @param font_family Font family
#' @return A ggplot2 theme object
#' @export
ieee_theme_double <- function(base_size = 8, font_family = "sans") {
  ieee_theme(base_size = base_size, font_family = font_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = base_size + 2)
    )
}

#' IEEE figure dimensions for single column
#' @export
ieee_dim_single <- list(width = 3.5, height = 2.5, dpi = 600)

#' IEEE figure dimensions for double column
#' @export
ieee_dim_double <- list(width = 7.16, height = 3.5, dpi = 600)

#' IEEE color palette (colorblind-friendly, works in grayscale)
#' @export
ieee_colors <- list(
  blue    = "#0072BD",
  orange  = "#D95319",
  yellow  = "#EDB120",
  purple  = "#7E2F8E",
  green   = "#77AC30",
  cyan    = "#4DBEEE",
  red     = "#A2142F",
  black   = "#000000",
  gray    = "#666666",
  ltgray  = "#cccccc"
)

#' Get IEEE palette for n items
#' @param n Number of colors needed
#' @export
get_ieee_palette <- function(n) {
  all_colors <- unname(unlist(ieee_colors[1:8]))
  if (n <= 8) return(all_colors[1:n])
  rep(all_colors, length.out = n)
}

#' IEEE fill scale
#' @export
scale_fill_ieee <- function(...) {
  ggplot2::scale_fill_manual(
    values = unname(unlist(ieee_colors[1:8])),
    ...
  )
}

#' IEEE color scale
#' @export
scale_color_ieee <- function(...) {
  ggplot2::scale_color_manual(
    values = unname(unlist(ieee_colors[1:8])),
    ...
  )
}

#' IEEE grayscale fill scale
#' @export
scale_fill_ieee_gray <- function(...) {
  grays <- c("#000000", "#333333", "#555555", "#777777",
             "#999999", "#aaaaaa", "#cccccc", "#eeeeee")
  ggplot2::scale_fill_manual(values = grays, ...)
}

#' IEEE grayscale color scale
#' @export
scale_color_ieee_gray <- function(...) {
  grays <- c("#000000", "#333333", "#555555", "#777777",
             "#999999", "#aaaaaa", "#cccccc", "#eeeeee")
  ggplot2::scale_color_manual(values = grays, ...)
}
