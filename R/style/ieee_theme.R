# ============================================================================
# ieee_theme.R - IEEE Publication Theme for ggplot2 (Enhanced)
# ============================================================================
# IEEE Q1 Journal Quality Standards:
# - Single column: 3.5 in x 2.5 in (8.89 cm x 6.35 cm)
# - Double column: 7.16 in x 3.5 in (18.2 cm x 8.9 cm)
# - Font: Times or sans-serif, 8-10 pt
# - Grid: subtle gray, 0.2-0.3 pt
# - Axis lines: 0.5-0.75 pt black
# - Legends: bottom-placed, compact
# ============================================================================

#' IEEE theme for single-column figures (Enhanced)
#'
#' @param base_size Base font size (default 8pt for IEEE)
#' @param font_family Font family
#' @param grid_major Show major grid lines
#' @param grid_minor Show minor grid lines
#' @return A ggplot2 theme object
#' @export
ieee_theme <- function(base_size = 8, font_family = "sans", 
                       grid_major = TRUE, grid_minor = FALSE) {
  t <- ggplot2::theme_bw(base_size = base_size, base_family = font_family)
  
  t <- t + ggplot2::theme(
    plot.title = ggplot2::element_text(
      size = base_size + 2, face = "bold", hjust = 0.5,
      margin = ggplot2::margin(b = 4, t = 0)
    ),
    plot.subtitle = ggplot2::element_text(
      size = base_size - 1, hjust = 0.5, face = "italic",
      margin = ggplot2::margin(b = 4)
    ),
    plot.caption = ggplot2::element_text(
      size = base_size - 2, hjust = 1, face = "italic",
      margin = ggplot2::margin(t = 4)
    ),
    axis.title = ggplot2::element_text(
      size = base_size, face = "bold",
      margin = ggplot2::margin(t = 4, r = 4, b = 2)
    ),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 6, b = 2)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 6), angle = 90),
    axis.text = ggplot2::element_text(
      size = base_size - 1, color = "black",
      margin = ggplot2::margin(t = 2, r = 2)
    ),
    axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    axis.text.y = ggplot2::element_text(hjust = 1),
    axis.ticks = ggplot2::element_line(linewidth = 0.3, color = "black"),
    axis.ticks.length = ggplot2::unit(0.15, "cm"),
    axis.line = ggplot2::element_line(linewidth = 0.5, color = "black"),
    panel.border = ggplot2::element_rect(linewidth = 0.5, fill = NA, color = "black"),
    panel.grid.major = if (grid_major) {
      ggplot2::element_line(linewidth = 0.25, color = "#E0E0E0", linetype = "solid")
    } else {
      ggplot2::element_blank()
    },
    panel.grid.minor = if (grid_minor) {
      ggplot2::element_line(linewidth = 0.15, color = "#F0F0F0", linetype = "dotted")
    } else {
      ggplot2::element_blank()
    },
    panel.background = ggplot2::element_rect(fill = "#FAFAFA", color = NA),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.justification = c(0.5, 0.5),
    legend.title = ggplot2::element_text(size = base_size - 1, face = "bold"),
    legend.text = ggplot2::element_text(size = base_size - 2),
    legend.key.size = ggplot2::unit(0.5, "cm"),
    legend.key = ggplot2::element_rect(fill = "white", color = NA),
    legend.background = ggplot2::element_rect(fill = "white", color = NA),
    legend.margin = ggplot2::margin(t = 2, r = 2, b = 2, l = 2),
    legend.box.background = ggplot2::element_rect(fill = NA, color = NA),
    legend.spacing = ggplot2::unit(0.1, "cm"),
    strip.background = ggplot2::element_rect(fill = "#F0F0F0", color = "black", linewidth = 0.3),
    strip.text = ggplot2::element_text(size = base_size - 1, face = "bold"),
    strip.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 2, b = 2)),
    strip.text.y = ggplot2::element_text(angle = 0),
    plot.margin = ggplot2::margin(t = 6, r = 6, b = 6, l = 6)
  )
  
  t
}

#' IEEE theme for wide figures (full page width)
#' @export
ieee_theme_wide <- function(base_size = 9, font_family = "sans") {
  ieee_theme(base_size = base_size, font_family = font_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = base_size + 3),
      axis.title = ggplot2::element_text(size = base_size + 1),
      legend.position = "right",
      legend.direction = "vertical"
    )
}

#' IEEE theme for tall figures
#' @export
ieee_theme_tall <- function(base_size = 8, font_family = "sans") {
  ieee_theme(base_size = base_size, font_family = font_family) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )
}

#' IEEE theme for scatter plots with emphasis
#' @export
ieee_theme_scatter <- function(base_size = 8) {
  ieee_theme(base_size = base_size, grid_major = TRUE, grid_minor = TRUE) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(linewidth = 0.3, color = "#D0D0D0"),
      panel.grid.minor = ggplot2::element_line(linewidth = 0.15, color = "#E8E8E8")
    )
}

#' IEEE theme for time series
#' @export
ieee_theme_timeseries <- function(base_size = 8) {
  ieee_theme(base_size = base_size, grid_major = TRUE) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.25, color = "#CCCCCC"),
      panel.grid.major.x = ggplot2::element_line(linewidth = 0.15, color = "#DDDDDD"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}

#' IEEE theme for bar charts
#' @export
ieee_theme_bar <- function(base_size = 8) {
  ieee_theme(base_size = base_size, grid_major = TRUE) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = base_size - 1)
    )
}

#' IEEE figure dimensions for single column
#' @export
ieee_dim_single <- list(width = 3.5, height = 2.5, dpi = 600)

#' IEEE figure dimensions for double column
#' @export
ieee_dim_double <- list(width = 7.16, height = 3.5, dpi = 600)

#' IEEE figure dimensions for square
#' @export
ieee_dim_square <- list(width = 3.5, height = 3.5, dpi = 600)

#' IEEE figure dimensions for wide
#' @export
ieee_dim_wide <- list(width = 7.16, height = 2.5, dpi = 600)

#' IEEE color palette (colorblind-friendly)
#' @export
ieee_colors <- list(
  primary    = c("#0072BD", "#D95319", "#EDB120", "#7E2F8E", "#77AC30", "#4DBEEE", "#A2142F"),
  sequential = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594"),
  diverging  = c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"),
  grayscale  = c("#000000", "#1A1A1A", "#333333", "#4D4D4D", "#666666", "#808080", "#999999", "#B3B3B3", "#CCCCCC", "#E6E6E6"),
  blue       = "#0072BD",
  orange     = "#D95319",
  yellow     = "#EDB120",
  purple     = "#7E2F8E",
  green      = "#77AC30",
  cyan       = "#4DBEEE",
  red        = "#A2142F",
  gray       = "#777777"
)

#' Get IEEE palette for n items
#' @param n Number of colors needed
#' @param type Palette type: "primary", "sequential", "diverging", "grayscale"
#' @export
get_ieee_palette <- function(n, type = "primary") {
  base_colors <- switch(type,
    primary = ieee_colors$primary,
    sequential = ieee_colors$sequential,
    diverging = ieee_colors$diverging,
    grayscale = ieee_colors$grayscale,
    ieee_colors$primary
  )
  
  if (n <= length(base_colors)) {
    return(base_colors[1:n])
  }
  
  colorRampPalette(base_colors)(n)
}

#' IEEE fill scale
#' @export
scale_fill_ieee <- function(..., n = 8, type = "primary") {
  ggplot2::scale_fill_manual(values = get_ieee_palette(n, type), ...)
}

#' IEEE color scale
#' @export
scale_color_ieee <- function(..., n = 8, type = "primary") {
  ggplot2::scale_color_manual(values = get_ieee_palette(n, type), ...)
}

#' IEEE gradient fill scale
#' @export
scale_fill_ieee_gradient <- function(..., type = "sequential") {
  colors <- switch(type,
    sequential = c("#F7FBFF", "#084594"),
    diverging = c("#67001F", "#053061"),
    heat = c("#FFFFB2", "#BD0026"),
    c("#FFFFFF", "#0072BD")
  )
  ggplot2::scale_fill_gradient(low = colors[1], high = colors[2], ...)
}

#' IEEE gradient color scale
#' @export
scale_color_ieee_gradient <- function(..., type = "sequential") {
  colors <- switch(type,
    sequential = c("#F7FBFF", "#084594"),
    diverging = c("#67001F", "#053061"),
    heat = c("#FFFFB2", "#BD0026"),
    c("#FFFFFF", "#0072BD")
  )
  ggplot2::scale_color_gradient(low = colors[1], high = colors[2], ...)
}

#' IEEE grayscale fill scale
#' @export
scale_fill_ieee_gray <- function(...) {
  grays <- c("#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")
  ggplot2::scale_fill_manual(values = grays, ...)
}

#' IEEE grayscale color scale
#' @export
scale_color_ieee_gray <- function(...) {
  grays <- c("#000000", "#1A1A1A", "#333333", "#4D4D4D", "#666666", "#808080", "#999999")
  ggplot2::scale_color_manual(values = grays, ...)
}

#' Add equation annotation to plot
#' @param model_name Name of model
#' @param equation Equation string
#' @param r2 R-squared value
#' @param x Position x (default 0.05)
#' @param y Position y (default 0.95)
#' @param size Text size
#' @export
annotate_model <- function(model_name, equation, r2, x = 0.05, y = 0.95, size = 3) {
  ggplot2::annotate("text", x = Inf, y = Inf,
                    label = paste0(model_name, "\n", equation, "\nR² = ", format(r2, digits = 4)),
                    hjust = 1.1, vjust = 1.5, size = size, fontface = "bold",
                    family = "mono")
}

#' Add reference line with annotation
#' @export
annotate_reference <- function(intercept = NULL, slope = NULL, label = "", ...) {
  if (!is.null(intercept) && is.null(slope)) {
    ggplot2::geom_hline(yintercept = intercept, linetype = "dashed", color = "#666666", linewidth = 0.3, ...)
  } else if (!is.null(slope) && is.null(intercept)) {
    ggplot2::geom_abline(slope = slope, intercept = if (is.null(intercept)) 0 else intercept,
                         linetype = "dashed", color = "#666666", linewidth = 0.3, ...)
  } else {
    ggplot2::geom_hline(yintercept = intercept, linetype = "dashed", color = "#666666", linewidth = 0.3, ...)
  }
}

#' Create IEEE-standard axis labels
#' @export
ieee_labels <- function(x_lab = "", y_lab = "", title = "", subtitle = "", caption = "") {
  ggplot2::labs(
    x = x_lab,
    y = y_lab,
    title = title,
    subtitle = subtitle,
    caption = caption
  )
}

#' Format axis numbers in IEEE style (scientific for large numbers)
#' @export
ieee_format_axis <- function(scale = 1, accuracy = 1, scientific = FALSE) {
  if (scientific) {
    scales::label_scientific()
  } else {
    scales::label_number(scale = scale, accuracy = accuracy, big.mark = ",")
  }
}

#' Set IEEE-standard axis limits with expansion
#' @export
ieee_coord_cartesian <- function(xlim = NULL, ylim = NULL, expand = TRUE) {
  if (expand) {
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = TRUE)
  } else {
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE)
  }
}

#' IEEE theme with minor ticks and grid
#' @export
ieee_theme_detailed <- function(base_size = 8) {
  ieee_theme(base_size = base_size, grid_major = TRUE, grid_minor = TRUE) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_line(linewidth = 0.1, color = "#E8E8E8"),
      axis.ticks.length = ggplot2::unit(0.2, "cm")
    )
}