# ================================================================
# IEEE Theme for Plots
# ================================================================


library(ggplot2)
library(gridExtra)
library(tibble)



# Generate Color Palette Visualization
THEME_COLORS <- IEEE_MAYORGA_THEME_COLORS
generate_palette_png(THEME_COLORS, "PALETTE_VISUALIZATION__IEEE_MAYORGA_THEME_COLORS.png")


LABEL_MAPPING <- c(
  article = "Article",
  article_article = "Article (Revised)",
  article_conference_paper = "Article & Conference Paper",
  article_review = "Article Review",
  conference_paper = "Conference Paper",
  review = "Review"
)

# Define IEEE style themes
ieee_theme <- theme(
  text = element_text(size = 8, family = 'sans'),
  legend.title = element_text(size = 7),
  legend.text = element_text(size = 6),
  legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
  legend.box.margin = margin(t = -7, r = 0, b = 0, l = 0),
  axis.text = element_text(size = 7),
  axis.line = element_line(color = "black", linewidth = 0.25),
  axis.title.x = element_text(size = 8, margin = margin(t = 1)),
  axis.title.y = element_text(size = 8),
  axis.ticks = element_line(color = "black"),
  axis.ticks.length = unit(0.15, "cm"),
  panel.background = element_rect(fill = "white"),
  panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
  panel.grid.minor = element_line(color = "gray95", linewidth = 0.125),
  plot.title = element_text(size = 9, face = "bold", hjust = 0.5, margin = margin(b = 5)),
  plot.margin = margin(t = 1, r = 1, b = 1, l = 1)
)

ieee_theme_template <- theme(
  text = element_text(size = 12, family = "Times New Roman"),
  legend.title = element_text(size = 10),
  legend.text = element_text(size = 8),
  axis.title = element_text(size = 12),
  axis.line = element_line(color = "black", linewidth = 0.25),
  axis.text = element_text(size = 10),
  panel.background = element_rect(fill = "white"),
  panel.grid.major = element_line(color = "gray95", linewidth = 0.25),
  panel.grid.minor = element_line(color = "gray95", linewidth = 0.125),
  plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
  plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
  axis.ticks = element_line(color = "black"),
  axis.ticks.length = unit(0.2, "cm"),
  axis.ticks.length.minor = unit(0.1, "cm")
)



# src/plots/theme_ieee.R
theme_ieee <- function(base_size = 9, base_family = "Times New Roman") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      # Title
      plot.title = ggplot2::element_text(
        face = "bold", hjust = 0.5, size = base_size + 1,
        margin = ggplot2::margin(b = 6)
      ),
      
      # Axes
      axis.title.x = ggplot2::element_text(size = base_size, margin = ggplot2::margin(t = 4)),
      axis.title.y = ggplot2::element_text(size = base_size, margin = ggplot2::margin(r = 4)),
      axis.text    = ggplot2::element_text(size = base_size - 1),
      axis.ticks   = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.ticks.length = grid::unit(2, "pt"),
      
      # Panel & grid
      panel.grid.major.y = ggplot2::element_line(color = "grey80", linewidth = 0.3),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.border       = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.3),
      
      # Legend
      legend.position = "right",
      legend.title    = ggplot2::element_text(size = base_size - 1),
      legend.text     = ggplot2::element_text(size = base_size - 2),
      
      # Margins
      plot.margin = grid::unit(c(0.2, 0.2, 0.2, 0.2), "cm")
    )
}
