

library(ggplot2)
library(gridExtra)
library(tibble)

# ---------------------------------------------------------------------------- #
# THEME_COLORS Palette
# ---------------------------------------------------------------------------- #
IEEE_MAYORGA_THEME_COLORS <- list(
  Main = c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2", "#59a14f", "#edc949"),
  Grayscale = list(
    Black = "#000000",
    VeryDarkGray = "#2b2b2b",
    DarkGray = "#4d4d4d",
    MediumGray = "#808080",
    LightGray = "#d3d3d3",
    VeryLightGray = "#f2f2f2",
    White = "#ffffff"
  ),
  Categorical = c(
    Lower = "#e15759",
    Medium = "#f28e2b",
    Bigger = "#4e79a7"
  ),
  Sequential = c(
    VeryLight = "#deebf7",
    Light = "#9ecae1",
    Medium = "#3182bd",
    Dark = "#08306b"
  ),
  Diverging = c(
    VeryLow = "#d73027",
    Low = "#fc8d59",
    Neutral = "#fee08b",
    High = "#91bfdb",
    VeryHigh = "#4575b4"
  ),
  Highlight = c(
    Positive = "#1b9e77",
    Negative = "#d95f02",
    Neutral = "#7570b3"
  ),
  Text = list(
    Title = "#333333",
    Subtitle = "#555555",
    Caption = "#888888",
    Axis = "#444444",
    Highlight = "#1b9e77"
  ),
  Background = list(
    Light = "#ffffff",
    Medium = "#f4f4f4",
    Dark = "#2b2b2b"
  )
)

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

