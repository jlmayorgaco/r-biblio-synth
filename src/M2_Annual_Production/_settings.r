# ---------------------------------------------------------------------------- #
# -- _settings.r: Plot Styling and Export Settings -------------------------- #
# ---------------------------------------------------------------------------- #

# General Plot Theme
ieee_theme <- theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    axis.ticks = element_line(linewidth = 0.1),
    panel.grid.major = element_line(linewidth = 0.2, color = "#dddddd"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.25),
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8, face = "bold")
  )

# Update defaults for geom_line and geom_point
update_geom_defaults("line", list(linewidth = 0.37, color = "#565656"))
update_geom_defaults("point", list(size = 2, shape = 16, color = "#565656"))

# Color Palettes
plot_colors <- list(
  primary = "#1b3c59",      # A dark blue for primary elements
  secondary = "#5a5a5a",    # A neutral gray for secondary elements
  tertiary = "#e4e4e4",     # A light gray for backgrounds or secondary highlights
  highlight = "#5a5a5a"     # A deep red for accents or highlights
)

# Export Settings
export_settings <- list(
  plot_width = 8,         # Width in inches
  plot_height = 5,        # Height in inches
  dpi = 300,              # Resolution for images
  json_pretty = TRUE,     # JSON formatting
  auto_unbox = TRUE       # Automatically unbox single elements in JSON
)

# File Naming Settings
file_naming <- list(
  prefix = "M2_",
  plot_extension = list(png = ".png", svg = ".svg"),
  json_extension = ".json"
)

# Default Breaks for Plots
break_settings <- list(
  x_breaks = 10,
  y_breaks = 5
)
