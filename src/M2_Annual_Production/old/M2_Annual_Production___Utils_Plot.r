# M2_Annual_Production___Utils_Plot.R

create_plot_theme <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      panel.grid.major = element_line(size = 0.2),
      panel.grid.minor = element_blank()
    )
}

add_annotation <- function(plot, x, y, label) {
  plot + annotate(
    "text", x = x, y = y, label = label, hjust = 0, vjust = 1, size = 3.5, color = "black"
  )
}
