# Define a color palette
THEME_COLORS <- c(
  "Blue" = "#3785cf",
  "Green" = "#46a152",
  "Orange" = "#e85803",
  "Yellow" = "#ffc103",
  "Brown" = "#8f2802",
  "DarkBlue" = "#0a2967",
  "Darkgrey" = "#232323"
)

#c("sans", "serif", "mono")

# Define IEEE style theme
ieee_theme <- theme(

    text = element_text(size = 8, family = 'sans'),  # Compact text size

    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Reduce space around the legend
    legend.box.margin = margin(t = -7, r = 0, b = 0, l = 0),  # Move the legend closer to the plot
    
    axis.text = element_text(size = 7),
    axis.line = element_line(color = "black", size = 0.25, linetype = "solid"),
    axis.title.x = element_text(size = 8, margin = margin(t = 1)),  # Add space above x-axis title
    axis.title.y = element_text(size = 8),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.length.minor = unit(0.075, "cm"),
    
    panel.background = element_rect(fill = "white"),  # White background
    panel.grid.major = element_line(color = "gray90", size = 0.25),  # Grid lines on
    panel.grid.minor = element_line(color = "gray95", size = 0.125),
    
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5, margin = margin(b = 5)),  # Centered title
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1)  # Reduced margins
)


ieee_theme_template <- theme(
    text = element_text(size = 12, family = "Times New Roman"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.title = element_text(size = 12),
    axis.line = element_line(color = "black", size = 0.25, linetype = "solid"),
    axis.text = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray95", size = 0.25),
    panel.grid.minor = element_line(color = "gray95", size = 0.125),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks.length.minor = unit(0.1, "cm")
)