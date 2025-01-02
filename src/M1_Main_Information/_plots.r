# ---------------------------------------------------------------------------- #
# File: _plots.r
# Description: Functions to generate and save various types of plots
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# Function: Generate Bar Plot with New Theme
# ---------------------------------------------------------------------------- #
generate_bar_plot <- function(data, title, x_label, y_label, x_var, y_var, threshold_var = NULL, file_name = NULL) {
  message("[DEBUG] Generating bar plot...")
  message("[DEBUG] Data for the plot:")
  print(head(data))
  message("[DEBUG] x_var: ", x_var)
  message("[DEBUG] y_var: ", y_var)
  message("[DEBUG] x_label: ", x_label)
  message("[DEBUG] y_label: ", y_label)

  # Calculate threshold information if provided
  if (!is.null(threshold_var)) {
    data$cumulative_percentage <- cumsum(data[[y_var]]) / sum(data[[y_var]])
    threshold_row <- which(data$cumulative_percentage >= 0.8)[1]
    max_x_value <- max(data[[y_var]], na.rm = TRUE)
  } else {
    threshold_row <- NULL
    max_x_value <- NULL
  }

  # Create the bar plot
  bar_plot <- ggplot(data, aes(x = reorder(.data[[x_var]], -(.data[[y_var]])), y = .data[[y_var]])) +
    geom_bar(
      stat = "identity",
      fill = THEME_COLORS$Main[1], # Use the first main theme color
      color = THEME_COLORS$Grayscale$Black, # Border color
      linewidth = 0.3
    ) +
    coord_flip() +
    scale_y_continuous(expand = c(0, 0)) + # Set y-axis limits starting at 0
    labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    ieee_theme +
    theme(
      plot.title = element_text(
        size = 16, face = "bold", hjust = 0.5,
        color = THEME_COLORS$Text$Title,
        margin = margin(t = 10, b = 10)
      ),
      axis.title.x = element_text(
        size = 14, margin = margin(t = 15),
        color = THEME_COLORS$Text$Title
      ),
      axis.title.y = element_text(
        size = 14, margin = margin(r = 15),
        color = THEME_COLORS$Text$Title
      ),
      axis.text = element_text(
        size = 12, color = THEME_COLORS$Text$Body
      ),
      plot.background = element_rect(
        fill = "transparent", color = NA
      ),
      panel.background = element_rect(
        fill = THEME_COLORS$Grayscale$White, color = NA
      )
    )

  # Add threshold line and annotation if threshold information is available
  if (!is.null(threshold_row)) {
    bar_plot <- bar_plot +
      geom_hline(
        yintercept = threshold_row, linetype = "dashed",
        color = THEME_COLORS$Grayscale$DarkGray, linewidth = 1
      ) +
      annotate(
        "text",
        x = threshold_row + 0.25,
        y = max_x_value - 0.75,
        label = "80%",
        color = THEME_COLORS$Grayscale$Black, # Annotation text color
        size = 4
      )
  }

  if (!is.null(file_name)) {
    # Save the plot to the specified directory
    save_plot(bar_plot, file_name, width = 8, height = 6, dpi = 600)
  }

  return(bar_plot)
}







 # ---------------------------------------------------------------------------- #
# Function: Generate Lorenz Curve
# ---------------------------------------------------------------------------- #
generate_lorenz_curve <- function(
  data,  
  value_col, 
  entity_col, 
  plot_title = "Lorenz Curve",
  x_label = "Cumulative Percentage of Values",
  y_label = "Cumulative Percentage of Entities",
  file_name = "lorenz_curve_plot",
  aspect_ratio = 1,
  theme_colors = THEME_COLORS
) {

  # Validate required columns
  if (!all(c(value_col, entity_col) %in% colnames(data))) {
    stop("[ERROR] Missing required columns: ", value_col, " and ", entity_col)
  }

  # Ensure the value column is numeric and valid
  if (!is.numeric(data[[value_col]]) || any(is.na(data[[value_col]]))) {
    stop("[ERROR] '", value_col, "' column contains non-numeric or missing values.")
  }

  # Sort data by the value column in descending order
  data <- data[order(-data[[value_col]]), ]

  # Calculate cumulative percentages
  cumulative_values <- cumsum(data[[value_col]]) / sum(data[[value_col]])
  cumulative_entities <- cumsum(rep(1, nrow(data))) / nrow(data)

  # Calculate Gini coefficient
  area_trapezoids <- (cumulative_values[-1] + cumulative_values[-length(cumulative_values)]) * diff(cumulative_entities) / 2
  gini <- 2 * sum(area_trapezoids) - 1

  # Prepare Lorenz plot data
  lorenz_data <- data.frame(
    CumulativeValues = c(0, cumulative_values, 1),
    CumulativeEntities = c(0, cumulative_entities, 1)
  )
  
# Generate Lorenz plot
lorenz_plot <- ggplot(lorenz_data, aes(x = CumulativeValues, y = CumulativeEntities)) +
  geom_line(color = theme_colors$Main[1], linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = theme_colors$Grayscale$Gray) +
  labs(
    title = plot_title,
    x = x_label,
    y = y_label
  ) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  coord_fixed(ratio = aspect_ratio) +
  annotate(
    "text", x = 0.2, y = 0.8, 
    label = paste0("Gini Coefficient: ", round(gini, 3)),
    color = theme_colors$Text$Body, size = 4, hjust = 0
  ) +
  ieee_theme +
  theme(
    plot.title = element_text(size = 14, margin = margin(b = 15, t = 15), color = theme_colors$Text$Title),
    axis.title.x = element_text(size = 10, margin = margin(t = 15, b = 5), color = theme_colors$Text$Body),
    axis.title.y = element_text(size = 10, margin = margin(r = 15, l = 5), color = theme_colors$Text$Body),
    axis.text = element_text(size = 8, color = theme_colors$Text$Body),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )



  # Save the plot 
  save_plot(
    lorenz_plot, 
    file_name,
    width = 5, height = 5, dpi = 600, aspect_ratio = aspect_ratio
  )

  message("[INFO] Lorenz Curve generated and saved successfully.")
  return(gini)
}


# ---------------------------------------------------------------------------- #
# Function: Generic Pie Chart Plotting
# ---------------------------------------------------------------------------- #
plot_pie_chart <- function(df, title, fill_var, count_var) {
  ggplot(df, aes(x = "", y = .data[[count_var]], fill = .data[[fill_var]])) +
    geom_bar(width = 1, stat = "identity", color = THEME_COLORS$Grayscale$Black, size = 0.25) +
    coord_polar("y", start = 0) +
    labs(
      title = title
    ) +
    theme_void() +
    theme(
      plot.title = element_text(
        size = 16,
        face = "bold",
        hjust = 0.5,
        color = THEME_COLORS$Text$Title,
        margin = margin(t = 10, b = 10)
      ),
      legend.title = element_blank(),
      legend.text = element_text(size = 12, color = THEME_COLORS$Text$Body),
      legend.position = "right"
    ) +
    scale_fill_manual(values = THEME_COLORS$Main)
}


# ---------------------------------------------------------------------------- #
# Function: Generate Bar Plot with Line
# ---------------------------------------------------------------------------- #
generate_bar_plot_with_line <- function(
  data,
  title,
  x_label,
  y_label,
  secondary_y_label,
  x_var,
  y_var,
  line_var,
  file_name = NULL,
  theme_colors = THEME_COLORS
) {


    message("[DEBUG] Generating bar plot...")
  message("[DEBUG] Data for the plot:")
  print(head(data))
  message("[DEBUG] x_var: ", x_var)
  message("[DEBUG] y_var: ", y_var)


  # Validate input columns
  if (!all(c(x_var, y_var, line_var) %in% colnames(data))) {
    stop("[ERROR] Missing required columns in the dataset: ", x_var, ", ", y_var, ", or ", line_var)
  }

 # Create the bar and line plot
bar_line_plot <- ggplot(data, aes(x = reorder(.data[[x_var]], .data[[y_var]]))) +
  # Bar plot for total articles
  geom_bar(aes(y = .data[[y_var]]), stat = "identity", fill = THEME_COLORS$Main[1], color = THEME_COLORS$Grayscale$Black, linewidth = 0.3) +
  coord_flip() +
  # Line plot for secondary variable
  geom_line(aes(y = .data[[line_var]], group = 1, color = "Citations per Year"), size = 1.2) +
  geom_point(aes(y = .data[[line_var]], color = "Citations per Year"), size = 3) +
  # Add scale and labels
  scale_color_manual(
    values = THEME_COLORS$Main[2], # Line color
    name = NULL, # Legend title (set to NULL to hide the title)
    labels = c("Citations per Year") # Legend label
  ) +
  scale_y_continuous(
    expand = c(0, 0), # Ensure y-axis starts at 0
    limits = c(0, max(data[[y_var]], na.rm = TRUE) * 1.01), # Add 0% buffer for aesthetics
    sec.axis = sec_axis(~ ., name = "Citations per Year") # Secondary axis for the line
  ) +
  labs(
    title = title,
    x = x_label,
    y = y_label
  ) +
  ieee_theme +
  theme(
    plot.title = element_text(size = 16, margin = margin(b = 15), hjust = 0.5, color = THEME_COLORS$Text$Title),
    axis.title.x = element_text(size = 12, margin = margin(t = 15), color = THEME_COLORS$Text$Title),
    axis.title.y = element_text(size = 12, margin = margin(r = 15), color = THEME_COLORS$Text$Title),
    axis.text = element_text(size = 10, color = THEME_COLORS$Text$Body),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10), # Adjust plot margins
    legend.position = c(0.9, 0.1), # Bottom right corner
    legend.justification = c(1, 0), # Align the legend to bottom right
    legend.background = element_rect(fill = "white", color = NA, linewidth = 0.2), # Add a background for clarity
    legend.key = element_rect(fill = "white", color = NA), # Transparent legend key
    legend.text = element_text(size = 10, color = THEME_COLORS$Text$Body),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

  # Save the plot if a file name is provided
  if (!is.null(file_name)) {
    save_plot(
      bar_line_plot,
      file_name,
      width = 8,
      height = 6,
      dpi = 600
    )
  }

  

  return(bar_line_plot)
}
# ---------------------------------------------------------------------------- #
# Function: Generate Bubble Chart
# ---------------------------------------------------------------------------- #
generate_bubble_chart <- function(
  data, x_var, y_var, size_var, label_var,
  title, x_label, y_label, size_label
) {
  ggplot(data, aes(
    x = .data[[x_var]],
    y = .data[[y_var]],
    size = .data[[size_var]],
    label = .data[[label_var]]
  )) +
    geom_point(alpha = 0.7, color = THEME_COLORS$Main[2]) +
    geom_text(hjust = 0.5, vjust = -0.5, size = 3, color = THEME_COLORS$Text$Body) +
    labs(
      title = title,
      x = x_label,
      y = y_label,
      size = size_label
    ) +
    ieee_theme +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 15), color = THEME_COLORS$Text$Title),
      axis.title.x = element_text(size = 12, margin = margin(t = 10), color = THEME_COLORS$Text$Body),
      axis.title.y = element_text(size = 12, margin = margin(r = 10), color = THEME_COLORS$Text$Body),
      axis.text = element_text(size = 10, color = THEME_COLORS$Text$Body),
      legend.position = "right",
      legend.title = element_text(size = 12, color = THEME_COLORS$Text$Title),
      legend.text = element_text(size = 10, color = THEME_COLORS$Text$Body),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA)
    )
}



generate_world_map_ggplot <- function(data, file_name, title) {
  ggplot(data, aes(map_id = Country, fill = Articles)) +
    geom_map(map = world_map, aes_string(fill = "Articles"), color = "black", linewidth = 0.2) +
    expand_limits(x = world_map$long, y = world_map$lat) +
    scale_fill_gradient(
      low = THEME_COLORS$Main[1], high = THEME_COLORS$Main[2],
      name = "Number of Articles"
    ) +
    labs(
      title = title
    ) +
    ieee_theme +
    theme_void() +
    theme(
      plot.title = element_text(
        size = 16, face = "bold", hjust = 0.5, color = THEME_COLORS$Text$Title,
        margin = margin(b = 10)
      ),
      legend.position = "bottom"
    ) +
    ggsave(file_name, width = 10, height = 6, dpi = 600)
}


# Generate World Map with Enhanced Features
generate_world_map <- function(
  map_data,
  output_dir = "results/M1_Main_Information/figures",
  value_col = "Articles",
  map_title = "Global Distribution of Articles",
  file_name = "world_map",
  color_scheme = "grayscale"
) {
  # Validate input
  if (!value_col %in% colnames(map_data)) {
    stop("[ERROR] The specified value column does not exist in the data.")
  }

  if (!"Country" %in% colnames(map_data)) {
    stop("[ERROR] The 'Country' column is missing in the data.")
  }

  # Choose color palette
  if (color_scheme == "grayscale") {
    palette <- grey.colors(5, start = 0.9, end = 0.1)
  } else if (color_scheme == "gradient") {
    palette <- RColorBrewer::brewer.pal(9, "Blues")
  } else {
    stop("[ERROR] Invalid color scheme. Choose 'grayscale' or 'gradient'.")
  }

  # Prepare output path
  png_filename <- file.path(output_dir, "figures", paste0(file_name, ".png"))
  dir.create(dirname(png_filename), recursive = TRUE, showWarnings = FALSE)

  # Plot the map
  png(png_filename, width = 1200, height = 800, units = "px")
  par(cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)

  tryCatch({
    mapCountryData(
      map_data,
      nameColumnToPlot = value_col,
      mapTitle = map_title,
      catMethod = "fixedWidth",
      colourPalette = palette,
      addLegend = TRUE,
      borderCol = "#000000"
    )
  }, error = function(e) {
    dev.off() # Close device on error
    stop("[ERROR] Failed to generate world map: ", e$message)
  })

  # Reset parameters and close device
  par(cex.main = 1, cex.axis = 1, cex.lab = 1)
  dev.off()

  # Log success
  message("[INFO] World map saved successfully at: ", png_filename)
}

