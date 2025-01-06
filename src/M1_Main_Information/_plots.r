# ---------------------------------------------------------------------------- #
# File: plots.R
# Description: Refactored functions for generating and saving various types of plots
# ---------------------------------------------------------------------------- #
library(ggplot2)
library(dplyr)
library(forcats)
library(tidyr)
library(sf)
library(viridis)
library(countrycode)
library(rnaturalearth)    
library(dplyr)
library(countrycode)
library(treemapify)

# ---------------------------------------------------------------------------- #
# Function: Save Plot
# ---------------------------------------------------------------------------- #
save_plot <- function(plot, filename_prefix, width = 4, height = 3, dpi = 300, aspect_ratio = NULL) {

  # Define the output directory
  output_dir <- "results/M1_Main_Information/figures"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Adjust height based on aspect ratio if specified
  if (!is.null(aspect_ratio)) {
    height <- width * aspect_ratio
  }

  # Save as PNG
  tryCatch({
    ggsave(
      filename = file.path(output_dir, paste0(filename_prefix, "_PNG.png")),
      plot = plot,
      width = width,
      height = height,
      dpi = dpi,
      device = "png"
    )
    message("[INFO] PNG plot saved: ", file.path(output_dir, paste0(filename_prefix, "_PNG.png")))
  }, error = function(e) {
    warning("[WARNING] Failed to save PNG plot: ", e$message)
  })

  # Save as SVG
  tryCatch({
    ggsave(
      filename = file.path(output_dir, paste0(filename_prefix, "_SVG.svg")),
      plot = plot,
      width = width,
      height = height,
      dpi = dpi,
      device = "svg"
    )
    message("[INFO] SVG plot saved: ", file.path(output_dir, paste0(filename_prefix, "_SVG.svg")))
  }, error = function(e) {
    warning("[WARNING] Failed to save SVG plot: ", e$message)
  })

  # Save as EPS
  tryCatch({
    ggsave(
      filename = file.path(output_dir, paste0(filename_prefix, "_EPS.eps")),
      plot = plot,
      width = width,
      height = height,
      dpi = dpi,
      device = "eps"
    )
    message("[INFO] EPS plot saved: ", file.path(output_dir, paste0(filename_prefix, "_EPS.eps")))
  }, error = function(e) {
    warning("[WARNING] Failed to save EPS plot: ", e$message)
  })
}
# ---------------------------------------------------------------------------- #
# Function: Generate Bar Plot
# ---------------------------------------------------------------------------- #
generate_bar_plot_horizontal <- function(data, title, x_label, y_label, x_var, y_var, file_name = NULL, add_threshold_line = TRUE, threshold_value = 0.8) {
  # Log input parameters for debugging

  # Ensure y_var is numeric
  if (!is.numeric(data[[y_var]])) {
    stop("[ERROR] y_var must be numeric. Found: ", class(data[[y_var]]))
  }
  
  # Ensure x_var is a factor and reorder based on y_var
  if (!is.factor(data[[x_var]])) {
    message("[INFO] Converting x_var to factor and reordering by y_var.")
    data[[x_var]] <- factor(data[[x_var]], levels = data[[x_var]][order(data[[y_var]], decreasing = FALSE)])
  }
  
  # Calculate cumulative percentage
  data$cumulative_percentage <- cumsum(data[[y_var]]) / sum(data[[y_var]])
  
  # Find the X-axis value at threshold percentage
  threshold_row <- which(data$cumulative_percentage >= threshold_value)[1]
  threshold_x_value <- as.numeric(data[[x_var]][threshold_row])  # Get the factor level
  
  if (add_threshold_line) {
    message("[DEBUG] Threshold X-axis value: ", threshold_x_value)
  }
  
  # Create the bar plot
  bar_plot <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_bar(
      stat = "identity",
      fill = THEME_COLORS$Main[1], # Use the first main theme color
      color = THEME_COLORS$Grayscale$Black, # Border color
      linewidth = 0.3
    ) +
    coord_flip() +
    scale_y_continuous(
      limits = c(0, max(data[[y_var]], na.rm = TRUE)),
      expand = c(0, 0)
    ) +
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

  # Add threshold line and annotation if enabled
  if (add_threshold_line) {
    bar_plot <- bar_plot +
      geom_vline(
        xintercept = threshold_row,  # This defines the vertical line position
        linetype = "dashed",
        color = THEME_COLORS$Grayscale$Black,
        linewidth = 0.8
      ) +
      annotate(
        "text",
        x = threshold_row, # Dynamically center horizontally
        y = 0.9 * max(data[[y_var]], na.rm = TRUE), # Slightly above the threshold line
        label = paste0(threshold_value * 100, "% threshold"),
        hjust = 0.5, # Center horizontally
        vjust = -0.5, # Slightly above the line
        color = THEME_COLORS$Grayscale$Black,
        size = 4
      )
  }

  # Save the plot if file_name is provided
  if (!is.null(file_name)) {
    message("[DEBUG] Saving plot to: ", file_name)
    save_plot(bar_plot, file_name, width = 8, height = 6, dpi = 600)
  }
  
  return(bar_plot)
}


generate_bar_plot_vertical <- function(data, title, x_label, y_label, x_var, y_var, file_name = NULL, add_threshold_line = FALSE) {
  # Define threshold for the cumulative percentage
  threshold_value <- 0.8

  # Log input parameters for debugging
  message("[DEBUG] Entering generate_bar_plot_horizontal2 function")
  message("[DEBUG] Function input parameters:")
  message("       Title: ", title)
  message("       x_label: ", x_label)
  message("       y_label: ", y_label)
  message("       x_var: ", x_var)
  message("       y_var: ", y_var)
  message("       add_threshold_line: ", add_threshold_line)

  # Inspect input data
  message("[DEBUG] Inspecting input data:")
  print(head(data))
  message("[DEBUG] Data column types:")
  print(sapply(data, class))

  # Ensure x_var and y_var exist in the data
  if (!x_var %in% colnames(data)) {
    stop("[ERROR] x_var not found in data: ", x_var)
  }
  if (!y_var %in% colnames(data)) {
    stop("[ERROR] y_var not found in data: ", y_var)
  }

  # Ensure x_var is numeric
  if (!is.numeric(data[[x_var]])) {
    stop("[ERROR] x_var must be numeric. Found: ", class(data[[x_var]]))
  }

  # Ensure y_var is a factor and reorder based on x_var
  if (!is.factor(data[[y_var]])) {
    message("[INFO] Converting y_var to factor and reordering by x_var.")
    data[[y_var]] <- factor(data[[y_var]], levels = data[[y_var]][order(data[[x_var]], decreasing = FALSE)])
  }

  # Calculate cumulative percentage
  data$cumulative_percentage <- cumsum(data[[x_var]]) / sum(data[[x_var]])

  # Find the Y-value at 80% cumulative percentage
  threshold_row <- which(data$cumulative_percentage >= threshold_value)[1]
  threshold_y_value <- levels(data[[y_var]])[threshold_row]

  message("[DEBUG] Threshold Y-value for 80%: ", threshold_y_value)

  # Create the base bar plot
  bar_plot <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_bar(
      stat = "identity",
      fill = THEME_COLORS$Main[1], # Use the first main theme color
      color = THEME_COLORS$Grayscale$Black, # Border color
      linewidth = 0.3
    ) +
    
    scale_x_continuous(
      limits = c(0, max(data[[x_var]], na.rm = TRUE)),
      expand = c(0, 0)
    ) +
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

  # Add threshold line and annotation if enabled
  if (add_threshold_line) {
    bar_plot <- bar_plot +
      geom_vline(
        xintercept = data[[x_var]][threshold_row],
        linetype = "dashed",
        color = THEME_COLORS$Grayscale$Black,
        linewidth = 0.8
      ) +
      annotate(
        "text",
        x = data[[x_var]][threshold_row],
        y = max(seq_along(data[[y_var]])), # Place the label above the threshold line
        label = paste0("80% threshold: ", data[[x_var]][threshold_row]),
        hjust = -0.2,
        vjust = -0.5,
        color = THEME_COLORS$Grayscale$Black,
        size = 4
      )
  }

  # Save the plot if file_name is provided
  if (!is.null(file_name)) {
    message("[DEBUG] Saving plot to: ", file_name)
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
generate_world_map <- function(
  map_data,
  output_dir = "results/M1_Main_Information/figures",
  value_col = "Articles",
  map_title = "Global Distribution of Scientific Articles",
  file_name = "world_map",
  color_scheme = "viridis",
  caption_text = "Source: Custom Analysis"
) {
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  map_data <- map_data %>%
    dplyr::rename(Articles = !!value_col) %>%
    dplyr::mutate(Country = countrycode(Country, "country.name", "country.name"))


  world$name <- countrycode(world$name, "country.name", "country.name")


  # Step 4: Join map_data with world
  world <- dplyr::left_join(world, map_data, by = c("name" = "Country"))

  # Step 5: Identify mismatched countries
  mismatched <- dplyr::anti_join(map_data, world, by = c("Country" = "name"))

  # Step 6: Exclude Antarctica
  world <- world[!world$name %in% c("Antarctica"), ]

  # Step 7: Define color palette
  palette <- switch(color_scheme,
    "viridis" = viridis::viridis(10),
    "blues" = c("#d7ecf5", "#004c91"),
    "reds" = c("#fee5d9", "#de2d26"),
    "greyscale" = grey.colors(10, start = 0.9, end = 0.1),
    "greens" = c("#d0f0c0", "#006400"),
    "oranges" = c("#ffeda0", "#f03b20"),
    "purples" = c("#efedf5", "#756bb1"),
    "cyans" = c("#e0f7fa", "#00796b"),
    grey.colors(10, start = 0.9, end = 0.1) # Default fallback
  )


  # Step 9: Create the map plot
 map_plot <- ggplot(data = world) +
  geom_sf(aes(fill = Articles), color = "black", size = 0.2) +
  scale_fill_gradientn(
    colours = if (is.character(palette)) palette else palette,
    na.value = "lightgray", # Color for missing data
    name = "Articles",
    guide = guide_colorbar(
      barwidth = unit(20, "cm"),  # Adjust width of the gradient bar
      barheight = unit(0.25, "cm") # Adjust height (thickness) of the gradient bar
    )
  ) +
  labs(
    title = map_title
    #caption = "Source: Scopus"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 12, face = "bold", color = "black", vjust = 1.14), # Adjust vertical alignment
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm"), 
    plot.background = element_rect(fill = "transparent", color = NA), # Transparent background
    panel.background = element_rect(fill = "transparent", color = NA) # Transparent panel
  )


  # Step 8: Define output file path

  output_file_base <- file.path(output_dir, file_name)
  dir.create(dirname(output_file_base), recursive = TRUE, showWarnings = FALSE)

  # Step 10: Save the plot
  # Save as PNG
  ggsave(
    paste0(output_file_base, ".png"), plot = map_plot,
    width = 10.5, height = 5, dpi = 1200, bg = "transparent"
  )

  # Save as SVG
  ggsave(
    paste0(output_file_base, ".svg"), plot = map_plot,
    width = 10.5, height = 5, dpi = 1200, bg = "transparent", device = "svg"
  )

  # Save as EPS
  ggsave(
    paste0(output_file_base, ".eps"), plot = map_plot,
    width = 10.5, height = 5, dpi = 1200, bg = "transparent", device = "eps"
  )
}


# ---------------------------------------------------------------------------- #
# Function: Generate Stacked Bar Plot
# ---------------------------------------------------------------------------- #

generate_bar_stacked2_old <- function(data, title, x_label, y_label, file_name) {
  # Ensure numeric types for SCP and MCP
  data <- data %>%
    dplyr::mutate(
      SCP = as.numeric(SCP),
      MCP = as.numeric(MCP)
    )

  # Preprocess data
  top_countries <- data %>%
    dplyr::mutate(Total = SCP + MCP) %>%
    dplyr::arrange(desc(Total)) %>%
    dplyr::slice_head(n = 10) %>%
    dplyr::select(Country, SCP, MCP) %>%
    tidyr::pivot_longer(
      cols = c("SCP", "MCP"),
      names_to = "Collaboration",
      values_to = "Articles"
    )

  # Plot using colors from THEME_COLORS
 stacked_bar_plot <- ggplot(top_countries, aes(
    x = Articles,
    y = forcats::fct_reorder(Country, Articles),
    fill = Collaboration
  )) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = THEME_COLORS$Text$Title),
      axis.text.x = element_text(size = 10, color = THEME_COLORS$Text$Body),
      axis.text.y = element_text(size = 10, color = THEME_COLORS$Text$Body),
      axis.title.x = element_text(size = 12, face = "bold", color = THEME_COLORS$Text$Title),
      axis.title.y = element_text(size = 12, face = "bold", color = THEME_COLORS$Text$Title),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold", color = THEME_COLORS$Text$Title),
      legend.text = element_text(size = 10, color = THEME_COLORS$Text$Body),
      panel.background = element_rect(fill = THEME_COLORS$Grayscale$White, color = NA)
      #plot.background = element_rect(fill = THEME_COLORS$Grayscale$White, color = NA)
    ) +
    scale_fill_manual(values = c(
      SCP = THEME_COLORS$Main[1],  # First color from Main
      MCP = THEME_COLORS$Main[2]   # Second color from Main
    )) +
    scale_x_continuous(expand = c(0, 0))  # Ensure the x-axis starts exactly at 0


  # Save plot as PNG
  ggsave(
    filename = paste0("results/M1_Main_Information/figures/", file_name, ".png"),
    plot = stacked_bar_plot,
    width = 10, height = 7, dpi = 300, bg = "transparent"
  )

  # Save plot as SVG
  ggsave(
    filename = paste0("results/M1_Main_Information/figures/", file_name, ".svg"),
    plot = stacked_bar_plot,
    width = 10, height = 7, dpi = 300, bg = "transparent"
  )

  # Save plot as EPS
  ggsave(
    filename = paste0("results/M1_Main_Information/figures/", file_name, ".eps"),
    plot = stacked_bar_plot,
    width = 10, height = 7, dpi = 300, bg = "transparent"
  )

  message(paste("[INFO] Stacked bar plot saved successfully in PNG, SVG, and EPS formats at: results/M1_Main_Information/figures/", file_name))
}



generate_bar_stacked <- function(data, title, x_label, y_label, file_name, categorical_var_col, col_a, col_b, col_a_label = "Category A", col_b_label = "Category B") {
  # Ensure numeric types for col_a and col_b
  data <- data %>%
    dplyr::mutate(
      col_a_numeric = as.numeric(.data[[col_a]]),
      col_b_numeric = as.numeric(.data[[col_b]])
    )

  # Preprocess data for top categories
  processed_data <- data %>%
    dplyr::mutate(Total = col_a_numeric + col_b_numeric) %>%
    dplyr::arrange(desc(Total)) %>%
    dplyr::slice_head(n = 10) %>%
    tidyr::pivot_longer(
      cols = c(col_a_numeric, col_b_numeric),
      names_to = "Category",
      values_to = "Value"
    ) %>%
    dplyr::mutate(
      Category = dplyr::case_when(
        Category == "col_a_numeric" ~ col_a_label,
        Category == "col_b_numeric" ~ col_b_label,
        TRUE ~ Category
      )
    )

  # Define colors dynamically for the categories
  color_mapping <- setNames(
    c("#1b9e77", "#d95f02"),  # Assign specific colors
    c(col_a_label, col_b_label)  # Map colors to labels
  )

  # Plot using ggplot2
  stacked_bar_plot <- ggplot(processed_data, aes(
    x = Value,
    y = forcats::fct_reorder(.data[[categorical_var_col]], Value),
    fill = Category
  )) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    labs(
      title = title,
      x = x_label,
      y = y_label,
      fill = "Category"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10)
    ) +
    scale_fill_manual(values = color_mapping) +
    scale_x_continuous(expand = c(0, 0))  # Ensure the x-axis starts exactly at 0

  # Save plot as specified
  save_plot(
    filename = file_name,
    plot = stacked_bar_plot,
    width = 8,
    height = 6,
    dpi = 900
  )
  
  message("[INFO] Stacked bar plot saved successfully.")
  return(stacked_bar_plot)
}







# ---------------------------------------------------------------------------- #
# Function: Generate Treemap for Article Distribution
# Description: Creates and saves a treemap for the given data.
# ---------------------------------------------------------------------------- #
generate_treemap <- function(data, value_col, label_col, title = "Global Distribution of Articles (Treemap)", 
                              file_name = "M1_G5_MOST_PROD_COUNTRIES_TREEMAP") {
  tryCatch({
 
   # Sort data so the largest value starts at the top-left
    data <- data %>%
      arrange(desc(.data[[value_col]])) %>%
      mutate(
        label_text = paste0(.data[[label_col]], "\n", .data[[value_col]])
      )


    # Generate the treemap
    treemap_plot <- ggplot(data, aes(
      area = .data[[value_col]],
      fill = .data[[label_col]]
    )) +
      geom_treemap(color = "white", size = 5) + 
      geom_treemap_text(
        aes(label = label_text), # Use the generated label column
        fontface = "bold", # Set label to bold
        colour = "black",
        place = "bottomleft",
        grow = FALSE,
        size = 12, # Adjust size for better readability
        padding.x = grid::unit(3, "mm"), # Add horizontal margin
        padding.y = grid::unit(3, "mm")  # Add vertical margin
      ) +
      scale_fill_brewer(palette = "Spectral") +
      labs(
        title = title,
        fill = NULL # Remove unnecessary legend title
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "none" # Remove legends
      )

    # Save the treemap directly
    save_plot(
      filename = file_name,
      plot = treemap_plot,
      width = 6,
      height = 6,
      dpi = 900
    )

    message("[INFO] Treemap generated and saved successfully.")
  }, error = function(e) {
    message("[ERROR] Failed to generate treemap: ", e$message)
  })
} 