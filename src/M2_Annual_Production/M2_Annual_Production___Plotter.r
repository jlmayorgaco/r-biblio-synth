# Load necessary libraries
library(ggplot2)
library(ggsci)



# Function to add a valid line to the plot
add_line_if_valid <- function(plot, data, color, linetype, model_name, best_model) {
  if (model_name == best_model) {
    plot <- plot + geom_line(data = data, aes(x = Year, y = Articles), color = plotters_colors[color], linetype = linetype)
  }
  return(plot)
}

# Function to add a vertical line to the plot if valid
add_vline_if_valid <- function(plot, xintercept, color, linetype, label = NULL) {
  if (!is.na(xintercept)) {
    plot <- plot + geom_vline(xintercept = xintercept, linetype = linetype, color = plotters_colors[color])
    if (!is.null(label)) {
      plot <- plot + annotate("text", x = xintercept, y = Inf, label = label, angle = 90, vjust = -0.5, hjust = 1, color = plotters_colors[color])
    }
  }
  return(plot)
}

# Function to save plots with moving averages
save_moving_average_plots <- function(df_title, df_name, year_column, df, moving_avgs, window_sizes) {
  # Remove rows with NA values
  df <- clean_data(df)

  # Create directory for saving plots
  dir_path <- "results/M2_Annual_Production"
  ensure_directory(dir_path)

  

  # Plot all moving averages together
  p_all <- ggplot(df, aes_string(x = year_column)) +
    geom_line(aes(y = df[[df_name]]), color = "gray45", linetype = "solid") +
    geom_point(aes(y = df[[df_name]]), color = "black", size = 2, shape = 20) +
    theme_minimal(base_size = 10) +
    scale_color_npg() +
    ieee_theme +
    labs(title = paste0(df_title, " with Moving Averages"), x = "Year", y = df_name)

  for (i in seq_along(moving_avgs)) {
    p_all <- p_all +
      geom_line(data = moving_avgs[[i]], aes_string(y = "Moving_Average"), 
                color = scales::hue_pal()(length(window_sizes))[i], 
                linetype = "dashed")
  }

  ggsave(filename = file.path(dir_path, paste0(df_name, "_all_moving_averages.png")), 
         plot = p_all, width = 3.5, height = 2.5, dpi = 900)

  # Plot each moving average individually
  for (i in seq_along(moving_avgs)) {
    p_individual <- ggplot(df, aes_string(x = year_column)) +
      geom_line(aes(y = df[[df_name]]), color = "gray45", linetype = "solid") +
      geom_point(aes(y = df[[df_name]]), color = "black", size = 2, shape = 20) +
      geom_line(data = moving_avgs[[i]], aes_string(y = "Moving_Average"), 
                color = scales::hue_pal()(length(window_sizes))[i], 
                linetype = "dashed") +
      theme_minimal(base_size = 10) +
      scale_color_npg() +
      ieee_theme +
      labs(title = paste0(df_title, " with ", window_sizes[i], "-Year Moving Average"), 
           x = "Year", y = df_name)

    ggsave(filename = file.path(dir_path, paste0(df_name, "_", window_sizes[i], "_year_moving_average.png")), 
           plot = p_individual, width = 3.5, height = 2.5, dpi = 900)
  }
}



# Function to create the plot
create_moving_average_plot <- function(data, output_path) {


    df <- data
    year_column <- 'Year'
    df_column <- 'Articles'

    # Calculate minimum and maximum values
    MinX <- min(df[[year_column]], na.rm = TRUE)
    MinY <- min(df[[df_column]], na.rm = TRUE)
    MaxX <- max(df[[year_column]], na.rm = TRUE)
    MaxY <- max(df[[df_column]], na.rm = TRUE)

    # Calculate break intervals
    Nx <- 5
    Ny <- 6
    DX <- MaxX - MinX
    DY <- MaxY - MinY

    # Ensure nx and ny are valid
    nx <- max(1, ceiling(DX / Nx), na.rm = TRUE)  # Ensure nx is at least 1
    ny <- max(1, ceiling(DY / Ny), na.rm = TRUE)  # Ensure ny is at least 1

    # Calculate breaks with a check to prevent invalid sequences
    x_breaks <- seq(MinX, MaxX, by = nx)
    x_breaks <- ceiling(x_breaks[x_breaks <= MaxX])  # Ensure breaks do not exceed max value
    y_breaks <- seq(MinY, MaxY, by = ny)
    y_breaks <- ceiling(y_breaks[y_breaks <= MaxY])  # Ensure breaks do not exceed max value

    v_labs <- labs(
        title = "Moving Averages Over the Years",
        x = "Year",
        y = "Moving Average of Articles",
        color = "Type"
    )

    color1 <- THEME_COLORS["Yellow"]
    colors <- c("#8f2802", "#232323",  "#cfcfcf")

    # Plotting the growth rate over the years
    p <- ggplot(data, aes(x = Year, y = Articles, color = Type))
    p <- p + geom_line( size = 0.15, linetype = "dashed")
    p <- p + geom_point( size = 1, shape = 20)
    p <- p + theme_minimal(base_size = 10)
    p <- p + v_labs
    p <- p + ieee_theme 
    p <- p + scale_color_manual(values = colors)
    p <- p + theme(legend.position = "bottom")
    #p <- p + theme_publish()

    p <- p + scale_y_continuous(
      expand = c(0, 0),
      limits = c(MinY, MaxY),
      breaks = y_breaks
      #minor_breaks = seq(min(y_breaks), max(y_breaks), by = ny / 10)  # Adding minor breaks
    )
    p <- p + scale_x_continuous(
      expand = c(0, 0),
      limits = c(MinX, MaxX),
      breaks = x_breaks
      #minor_breaks = seq(min(x_breaks), max(x_breaks), by = nx / 10)  # Adding minor breaks
    )
   

  ggsave(filename = output_path, plot = p, width = 3.5, height = 2.5, dpi = 900)
}


# Function to save individual plots
save_individual_plots <- function(data, output_dir) {
  
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Extract unique types from the data
  types <- unique(data$Type)
  
  # Loop through each type to create and save individual plots
  for (type in types) {
    # Filter data for the current type
    df_type <- subset(data, Type == type)
    
    # Define the output file paths
    output_file_png <- file.path(output_dir, paste0("Plot_", gsub(" ", "_", type), ".png"))
    output_file_svg <- file.path(output_dir, paste0("Plot_", gsub(" ", "_", type), ".svg"))
    
    # Create the plot
    p <- ggplot(df_type, aes(x = Year, y = Articles, color = Type)) +
      geom_line(size = 0.8, linetype = "solid") +
      geom_point(size = 1.5, shape = 20) +
      theme_minimal(base_size = 10) +
      labs(
        title = paste(type, "Over the Years"),
        x = "Year",
        y = "Moving Average of Articles",
        color = "Type"
      ) +
      ieee_theme +
      scale_color_manual(values = c(THEME_COLORS[type])) +
      theme(legend.position = "bottom") +
      scale_y_continuous(expand = c(0, 0), limits = range(df_type$Articles, na.rm = TRUE)) +
      scale_x_continuous(expand = c(0, 0), limits = range(df_type$Year, na.rm = TRUE))
    
    # Save the plot as PNG
    ggsave(filename = output_file_png, plot = p, width = 3.5, height = 2.5, dpi = 900)
    
    # Save the plot as SVG
    ggsave(filename = output_file_svg, plot = p, width = 3.5, height = 2.5, device = "svg")
  }
}