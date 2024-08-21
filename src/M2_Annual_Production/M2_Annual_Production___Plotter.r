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

# Helper function to calculate breaks
calculate_breaks <- function(min_val, max_val, num_intervals) {
  breaks <- seq(min_val, max_val, by = ceiling((max_val - min_val) / num_intervals))
  breaks[breaks <= max_val]
}

# Helper function to convert moving averages to data frames
convert_moving_averages_to_df <- function(moving_averages, labels) {
  lapply(seq_along(moving_averages), function(i) {
    convert_to_df(moving_averages[[i]], labels[i])
  })
}

# Function to parse the parameter string into a named list
parse_params <- function(params_str) {
  params_list <- strsplit(params_str, ",")[[1]]
  params <- setNames(
    as.numeric(sapply(params_list, function(x) strsplit(trimws(x), " = ")[[1]][2])),
    sapply(params_list, function(x) strsplit(trimws(x), " = ")[[1]][1])
  )
  return(params)
}

# Main function to create and save the plot
create_moving_average_multiplot <- function(metric_eda, output_path) {
  
  moving_averages_window_size <- metric_eda$moving_averages_window_size
  moving_averages_arrays <- metric_eda$moving_averages_arrays
    
  output_file_png <- file.path(output_path, "Multiplot_Average_PNG.png")
  output_file_svg <- file.path(output_path, "Multiplot_Average_SVG.svg")

  # Generate labels dynamically based on the moving averages window size
  labels <- sapply(moving_averages_window_size, function(size) {
      paste("Avg ", size, " Years", sep = "")
  })

  # Convert lists to data frames with types
  df_list <- lapply(seq_along(moving_averages_arrays), function(i) {
      convert_to_df(moving_averages_arrays[i], labels[i])
  })

  # Combine all data frames
  combined_df <- do.call(rbind, df_list)

  df <- combined_df
  df_column_name_year <- 'Year'
  df_column_name_articles <- 'Articles'

 # Calculate minimum and maximum values
  MinX <- min(df[[df_column_name_year]], na.rm = TRUE)
  MinY <- min(df[[df_column_name_articles]], na.rm = TRUE)
  MaxX <- max(df[[df_column_name_year]], na.rm = TRUE)
  MaxY <- max(df[[df_column_name_articles]], na.rm = TRUE)

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

  # Generate a dynamic color palette
  num_colors <- length(moving_averages_window_size)
  colors <- colorRampPalette(c("#8f2802", "#232323", "#cfcfcf"))(num_colors)

  # Plotting the growth rate over the years
  p <- ggplot(combined_df, aes(x = Year, y = Articles, color = Type))
  p <- p + geom_line(size = 0.15, linetype = "dashed")
  p <- p + geom_point(size = 1, shape = 20)
  p <- p + theme_minimal(base_size = 10)
  p <- p + v_labs
  p <- p + ieee_theme 
  p <- p + scale_color_manual(values = colors)
  p <- p + theme(legend.position = "bottom")

  p <- p + scale_y_continuous(
    expand = c(0, 0),
    limits = c(MinY, MaxY),
    breaks = y_breaks
  )
  p <- p + scale_x_continuous(
    expand = c(0, 0),
    limits = c(MinX, MaxX),
    breaks = x_breaks
  )

  # Save the plot as PNG and SVG
  ggsave(filename = output_file_png, plot = p, width = 3.5, height = 2.5, dpi = 900)
  ggsave(filename = output_file_svg, plot = p, width = 3.5, height = 2.5, device = "svg")

}

# Function to save individual plots
create_moving_average_individual_plots <- function(metric_eda, output_dir) {

  moving_averages_window_size <- metric_eda$moving_averages_window_size
  moving_averages_arrays <- metric_eda$moving_averages_arrays

  # Generate labels dynamically based on the moving averages window size
  labels <- sapply(moving_averages_window_size, function(size) {
      paste("Avg ", size, " Years", sep = "")
  })

  # Convert lists to data frames with types
  df_list <- lapply(seq_along(moving_averages_arrays), function(i) {
      convert_to_df(moving_averages_arrays[i], labels[i])
  })

  # Combine all data frames
  combined_df <- do.call(rbind, df_list)
  data <- combined_df

  
  # Extract unique types from the data
  types <- unique(data$Type)
  
  # Loop through each type to create and save individual plots
  for (type in types) {
    # Filter data for the current type
    df_type <- subset(data, Type == type)
    
    # Define the output file paths
    output_file_png <- file.path(output_dir, paste0("SinglePlot_", gsub(" ", "_", type), ".png"))
    output_file_svg <- file.path(output_dir, paste0("SinglePlot_", gsub(" ", "_", type), ".svg"))
    
    # Create the plot
    p <- ggplot(df_type, aes(x = Year, y = Articles, color = Type)) +
      geom_line(size = 0.35, linetype = "solid") +
      geom_point(size = 1.25, shape = 20) +
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

# Main function to create and save the plot
create_regression_articles_plots <- function(metric_regression, models_regression, x, y, output_path) {
  
  # Get the model function
  v_model <- models_regression[[metric_regression$model_name]]
  
  # Parse the parameters
  v_params <- parse_params(metric_regression$model_params)
  v_r_squared <- metric_regression$model_r_squared
  
  # Real data
  t_real <- x
  y_real <- y

  # Generate the sequence for regression
  dt <- 0.1
  t_regression <- seq(min(t_real), max(t_real), by = dt)
  y_regression <- do.call(v_model, c(list(t = t_regression), v_params))

 # Create a data frame for plotting
  df_real <- data.frame(t = t_real, y = y_real)
  df_regression <- data.frame(t = t_regression, y = y_regression)

  # Create the plot
  p <- ggplot()
  p <- p + geom_point(data = df_real, aes(x = Year, y = Articles), size = 1.25, shape = 20)
  p <- p + geom_line(data = df_regression, aes(x = t, y = y), color = '#232323', size = 0.35, linetype = "dashed")
  p <- p + labs(title = 'Annual Articles Regression Plot', x = 'Year', y = 'Number of Articles')
  p <- p + theme_minimal(base_size = 10)
  p <- p + ieee_theme
  
  # Add the white square with model name and R²
  label <- paste(metric_regression$model_name, sprintf("R² = %.3f", v_r_squared), sep = "\n")

  p <- p + annotate("text", 
    x = min(t_real) + 0, 
    y = max(y_real) - 0, 
    label = label, 
    hjust = 0, vjust = 1,
    size = 3.5, color = "black"
  )


  # Save the plot as PNG and SVG
  output_file_png <- file.path(output_path, "Annual_Articles_Regression_Plot_PNG.png")
  output_file_svg <- file.path(output_path, "Annual_Articles_Regression_Plot_SVG.svg")
  
  v_k_scaling <- 0.5;
  v_k_width_hight <- 1.5; 
  v_width <- 8.8 * v_k_scaling;
  v_hight <- v_width / v_k_width_hight;

  ggsave(filename = output_file_png, plot = p, width = v_width, height = v_hight, dpi = 900)
  ggsave(filename = output_file_svg, plot = p, width = v_width, height = v_hight, device = "svg")

}


# Main function to create and save the plot
create_diff_nominal_articles_plots <- function(x, y, output_path) {
  
  # Real data
  t_real <- x$Year
  y_real <- y$Articles

  # Calculate the numerical derivative of y with respect to x (Year)
  y_diff <- diff(y_real)
  t_diff <- t_real[-1]  # Remove the first time point to match the length of dy_dt

  # Create a data frame for plotting
  df_diff <- data.frame(Year = t_diff, Articles = y_diff)

  # Create the plot
  p <- ggplot(data = df_diff, aes(x = Year, y = Articles))
  p <- p + geom_point(size = 1.25, shape = 20)
  p <- p + geom_line(size = 0.25, linetype = "solid")
  p <- p + labs(title = 'Derivate Nominal Annual Articles Regression Plot', x = 'Year', y = 'Diff of Articles')
  p <- p + theme_minimal(base_size = 10)
  p <- p + ieee_theme


  # Save the plot as PNG and SVG
  output_file_png <- file.path(output_path, "Nominal_Annual_Diff_Articles_Plot_PNG.png")
  output_file_svg <- file.path(output_path, "Nominal_Annual_Diff_Articles_Plot_SVG.svg")
  
  v_k_scaling <- 0.5;
  v_k_width_hight <- 1.5; 
  v_width <- 8.8 * v_k_scaling;
  v_hight <- v_width / v_k_width_hight;

  ggsave(filename = output_file_png, plot = p, width = v_width, height = v_hight, dpi = 900)
  ggsave(filename = output_file_svg, plot = p, width = v_width, height = v_hight, device = "svg")

}

create_diff_percentage_articles_plots <- function(x, y, output_path) {
  
  # Real data
  t_real <- x$Year
  y_real <- y$Articles

  # Calculate the numerical derivative of y with respect to x (Year)
  t_diff <- t_real[-1] 
  y_diff_percentage <- sapply(1:(length(y_real) - 1), function(i) {
    100 * (y_real[i + 1] - y_real[i]) / (y_real[i] + 0.00001)
  })
  # Create a data frame for plotting
  df_diff <- data.frame(Year = t_diff, Articles = y_diff_percentage)

  # Create the plot
  p <- ggplot(data = df_diff, aes(x = Year, y = Articles))
  p <- p + geom_point(size = 1.25, shape = 20)
  p <- p + geom_line(size = 0.25, linetype = "solid")
  p <- p + labs(title = 'Derivate Nominal Annual Articles Regression Plot', x = 'Year', y = '% Change Articles')
  p <- p + theme_minimal(base_size = 10)
  p <- p + ieee_theme


  # Save the plot as PNG and SVG
  output_file_png <- file.path(output_path, "Percentage_Annual_Diff_Articles_Plot_PNG.png")
  output_file_svg <- file.path(output_path, "Percentage__Annual_Diff_Articles_Plot_SVG.svg")
  
  v_k_scaling <- 0.5;
  v_k_width_hight <- 1.5; 
  v_width <- 8.8 * v_k_scaling;
  v_hight <- v_width / v_k_width_hight;

  ggsave(filename = output_file_png, plot = p, width = v_width, height = v_hight, dpi = 900)
  ggsave(filename = output_file_svg, plot = p, width = v_width, height = v_hight, device = "svg")

}

# Define a custom function to format numbers
format_number <- function(x) {
  if (is.na(x)) {
    return(NA)
  } else if (abs(x) >= 1000) {
    # Format large numbers in scientific notation
    sign <- ifelse(x > 0, 1, -1)
    exponent <- floor(log10(abs(x)))
    mantissa <- sign * abs(x) / 10^exponent
    return(sprintf("%.1fx10^%d", mantissa, exponent))
  } else if (abs(x) >= 1) {
    return(formatC(x, format = "f", digits = 2))
  } else if (abs(x) < 1) {
    return(formatC(x, format = "f", digits = 4))
  }
}



create_regression_articles_table <- function(table, path){

      # Save the plot as PNG and SVG
      output_tex <- file.path(path, "models_table_ieee.tex")
      output_svg <- file.path(path, "models_table_ieee.svg")

      models_table_unformatted <- table[, !names(table) %in% c("Parameters")]

      # Apply the formatting function to all numeric columns
      models_table <- as.data.frame(lapply(models_table_unformatted, function(column) {
        if (is.numeric(column)) {
          return(column)
          #return(sapply(column, format_number))
        } else {
          return(column)
        }
      }))

      # Create the table in LaTeX format using kableExtra
      kable_ieee1 <- kable(models_table, format = "latex", booktabs = TRUE, linesep = "")

      # Save the LaTeX table to a .tex file
      save_kable(kable_ieee1, file = output_tex)

      # Convert the LaTeX table to SVG
      # Note: You'll need to have LaTeX installed and configured with the svg package.
      # system(paste("pdflatex -interaction=batchmode ", output_tex, "; pdf2svg models_table_ieee.pdf", output_svg))
}