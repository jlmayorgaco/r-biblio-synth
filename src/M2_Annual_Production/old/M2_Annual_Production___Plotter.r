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
get_regression_models <- function(x, y) {
  
  annual_production <- data.frame(Year = x, Articles = y)
  annual_production$Year <- as.numeric(as.character(x))
  annual_production$Articles <- as.numeric(as.character(y))

  # Fit models
  models <- list(
    Linear = lm(Articles ~ Year, data = annual_production),
    Exponential = fit_specific_model(exponential_growth, list(r = 0.0651, N0 = 2.87, t0 = 0.01), annual_production),
    Logarithmic = fit_specific_model(logarithmic_growth, list(a = 1, b = 1), annual_production),
    Logistic = fit_specific_model(logistic_growth, list(K = 150, r = 0.1, t0 = 2000), annual_production),
    Gompertz = fit_specific_model(gompertz_growth, list(N0 = 115, Nmax = 15.5, k = 0.1, t0 = 2000, y0 = 0.001), annual_production),
    GompertzDerivative = fit_specific_model(gompertz_growth_derivative, list(N0 = 115, Nmax = 15.5, k = 0.1, t0 = 2000, y0 = 0.001), annual_production),
    Weibull = fit_specific_model(weibull_growth, list(K = 150, r = 0.1, t0 = 2000), annual_production),
    VonBertalanffy = fit_specific_model(vonbertalanffy_growth, list(Linf = 150, k = 0.1, t0 = 2000), annual_production),
    Normal = fit_specific_model(normal_growth, list(mu = mean(annual_production$Year), sigma = sd(annual_production$Year), A = max(annual_production$Articles)), annual_production)
  )

  # Remove NULL models
  models <- Filter(Negate(is.null), models)

  # Debug available models
  message("[DEBUG] Successfully fitted models: ", paste(names(models), collapse = ", "))

  return(models)
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





create_fourier_spectrum_plot <- function(freq, magnitude, output_path, time_unit = "Year") {

  output_file_png <- file.path(output_path, "Fourier_Spectrum_PNG.png")
  output_file_svg <- file.path(output_path, "Fourier_Spectrum_SVG.svg")
  
  # Calculate minimum and maximum values
  MinX <- min(freq, na.rm = TRUE)
  MaxX <- max(freq, na.rm = TRUE)
  MinY <- floor(min(magnitude, na.rm = TRUE))  # Force y-axis to integers
  MaxY <- ceiling(max(magnitude, na.rm = TRUE))
  
  # Calculate break intervals
  Nx <- 6
  Ny <- 6
  DX <- MaxX - MinX
  DY <- MaxY - MinY

  nx <- max(1, ceiling(DX / Nx), na.rm = TRUE)
  ny <- max(1, ceiling(DY / Ny), na.rm = TRUE)

  x_breaks <- seq(MinX, MaxX, by = nx)
  y_breaks <- seq(MinY, MaxY, by = ny) 

  x_label <- paste("Frequency [Cycles per", time_unit, "]")
  y_label <- "Magnitude [Amplitude]"

  # Labels for the plot
  v_labs <- labs(
    title = "Fourier Spectrum",
    x = x_label,
    y = y_label
  )

  # Colors
  colors <- c("#232323", "#8f2802",  "#cfcfcf")  # Similar to your moving average plot
  
  # Plot the Fourier spectrum
  p <- ggplot(
              data.frame(Frequency = freq, Magnitude = magnitude), 
              aes(x = Frequency, y = Magnitude)
  ) +
       geom_line(size = 0.15, linetype = "dashed", color = colors[1]) +
       geom_point(size = 1, shape = 20, color = colors[1]) +
       theme_minimal(base_size = 10) +
       v_labs + 
       scale_y_continuous(
         expand = c(0, 0),
         limits = c(MinY, MaxY),
         breaks = y_breaks
       ) +
       theme(
         plot.title = element_text(hjust = 0.5)
       )

  # Save the plot
  ggsave(filename = output_file_png, plot = p, width = 4, height = 3, dpi = 900)
  ggsave(filename = output_file_svg, plot = p, width = 4, height = 3, device = "svg")
}



create_acf_plot <- function(acf_values, output_path, title = "Autocorrelation Plot") {
  
  # Prepare data
  lag <- seq_along(acf_values)
  df <- data.frame(Lag = lag, Autocorrelation = acf_values)
  
  # Calculate y-axis range and breaks
  MinY <- floor(min(acf_values, na.rm = TRUE))
  MaxY <- ceiling(max(acf_values, na.rm = TRUE))
  y_breaks <- seq(MinY, MaxY, by = 0.1)  # Breaks every 0.1 for autocorrelation
  
  # Labels
  x_label <- "Lag"
  y_label <- "Autocorrelation"
  
  # File names
  output_file_png <- file.path(output_path, "Annual_Articles_AutoCorrelation_TimeSeries.png")
  output_file_svg <- file.path(output_path, "Annual_Articles_AutoCorrelation_TimeSeries.svg")
  
  # Colors
  color_fill <- "#232323" 
  
  # Plot the autocorrelation
  acf_plot <- ggplot(df, aes(x = Lag, y = Autocorrelation)) +
    geom_bar(stat = "identity", fill = color_fill, width = 0.8) +
    theme_minimal(base_size = 10) +
    labs(title = title, x = x_label, y = y_label) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(MinY, MaxY),
      breaks = y_breaks
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = lag
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.major = element_line(size = 0.2),
      panel.grid.minor = element_blank()
    )

  # Save the plot
  ggsave(filename = output_file_png, plot = acf_plot, width = 3.5, height = 2.5, dpi = 900)
  ggsave(filename = output_file_svg, plot = acf_plot, width = 3.5, height = 2.5, device = "svg")
}


create_lomb_scargle_periodogram <- function(x, y, output_path, time_unit = "Year") {
  library(lomb)

  # File paths for saving plots
  output_file_png <- file.path(output_path, "Lomb_Scargle_Periodogram.png")
  output_file_svg <- file.path(output_path, "Lomb_Scargle_Periodogram.svg")

  # Lomb-Scargle Periodogram
  periodogram <- lsp(y, times = x, type = "period", ofac = 10, plot = FALSE)

  # Extract data for plotting
  df <- data.frame(Frequency = 1 / periodogram$scanned, Power = periodogram$power)  # Period (1/Freq) is on X-axis

  # Find the dominant period
  dominant_period <- df$Frequency[which.max(df$Power)]

  # Axis labels with units
  x_label <- paste("Period (", time_unit, ")", sep = "")
  y_label <- "Power"

  # Plot Lomb-Scargle Periodogram
  lomb_plot <- ggplot(df, aes(x = Frequency, y = Power)) +
    geom_line(size = 0.5, color = "#8f2802") +
    geom_point(size = 1, color = "#8f2802") +
    theme_minimal(base_size = 10) +
    labs(
      title = "Lomb-Scargle Periodogram",
      x = x_label,
      y = y_label
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.major = element_line(size = 0.2),
      panel.grid.minor = element_blank()
    )

  # Save the plot as PNG and SVG
  ggsave(filename = output_file_png, plot = lomb_plot, width = 3.5, height = 2.5, dpi = 900)
  ggsave(filename = output_file_svg, plot = lomb_plot, width = 3.5, height = 2.5, device = "svg")

  # Print dominant period
  message("Dominant Period: ", round(dominant_period, 2), " ", time_unit)

  return(list(plot = lomb_plot, dominant_period = dominant_period))
}


create_wavelet_power_spectrum <- function(time, signal, output_path, time_unit = "Year") {
  library(WaveletComp)

  # Ensure time is numeric and sorted
  if (inherits(time, "Date")) {
    time <- as.numeric(time)
  } else if (!is.numeric(time)) {
    stop("Time must be numeric or in Date format.")
  }
  if (any(diff(time) <= 0)) {
    stop("Time values must be in chronological order and unique.")
  }

  # Create input data frame
  df <- data.frame(Time = time, Signal = signal)

  # Compute wavelet transform
  wavelet_result <- analyze.wavelet(df, my.series = "Signal", loess.span = 0, verbose = FALSE)

  # File paths for saving outputs
  output_file_png <- file.path(output_path, "Wavelet_Power_Spectrum.png")
  output_file_svg <- file.path(output_path, "Wavelet_Power_Spectrum.svg")

  # Save the Wavelet Power Spectrum Plot
  png(output_file_png, width = 900, height = 600, res = 150)
  wt.image(wavelet_result,
           main = "Wavelet Power Spectrum",
           legend.params = list(lab = "Power", mar = 5),
           show.date = FALSE,  # Use default numeric axis if time is numeric
           periodlab = paste("Period (", time_unit, ")", sep = ""))
  dev.off()

  svg(output_file_svg, width = 8, height = 5)
  wt.image(wavelet_result,
           main = "Wavelet Power Spectrum",
           legend.params = list(lab = "Power", mar = 5),
           show.date = FALSE,
           periodlab = paste("Period (", time_unit, ")", sep = ""))
  dev.off()

  # Extract dominant period (ignore NA values)
  power_values <- as.numeric(wavelet_result$Power)
  if (length(power_values) == 0 || all(is.na(power_values))) {
    warning("No power values available for wavelet analysis.")
    dominant_period <- NA
  } else {
    valid_indices <- which(!is.na(power_values))
    if (length(valid_indices) > 0) {
      dominant_period <- wavelet_result$Period[which.max(power_values[valid_indices])]
    } else {
      dominant_period <- NA
    }
  }

  if (is.na(dominant_period)) {
    message("No significant periodic components detected.")
    is_periodic <- FALSE
  } else {
    message("Dominant Period Detected: ", round(dominant_period, 2), " ", time_unit)
    is_periodic <- TRUE
  }

  return(list(wavelet = wavelet_result, dominant_period = dominant_period, is_periodic = is_periodic))
}

create_loess_trend <- function(time_values, signal, output_path, time_unit = "Year") {

  # Ensure time is sorted and increasing
  if (any(diff(time_values) <= 0)) stop("Time values must be strictly increasing and unique.")

  # Create a data frame for Loess smoothing
  df <- data.frame(Time = time_values, Signal = signal)

  # Apply Loess smoothing for trend extraction
  loess_trend <- loess(Signal ~ Time, data = df, span = 0.3)  # Adjust 'span' for smoothing
  df$Trend <- predict(loess_trend)

  # File paths for saving plots
  output_file_png <- file.path(output_path, "Loess_Trend.png")
  output_file_svg <- file.path(output_path, "Loess_Trend.svg")

  # Plot the original signal and trend
  library(ggplot2)
  p <- ggplot(df, aes(x = Time)) +
    geom_line(aes(y = Signal), color = "gray", size = 0.7, linetype = "dashed") +
    geom_line(aes(y = Trend), color = "#8f2802", size = 1) +
    labs(title = "Loess Trend Extraction",
         x = time_unit,
         y = "Signal and Trend") +
    theme_minimal()

  # Save the plots
  ggsave(filename = output_file_png, plot = p, width = 8, height = 5, dpi = 300)
  ggsave(filename = output_file_svg, plot = p, width = 8, height = 5, device = "svg")

  # Return results
  return(list(
    trend = df$Trend
  ))
}


create_harmonic_regression <- function(time_values, signal, output_path, time_unit = "Year") {
  
  # Ensure the time series is sorted and unique
  if (any(diff(time_values) <= 0)) stop("Time values must be strictly increasing and unique.")
  
  # Convert time to a numeric range
  time_numeric <- as.numeric(time_values - min(time_values))

  # Define a range of frequencies to test
  freq_range <- seq(0.01, 10, by = 0.01)  # Frequencies to test
  freq_r2_array <- data.frame(Frequency = numeric(), R2 = numeric())  # Store all frequencies and R²
  best_freq <- NULL
  best_r2 <- -Inf
  best_model <- NULL

  # Search for the best-fitting harmonic model
  for (freq in freq_range) {
    sine_term <- sin(2 * pi * freq * time_numeric)
    cosine_term <- cos(2 * pi * freq * time_numeric)
    model <- lm(signal ~ sine_term + cosine_term)
    r2 <- summary(model)$r.squared
    
    # Save frequency and R² to the array
    freq_r2_array <- rbind(freq_r2_array, data.frame(Frequency = freq, R2 = r2))
    
    if (r2 > best_r2) {
      best_r2 <- r2
      best_freq <- freq
      best_model <- model
    }
  }

  # Generate harmonic fit using the best frequency
  fitted_values <- predict(best_model)

  # Prepare data for plotting
  df <- data.frame(
    Time = time_values,
    Signal = signal,
    Harmonic_Fit = fitted_values
  )

  # File paths for saving plots
  output_file_fit_png <- file.path(output_path, "Harmonic_Regression_Fit.png")
  output_file_fit_svg <- file.path(output_path, "Harmonic_Regression_Fit.svg")
  output_file_r2_png <- file.path(output_path, "Harmonic_R2_vs_Frequency.png")
  output_file_r2_svg <- file.path(output_path, "Harmonic_R2_vs_Frequency.svg")
  output_file_csv <- file.path(output_path, "Harmonic_Frequencies_R2.csv")

  # Plot original signal and harmonic fit
  library(ggplot2)
  p_fit <- ggplot(df, aes(x = Time)) +
    geom_line(aes(y = Signal), color = "gray", size = 0.7, linetype = "dashed") +
    geom_line(aes(y = Harmonic_Fit), color = "#8f2802", size = 1) +
    annotate("rect", xmin = min(df$Time) + 2, xmax = min(df$Time) + 10, 
             ymin = max(df$Signal) - 20, ymax = max(df$Signal), fill = "white", color = "black") +
    annotate("text", x = min(df$Time) + 6, y = max(df$Signal) - 5, hjust = 0,
             label = paste(
               "Best Frequency: ", round(best_freq, 3), " cycles/year\n",
               "R²: ", round(best_r2, 3),
               sep = ""
             ), size = 3) +
    labs(
      title = "Harmonic Regression Fit",
      x = time_unit,
      y = "Signal and Harmonic Fit"
    ) +
    theme_minimal()

  # Save the harmonic regression fit plot
  ggsave(filename = output_file_fit_png, plot = p_fit, width = 8, height = 5, dpi = 300)
  ggsave(filename = output_file_fit_svg, plot = p_fit, width = 8, height = 5, device = "svg")

  # Plot R² vs Frequency
  p_r2 <- ggplot(freq_r2_array, aes(x = Frequency, y = R2)) +
    geom_line(color = "#8f2802", size = 1) +
    geom_point(color = "#232323", size = 1.5) +
    labs(
      title = "R² vs. Frequency for Harmonic Regression",
      x = "Frequency (cycles per year)",
      y = "R² Value"
    ) +
    theme_minimal() +
    annotate("text", x = best_freq, y = best_r2, label = paste0("Best Frequency: ", round(best_freq, 3), "\nR²: ", round(best_r2, 3)),
             hjust = 1.1, vjust = -0.5, size = 3.5, color = "blue")

  # Save the R² vs. Frequency plot
  ggsave(filename = output_file_r2_png, plot = p_r2, width = 8, height = 5, dpi = 300)
  ggsave(filename = output_file_r2_svg, plot = p_r2, width = 8, height = 5, device = "svg")

  # Save frequency and R² array to a CSV file
  write.csv(freq_r2_array, file = output_file_csv, row.names = FALSE)

  # Return results
  return(list(
    best_frequency = best_freq,
    r_squared = best_r2,
    model = best_model,
    freq_r2_array = freq_r2_array
  ))
}
