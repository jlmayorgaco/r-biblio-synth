library(WaveletComp)
# ---------------------------------------------------------------------------- #
# Metric 2: Harmonic Analysis
# ---------------------------------------------------------------------------- #
M2_M2_Harmonic <- setRefClass(
  "M2_M2_Harmonic",

  # Fields
  fields = list(
    df = "data.frame",
    year_col = "character",
    articles_col = "character",
    results = "list"
  ),

  # Methods
  methods = list(

    # Constructor
    initialize = function(df, year_col = "Year", articles_col = "Articles") {
      .self$df <- df
      .self$year_col <- year_col
      .self$articles_col <- articles_col
      .self$results <- list()
    },

    # Run Harmonic Analysis
    run = function() {
      message(" ==> M2_M2 :: run")

      x <- .self$df[[.self$year_col]]
      y <- .self$df[[.self$articles_col]]

      # Perform FFT
      fft_result <- perform_fft(y)

      # Perform Lomb-Scargle Periodogram
      lomb_result <- calculate_lomb_scargle(x, y)

      # Perform Wavelet Power Spectrum Analysis
      wavelet_result <- calculate_wavelet_power(x, y)

      # Fit Harmonic Regression Models
      harmonic_models <- fit_harmonic_regression(x, y, frequencies = fft_result$frequencies)

      # Evaluate R^2 for each frequency
      r_squared_table <- evaluate_r_squared(harmonic_models, y)

      # Save results
      .self$results <- list(
        fft = list(
          magnitude = fft_result$magnitude,
          phase = fft_result$phase,
          frequencies = fft_result$frequencies
        ),
        lomb_scargle = lomb_result,
        wavelet = wavelet_result,
        harmonic_models = harmonic_models,
        r_squared_table = r_squared_table
      )
    },

    # Save Plots
    save_plot = function(output_path) {
      message(" ==> M2_M2 :: save_plot")

      # **Create Directory Paths**
      one_column_path <- file.path(output_path, "OneColumn")
      double_column_path <- file.path(output_path, "DoubleColumn")
      json_output_path <- gsub("figures", "jsons", output_path)

      x <- .self$df[[.self$year_col]]
      y <- .self$df[[.self$articles_col]]

      # Instantiate Plot Classes
      plot_classes <- list(
        FFT_Harmonics_Plot$new(.self$results$fft),
        Lomb_Scargle_Plot$new(.self$results$lomb_scargle),
        R2_vs_Frequency_Plot$new(.self$results$r_squared_table),
        Wavelet_Power_Spectrum_Plot$new(.self$results$wavelet, years = x)  # Pass years to adjust x-axis
      )

      # Define names for each plot type
      plot_names <- c("FFT_Harmonics", "Lomb_Scargle", "R2_vs_Frequency", "Wavelet_Power")

      # Loop through and save each plot
      for (i in seq_along(plot_classes)) {
        m2_save_plot(plot_classes[[i]], "M2_Harmonics", plot_names[i], one_column_path, double_column_path)
      }

    },

    # Save Results to JSON
    save_json = function(output_path) {
      message(" ==> M2_M2 :: save_json")
      
      # Serialize FFT and Lomb-Scargle Periodogram results
      fft_serialized <- list(
        magnitude = .self$results$fft$magnitude,
        phase = .self$results$fft$phase,
        frequencies = .self$results$fft$frequencies
      )
      
      lsp_serialized <- serialize_lsp(.self$results$lomb_scargle)

      wavelet_serialized <- serialize_wavelet(.self$results$wavelet)
      
      # Combine results
      results_serialized <- list(
        fft = fft_serialized,
        lsp = lsp_serialized,
        wavelet = wavelet_serialized
      )
      
      # Save as JSON
      json_data <- toJSON(results_serialized, pretty = TRUE, auto_unbox = TRUE)
      write(json_data, file = file.path(output_path, "m2_m2_results.json"))
      message("[INFO] JSON saved successfully.")
    }

  )
)

# ---------------------------------------------------------------------------- #
# Plot FFT Results
# ---------------------------------------------------------------------------- #
create_fft_plot <- function(fft_result, output_path) {
  
  # Validate FFT result
  if (!all(c("frequencies", "magnitude") %in% names(fft_result))) {
    stop("[ERROR] FFT result must contain 'frequencies' and 'magnitude' columns.")
  }
  
  # Adjust frequencies length to match magnitude if necessary
  if (length(fft_result$frequencies) != length(fft_result$magnitude)) {
    message("[DEBUG] Adjusting frequencies to match magnitude.")
    fft_result$frequencies <- fft_result$frequencies[1:length(fft_result$magnitude)]
  }

  # Prepare data for plotting
  df_fft <- data.frame(
    Frequency = fft_result$frequencies,
    Magnitude = fft_result$magnitude
  )

  # Create the plot
  p <- ggplot(data = df_fft, aes(x = Frequency, y = Magnitude)) +
    geom_line() +
    labs(
      title = "FFT Magnitude Spectrum",
      x = "Frequency (Hz)",
      y = "Magnitude"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.background = element_rect(fill = "white", color = NA), # White inside axes
      plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      axis.ticks = element_line(linewidth = 0.2),
      axis.line = element_line(color = "black", linewidth = 0.5), # Axis line style
      panel.grid.major = element_line(linewidth = 0.4, color = "#dddddd"), # Major grid lines
      panel.grid.minor = element_line(linewidth = 0.2, color = "#f1f1f1"), # Minor grid lines
      panel.border = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8, face = "bold")
    ) +
    scale_y_continuous(
      breaks = scales::extended_breaks()(df_fft$Magnitude),  # Major grid breaks
      minor_breaks = scales::extended_breaks(n = 10)(df_fft$Magnitude) # Minor grid breaks
    ) +
    scale_x_continuous(
      breaks = scales::extended_breaks()(df_fft$Frequency),  # Major grid breaks
      minor_breaks = scales::extended_breaks(n = 10)(df_fft$Frequency) # Minor grid breaks
    )

  # Define file paths for saving plots
  output_file_png <- file.path(output_path, "FFT_Magnitude_Spectrum.png")
  output_file_svg <- file.path(output_path, "FFT_Magnitude_Spectrum.svg")

  # Save the plot
  ggsave(filename = output_file_png, plot = p, width = 6, height = 4, dpi = 900)
  ggsave(filename = output_file_svg, plot = p, width = 6, height = 4, device = "svg")
}


# ---------------------------------------------------------------------------- #
# Plot Lomb-Scargle Periodogram
# ---------------------------------------------------------------------------- #
create_lomb_scargle_plot <- function(lomb_result, output_path) {
  # Prepare data for plotting
  df_lomb <- data.frame(
    Frequency = lomb_result$scanned,
    Power = lomb_result$power
  )

  # Create Lomb-Scargle plot
  p <- ggplot(df_lomb, aes(x = Frequency, y = Power)) +
    geom_line(linewidth = 0.5, color = plot_colors$primary) +  # Use primary color and adjusted linewidth
    labs(
      title = "Lomb-Scargle Periodogram",
      x = "Frequency (Hz)",
      y = "Power"
    ) +
    ieee_theme +  # Apply IEEE theme
    scale_x_continuous(
      breaks = scales::extended_breaks()(df_lomb$Frequency),
      minor_breaks = scales::extended_breaks(n = 10)(df_lomb$Frequency)
    ) +
    scale_y_continuous(
      breaks = scales::extended_breaks()(df_lomb$Power),
      minor_breaks = scales::extended_breaks(n = 10)(df_lomb$Power)
    )

  # Define file paths for saving plots
  output_file_png <- file.path(output_path, "Lomb_Scargle_Plot.png")
  output_file_svg <- file.path(output_path, "Lomb_Scargle_Plot.svg")

  # Save the plot with specified dimensions
  ggsave(filename = output_file_png, plot = p, width = export_settings$plot_width, height = export_settings$plot_height, dpi = export_settings$dpi)
  ggsave(filename = output_file_svg, plot = p, width = export_settings$plot_width, height = export_settings$plot_height, device = "svg")
}






# ---------------------------------------------------------------------------- #
# Plot R^2 vs Frequency
# ---------------------------------------------------------------------------- #
create_r2_vs_frequency_plot <- function(r_squared_table, output_path) {
  # Validate the R² table structure
  if (!all(c("Frequency", "R2") %in% colnames(r_squared_table))) {
    stop("[ERROR] The R² table must contain 'Frequency' and 'R2' columns.")
  }

  # Create the plot
  p <- ggplot(r_squared_table, aes(x = Frequency, y = R2)) +
    geom_line(color = plot_colors$primary, linewidth = 0.7) +
    geom_point(color = plot_colors$highlight, size = 1.5) +
    labs(
      title = expression(R^2 ~ "vs Frequency"),
      x = "Frequency (Hz)",
      y = expression(R^2)
    ) +
    ieee_theme +
    scale_x_continuous(
      breaks = scales::extended_breaks()(r_squared_table$Frequency),
      minor_breaks = scales::extended_breaks(n = 10)(r_squared_table$Frequency)
    ) +
    scale_y_continuous(
      breaks = scales::extended_breaks()(r_squared_table$R2),
      minor_breaks = scales::extended_breaks(n = 10)(r_squared_table$R2)
    )

  # Add an annotation for the highest R² value
  max_r2 <- max(r_squared_table$R2, na.rm = TRUE)
  max_freq <- r_squared_table$Frequency[which.max(r_squared_table$R2)]

p <- p + geom_text(
  x = max_freq, 
  y = max_r2, 
  label = paste0("Max R²: ", round(max_r2, 3), "\nFreq: ", round(max_freq, 3), " Hz"),
  hjust = -0.1,
  vjust = -0.5,
  size = 3,
  color = plot_colors$secondary
)
  # Save the plot
  output_file_png <- file.path(output_path, "R2_vs_Frequency_Plot.png")
  output_file_svg <- file.path(output_path, "R2_vs_Frequency_Plot.svg")
  ggsave(filename = output_file_png, plot = p, width = 8, height = 5, dpi = 300)
  ggsave(filename = output_file_svg, plot = p, width = 8, height = 5, device = "svg")
}


# ---------------------------------------------------------------------------- #
# Helper Functions
# ---------------------------------------------------------------------------- #
perform_fft <- function(y) {
  n <- length(y)
  fft_result <- fft(y)
  frequencies <- seq(0, 1, length.out = n / 2 + 1)
  magnitude <- Mod(fft_result[1:(n / 2 + 1)])
  phase <- Arg(fft_result[1:(n / 2 + 1)])

  list(frequencies = frequencies, magnitude = magnitude, phase = phase)
}


calculate_lomb_scargle <- function(x, y) {
  library(lomb)
  lsp(y, times = x, type = "period", ofac = 10, plot = FALSE)
}

fit_harmonic_regression <- function(x, y, frequencies) {
  models <- list()
  for (freq in frequencies) {
    formula <- as.formula("y ~ sin(2 * pi * freq * x) + cos(2 * pi * freq * x)")
    fit <- tryCatch(lm(formula, data = data.frame(x = x, y = y)), error = function(e) NULL)
    if (!is.null(fit)) {
      models[[as.character(freq)]] <- list(frequency = freq, fit = fit)
    }
  }
  models
}

evaluate_r_squared <- function(models, y) {
  r_squared <- data.frame(
    Frequency = numeric(0),
    R2 = numeric(0)
  )

  for (model in models) {
    preds <- predict(model$fit, list(x = seq_along(y)))
    r2 <- cor(y, preds)^2
    r_squared <- rbind(r_squared, data.frame(Frequency = model$frequency, R2 = r2))
  }

  r_squared
}

serialize_lsp <- function(lsp_result) {

  if (!inherits(lsp_result, "lsp")) {
    stop("[ERROR] Input is not an lsp object.")
  }

  # Extract relevant components
  result <- list(
    frequency = lsp_result$freq,
    power = lsp_result$power,
    metadata = list(
      type = lsp_result$type,
      ofac = lsp_result$ofac,
      n = lsp_result$n
    )
  )
  
  return(result)
}


# ---------------------------------------------------------------------------- #
# Plot Wavelet Power Spectrum
# ---------------------------------------------------------------------------- #
create_wavelet_power_plot <- function(wavelet_result, years, output_path) {
  # Combine Time and Period with Power
  df_wavelet <- expand.grid(
    Time = wavelet_result$time,
    Period = wavelet_result$period
  )
  df_wavelet$Power <- c(wavelet_result$power)

  # Debugging outputs
  message("[DEBUG] Wavelet data frame structure:")
  print(head(df_wavelet))
  message(' ')
  print(years)

  # Create the plot
 
p <- ggplot(data = df_wavelet, aes(x = Time, y = Period, fill = Power)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_continuous(
    breaks = seq(min(df_wavelet$Time), max(df_wavelet$Time), length.out = 10), # Dynamically set breaks
    labels = round(seq(min(years), max(years), length.out = 10)) # Dynamically match labels to real years
  ) +
  labs(
    title = "Wavelet Power Spectrum",
    x = "Year",
    y = "Period (Years)",
    fill = "Power"
  ) +
  ieee_theme


  output_file_png <- file.path(output_path, "Wavelet_Power_Spectrum.png")
  output_file_svg <- file.path(output_path, "Wavelet_Power_Spectrum.svg")
  
  ggsave(filename = output_file_png, plot = p, width = 6, height = 6, dpi = 300)
  ggsave(filename = output_file_svg, plot = p, width = 6, height = 6, device = "svg")
}

calculate_wavelet_power <- function(time, signal) {
  # Validate inputs
  if (length(time) != length(signal)) {
    stop("[ERROR] Time and signal vectors must have the same length.")
  }

  # Perform wavelet analysis using WaveletComp
  wavelet_result <- WaveletComp::analyze.wavelet(
    my.data = data.frame(Time = time, Signal = signal),
    my.series = "Signal",
    verbose = FALSE
  )

  # Validate output structure
  if (!is.matrix(wavelet_result$Power)) {
    stop("[ERROR] WaveletComp did not return a valid power matrix.")
  }

  # Extract relevant components for further processing
  list(
    time = wavelet_result$axis.1,
    period = wavelet_result$axis.2,
    power = wavelet_result$Power
  )
}



serialize_wavelet <- function(wavelet_result) {
  list(
    time = wavelet_result$time,
    period = wavelet_result$period,
    power = wavelet_result$power
  )
}
