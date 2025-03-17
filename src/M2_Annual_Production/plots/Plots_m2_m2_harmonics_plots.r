library(R6)
library(ggplot2)
library(ggfortify)
library(jsonlite)
library(viridis)

# ---------------------------------------------------------------------------- #
# 📌 Improved FFT_Harmonics_Plot (Now in Periods - Years)
# ---------------------------------------------------------------------------- #
FFT_Harmonics_Plot <- R6Class("FFT_Harmonics_Plot",
  inherit = R_IEEE_Plot,
  public = list(
    initialize = function(fft_result) {
      super$initialize("FFT Magnitude Spectrum", "Period (Years)", "Magnitude")

      df_fft <- data.frame(
        Frequency = fft_result$frequencies,  # Original frequency in Hz
        Magnitude = fft_result$magnitude
      )

      # Avoid division by zero (handle DC component at Frequency = 0)
      df_fft$Period <- ifelse(df_fft$Frequency > 0, 1 / df_fft$Frequency, Inf)

      # Remove the infinite period (DC component) from plotting
      df_fft <- df_fft[df_fft$Period != Inf, ]

      # Sort by Period in ascending order for correct visualization
      df_fft <- df_fft[order(df_fft$Period, decreasing = FALSE), ]

      # Remove noise (values below a threshold)
      threshold <- 0.01 * max(df_fft$Magnitude)  # 1% of max peak as threshold
      df_fft <- df_fft[df_fft$Magnitude > threshold, ]

      # Check if there are any significant peaks
      if (nrow(df_fft) == 0) {
        message("[INFO] No significant peaks found in FFT. Skipping plot.")
        self$plot <- NULL
        self$metrics <- list(dominant_period = NA, max_magnitude = NA)
        return()
      }

      # Identify the dominant period
      dominant_index <- which.max(df_fft$Magnitude)
      dominant_period <- df_fft$Period[dominant_index]
      dominant_magnitude <- df_fft$Magnitude[dominant_index]

      # X-axis improvements
      x_breaks <- scales::pretty_breaks(n = 6)(df_fft$Period)  # Dynamically choose 6 readable ticks

      # Plot with Period instead of Frequency
      self$plot <- ggplot(data = df_fft, aes(x = Period, y = Magnitude)) +
        geom_line(linewidth = 0.7, color = "black") +
        scale_x_continuous(trans = "log", breaks = x_breaks) +  # Log scale & improved tick placement
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +  # Add space for annotations
        super$getTheme() +
        geom_vline(xintercept = dominant_period, linetype = "dashed", color = "red") + 
        geom_text(
          aes(x = dominant_period, y = dominant_magnitude, 
              label = paste0("Dominant: ", round(dominant_period, 2), " yrs")),
          color = "red", hjust = -0.1, vjust = -0.5, size = 5
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate X-axis labels

      # Metrics with dominant period
      self$metrics <- list(
        dominant_period = dominant_period,
        max_magnitude = max(df_fft$Magnitude)
      )
    }
  )
)



# ---------------------------------------------------------------------------- #
# 📌 Lomb-Scargle Periodogram Plot Class
# ---------------------------------------------------------------------------- #
Lomb_Scargle_Plot <- R6Class("LombScargle_Plot",
  inherit = R_IEEE_Plot,
  public = list(
    initialize = function(lomb_result) {
      super$initialize("Lomb-Scargle Periodogram", "Period (Years)", "Power Spectrum")

      df_lsp <- data.frame(
        Frequency = lomb_result$scanned, 
        Power = lomb_result$power
      )

      # Convert frequency to period (Years)
      df_lsp$Period <- ifelse(df_lsp$Frequency > 0, 1 / df_lsp$Frequency, NA)
      df_lsp <- df_lsp[!is.na(df_lsp$Period), ]  
      df_lsp <- df_lsp[order(df_lsp$Period), ]  # Sort periods

      # Find Peaks (Keep Only Strong Peaks)
      peak_indices <- find_peaks(df_lsp$Power, n = 5)
      peaks_df <- df_lsp[peak_indices, ]
      
      # **Filter Peaks That Are Meaningful**
      power_threshold <- 0.1 * max(df_lsp$Power)  # 10% of max power
      peaks_df <- peaks_df[peaks_df$Power > power_threshold, ]  

      # Generate the Plot
      self$plot <- ggplot(data = df_lsp, aes(x = Period, y = Power)) +
        geom_line(linewidth = 0.8, color = "black") +  # IEEE-style monochrome
        scale_x_continuous(
          trans = "log10",
          breaks = c(1, 5, 10, 20, 50, 100),
          labels = c("1", "5", "10", "20", "50", "100")
        ) +
        super$getTheme()

      # Add Peaks (Only if There Are Valid Ones)
      if (nrow(peaks_df) > 0) {
        self$plot <- self$plot +
          geom_point(data = peaks_df, aes(x = Period, y = Power), 
                     color = "red", shape = 8, size = 3) +
          geom_vline(data = peaks_df, aes(xintercept = Period), 
                     linetype = "dashed", color = "red", linewidth = 0.5) +
          geom_text(data = peaks_df, aes(x = Period, y = Power, 
                        label = paste0(round(Period, 1), " yrs")), 
                    color = "red", hjust = "right", vjust = -1.2, size = 4, angle = 30)  # 🔹 Rotate Text to Avoid Overlap
      }

      # Store Metrics (Only Significant Peaks)
      self$metrics <- list(
        dominant_periods = ifelse(nrow(peaks_df) > 0, peaks_df$Period, NA),
        peak_powers = ifelse(nrow(peaks_df) > 0, peaks_df$Power, NA)
      )
    }
  )
)





# ---------------------------------------------------------------------------- #
# 📌 R² vs Frequency Plot Class
# ---------------------------------------------------------------------------- #
R2_vs_Frequency_Plot <- R6Class("R2_vs_Frequency_Plot",
  inherit = R_IEEE_Plot,
  public = list(
    initialize = function(r_squared_table) {
      super$initialize(expression(R^2 ~ "vs Frequency"), "Frequency (Hz)", expression(R^2))

      df_r2 <- data.frame(Frequency = r_squared_table$Frequency, R2 = r_squared_table$R2)

      self$plot <- ggplot(df_r2, aes(x = Frequency, y = R2)) +
        geom_line(color = "black", linewidth = 0.7) +
        geom_point(color = "red", size = 1.5) +
        super$getTheme()

      self$metrics <- list(
        best_frequency = df_r2$Frequency[which.max(df_r2$R2)],
        best_r_squared = max(df_r2$R2)
      )
    }
  )
)

# ---------------------------------------------------------------------------- #
# 📌 Wavelet Power Spectrum Plot Class
# ---------------------------------------------------------------------------- #
Wavelet_Power_Spectrum_Plot <- R6Class("Wavelet_Power_Spectrum_Plot",
  inherit = R_IEEE_Plot,
  public = list(
    initialize = function(wavelet_result, years) {
      super$initialize("Wavelet Power Spectrum", "Year", "Period (Years)")

      df_wavelet <- expand.grid(Time = wavelet_result$time, Period = wavelet_result$period)
      df_wavelet$Power <- c(wavelet_result$power)

      self$plot <- ggplot(data = df_wavelet, aes(x = Time, y = Period, fill = Power)) +
        geom_tile() +
        scale_fill_viridis_c() +
        scale_x_continuous(
          breaks = seq(min(df_wavelet$Time), max(df_wavelet$Time), length.out = 6),  # Reduce number of ticks
          labels = round(seq(min(years), max(years), length.out = 6))  # Match real years
        ) +
        super$getTheme() +
        theme(
          legend.position = "right",  # Corrected typo (was "rigth")
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10)  # Rotate x-axis labels
        )

      self$metrics <- list(
        max_power_period = df_wavelet$Period[which.max(df_wavelet$Power)]
      )
    }
  )
)
