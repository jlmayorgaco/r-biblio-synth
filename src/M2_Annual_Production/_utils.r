# ---------------------------------------------------------------------------- #
# -- _utils.r: Utility Functions --------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Ensure directory exists
# Creates the directory if it does not exist
ensure_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message("Directory created at: ", path)
  }
}


save_json <- function(data, path) {
  if (missing(data) || is.null(data)) {
    stop("Data cannot be NULL when saving JSON.")
  }
  if (missing(path) || path == "") {
    stop("Path cannot be empty.")
  }
  tryCatch(
    {
      library(jsonlite)
      write_json(data, path, pretty = TRUE, auto_unbox = TRUE)
      message("[INFO] JSON saved successfully at: ", path)
    },
    error = function(e) {
      stop("[ERROR] Failed to save JSON: ", e$message)
    }
  )
}

# Save plot with standardized settings
# Saves a ggplot object as a file with the specified parameters
save_plot_m2 <- function(plot, filename, width = 8, height = 5, dpi = 300) {
  if (missing(plot) || !inherits(plot, "gg")) {
    stop("The 'plot' argument must be a valid ggplot object.")
  }
  if (missing(filename) || filename == "") {
    stop("The 'filename' argument cannot be empty.")
  }
  tryCatch(
    {
      library(ggplot2)
      ggsave(filename = filename, plot = plot, width = width, height = height, dpi = dpi)
      message("Plot saved at: ", filename)
    },
    error = function(e) {
      stop("Error saving plot: ", e$message)
    }
  )
}

# Load data from CSV
# Reads a CSV file into a data frame
load_csv <- function(file_path) {
  if (missing(file_path) || file_path == "") {
    stop("File path cannot be empty.")
  }
  if (!file.exists(file_path)) {
    stop("File does not exist at path: ", file_path)
  }
  tryCatch(
    {
      library(readr)
      data <- read_csv(file_path)
      message("CSV loaded successfully from: ", file_path)
      return(data)
    },
    error = function(e) {
      stop("Error loading CSV: ", e$message)
    }
  )
}

# Format numbers for consistent output
# Formats numbers for better readability (e.g., scientific notation or fixed decimals)
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
  } else {
    return(formatC(x, format = "f", digits = 4))
  }
}

# Generate dynamic breaks for plots
# Creates evenly spaced breaks for axes based on the input range
calculate_breaks <- function(min_val, max_val, num_intervals) {
  if (missing(min_val) || missing(max_val)) {
    stop("Both 'min_val' and 'max_val' must be specified.")
  }
  if (num_intervals <= 1) {
    stop("'num_intervals' must be greater than 1.")
  }
  seq(min_val, max_val, length.out = num_intervals)
}

# Validate input data frame
# Checks if the required columns are present in the data frame
validate_dataframe <- function(df, required_columns) {
  if (missing(df) || !is.data.frame(df)) {
    stop("Input must be a data frame.")
  }
  if (missing(required_columns) || length(required_columns) == 0) {
    stop("A list of required columns must be provided.")
  }
  missing_columns <- setdiff(required_columns, colnames(df))
  if (length(missing_columns) > 0) {
    stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
  }
}

# Extract peak value from a column
# Returns the row with the maximum value in the specified column
get_peak <- function(data, column) {
  if (missing(data) || !is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  if (missing(column) || !column %in% colnames(data)) {
    stop("Column '", column, "' not found in the data frame.")
  }
  peak_row <- data[which.max(data[[column]]), , drop = FALSE]
  return(peak_row)
}


# Convert Moving Averages to a Data Frame
convert_to_df <- function(data, label) {
  # Check if the data is in list format
  if (is.list(data)) {
    data <- data[[1]]
  }
  
  # Ensure the data has valid length
  if (is.null(data$Year) || is.null(data$Articles)) {
    stop("Invalid data structure: 'Year' and 'Articles' columns are required.")
  }
  
  # Convert the moving average list to a data frame
  df <- data.frame(
    Year = data$Year,
    Articles = data$Articles,
    Type = label
  )
  
  return(df)
}



fit_model <- function(formula, data, start, model_func_name) {
  tryCatch(
    {
      nlsLM(formula, data = data, start = start)
    },
    error = function(e) {
      message("Error fitting formula for model function: ", model_func_name)
      message("Error fitting model: ", e$message)
      NULL
    }
  )
}


fit_specific_model <- function(model_func, start_params, data) {
  model_func_name <- deparse(substitute(model_func))
  years <- data$Year
  papers <- data$Articles
  formula <- as.formula(paste("papers ~", model_func_name, "(years,", paste(names(start_params), collapse = ", "), ")"))
  fit_model(formula, data, start = start_params, model_func_name = model_func_name)
}


get_regression_models <- function(x, y) {
  annual_production <- data.frame(Year = x, Articles = y)
  annual_production$Year <- as.numeric(as.character(x))
  annual_production$Articles <- as.numeric(as.character(y))

  # Fit models
  linear_model <- lm(Articles ~ Year, data = annual_production)
  polynomial_model <- lm(Articles ~ I(Year) + I(Year^2) + I(Year^3), data = annual_production)
  spline_model <- lm(Articles ~ bs(Year, df = 5), data = annual_production)

  exponential_model <- fit_specific_model(exponential_growth, list(r = 0.0651, N0 = 2.87, t0 = 0.01), annual_production)
  logarithmic_model <- fit_specific_model(logarithmic_growth, list(a = 1, b = 1), annual_production)
  logistic_model <- fit_specific_model(logistic_growth, list(K = 150, r = 0.1, t0 = 2000), annual_production)
  gompertz_model <- fit_specific_model(gompertz_growth, list(N0 = 115, Nmax = 15.5, k = 0.1, t0 = 2000, y0 = 0.001), annual_production)
  gompertz_derivate_model <- fit_specific_model(gompertz_growth_derivative, list(N0 = 115, Nmax = 15.5, k = 0.1, t0 = 2000, y0 = 0.001), annual_production)
  weibull_model <- fit_specific_model(weibull_growth, list(K = 150, r = 0.1, t0 = 2000), annual_production)
  vonbertalanffy_model <- fit_specific_model(vonbertalanffy_growth, list(Linf = 150, k = 0.1, t0 = 2000), annual_production)
  normal_model <- fit_specific_model(normal_growth, list(mu = mean(annual_production$Year), sigma = sd(annual_production$Year), A = max(annual_production$Articles)), annual_production)

  # New models
  richards_model <- fit_specific_model(richards_growth, list(K = 115, r = 0.1, t0 = 2000, nu = 1.5), annual_production)
  fourier_model <- lm(Articles ~ sin(2 * pi * Year / 10) + cos(2 * pi * Year / 10), data = annual_production)

  # ARIMA model for time series data
  #library(forecast)
  #arima_model <- auto.arima(annual_production$Articles)

  # Correct way to forecast ARIMA without `newdata`
  #arima_forecast <- forecast(arima_model, h = 10)  # Forecast 10 steps ahead

  # Collect models
  models <- list(
    Linear = linear_model,
    Polynomial = polynomial_model,
    Spline = spline_model,
    Exponential = exponential_model,
    Logarithmic = logarithmic_model,
    Logistic = logistic_model,
    Gompertz = gompertz_model,
    GompertzDerivate = gompertz_derivate_model,
    Weibull = weibull_model,
    VonBertalanffy = vonbertalanffy_model,
    Normal = normal_model,
    Richards = richards_model,
    Fourier = fourier_model
    #ARIMA = arima_model
  )

  # Remove NULL models
  models <- Filter(Negate(is.null), models)

  return(models)
}


# Define growth model functions
linear_growth <- function(t, a, b) { a * t + b }
exponential_growth <- function(t, r, N0, t0) { N0 * exp(r * (t - t0)) }
logarithmic_growth <- function(t, a, b) { a * log(t) + b }
powerlaw_growth <- function(t, a, b) { a * t^b }
gompertz_growth <- function(t, N0, Nmax, k, t0, y0) {
  N <- y0 + N0 * exp(log(Nmax / N0) * exp(-k * (t - t0)))
  return(N)
}
gompertz_growth_derivative <- function(t, N0, Nmax, k, t0, y0) {
  A <- log(Nmax / N0)
  B <- exp(-k * (t - t0))
  N <- y0 + N0 * exp(A * B)
  dNdt <- -k * A * B * (N - y0)
  return(dNdt)
}
weibull_growth <- function(t, K, r, t0) { K * (1 - exp(-(t/t0)^r)) }
vonbertalanffy_growth <- function(t, Linf, k, t0) { Linf * (1 - exp(-k * (t - t0))) }
normal_growth <- function(t, mu, sigma, A) { A * exp(-0.5 * ((t - mu) / sigma)^2) }
logistic_growth <- function(t, K, r, t0) { K / (1 + exp(-r * (t - t0))) }




# **New Models**
# Richards Growth Model (Generalized Logistic Model)
richards_growth <- function(t, K, r, t0, nu) {
  return(K / (1 + exp(-r * (t - t0)))^nu)
}

# Polynomial Growth Model (Degree 3)
polynomial_growth <- function(t, a, b, c, d) {
  return(a + b * t + c * t^2 + d * t^3)
}

# Fourier Series Model (Captures periodic behavior)
fourier_growth <- function(t, a, b, c) {
  return(a * sin(2 * pi * t / b) + c * cos(2 * pi * t / b))
}

# Model Function Map
model_function_map <- list(
  Linear = linear_growth,
  Exponential = exponential_growth,
  Logarithmic = logarithmic_growth,
  Logistic = logistic_growth,
  Gompertz = gompertz_growth,
  GompertzDerivate = gompertz_growth_derivative,
  Weibull = weibull_growth,
  VonBertalanffy = vonbertalanffy_growth,
  Normal = normal_growth,
  Richards = richards_growth,   # Newly added
  Polynomial = polynomial_growth,  # Newly added
  Fourier = fourier_growth  # Newly added
)


# Function to format numbers for labels
format_number <- function(num) {
  if (num < 1) return(sprintf("%.3f", num))  
  if (num < 10) return(sprintf("%.1f", num))  
  return(sprintf("%.0f", num))  
}



# Function to check normality of residuals
check_residual_normality <- function(data, column) {
  shapiro_test <- shapiro.test(data[[column]])
  message("[INFO] Shapiro-Wilk Test for Normality: W = ", shapiro_test$statistic, ", p-value = ", shapiro_test$p.value)
  if (shapiro_test$p.value < 0.05) {
    message("[WARNING] Residuals are NOT normally distributed (p < 0.05). Consider further analysis.")
  } else {
    message("[INFO] Residuals appear to be normally distributed (p >= 0.05).")
  }
}




# Function to perform and store statistical test results, including FFT harmonic analysis
perform_statistical_tests <- function(data, column, output_path) {
  
  # Initialize results list
  results <- list()

  # **1️⃣ Shapiro-Wilk Test (Normality)**
  shapiro_test <- shapiro.test(data[[column]])
  results$shapiro <- list(
    test = "Shapiro-Wilk Normality Test",
    statistic = shapiro_test$statistic,
    p_value = shapiro_test$p.value,
    interpretation = if (shapiro_test$p.value < 0.05) {
      "Residuals are NOT normally distributed (p < 0.05). Consider transformations or alternative models."
    } else {
      "Residuals appear to be normally distributed (p >= 0.05). No immediate action required."
    }
  )

  # **2️⃣ Durbin-Watson Test (Autocorrelation)**
  dw_test <- dwtest(as.formula(paste(column, "~ Year")), data = data)
  results$durbin_watson <- list(
    test = "Durbin-Watson Autocorrelation Test",
    statistic = dw_test$statistic,
    p_value = dw_test$p.value,
    interpretation = if (dw_test$statistic < 1.5) {
      "Residuals exhibit positive autocorrelation. Consider time-series models like ARIMA."
    } else if (dw_test$statistic > 2.5) {
      "Residuals exhibit negative autocorrelation. Review model assumptions."
    } else {
      "No significant autocorrelation detected (DW ~ 2)."
    }
  )

  # **3️⃣ Breusch-Pagan Test (Heteroscedasticity)**
  bp_test <- bptest(as.formula(paste(column, "~ Year")), data = data)
  results$breusch_pagan <- list(
    test = "Breusch-Pagan Heteroscedasticity Test",
    statistic = bp_test$statistic,
    p_value = bp_test$p.value,
    interpretation = if (bp_test$p.value < 0.05) {
      "Residuals exhibit heteroscedasticity (p < 0.05). Consider using weighted regression."
    } else {
      "No significant heteroscedasticity detected (p >= 0.05)."
    }
  )

  # Perform FFT analysis
  results$fft <- fft_analysis(data, column, "Year", output_path)

  return(results)
}



fft_analysis <- function(data, column, time_column, output_path) {
  
  library(ggplot2)

  # Sort data by time to ensure proper frequency calculations
  data <- data[order(data[[time_column]]), ]
  
  # Extract signal and time values
  signal <- data[[column]]
  time_values <- data[[time_column]]
  
  # Compute time intervals (Years)
  dt <- mean(diff(time_values))  # Average time step in years
  n <- length(signal)  # Number of observations
  
  # Perform FFT
  fft_values <- fft(signal)
  
  # Compute corresponding frequencies in Years⁻¹
  frequencies <- (0:(n / 2 - 1)) / (n * dt)
  
  # Compute power spectrum (energy at each frequency)
  power_spectrum <- Mod(fft_values[1:(n / 2)])^2
  
  # Normalize power to get importance in %
  power_percent <- 100 * power_spectrum / sum(power_spectrum)
  
  # Identify the dominant frequency
  max_index <- which.max(power_spectrum[-1]) + 1  # Ignore DC component (index 1)
  dominant_frequency <- frequencies[max_index]
  dominant_period <- ifelse(dominant_frequency == 0, NA, 1 / dominant_frequency)  # Convert to Years
  
  # Get the top 5 most important frequencies
  top_frequencies <- order(power_spectrum[-1], decreasing = TRUE)[1:5] + 1
  top_freq_values <- frequencies[top_frequencies]
  top_freq_periods <- 1 / top_freq_values  # Convert to Years
  top_freq_importance <- power_percent[top_frequencies]
  
  # Create a dataframe for the top 5 frequencies
  df_top_frequencies <- data.frame(
    Rank = 1:5,
    Frequency_YearsInv = round(top_freq_values, 4),
    Period_Years = round(top_freq_periods, 2),
    Importance_Percent = round(top_freq_importance, 2)
  )

  # Interpretation
  interpretation <- if (is.na(dominant_period)) {
    "No clear periodic component detected."
  } else {
    paste0("A periodic component with a dominant cycle of approximately ", 
           round(dominant_period, 2), " years was detected. ",
           "Top detected cycles range from ", 
           round(min(top_freq_periods), 2), " to ", 
           round(max(top_freq_periods), 2), " years.")
  }
  
  # **🔹 Plot Power Spectrum**
  df_spectrum <- data.frame(Frequency = frequencies, Power = power_spectrum)
  
  p_fft <- ggplot(df_spectrum, aes(x = Frequency, y = Power)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_point(color = "red", size = 2) +
    labs(
      title = "FFT Power Spectrum (Harmonic Analysis)",
      x = "Frequency (Cycles per Year)",
      y = "Power Spectrum"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      axis.ticks = element_line(linewidth = 0.2),
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_line(linewidth = 0.4, color = "#dddddd"),
      panel.grid.minor = element_line(linewidth = 0.2, color = "#f1f1f1"),
      panel.border = element_blank()
    )

  # **🔹 Save FFT Plot**
  output_file_png <- file.path(output_path, "FFT_Harmonic_Analysis.png")
  output_file_svg <- file.path(output_path, "FFT_Harmonic_Analysis.svg")

  ggsave(filename = output_file_png, plot = p_fft, width = 7.16, height = 4.5, dpi = 600)
  ggsave(filename = output_file_svg, plot = p_fft, width = 7.16, height = 4.5, device = "svg")

  message("[INFO] FFT analysis plot saved successfully.")

  return(list(
    test = "FFT Harmonic Analysis",
    dominant_frequency = round(dominant_frequency, 4),
    dominant_period = round(dominant_period, 2),
    interpretation = interpretation,
    top_frequencies = df_top_frequencies,
    plot_saved = list(png = output_file_png, svg = output_file_svg)
  ))
}

