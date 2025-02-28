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
  polynomial_model <- lm(Articles ~ poly(Year, 3, raw = TRUE), data = annual_production)
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
  arima_model <- auto.arima(annual_production$Articles)

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
    Fourier = fourier_model,
    ARIMA = arima_model
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

# Function to perform and store statistical test results
perform_statistical_tests <- function(data, column) {
  
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

  return(results)
}