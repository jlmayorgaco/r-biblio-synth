# ---------------------------------------------------------------------------- #
# helpers.R: Shared Helper Functions                                          #
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Calculate Moving Average                                                   #
# ---------------------------------------------------------------------------- #
# Computes a moving average for a given column and window size.
# Returns a data frame with Year and Moving_Avg columns.
calculate_moving_average <- function(data, column, window_size) {
  library(zoo)
  data <- data %>%
    mutate(
      Moving_Avg = zoo::rollmeanr(get(column), k = window_size, fill = NA)
    )
  return(data.frame(Year = data$Year, Articles = data$Moving_Avg))
}

# ---------------------------------------------------------------------------- #
# Detect Anomalies                                                           #
# ---------------------------------------------------------------------------- #
# Identifies anomalies in a column based on a threshold of mean + 2*SD.
calculate_anomalies <- function(data, column) {
  library(dplyr)
  threshold <- mean(data[[column]], na.rm = TRUE) + 2 * sd(data[[column]], na.rm = TRUE)
  anomalies <- data %>% filter(get(column) > threshold)
  return(anomalies)
}

# ---------------------------------------------------------------------------- #
# Detect Outliers Using Z-Score                                               #
# ---------------------------------------------------------------------------- #
# Identifies outliers in a column using z-scores.
calculate_outliers <- function(data, column) {
  z_scores <- scale(data[[column]])
  outliers <- which(abs(z_scores) > 3)
  return(outliers)
}

# ---------------------------------------------------------------------------- #
# Fit Linear Regression Model                                                 #
# ---------------------------------------------------------------------------- #
# Fits a simple linear regression model and returns it.
fit_linear_model <- function(x, y) {
  return(lm(y ~ x))
}

# ---------------------------------------------------------------------------- #
# Predict Using Linear Model                                                  #
# ---------------------------------------------------------------------------- #
# Predicts values using a linear regression model.
predict_linear_model <- function(model, x) {
  return(predict(model, newdata = data.frame(x = x)))
}

# ---------------------------------------------------------------------------- #
# Calculate Autocorrelation                                                   #
# ---------------------------------------------------------------------------- #
# Computes autocorrelation for a given column, excluding lag 0.
calculate_acf <- function(data, column) {
  return(acf(data[[column]], plot = FALSE)$acf[-1])
}

# ---------------------------------------------------------------------------- #
# Perform Fourier Transform                                                   #
# ---------------------------------------------------------------------------- #
# Performs Fourier transform and returns magnitude and phase.
perform_fft <- function(y) {
  fft_result <- fft(y)
  return(list(magnitude = Mod(fft_result), phase = Arg(fft_result)))
}

# ---------------------------------------------------------------------------- #
# Calculate Lomb-Scargle Periodogram                                          #
# ---------------------------------------------------------------------------- #
# Computes the Lomb-Scargle periodogram.
calculate_lomb_scargle <- function(times, values) {
  library(lomb)
  return(lsp(values, times = times, type = "period", ofac = 10, plot = FALSE))
}

# ---------------------------------------------------------------------------- #
# Perform Wavelet Power Spectrum Analysis                                     #
# ---------------------------------------------------------------------------- #
# Computes wavelet power spectrum analysis for a signal.
calculate_wavelet_power <- function(time, signal) {
  library(WaveletComp)
  return(analyze.wavelet(data.frame(Time = time, Signal = signal), "Signal", verbose = FALSE))
}

# ---------------------------------------------------------------------------- #
# Normalize Data                                                              #
# ---------------------------------------------------------------------------- #
# Normalizes data to a 0-1 range.
normalize <- function(data) {
  return((data - min(data, na.rm = TRUE)) / (max(data, na.rm = TRUE) - min(data, na.rm = TRUE)))
}

# ---------------------------------------------------------------------------- #
# Detrend Time Series Data                                                    #
# ---------------------------------------------------------------------------- #
# Removes trend from a time series using linear regression.
detrend_data <- function(x, y) {
  return(y - predict(lm(y ~ x)))
}

# ---------------------------------------------------------------------------- #
# Validate No NA Values                                                       #
# ---------------------------------------------------------------------------- #
# Ensures no NA values are present in specified columns.
validate_no_na <- function(data, columns) {
  for (column in columns) {
    if (any(is.na(data[[column]]))) {
      stop(paste("NA values found in column:", column))
    }
  }
}

# ---------------------------------------------------------------------------- #
# Validate Monotonic Time                                                     #
# ---------------------------------------------------------------------------- #
# Ensures that time values are strictly increasing.
validate_monotonic_time <- function(times) {
  if (any(diff(times) <= 0)) {
    stop("Time values must be strictly increasing.")
  }
}

# ---------------------------------------------------------------------------- #
# Detect Anomalies in a Dataset                                               #
# ---------------------------------------------------------------------------- #
# Identifies anomalies in a dataset based on z-scores.
get_anomalies <- function(df, value_col, time_col, model_name = NULL, model_params = NULL) {
  if (missing(df) || !is.data.frame(df)) {
    stop("Input 'df' must be a data frame.")
  }
  validate_no_na(df, c(value_col, time_col))

  value_mean <- mean(df[[value_col]], na.rm = TRUE)
  value_sd <- sd(df[[value_col]], na.rm = TRUE)

  if (value_sd == 0 || is.na(value_sd)) {
    stop("Standard deviation is zero or NA; anomaly detection cannot be performed.")
  }

  df$z_score <- (df[[value_col]] - value_mean) / value_sd
  df$anomaly <- abs(df$z_score) > 3

  anomalies <- df[df$anomaly, , drop = FALSE]
  if (nrow(anomalies) == 0) {
    message("No anomalies detected.")
    return(data.frame())
  }

  anomalies$model_name <- model_name
  anomalies$model_params <- model_params

  return(anomalies[, c(time_col, value_col, "z_score", "anomaly", "model_name", "model_params"), drop = FALSE])
}

# ---------------------------------------------------------------------------- #
# Detect Outliers Using Z-Score                                               #
# ---------------------------------------------------------------------------- #
detect_outliers_zscore <- function(df, value_col, threshold = 3) {
  validate_no_na(df, value_col)

  value_mean <- mean(df[[value_col]], na.rm = TRUE)
  value_sd <- sd(df[[value_col]], na.rm = TRUE)

  if (value_sd == 0 || is.na(value_sd)) {
    stop("Standard deviation is zero or NA; outlier detection cannot be performed.")
  }

  z_scores <- (df[[value_col]] - value_mean) / value_sd
  outliers <- which(abs(z_scores) > threshold)

  if (length(outliers) == 0) {
    message("No outliers detected.")
    return(data.frame())
  }

  return(data.frame(
    Index = outliers,
    Value = df[[value_col]][outliers],
    Z_Score = z_scores[outliers]
  ))
}

# ---------------------------------------------------------------------------- #
# Identify Outliers Using IQR                                                 #
# ---------------------------------------------------------------------------- #
identify_outliers <- function(df, value_col, time_col) {
  validate_no_na(df, c(value_col, time_col))

  Q1 <- quantile(df[[value_col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[value_col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1

  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR

  outliers <- df[df[[value_col]] < lower_bound | df[[value_col]] > upper_bound, ]

  if (nrow(outliers) == 0) {
    message("No outliers identified using IQR.")
    return(data.frame())
  }

  return(outliers[, c(time_col, value_col)])
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
  exponential_model <- fit_specific_model(exponential_growth, list(r = 0.0651, N0 = 2.87, t0 = 0.01), annual_production)
  logarithmic_model <- fit_specific_model(logarithmic_growth, list(a = 1, b = 1), annual_production)
  logistic_model <- fit_specific_model(logistic_growth, list(K = 150, r = 0.1, t0 = 2000), annual_production)
  gompertz_model <- fit_specific_model(gompertz_growth, list(N0 = 115, Nmax = 15.5, k = 0.1, t0 = 2000, y0 = 0.001), annual_production)
  gompertz_derivate_model <- fit_specific_model(gompertz_growth_derivative, list(N0 = 115, Nmax = 15.5, k = 0.1, t0 = 2000, y0 = 0.001), annual_production)
  weibull_model <- fit_specific_model(weibull_growth, list(K = 150, r = 0.1, t0 = 2000), annual_production)
  vonbertalanffy_model <- fit_specific_model(vonbertalanffy_growth, list(Linf = 150, k = 0.1, t0 = 2000), annual_production)
  normal_model <- fit_specific_model(normal_growth, list(mu = mean(annual_production$Year), sigma = sd(annual_production$Year), A = max(annual_production$Articles)), annual_production)

  # Collect models
  models <- list(
    Linear = linear_model,
    Exponential = exponential_model,
    Logarithmic = logarithmic_model,
    Logistic = logistic_model,
    Gompertz = gompertz_model,
    GompertzDerivate = gompertz_derivate_model,
    Weibull = weibull_model,
    VonBertalanffy = vonbertalanffy_model,
    Normal = normal_model
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
  # Explicit formula for the Gompertz growth model
  A <- log(Nmax / N0)
  B <- exp(-k * (t - t0))
  N <- y0 + N0 * exp(A * B)
  # Derivative of the Gompertz growth model
  dNdt <- -k * A * B * (N - y0)
  return(dNdt)
}
weibull_growth <- function(t, K, r, t0) { K * (1 - exp(-(t/t0)^r)) }
vonbertalanffy_growth <- function(t, Linf, k, t0) { Linf * (1 - exp(-k * (t - t0))) }
normal_growth <- function(t, mu, sigma, A) { A * exp(-0.5 * ((t - mu) / sigma)^2) }
logistic_growth <- function(t, K, r, t0) { K / (1 + exp(-r * (t - t0))) }

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
  Normal = normal_growth
)
  
serialize_model <- function(model) {
  if (inherits(model, "lm")) {
    return(list(
      coefficients = as.list(coef(model)),
      r_squared = summary(model)$r.squared,
      adj_r_squared = summary(model)$adj.r.squared,
      residuals = head(residuals(model), 10)  # Only save the first 10 residuals to limit size
    ))
  } else if (inherits(model, "nls")) {
    return(list(
      coefficients = as.list(coef(model)),
      residuals = head(residuals(model), 10),  # Limit residuals to the first 10 values
      fitted_values = head(fitted(model), 10) # Limit fitted values
    ))
  } else {
    return(NULL)  # Unsupported model type
  }
}



# helpers.R
# ---------------------------------------------------------------------------- #
# -- _helpers.r: Shared Helper Functions ------------------------------------- #
# ---------------------------------------------------------------------------- #


# Calculate moving average
calculate_moving_average <- function(data, column, window_size) {
  library(zoo)
  data <- data %>%
    mutate(
      Moving_Avg = zoo::rollmeanr(get(column), k = window_size, fill = NA)
    )
  
  # Return a new data frame containing Year and Moving_Avg columns
  result <- data.frame(
    Year = data$Year,
    Articles = data$Moving_Avg
  )
  
  return(result)
}

# Detect anomalies in a column
calculate_anomalies <- function(data, column) {
  library(dplyr)
  threshold <- mean(data[[column]], na.rm = TRUE) + 2 * sd(data[[column]], na.rm = TRUE)
  anomalies <- data %>% filter(get(column) > threshold)
  return(anomalies)
}

# Detect outliers using z-score
calculate_outliers <- function(data, column) {
  z_scores <- scale(data[[column]])
  outliers <- which(abs(z_scores) > 3)
  return(outliers)
}

# Fit a linear regression model
fit_linear_model <- function(x, y) {
  lm(y ~ x)
}

# Predict using a linear model
predict_linear_model <- function(model, x) {
  predict(model, newdata = data.frame(x = x))
}

# Calculate autocorrelation
calculate_acf <- function(data, column) {
  acf(data[[column]], plot = FALSE)$acf[-1]  # Skip lag 0
}

# Perform Fourier Transform
perform_fft <- function(y) {
  fft_result <- fft(y)
  list(magnitude = Mod(fft_result), phase = Arg(fft_result))
}

# Calculate Lomb-Scargle Periodogram
calculate_lomb_scargle <- function(times, values) {
  library(lomb)
  lsp(values, times = times, type = "period", ofac = 10, plot = FALSE)
}

# Perform Wavelet Power Spectrum Analysis
calculate_wavelet_power <- function(time, signal) {
  library(WaveletComp)
  analyze.wavelet(data.frame(Time = time, Signal = signal), "Signal", verbose = FALSE)
}

# Normalize data
normalize <- function(data) {
  (data - min(data, na.rm = TRUE)) / (max(data, na.rm = TRUE) - min(data, na.rm = TRUE))
}

# Detrend time series data
detrend_data <- function(x, y) {
  y - predict(lm(y ~ x))
}

# Validate no NA values in specified columns
validate_no_na <- function(data, columns) {
  for (column in columns) {
    if (any(is.na(data[[column]]))) {
      stop(paste("NA values found in column:", column))
    }
  }
}

# Validate that time values are strictly increasing
validate_monotonic_time <- function(times) {
  if (any(diff(times) <= 0)) {
    stop("Time values must be strictly increasing.")
  }
}


# ---------------------------------------------------------------------------- #
# Detect anomalies in a dataset
# ---------------------------------------------------------------------------- #
get_anomalies <- function(df, value_col, time_col, model_name = NULL, model_params = NULL) {
  if (missing(df) || !is.data.frame(df)) {
    stop("Input 'df' must be a data frame.")
  }
  if (missing(value_col) || !(value_col %in% colnames(df))) {
    stop("Column '", value_col, "' not found in the data frame.")
  }
  if (missing(time_col) || !(time_col %in% colnames(df))) {
    stop("Column '", time_col, "' not found in the data frame.")
  }

  # Calculate mean and standard deviation, handling NA values
  value_mean <- mean(df[[value_col]], na.rm = TRUE)
  value_sd <- sd(df[[value_col]], na.rm = TRUE)

  if (value_sd == 0 || is.na(value_sd)) {
    stop("Standard deviation is zero or NA; anomaly detection cannot be performed.")
  }

  # Calculate z-scores for the value column
  df$z_score <- (df[[value_col]] - value_mean) / value_sd

  # Identify anomalies where z-score exceeds the threshold (default: ±3)
  threshold <- 3
  df$anomaly <- abs(df$z_score) > threshold

  # Subset anomalies with required columns
  if (!"anomaly" %in% colnames(df)) {
    stop("The 'anomaly' column was not created.")
  }
  anomalies <- df[df$anomaly, , drop = FALSE]

  if (nrow(anomalies) == 0) {
    message("No anomalies detected.")
    return(data.frame())
  }

  # Include metadata
  anomalies$model_name <- model_name
  anomalies$model_params <- model_params

  # Return anomalies with specific columns
  return(anomalies[, c(time_col, value_col, "z_score", "anomaly", "model_name", "model_params"), drop = FALSE])
}
detect_outliers_zscore <- function(df, value_col, threshold = 3) {
  if (missing(df) || !is.data.frame(df)) {
    stop("Input 'df' must be a data frame.")
  }
  if (missing(value_col) || !(value_col %in% colnames(df))) {
    stop("Column '", value_col, "' not found in the data frame.")
  }
  if (!is.numeric(df[[value_col]])) {
    stop("Column '", value_col, "' must be numeric for outlier detection.")
  }

  # Calculate mean and standard deviation
  value_mean <- mean(df[[value_col]], na.rm = TRUE)
  value_sd <- sd(df[[value_col]], na.rm = TRUE)

  if (value_sd == 0 || is.na(value_sd)) {
    stop("Standard deviation is zero or NA; outlier detection cannot be performed.")
  }

  # Calculate z-scores
  z_scores <- (df[[value_col]] - value_mean) / value_sd

  # Identify outliers based on z-score threshold
  outliers <- which(abs(z_scores) > threshold)

  if (length(outliers) == 0) {
    message("No outliers detected.")
    return(data.frame())
  }

  # Return a data frame with outlier information
  return(data.frame(
    Index = outliers,
    Value = df[[value_col]][outliers],
    Z_Score = z_scores[outliers]
  ))
}
# Function: Identify outliers using interquartile range (IQR)
identify_outliers <- function(df, value_col, time_col) {
  if (missing(df) || !is.data.frame(df)) {
    stop("Input 'df' must be a data frame.")
  }
  if (missing(value_col) || !(value_col %in% colnames(df))) {
    stop("Column '", value_col, "' not found in the data frame.")
  }
  if (!is.numeric(df[[value_col]])) {
    stop("Column '", value_col, "' must be numeric for outlier detection.")
  }
  if (missing(time_col) || !(time_col %in% colnames(df))) {
    stop("Column '", time_col, "' not found in the data frame.")
  }

  # Calculate the interquartile range (IQR)
  Q1 <- quantile(df[[value_col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[value_col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1

  # Define outlier thresholds
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR

  # Identify outliers
  outliers <- df[df[[value_col]] < lower_bound | df[[value_col]] > upper_bound, ]

  if (nrow(outliers) == 0) {
    message("No outliers identified using IQR.")
    return(data.frame())
  }

  # Return a data frame with outlier information
  return(outliers[, c(time_col, value_col)])
}



# ---------------------------------------------------------------------------- #
# Helper Function: Create Comparison Table for Regression Models
# ---------------------------------------------------------------------------- #
get_metrics_comparison_table <- function(models, data) {
  library(broom)  # For tidying model outputs
  library(Metrics)  # For RMSE calculation
  
  # Extract x and y columns
  x <- as.numeric(data[[1]])
  y <- as.numeric(data[[2]])
  
  # Initialize table to store results
  results <- data.frame(
    Model = character(),
    R2 = numeric(),
    RMSE = numeric(),
    AIC = numeric(),
    Parameters = character(),
    stringsAsFactors = FALSE
  )

  # Evaluate each model
  for (name in names(models)) {
    model <- models[[name]]    
    # Handle linear and non-linear models separately
    if (inherits(model, "lm")) {
      preds <- predict(model)
      r_squared <- summary(model)$r.squared
      aic <- AIC(model)
      bic <- BIC(model)
      params <- coef(model)  # Get model parameters
    } else if (inherits(model, "nls")) {
      preds <- predict(model)
      r_squared <- cor(y, preds)^2  # R² for nls
      aic <- AIC(model)
      bic <- BIC(model)
      params <- coef(model)  # Get model parameters
    } else {
      next  # Skip invalid models
    }

    message(' ----- > model ')
    print(model)
    message(' ----- > r_squared ')
    print(r_squared)

    # Convert parameters to a readable string
    param_str <- paste(names(params), "=", round(params, 4), collapse = ", ")

    # Calculate RMSE
    rmse <- rmse(y, preds)
    
    # Append to results table
    results <- rbind(results, data.frame(
      Model = name,
      R2 = r_squared,
      RMSE = rmse,
      AIC = aic,
      BIC = bic,
      Parameters = param_str
    ))
  }
 
  # Sort results by R² in descending order
  results <- results[order(-results$R2), ]
  
  return(results)
}

# ---------------------------------------------------------------------------- #
# Helper Function: Get Best Regression Model Based on R²
# ---------------------------------------------------------------------------- #
get_best_model <- function(comparison_table) {

  # Ensure the table has the necessary columns
  if (!all(c("Model", "R2") %in% colnames(comparison_table))) {
    stop("The comparison table must include 'Model' and 'R2' columns.")
  }

  # Find the row with the maximum R²
  best_model_row <- comparison_table[which.max(comparison_table$R2), ]

  # Return as a list
  best_model <- list(
    name = best_model_row$Model,
    R2 = best_model_row$R2,
    params = best_model_row$Parameters
  )

  return(best_model)
}



# ---------------------------------------------------------------------------- #
# Model Function Map: Regression Models
# ---------------------------------------------------------------------------- #

model_function_map <- list(
  Linear = function(t, a, b) { a + b * t },
  Quadratic = function(t, a, b, c) { a + b * t + c * t^2 },
  Exponential = function(t, a, b) { a * exp(b * t) },
  Logistic = function(t, a, b, c) { a / (1 + exp(-b * (t - c))) }
)



parse_params <- function(params_str) {
  if (is.null(params_str) || params_str == "") {
    stop("[ERROR] Parameter string is NULL or empty.")
  }
  message("[DEBUG] Parsing parameter string: ", params_str)

  params_list <- strsplit(params_str, ",")[[1]]
  message("[DEBUG] Split params list: ", paste(params_list, collapse = ", "))

  params <- setNames(
    as.numeric(sapply(params_list, function(x) strsplit(trimws(x), " = ")[[1]][2])),
    sapply(params_list, function(x) strsplit(trimws(x), " = ")[[1]][1])
  )
  
  return(params)
}







serialize_model <- function(model) {
  if (inherits(model, "lm")) {
    return(list(
      type = "lm",
      coefficients = as.list(coef(model)),
      r_squared = summary(model)$r.squared,
      adj_r_squared = summary(model)$adj.r.squared,
      residuals = head(residuals(model), 10),  # Limit to first 10 values
      fitted_values = head(fitted(model), 10) # Limit to first 10 values
    ))
  } else if (inherits(model, "nls")) {
    return(list(
      type = "nls",
      coefficients = as.list(coef(model)),
      residuals = head(residuals(model), 10),
      fitted_values = head(fitted(model), 10)
    ))
  } else {
    return(NULL)  # Skip unsupported model types
  }
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
  exponential_model <- fit_specific_model(exponential_growth, list(r = 0.0651, N0 = 2.87, t0 = 0.01), annual_production)
  logarithmic_model <- fit_specific_model(logarithmic_growth, list(a = 1, b = 1), annual_production)
  logistic_model <- fit_specific_model(logistic_growth, list(K = 150, r = 0.1, t0 = 2000), annual_production)
  gompertz_model <- fit_specific_model(gompertz_growth, list(N0 = 115, Nmax = 15.5, k = 0.1, t0 = 2000, y0 = 0.001), annual_production)
  gompertz_derivate_model <- fit_specific_model(gompertz_growth_derivative, list(N0 = 115, Nmax = 15.5, k = 0.1, t0 = 2000, y0 = 0.001), annual_production)
  weibull_model <- fit_specific_model(weibull_growth, list(K = 150, r = 0.1, t0 = 2000), annual_production)
  vonbertalanffy_model <- fit_specific_model(vonbertalanffy_growth, list(Linf = 150, k = 0.1, t0 = 2000), annual_production)
  normal_model <- fit_specific_model(normal_growth, list(mu = mean(annual_production$Year), sigma = sd(annual_production$Year), A = max(annual_production$Articles)), annual_production)

  # Collect models
  models <- list(
    Linear = linear_model,
    Exponential = exponential_model,
    Logarithmic = logarithmic_model,
    Logistic = logistic_model,
    Gompertz = gompertz_model,
    GompertzDerivate = gompertz_derivate_model,
    Weibull = weibull_model,
    VonBertalanffy = vonbertalanffy_model,
    Normal = normal_model
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
  # Explicit formula for the Gompertz growth model
  A <- log(Nmax / N0)
  B <- exp(-k * (t - t0))
  N <- y0 + N0 * exp(A * B)
  # Derivative of the Gompertz growth model
  dNdt <- -k * A * B * (N - y0)
  return(dNdt)
}
weibull_growth <- function(t, K, r, t0) { K * (1 - exp(-(t/t0)^r)) }
vonbertalanffy_growth <- function(t, Linf, k, t0) { Linf * (1 - exp(-k * (t - t0))) }
normal_growth <- function(t, mu, sigma, A) { A * exp(-0.5 * ((t - mu) / sigma)^2) }
logistic_growth <- function(t, K, r, t0) { K / (1 + exp(-r * (t - t0))) }

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
  Normal = normal_growth
)
