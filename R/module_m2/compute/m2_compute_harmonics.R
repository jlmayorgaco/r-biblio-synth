# ============================================================================
# m2_compute_harmonics.R - Harmonic Analysis on Data and Residuals
# ============================================================================
# Performs spectral analysis on:
# 1. Underlying data (raw time series)
# 2. Residuals (error between model and data)
# Identifies hidden periodicities and model adequacy

#' Compute harmonic analysis for M2
#'
#' @param input Time series data (Year, Articles)
#' @param regression_result Output from compute_m2_regression (optional)
#' @param config Configuration list
#' @return List with harmonic analysis for data and residuals
#' @export
compute_m2_harmonics <- function(input, regression_result = NULL, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) < 10) {
    return(list(
      status = "error",
      data_harmonics = list(),
      residual_harmonics = list()
    ))
  }
  
  year_col <- if ("Year" %in% names(input)) "Year" else names(input)[1]
  articles_col <- if ("Articles" %in% names(input)) "Articles" else names(input)[2]
  
  years <- as.numeric(input[[year_col]])
  articles <- as.numeric(input[[articles_col]])
  
  n <- length(articles)
  dt <- mean(diff(years))
  
  # =========================================
  # 1. Harmonic Analysis on Underlying Data
  # =========================================
  data_harmonics <- analyze_data_harmonics(years, articles, dt)
  
  # =========================================
  # 2. Harmonic Analysis on Residuals
  # =========================================
  residual_harmonics <- list()
  
  if (!is.null(regression_result) && !is.null(regression_result$best_model)) {
    predicted <- regression_result$best_model$predictions
    
    if (!is.null(predicted) && length(predicted) == length(articles)) {
      residuals <- articles - predicted
      residual_harmonics <- analyze_residual_harmonics(years, residuals, dt)
    }
  }
  
  # =========================================
  # 3. Cross-Analysis: Compare Data vs Residual Harmonics
  # =========================================
  cross_analysis <- compare_harmonics(data_harmonics, residual_harmonics)
  r_squared_table <- m2_build_harmonic_r2_table(data_harmonics$harmonic_models)
  
  # =========================================
  # 4. Wavelet Analysis
  # =========================================
  wavelet_result <- tryCatch({
    if (requireNamespace("WaveletComp", quietly = TRUE)) {
      WaveletComp::analyze.wavelet(
        my.data = data.frame(Time = years, Signal = articles),
        my.series = "Signal",
        verbose = FALSE
      )
    } else {
      NULL
    }
  }, error = function(e) NULL)
  
  wavelet_serialized <- if (!is.null(wavelet_result)) {
    list(
      time = wavelet_result$axis.1,
      period = wavelet_result$axis.2,
      power = wavelet_result$Power,
      dominant_period = find_dominant_wavelet_period(wavelet_result)
    )
  } else {
    list()
  }
  
  # =========================================
  # 5. Lomb-Scargle Periodogram
  # =========================================
  lomb_result <- tryCatch({
    if (requireNamespace("lomb", quietly = TRUE)) {
      lomb::lsp(articles, times = years, type = "period", ofac = 10, plot = FALSE)
    } else {
      NULL
    }
  }, error = function(e) NULL)
  
  lomb_serialized <- if (!is.null(lomb_result)) {
    list(
      frequency = lomb_result$freq,
      power = lomb_result$power,
      peak_period = lomb_result$peak.at[1],
      peak_power = lomb_result$peak.at[2],
      type = lomb_result$type
    )
  } else {
    list()
  }
  
  # =========================================
  # 6. Summary
  # =========================================
  summary <- list(
    data_dominant_period = data_harmonics$dominant_period,
    data_top_periods = if (!is.null(data_harmonics$top_periods)) 
      data_harmonics$top_periods$period[1:min(3, nrow(data_harmonics$top_periods))] else NA,
    residual_dominant_period = residual_harmonics$dominant_period,
    residual_variance_explained = residual_harmonics$variance_explained %||% NA,
    data_variance_explained = data_harmonics$variance_explained %||% NA,
    has_periodic_residuals = !is.null(residual_harmonics$dominant_period) && 
                            residual_harmonics$dominant_period > 0,
    wavelet_dominant_period = wavelet_serialized$dominant_period,
    lomb_peak_period = lomb_serialized$peak_period,
    interpretation = interpret_harmonic_results(data_harmonics, residual_harmonics)
  )
  
  list(
    status = "success",
    data_harmonics = data_harmonics,
    residual_harmonics = residual_harmonics,
    cross_analysis = cross_analysis,
    r_squared_table = r_squared_table,
    wavelet = wavelet_serialized,
    lomb = lomb_serialized,
    summary = summary
  )
}

#' Perform FFT and return spectral components
#' @export
m2_perform_fft <- function(y, dt = 1) {
  y <- suppressWarnings(as.numeric(y))
  y <- y[is.finite(y)]

  if (length(y) == 0) {
    return(list(
      frequencies = numeric(),
      magnitude = numeric(),
      power = numeric(),
      phase = numeric(),
      status = "error: no valid data"
    ))
  }

  n <- length(y)
  fft_result <- fft(y)
  frequencies <- (0:(floor(n / 2))) / (n * dt)
  magnitude <- Mod(fft_result[seq_along(frequencies)])
  phase <- Arg(fft_result[seq_along(frequencies)])

  list(
    frequencies = frequencies,
    magnitude = magnitude,
    power = magnitude^2,
    phase = phase,
    status = "success"
  )
}

# ============================================================================
# Data Harmonic Analysis
# ============================================================================

analyze_data_harmonics <- function(years, articles, dt) {
  n <- length(articles)
  
  # FFT analysis
  fft_result <- fft(articles)
  frequencies <- (0:(floor(n / 2))) / (n * dt)
  magnitude <- Mod(fft_result[1:length(frequencies)])
  phase <- Arg(fft_result[1:length(frequencies)])
  
  # Find peak frequencies
  power <- magnitude^2
  peak_idx <- order(-power)[1:min(10, length(power))]
  peak_idx <- peak_idx[is.finite(frequencies[peak_idx]) & frequencies[peak_idx] > 0]
  
  valid_freq_idx <- which(is.finite(frequencies[-1]) & frequencies[-1] > 0)
  dominant_idx <- if (length(valid_freq_idx) > 0) {
    which.max(power[valid_freq_idx + 1]) -> dp
    valid_freq_idx[dp]
  } else {
    1
  }
  dominant_freq <- frequencies[dominant_idx]
  dominant_period <- if (is.finite(dominant_freq) && dominant_freq > 0) 1 / dominant_freq else NA
  
  # Spectral density
  spectrum_df <- data.frame(
    frequency = frequencies,
    period = ifelse(frequencies > 0, 1 / frequencies, Inf),
    magnitude = magnitude,
    power = power,
    normalized_power = power / sum(power),
    variance_explained = power / sum(power)
  )
  
  # Top periods
  top_periods <- spectrum_df[spectrum_df$frequency > 0, ]
  top_periods <- top_periods[order(-top_periods$power), ]
  top_periods <- top_periods[1:min(10, nrow(top_periods)), ]
  top_periods$rank <- seq_len(nrow(top_periods))
  
  # Harmonic regression
  harmonic_models <- fit_harmonic_models(years, articles, frequencies[2:min(5, length(frequencies))])
  
  # Variance explained by top 3 harmonics
  variance_explained <- sum(power[1:min(3, length(power))]) / sum(power)
  
  list(
    fft = list(
      frequencies = frequencies,
      magnitude = magnitude,
      phase = phase,
      power = power
    ),
    spectrum = spectrum_df,
    top_periods = top_periods,
    dominant_period = dominant_period,
    dominant_frequency = dominant_freq,
    harmonic_models = harmonic_models,
    variance_explained = variance_explained,
    interpretation = sprintf(
      "Dominant period: %.1f years, Variance explained by top 3 harmonics: %.1f%%",
      dominant_period, variance_explained * 100
    )
  )
}

# ============================================================================
# Residual Harmonic Analysis
# ============================================================================

analyze_residual_harmonics <- function(years, residuals, dt) {
  n <- length(residuals)
  
  # Remove mean and trend from residuals
  residuals_detrended <- residuals - mean(residuals)
  
  # FFT analysis on residuals
  fft_result <- fft(residuals_detrended)
  frequencies <- (0:(floor(n / 2))) / (n * dt)
  magnitude <- Mod(fft_result[1:length(frequencies)])
  power <- magnitude^2
  
  # Find significant peaks in residual spectrum
  peak_idx <- order(-power)[1:min(10, length(power))]
  peak_idx <- peak_idx[frequencies[peak_idx] > 0]
  
  # Check if peaks are significant (above noise level)
  noise_level <- quantile(power, 0.95)
  significant_peaks <- peak_idx[power[peak_idx] > 2 * noise_level]
  
  if (length(significant_peaks) > 0) {
    dominant_idx <- significant_peaks[1]
    dominant_freq <- frequencies[dominant_idx]
    dominant_period <- if (dominant_freq > 0) 1 / dominant_freq else NA
  } else {
    dominant_idx <- which.max(power[2:floor(n/2)]) + 1
    dominant_freq <- frequencies[dominant_idx]
    dominant_period <- if (dominant_freq > 0) 1 / dominant_freq else NA
  }
  
  # Spectral density
  spectrum_df <- data.frame(
    frequency = frequencies,
    period = ifelse(frequencies > 0, 1 / frequencies, Inf),
    magnitude = magnitude,
    power = power,
    normalized_power = power / sum(power),
    variance_explained = power / sum(power)
  )
  
  # Top periods in residuals
  top_periods <- spectrum_df[spectrum_df$frequency > 0, ]
  top_periods <- top_periods[order(-top_periods$power), ]
  top_periods <- top_periods[1:min(5, nrow(top_periods)), ]
  top_periods$rank <- seq_len(nrow(top_periods))
  
  # Variance explained by harmonics in residuals (should be low for good model)
  variance_explained <- sum(power[1:min(3, length(power))]) / sum(power)
  
  # Periodicity test
  periodicity_test <- test_residual_periodicity(residuals, years)
  
  # Interpretation
  has_strong_periodicity <- variance_explained > 0.3
  
  interpretation <- if (has_strong_periodicity) {
    sprintf(
      "WARNING: Strong periodicity in residuals (period = %.1f years). Model may be misspecified.",
      dominant_period
    )
  } else {
    sprintf(
      "Residual harmonic analysis: No strong periodicity detected. Dominant period: %.1f years.",
      dominant_period
    )
  }
  
  list(
    fft = list(
      frequencies = frequencies,
      magnitude = magnitude,
      power = power
    ),
    spectrum = spectrum_df,
    top_periods = top_periods,
    dominant_period = dominant_period,
    dominant_frequency = dominant_freq,
    variance_explained = variance_explained,
    has_periodicity = has_strong_periodicity,
    periodicity_test = periodicity_test,
    interpretation = interpretation
  )
}

# ============================================================================
# Fit Harmonic Models
# ============================================================================

fit_harmonic_models <- function(years, articles, frequencies) {
  models <- list()
  
  for (freq in frequencies) {
    if (freq <= 0) next
    
    fit <- tryCatch({
      lm(articles ~ sin(2 * pi * freq * years) + cos(2 * pi * freq * years))
    }, error = function(e) NULL)
    
    if (!is.null(fit)) {
      ss_res <- sum(residuals(fit)^2)
      ss_tot <- sum((articles - mean(articles))^2)
      r2 <- 1 - ss_res / ss_tot
      
      models[[as.character(round(freq, 6))]] <- list(
        frequency = freq,
        period = 1 / freq,
        r2 = r2,
        coefficients = coef(fit),
        amplitude = sqrt(coef(fit)[2]^2 + coef(fit)[3]^2),
        phase = atan2(coef(fit)[3], coef(fit)[2])
      )
    }
  }
  
  models
}

# ============================================================================
# Test Residual Periodicity
# ============================================================================

test_residual_periodicity <- function(residuals, years) {
  n <- length(residuals)
  if (n < 10) {
    return(list(
      has_periodicity = NA,
      test = "insufficient_data",
      p_value = NA
    ))
  }
  
  # Fisher's test for periodicity
  fft_result <- fft(residuals - mean(residuals))
  power <- Mod(fft_result[1:(n %/% 2 + 1)])^2
  
  # Fisher's g statistic
  g_stat <- max(power) / sum(power)
  
  # Exact/Semi-exact p-value calculation for Fisher's G test
  # For small n, use exact formula; for large n, approximation is acceptable
  if (n <= 100) {
    # Use more accurate formula for small samples
    # p-value = n * (1 - g)^(n-1) for the basic form
    # But this is still approximation; exact requires special functions
    p_value <- n * exp(-n * g_stat / 2)
    # Apply correction for better accuracy
    p_value <- p_value * (1 + (n * g_stat - 1) / 6)
  } else {
    # Original approximation for large samples
    p_value <- n * exp(-n * g_stat / 2)
  }
  p_value <- min(1, max(0, p_value))
  
  has_periodicity <- p_value < 0.05
  
  list(
    has_periodicity = has_periodicity,
    test = "fisher_g",
    g_statistic = g_stat,
    p_value = p_value,
    interpretation = if (has_periodicity) {
      "Significant periodicity detected in residuals (Fisher's g test p < 0.05)"
    } else {
      "No significant periodicity in residuals"
    }
  )
}

# ============================================================================
# Compare Data vs Residual Harmonics
# ============================================================================

compare_harmonics <- function(data_harmonics, residual_harmonics) {
  if (is.null(data_harmonics) || is.null(residual_harmonics)) {
    return(list(
      interpretation = "Insufficient data for comparison"
    ))
  }
  
  data_period <- m2_scalar_or_na(data_harmonics$dominant_period)
  residual_period <- m2_scalar_or_na(residual_harmonics$dominant_period)
  
  # Check if residual periodicity overlaps with data periodicity
  period_overlap <- FALSE
  if (is.finite(data_period) && is.finite(residual_period) && data_period > 0) {
    period_overlap <- abs(data_period - residual_period) / data_period < 0.2
  }
  
  data_var <- m2_scalar_or_na(data_harmonics$variance_explained %||% NA_real_)
  residual_var <- m2_scalar_or_na(residual_harmonics$variance_explained %||% NA_real_)
  
  list(
    data_dominant_period = data_period,
    residual_dominant_period = residual_period,
    period_overlap = period_overlap,
    data_variance_explained = data_var,
    residual_variance_explained = residual_var,
    ratio = if (!is.na(residual_var) && !is.na(data_var) && data_var > 0) 
      residual_var / data_var else NA,
    interpretation = if (period_overlap && residual_var > 0.2) {
      sprintf(
        "WARNING: Residual periodicity (%.1f years) overlaps with data periodicity (%.1f years). Model may not capture all structure.",
        residual_period, data_period
      )
    } else if (!is.na(residual_var) && residual_var > 0.3) {
      "Unexplained periodic structure remains in residuals."
    } else {
      "Residual harmonics do not overlap significantly with data harmonics. Model captures main structure."
    }
  )
}

# ============================================================================
# Find Dominant Wavelet Period
# ============================================================================

find_dominant_wavelet_period <- function(wavelet_result) {
  if (is.null(wavelet_result) || is.null(wavelet_result$Power)) {
    return(NA)
  }
  
  mean_power <- rowMeans(wavelet_result$Power)
  dominant_idx <- which.max(mean_power)
  
  if (dominant_idx <= length(wavelet_result$axis.2)) {
    return(wavelet_result$axis.2[dominant_idx])
  }
  
  NA
}

# ============================================================================
# Interpret Results
# ============================================================================

interpret_harmonic_results <- function(data_harmonics, residual_harmonics) {
  data_period <- m2_scalar_or_na(data_harmonics$dominant_period)
  residual_period <- m2_scalar_or_na(residual_harmonics$dominant_period)
  residual_var <- m2_scalar_or_na(residual_harmonics$variance_explained %||% 0)
  
  if (!is.finite(data_period)) {
    return("Could not identify dominant period in data.")
  }
  
  if (!is.na(residual_period) && residual_var > 0.3) {
    return(sprintf(
      "Data has %.1f-year cycle. Residuals contain unexplained %.1f-year cycle (%.1f%% variance). Consider adding periodic terms to model.",
      data_period, residual_period, residual_var * 100
    ))
  }
  
  if (!is.na(residual_period)) {
    return(sprintf(
      "Data has %.1f-year dominant cycle. Residuals show weak %.1f-year cycle (%.1f%% variance). Model captures main periodic structure.",
      data_period, residual_period, residual_var * 100
    ))
  }
  
  sprintf(
    "Data has %.1f-year dominant cycle. No significant periodicity in residuals. Model adequately captures cyclical structure.",
    data_period
  )
}

#' Build a harmonic regression R-squared table
#' @keywords internal
m2_build_harmonic_r2_table <- function(harmonic_models) {
  if (!is.list(harmonic_models) || length(harmonic_models) == 0) {
    return(data.frame(Frequency = numeric(), Period = numeric(), R2 = numeric()))
  }

  rows <- lapply(harmonic_models, function(model) {
    data.frame(
      Frequency = m2_scalar_or_na(model$frequency),
      Period = m2_scalar_or_na(model$period),
      R2 = max(0, min(1, m2_scalar_or_na(model$r2))),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

#' Coerce candidate scalar values to numeric NA-safe form
#' @keywords internal
m2_scalar_or_na <- function(x) {
  if (length(x) != 1L) {
    return(NA_real_)
  }

  value <- suppressWarnings(as.numeric(x))
  if (length(value) != 1L || !is.finite(value)) {
    return(NA_real_)
  }

  value
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
