# ============================================================================
# m2_compute_residual_analysis.R - Residual Diagnostics
# ============================================================================
# Analyzes residuals from regression models:
# - Normality tests (Shapiro-Wilk, Anderson-Darling)
# - Breakpoint detection (CUSUM, optimal partitioning)
# - Autocorrelation tests
# - Harmonic analysis on residuals

#' Compute residual diagnostics for M2
#'
#' @param input Time series data (Year, Articles)
#' @param regression_result Output from compute_m2_regression
#' @param config Configuration list
#' @return List with residual diagnostics
#' @export
compute_m2_residual_analysis <- function(input, regression_result, config = biblio_config()) {
  if (!is.data.frame(input) || is.null(regression_result)) {
    return(list(status = "error: invalid inputs"))
  }
  
  year_col <- if ("Year" %in% names(input)) "Year" else names(input)[1]
  articles_col <- if ("Articles" %in% names(input)) "Articles" else names(input)[2]
  
  years <- input[[year_col]]
  observed <- input[[articles_col]]
  
  best_model <- regression_result$best_model
  
  if (is.null(best_model) || is.null(best_model$predictions)) {
    return(list(status = "error: no model predictions"))
  }
  
  predicted <- best_model$predictions
  
  if (length(predicted) != length(observed)) {
    predicted <- predicted[1:length(observed)]
    observed <- observed[1:length(predicted)]
  }
  
  residuals <- observed - predicted
  
  n <- length(residuals)
  
  normality_tests <- test_residual_normality(residuals)
  
  breakpoint_tests <- detect_residual_breakpoints(years, residuals)
  
  acf_test <- test_residual_autocorrelation(residuals)
  
  harmonic_analysis <- analyze_residual_harmonics(years, residuals)
  
  model_adequacy <- assess_model_adequacy(residuals, predicted, observed)
  
  list(
    residuals = residuals,
    years = years,
    predicted = predicted,
    observed = observed,
    normality = normality_tests,
    breakpoints = breakpoint_tests,
    autocorrelation = acf_test,
    harmonics = harmonic_analysis,
    model_adequacy = model_adequacy,
    summary = list(
      mean_residual = mean(residuals, na.rm = TRUE),
      sd_residual = sd(residuals, na.rm = TRUE),
      is_normal = normality_tests$overall_normal,
      has_breakpoints = breakpoint_tests$has_breakpoints,
      breakpoint_years = breakpoint_tests$breakpoint_years,
      has_autocorrelation = acf_test$has_autocorrelation,
      dominant_period = harmonic_analysis$dominant_period
    ),
    status = "success"
  )
}

# ============================================================================
# Normality Tests
# ============================================================================

test_residual_normality <- function(residuals) {
  n <- length(residuals)
  if (n < 8) {
    return(list(
      shapiro_wilk = list(statistic = NA, p_value = NA, normal = NA),
      anderson_darling = list(statistic = NA, p_value = NA, normal = NA),
      jarque_bera = list(statistic = NA, p_value = NA, normal = NA),
      overall_normal = NA,
      interpretation = "Insufficient data for normality tests"
    ))
  }
  
  # Shapiro-Wilk test
  sw_test <- tryCatch({
    test <- shapiro.test(residuals)
    list(
      statistic = test$statistic,
      p_value = test$p.value,
      normal = test$p.value > 0.05
    )
  }, error = function(e) {
    list(statistic = NA, p_value = NA, normal = NA)
  })
  
  # Anderson-Darling test (using nortest if available)
  ad_test <- tryCatch({
    if (requireNamespace("nortest", quietly = TRUE)) {
      test <- nortest::ad.test(residuals)
      list(
        statistic = test$statistic,
        p_value = test$p.value,
        normal = test$p.value > 0.05
      )
    } else {
      list(statistic = NA, p_value = NA, normal = NA)
    }
  }, error = function(e) {
    list(statistic = NA, p_value = NA, normal = NA)
  })
  
  # Jarque-Bera test (using population moments for correct formula)
  jb_test <- tryCatch({
    n <- length(residuals)
    m <- mean(residuals)
    # Population variance (n denominator, not n-1)
    s_pop <- sqrt(mean((residuals - m)^2))
    if (s_pop == 0 || !is.finite(s_pop)) {
      return(list(statistic = NA, p_value = NA, normal = NA))
    }
    # Population skewness and excess kurtosis
    skewness <- mean((residuals - m)^3) / (s_pop^3)
    kurtosis <- mean((residuals - m)^4) / (s_pop^4) - 3
    jb_stat <- n * (skewness^2 / 6 + kurtosis^2 / 24)
    p_value <- 1 - pchisq(jb_stat, 2)
    list(
      statistic = jb_stat,
      p_value = p_value,
      normal = p_value > 0.05
    )
  }, error = function(e) {
    list(statistic = NA, p_value = NA, normal = NA)
  })
  
  overall_normal <- (!is.na(sw_test$normal) && sw_test$normal) ||
                    (!is.na(ad_test$normal) && ad_test$normal)
  
  interpretation <- if (overall_normal) {
    "Residuals appear normally distributed"
  } else {
    "Residuals deviate from normality - model may be misspecified"
  }
  
  list(
    shapiro_wilk = sw_test,
    anderson_darling = ad_test,
    jarque_bera = jb_test,
    overall_normal = overall_normal,
    skewness = if (n > 2) sum((residuals - mean(residuals))^3) / (n * sd(residuals)^3) else NA,
    kurtosis = if (n > 3) sum((residuals - mean(residuals))^4) / (n * sd(residuals)^4) - 3 else NA,
    interpretation = interpretation
  )
}

# ============================================================================
# Breakpoint Detection
# ============================================================================

detect_residual_breakpoints <- function(years, residuals) {
  n <- length(residuals)
  if (n < 10) {
    return(list(
      has_breakpoints = FALSE,
      n_breakpoints = 0,
      breakpoint_years = integer(0),
      method = "insufficient_data",
      interpretation = "Insufficient data for breakpoint detection"
    ))
  }
  
  # Method 1: CUSUM test
  cusum_results <- cusum_breakpoint_test(years, residuals)
  
  # Method 2: Optimal partitioning (simplified BIC-based)
  partition_results <- optimal_partition_breakpoints(years, residuals)
  
  # Method 3: Rolling variance
  variance_breaks <- rolling_variance_breaks(years, residuals)
  
  # Combine results
  all_breakpoints <- c(cusum_results$breakpoint_years,
                       partition_results$breakpoint_years,
                       variance_breaks$breakpoint_years)
  
  all_breakpoints <- sort(unique(all_breakpoints))
  
  # Score breakpoints by how many methods detected them
  breakpoint_scores <- table(all_breakpoints)
  high_confidence_breaks <- as.numeric(names(breakpoint_scores[breakpoint_scores >= 2]))
  
  has_breaks <- length(high_confidence_breaks) > 0
  
  list(
    has_breakpoints = has_breaks,
    n_breakpoints = length(high_confidence_breaks),
    breakpoint_years = high_confidence_breaks,
    cusum = cusum_results,
    partition = partition_results,
    variance = variance_breaks,
    interpretation = if (has_breaks) {
      sprintf("Breakpoints detected at years: %s", paste(high_confidence_breaks, collapse = ", "))
    } else {
      "No significant breakpoints detected in residuals"
    }
  )
}

#' CUSUM breakpoint test
cusum_breakpoint_test <- function(years, residuals) {
  n <- length(residuals)
  
  cumulative <- cumsum(residuals - mean(residuals))
  
  cusum_max <- max(abs(cumulative))
  cusum_threshold <- 1.143 * sqrt(n) * sd(residuals)
  
  has_break <- cusum_max > cusum_threshold
  
  if (has_break) {
    break_idx <- which.max(abs(cumulative))
    break_year <- years[break_idx]
  } else {
    break_year <- NA
  }
  
  list(
    cusum_max = cusum_max,
    threshold = cusum_threshold,
    has_break = has_break,
    breakpoint_years = if (!is.na(break_year)) break_year else integer(0),
    statistic = cusum_max / cusum_threshold
  )
}

#' Optimal partition breakpoints
optimal_partition_breakpoints <- function(years, residuals) {
  n <- length(residuals)
  
  if (n < 10) {
    return(list(breakpoint_years = integer(0), bic_values = NA))
  }
  
  # Test models with 0, 1, 2 breaks
  bic_values <- numeric(3)
  break_points <- vector("list", 3)
  
  # No breaks
  bic_values[1] <- n * log(var(residuals)) + 2 * log(n)
  break_points[[1]] <- integer(0)
  
  # One break
  best_one_break <- Inf
  best_one_idx <- NA
  for (i in 2:(n-2)) {
    seg1 <- residuals[1:i]
    seg2 <- residuals[(i+1):n]
    bic <- i * log(var(seg1)) + (n-i) * log(var(seg2)) + 3 * log(n)
    if (bic < best_one_break) {
      best_one_break <- bic
      best_one_idx <- i
    }
  }
  bic_values[2] <- best_one_break
  break_points[[2]] <- years[best_one_idx]
  
  # Two breaks
  best_two_break <- Inf
  best_two_idx <- c(NA, NA)
  for (i in 2:(n-4)) {
    for (j in (i+2):(n-2)) {
      seg1 <- residuals[1:i]
      seg2 <- residuals[(i+1):j]
      seg3 <- residuals[(j+1):n]
      bic <- i * log(var(seg1)) + (j-i) * log(var(seg2)) + (n-j) * log(var(seg3)) + 4 * log(n)
      if (bic < best_two_break) {
        best_two_break <- bic
        best_two_idx <- c(i, j)
      }
    }
  }
  bic_values[3] <- best_two_break
  break_points[[3]] <- years[best_two_idx]
  
  # Select best model
  best_model <- which.min(bic_values)
  
  list(
    bic_values = bic_values,
    best_model = best_model - 1, # 0, 1, or 2 breaks
    breakpoint_years = break_points[[best_model]]
  )
}

#' Rolling variance breakpoints
rolling_variance_breaks <- function(years, residuals, window = 5) {
  n <- length(residuals)
  if (n < window * 2) return(list(breakpoint_years = integer(0)))
  
  rolling_vars <- numeric(n - window + 1)
  for (i in 1:(n - window + 1)) {
    rolling_vars[i] <- var(residuals[i:(i + window - 1)])
  }
  
  var_changes <- abs(diff(rolling_vars))
  threshold <- quantile(var_changes, 0.9)
  
  break_indices <- which(var_changes > threshold)
  if (length(break_indices) == 0) {
    return(list(breakpoint_years = integer(0)))
  }
  
  break_years <- years[break_indices + window %/% 2]
  
  list(
    rolling_vars = rolling_vars,
    var_changes = var_changes,
    threshold = threshold,
    breakpoint_years = unique(break_years)
  )
}

# ============================================================================
# Autocorrelation Test
# ============================================================================

test_residual_autocorrelation <- function(residuals) {
  n <- length(residuals)
  if (n < 10) {
    return(list(
      has_autocorrelation = NA,
      dw_statistic = NA,
      p_value = NA,
      interpretation = "Insufficient data"
    ))
  }
  
  # Durbin-Watson statistic
  dw_stat <- sum(diff(residuals)^2) / sum(residuals^2)
  
  # Approximate test (exact p-values require special tables)
  dw_lower <- 1.5
  dw_upper <- 2.5
  
  has_autocorr <- dw_stat < dw_lower || dw_stat > dw_upper
  
  # Ljung-Box test
  lb_test <- tryCatch({
    if (requireNamespace("stats", quietly = TRUE)) {
      acf_vals <- acf(residuals, lag.max = min(10, n-1), plot = FALSE)$acf[-1]
      n_lags <- length(acf_vals)
      lb_stat <- n * (n + 2) * sum(acf_vals^2 / (n - (1:n_lags)))
      p_value <- 1 - pchisq(lb_stat, n_lags)
      list(statistic = lb_stat, p_value = p_value)
    } else {
      list(statistic = NA, p_value = NA)
    }
  }, error = function(e) {
    list(statistic = NA, p_value = NA)
  })
  
  list(
    has_autocorrelation = has_autocorr,
    dw_statistic = dw_stat,
    lb_statistic = lb_test$statistic,
    lb_p_value = lb_test$p_value,
    interpretation = if (has_autocorr) {
      "Residuals show autocorrelation - consider time series model"
    } else {
      "No significant autocorrelation in residuals"
    }
  )
}

# ============================================================================
# Harmonic Analysis on Residuals
# ============================================================================

analyze_residual_harmonics <- function(years, residuals) {
  n <- length(residuals)
  if (n < 8) {
    return(list(
      dominant_period = NA,
      harmonics = data.frame(),
      interpretation = "Insufficient data for harmonic analysis"
    ))
  }
  
  # FFT analysis
  fft_result <- fft(residuals)
  fft_mod <- Mod(fft_result)[1:(n %/% 2 + 1)]
  freqs <- (0:(n %/% 2)) / n
  
  # Find dominant frequencies
  peak_idx <- which.max(fft_mod[2:((n %/% 2))]) + 1
  
  if (freqs[peak_idx] > 0) {
    dominant_period <- 1 / freqs[peak_idx]
  } else {
    dominant_period <- NA
  }
  
  # Spectral density
  spectrum_df <- data.frame(
    frequency = freqs,
    period = ifelse(freqs > 0, 1/freqs, Inf),
    power = fft_mod^2,
    normalized_power = fft_mod^2 / sum(fft_mod^2)
  )
  
  # Top harmonics
  top_harmonics <- spectrum_df[order(-spectrum_df$power), ]
  top_harmonics <- top_harmonics[1:min(5, nrow(top_harmonics)), ]
  
  # Lomb-Scargle periodogram (if available)
  lomb_results <- tryCatch({
    if (requireNamespace("lomb", quietly = TRUE)) {
      ls_result <- lomb::lsp(residuals, times = years, from = 2, to = n/2, plot = FALSE)
      list(
        periods = ls_result$scanned.periods,
        power = ls_result$power,
        peak_period = ls_result$peak.at[1]
      )
    } else {
      list(periods = NA, power = NA, peak_period = NA)
    }
  }, error = function(e) {
    list(periods = NA, power = NA, peak_period = NA)
  })
  
  interpretation <- if (!is.na(dominant_period)) {
    sprintf("Dominant period: %.1f years in residuals", dominant_period)
  } else {
    "No clear periodic structure in residuals"
  }
  
  list(
    dominant_period = dominant_period,
    dominant_frequency = freqs[peak_idx],
    spectrum = spectrum_df,
    top_harmonics = top_harmonics,
    lomb_scargle = lomb_results,
    variance_explained = sum(fft_mod[1:3]^2) / sum(fft_mod^2),
    interpretation = interpretation
  )
}

# ============================================================================
# Model Adequacy Assessment
# ============================================================================

assess_model_adequacy <- function(residuals, predicted, observed) {
  n <- length(residuals)
  
  # R-squared
  ss_res <- sum(residuals^2)
  ss_tot <- sum((observed - mean(observed))^2)
  r_squared <- 1 - ss_res / ss_tot
  
  # Adjusted R-squared
  k <- 2  # linear model has 2 parameters
  adj_r_squared <- 1 - (1 - r_squared) * (n - 1) / (n - k - 1)
  
  # RMSE
  rmse <- sqrt(mean(residuals^2))
  
  # MAE
  mae <- mean(abs(residuals))
  
  # MAPE
  mape <- mean(abs(residuals / observed)) * 100
  
  # AIC/BIC (using normal likelihood)
  aic <- n * log(ss_res / n) + 2 * k
  bic <- n * log(ss_res / n) + log(n) * k
  
  # Residual standard error
  rse <- sqrt(ss_res / (n - k))
  
  # Coefficient of variation
  cv <- rse / mean(observed)
  
  model_adequate <- r_squared > 0.7 && 
                   abs(mean(residuals)) < 0.1 * sd(residuals) &&
                   cv < 0.5
  
  list(
    r_squared = r_squared,
    adj_r_squared = adj_r_squared,
    rmse = rmse,
    mae = mae,
    mape = mape,
    aic = aic,
    bic = bic,
    rse = rse,
    cv = cv,
    model_adequate = model_adequate,
    interpretation = if (model_adequate) {
      sprintf("Model adequacy: R²=%.3f, RMSE=%.2f, CV=%.2f", r_squared, rmse, cv)
    } else {
      sprintf("Model may be inadequate: R²=%.3f, CV=%.2f", r_squared, cv)
    }
  )
}