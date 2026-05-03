# ============================================================================
# m2_compute_residual_analysis.R - Residual Diagnostics
# ============================================================================
# Analyzes residuals from regression models:
# - Normality tests (Shapiro-Wilk, Anderson-Darling, Jarque-Bera)
# - Breakpoint detection (CUSUM, partitioning, rolling variance)
# - Autocorrelation diagnostics (ACF, PACF, Durbin-Watson, Ljung-Box)
# - Heteroscedasticity diagnostics
# - Harmonic analysis on residuals
# - Variance trend modeling for squared residuals

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

  years <- suppressWarnings(as.numeric(input[[year_col]]))
  observed <- suppressWarnings(as.numeric(input[[articles_col]]))

  best_model <- regression_result$best_model
  if (is.null(best_model) || is.null(best_model$predictions)) {
    return(list(status = "error: no model predictions"))
  }

  predicted <- suppressWarnings(as.numeric(best_model$predictions))
  if (length(predicted) != length(observed)) {
    limit <- min(length(predicted), length(observed))
    predicted <- predicted[seq_len(limit)]
    observed <- observed[seq_len(limit)]
    years <- years[seq_len(limit)]
  }

  residuals <- observed - predicted
  standardized_residuals <- m2_standardize_vector(residuals)
  squared_residuals <- residuals^2

  normality_tests <- test_residual_normality(residuals)
  breakpoint_tests <- detect_residual_breakpoints(years, residuals)
  autocorrelation_tests <- test_residual_autocorrelation(residuals)
  heteroscedasticity_tests <- test_residual_heteroscedasticity(years, residuals)
  harmonic_analysis <- analyze_residual_harmonics_residuals(years, residuals)
  variance_model <- fit_residual_variance_models(years, squared_residuals)
  model_adequacy <- assess_model_adequacy(residuals, predicted, observed)

  list(
    residuals = residuals,
    standardized_residuals = standardized_residuals,
    squared_residuals = squared_residuals,
    years = years,
    predicted = predicted,
    observed = observed,
    normality = normality_tests,
    breakpoints = breakpoint_tests,
    autocorrelation = autocorrelation_tests,
    heteroscedasticity = heteroscedasticity_tests,
    harmonics = harmonic_analysis,
    variance_model = variance_model,
    model_adequacy = model_adequacy,
    summary = list(
      mean_residual = mean(residuals, na.rm = TRUE),
      sd_residual = sd(residuals, na.rm = TRUE),
      mean_abs_residual = mean(abs(residuals), na.rm = TRUE),
      is_normal = normality_tests$overall_normal,
      has_breakpoints = breakpoint_tests$has_breakpoints,
      breakpoint_years = breakpoint_tests$breakpoint_years,
      has_autocorrelation = autocorrelation_tests$has_autocorrelation,
      has_heteroscedasticity = heteroscedasticity_tests$heteroscedastic,
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

  sw_test <- tryCatch({
    test <- stats::shapiro.test(residuals)
    list(
      statistic = as.numeric(test$statistic),
      p_value = test$p.value,
      normal = test$p.value > 0.05
    )
  }, error = function(e) {
    list(statistic = NA, p_value = NA, normal = NA)
  })

  ad_test <- tryCatch({
    if (requireNamespace("nortest", quietly = TRUE)) {
      test <- nortest::ad.test(residuals)
      list(
        statistic = as.numeric(test$statistic),
        p_value = test$p.value,
        normal = test$p.value > 0.05
      )
    } else {
      list(statistic = NA, p_value = NA, normal = NA)
    }
  }, error = function(e) {
    list(statistic = NA, p_value = NA, normal = NA)
  })

  jb_test <- tryCatch({
    center <- mean(residuals)
    s_pop <- sqrt(mean((residuals - center)^2))
    if (!is.finite(s_pop) || s_pop <= .Machine$double.eps) {
      return(list(statistic = NA, p_value = NA, normal = NA))
    }
    skewness <- mean((residuals - center)^3) / (s_pop^3)
    kurtosis <- mean((residuals - center)^4) / (s_pop^4) - 3
    jb_stat <- n * (skewness^2 / 6 + kurtosis^2 / 24)
    p_value <- 1 - stats::pchisq(jb_stat, df = 2)
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

  list(
    shapiro_wilk = sw_test,
    anderson_darling = ad_test,
    jarque_bera = jb_test,
    overall_normal = overall_normal,
    skewness = m2_sample_skewness(residuals),
    kurtosis = m2_sample_kurtosis(residuals),
    interpretation = if (overall_normal) {
      "Residuals appear approximately normal"
    } else {
      "Residuals deviate from normality; review model adequacy"
    }
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

  cusum_results <- cusum_breakpoint_test(years, residuals)
  partition_results <- optimal_partition_breakpoints(years, residuals)
  variance_breaks <- rolling_variance_breaks(years, residuals)

  all_breakpoints <- c(
    cusum_results$breakpoint_years,
    partition_results$breakpoint_years,
    variance_breaks$breakpoint_years
  )
  all_breakpoints <- sort(unique(all_breakpoints))

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
  residual_sd <- stats::sd(residuals, na.rm = TRUE)
  cumulative <- cumsum(residuals - mean(residuals, na.rm = TRUE))
  cusum_max <- max(abs(cumulative), na.rm = TRUE)
  cusum_threshold <- 1.143 * sqrt(n) * ifelse(is.finite(residual_sd), residual_sd, 0)
  has_break <- is.finite(cusum_threshold) && cusum_threshold > 0 && cusum_max > cusum_threshold

  break_year <- if (has_break) years[which.max(abs(cumulative))] else NA_real_

  list(
    cusum_max = cusum_max,
    threshold = cusum_threshold,
    has_break = has_break,
    breakpoint_years = if (is.finite(break_year)) break_year else integer(0),
    statistic = if (cusum_threshold > 0) cusum_max / cusum_threshold else NA_real_
  )
}

#' Optimal partition breakpoints
optimal_partition_breakpoints <- function(years, residuals) {
  n <- length(residuals)
  if (n < 10) {
    return(list(breakpoint_years = integer(0), bic_values = NA_real_))
  }

  bic_values <- numeric(3)
  break_points <- vector("list", 3)

  bic_values[1] <- n * log(m2_safe_variance(residuals)) + 2 * log(n)
  break_points[[1]] <- integer(0)

  best_one_break <- Inf
  best_one_idx <- NA_integer_
  for (i in 2:(n - 2)) {
    seg1 <- residuals[1:i]
    seg2 <- residuals[(i + 1):n]
    bic <- i * log(m2_safe_variance(seg1)) +
      (n - i) * log(m2_safe_variance(seg2)) + 3 * log(n)
    if (is.finite(bic) && bic < best_one_break) {
      best_one_break <- bic
      best_one_idx <- i
    }
  }
  bic_values[2] <- best_one_break
  break_points[[2]] <- if (!is.na(best_one_idx)) years[best_one_idx] else integer(0)

  best_two_break <- Inf
  best_two_idx <- c(NA_integer_, NA_integer_)
  for (i in 2:(n - 4)) {
    for (j in (i + 2):(n - 2)) {
      seg1 <- residuals[1:i]
      seg2 <- residuals[(i + 1):j]
      seg3 <- residuals[(j + 1):n]
      bic <- i * log(m2_safe_variance(seg1)) +
        (j - i) * log(m2_safe_variance(seg2)) +
        (n - j) * log(m2_safe_variance(seg3)) + 4 * log(n)
      if (is.finite(bic) && bic < best_two_break) {
        best_two_break <- bic
        best_two_idx <- c(i, j)
      }
    }
  }
  bic_values[3] <- best_two_break
  break_points[[3]] <- if (all(!is.na(best_two_idx))) years[best_two_idx] else integer(0)

  best_model <- which.min(bic_values)

  list(
    bic_values = bic_values,
    best_model = best_model - 1,
    breakpoint_years = break_points[[best_model]]
  )
}

#' Rolling variance breakpoints
rolling_variance_breaks <- function(years, residuals, window = 5) {
  n <- length(residuals)
  if (n < window * 2) {
    return(list(breakpoint_years = integer(0)))
  }

  rolling_vars <- numeric(n - window + 1)
  for (i in seq_len(n - window + 1)) {
    rolling_vars[i] <- stats::var(residuals[i:(i + window - 1)], na.rm = TRUE)
  }

  var_changes <- abs(diff(rolling_vars))
  threshold <- stats::quantile(var_changes, 0.9, na.rm = TRUE)
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
# Autocorrelation and Heteroscedasticity Tests
# ============================================================================

test_residual_autocorrelation <- function(residuals) {
  n <- length(residuals)
  if (n < 10) {
    return(list(
      has_autocorrelation = NA,
      dw_statistic = NA,
      lb_p_value = NA,
      acf_values = numeric(),
      acf_lags = numeric(),
      pacf_values = numeric(),
      pacf_lags = numeric(),
      interpretation = "Insufficient data"
    ))
  }

  max_lag <- min(10, n - 1)
  acf_obj <- tryCatch(stats::acf(residuals, lag.max = max_lag, plot = FALSE), error = function(e) NULL)
  pacf_obj <- tryCatch(stats::pacf(residuals, lag.max = max_lag, plot = FALSE), error = function(e) NULL)

  acf_values <- if (!is.null(acf_obj)) as.numeric(acf_obj$acf[-1]) else numeric()
  acf_lags <- if (!is.null(acf_obj)) as.numeric(acf_obj$lag[-1]) else numeric()
  pacf_values <- if (!is.null(pacf_obj)) as.numeric(pacf_obj$acf) else numeric()
  pacf_lags <- if (length(pacf_values) > 0) seq_along(pacf_values) else numeric()

  dw_stat <- tryCatch({
    aux_df <- data.frame(residual = residuals, time = seq_along(residuals))
    as.numeric(lmtest::dwtest(residual ~ time, data = aux_df)$statistic)
  }, error = function(e) {
    sum(diff(residuals)^2) / sum(residuals^2)
  })

  lb_test <- tryCatch({
    test <- stats::Box.test(residuals, lag = max_lag, type = "Ljung-Box")
    list(statistic = as.numeric(test$statistic), p_value = test$p.value)
  }, error = function(e) {
    list(statistic = NA, p_value = NA)
  })

  has_autocorr <- (!is.na(dw_stat) && (dw_stat < 1.5 || dw_stat > 2.5)) ||
    (!is.na(lb_test$p_value) && lb_test$p_value < 0.05)

  list(
    has_autocorrelation = has_autocorr,
    dw_statistic = dw_stat,
    lb_statistic = lb_test$statistic,
    lb_p_value = lb_test$p_value,
    acf_values = acf_values,
    acf_lags = acf_lags,
    pacf_values = pacf_values,
    pacf_lags = pacf_lags,
    interpretation = if (has_autocorr) {
      "Residuals show autocorrelation; consider time-series structure"
    } else {
      "No significant autocorrelation detected in residuals"
    }
  )
}

test_residual_heteroscedasticity <- function(years, residuals) {
  n <- length(residuals)
  if (n < 8) {
    return(list(
      heteroscedastic = NA,
      statistic = NA,
      p_value = NA,
      slope = NA,
      slope_p_value = NA,
      interpretation = "Insufficient data for heteroscedasticity diagnostics"
    ))
  }

  aux_df <- data.frame(
    year = suppressWarnings(as.numeric(years)),
    residual = suppressWarnings(as.numeric(residuals))
  )

  bp_result <- tryCatch({
    base_model <- stats::lm(residual ~ year, data = aux_df)
    test <- lmtest::bptest(base_model)
    list(statistic = as.numeric(test$statistic), p_value = test$p.value)
  }, error = function(e) {
    list(statistic = NA, p_value = NA)
  })

  variance_fit <- tryCatch(stats::lm(I(residual^2) ~ year, data = aux_df), error = function(e) NULL)
  slope <- NA_real_
  slope_p_value <- NA_real_
  if (!is.null(variance_fit)) {
    variance_summary <- summary(variance_fit)
    coefs <- variance_summary$coefficients
    if ("year" %in% rownames(coefs)) {
      slope <- coefs["year", "Estimate"]
      slope_p_value <- coefs["year", "Pr(>|t|)"]
    }
  }

  heteroscedastic <- (!is.na(bp_result$p_value) && bp_result$p_value < 0.05) ||
    (!is.na(slope_p_value) && slope_p_value < 0.05)

  list(
    heteroscedastic = heteroscedastic,
    statistic = bp_result$statistic,
    p_value = bp_result$p_value,
    slope = slope,
    slope_p_value = slope_p_value,
    interpretation = if (heteroscedastic) {
      "Residual variance changes over time; heteroscedasticity detected"
    } else {
      "Residual variance appears approximately stable over time"
    }
  )
}

# ============================================================================
# Harmonic Analysis on Residuals
# ============================================================================

analyze_residual_harmonics_residuals <- function(years, residuals) {
  n <- length(residuals)
  if (n < 8) {
    return(list(
      dominant_period = NA,
      harmonics = data.frame(),
      interpretation = "Insufficient data for harmonic analysis"
    ))
  }

  fft_result <- stats::fft(residuals)
  fft_mod <- Mod(fft_result)[1:(n %/% 2 + 1)]
  freqs <- (0:(n %/% 2)) / n
  power <- fft_mod^2
  total_power <- sum(power, na.rm = TRUE)
  if (!is.finite(total_power) || total_power <= .Machine$double.eps) {
    total_power <- 1
  }

  peak_idx <- which.max(fft_mod[2:(n %/% 2)]) + 1
  dominant_period <- if (freqs[peak_idx] > 0) 1 / freqs[peak_idx] else NA_real_

  spectrum_df <- data.frame(
    frequency = freqs,
    period = ifelse(freqs > 0, 1 / freqs, Inf),
    power = power,
    normalized_power = power / total_power
  )

  top_harmonics <- spectrum_df[spectrum_df$frequency > 0, , drop = FALSE]
  top_harmonics <- top_harmonics[order(-top_harmonics$power), , drop = FALSE]
  top_harmonics <- head(top_harmonics, 5)

  lomb_results <- tryCatch({
    if (requireNamespace("lomb", quietly = TRUE)) {
      ls_result <- lomb::lsp(residuals, times = years, from = 2, to = n / 2, plot = FALSE)
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

  list(
    dominant_period = dominant_period,
    dominant_frequency = freqs[peak_idx],
    spectrum = spectrum_df,
    top_harmonics = top_harmonics,
    lomb_scargle = lomb_results,
    variance_explained = sum(head(power, 3), na.rm = TRUE) / total_power,
    interpretation = if (!is.na(dominant_period)) {
      sprintf("Dominant residual period: %.1f years", dominant_period)
    } else {
      "No clear periodic structure in residuals"
    }
  )
}

# ============================================================================
# Model Adequacy Assessment and Variance Models
# ============================================================================

assess_model_adequacy <- function(residuals, predicted, observed) {
  n <- length(residuals)
  k <- 2

  ss_res <- sum(residuals^2, na.rm = TRUE)
  ss_tot <- sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
  r_squared <- if (ss_tot > 0) 1 - ss_res / ss_tot else 0
  adj_r_squared <- if (n > k + 1) 1 - (1 - r_squared) * (n - 1) / (n - k - 1) else r_squared
  rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
  mae <- mean(abs(residuals), na.rm = TRUE)
  mape <- mean(abs(residuals / pmax(abs(observed), .Machine$double.eps)), na.rm = TRUE) * 100
  aic <- n * log(max(ss_res / max(n, 1), .Machine$double.eps)) + 2 * k
  bic <- n * log(max(ss_res / max(n, 1), .Machine$double.eps)) + log(max(n, 1)) * k
  rse <- sqrt(ss_res / max(n - k, 1))
  cv <- rse / max(mean(abs(observed), na.rm = TRUE), .Machine$double.eps)

  model_adequate <- r_squared > 0.7 &&
    abs(mean(residuals, na.rm = TRUE)) < 0.1 * stats::sd(residuals, na.rm = TRUE) &&
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
      sprintf("Model adequacy: R2 = %.3f, RMSE = %.2f, CV = %.2f", r_squared, rmse, cv)
    } else {
      sprintf("Model may be inadequate: R2 = %.3f, CV = %.2f", r_squared, cv)
    }
  )
}

fit_residual_variance_models <- function(years, squared_residuals) {
  if (length(years) < 5 || all(!is.finite(squared_residuals))) {
    return(list(status = "error", comparison = data.frame()))
  }

  df <- data.frame(
    year = suppressWarnings(as.numeric(years)),
    value = pmax(suppressWarnings(as.numeric(squared_residuals)), 0)
  )
  epsilon <- max(mean(df$value[df$value > 0], na.rm = TRUE) * 1e-6, .Machine$double.eps)

  candidates <- list()
  candidates$Linear <- m2_fit_variance_candidate(
    df, "Linear",
    fit = function(d) stats::lm(value ~ year, data = d),
    predict = function(mod, new_years) stats::predict(mod, newdata = data.frame(year = new_years))
  )
  candidates$Quadratic <- m2_fit_variance_candidate(
    df, "Quadratic",
    fit = function(d) stats::lm(value ~ year + I(year^2), data = d),
    predict = function(mod, new_years) stats::predict(mod, newdata = data.frame(year = new_years))
  )
  candidates$Cubic <- m2_fit_variance_candidate(
    df, "Cubic",
    fit = function(d) stats::lm(value ~ year + I(year^2) + I(year^3), data = d),
    predict = function(mod, new_years) stats::predict(mod, newdata = data.frame(year = new_years))
  )
  if (all(df$year > 0, na.rm = TRUE)) {
    candidates$Logarithmic <- m2_fit_variance_candidate(
      df, "Logarithmic",
      fit = function(d) stats::lm(value ~ log(year), data = d),
      predict = function(mod, new_years) stats::predict(mod, newdata = data.frame(year = new_years))
    )
  }
  candidates$Exponential <- m2_fit_variance_candidate(
    df, "Exponential",
    fit = function(d) stats::lm(log(value + epsilon) ~ year, data = d),
    predict = function(mod, new_years) exp(stats::predict(mod, newdata = data.frame(year = new_years))) - epsilon
  )

  candidates <- Filter(Negate(is.null), candidates)
  if (length(candidates) == 0) {
    return(list(status = "error", comparison = data.frame()))
  }

  comparison <- do.call(rbind, lapply(candidates, function(candidate) {
    data.frame(
      model = candidate$model_name,
      r_squared = candidate$r_squared,
      rmse = candidate$rmse,
      stringsAsFactors = FALSE
    )
  }))
  comparison <- comparison[order(-comparison$r_squared, comparison$rmse), , drop = FALSE]
  best_name <- comparison$model[1]
  best_model <- candidates[[best_name]]

  year_grid <- seq(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE), length.out = 100)
  smooth_fit <- data.frame(
    Year = year_grid,
    Fitted = pmax(best_model$predict(year_grid), 0)
  )
  observed_fit <- data.frame(
    Year = df$year,
    Fitted = pmax(best_model$fitted, 0)
  )

  list(
    status = "success",
    best_model = best_name,
    best_r_squared = best_model$r_squared,
    comparison = comparison,
    observed_fit = observed_fit,
    smooth_fit = smooth_fit
  )
}

m2_fit_variance_candidate <- function(df, model_name, fit, predict) {
  model <- tryCatch(fit(df), error = function(e) NULL)
  if (is.null(model)) {
    return(NULL)
  }

  fitted <- tryCatch(predict(model, df$year), error = function(e) NULL)
  if (is.null(fitted)) {
    return(NULL)
  }

  fitted <- pmax(as.numeric(fitted), 0)
  ss_res <- sum((df$value - fitted)^2, na.rm = TRUE)
  ss_tot <- sum((df$value - mean(df$value, na.rm = TRUE))^2, na.rm = TRUE)
  r_squared <- if (ss_tot > 0) 1 - ss_res / ss_tot else 0
  rmse <- sqrt(mean((df$value - fitted)^2, na.rm = TRUE))

  list(
    model_name = model_name,
    fitted = fitted,
    r_squared = r_squared,
    rmse = rmse,
    predict = function(new_years) pmax(as.numeric(predict(model, new_years)), 0)
  )
}

# ============================================================================
# Helpers
# ============================================================================

m2_standardize_vector <- function(x) {
  x_sd <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(x_sd) || x_sd <= .Machine$double.eps) {
    return(rep(0, length(x)))
  }
  x / x_sd
}

m2_sample_skewness <- function(x) {
  center <- mean(x, na.rm = TRUE)
  spread <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(spread) || spread <= .Machine$double.eps) {
    return(NA_real_)
  }
  mean((x - center)^3, na.rm = TRUE) / (spread^3)
}

m2_sample_kurtosis <- function(x) {
  center <- mean(x, na.rm = TRUE)
  spread <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(spread) || spread <= .Machine$double.eps) {
    return(NA_real_)
  }
  mean((x - center)^4, na.rm = TRUE) / (spread^4) - 3
}

m2_safe_variance <- function(x) {
  value <- stats::var(x, na.rm = TRUE)
  if (!is.finite(value) || value <= .Machine$double.eps) {
    return(.Machine$double.eps)
  }
  value
}
