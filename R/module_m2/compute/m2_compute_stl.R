# ============================================================================
# m2_compute_stl.R - STL Time Series Decomposition
# ============================================================================
# Seasonal-Trend decomposition using LOESS (STL) for bibliometric time series.
#
# IEEE Q1 Enhancement: Robust decomposition into trend, seasonal, and
# remainder components with configurable seasonal and trend windows.

#' Compute STL decomposition for time series
#'
#' Decomposes publication time series into trend, seasonal, and remainder
#' components using LOESS smoothing.
#'
#' @param data Data frame with Year and Articles columns
#' @param config Configuration list
#' @return List with STL decomposition results
#' @export
compute_m2_stl <- function(data, config = biblio_config()) {
  validate_is_data_frame(data)
  
  required_cols <- c("Year", "Articles")
  validation <- validate_required_columns(data, required_cols)
  if (!validation$ok) {
    return(list(
      stl = list(),
      components = data.frame(),
      summary = list(),
      status = paste("error: missing columns:", paste(validation$missing_columns, collapse = ", "))
    ))
  }
  
  if (nrow(data) < 2 * 12) {
    seasonal_period <- max(1, floor(nrow(data) / 4))
  } else {
    seasonal_period <- identify_seasonality(data$Articles, data$Year)
  }
  
  if (seasonal_period < 2) seasonal_period <- 1
  if (seasonal_period > nrow(data) / 2) seasonal_period <- floor(nrow(data) / 2)
  
  ts_data <- ts(data$Articles, frequency = max(1, seasonal_period))
  
  stl_result <- tryCatch({
    stl_with_fallback(ts_data, seasonal_period)
  }, error = function(e) {
    list(
      time.series = NULL,
      error = e$message
    )
  })
  
  if (is.null(stl_result$time.series) && !is.null(stl_result$error)) {
    stl_result <- simple_decomposition(data$Articles, data$Year)
  }
  
  components <- extract_stl_components(stl_result, data$Year)
  
  summary_stats <- compute_stl_summary(components)
  
  trend_slopes <- compute_trend_slopes(components)
  
  list(
    stl = list(
      seasonal_period = seasonal_period,
      components = components,
      trend_interpretation = interpret_trend(trend_slopes),
      seasonal_strength = summary_stats$seasonal_strength,
      trend_strength = summary_stats$trend_strength,
      remainder_variance = summary_stats$remainder_variance,
      is_seasonal = summary_stats$seasonal_strength > 0.5,
      trend_direction = if (trend_slopes$overall_slope > 0) "increasing" else if (trend_slopes$overall_slope < 0) "decreasing" else "stable"
    ),
    components = components,
    summary = summary_stats,
    trend_slopes = trend_slopes,
    status = "success"
  )
}

#' STL with fallback for short series
#' @keywords internal
stl_with_fallback <- function(ts_data, seasonal_period) {
  if (length(ts_data) < 2 * frequency(ts_data)) {
    return(simple_decomposition(as.numeric(ts_data), seq_along(ts_data)))
  }
  
  n <- length(ts_data)
  s.window <- min(n - 1, 7)
  t.window <- min(n - 1, 21)
  
  if (frequency(ts_data) < 2) {
    return(simple_decomposition(as.numeric(ts_data), seq_along(ts_data)))
  }
  
  tryCatch({
    result <- stl(ts_data, s.window = s.window, t.window = t.window, robust = TRUE)
    list(time.series = result$time.series, method = "stl")
  }, error = function(e) {
    simple_decomposition(as.numeric(ts_data), seq_along(ts_data))
  })
}

#' Simple decomposition fallback
#' @keywords internal
simple_decomposition <- function(values, years) {
  n <- length(values)
  
  trend_window <- max(3, min(21, floor(n / 3)))
  if (trend_window %% 2 == 0) trend_window <- trend_window + 1
  
  trend <- stats::filter(values, rep(1/trend_window, trend_window), sides = 2)
  
  trend[is.na(trend)] <- values[is.na(trend)]
  
  detrended <- values - trend
  
  period <- identify_seasonality(values, years)
  if (period < 2) period <- 1
  
  seasonal <- rep(0, n)
  if (period > 1 && n >= 2 * period) {
    seasonal_matrix <- matrix(detrended[1:(period * floor(n / period))], 
                               nrow = period)
    seasonal_pattern <- rowMeans(seasonal_matrix, na.rm = TRUE)
    seasonal_pattern <- seasonal_pattern - mean(seasonal_pattern)
    seasonal <- rep(seasonal_pattern, ceiling(n / period))[1:n]
  }
  
  remainder <- values - trend - seasonal
  
  ts_result <- cbind(
    seasonal = seasonal,
    trend = trend,
    remainder = remainder
  )
  
  list(time.series = ts_result, method = "simple")
}

#' Identify seasonality in data
#' @keywords internal
identify_seasonality <- function(values, years) {
  n <- length(values)
  if (n < 4) return(1)
  
  acf_vals <- acf(values, lag.max = min(n - 1, 12), plot = FALSE)$acf[-1]
  
  threshold <- qnorm(0.95) / sqrt(n)
  
  significant_lags <- which(abs(acf_vals) > threshold)
  
  if (length(significant_lags) == 0) return(1)
  
  candidate_periods <- significant_lags[significant_lags > 1]
  
  if (length(candidate_periods) == 0) return(1)
  
  best_period <- candidate_periods[which.max(abs(acf_vals[candidate_periods]))]
  
  min(best_period, n %/% 2)
}

#' Extract STL components
#' @keywords internal
extract_stl_components <- function(stl_result, years) {
  if (is.null(stl_result$time.series)) {
    return(data.frame(
      year = years,
      observed = NA_real_,
      trend = NA_real_,
      seasonal = NA_real_,
      remainder = NA_real_
    ))
  }
  
  ts_components <- stl_result$time.series
  
  data.frame(
    year = years,
    observed = ts_components[, "trend"] + ts_components[, "seasonal"] + ts_components[, "remainder"],
    trend = as.numeric(ts_components[, "trend"]),
    seasonal = as.numeric(ts_components[, "seasonal"]),
    remainder = as.numeric(ts_components[, "remainder"])
  )
}

#' Compute STL summary statistics
#' @keywords internal
compute_stl_summary <- function(components) {
  if (nrow(components) == 0 || all(is.na(components$trend))) {
    return(list(
      seasonal_strength = NA_real_,
      trend_strength = NA_real_,
      remainder_variance = NA_real_
    ))
  }
  
  observed_var <- var(components$observed, na.rm = TRUE)
  if (is.na(observed_var) || observed_var == 0) {
    return(list(
      seasonal_strength = 0,
      trend_strength = 0,
      remainder_variance = 0
    ))
  }
  
  seasonal_var <- var(components$seasonal, na.rm = TRUE)
  remainder_var <- var(components$remainder, na.rm = TRUE)
  trend_var <- var(components$trend, na.rm = TRUE)
  
  seasonal_strength <- if (remainder_var > 0) {
    max(0, 1 - remainder_var / (seasonal_var + remainder_var + 1e-10))
  } else {
    seasonal_var / observed_var
  }
  
  trend_strength <- if (remainder_var > 0) {
    max(0, 1 - remainder_var / (trend_var + remainder_var + 1e-10))
  } else {
    trend_var / observed_var
  }
  
  list(
    seasonal_strength = seasonal_strength,
    trend_strength = trend_strength,
    remainder_variance = remainder_var,
    observed_variance = observed_var,
    seasonal_variance = seasonal_var,
    trend_variance = trend_var
  )
}

#' Compute trend slopes (early, mid, late periods)
#' @keywords internal
compute_trend_slopes <- function(components) {
  n <- nrow(components)
  if (n < 3) {
    return(list(
      early_slope = NA_real_,
      mid_slope = NA_real_,
      late_slope = NA_real_,
      overall_slope = NA_real_
    ))
  }
  
  early_end <- floor(n / 3)
  mid_start <- early_end + 1
  mid_end <- floor(2 * n / 3)
  late_start <- mid_end + 1
  
  early_trend <- if (early_end >= 2) {
    lm(components$trend[1:early_end] ~ components$year[1:early_end])$coefficients[2]
  } else {
    NA_real_
  }
  
  mid_trend <- if (mid_end - mid_start >= 1) {
    lm(components$trend[mid_start:mid_end] ~ components$year[mid_start:mid_end])$coefficients[2]
  } else {
    NA_real_
  }
  
  late_trend <- if (n - late_start >= 1) {
    lm(components$trend[late_start:n] ~ components$year[late_start:n])$coefficients[2]
  } else {
    NA_real_
  }
  
  overall_trend <- lm(components$trend ~ components$year)$coefficients[2]
  
  list(
    early_slope = early_trend,
    mid_slope = mid_trend,
    late_slope = late_trend,
    overall_slope = overall_trend
  )
}

#' Interpret trend
#' @keywords internal
interpret_trend <- function(trend_slopes) {
  overall <- trend_slopes$overall_slope
  
  if (is.na(overall)) {
    return("Unable to determine trend direction")
  }
  
  slope_per_year <- overall
  
  if (overall > 0) {
    base_value <- 100
    growth_pct <- (slope_per_year / base_value) * 100
    
    if (abs(growth_pct) < 1) {
      paste0("Steady increasing trend (+", round(slope_per_year, 1), " papers/year)")
    } else if (growth_pct < 5) {
      paste0("Moderate increasing trend (+", round(growth_pct, 1), "% annual growth)")
    } else {
      paste0("Strong increasing trend (+", round(growth_pct, 1), "% annual growth)")
    }
  } else if (overall < 0) {
    base_value <- 100
    decline_pct <- abs((slope_per_year / base_value) * 100)
    
    if (abs(decline_pct) < 1) {
      paste0("Steady declining trend (", round(slope_per_year, 1), " papers/year)")
    } else if (decline_pct < 5) {
      paste0("Moderate declining trend (", round(decline_pct, 1), "% annual decline)")
    } else {
      paste0("Strong declining trend (", round(decline_pct, 1), "% annual decline)")
    }
  } else {
    "Stable trend (no significant change)"
  }
}