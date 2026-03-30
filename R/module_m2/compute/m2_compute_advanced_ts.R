# ============================================================================
# m2_compute_advanced.ts.R - Advanced Time Series Analysis
# ============================================================================
# SARIMA, Prophet, TBATS, Bai-Perron, CUSUM, Wavelet analysis

#' Compute advanced time series analysis
#'
#' @param input Data frame with Year and Articles columns
#' @param config Configuration list
#' @return List with advanced TS models
#' @export
compute_m2_advanced_ts <- function(input, config = biblio_config()) {
  year_col <- if ("Year" %in% names(input)) "Year" else names(input)[1]
  articles_col <- if ("Articles" %in% names(input)) "Articles" else names(input)[2]
  
  years <- input[[year_col]]
  articles <- input[[articles_col]]
  
  if (length(years) < 10) {
    return(list(status = "error: insufficient data for advanced TS (need10+ years)"))
  }
  
  results <- list()
  
  # 1. SARIMA (if seasonal)
  results$sarima <- fit_sarima(articles, years)
  
  # 2. TBATS (complex seasonality)
  results$tbats <- fit_tbats(articles, years)
  
  # 3. Prophet-style forecast
  results$prophet <- fit_prophet_simple(articles, years)
  
  # 4. State space model (Kalman filter)
  results$state_space <- fit_state_space(articles, years)
  
  # 5. Dynamic regression
  results$dynamic_regression <- fit_dynamic_regression(articles, years)
  
  # 6. Model comparison
  results$comparison <- compare_ts_models(results)
  
  # 7. Best model
  best_model <- names(which.min(sapply(results, function(x) {
    if (is.list(x) && !is.null(x$AIC)) x$AIC else Inf
  })))
  results$best_model <- best_model
  
  results$status <- "success"
  results
}

#' Fit SARIMA (Seasonal ARIMA)
#' @keywords internal
fit_sarima <- function(articles, years) {
  n <- length(articles)
  
  # Check for seasonality
  season_length <- detect_seasonality(articles)
  
  if (season_length == 0) {
    # No seasonality detected, use regular ARIMA
    return(list(
      status = "no seasonality detected",
      model = NULL,
      AIC = NA,
      BIC = NA,
      forecast = NULL
    ))
  }
  
  # Try to fit SARIMA using forecast package
  sarima_result <- tryCatch({
    if (requireNamespace("forecast", quietly = TRUE)) {
      ts_data <- stats::ts(articles, frequency = season_length)
      
      # Auto SARIMA
      fit <- forecast::auto.arima(ts_data, 
                                  seasonal = TRUE,
                                  stepwise = FALSE,
                                  trace = FALSE)
      
      fitted_vals <- as.numeric(fitted(fit))
      residuals <- as.numeric(residuals(fit))
      
      # Forecast
      fc <- forecast::forecast(fit, h = 5)
      
      list(
        status = "success",
        model = fit,
        order = fit$arma[c(1, 6, 2)],  # p, d, q
        seasonal_order = fit$arma[c(3, 7, 4)],  # P, D, Q
        season_length = season_length,
        AIC = AIC(fit),
        BIC = BIC(fit),
        fitted = fitted_vals,
        residuals = residuals,
        forecast = list(
          point = as.numeric(fc$mean),
          lower = as.numeric(fc$lower[, 2]),
          upper = as.numeric(fc$upper[, 2])
        ),
        forecast_years = (max(years) + 1):(max(years) + 5)
      )
    } else {
      list(status = "forecast package not available")
    }
  }, error = function(e) {
    list(status = paste("error:", e$message))
  })
  
  sarima_result
}

#' Detect seasonality in time series
#' @keywords internal
detect_seasonality <- function(x) {
  n <- length(x)
  if (n < 8) return(0)
  
  # Try common seasonal periods
  periods <- c(2, 4, 6, 12)  # bi-annual, quarterly, semi-annual, annual
  
  best_period <- 0
  best_strength <- 0
  
  for (p in periods) {
    if (n < 2 * p) next
    
    # Calculate seasonal strength
    decomp <- tryCatch({
      stl(ts(x, frequency = p), s.window = "periodic", robust = TRUE)
    }, error = function(e) NULL)
    
    if (!is.null(decomp)) {
      var_seasonal <- var(decomp$time.series[, "seasonal"])
      var_residual <- var(decomp$time.series[, "remainder"])
      
      strength <- var_seasonal / (var_seasonal + var_residual)
      
      if (strength > best_strength && strength > 0.3) {
        best_strength <- strength
        best_period <- p
      }
    }
  }
  
  best_period
}

#' Fit TBATS model
#' @keywords internal
fit_tbats <- function(articles, years) {
  n <- length(articles)
  
  tbats_result <- tryCatch({
    if (requireNamespace("forecast", quietly = TRUE)) {
      ts_data <- stats::ts(articles)
      
      fit <- forecast::tbats(ts_data, use.box.cox = FALSE)
      
      fitted_vals <- as.numeric(fitted(fit))
      residuals <- as.numeric(residuals(fit))
      
      # Extract components
      components <- list(
        trend = if (!is.null(fit$trend)) as.numeric(fit$trend$fitted) else NULL,
        season = if (!is.null(fit$seasonal)) as.numeric(fit$seasonal$fitted) else NULL
      )
      
      # Forecast
      fc <- forecast::forecast(fit, h = 5)
      
      list(
        status = "success",
        model = fit,
        AIC = AIC(fit),
        BIC = BIC(fit),
        lambda = fit$lambda,
        fitted = fitted_vals,
        residuals = residuals,
        components = components,
        forecast = list(
          point = as.numeric(fc$mean),
          lower = as.numeric(fc$lower[, 2]),
          upper = as.numeric(fc$upper[, 2])
        ),
        forecast_years = (max(years) + 1):(max(years) + 5)
      )
    } else {
      list(status = "forecast package not available")
    }
  }, error = function(e) {
    list(status = paste("error:", e$message))
  })
  
  tbats_result
}

#' Fit Prophet-style forecast (simplified)
#' @keywords internal
fit_prophet_simple <- function(articles, years) {
  n <- length(articles)
  
  # Prophet-like approach: piecewise linear with trend changes
  # Fit trend first, then add seasonality and holidays
  
  prophet_result <- tryCatch({
    # Trend component
    df <- data.frame(ds = years, y = articles)
    
    # Fit trend (piecewise linear)
    trend_fit <- fit_piecewise_trend(years, articles)
    
    # Detrend
    detrended <- articles - trend_fit$fitted
    
    # Fit yearly seasonality
    yearly_season <- fit_yearly_seasonality(years, detrended)
    
    # Components
    trend <- trend_fit$fitted
    yearly <- yearly_season$fitted
    residual <- articles - trend - yearly
    
    # Forecast
    forecast_years <- (max(years) + 1):(max(years) + 5)
    forecast_trend <- predict_trend(trend_fit, forecast_years)
    forecast_yearly <- predict_yearly(yearly_season, forecast_years)
    
    forecast_point <- forecast_trend + forecast_yearly
    
    # Add prediction intervals
    residual_sd <- sd(residual, na.rm = TRUE)
    forecast_lower <- forecast_point - 1.96 * residual_sd
    forecast_upper <- forecast_point + 1.96 * residual_sd
    
    list(
      status = "success",
      trend = trend,
      yearly_seasonality = yearly,
      residual = residual,
      trend_fit = trend_fit,
      yearly_fit = yearly_season,
      AIC = n * log(sum(residual^2) / n) + 4,  # Approximate AIC
      fitted = trend + yearly,
      forecast = list(
        point = forecast_point,
        lower = forecast_lower,
        upper = forecast_upper
      ),
      forecast_years = forecast_years,
      changepoints = trend_fit$changepoints
    )
  }, error = function(e) {
    list(status = paste("error:", e$message))
  })
  
  prophet_result
}

#' Fit piecewise trend
#' @keywords internal
fit_piecewise_trend <- function(years, articles) {
  n <- length(years)
  
  # Detect changepoints
  n_changepoints <- max(1, floor(n / 10))
  changepoint_candidates <- years[2:(n - 1)]
  
  # Use simple approach: fit linear trend
  fit <- lm(articles ~ years)
  fitted_vals <- fitted(fit)
  
  # Potentially detect changes
  residuals <- articles - fitted_vals
  
  # CUSUM for changepoint detection
  standardized_resid <- residuals / sd(residuals)
  cusum <- cumsum(standardized_resid - mean(standardized_resid))
  
  # Find significant changes
  threshold <- 1.358 * sqrt(n)  # Approximate 5% threshold
  changepoint_indices <- which(abs(cusum) > threshold)
  changepoints <- years[changepoint_indices]
  
  list(
    fitted = fitted_vals,
    slope = coef(fit)[2],
    intercept = coef(fit)[1],
    changepoints = changepoints,
    residuals = residuals
  )
}

#' Fit yearly seasonality
#' @keywords internal
fit_yearly_seasonality <- function(years, detrended) {
  n <- length(years)
  
  # Fit Fourier series
  t <- years - min(years)
  period <- 1  # Yearly
  
  # Use 2 harmonics
  X <- cbind(
    sin(2 * pi * t / period),
    cos(2 * pi * t / period),
    sin(4 * pi * t / period),
    cos(4 * pi * t / period)
  )
  
  fit <- lm(detrended ~ X)
  fitted_vals <- fitted(fit)
  
  list(
    fitted = fitted_vals,
    coefficients = coef(fit),
    n_harmonics = 2
  )
}

#' Predict trend
#' @keywords internal
predict_trend <- function(trend_fit, years) {
  trend_fit$intercept + trend_fit$slope * years
}

#' Predict yearly seasonality
#' @keywords internal
predict_yearly <- function(yearly_fit, years) {
  t <- years - min(years)
  period <- 1
  
  X <- cbind(
    sin(2 * pi * t / period),
    cos(2 * pi * t / period),
    sin(4 * pi * t / period),
    cos(4 * pi * t / period)
  )
  
  yearly_fit$coefficients[1] + X %*% yearly_fit$coefficients[-1]
}

#' Fit state space model (Kalman filter)
#' @keywords internal
fit_state_space <- function(articles, years) {
  n <- length(articles)
  
  # Local linear trend model
  # y[t] = mu[t] + e[t]
  # mu[t+1] = mu[t] + nu[t] + eta[t]
  # nu[t+1] = nu[t] + xi[t]
  
  state_result <- tryCatch({
    # Initialize
    mu <- articles[1]
    nu <- articles[2] - articles[1]
    var_e <- var(diff(articles)) / 2
    var_eta <- var_e / 10
    var_xi <- var_e / 100
    
    # Kalman filter
    mu_filtered <- numeric(n)
    nu_filtered <- numeric(n)
    
    P_mu <- 1
    P_nu <- 1
    
    for (t in 1:n) {
      # Prediction
      mu_pred <- mu
      nu_pred <- nu
      
      # Update
      residual <- articles[t] - mu_pred
      
      # Kalman gain
      K_mu <- P_mu / (P_mu + var_e)
      
      mu <- mu_pred + K_mu * residual
      nu <- nu_pred + 0.01 * residual  # Small update for trend
      
      mu_filtered[t] <- mu
      nu_filtered[t] <- nu
      
      P_mu <- (1 - K_mu) * P_mu + var_eta
    }
    
    # Smoothed estimates
    fitted_vals <- mu_filtered
    
    # Forecast
    forecast_years <- (max(years) + 1):(max(years) + 5)
    forecast <- mu + nu * (1:5)
    
    list(
      status = "success",
      fitted = fitted_vals,
      trend = mu_filtered,
      slope = nu_filtered,
      forecast = list(
        point = forecast,
        lower = forecast - 1.96 * sqrt(var_e),
        upper = forecast + 1.96 * sqrt(var_e)
      ),
      forecast_years = forecast_years,
      variance = var_e
    )
  }, error = function(e) {
    list(status = paste("error:", e$message))
  })
  
  state_result
}

#' Fit dynamic regression (regression with ARIMA errors)
#' @keywords internal
fit_dynamic_regression <- function(articles, years) {
  n <- length(articles)
  
  dyn_result <- tryCatch({
    # Create time trend
    t <- 1:n
    
    # Fit regression with AR errors
    # Uses Cochrane-Orcutt or similar
    
    # First, OLS
    ols_fit <- lm(articles ~ t)
    residuals <- resid(ols_fit)
    
    # Estimate AR(1) coefficient
    ar_coef <- tryCatch({
      ar(residuals, order.max = 1)$ar[1]
    }, error = function(e) 0.5)
    
    # Apply Cochrane-Orcutt transformation
    if (abs(ar_coef) < 0.99) {
      y_transformed <- articles[-1] - ar_coef * articles[-n]
      t_transformed <- t[-1] - ar_coef * t[-n]
      
      fit_transformed <- lm(y_transformed ~ t_transformed)
      
      # Back-transform
      intercept <- coef(fit_transformed)[1] / (1 - ar_coef)
      slope <- coef(fit_transformed)[2]
      
      fitted_vals <- intercept + slope * t
      fitted_vals[1] <- articles[1]  # First observation
    } else {
      fitted_vals <- fitted(ols_fit)
      intercept <- coef(ols_fit)[1]
      slope <- coef(ols_fit)[2]
    }
    
    # Forecast
    forecast_years <- (max(years) + 1):(max(years) + 5)
    forecast_t <- (n + 1):(n + 5)
    forecast <- intercept + slope * forecast_t
    
    # Adjust for AR errors
    last_residual <- residuals[n]
    forecast <- forecast + ar_coef * last_residual
    
    list(
      status = "success",
      intercept = intercept,
      slope = slope,
      ar_coef = ar_coef,
      fitted = fitted_vals,
      residuals = residuals,
      AIC = n * log(sum(residuals^2) / n) + 4,  # Approximate
      forecast = list(
        point = forecast,
        lower = forecast - 1.96 * sd(residuals),
        upper = forecast + 1.96 * sd(residuals)
      ),
      forecast_years = forecast_years
    )
  }, error = function(e) {
    list(status = paste("error:", e$message))
  })
  
  dyn_result
}

#' Compare time series models
#' @keywords internal
compare_ts_models <- function(results) {
  models <- c("sarima", "tbats", "prophet", "state_space", "dynamic_regression")
  
  comparison <- data.frame(
    model = character(0),
    AIC = numeric(0),
    BIC = numeric(0),
    RMSE = numeric(0),
    MAE = numeric(0),
    MAPE = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for (m in models) {
    if (!is.null(results[[m]]) && results[[m]]$status == "success") {
      r <- results[[m]]
      
      fitted <- if (!is.null(r$fitted)) r$fitted else NULL
      actual <- if (!is.null(results$sarima) && !is.null(results$sarima$fitted)) {
        # Get original series
        attr(results$sarima$fitted, "actual")
      } else NULL
      
      rmse_val <- NA
      mae_val <- NA
      mape_val <- NA
      
      if (!is.null(fitted) && !is.null(actual)) {
        rmse_val <- sqrt(mean((actual - fitted)^2))
        mae_val <- mean(abs(actual - fitted))
        mape_val <- mean(abs((actual - fitted) / actual)) * 100
      }
      
      comparison <- rbind(comparison, data.frame(
        model = m,
        AIC = if (!is.null(r$AIC)) r$AIC else NA,
        BIC = if (!is.null(r$BIC)) r$BIC else NA,
        RMSE = rmse_val,
        MAE = mae_val,
        MAPE = mape_val
      ))
    }
  }
  
  comparison
}

#' Bai-Perron test for multiple structural breaks
#'
#' @param years Time points
#' @param articles Values
#' @param max_breaks Maximum number of breaks (default 5)
#' @return List with break points and test results
#' @export
compute_bai_perron <- function(years, articles, max_breaks = 5) {
  n <- length(years)
  
  if (n < 10) {
    return(list(status = "error: insufficient data"))
  }
  
  # Sequential procedure to find breakpoints
  breaks_found <- numeric(0)
  ssr_all <- sum((articles - mean(articles))^2)  # Total SSR
  
  # Trim parameter (minimum segment size)
  trim <- 0.15
  min_size <- ceiling(trim * n)
  
  current_data <- articles
  current_years <- years
  offset <- 0
  
  for (b in 1:min(max_breaks, floor(n / (2 * min_size)))) {
    best_break <- NA
    best_ssr <- Inf
    
    # Test each potential break point
    for (i in (min_size + 1):(length(current_data) - min_size)) {
      ssr_left <- sum((current_data[1:i] - mean(current_data[1:i]))^2)
      ssr_right <- sum((current_data[(i + 1):length(current_data)] - 
                          mean(current_data[(i + 1):length(current_data)]))^2)
      ssr_total <- ssr_left + ssr_right
      
      if (ssr_total < best_ssr) {
        best_ssr <- ssr_total
        best_break <- i + offset
      }
    }
    
    if (!is.na(best_break)) {
      # F-test for significance
      ssr_unrestricted <- best_ssr
      ssr_restricted <- ssr_all
      
      k <- length(breaks_found) + 2  # Number of regimes
      df_num <- k - 1
      df_den <- n - k
      
      F_stat <- ((ssr_restricted - ssr_unrestricted) / df_num) / (ssr_unrestricted / df_den)
      p_value <- 1 - pf(F_stat, df_num, df_den)
      
      if (p_value < 0.05) {
        breaks_found <- c(breaks_found, years[best_break])
        
        # Update for next iteration
        offset <- best_break
        current_data <- current_data[(best_break - offset + 1):length(current_data)]
        current_years <- years[(best_break + 1):n]
      } else {
        break
      }
    } else {
      break
    }
  }
  
  # Calculate SSR for each segment
  segments <- list()
  break_points <- sort(breaks_found)
  break_indices <- which(years %in% break_points)
  
  start_idx <- 1
  for (i in seq_along(break_indices)) {
    end_idx <- break_indices[i]
    segments[[i]] <- list(
      start_year = years[start_idx],
      end_year = years[end_idx],
      mean = mean(articles[start_idx:end_idx]),
      slope = if (end_idx - start_idx > 1) {
        coef(lm(articles[start_idx:end_idx] ~ years[start_idx:end_idx]))[2]
      } else NA
    )
    start_idx <- end_idx + 1
  }
  
  # Last segment
  segments[[length(break_indices) + 1]] <- list(
    start_year = years[start_idx],
    end_year = years[n],
    mean = mean(articles[start_idx:n]),
    slope = if (n - start_idx > 0) {
      coef(lm(articles[start_idx:n] ~ years[start_idx:n]))[2]
    } else NA
  )
  
  list(
    status = "success",
    n_breaks = length(breaks_found),
    break_dates = break_points,
    break_indices = break_indices,
    segments = segments,
    F_statistic = F_stat,
    p_value = p_value
  )
}

#' CUSUM test for structural change
#'
#' @param years Time points
#' @param articles Values
#' @return List with CUSUM test results
#' @export
compute_cusum_test <- function(years, articles) {
  n <- length(years)
  
  if (n < 10) {
    return(list(status = "error: insufficient data"))
  }
  
  # Fit OLS
  fit <- lm(articles ~ years)
  residuals <- resid(fit)
  
  # Standardized recursive residuals
  sigma <- sd(residuals)
  rec_resid <- residuals / sigma
  
  # CUSUM
  cusum <- cumsum(rec_resid)
  
  # CUSUM of squares
  cusum_sq <- cumsum(rec_resid^2) / sum(rec_resid^2)
  
  # Critical values (5%)
  cusum_critical <- 0.948 * sqrt(n) + 0.305
  cusum_lower <- -cusum_critical
  cusum_upper <- cusum_critical
  
  # CUSUMSQ critical values
  cusum_sq_lower <- 0.146 * (n - 2)
  cusum_sq_upper <- 1 - 0.146 * (n - 2)
  
  # Test statistics
  cusum_max <- max(abs(cusum))
  cusum_sq_max <- max(abs(cusum_sq - (1:n) / n))
  
  # P-values (approximate)
  cusum_pvalue <- 2 * (1 - pnorm(cusum_max / sqrt(n)))
  cusum_sq_pvalue <- 2 * (1 - pnorm(cusum_sq_max * sqrt(n)))
  
  # Significant breaks
  break_points <- years[abs(cusum) > cusum_critical]
  
  list(
    status = "success",
    cusum = cusum,
    cusum_sq = cusum_sq,
    cusum_critical = cusum_critical,
    cusum_sq_critical_lower = cusum_sq_lower,
    cusum_sq_critical_upper = cusum_sq_upper,
    max_cusum = cusum_max,
    max_cusum_sq = cusum_sq_max,
    cusum_pvalue = cusum_pvalue,
    cusum_sq_pvalue = cusum_sq_pvalue,
    significant_cusum = cusum_max > cusum_critical,
    significant_cusum_sq = cusum_sq_max > cusum_sq_critical_upper,
    break_points = break_points,
    n_breaks = length(break_points)
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b