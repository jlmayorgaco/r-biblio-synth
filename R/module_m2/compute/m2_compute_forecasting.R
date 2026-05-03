# ============================================================================
# m2_compute_forecasting.R - Time Series Forecasting & Validation
# ============================================================================
# Provides ARIMA, ETS, and proper time series cross-validation

#' Compute time series forecasts with cross-validation
#'
#' @param input Time series data (Year, Articles)
#' @param config Configuration list
#' @return List with forecasts, cross-validation results, and prediction intervals
#' @export
compute_m2_forecasting <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) < 8) {
    return(list(
      status = "error: insufficient data for forecasting",
      forecasts = list(),
      cv_results = list()
    ))
  }
  
  year_col <- if ("Year" %in% names(input)) "Year" else names(input)[1]
  articles_col <- if ("Articles" %in% names(input)) "Articles" else names(input)[2]
  
  years <- input[[year_col]]
  articles <- input[[articles_col]]
  
  n <- length(articles)
  horizon <- config$forecast_horizon %||% 5
  
  # 1. Time series cross-validation
  cv_results <- time_series_cross_validation(years, articles, config)
  
  # 2. ARIMA model
  arima_result <- fit_arima_model(years, articles)
  
  # 3. ETS model
  ets_result <- fit_ets_model(years, articles)
  
  # 4. Naive forecasts (baseline)
  naive_result <- fit_naive_forecast(years, articles)
  
  # 5. Ensemble forecast
  ensemble_result <- create_ensemble_forecast(arima_result, ets_result, naive_result, years, horizon)
  
  # 6. Prediction intervals
  prediction_intervals <- compute_prediction_intervals(articles, arima_result, ets_result, horizon)
  
  # 7. Model comparison
  model_comparison <- compare_forecast_models(arima_result, ets_result, naive_result, cv_results)
  
  # 8. Forecast summary
  forecast_summary <- summarize_forecasts(arima_result, ets_result, ensemble_result, years, horizon)
  
  list(
    status = "success",
    arima = arima_result,
    ets = ets_result,
    naive = naive_result,
    ensemble = ensemble_result,
    cv_results = cv_results,
    prediction_intervals = prediction_intervals,
    model_comparison = model_comparison,
    forecast_summary = forecast_summary,
    horizon = horizon,
    years = years,
    articles = articles
  )
}

# ============================================================================
# Time Series Cross-Validation
# ============================================================================

#' Perform time series cross-validation
#' @keywords internal
time_series_cross_validation <- function(years, articles, config) {
  n <- length(articles)
  min_train <- config$min_train_size %||% max(5, n %/% 3)
  
  if (n < min_train + 2) {
    return(list(
      status = "insufficient_data",
      cv_scores = data.frame(),
      mean_mae = NA,
      mean_rmse = NA
    ))
  }
  
  # Rolling window cross-validation
  cv_scores <- data.frame(
    fold = integer(),
    train_size = integer(),
    model = character(),
    mae = numeric(),
    rmse = numeric(),
    mape = numeric(),
    stringsAsFactors = FALSE
  )
  
  n_folds <- min(5, n - min_train)
  
  for (i in 1:n_folds) {
    train_end <- min_train + i - 1
    test_end <- train_end + 1
    
    if (test_end > n) break
    
    train_years <- years[1:train_end]
    train_articles <- articles[1:train_end]
    test_year <- years[test_end]
    test_article <- articles[test_end]
    
    # ARIMA forecast
    arima_fc <- tryCatch({
      fit <- forecast::Arima(train_articles, order = c(1, 1, 0))
      fc <- forecast::forecast(fit, h = 1)
      as.numeric(fc$mean[1])
    }, error = function(e) NA)
    
    # ETS forecast
    ets_fc <- tryCatch({
      fit <- forecast::ets(train_articles)
      fc <- forecast::forecast(fit, h = 1)
      as.numeric(fc$mean[1])
    }, error = function(e) NA)
    
    # Naive forecast
    naive_fc <- train_articles[train_end]
    
    # Calculate errors
    for (model in c("ARIMA", "ETS", "Naive")) {
      fc_value <- switch(model,
                         "ARIMA" = arima_fc,
                         "ETS" = ets_fc,
                         "Naive" = naive_fc)
      
      if (is.na(fc_value)) next
      
      error <- test_article - fc_value
      
      cv_scores <- rbind(cv_scores, data.frame(
        fold = i,
        train_size = train_end,
        model = model,
        mae = abs(error),
        rmse = error^2,
        mape = if (test_article != 0) abs(error / test_article) * 100 else NA
      ))
    }
  }
  
  # Aggregate
  if (nrow(cv_scores) > 0) {
    cv_scores$rmse <- sqrt(cv_scores$rmse)
    
    agg <- aggregate(cbind(mae, rmse, mape) ~ model, data = cv_scores, FUN = mean)
    
    list(
      status = "success",
      cv_scores = cv_scores,
      cv_aggregated = agg,
      mean_mae = mean(cv_scores$mae, na.rm = TRUE),
      mean_rmse = mean(cv_scores$rmse, na.rm = TRUE),
      best_model = agg$model[which.min(agg$mae)]
    )
  } else {
    list(
      status = "no_cv_folds",
      cv_scores = data.frame(),
      mean_mae = NA,
      mean_rmse = NA
    )
  }
}

# ============================================================================
# ARIMA Model
# ============================================================================

#' Fit ARIMA model
#' @keywords internal
fit_arima_model <- function(years, articles) {
  n <- length(articles)
  
  # Fit using forecast package if available
  fit_result <- tryCatch({
    if (requireNamespace("forecast", quietly = TRUE)) {
      # Use auto.arima
      fit <- forecast::auto.arima(
        articles,
        max.p = 3,
        max.q = 3,
        max.d = 2,
        seasonal = FALSE,
        stepwise = FALSE,
        approximation = FALSE,
        trace = FALSE
      )
      
      order <- fit$arma[c(1, 6, 2)]
      coef <- coef(fit)
      fitted_values <- as.numeric(fitted(fit))
      residuals_val <- as.numeric(residuals(fit))
      
      list(
        status = "success",
        order = order,
        coefficients = coef,
        fitted = fitted_values,
        residuals = residuals_val,
        AIC = AIC(fit),
        BIC = BIC(fit),
        logLik = as.numeric(logLik(fit)),
        sigma2 = fit$sigma2,
        model = fit
      )
    } else {
      # Fallback: simple AR(1)
      fit_simple_ar1(years, articles)
    }
  }, error = function(e) {
    fit_simple_ar1(years, articles)
  })
  
  fit_result
}

#' Simple AR(1) fallback
#' @keywords internal
fit_simple_ar1 <- function(years, articles) {
  n <- length(articles)
  
  # AR(1): y_t = phi * y_{t-1} + epsilon
  y <- articles[-1]
  y_lag <- articles[-n]
  
  phi <- sum(y * y_lag) / sum(y_lag^2)
  
  fitted_values <- c(articles[1], phi * y_lag)
  residuals_val <- articles - fitted_values
  
  list(
    status = "success",
    order = c(1, 0, 0),
    coefficients = c(ar1 = phi),
    fitted = fitted_values,
    residuals = residuals_val,
    AIC = n * log(var(residuals_val)) + 2 * 1,
    BIC = n * log(var(residuals_val)) + log(n) * 1,
    logLik = -n/2 * log(2 * pi) - n/2 * log(var(residuals_val)),
    sigma2 = var(residuals_val),
    model = NULL
  )
}

# ============================================================================
# ETS Model
# ============================================================================

#' Fit ETS (Exponential Smoothing) model
#' @keywords internal
fit_ets_model <- function(years, articles) {
  n <- length(articles)
  
  fit_result <- tryCatch({
    if (requireNamespace("forecast", quietly = TRUE)) {
      fit <- forecast::ets(articles, model = "ZZZ")
      
      list(
        status = "success",
        model_type = fit$method,
        alpha = fit$par["alpha"],
        beta = fit$par["beta"],
        gamma = fit$par["gamma"],
        fitted = as.numeric(fitted(fit)),
        residuals = as.numeric(residuals(fit)),
        AIC = AIC(fit),
        BIC = BIC(fit),
        logLik = as.numeric(logLik(fit)),
        sigma2 = fit$sigma2,
        model = fit
      )
    } else {
      # Fallback: Holt-Winters manually
      fit_holt_winters_simple(years, articles)
    }
  }, error = function(e) {
    fit_holt_winters_simple(years, articles)
  })
  
  fit_result
}

#' Simple Holt-Winters fallback
#' @keywords internal
fit_holt_winters_simple <- function(years, articles) {
  n <- length(articles)
  
  # Holt's linear method
  alpha <- 0.3
  beta <- 0.1
  
  level <- articles[1]
  trend <- articles[2] - articles[1]
  
  fitted_values <- numeric(n)
  fitted_values[1] <- articles[1]
  
  for (i in 2:n) {
    fitted_values[i] <- level + trend
    prev_level <- level
    level <- alpha * articles[i] + (1 - alpha) * (level + trend)
    trend <- beta * (level - prev_level) + (1 - beta) * trend
  }
  
  residuals_val <- articles - fitted_values
  
  list(
    status = "success",
    model_type = "Holt's Linear",
    alpha = alpha,
    beta = beta,
    gamma = NA,
    fitted = fitted_values,
    residuals = residuals_val,
    AIC = n * log(var(residuals_val)) + 2 * 2,
    BIC = n * log(var(residuals_val)) + log(n) * 2,
    logLik = -n/2 * log(2 * pi) - n/2 * log(var(residuals_val)),
    sigma2 = var(residuals_val),
    model = NULL
  )
}

# ============================================================================
# Naive Forecast
# ============================================================================

#' Fit naive forecast (random walk)
#' @keywords internal
fit_naive_forecast <- function(years, articles) {
  n <- length(articles)
  
  # Naive: y_{t+1} = y_t
  fitted_values <- c(articles[1], articles[-n])
  residuals_val <- articles - fitted_values
  
  list(
    status = "success",
    model_type = "Naive",
    fitted = fitted_values,
    residuals = residuals_val,
    AIC = n * log(var(residuals_val)) + 2 * 0,
    BIC = n * log(var(residuals_val)) + log(n) * 0,
    sigma2 = var(residuals_val)
  )
}

# ============================================================================
# Ensemble Forecast
# ============================================================================

#' Create ensemble forecast
#' @keywords internal
create_ensemble_forecast <- function(arima_result, ets_result, naive_result, years, horizon) {
  n <- length(years)
  
  # Generate forecasts
  arima_fc <- generate_forecast(arima_result, horizon)
  ets_fc <- generate_forecast(ets_result, horizon)
  
  # Naive forecast: last observed value
  last_arima_val <- tail(arima_result$fitted, 1)
  naive_fc_values <- rep(last_arima_val, horizon)
  
  # Weight by inverse of AIC
  aics <- c(
    arima_result$AIC,
    ets_result$AIC,
    naive_result$AIC
  )
  
  # Handle NA
  aics[is.na(aics)] <- max(aics, na.rm = TRUE) * 1.5
  
  weights <- exp(-0.5 * aics)
  weights <- weights / sum(weights)
  
  # Weighted average
  ensemble_fc <- weights[1] * arima_fc + weights[2] * ets_fc + weights[3] * naive_fc_values
  
  forecast_years <- (max(years) + 1):(max(years) + horizon)
  
  list(
    status = "success",
    forecast = ensemble_fc,
    years = forecast_years,
    weights = weights,
    arima_forecast = arima_fc,
    ets_forecast = ets_fc,
    naive_forecast = naive_fc_values
  )
}

#' Generate forecast from model
#' @keywords internal
generate_forecast <- function(model_result, horizon) {
  if (!is.null(model_result$model)) {
    tryCatch({
      fc <- forecast::forecast(model_result$model, h = horizon)
      as.numeric(fc$mean)
    }, error = function(e) {
      rep(tail(model_result$fitted, 1), horizon)
    })
  } else {
    # Simple extrapolation
    if (model_result$status == "success" && length(model_result$fitted) > 0) {
      last_val <- tail(model_result$fitted, 1)
      trend <- mean(diff(tail(model_result$fitted, min(5, length(model_result$fitted)))))
      last_val + trend * (1:horizon)
    } else {
      rep(NA, horizon)
    }
  }
}

# ============================================================================
# Prediction Intervals
# ============================================================================

#' Compute prediction intervals for forecasts
#'
#' Uses proper forecast package prediction intervals that account for
#' both parameter uncertainty and process variance.
#'
#' @keywords internal
compute_prediction_intervals <- function(articles, arima_result, ets_result, horizon) {
  n <- length(articles)

  if (is.null(articles) || length(articles) < 5) {
    return(list(
      arima = list(lower_80 = numeric(0), upper_80 = numeric(0),
                   lower_95 = numeric(0), upper_95 = numeric(0)),
      ets = list(lower_80 = numeric(0), upper_80 = numeric(0),
                 lower_95 = numeric(0), upper_95 = numeric(0))
    ))
  }

  arima_pi <- tryCatch({
    if (!is.null(arima_result$model) && requireNamespace("forecast", quietly = TRUE)) {
      fc <- forecast::forecast(arima_result$model, h = horizon, level = c(80, 95))
      list(
        lower_80 = as.numeric(fc$lower[, "80%"]),
        upper_80 = as.numeric(fc$upper[, "80%"]),
        lower_95 = as.numeric(fc$lower[, "95%"]),
        upper_95 = as.numeric(fc$upper[, "95%"]),
        method = "forecast_package"
      )
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (is.null(arima_pi)) {
    arima_sigma <- if (!is.null(arima_result$sigma2)) sqrt(arima_result$sigma2) else sd(arima_result$residuals, na.rm = TRUE)
    last_fitted <- arima_result$fitted[n]
    h_seq <- seq_len(horizon)
    lower_80_arima <- last_fitted - qnorm(0.90) * arima_sigma * sqrt(h_seq)
    upper_80_arima <- last_fitted + qnorm(0.90) * arima_sigma * sqrt(h_seq)
    lower_95_arima <- last_fitted - qnorm(0.975) * arima_sigma * sqrt(h_seq)
    upper_95_arima <- last_fitted + qnorm(0.975) * arima_sigma * sqrt(h_seq)

    arima_pi <- list(
      lower_80 = lower_80_arima,
      upper_80 = upper_80_arima,
      lower_95 = lower_95_arima,
      upper_95 = upper_95_arima,
      method = "naive_residual"
    )
  }

  ets_pi <- tryCatch({
    if (!is.null(ets_result$model) && requireNamespace("forecast", quietly = TRUE)) {
      fc <- forecast::forecast(ets_result$model, h = horizon, level = c(80, 95))
      list(
        lower_80 = as.numeric(fc$lower[, "80%"]),
        upper_80 = as.numeric(fc$upper[, "80%"]),
        lower_95 = as.numeric(fc$lower[, "95%"]),
        upper_95 = as.numeric(fc$upper[, "95%"]),
        method = "forecast_package"
      )
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (is.null(ets_pi)) {
    ets_sigma <- if (!is.null(ets_result$sigma2)) sqrt(ets_result$sigma2) else sd(ets_result$residuals, na.rm = TRUE)
    last_fitted <- ets_result$fitted[n]
    h_seq <- seq_len(horizon)
    lower_80_ets <- last_fitted - qnorm(0.90) * ets_sigma * sqrt(h_seq)
    upper_80_ets <- last_fitted + qnorm(0.90) * ets_sigma * sqrt(h_seq)
    lower_95_ets <- last_fitted - qnorm(0.975) * ets_sigma * sqrt(h_seq)
    upper_95_ets <- last_fitted + qnorm(0.975) * ets_sigma * sqrt(h_seq)

    ets_pi <- list(
      lower_80 = lower_80_ets,
      upper_80 = upper_80_ets,
      lower_95 = lower_95_ets,
      upper_95 = upper_95_ets,
      method = "naive_residual"
    )
  }

  list(
    arima = list(
      lower_80 = arima_pi$lower_80,
      upper_80 = arima_pi$upper_80,
      lower_95 = arima_pi$lower_95,
      upper_95 = arima_pi$upper_95
    ),
    ets = list(
      lower_80 = ets_pi$lower_80,
      upper_80 = ets_pi$upper_80,
      lower_95 = ets_pi$lower_95,
      upper_95 = ets_pi$upper_95
    )
  )
}

# ============================================================================
# Model Comparison
# ============================================================================

#' Compare forecast models
#' @keywords internal
compare_forecast_models <- function(arima_result, ets_result, naive_result, cv_results) {
  models <- c("ARIMA", "ETS", "Naive")
  
  comparison <- data.frame(
    model = models,
    AIC = c(arima_result$AIC, ets_result$AIC, naive_result$AIC),
    BIC = c(arima_result$BIC, ets_result$BIC, naive_result$BIC),
    stringsAsFactors = FALSE
  )
  
  if (!is.null(cv_results$cv_aggregated)) {
    cv_agg <- cv_results$cv_aggregated
    comparison$CV_MAE <- c(
      cv_agg$mae[cv_agg$model == "ARIMA"][1],
      cv_agg$mae[cv_agg$model == "ETS"][1],
      cv_agg$mae[cv_agg$model == "Naive"][1]
    )
    comparison$CV_RMSE <- c(
      cv_agg$rmse[cv_agg$model == "ARIMA"][1],
      cv_agg$rmse[cv_agg$model == "ETS"][1],
      cv_agg$rmse[cv_agg$model == "Naive"][1]
    )
    comparison$CV_MAPE <- c(
      cv_agg$mape[cv_agg$model == "ARIMA"][1],
      cv_agg$mape[cv_agg$model == "ETS"][1],
      cv_agg$mape[cv_agg$model == "Naive"][1]
    )
  }
  
  # Composite model ranking
  comparison$rank_AIC <- rank(comparison$AIC)
  comparison$rank_BIC <- rank(comparison$BIC)
  if ("CV_MAE" %in% names(comparison)) {
    comparison$rank_CV <- rank(comparison$CV_MAE)
  }

  comparison$AIC_score <- m2_normalize_metric(comparison$AIC, higher_is_better = FALSE)
  comparison$BIC_score <- m2_normalize_metric(comparison$BIC, higher_is_better = FALSE)
  if ("CV_MAE" %in% names(comparison)) {
    comparison$CV_MAE_score <- m2_normalize_metric(comparison$CV_MAE, higher_is_better = FALSE)
  }
  if ("CV_RMSE" %in% names(comparison)) {
    comparison$CV_RMSE_score <- m2_normalize_metric(comparison$CV_RMSE, higher_is_better = FALSE)
  }
  if ("CV_MAPE" %in% names(comparison)) {
    comparison$CV_MAPE_score <- m2_normalize_metric(comparison$CV_MAPE, higher_is_better = FALSE)
  }

  score_cols <- intersect(
    c("AIC_score", "BIC_score", "CV_MAE_score", "CV_RMSE_score", "CV_MAPE_score"),
    names(comparison)
  )
  comparison$CompositeScore <- rowMeans(comparison[, score_cols, drop = FALSE], na.rm = TRUE)
  comparison$CompositeRank <- rank(-comparison$CompositeScore, ties.method = "first")

  best_model <- comparison$model[which.max(comparison$CompositeScore)]
  
  list(
    comparison = comparison,
    best_model = best_model,
    recommendation = sprintf("Recommended model: %s (best composite score across information criteria and temporal CV)", best_model)
  )
}

# ============================================================================
# Forecast Summary
# ============================================================================

#' Summarize forecasts
#' @keywords internal
summarize_forecasts <- function(arima_result, ets_result, ensemble_result, years, horizon) {
  last_year <- max(years)
  forecast_years <- (last_year + 1):(last_year + horizon)
  
  # ARIMA forecast
  arima_fc <- generate_forecast(arima_result, horizon)
  
  # ETS forecast
  ets_fc <- generate_forecast(ets_result, horizon)
  
  # Ensemble forecast
  ensemble_fc <- ensemble_result$forecast
  
  list(
    years = forecast_years,
    arima_forecast = arima_fc,
    ets_forecast = ets_fc,
    ensemble_forecast = ensemble_fc,
    mean_forecast = (arima_fc + ets_fc) / 2,
    last_observed_year = last_year,
    last_observed_value = tail(arima_result$fitted, 1),
    growth_rate = if (horizon > 0 && !is.na(ensemble_fc[1])) {
      (ensemble_fc[horizon] - tail(arima_result$fitted, 1)) / tail(arima_result$fitted, 1)
    } else NA
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
