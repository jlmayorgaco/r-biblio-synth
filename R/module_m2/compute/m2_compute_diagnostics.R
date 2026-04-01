# ============================================================================
# m2_compute_diagnostics.R - Model Diagnostics and Evaluation
# ============================================================================
# AIC/BIC comparison, cross-validation, forecast evaluation, model averaging

#' Compute comprehensive model diagnostics
#'
#' @param input Data frame with Year and Articles
#' @param models List of fitted models
#' @param config Configuration list
#' @return List with diagnostics
#' @export
compute_m2_diagnostics <- function(input, models, config = biblio_config()) {
  year_col <- if ("Year" %in% names(input)) "Year" else names(input)[1]
  articles_col <- if ("Articles" %in% names(input)) "Articles" else names(input)[2]
  
  years <- input[[year_col]]
  articles <- input[[articles_col]]
  n <- length(articles)
  
  diagnostics <- list()
  
  # 1. AIC/BIC comparison
  diagnostics$comparison <- compare_models_aic_bic(models)
  
  # 2. Cross-validation
  diagnostics$cv_results <- cross_validate_models(years, articles, models)
  
  # 3. Forecast accuracy measures
  diagnostics$accuracy <- compute_forecast_accuracy(years, articles, models)
  
  # 4. Residual diagnostics
  diagnostics$residuals <- compute_residual_diagnostics(models)
  
  # 5. Model averaging weights
  diagnostics$weights <- compute_model_weights(diagnostics$comparison)
  
  # 6. Ensemble forecast
  diagnostics$ensemble <- ensemble_forecast(models, diagnostics$weights)
  
  # 7. Best model
  diagnostics$best_model <- select_best_model(diagnostics)
  
  diagnostics$status <- "success"
  diagnostics
}

#' Compare models by AIC and BIC
#' @keywords internal
compare_models_aic_bic <- function(models) {
  comparison <- data.frame(
    model = character(0),
    AIC = numeric(0),
    BIC = numeric(0),
    logLik = numeric(0),
    n_params = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for (name in names(models)) {
    model <- models[[name]]
    
    if (!is.null(model) && is.list(model)) {
      aic <- if (!is.null(model$AIC)) model$AIC else NA
      bic <- if (!is.null(model$BIC)) model$BIC else NA
      ll <- if (!is.null(model$logLik)) model$logLik else NA
      n_params <- if (!is.null(model$n_params)) model$n_params else NA
      
      if (!is.na(aic) || !is.na(bic)) {
        comparison <- rbind(comparison, data.frame(
          model = name,
          AIC = aic,
          BIC = bic,
          logLik = ll,
          n_params = n_params,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  if (nrow(comparison) > 0) {
    # Delta AIC/BIC
    comparison$delta_AIC <- comparison$AIC - min(comparison$AIC, na.rm = TRUE)
    comparison$delta_BIC <- comparison$BIC - min(comparison$BIC, na.rm = TRUE)
    
    # Akaike weights
    comparison$weight_AIC <- exp(-0.5 * comparison$delta_AIC)
    comparison$weight_AIC <- comparison$weight_AIC / sum(comparison$weight_AIC, na.rm = TRUE)
    
    comparison$weight_BIC <- exp(-0.5 * comparison$delta_BIC)
    comparison$weight_BIC <- comparison$weight_BIC / sum(comparison$weight_BIC, na.rm = TRUE)
    
    # Rank
    comparison$rank_AIC <- rank(comparison$AIC, na.last = "keep")
    comparison$rank_BIC <- rank(comparison$BIC, na.last = "keep")
  }
  
  comparison
}

#' Cross-validate models
#' @keywords internal
cross_validate_models <- function(years, articles, models, k = 5) {
  n <- length(articles)
  
  if (n < 10) {
    return(list(status = "error: insufficient data for cross-validation"))
  }
  
  # Time series CV (rolling window)
  folds <- create_time_series_folds(n, k)
  
  cv_results <- data.frame(
    model = character(0),
    fold = integer(0),
    MAE = numeric(0),
    RMSE = numeric(0),
    MAPE = numeric(0),
    MASE = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for (name in names(models)) {
    model <- models[[name]]
    
    if (is.null(model) || !is.list(model)) next
    
    for (i in seq_along(folds$train)) {
      train_idx <- folds$train[[i]]
      test_idx <- folds$test[[i]]
      
      # Get predictions for test set
      fc <- get_forecast_from_model(model, years[train_idx], years[test_idx])
      
      actual <- articles[test_idx]
      predicted <- fc$point
      
      if (length(predicted) == length(actual)) {
        mae <- mean(abs(actual - predicted), na.rm = TRUE)
        rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
        mape <- mean(abs((actual - predicted) / actual), na.rm = TRUE) * 100
        
        # MASE (Mean Absolute Scaled Error)
        naive_mae <- mean(abs(diff(articles[train_idx])), na.rm = TRUE)
        mase <- if (naive_mae > 0) mae / naive_mae else NA
        
        cv_results <- rbind(cv_results, data.frame(
          model = name,
          fold = i,
          MAE = mae,
          RMSE = rmse,
          MAPE = mape,
          MASE = mase
        ))
      }
    }
  }
  
  # Aggregate results
  if (nrow(cv_results) > 0) {
    aggregated <- aggregate(cbind(MAE, RMSE, MAPE, MASE) ~ model, 
                           data = cv_results, 
                           FUN = function(x) c(mean = mean(x), sd = sd(x)))
    
    # Flatten
    agg_flat <- data.frame(
      model = aggregated$model,
      MAE_mean = aggregated$MAE[, "mean"],
      MAE_sd = aggregated$MAE[, "sd"],
      RMSE_mean = aggregated$RMSE[, "mean"],
      RMSE_sd = aggregated$RMSE[, "sd"],
      MAPE_mean = aggregated$MAPE[, "mean"],
      MAPE_sd = aggregated$MAPE[, "sd"],
      MASE_mean = aggregated$MASE[, "mean"],
      MASE_sd = aggregated$MASE[, "sd"]
    )
  } else {
    agg_flat <- data.frame()
  }
  
  list(
    cv_results = cv_results,
    aggregated = agg_flat,
    n_folds = k,
    status = "success"
  )
}

#' Create time series folds
#' @keywords internal
create_time_series_folds <- function(n, k) {
  # Rolling origin CV
  min_train <- max(5, floor(n * 0.5))
  fold_size <- floor((n - min_train) / k)
  
  train_folds <- vector("list", k)
  test_folds <- vector("list", k)
  
  for (i in 1:k) {
    train_end <- min_train + (i - 1) * fold_size
    test_start <- train_end + 1
    test_end <- min(train_end + fold_size, n)
    
    train_folds[[i]] <- 1:train_end
    test_folds[[i]] <- test_start:test_end
  }
  
  list(train = train_folds, test = test_folds)
}

#' Get forecast from model
#' @keywords internal
get_forecast_from_model <- function(model, train_years, test_years) {
  n_test <- length(test_years)
  
  if (!is.null(model$forecast) && !is.null(model$forecast$point)) {
    # Model has forecast
    n_fc <- length(model$forecast$point)
    if (n_fc >= n_test) {
      point <- model$forecast$point[1:n_test]
      lower <- if (!is.null(model$forecast$lower)) model$forecast$lower[1:n_test] else rep(NA, n_test)
      upper <- if (!is.null(model$forecast$upper)) model$forecast$upper[1:n_test] else rep(NA, n_test)
    } else {
      point <- c(model$forecast$point, rep(NA, n_test - n_fc))
      lower <- c(model$forecast$lower[1:n_fc], rep(NA, n_test - n_fc))
      upper <- c(model$forecast$upper[1:n_fc], rep(NA, n_test - n_fc))
    }
  } else if (!is.null(model$fitted)) {
    # Extrapolate from fitted
    n_train <- length(train_years)
    last_val <- model$fitted[n_train]
    trend <- if (n_train > 1) (model$fitted[n_train] - model$fitted[n_train - 1]) else 0
    
    point <- last_val + trend * (1:n_test)
    lower <- point - 1.96 * sd(model$residuals, na.rm = TRUE)
    upper <- point + 1.96 * sd(model$residuals, na.rm = TRUE)
  } else {
    # Naive forecast
    point <- rep(NA, n_test)
    lower <- rep(NA, n_test)
    upper <- rep(NA, n_test)
  }
  
  list(point = point, lower = lower, upper = upper)
}

#' Compute forecast accuracy measures
#' @keywords internal
compute_forecast_accuracy <- function(years, articles, models) {
  n <- length(articles)
  holdout <- max(5, floor(n * 0.2))  # 20% holdout
  train_n <- n - holdout
  
  accuracy <- data.frame(
    model = character(0),
    ME = numeric(0),    # Mean Error
    RMSE = numeric(0),  # Root Mean Squared Error
    MAE = numeric(0),   # Mean Absolute Error
    MPE = numeric(0),  # Mean Percentage Error
    MAPE = numeric(0), # Mean Absolute Percentage Error
    MASE = numeric(0), # Mean Absolute Scaled Error
    TheilU = numeric(0), # Theil's U
    stringsAsFactors = FALSE
  )
  
  actual <- articles[(train_n + 1):n]
  
  for (name in names(models)) {
    model <- models[[name]]
    
    if (is.null(model) || !is.list(model)) next
    
    fc <- get_forecast_from_model(model, years[1:train_n], years[(train_n + 1):n])
    predicted <- fc$point
    
    if (length(predicted) != length(actual)) next
    
    errors <- actual - predicted
    
    # Mean Error
    me <- mean(errors, na.rm = TRUE)
    
    # RMSE
    rmse <- sqrt(mean(errors^2, na.rm = TRUE))
    
    # MAE
    mae <- mean(abs(errors), na.rm = TRUE)
    
    # MPE
    mpe <- mean(errors / actual, na.rm = TRUE) * 100
    
    # MAPE
    mape <- mean(abs(errors / actual), na.rm = TRUE) * 100
    
    # MASE
    naive_mae <- mean(abs(diff(articles[1:train_n])), na.rm = TRUE)
    mase <- if (naive_mae > 0) mae / naive_mae else NA
    
    # Theil's U (with division-by-zero protection)
    if (length(actual) < 2 || length(predicted) < 2) {
      theil_u <- NA
    } else {
      y <- actual[2:length(actual)]
      y_prev <- actual[1:(length(actual) - 1)]
      f <- predicted[2:length(predicted)]
      
      # Filter out zero/near-zero y_prev values
      valid_idx <- which(abs(y_prev) > .Machine$double.eps)
      if (length(valid_idx) < 2) {
        theil_u <- NA
      } else {
        y <- y[valid_idx]
        y_prev <- y_prev[valid_idx]
        f <- f[valid_idx]
        
        numerator <- sqrt(sum(((y - f) / y_prev)^2, na.rm = TRUE))
        denominator <- sqrt(sum(((y - y_prev) / y_prev)^2, na.rm = TRUE))
        theil_u <- if (is.finite(denominator) && denominator > 0) numerator / denominator else NA
      }
    }
    
    accuracy <- rbind(accuracy, data.frame(
      model = name,
      ME = me,
      RMSE = rmse,
      MAE = mae,
      MPE = mpe,
      MAPE = mape,
      MASE = mase,
      TheilU = theil_u
    ))
  }
  
  accuracy
}

#' Compute residual diagnostics
#' @keywords internal
compute_residual_diagnostics <- function(models) {
  diagnostics <- list()
  
  for (name in names(models)) {
    model <- models[[name]]
    
    if (is.null(model) || !is.list(model) || is.null(model$residuals)) next
    
    res <- model$residuals[!is.na(model$residuals)]
    
    if (length(res) < 5) next
    
    diag <- list()
    
    # Normality tests
    diag$shapiro_wilk <- tryCatch({
      test <- shapiro.test(res)
      list(W = test$statistic, p_value = test$p.value)
    }, error = function(e) list(W = NA, p_value = NA))
    
    diag$jarque_bera <- tryCatch({
      n <- length(res)
      m <- mean(res)
      # Population variance (n denominator, not n-1)
      s_pop <- sqrt(mean((res - m)^2))
      if (s_pop == 0 || !is.finite(s_pop) || n < 3) {
        return(list(statistic = NA, p_value = NA, skewness = NA, kurtosis = NA))
      }
      # Population skewness and excess kurtosis
      skewness <- mean((res - m)^3) / (s_pop^3)
      kurtosis <- mean((res - m)^4) / (s_pop^4)
      jb <- n * (skewness^2 / 6 + (kurtosis - 3)^2 / 24)
      p_value <- 1 - pchisq(jb, 2)
      list(statistic = jb, p_value = p_value, skewness = skewness, kurtosis = kurtosis)
    }, error = function(e) list(statistic = NA, p_value = NA))
    
    # ACF and PACF (autocorrelation)
    diag$acf <- tryCatch({
      acf_vals <- acf(res, plot = FALSE, lag.max = min(20, length(res) - 1))
      list(values = as.numeric(acf_vals$acf), lags = acf_vals$lag)
    }, error = function(e) list(values = NA, lags = NA))
    
    diag$pacf <- tryCatch({
      pacf_vals <- pacf(res, plot = FALSE, lag.max = min(15, length(res) - 2))
      list(values = as.numeric(pacf_vals$acf), lags = pacf_vals$lag)
    }, error = function(e) list(values = NA, lags = NA))
    
    # Ljung-Box test (autocorrelation)
    diag$ljung_box <- tryCatch({
      lb <- Box.test(res, lag = min(10, length(res) - 1), type = "Ljung-Box")
      list(statistic = lb$statistic, p_value = lb$p.value)
    }, error = function(e) list(statistic = NA, p_value = NA))
    
    # Durbin-Watson test
    diag$durbin_watson <- tryCatch({
      dw <- sum(diff(res)^2) / sum(res^2)
      list(statistic = dw)
    }, error = function(e) list(statistic = NA))
    
    # Heteroscedasticity (Breusch-Pagan)
    diag$breusch_pagan <- tryCatch({
      fitted <- model$fitted[!is.na(model$fitted)]
      if (length(fitted) != length(res)) {
        list(statistic = NA, p_value = NA)
      } else {
        res_sq <- res^2
        bp_fit <- lm(res_sq ~ fitted)
        n <- length(res)
        bp_stat <- n * summary(bp_fit)$r.squared
        p_value <- 1 - pchisq(bp_stat, 1)
        list(statistic = bp_stat, p_value = p_value)
      }
    }, error = function(e) list(statistic = NA, p_value = NA))
    
    # Summary statistics
    diag$mean <- mean(res)
    diag$sd <- sd(res)
    diag$skewness <- e1071::skewness(res)
    diag$kurtosis <- e1071::kurtosis(res)
    
    diagnostics[[name]] <- diag
  }
  
  diagnostics
}

#' Compute model weights for averaging
#' @keywords internal
compute_model_weights <- function(comparison) {
  if (is.null(comparison) || nrow(comparison) == 0) {
    return(list())
  }
  
  weights <- list()
  
  # Akaike weights (based on AIC)
  if (!is.null(comparison$weight_AIC)) {
    weights$aic <- setNames(comparison$weight_AIC, comparison$model)
  }
  
  # BIC weights
  if (!is.null(comparison$weight_BIC)) {
    weights$bic <- setNames(comparison$weight_BIC, comparison$model)
  }
  
  # Stacking weights (based on CV performance)
  # (Would require cv_results input)
  
  weights
}

#' Ensemble forecast
#' @keywords internal
ensemble_forecast <- function(models, weights) {
  if (is.null(weights) || length(weights) == 0) {
    # Equal weights
    model_names <- names(models)
    n_models <- sum(sapply(models, function(m) !is.null(m) && is.list(m)))
    equal_weight <- 1 / n_models
    weights <- list(equal = setNames(rep(equal_weight, n_models), model_names))
  }
  
  ensemble <- list()
  
  for (method in names(weights)) {
    w <- weights[[method]]
    
    # Get all forecasts
    forecasts <- list()
    for (name in names(w)) {
      if (!is.null(models[[name]]) && !is.null(models[[name]]$forecast)) {
        forecasts[[name]] <- models[[name]]$forecast
      }
    }
    
    if (length(forecasts) == 0) next
    
    # Align forecasts
    n_fc <- max(sapply(forecasts, function(f) length(f$point)))
    
    point_ensemble <- numeric(n_fc)
    lower_ensemble <- numeric(n_fc)
    upper_ensemble <- numeric(n_fc)
    
    for (i in 1:n_fc) {
      point_vals <- sapply(forecasts, function(f) {
        if (i <= length(f$point)) f$point[i] else NA
      })
      lower_vals <- sapply(forecasts, function(f) {
        if (i <= length(f$lower)) f$lower[i] else NA
      })
      upper_vals <- sapply(forecasts, function(f) {
        if (i <= length(f$upper)) f$upper[i] else NA
      })
      
      name_vals <- names(point_vals)
      w_subset <- w[name_vals]
      w_subset <- w_subset / sum(w_subset)
      
      point_ensemble[i] <- sum(point_vals * w_subset, na.rm = TRUE)
      lower_ensemble[i] <- min(lower_vals, na.rm = TRUE)
      upper_ensemble[i] <- max(upper_vals, na.rm = TRUE)
    }
    
    ensemble[[method]] <- list(
      point = point_ensemble,
      lower = lower_ensemble,
      upper = upper_ensemble,
      weights = w
    )
  }
  
  ensemble
}

#' Select best model based on criteria
#' @keywords internal
select_best_model <- function(diagnostics) {
  if (is.null(diagnostics$comparison) || nrow(diagnostics$comparison) == 0) {
    return(list(best_by_aic = NA, best_by_bic = NA, best_by_cv = NA))
  }
  
  comparison <- diagnostics$comparison
  
  best_aic <- comparison$model[which.min(comparison$AIC)]
  best_bic <- comparison$model[which.min(comparison$BIC)]
  
  # CV-based selection
  if (!is.null(diagnostics$cv_results$aggregated) && nrow(diagnostics$cv_results$aggregated) > 0) {
    best_cv <- diagnostics$cv_results$aggregated$model[which.min(diagnostics$cv_results$aggregated$MAE_mean)]
  } else {
    best_cv <- NA
  }
  
  list(
    best_by_aic = best_aic,
    best_by_bic = best_bic,
    best_by_cv = best_cv,
    recommendation = if (!is.na(best_aic)) best_aic else best_bic
  )
}