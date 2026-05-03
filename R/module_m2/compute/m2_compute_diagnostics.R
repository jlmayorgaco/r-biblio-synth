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
compute_m2_diagnostics <- function(input,
                                   models,
                                   config = biblio_config(),
                                   changepoint_result = NULL,
                                   forecasting_result = NULL) {
  year_col <- if ("Year" %in% names(input)) "Year" else names(input)[1]
  articles_col <- if ("Articles" %in% names(input)) "Articles" else names(input)[2]
  
  years <- m2_num_vec(input[[year_col]])
  articles <- m2_num_vec(input[[articles_col]])

  annual_models <- if (is.list(models) && "annual" %in% names(models)) models$annual else models
  cumulative_models <- if (is.list(models) && "cumulative" %in% names(models)) models$cumulative else list()

  diagnostics <- list(
    series = data.frame(Year = years, Articles = articles),
    comparison = m2_build_registry_comparison(annual_models, config),
    growth_comparison = m2_build_registry_comparison(cumulative_models, config),
    cv_results = if (!is.null(forecasting_result$cv_results)) forecasting_result$cv_results else list(),
    accuracy = m2_build_accuracy_table(annual_models),
    forecast_comparison = if (!is.null(forecasting_result$model_comparison$comparison)) {
      forecasting_result$model_comparison$comparison
    } else {
      data.frame()
    },
    residuals = m2_collect_card_residuals(annual_models),
    trend_statistics = m2_compute_trend_statistics(years, articles, changepoint_result),
    changepoint_profile = m2_summarize_changepoint_profile(changepoint_result)
  )

  diagnostics$weights <- m2_compute_card_weights(diagnostics$comparison)
  diagnostics$ensemble <- m2_ensemble_from_cards(annual_models, diagnostics$weights)
  diagnostics$best_model <- m2_select_diagnostics_best_model(diagnostics$comparison)
  diagnostics$status <- "success"
  diagnostics
}

#' Comparison table builder for canonical registries
#' @keywords internal
m2_build_registry_comparison <- function(cards, config = biblio_config()) {
  if (!is.list(cards) || length(cards) == 0) {
    return(data.frame())
  }

  rows <- lapply(cards, m2_model_card_to_row)
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame())
  }

  comparison <- do.call(rbind, rows)
  m2_score_model_table(comparison, weights = config[["m2_selection_weights"]])
}

#' Accuracy table for canonical cards
#' @keywords internal
m2_build_accuracy_table <- function(cards) {
  if (!is.list(cards) || length(cards) == 0) {
    return(data.frame())
  }

  do.call(rbind, lapply(cards, function(card) {
    data.frame(
      model = card$name,
      family = card$family,
      source = card$source,
      RMSE = round(card$metrics$rmse, 4),
      MAE = round(card$metrics$mae, 4),
      MAPE = round(card$metrics$mape, 2),
      SMAPE = round(card$metrics$smape, 2),
      TheilU = round(card$metrics$theil_u, 4),
      Stability = round(card$diagnostics$stability_score, 4),
      stringsAsFactors = FALSE
    )
  }))
}

#' Residual payload collector
#' @keywords internal
m2_collect_card_residuals <- function(cards) {
  if (!is.list(cards) || length(cards) == 0) {
    return(list())
  }

  payloads <- lapply(cards, m2_card_residual_payload)
  payloads[!vapply(payloads, is.null, logical(1))]
}

#' Weight computation for canonical comparison tables
#' @keywords internal
m2_compute_card_weights <- function(comparison) {
  if (!is.data.frame(comparison) || nrow(comparison) == 0) {
    return(list())
  }

  weights <- list()

  if ("AIC" %in% names(comparison) && any(is.finite(comparison$AIC))) {
    delta_aic <- comparison$AIC - min(comparison$AIC, na.rm = TRUE)
    w_aic <- exp(-0.5 * delta_aic)
    weights$aic <- setNames(w_aic / sum(w_aic, na.rm = TRUE), comparison$Model)
  }

  if ("CompositeScore" %in% names(comparison) && any(is.finite(comparison$CompositeScore))) {
    comp <- pmax(comparison$CompositeScore, 0)
    if (sum(comp, na.rm = TRUE) > 0) {
      weights$composite <- setNames(comp / sum(comp, na.rm = TRUE), comparison$Model)
    }
  }

  weights
}

#' Simple forecast ensemble from annual cards
#' @keywords internal
m2_ensemble_from_cards <- function(cards, weights) {
  if (!is.list(cards) || length(cards) == 0) {
    return(list())
  }

  selected_weights <- if (!is.null(weights$composite)) weights$composite else if (!is.null(weights$aic)) weights$aic else NULL
  if (is.null(selected_weights)) {
    return(list())
  }

  forecast_models <- Filter(function(card) {
    is.list(card) && length(card$forecast$point) > 0 && identical(card$target, "annual")
  }, cards)
  if (length(forecast_models) == 0) {
    return(list())
  }

  horizon <- min(vapply(forecast_models, function(card) length(card$forecast$point), integer(1)))
  if (horizon == 0) {
    return(list())
  }

  model_names <- intersect(names(forecast_models), names(selected_weights))
  if (length(model_names) == 0) {
    return(list())
  }

  norm_weights <- selected_weights[model_names]
  norm_weights <- norm_weights / sum(norm_weights, na.rm = TRUE)
  years <- forecast_models[[model_names[1]]]$forecast$years[seq_len(horizon)]
  point <- rep(0, horizon)

  for (nm in model_names) {
    point <- point + norm_weights[[nm]] * forecast_models[[nm]]$forecast$point[seq_len(horizon)]
  }

  list(
    years = years,
    point = point,
    weights = norm_weights,
    method = if (!is.null(weights$composite)) "composite" else "aic"
  )
}

#' Pick best diagnostics model from comparison table
#' @keywords internal
m2_select_diagnostics_best_model <- function(comparison) {
  if (!is.data.frame(comparison) || nrow(comparison) == 0) {
    return(list(best_by_composite = NA_character_))
  }

  best_row <- comparison[order(comparison$Rank, -comparison$CompositeScore, comparison$RMSE), , drop = FALSE][1, , drop = FALSE]
  list(
    best_by_composite = as.character(best_row$Model[1]),
    composite_score = m2_scalar_num(best_row$CompositeScore[1]),
    rank = m2_scalar_num(best_row$Rank[1]),
    source = as.character(best_row$Source[1]),
    family = as.character(best_row$Family[1])
  )
}

#' Trend statistics for M2
#' @keywords internal
m2_compute_trend_statistics <- function(years, articles, changepoint_result = NULL) {
  years <- m2_num_vec(years)
  articles <- m2_num_vec(articles)
  keep <- is.finite(years) & is.finite(articles)
  years <- years[keep]
  articles <- articles[keep]
  n <- length(articles)

  if (n < 3) {
    return(list(status = "insufficient_data"))
  }

  linear_fit <- stats::lm(articles ~ years)
  slope <- m2_scalar_num(stats::coef(linear_fit)[2])
  intercept <- m2_scalar_num(stats::coef(linear_fit)[1])
  acceleration <- mean(diff(diff(articles)), na.rm = TRUE)
  growth_rates <- diff(articles) / pmax(abs(articles[-n]), .Machine$double.eps)
  cagr <- if (n >= 2 && articles[1] > 0) {
    (articles[n] / articles[1])^(1 / (n - 1)) - 1
  } else {
    NA_real_
  }
  recent_window <- min(5L, n)
  recent_cagr <- if (recent_window >= 2 && articles[n - recent_window + 1] > 0) {
    (articles[n] / articles[n - recent_window + 1])^(1 / (recent_window - 1)) - 1
  } else {
    NA_real_
  }

  mk <- m2_mann_kendall_test(articles)
  sen <- m2_sen_slope(years, articles)
  hurst <- m2_hurst_exponent(articles)

  list(
    slope = slope,
    intercept = intercept,
    r_squared = summary(linear_fit)$r.squared,
    cagr = cagr,
    recent_cagr = recent_cagr,
    mean_growth_rate = mean(growth_rates, na.rm = TRUE),
    median_growth_rate = stats::median(growth_rates, na.rm = TRUE),
    acceleration = acceleration,
    volatility = stats::sd(growth_rates, na.rm = TRUE),
    coefficient_of_variation = stats::sd(articles, na.rm = TRUE) / pmax(mean(articles, na.rm = TRUE), .Machine$double.eps),
    mann_kendall = mk,
    sen_slope = sen,
    hurst_exponent = hurst,
    breakpoint_years = if (!is.null(changepoint_result$summary$changepoint_years)) changepoint_result$summary$changepoint_years else numeric(0),
    n_breakpoints = if (!is.null(changepoint_result$summary$n_changepoints)) changepoint_result$summary$n_changepoints else 0L,
    status = "success"
  )
}

#' Summarize changepoint profile
#' @keywords internal
m2_summarize_changepoint_profile <- function(changepoint_result) {
  if (is.null(changepoint_result) || !identical(changepoint_result$status, "success")) {
    return(list())
  }

  list(
    years = changepoint_result$summary$changepoint_years,
    n_changepoints = changepoint_result$summary$n_changepoints,
    agreement_rate = changepoint_result$summary$agreement_rate,
    n_segments = changepoint_result$summary$n_segments,
    segments = changepoint_result$segments
  )
}

#' Mann-Kendall trend test
#' @keywords internal
m2_mann_kendall_test <- function(x) {
  x <- m2_num_vec(x)
  n <- length(x)
  if (n < 3) {
    return(list(statistic = NA_real_, z = NA_real_, p_value = NA_real_))
  }

  s <- 0
  for (i in seq_len(n - 1)) {
    s <- s + sum(sign(x[(i + 1):n] - x[i]), na.rm = TRUE)
  }

  tie_lengths <- table(x)
  var_s <- n * (n - 1) * (2 * n + 5)
  if (length(tie_lengths) > 0) {
    var_s <- var_s - sum(tie_lengths * (tie_lengths - 1) * (2 * tie_lengths + 5))
  }
  var_s <- var_s / 18

  z <- if (s > 0) {
    (s - 1) / sqrt(var_s)
  } else if (s < 0) {
    (s + 1) / sqrt(var_s)
  } else {
    0
  }

  list(
    statistic = s,
    z = z,
    p_value = 2 * (1 - stats::pnorm(abs(z)))
  )
}

#' Sen slope estimator
#' @keywords internal
m2_sen_slope <- function(years, values) {
  years <- m2_num_vec(years)
  values <- m2_num_vec(values)
  n <- length(values)
  if (n < 2) {
    return(NA_real_)
  }

  slopes <- numeric(0)
  for (i in seq_len(n - 1)) {
    denom <- years[(i + 1):n] - years[i]
    valid <- abs(denom) > .Machine$double.eps
    slopes <- c(slopes, (values[(i + 1):n][valid] - values[i]) / denom[valid])
  }
  stats::median(slopes, na.rm = TRUE)
}

#' Approximate Hurst exponent via rescaled range
#' @keywords internal
m2_hurst_exponent <- function(x) {
  x <- m2_num_vec(x)
  n <- length(x)
  if (n < 8) {
    return(NA_real_)
  }

  centered <- x - mean(x, na.rm = TRUE)
  cumulative <- cumsum(centered)
  r <- max(cumulative, na.rm = TRUE) - min(cumulative, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(r) || !is.finite(s) || s <= .Machine$double.eps) {
    return(NA_real_)
  }
  log(r / s) / log(n)
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
  if (n < 6) {
    return(data.frame(
      model = character(0),
      ME = numeric(0),
      RMSE = numeric(0),
      MAE = numeric(0),
      MPE = numeric(0),
      MAPE = numeric(0),
      MASE = numeric(0),
      TheilU = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  holdout <- max(5, floor(n * 0.2))  # 20% holdout
  if (holdout >= n) {
    holdout <- max(1, n - 2)
  }
  train_n <- n - holdout
  if (train_n < 2) {
    return(data.frame(
      model = character(0),
      ME = numeric(0),
      RMSE = numeric(0),
      MAE = numeric(0),
      MPE = numeric(0),
      MAPE = numeric(0),
      MASE = numeric(0),
      TheilU = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
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
  
  actual <- articles[seq.int(train_n + 1, n)]
  
  for (name in names(models)) {
    model <- models[[name]]
    
    if (is.null(model) || !is.list(model)) next
    
    fc <- get_forecast_from_model(model, years[seq_len(train_n)], years[seq.int(train_n + 1, n)])
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
    valid_model_names <- names(Filter(function(m) !is.null(m) && is.list(m), models))
    n_models <- length(valid_model_names)
    if (n_models == 0) {
      return(list())
    }
    equal_weight <- 1 / n_models
    weights <- list(equal = setNames(rep(equal_weight, n_models), valid_model_names))
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
    n_fc <- max(vapply(forecasts, function(f) length(f$point), integer(1)))
    
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
