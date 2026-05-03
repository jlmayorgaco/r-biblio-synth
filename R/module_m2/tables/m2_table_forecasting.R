# ============================================================================
# m2_table_forecasting.R - Forecasting Tables
# ============================================================================
# Creates tables for time series forecasting results

#' Build forecasting tables
#'
#' @param data Output from compute_m2_forecasting
#' @param config Configuration list
#' @return List with tables
#' @keywords internal
build_m2_forecasting_tables_impl <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(list(tables = list(), status = "error: invalid data"))
  }
  
  tables <- list()
  
  # Model comparison table
  tables$model_comparison <- create_model_comparison_table(data)
  
  # Forecast values table
  tables$forecasts <- create_forecast_values_table(data)
  
  # CV results table
  if (!is.null(data$cv_results) && !is.null(data$cv_results$cv_aggregated)) {
    tables$cv_results <- create_cv_results_table(data)
  }
  
  # Ensemble weights table
  if (!is.null(data$ensemble) && !is.null(data$ensemble$weights)) {
    tables$ensemble_weights <- create_ensemble_weights_table(data)
  }
  
  list(
    tables = tables,
    status = "success"
  )
}

#' Create model comparison table
#' @keywords internal
create_model_comparison_table <- function(data) {
  comparison <- data$model_comparison$comparison
  
  if (is.null(comparison)) return(NULL)
  
  df <- comparison[, c("model", "AIC", "BIC")]
  
  if ("CV_MAE" %in% names(comparison)) {
    df$CV_MAE <- comparison$CV_MAE
  }
  
  if ("CV_RMSE" %in% names(comparison)) {
    df$CV_RMSE <- comparison$CV_RMSE
  }

  if ("CV_MAPE" %in% names(comparison)) {
    df$CV_MAPE <- comparison$CV_MAPE
  }
  
  if ("rank_AIC" %in% names(comparison)) {
    df$Rank_AIC <- comparison$rank_AIC
  }

  if ("CompositeScore" %in% names(comparison)) {
    df$CompositeScore <- comparison$CompositeScore
  }

  if ("CompositeRank" %in% names(comparison)) {
    df$CompositeRank <- comparison$CompositeRank
  }
  
  colnames(df) <- gsub("_", " ", colnames(df))
  
  df
}

#' Create forecast values table
#' @keywords internal
create_forecast_values_table <- function(data) {
  years <- data$years
  horizon <- data$horizon
  last_year <- max(years)
  forecast_years <- (last_year + 1):(last_year + horizon)
  
  arima_fc <- generate_forecast_values(data$arima, horizon)
  ets_fc <- generate_forecast_values(data$ets, horizon)
  ensemble_fc <- data$ensemble$forecast
  
  pi_lower <- data$prediction_intervals$arima$lower_95
  pi_upper <- data$prediction_intervals$arima$upper_95
  
  df <- data.frame(
    Year = forecast_years,
    ARIMA = round(arima_fc, 1),
    ETS = round(ets_fc, 1),
    Ensemble = round(ensemble_fc, 1),
    Lower_95 = round(pi_lower, 1),
    Upper_95 = round(pi_upper, 1)
  )
  
  df
}

#' Create CV results table
#' @keywords internal
create_cv_results_table <- function(data) {
  cv_agg <- data$cv_results$cv_aggregated
  
  if (is.null(cv_agg)) return(NULL)
  
  df <- cv_agg
  colnames(df) <- gsub("_", " ", colnames(df))
  
  df
}

#' Create ensemble weights table
#' @keywords internal
create_ensemble_weights_table <- function(data) {
  weights <- data$ensemble$weights
  models <- c("ARIMA", "ETS", "Naive")
  
  df <- data.frame(
    Model = models,
    Weight = round(weights, 4),
    AIC = c(data$arima$AIC, data$ets$AIC, data$naive$AIC)
  )
  
  df
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
