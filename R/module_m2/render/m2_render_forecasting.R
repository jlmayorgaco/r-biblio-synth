# ============================================================================
# m2_render_forecasting.R - Render Time Series Forecasting Plots
# ============================================================================
# Creates visualization for forecasting results:
# - Forecasts with prediction intervals
# - Model comparison
# - Cross-validation results

#' Render forecasting plots
#'
#' @param data Output from compute_m2_forecasting
#' @param config Configuration list
#' @return List with plots
#' @export
render_m2_forecasting <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(list(plots = list(), status = "error: invalid data"))
  }
  
  plots <- list()
  
  # Main forecast plot
  plots$forecast <- create_forecast_plot(data, config)
  
  # Model comparison plot
  if (!is.null(data$model_comparison)) {
    plots$model_comparison <- create_model_comparison_plot(data, config)
  }
  
  # Cross-validation results
  if (!is.null(data$cv_results) && data$cv_results$status == "success") {
    plots$cv_errors <- create_cv_error_plot(data, config)
  }
  
  # Prediction intervals
  if (!is.null(data$prediction_intervals)) {
    plots$prediction_intervals <- create_prediction_interval_plot(data, config)
  }
  
  # Ensemble weights
  if (!is.null(data$ensemble) && !is.null(data$ensemble$weights)) {
    plots$ensemble_weights <- create_ensemble_weights_plot(data, config)
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create main forecast plot
#' @keywords internal
create_forecast_plot <- function(data, config) {
  years <- data$years
  articles <- data$articles
  horizon <- data$horizon
  last_year <- max(years)
  forecast_years <- (last_year + 1):(last_year + horizon)
  
  # Build data frame
  df_obs <- data.frame(
    Year = years,
    Articles = articles,
    Type = "Observed"
  )
  
  # ARIMA forecast
  arima_fc <- data$arima$fitted
  df_arima <- data.frame(
    Year = years,
    Articles = arima_fc,
    Type = "ARIMA Fitted"
  )
  
  # ETS forecast
  ets_fc <- data$ets$fitted
  df_ets <- data.frame(
    Year = years,
    Articles = ets_fc,
    Type = "ETS Fitted"
  )
  
  # Forecast values
  arima_forecast <- generate_forecast_values(data$arima, horizon)
  ets_forecast <- generate_forecast_values(data$ets, horizon)
  ensemble_forecast <- data$ensemble$forecast
  
  df_fc_arima <- data.frame(
    Year = forecast_years,
    Articles = arima_forecast,
    Type = "ARIMA Forecast"
  )
  
  df_fc_ets <- data.frame(
    Year = forecast_years,
    Articles = ets_forecast,
    Type = "ETS Forecast"
  )
  
  df_fc_ensemble <- data.frame(
    Year = forecast_years,
    Articles = ensemble_forecast,
    Type = "Ensemble Forecast"
  )
  
  # Combine
  df_all <- rbind(df_obs, df_arima, df_ets)
  
  # Plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = df_obs, 
                       ggplot2::aes(x = Year, y = Articles, color = Type),
                       linewidth = 0.8) +
    ggplot2::geom_point(data = df_obs,
                        ggplot2::aes(x = Year, y = Articles),
                        color = "#0072BD", size = 1.5) +
    ggplot2::geom_line(data = df_arima,
                       ggplot2::aes(x = Year, y = Articles, color = Type),
                       linetype = "dashed", linewidth = 0.6) +
    ggplot2::geom_line(data = df_fc_arima,
                       ggplot2::aes(x = Year, y = Articles, color = Type),
                       linetype = "dotted", linewidth = 0.8) +
    ggplot2::geom_line(data = df_fc_ensemble,
                       ggplot2::aes(x = Year, y = Articles),
                       color = "#D95319", linewidth = 1) +
    ggplot2::scale_color_manual(
      values = c("Observed" = "#0072BD", 
                 "ARIMA Fitted" = "#77AC30",
                 "ETS Fitted" = "#A2142F",
                 "ARIMA Forecast" = "#77AC30",
                 "ETS Forecast" = "#A2142F"),
      name = "Series"
    ) +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_y_continuous(name = "Articles") +
    ieee_theme_timeseries() +
    ggplot2::labs(
      title = "Time Series Forecast",
      subtitle = sprintf("ARIMA(%s) vs ETS vs Ensemble", 
                        paste(data$arima$order, collapse = ","))
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  p
}

#' Create model comparison plot
#' @keywords internal
create_model_comparison_plot <- function(data, config) {
  comparison <- data$model_comparison$comparison
  
  if (is.null(comparison) || nrow(comparison) == 0) return(NULL)
  
  # Reshape for plotting
  metrics <- c("AIC", "BIC")
  if ("CV_MAE" %in% names(comparison)) metrics <- c(metrics, "CV_MAE", "CV_RMSE")
  
  df_list <- list()
  for (metric in metrics) {
    if (metric %in% names(comparison)) {
      df_list[[length(df_list) + 1]] <- data.frame(
        Model = comparison$model,
        Metric = metric,
        Value = comparison[[metric]]
      )
    }
  }
  
  df <- do.call(rbind, df_list)
  df$Metric <- factor(df$Metric, levels = unique(df$Metric))
  
  # Normalize values within each metric for visualization
  df$ValueNormalized <- ave(df$Value, df$Metric, FUN = function(x) (x - min(x)) / (max(x) - min(x) + 0.001))
  
  colors <- c("AIC" = "#0072BD", "BIC" = "#D95319", 
              "CV_MAE" = "#77AC30", "CV_RMSE" = "#A2142F")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Model, y = Value, fill = Metric)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8),
                       width = 0.6, color = "black", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_y_continuous(name = "Value") +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Model Comparison",
      subtitle = sprintf("Best model: %s", data$model_comparison$best_model)
    ) +
    ggplot2::theme(legend.position = "right")
  
  p
}

#' Create cross-validation error plot
#' @keywords internal
create_cv_error_plot <- function(data, config) {
  cv_scores <- data$cv_results$cv_scores
  
  if (is.null(cv_scores) || nrow(cv_scores) == 0) return(NULL)
  
  # Aggregate by fold and model
  p <- ggplot2::ggplot(cv_scores, ggplot2::aes(x = fold, y = mae, color = model)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_color_manual(
      values = c("ARIMA" = "#0072BD", "ETS" = "#D95319", "Naive" = "#77AC30"),
      name = "Model"
    ) +
    ggplot2::scale_x_continuous(name = "CV Fold", breaks = unique(cv_scores$fold)) +
    ggplot2::scale_y_continuous(name = "Mean Absolute Error") +
    ieee_theme() +
    ggplot2::labs(
      title = "Cross-Validation Performance",
      subtitle = sprintf("Best model by MAE: %s", data$cv_results$best_model)
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  p
}

#' Create prediction interval plot
#' @keywords internal
create_prediction_interval_plot <- function(data, config) {
  pi <- data$prediction_intervals
  years <- data$years
  horizon <- data$horizon
  last_year <- max(years)
  forecast_years <- (last_year + 1):(last_year + horizon)
  articles <- data$articles
  
  # Observed data
  df_obs <- data.frame(
    Year = years,
    Articles = articles,
    Type = "Observed"
  )
  
  # Prediction intervals
  last_val <- tail(articles, 1)
  
  df_pi <- data.frame(
    Year = rep(forecast_years, 2),
    Bound = rep(c("Lower 95%", "Upper 95%"), each = horizon),
    Value = c(pi$arima$lower_95, pi$arima$upper_95),
    Model = "ARIMA"
  )
  
  # Ensemble forecast
  ensemble_fc <- data$ensemble$forecast
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = df_obs,
                       ggplot2::aes(x = Year, y = Articles),
                       color = "#0072BD", linewidth = 0.8) +
    ggplot2::geom_point(data = df_obs,
                        ggplot2::aes(x = Year, y = Articles),
                        color = "#0072BD", size = 1.5) +
    ggplot2::geom_ribbon(
      data = data.frame(
        Year = forecast_years,
        ymin = pi$arima$lower_95,
        ymax = pi$arima$upper_95
      ),
      ggplot2::aes(x = Year, ymin = ymin, ymax = ymax),
      fill = "#0072BD", alpha = 0.2
    ) +
    ggplot2::geom_line(data = data.frame(Year = forecast_years, Forecast = ensemble_fc),
                       ggplot2::aes(x = Year, y = Forecast),
                       color = "#D95319", linewidth = 1) +
    ggplot2::geom_point(data = data.frame(Year = forecast_years, Forecast = ensemble_fc),
                        ggplot2::aes(x = Year, y = Forecast),
                        color = "#D95319", size = 2) +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_y_continuous(name = "Articles") +
    ieee_theme_timeseries() +
    ggplot2::labs(
      title = "Forecast with 95% Prediction Intervals",
      subtitle = sprintf("%d-step ahead forecast", horizon)
    )
  
  p
}

#' Create ensemble weights plot
#' @keywords internal
create_ensemble_weights_plot <- function(data, config) {
  weights <- data$ensemble$weights
  models <- c("ARIMA", "ETS", "Naive")
  
  df <- data.frame(
    Model = models,
    Weight = weights
  )
  
  df$Model <- factor(df$Model, levels = models)
  
  colors <- c("ARIMA" = "#0072BD", "ETS" = "#D95319", "Naive" = "#77AC30")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Model, y = Weight, fill = Model)) +
    ggplot2::geom_col(width = 0.6, color = "black", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_y_continuous(name = "Weight", expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Ensemble Weights",
      subtitle = "Based on AIC"
    ) +
    ggplot2::theme(legend.position = "none")
  
  p
}

#' Generate forecast values from model result
#' @keywords internal
generate_forecast_values <- function(model_result, horizon) {
  if (!is.null(model_result$model)) {
    tryCatch({
      fc <- forecast::forecast(model_result$model, h = horizon)
      as.numeric(fc$mean)
    }, error = function(e) {
      rep(tail(model_result$fitted, 1), horizon)
    })
  } else {
    last_val <- tail(model_result$fitted, 1)
    trend <- if (length(model_result$fitted) > 1) {
      mean(diff(tail(model_result$fitted, min(5, length(model_result$fitted)))))
    } else 0
    last_val + trend * (1:horizon)
  }
}

`%||%` <- function(a, b) if (!is.null(a)) a else b