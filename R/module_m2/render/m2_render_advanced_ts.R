# ============================================================================
# m2_render_advanced_ts.R - Advanced Time Series Visualization
# ============================================================================

#' Render advanced time series analysis
#'
#' @param data Output from compute_m2_advanced_ts
#' @param config Configuration list
#' @return List with plots
#' @export
render_m2_advanced_ts <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(list(plots = list(), status = data$status %||% "error"))
  }
  
  plots <- list()
  
  if (!is.null(data$sarima)) {
    plots$sarima <- create_sarima_plot(data$sarima, config)
  }
  
  if (!is.null(data$tbats)) {
    plots$tbats <- create_tbats_plot(data$tbats, config)
  }
  
  if (!is.null(data$prophet)) {
    plots$prophet <- create_prophet_plot(data$prophet, config)
  }
  
  if (!is.null(data$state_space)) {
    plots$state_space <- create_state_space_plot(data$state_space, config)
  }
  
  if (!is.null(data$comparison)) {
    plots$model_comparison <- create_ts_model_comparison_plot(data$comparison, config)
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create SARIMA plot
#' @keywords internal
create_sarima_plot <- function(sarima_data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (is.null(sarima_data) || sarima_data$status != "success") return(NULL)
  
  years <- sarima_data$years
  fitted <- sarima_data$fitted
  forecast <- sarima_data$forecast
  
  df <- data.frame(
    year = c(years, years),
    value = c(fitted, forecast),
    type = c(rep("Fitted", length(years)), rep("Forecast", length(years)))
  )
  
  ggplot2::ggplot(df, ggplot2::aes(x = year, y = value, color = type)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(values = c("Fitted" = "#2166AC", "Forecast" = "#B2182B")) +
    ggplot2::labs(
      title = "SARIMA Model",
      x = "Year",
      y = "Articles",
      color = NULL
    ) +
  ieee_theme()
}

#' Create TBATS plot
#' @keywords internal
create_tbats_plot <- function(tbats_data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (is.null(tbats_data) || tbats_data$status != "success") return(NULL)
  
  years <- tbats_data$years
  fitted <- tbats_data$fitted
  
  df <- data.frame(year = years, fitted = fitted)
  
  ggplot2::ggplot(df, ggplot2::aes(x = year, y = fitted)) +
    ggplot2::geom_line(color = "#2166AC", linewidth = 1) +
    ggplot2::labs(
      title = "TBATS Model Fit",
      x = "Year",
      y = "Articles"
    ) +ieee_theme()
}

#' Create Prophet-style plot
#' @keywords internal
create_prophet_plot <- function(prophet_data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (is.null(prophet_data) || prophet_data$status != "success") return(NULL)
  
  trend <- prophet_data$trend
  years <- prophet_data$years %||% seq_along(trend)
  forecast_years <- prophet_data$forecast_years %||% numeric()
  forecast <- prophet_data$forecast$point %||% numeric()

  if (length(trend) == 0 || length(years) != length(trend)) {
    return(NULL)
  }

  df_trend <- data.frame(year = years, value = trend, component = "Trend")
  df_forecast <- if (length(forecast) > 0 && length(forecast_years) == length(forecast)) {
    data.frame(year = forecast_years, value = forecast, component = "Forecast")
  } else {
    data.frame(year = numeric(), value = numeric(), component = character(), stringsAsFactors = FALSE)
  }
  df <- rbind(df_trend, df_forecast)

  if (nrow(df) == 0) {
    return(NULL)
  }
  
  ggplot2::ggplot(df, ggplot2::aes(x = year, y = value, color = component)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(values = c("Trend" = "#2166AC", "Forecast" = "#67A9CF")) +
    ggplot2::labs(
      title = "Prophet-style Decomposition",
      x = "Year",
      y = "Articles",
      color = NULL
    ) +ieee_theme()
}

#' Create state space plot
#' @keywords internal
create_state_space_plot <- function(ss_data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (is.null(ss_data) || ss_data$status != "success") return(NULL)
  
  years <- ss_data$years
  filtered <- ss_data$filtered
  smoothed <- ss_data$smoothed
  
  df <- data.frame(
    year = rep(years, 2),
    value = c(filtered, smoothed),
    type = c(rep("Filtered", length(years)), rep("Smoothed", length(years)))
  )
  
  ggplot2::ggplot(df, ggplot2::aes(x = year, y = value, color = type)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(values = c("Filtered" = "#2166AC", "Smoothed" = "#B2182B")) +
    ggplot2::labs(
      title = "State Space Model (Kalman Filter)",
      x = "Year",
      y = "Articles",
      color = NULL
    ) +ieee_theme()
}

#' Create time series model comparison plot
#' @keywords internal
create_ts_model_comparison_plot <- function(comparison, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (is.null(comparison) || nrow(comparison) == 0) return(NULL)
  
  comparison$model <- factor(comparison$model, levels = comparison$model)
  
  ggplot2::ggplot(comparison, ggplot2::aes(x = model, y = AIC)) +
    ggplot2::geom_col(fill = "#2166AC", alpha = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Time Series Model Comparison (AIC)",
      x = "Model",
      y = "AIC"
    ) +ieee_theme()
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
