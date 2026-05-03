# ============================================================================
# m2_model_registry.R - Build unified model registries for M2
# ============================================================================

#' Build unified model registries for M2 diagnostics
#' @keywords internal
m2_build_model_registry <- function(m2_data, input, config = biblio_config()) {
  input <- m2_normalize_input(input)
  years <- m2_num_vec(input$Year)
  articles <- m2_num_vec(input$Articles)
  cumulative <- cumsum(articles)

  annual_models <- list()
  cumulative_models <- list()

  if (!is.null(m2_data$regression$models)) {
    annual_models <- c(annual_models, m2_data$regression$models)
  }

  if (!is.null(m2_data$forecasting) && identical(m2_data$forecasting$status, "success")) {
    annual_models <- c(
      annual_models,
      m2_build_forecasting_cards(m2_data$forecasting, years, articles)
    )
  }

  if (!is.null(m2_data$advanced_ts) && identical(m2_data$advanced_ts$status, "success")) {
    annual_models <- c(
      annual_models,
      m2_build_advanced_ts_cards(m2_data$advanced_ts, years, articles)
    )
  }

  if (!is.null(m2_data$growth_models) && identical(m2_data$growth_models$status, "success")) {
    cumulative_models <- c(
      cumulative_models,
      m2_build_growth_cards(m2_data$growth_models, years, cumulative)
    )
  }

  list(
    annual = annual_models,
    cumulative = cumulative_models,
    all = c(annual_models, cumulative_models)
  )
}

#' Convert forecasting outputs to canonical cards
#' @keywords internal
m2_build_forecasting_cards <- function(forecasting, years, articles) {
  cards <- list()
  horizon <- m2_coalesce_null(forecasting$horizon, 0)
  future_years <- if (horizon > 0) (max(years) + 1):(max(years) + horizon) else numeric(0)

  candidates <- list(
    ARIMA = forecasting$arima,
    ETS = forecasting$ets,
    Naive = forecasting$naive
  )

  for (nm in names(candidates)) {
    result <- candidates[[nm]]
    if (is.null(result) || !identical(result$status, "success")) {
      next
    }

    forecast_point <- if (!is.null(result$forecast) && !is.null(result$forecast$point)) {
      result$forecast$point
    } else if (horizon > 0) {
      generate_forecast(result, horizon)
    } else {
      numeric(0)
    }

    lower <- if (!is.null(result$forecast) && !is.null(result$forecast$lower)) result$forecast$lower else NULL
    upper <- if (!is.null(result$forecast) && !is.null(result$forecast$upper)) result$forecast$upper else NULL

    cards[[nm]] <- m2_new_model_card(
      name = nm,
      family = tolower(nm),
      years = years,
      actual = articles,
      fitted = result$fitted,
      model_object = NULL,
      forecast = m2_build_forecast_payload(
        point = forecast_point,
        years = future_years,
        lower = lower,
        upper = upper,
        method = tolower(nm)
      ),
      equation = nm,
      source = "forecasting",
      target = "annual",
      metadata = list(
        AIC = result$AIC,
        BIC = result$BIC,
        logLik = result$logLik,
        n_params = length(m2_coalesce_null(result$coefficients, m2_coalesce_null(result$parameters, numeric(0))))
      )
    )
  }

  if (!is.null(forecasting$ensemble) && identical(forecasting$ensemble$status, "success")) {
    ensemble_fitted <- m2_build_ensemble_fitted(forecasting)
    cards$Ensemble <- m2_new_model_card(
      name = "Ensemble",
      family = "ensemble",
      years = years,
      actual = articles,
      fitted = ensemble_fitted,
      model_object = NULL,
      forecast = m2_build_forecast_payload(
        point = forecasting$ensemble$forecast,
        years = forecasting$ensemble$years,
        method = "ensemble"
      ),
      equation = "Weighted ensemble forecast",
      source = "forecasting",
      target = "annual",
      metadata = list(
        AIC = if (!is.null(forecasting$model_comparison$comparison$AIC)) {
          mean(forecasting$model_comparison$comparison$AIC, na.rm = TRUE)
        } else {
          NA_real_
        },
        BIC = if (!is.null(forecasting$model_comparison$comparison$BIC)) {
          mean(forecasting$model_comparison$comparison$BIC, na.rm = TRUE)
        } else {
          NA_real_
        },
        n_params = 3
      )
    )
  }

  cards
}

#' Build fitted ensemble series from component fits
#' @keywords internal
m2_build_ensemble_fitted <- function(forecasting) {
  fitted_candidates <- list(
    arima = forecasting$arima$fitted,
    ets = forecasting$ets$fitted,
    naive = forecasting$naive$fitted
  )
  lengths <- vapply(fitted_candidates, length, integer(1))
  if (length(unique(lengths[lengths > 0])) != 1) {
    return(numeric(0))
  }

  weights <- forecasting$ensemble$weights
  if (is.null(weights) || length(weights) != 3) {
    weights <- rep(1 / 3, 3)
  }

  as.numeric(weights[1] * fitted_candidates$arima +
    weights[2] * fitted_candidates$ets +
    weights[3] * fitted_candidates$naive)
}

#' Convert advanced time-series outputs to canonical cards
#' @keywords internal
m2_build_advanced_ts_cards <- function(advanced_ts, years, articles) {
  cards <- list()
  model_names <- c("sarima", "tbats", "prophet", "state_space", "dynamic_regression")

  for (nm in model_names) {
    result <- advanced_ts[[nm]]
    if (is.null(result) || !identical(result$status, "success") || is.null(result$fitted)) {
      next
    }

    forecast_payload <- if (!is.null(result$forecast)) {
      m2_build_forecast_payload(
        point = m2_coalesce_null(result$forecast$point, result$forecast),
        years = result$forecast_years,
        lower = result$forecast$lower,
        upper = result$forecast$upper,
        method = nm
      )
    } else {
      m2_build_forecast_payload()
    }

    cards[[toupper(nm)]] <- m2_new_model_card(
      name = toupper(nm),
      family = nm,
      years = years,
      actual = articles,
      fitted = result$fitted,
      model_object = m2_coalesce_null(result$model, NULL),
      forecast = forecast_payload,
      equation = nm,
      source = "advanced_ts",
      target = "annual",
      metadata = list(
        AIC = result$AIC,
        BIC = result$BIC,
        logLik = result$logLik,
        n_params = length(m2_coalesce_null(result$coefficients, m2_coalesce_null(result$params, numeric(0))))
      )
    )
  }

  cards
}

#' Convert growth outputs to canonical cards
#' @keywords internal
m2_build_growth_cards <- function(growth_models, years, cumulative) {
  cards <- list()
  model_names <- c("bass", "gompertz", "weibull", "richards", "von_bertalanffy", "mmf")

  for (nm in model_names) {
    result <- growth_models[[nm]]
    if (is.null(result) || !identical(result$status, "success") || is.null(result$fitted)) {
      next
    }

    forecast_years <- seq(max(years) + 1, by = 1, length.out = 5)
    forecast_obj <- tryCatch(forecast_growth_model(result, forecast_years), error = function(e) NULL)
    cards[[toupper(nm)]] <- m2_new_model_card(
      name = toupper(nm),
      family = "growth",
      years = years,
      actual = cumulative,
      fitted = result$fitted,
      model_object = NULL,
      forecast = m2_build_forecast_payload(
        point = if (!is.null(forecast_obj$cumulative)) forecast_obj$cumulative else numeric(0),
        years = if (!is.null(forecast_obj$years)) forecast_obj$years else forecast_years,
        method = paste0(nm, "_growth")
      ),
      equation = nm,
      source = "growth_models",
      target = "cumulative",
      metadata = list(
        AIC = result$AIC,
        BIC = result$BIC,
        n_params = length(m2_coalesce_null(result$params, numeric(0)))
      )
    )
  }

  cards
}
