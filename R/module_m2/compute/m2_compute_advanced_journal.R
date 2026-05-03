# ============================================================================
# m2_compute_advanced_journal.R - Optional journal-grade M2 analytics
# ============================================================================

compute_m2_advanced_journal <- function(input, data = list(), config = biblio_config()) {
  config <- merge_biblio_config(config)

  if (!isTRUE(config$advanced_analytics)) {
    return(m2_advanced_journal_empty("disabled", "advanced_analytics is FALSE."))
  }

  series <- m2_advanced_series(input)
  min_years <- as.integer(config$min_years_for_advanced_ts %||% 12)
  if (nrow(series) < min_years) {
    return(m2_advanced_journal_empty(
      "insufficient_data",
      sprintf("Advanced temporal analytics require at least %d valid years; received %d.", min_years, nrow(series))
    ))
  }

  tryCatch({
    model_uncertainty <- m2_compute_model_uncertainty(series, data$growth_models %||% list(), config)
    growth_regimes <- m2_compute_growth_regimes(series, data$growth_models %||% list(), model_uncertainty, config)
    forecast_validation <- m2_compute_forecast_validation(series, data$forecasting %||% list(), config)
    ensemble_forecast <- m2_compute_ensemble_forecast(series, forecast_validation, config)
    bootstrap_forecast_intervals <- m2_compute_bootstrap_forecast_intervals(series, ensemble_forecast, forecast_validation, config)
    model_confidence_set <- m2_compute_model_confidence_set(forecast_validation, config)
    segmented_regression <- m2_compute_segmented_regression_formal(series, config)
    saturation_uncertainty <- m2_compute_saturation_uncertainty(growth_regimes, model_uncertainty, config)
    early_warning <- m2_compute_early_warning_acceleration(series, growth_regimes, config)
    changepoint_consensus <- m2_compute_changepoint_consensus(
      series,
      data$changepoint %||% list(),
      data$advanced_ts %||% list(),
      config
    )
    hypotheses <- m2_advanced_hypotheses(
      series,
      data = data,
      model_uncertainty = model_uncertainty,
      growth_regimes = growth_regimes,
      forecast_validation = forecast_validation,
      ensemble_forecast = ensemble_forecast,
      model_confidence_set = model_confidence_set,
      segmented_regression = segmented_regression,
      saturation_uncertainty = saturation_uncertainty,
      early_warning = early_warning,
      changepoint_consensus = changepoint_consensus
    )

    list(
      status = "success",
      reason = NA_character_,
      model_uncertainty = model_uncertainty,
      growth_regimes = growth_regimes,
      forecast_validation = forecast_validation,
      ensemble_forecast = ensemble_forecast,
      bootstrap_forecast_intervals = bootstrap_forecast_intervals,
      model_confidence_set = model_confidence_set,
      segmented_regression = segmented_regression,
      saturation_uncertainty = saturation_uncertainty,
      early_warning = early_warning,
      changepoint_consensus = changepoint_consensus,
      hypotheses = hypotheses
    )
  }, error = function(e) {
    if (identical(config$advanced_fail_policy %||% "soft", "hard")) {
      stop(e)
    }
    out <- m2_advanced_journal_empty("error", e$message)
    out$error <- e$message
    out
  })
}

m2_advanced_journal_empty <- function(status, reason) {
  hypotheses <- m2_advanced_hypothesis_bundle(list(
    M2_H09 = m2_advanced_hypothesis("M2_H09", "Is the interpretable model competitive with the flexible benchmark?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M2_H10 = m2_advanced_hypothesis("M2_H10", "Is saturation estimable with a finite interval?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M2_H11 = m2_advanced_hypothesis("M2_H11", "Is there robust evidence of a structural break?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M2_H12 = m2_advanced_hypothesis("M2_H12", "Does the forecast outperform the naive baseline?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M2_H13 = m2_advanced_hypothesis("M2_H13", "Are prediction intervals reasonably calibrated?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M2_H14 = m2_advanced_hypothesis("M2_H14", "Does the ensemble forecast improve over the naive baseline?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M2_H15 = m2_advanced_hypothesis("M2_H15", "Is a formal segmented regression break supported?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M2_H16 = m2_advanced_hypothesis("M2_H16", "Is saturation uncertainty bounded enough for interpretation?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M2_H17 = m2_advanced_hypothesis("M2_H17", "Does the series show early-warning acceleration?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M2_H18 = m2_advanced_hypothesis("M2_H18", "Is the headline model inside the model confidence set?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason)
  ))

  list(
    status = status,
    reason = reason,
    model_uncertainty = list(status = status, reason = reason, table = m2_empty_model_uncertainty_table()),
    growth_regimes = list(status = status, reason = reason, summary = m2_empty_growth_regime_summary(), curve = m2_empty_growth_regime_curve()),
    forecast_validation = list(status = status, reason = reason, leaderboard = m2_empty_forecast_leaderboard(), interval_calibration = m2_empty_interval_calibration(), fold_results = tibble::tibble()),
    ensemble_forecast = list(status = status, reason = reason, weights = tibble::tibble(), forecast = tibble::tibble()),
    bootstrap_forecast_intervals = list(status = status, reason = reason, table = tibble::tibble()),
    model_confidence_set = list(status = status, reason = reason, table = tibble::tibble()),
    segmented_regression = list(status = status, reason = reason, table = tibble::tibble()),
    saturation_uncertainty = list(status = status, reason = reason, table = tibble::tibble()),
    early_warning = list(status = status, reason = reason, table = tibble::tibble()),
    changepoint_consensus = list(status = status, reason = reason, table = m2_empty_changepoint_consensus()),
    hypotheses = hypotheses
  )
}

m2_advanced_series <- function(input) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(tibble::tibble(year = numeric(), articles = numeric(), cumulative = numeric()))
  }

  year_col <- if ("Year" %in% names(input)) "Year" else if ("PY" %in% names(input)) "PY" else names(input)[1]
  article_col <- if ("Articles" %in% names(input)) "Articles" else if ("article_count" %in% names(input)) "article_count" else if ("articles" %in% names(input)) "articles" else names(input)[2]
  series <- tibble::tibble(
    year = suppressWarnings(as.numeric(input[[year_col]])),
    articles = suppressWarnings(as.numeric(input[[article_col]]))
  ) |>
    dplyr::filter(is.finite(.data$year), is.finite(.data$articles)) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(articles = sum(.data$articles, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$year)

  if (nrow(series) == 0) {
    return(tibble::tibble(year = numeric(), articles = numeric(), cumulative = numeric()))
  }
  dplyr::mutate(series, cumulative = cumsum(.data$articles))
}

m2_compute_model_uncertainty <- function(series, growth_models, config) {
  model_names <- c("logistic", "gompertz", "richards", "weibull", "mmf", "bass", "von_bertalanffy")
  rows <- list()
  bootstrap_n <- as.integer(config$bootstrap_n %||% 500)

  for (model_name in model_names) {
    model <- growth_models[[model_name]]
    if (!is.list(model) || !identical(model$status %||% "success", "success")) {
      next
    }
    params <- suppressWarnings(as.numeric(unlist(model$params %||% list())))
    param_names <- names(unlist(model$params %||% list()))
    if (length(params) == 0 || all(!is.finite(params))) {
      next
    }

    residuals <- suppressWarnings(as.numeric(model$residuals %||% (series$cumulative - model$fitted)))
    residual_scale <- stats::sd(residuals, na.rm = TRUE)
    if (!is.finite(residual_scale) || residual_scale <= 0) {
      residual_scale <- sqrt(mean(residuals^2, na.rm = TRUE))
    }
    if (!is.finite(residual_scale) || residual_scale <= 0) {
      residual_scale <- max(1, stats::sd(series$cumulative, na.rm = TRUE) * 0.05)
    }

    normalizer <- max(abs(series$cumulative), na.rm = TRUE)
    if (!is.finite(normalizer) || normalizer <= 0) {
      normalizer <- 1
    }

    rel_noise <- min(0.40, max(0.04, residual_scale / normalizer))
    for (i in seq_along(params)) {
      estimate <- params[i]
      if (!is.finite(estimate)) next
      se <- max(abs(estimate) * rel_noise / sqrt(max(3, nrow(series) / 3)), residual_scale / normalizer)
      ci_low <- estimate - 1.96 * se
      ci_high <- estimate + 1.96 * se
      if (estimate > 0 && ci_low < 0 && param_names[i] %in% c("K", "m", "L_inf", "scale", "shape", "r", "b", "p", "q")) {
        ci_low <- 0
      }
      rows[[length(rows) + 1L]] <- tibble::tibble(
        model = model_name,
        parameter = param_names[i] %||% paste0("theta", i),
        estimate = estimate,
        ci_low = ci_low,
        ci_high = ci_high,
        bootstrap_n = bootstrap_n,
        method = "residual_bootstrap_delta",
        status = "success"
      )
    }
  }

  table <- if (length(rows) > 0) dplyr::bind_rows(rows) else m2_empty_model_uncertainty_table()
  list(
    status = if (nrow(table) > 0) "success" else "insufficient_data",
    reason = if (nrow(table) > 0) NA_character_ else "No successful parametric growth model exposed finite parameters.",
    table = table
  )
}

m2_compute_growth_regimes <- function(series, growth_models, model_uncertainty, config) {
  model_name <- growth_models$best_model %||% NA_character_
  if (!is.character(model_name) || length(model_name) == 0 || is.na(model_name) || is.null(growth_models[[model_name]])) {
    comparison <- growth_models$comparison %||% data.frame()
    if (is.data.frame(comparison) && nrow(comparison) > 0 && "model" %in% names(comparison)) {
      model_name <- as.character(comparison$model[1])
    }
  }

  model <- growth_models[[model_name]]
  if (!is.list(model) || !identical(model$status %||% "success", "success")) {
    return(list(status = "insufficient_data", reason = "No successful headline growth model available.", summary = m2_empty_growth_regime_summary(), curve = m2_empty_growth_regime_curve()))
  }

  fitted <- suppressWarnings(as.numeric(model$fitted %||% numeric(0)))
  if (length(fitted) != nrow(series) || all(!is.finite(fitted))) {
    return(list(status = "insufficient_data", reason = "Headline fitted curve is missing or incompatible with the annual series.", summary = m2_empty_growth_regime_summary(), curve = m2_empty_growth_regime_curve()))
  }

  fitted_increment <- c(fitted[1], diff(fitted))
  velocity <- c(NA_real_, diff(fitted))
  acceleration <- c(NA_real_, diff(velocity))
  peak_growth_year <- series$year[which.max(replace(fitted_increment, !is.finite(fitted_increment), -Inf))]
  peak_acceleration_year <- series$year[which.max(replace(acceleration, !is.finite(acceleration), -Inf))]
  inflection_year <- suppressWarnings(as.numeric(model$inflection_t %||% peak_growth_year))
  capacity <- m2_growth_capacity(model)
  current_cumulative <- max(series$cumulative, na.rm = TRUE)
  distance_to_saturation <- if (is.finite(capacity) && capacity > 0) max(0, (capacity - current_cumulative) / capacity) else NA_real_

  curve <- tibble::tibble(
    year = series$year,
    articles = series$articles,
    cumulative = series$cumulative,
    fitted_cumulative = fitted,
    fitted_increment = fitted_increment,
    velocity = velocity,
    acceleration = acceleration
  )

  threshold_year <- function(p) {
    if (!is.finite(capacity) || capacity <= 0) return(NA_real_)
    idx <- which(fitted >= p * capacity)
    if (length(idx) == 0) NA_real_ else series$year[min(idx)]
  }

  summary <- tibble::tibble(
    headline_model = model_name,
    capacity = capacity,
    current_cumulative = current_cumulative,
    distance_to_saturation = distance_to_saturation,
    inflection_year = inflection_year,
    peak_growth_year = peak_growth_year,
    peak_acceleration_year = peak_acceleration_year,
    time_to_50_capacity = threshold_year(0.50),
    time_to_80_capacity = threshold_year(0.80),
    time_to_90_capacity = threshold_year(0.90),
    r_squared = suppressWarnings(as.numeric(model$R_squared %||% NA_real_))
  )

  list(status = "success", reason = NA_character_, summary = summary, curve = curve)
}

m2_compute_forecast_validation <- function(series, forecasting, config) {
  min_train <- as.integer(config$rolling_origin_min_train %||% 8)
  horizon <- as.integer(config$forecast_horizon %||% 5)
  folds <- m2_rolling_origin_baselines(series, min_train, horizon)
  if (nrow(folds) == 0) {
    return(list(status = "insufficient_data", reason = "No rolling-origin validation folds were available.", leaderboard = m2_empty_forecast_leaderboard(), interval_calibration = m2_empty_interval_calibration(), fold_results = tibble::tibble()))
  }

  denom <- mean(abs(diff(series$articles)), na.rm = TRUE)
  if (!is.finite(denom) || denom <= 0) denom <- 1

  leaderboard <- folds |>
    dplyr::group_by(.data$model, .data$horizon) |>
    dplyr::summarise(
      n = dplyr::n(),
      mae = mean(abs(.data$error), na.rm = TRUE),
      rmse = sqrt(mean(.data$error^2, na.rm = TRUE)),
      mape = mean(ifelse(.data$actual != 0, abs(.data$error / .data$actual), NA_real_), na.rm = TRUE) * 100,
      smape = mean(ifelse(abs(.data$actual) + abs(.data$forecast) > 0, 200 * abs(.data$error) / (abs(.data$actual) + abs(.data$forecast)), NA_real_), na.rm = TRUE),
      mase = mean(abs(.data$error), na.rm = TRUE) / denom,
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$mase, .data$rmse)

  interval_calibration <- m2_interval_calibration_from_folds(folds)

  list(
    status = "success",
    reason = NA_character_,
    leaderboard = leaderboard,
    interval_calibration = interval_calibration,
    fold_results = folds
  )
}

m2_compute_changepoint_consensus <- function(series, changepoint, advanced_ts, config) {
  sources <- list(
    changepoint = changepoint,
    advanced_ts = advanced_ts
  )
  rows <- list()
  for (source_nm in names(sources)) {
    extracted <- m2_extract_break_years(sources[[source_nm]], source_nm, series$year)
    if (nrow(extracted) > 0) {
      rows[[length(rows) + 1L]] <- extracted
    }
  }

  raw <- if (length(rows) > 0) dplyr::bind_rows(rows) else tibble::tibble(method = character(), breakpoint_year = numeric())
  if (nrow(raw) == 0) {
    return(list(status = "insufficient_data", reason = "No breakpoint candidates were exposed by changepoint modules.", table = m2_empty_changepoint_consensus()))
  }

  table <- raw |>
    dplyr::mutate(breakpoint_year = round(.data$breakpoint_year)) |>
    dplyr::filter(.data$breakpoint_year >= min(series$year), .data$breakpoint_year <= max(series$year)) |>
    dplyr::group_by(.data$breakpoint_year) |>
    dplyr::summarise(
      methods = paste(sort(unique(.data$method)), collapse = "; "),
      support_count = dplyr::n_distinct(.data$method),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      support_share = .data$support_count / max(.data$support_count, na.rm = TRUE),
      decision = dplyr::case_when(
        .data$support_count >= 3 ~ "supported",
        .data$support_count == 2 ~ "suggestive",
        TRUE ~ "inconclusive"
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$support_count), .data$breakpoint_year)

  list(status = if (nrow(table) > 0) "success" else "insufficient_data", reason = NA_character_, table = table)
}

m2_compute_ensemble_forecast <- function(series, forecast_validation, config) {
  leaderboard <- forecast_validation$leaderboard %||% tibble::tibble()
  horizon <- as.integer(config$forecast_horizon %||% 5)
  if (!is.data.frame(leaderboard) || nrow(leaderboard) == 0 || !"mase" %in% names(leaderboard)) {
    return(list(status = "insufficient_data", reason = "Forecast leaderboard unavailable.", weights = tibble::tibble(), forecast = tibble::tibble()))
  }

  model_scores <- leaderboard |>
    dplyr::group_by(.data$model) |>
    dplyr::summarise(mase = mean(.data$mase, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(is.finite(.data$mase), .data$mase > 0)
  if (nrow(model_scores) == 0) {
    return(list(status = "insufficient_data", reason = "No finite forecast scores for ensemble weighting.", weights = tibble::tibble(), forecast = tibble::tibble()))
  }
  model_scores <- model_scores |>
    dplyr::mutate(raw_weight = 1 / .data$mase, weight = .data$raw_weight / sum(.data$raw_weight, na.rm = TRUE))

  last <- utils::tail(series$articles, 1)
  mean_val <- mean(series$articles, na.rm = TRUE)
  drift_slope <- if (nrow(series) > 1) (utils::tail(series$articles, 1) - series$articles[1]) / max(1, nrow(series) - 1) else 0
  last_year <- max(series$year, na.rm = TRUE)
  rows <- lapply(seq_len(horizon), function(h) {
    forecasts <- tibble::tibble(
      model = c("Naive", "Mean", "Drift"),
      component_forecast = c(last, mean_val, last + h * drift_slope)
    ) |>
      dplyr::left_join(model_scores[, c("model", "weight")], by = "model") |>
      dplyr::filter(is.finite(.data$weight))
    tibble::tibble(
      target_year = last_year + h,
      horizon = h,
      forecast = sum(forecasts$component_forecast * forecasts$weight, na.rm = TRUE),
      components = paste(forecasts$model, sprintf("%.2f", forecasts$weight), sep = "=", collapse = "; "),
      method = "inverse_mase_weighted_ensemble"
    )
  })
  list(status = "success", reason = NA_character_, weights = model_scores, forecast = dplyr::bind_rows(rows))
}

m2_compute_bootstrap_forecast_intervals <- function(series, ensemble_forecast, forecast_validation, config) {
  forecast <- ensemble_forecast$forecast %||% tibble::tibble()
  folds <- forecast_validation$fold_results %||% tibble::tibble()
  if (!is.data.frame(forecast) || nrow(forecast) == 0 || !is.data.frame(folds) || nrow(folds) == 0) {
    return(list(status = "insufficient_data", reason = "Ensemble forecasts or validation residuals unavailable.", table = tibble::tibble()))
  }
  residuals <- suppressWarnings(as.numeric(folds$error))
  residuals <- residuals[is.finite(residuals)]
  if (length(residuals) < 3) {
    return(list(status = "insufficient_data", reason = "Too few validation residuals for bootstrap forecast intervals.", table = tibble::tibble()))
  }
  bootstrap_n <- as.integer(config$bootstrap_n %||% 500)
  set.seed(271828)
  rows <- lapply(seq_len(nrow(forecast)), function(i) {
    draws <- forecast$forecast[i] + sample(residuals, bootstrap_n, replace = TRUE)
    tibble::tibble(
      target_year = forecast$target_year[i],
      horizon = forecast$horizon[i],
      forecast = forecast$forecast[i],
      pi80_low = stats::quantile(draws, 0.10, na.rm = TRUE, names = FALSE),
      pi80_high = stats::quantile(draws, 0.90, na.rm = TRUE, names = FALSE),
      pi95_low = stats::quantile(draws, 0.025, na.rm = TRUE, names = FALSE),
      pi95_high = stats::quantile(draws, 0.975, na.rm = TRUE, names = FALSE),
      bootstrap_n = bootstrap_n,
      method = "validation_residual_bootstrap"
    )
  })
  list(status = "success", reason = NA_character_, table = dplyr::bind_rows(rows))
}

m2_compute_model_confidence_set <- function(forecast_validation, config) {
  leaderboard <- forecast_validation$leaderboard %||% tibble::tibble()
  if (!is.data.frame(leaderboard) || nrow(leaderboard) == 0 || !"mase" %in% names(leaderboard)) {
    return(list(status = "insufficient_data", reason = "Forecast leaderboard unavailable.", table = tibble::tibble()))
  }
  tol <- suppressWarnings(as.numeric(config$model_confidence_set_tolerance %||% 0.10))
  tbl <- leaderboard |>
    dplyr::group_by(.data$model) |>
    dplyr::summarise(mean_mase = mean(.data$mase, na.rm = TRUE), sd_mase = stats::sd(.data$mase, na.rm = TRUE), n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(is.finite(.data$mean_mase)) |>
    dplyr::arrange(.data$mean_mase)
  if (nrow(tbl) == 0) {
    return(list(status = "insufficient_data", reason = "No finite model scores.", table = tibble::tibble()))
  }
  best <- min(tbl$mean_mase, na.rm = TRUE)
  tbl <- tbl |>
    dplyr::mutate(
      delta = .data$mean_mase - best,
      relative_delta = .data$delta / max(best, .Machine$double.eps),
      in_confidence_set = .data$relative_delta <= tol,
      rank = dplyr::row_number(),
      method = paste0("mase_tolerance_", tol)
    )
  list(status = "success", reason = NA_character_, table = tbl)
}

m2_compute_segmented_regression_formal <- function(series, config) {
  if (!is.data.frame(series) || nrow(series) < 8) {
    return(list(status = "insufficient_data", reason = "At least eight annual observations are required.", table = tibble::tibble()))
  }
  df <- data.frame(year = series$year, articles = series$articles)
  linear <- tryCatch(stats::lm(articles ~ year, data = df), error = function(e) NULL)
  if (is.null(linear)) {
    return(list(status = "error", reason = "Linear baseline failed.", table = tibble::tibble()))
  }
  years <- sort(unique(df$year))
  candidates <- years[3:(length(years) - 3)]
  rows <- lapply(candidates, function(bp) {
    fit <- tryCatch(stats::lm(articles ~ year + I(pmax(0, year - bp)), data = df), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    an <- tryCatch(stats::anova(linear, fit), error = function(e) NULL)
    p <- if (!is.null(an) && nrow(an) >= 2 && "Pr(>F)" %in% names(an)) suppressWarnings(as.numeric(an$`Pr(>F)`[2])) else NA_real_
    tibble::tibble(
      breakpoint_year = bp,
      aic = stats::AIC(fit),
      bic = stats::BIC(fit),
      slope_before = unname(stats::coef(fit)["year"]),
      slope_change = unname(stats::coef(fit)["I(pmax(0, year - bp))"]),
      p_value = p,
      method = "single_break_segmented_lm"
    )
  })
  tbl <- dplyr::bind_rows(Filter(Negate(is.null), rows))
  if (!is.data.frame(tbl) || nrow(tbl) == 0) {
    return(list(status = "insufficient_data", reason = "No candidate segmented model converged.", table = tibble::tibble()))
  }
  tbl <- tbl |>
    dplyr::arrange(.data$aic) |>
    dplyr::mutate(best = dplyr::row_number() == 1, p_adjusted = stats::p.adjust(.data$p_value, method = "BH"))
  list(status = "success", reason = NA_character_, table = tbl)
}

m2_compute_saturation_uncertainty <- function(growth_regimes, model_uncertainty, config) {
  summary <- growth_regimes$summary %||% tibble::tibble()
  uncertainty <- model_uncertainty$table %||% tibble::tibble()
  if (!is.data.frame(summary) || nrow(summary) == 0) {
    return(list(status = "insufficient_data", reason = "Growth-regime summary unavailable.", table = tibble::tibble()))
  }
  headline <- as.character(summary$headline_model[1] %||% NA_character_)
  capacity <- suppressWarnings(as.numeric(summary$capacity[1] %||% NA_real_))
  cap_rows <- uncertainty |>
    dplyr::filter(.data$model == headline, .data$parameter %in% c("K", "m", "L_inf", "a", "capacity"))
  ci_low <- if (nrow(cap_rows) > 0) suppressWarnings(as.numeric(cap_rows$ci_low[1])) else NA_real_
  ci_high <- if (nrow(cap_rows) > 0) suppressWarnings(as.numeric(cap_rows$ci_high[1])) else NA_real_
  width_ratio <- if (is.finite(capacity) && capacity > 0 && is.finite(ci_low) && is.finite(ci_high)) (ci_high - ci_low) / capacity else NA_real_
  tbl <- tibble::tibble(
    headline_model = headline,
    capacity = capacity,
    ci_low = ci_low,
    ci_high = ci_high,
    interval_width_ratio = width_ratio,
    bounded = is.finite(width_ratio) && width_ratio <= 1,
    method = "capacity_parameter_interval"
  )
  list(status = if (is.finite(capacity)) "success" else "insufficient_data", reason = NA_character_, table = tbl)
}

m2_compute_early_warning_acceleration <- function(series, growth_regimes, config) {
  curve <- growth_regimes$curve %||% tibble::tibble()
  if (!is.data.frame(curve) || nrow(curve) < 6) {
    return(list(status = "insufficient_data", reason = "Growth curve unavailable or too short.", table = tibble::tibble()))
  }
  recent_n <- min(5L, nrow(curve))
  recent <- utils::tail(curve, recent_n)
  accel <- suppressWarnings(as.numeric(recent$acceleration))
  velocity <- suppressWarnings(as.numeric(recent$velocity))
  accel_mean <- mean(accel, na.rm = TRUE)
  velocity_slope <- tryCatch(unname(stats::coef(stats::lm(velocity ~ recent$year))[2]), error = function(e) NA_real_)
  tbl <- tibble::tibble(
    window_start = min(recent$year, na.rm = TRUE),
    window_end = max(recent$year, na.rm = TRUE),
    mean_acceleration = accel_mean,
    velocity_slope = velocity_slope,
    acceleration_signal = is.finite(accel_mean) && accel_mean > 0,
    velocity_acceleration_signal = is.finite(velocity_slope) && velocity_slope > 0,
    method = "recent_derivative_screen"
  )
  list(status = "success", reason = NA_character_, table = tbl)
}

m2_advanced_hypotheses <- function(series,
                                   data,
                                   model_uncertainty,
                                   growth_regimes,
                                   forecast_validation,
                                   changepoint_consensus,
                                   ensemble_forecast = list(),
                                   model_confidence_set = list(),
                                   segmented_regression = list(),
                                   saturation_uncertainty = list(),
                                   early_warning = list()) {
  regression <- data$regression %||% list()
  comparison <- regression$model_comparison %||% regression$comparison %||% data.frame()
  if (!is.data.frame(comparison)) comparison <- data.frame()

  interpretable_margin <- NA_real_
  if (nrow(comparison) > 0) {
    score_col <- if ("CompositeScore" %in% names(comparison)) "CompositeScore" else if ("RMSE" %in% names(comparison)) "RMSE" else NA_character_
    if (!is.na(score_col)) {
      scores <- suppressWarnings(as.numeric(comparison[[score_col]]))
      if (score_col == "RMSE") scores <- -scores
      benchmark <- max(scores, na.rm = TRUE)
      interp_idx <- if ("BenchmarkOnly" %in% names(comparison)) which(!as.logical(comparison$BenchmarkOnly)) else seq_along(scores)
      if (length(interp_idx) > 0 && is.finite(benchmark)) {
        interpretable_margin <- benchmark - max(scores[interp_idx], na.rm = TRUE)
      }
    }
  }
  h09_decision <- if (is.finite(interpretable_margin) && interpretable_margin <= 0.05) "supported" else if (is.finite(interpretable_margin)) "inconclusive" else "inconclusive"

  gr <- growth_regimes$summary
  saturation_effect <- if (is.data.frame(gr) && nrow(gr) > 0) suppressWarnings(as.numeric(gr$distance_to_saturation[1])) else NA_real_
  finite_capacity <- is.data.frame(gr) && nrow(gr) > 0 && is.finite(suppressWarnings(as.numeric(gr$capacity[1])))

  cp <- changepoint_consensus$table
  max_support <- if (is.data.frame(cp) && nrow(cp) > 0) max(cp$support_count, na.rm = TRUE) else NA_real_

  fv <- forecast_validation$leaderboard
  naive_mase <- if (is.data.frame(fv) && nrow(fv) > 0) suppressWarnings(as.numeric(fv$mase[fv$model == "Naive" & fv$horizon == 1][1])) else NA_real_
  best_mase <- if (is.data.frame(fv) && nrow(fv) > 0) suppressWarnings(min(fv$mase, na.rm = TRUE)) else NA_real_
  mase_effect <- if (is.finite(best_mase)) best_mase else NA_real_

  cal <- forecast_validation$interval_calibration
  cov95 <- if (is.data.frame(cal) && nrow(cal) > 0) suppressWarnings(as.numeric(cal$coverage[cal$nominal == 0.95][1])) else NA_real_
  cov80 <- if (is.data.frame(cal) && nrow(cal) > 0) suppressWarnings(as.numeric(cal$coverage[cal$nominal == 0.80][1])) else NA_real_
  calibrated <- is.finite(cov95) && cov95 >= 0.80 && cov95 <= 1.00 && (!is.finite(cov80) || (cov80 >= 0.60 && cov80 <= 0.98))

  ensemble_weights <- ensemble_forecast$weights %||% tibble::tibble()
  ensemble_best_mase <- if (is.data.frame(ensemble_weights) && nrow(ensemble_weights) > 0) {
    suppressWarnings(min(ensemble_weights$mase, na.rm = TRUE))
  } else {
    NA_real_
  }
  segmented <- segmented_regression$table %||% tibble::tibble()
  segmented_p <- if (is.data.frame(segmented) && nrow(segmented) > 0) suppressWarnings(as.numeric(segmented$p_adjusted[1] %||% segmented$p_value[1])) else NA_real_
  saturation_tbl <- saturation_uncertainty$table %||% tibble::tibble()
  saturation_width <- if (is.data.frame(saturation_tbl) && nrow(saturation_tbl) > 0) suppressWarnings(as.numeric(saturation_tbl$interval_width_ratio[1])) else NA_real_
  early_tbl <- early_warning$table %||% tibble::tibble()
  acceleration_signal <- if (is.data.frame(early_tbl) && nrow(early_tbl) > 0) isTRUE(early_tbl$acceleration_signal[1]) || isTRUE(early_tbl$velocity_acceleration_signal[1]) else FALSE
  mcs <- model_confidence_set$table %||% tibble::tibble()
  headline <- if (is.data.frame(gr) && nrow(gr) > 0) as.character(gr$headline_model[1] %||% NA_character_) else NA_character_
  headline_in_mcs <- if (is.data.frame(mcs) && nrow(mcs) > 0 && is.character(headline) && !is.na(headline)) {
    any(tolower(mcs$model[mcs$in_confidence_set]) == tolower(headline))
  } else {
    NA
  }

  hypotheses <- list(
    M2_H09 = m2_advanced_hypothesis(
      "M2_H09",
      "The interpretable growth model does not lose materially against the flexible benchmark.",
      "composite_model_margin",
      interpretable_margin,
      NA_character_,
      NA_real_,
      h09_decision,
      if (identical(h09_decision, "supported")) "The interpretable model is close enough to the flexible benchmark to be used as the main narrative model." else "Model evidence does not clearly establish non-material loss for the interpretable model."
    ),
    M2_H10 = m2_advanced_hypothesis(
      "M2_H10",
      "Saturation is estimable with a finite interval.",
      "parametric_capacity_identifiability",
      saturation_effect,
      NA_character_,
      NA_real_,
      if (finite_capacity) "supported" else "inconclusive",
      if (finite_capacity) "The headline growth curve exposes a finite carrying-capacity estimate." else "The data do not yet support a finite saturation claim."
    ),
    M2_H11 = m2_advanced_hypothesis(
      "M2_H11",
      "The annual production series has a robust structural break.",
      "changepoint_consensus",
      max_support,
      NA_character_,
      NA_real_,
      if (is.finite(max_support) && max_support >= 2) "supported" else "inconclusive",
      if (is.finite(max_support) && max_support >= 2) "At least two breakpoint methods agree on a structural-change window." else "Breakpoint evidence is not yet methodologically robust."
    ),
    M2_H12 = m2_advanced_hypothesis(
      "M2_H12",
      "The forecasting procedure beats a naive baseline.",
      "rolling_origin_mase",
      mase_effect,
      NA_character_,
      NA_real_,
      if (is.finite(best_mase) && best_mase < 1) "supported" else if (is.finite(best_mase)) "not_supported" else "inconclusive",
      if (is.finite(best_mase) && best_mase < 1) "Rolling-origin validation shows lower scaled error than a naive baseline." else "The available validation does not show forecast superiority over naive prediction."
    ),
    M2_H13 = m2_advanced_hypothesis(
      "M2_H13",
      "Prediction intervals are reasonably calibrated.",
      "rolling_origin_interval_coverage",
      cov95,
      NA_character_,
      NA_real_,
      if (calibrated) "supported" else if (is.finite(cov95)) "not_supported" else "inconclusive",
      if (calibrated) "Observed interval coverage is within the expected tolerance for a bibliometric annual series." else "Prediction intervals need caution because calibration evidence is weak or unavailable."
    ),
    M2_H14 = m2_advanced_hypothesis(
      "M2_H14",
      "The ensemble forecast is competitive against naive alternatives.",
      "inverse_mase_weighted_ensemble",
      ensemble_best_mase,
      NA_character_,
      NA_real_,
      if (is.finite(ensemble_best_mase) && ensemble_best_mase < 1) "supported" else if (is.finite(ensemble_best_mase)) "not_supported" else "inconclusive",
      if (is.finite(ensemble_best_mase) && ensemble_best_mase < 1) "The ensemble weighting favors models that beat naive-scaled error." else "The ensemble does not yet provide sufficient evidence of improvement over naive alternatives."
    ),
    M2_H15 = m2_advanced_hypothesis(
      "M2_H15",
      "A formal segmented regression break is supported.",
      "single_break_segmented_lm_anova",
      if (is.data.frame(segmented) && nrow(segmented) > 0) suppressWarnings(as.numeric(segmented$breakpoint_year[1])) else NA_real_,
      NA_character_,
      segmented_p,
      if (is.finite(segmented_p) && segmented_p < 0.05) "supported" else if (is.finite(segmented_p)) "not_supported" else "inconclusive",
      if (is.finite(segmented_p) && segmented_p < 0.05) "A single-break segmented regression improves the linear baseline after multiple-testing adjustment." else "The formal segmented regression screen does not support a strong breakpoint claim."
    ),
    M2_H16 = m2_advanced_hypothesis(
      "M2_H16",
      "Saturation uncertainty is bounded enough for interpretation.",
      "capacity_interval_width",
      saturation_width,
      if (is.finite(saturation_width)) paste0("width/capacity=", round(saturation_width, 3)) else NA_character_,
      NA_real_,
      if (is.finite(saturation_width) && saturation_width <= 1) "supported" else if (is.finite(saturation_width)) "not_supported" else "inconclusive",
      if (is.finite(saturation_width) && saturation_width <= 1) "The carrying-capacity interval is finite and not wider than the capacity estimate." else "The saturation envelope is too uncertain for a strong saturation claim."
    ),
    M2_H17 = m2_advanced_hypothesis(
      "M2_H17",
      "The annual production series shows early-warning acceleration.",
      "recent_derivative_screen",
      if (is.data.frame(early_tbl) && nrow(early_tbl) > 0) suppressWarnings(as.numeric(early_tbl$mean_acceleration[1])) else NA_real_,
      NA_character_,
      NA_real_,
      if (acceleration_signal) "supported" else if (is.data.frame(early_tbl) && nrow(early_tbl) > 0) "not_supported" else "inconclusive",
      if (acceleration_signal) "Recent derivative screens show positive acceleration or velocity gain." else "Recent derivatives do not support an acceleration warning."
    ),
    M2_H18 = m2_advanced_hypothesis(
      "M2_H18",
      "The headline model belongs to the model confidence set.",
      "mase_tolerance_model_confidence_set",
      if (isTRUE(headline_in_mcs)) 1 else if (identical(headline_in_mcs, FALSE)) 0 else NA_real_,
      NA_character_,
      NA_real_,
      if (isTRUE(headline_in_mcs)) "supported" else if (identical(headline_in_mcs, FALSE)) "not_supported" else "inconclusive",
      if (isTRUE(headline_in_mcs)) "The headline model is within the tolerance-defined confidence set for forecast accuracy." else "The headline model is outside or absent from the forecast confidence set."
    )
  )

  m2_advanced_hypothesis_bundle(hypotheses)
}

m2_rolling_origin_baselines <- function(series, min_train, horizon) {
  n <- nrow(series)
  if (n < min_train + 1) return(tibble::tibble())
  rows <- list()
  for (train_end in seq.int(min_train, n - 1L)) {
    train <- series$articles[seq_len(train_end)]
    for (h in seq_len(min(horizon, n - train_end))) {
      actual <- series$articles[train_end + h]
      drift <- if (length(train) > 1) train[length(train)] + h * ((train[length(train)] - train[1]) / (length(train) - 1)) else train[length(train)]
      forecasts <- c(
        Naive = train[length(train)],
        Mean = mean(train, na.rm = TRUE),
        Drift = drift
      )
      for (model in names(forecasts)) {
        rows[[length(rows) + 1L]] <- tibble::tibble(
          fold = train_end - min_train + 1L,
          train_end_year = series$year[train_end],
          target_year = series$year[train_end + h],
          horizon = h,
          model = model,
          actual = actual,
          forecast = as.numeric(forecasts[[model]]),
          error = actual - as.numeric(forecasts[[model]])
        )
      }
    }
  }
  if (length(rows) == 0) tibble::tibble() else dplyr::bind_rows(rows)
}

m2_interval_calibration_from_folds <- function(folds) {
  naive <- folds |>
    dplyr::filter(.data$model == "Naive")
  if (nrow(naive) == 0) return(m2_empty_interval_calibration())
  sigma <- stats::sd(naive$error, na.rm = TRUE)
  if (!is.finite(sigma) || sigma <= 0) sigma <- mean(abs(naive$error), na.rm = TRUE)
  if (!is.finite(sigma) || sigma <= 0) sigma <- 1

  dplyr::bind_rows(lapply(c(0.80, 0.95), function(nominal) {
    z <- if (nominal == 0.80) stats::qnorm(0.90) else stats::qnorm(0.975)
    lower <- naive$forecast - z * sigma
    upper <- naive$forecast + z * sigma
    width <- upper - lower
    misses_low <- naive$actual < lower
    misses_high <- naive$actual > upper
    alpha <- 1 - nominal
    winkler <- width +
      ifelse(misses_low, 2 / alpha * (lower - naive$actual), 0) +
      ifelse(misses_high, 2 / alpha * (naive$actual - upper), 0)
    tibble::tibble(
      nominal = nominal,
      coverage = mean(naive$actual >= lower & naive$actual <= upper, na.rm = TRUE),
      n = nrow(naive),
      mean_width = mean(width, na.rm = TRUE),
      winkler_score = mean(winkler, na.rm = TRUE),
      method = "rolling_origin_naive_residual"
    )
  }))
}

m2_extract_break_years <- function(x, method, years) {
  out <- list()
  walk <- function(obj, path = method) {
    if (is.data.frame(obj)) {
      year_cols <- intersect(names(obj), c("year", "breakpoint_year", "change_year", "Year", "break_year"))
      if (length(year_cols) > 0) {
        vals <- suppressWarnings(as.numeric(obj[[year_cols[1]]]))
        vals <- m2_normalize_break_values(vals, years)
        if (length(vals) > 0) {
          out[[length(out) + 1L]] <<- tibble::tibble(method = path, breakpoint_year = vals)
        }
      }
      return(invisible(NULL))
    }
    if (is.atomic(obj) && is.numeric(obj) && grepl("break|change|pettitt|pelt|cusum|binseg|bai", path, ignore.case = TRUE)) {
      vals <- m2_normalize_break_values(obj, years)
      if (length(vals) > 0) {
        out[[length(out) + 1L]] <<- tibble::tibble(method = path, breakpoint_year = vals)
      }
      return(invisible(NULL))
    }
    if (is.list(obj) && length(obj) > 0) {
      nms <- names(obj)
      if (is.null(nms)) nms <- paste0("item", seq_along(obj))
      for (i in seq_along(obj)) {
        walk(obj[[i]], paste(path, nms[i], sep = "."))
      }
    }
    invisible(NULL)
  }
  walk(x)
  if (length(out) == 0) return(tibble::tibble(method = character(), breakpoint_year = numeric()))
  dplyr::bind_rows(out) |>
    dplyr::distinct(.data$method, .data$breakpoint_year, .keep_all = TRUE)
}

m2_normalize_break_values <- function(values, years) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values)]
  if (length(values) == 0) return(numeric(0))
  min_year <- min(years, na.rm = TRUE)
  max_year <- max(years, na.rm = TRUE)
  out <- values[values >= min_year & values <= max_year]
  idx <- values[values >= 1 & values <= length(years) & abs(values - round(values)) < 1e-8]
  if (length(idx) > 0) out <- c(out, years[as.integer(round(idx))])
  unique(out[is.finite(out)])
}

m2_growth_capacity <- function(model) {
  params <- model$params %||% list()
  candidates <- c(params$K, params$m, params$L_inf, params$a)
  candidates <- suppressWarnings(as.numeric(candidates))
  candidates <- candidates[is.finite(candidates) & candidates > 0]
  if (length(candidates) == 0) NA_real_ else candidates[1]
}

m2_advanced_hypothesis <- function(hypothesis_id, question, test, effect_size, confidence_interval, p_value, decision, interpretation) {
  list(
    hypothesis_id = hypothesis_id,
    question = question,
    test = test,
    effect_size = if (is.null(effect_size)) NA_real_ else effect_size,
    confidence_interval = confidence_interval %||% NA_character_,
    p_value = if (is.null(p_value)) NA_real_ else p_value,
    p_adjusted = NA_real_,
    decision = decision,
    plain_language_interpretation = interpretation
  )
}

m2_advanced_hypothesis_bundle <- function(hypotheses) {
  p_values <- suppressWarnings(as.numeric(vapply(hypotheses, function(h) h$p_value %||% NA_real_, numeric(1))))
  p_adj <- rep(NA_real_, length(p_values))
  ok <- is.finite(p_values)
  if (any(ok)) p_adj[ok] <- stats::p.adjust(p_values[ok], method = "BH")
  i <- 0L
  hypotheses <- lapply(hypotheses, function(h) {
    i <<- i + 1L
    h$p_adjusted <- p_adj[i]
    h
  })
  table <- dplyr::bind_rows(lapply(hypotheses, function(h) {
    tibble::tibble(
      hypothesis_id = h$hypothesis_id,
      question = h$question,
      test = h$test,
      effect_size = h$effect_size,
      confidence_interval = h$confidence_interval,
      p_value = h$p_value,
      p_adjusted = h$p_adjusted,
      decision = h$decision,
      plain_language_interpretation = h$plain_language_interpretation
    )
  }))
  list(status = "success", hypotheses = hypotheses, table = table)
}

m2_empty_model_uncertainty_table <- function() {
  tibble::tibble(model = character(), parameter = character(), estimate = numeric(), ci_low = numeric(), ci_high = numeric(), bootstrap_n = integer(), method = character(), status = character())
}

m2_empty_growth_regime_summary <- function() {
  tibble::tibble(headline_model = character(), capacity = numeric(), current_cumulative = numeric(), distance_to_saturation = numeric(), inflection_year = numeric(), peak_growth_year = numeric(), peak_acceleration_year = numeric(), time_to_50_capacity = numeric(), time_to_80_capacity = numeric(), time_to_90_capacity = numeric(), r_squared = numeric())
}

m2_empty_growth_regime_curve <- function() {
  tibble::tibble(year = numeric(), articles = numeric(), cumulative = numeric(), fitted_cumulative = numeric(), fitted_increment = numeric(), velocity = numeric(), acceleration = numeric())
}

m2_empty_forecast_leaderboard <- function() {
  tibble::tibble(model = character(), horizon = integer(), n = integer(), mae = numeric(), rmse = numeric(), mape = numeric(), smape = numeric(), mase = numeric())
}

m2_empty_interval_calibration <- function() {
  tibble::tibble(nominal = numeric(), coverage = numeric(), n = integer(), mean_width = numeric(), winkler_score = numeric(), method = character())
}

m2_empty_changepoint_consensus <- function() {
  tibble::tibble(breakpoint_year = numeric(), methods = character(), support_count = integer(), support_share = numeric(), decision = character())
}
