# ============================================================================
# m2_model_contract.R - Canonical model contract for M2
# ============================================================================

#' Null-coalescing helper for M2 internals
#' @keywords internal
m2_coalesce_null <- function(a, b) if (!is.null(a)) a else b

#' Safe numeric scalar conversion
#' @keywords internal
m2_scalar_num <- function(x, default = NA_real_) {
  value <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(value)) {
    return(default)
  }
  value
}

#' Safe numeric vector conversion
#' @keywords internal
m2_num_vec <- function(x) {
  if (is.null(x)) {
    return(numeric(0))
  }
  suppressWarnings(as.numeric(x))
}

#' Approximate Gaussian log-likelihood from residuals
#' @keywords internal
m2_residual_loglik <- function(residuals) {
  residuals <- residuals[is.finite(residuals)]
  n <- length(residuals)
  if (n == 0) {
    return(NA_real_)
  }

  sigma2 <- mean(residuals^2, na.rm = TRUE)
  sigma2 <- max(sigma2, .Machine$double.eps)
  -0.5 * n * (log(2 * pi * sigma2) + 1)
}

#' Information criteria for canonical cards
#' @keywords internal
m2_information_criteria <- function(model_object, residuals, n_params, n_obs) {
  if (!is.null(model_object)) {
    aic <- tryCatch(AIC(model_object), error = function(e) NA_real_)
    bic <- tryCatch(BIC(model_object), error = function(e) NA_real_)
    ll <- tryCatch(as.numeric(logLik(model_object)), error = function(e) NA_real_)
    return(list(AIC = m2_scalar_num(aic), BIC = m2_scalar_num(bic), logLik = m2_scalar_num(ll)))
  }

  loglik <- m2_residual_loglik(residuals)
  if (!is.finite(loglik) || !is.finite(n_params) || !is.finite(n_obs) || n_obs <= 0) {
    return(list(AIC = NA_real_, BIC = NA_real_, logLik = NA_real_))
  }

  list(
    AIC = -2 * loglik + 2 * n_params,
    BIC = -2 * loglik + log(n_obs) * n_params,
    logLik = loglik
  )
}

#' Compute scalar metrics for a fitted curve
#' @keywords internal
m2_compute_fit_metrics <- function(actual, fitted, n_params = 1L) {
  actual <- m2_num_vec(actual)
  fitted <- m2_num_vec(fitted)

  keep <- is.finite(actual) & is.finite(fitted)
  actual <- actual[keep]
  fitted <- fitted[keep]

  if (length(actual) == 0 || length(actual) != length(fitted)) {
    return(list(
      r_squared = NA_real_,
      adj_r_squared = NA_real_,
      rmse = NA_real_,
      mae = NA_real_,
      mape = NA_real_,
      smape = NA_real_,
      me = NA_real_,
      mbe = NA_real_,
      theil_u = NA_real_
    ))
  }

  residuals <- actual - fitted
  n <- length(actual)
  ss_res <- sum(residuals^2, na.rm = TRUE)
  ss_tot <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  if (is.finite(r2)) {
    r2 <- max(0, min(1, r2))
  }

  rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
  mae <- mean(abs(residuals), na.rm = TRUE)
  me <- mean(residuals, na.rm = TRUE)

  nonzero <- abs(actual) > .Machine$double.eps
  mape <- if (any(nonzero)) mean(abs(residuals[nonzero] / actual[nonzero]), na.rm = TRUE) * 100 else NA_real_

  scale_term <- abs(actual) + abs(fitted)
  smape <- if (any(scale_term > .Machine$double.eps)) {
    mean(200 * abs(residuals) / pmax(scale_term, .Machine$double.eps), na.rm = TRUE)
  } else {
    NA_real_
  }

  adj_r2 <- if (is.finite(r2) && n > n_params + 1) {
    1 - (1 - r2) * (n - 1) / (n - n_params - 1)
  } else {
    r2
  }
  if (is.finite(adj_r2)) {
    adj_r2 <- max(0, min(1, adj_r2))
  }

  naive_scale <- mean(abs(diff(actual)), na.rm = TRUE)
  theil_u <- if (n > 1 && is.finite(naive_scale) && naive_scale > .Machine$double.eps) {
    rmse / naive_scale
  } else {
    NA_real_
  }

  list(
    r_squared = r2,
    adj_r_squared = adj_r2,
    rmse = rmse,
    mae = mae,
    mape = mape,
    smape = smape,
    me = me,
    mbe = me,
    theil_u = theil_u
  )
}

#' Durbin-Watson statistic
#' @keywords internal
m2_durbin_watson_stat <- function(residuals) {
  residuals <- residuals[is.finite(residuals)]
  if (length(residuals) < 2) {
    return(NA_real_)
  }
  denom <- sum(residuals^2, na.rm = TRUE)
  if (denom <= .Machine$double.eps) {
    return(NA_real_)
  }
  sum(diff(residuals)^2, na.rm = TRUE) / denom
}

#' Breusch-Pagan style heteroscedasticity test
#' @keywords internal
m2_breusch_pagan <- function(fitted, residuals) {
  keep <- is.finite(fitted) & is.finite(residuals)
  fitted <- fitted[keep]
  residuals <- residuals[keep]
  n <- length(residuals)

  if (n < 5) {
    return(list(statistic = NA_real_, p_value = NA_real_))
  }

  aux <- tryCatch(lm(I(residuals^2) ~ fitted), error = function(e) NULL)
  if (is.null(aux)) {
    return(list(statistic = NA_real_, p_value = NA_real_))
  }

  r2 <- tryCatch(summary(aux)$r.squared, error = function(e) NA_real_)
  if (!is.finite(r2)) {
    return(list(statistic = NA_real_, p_value = NA_real_))
  }

  stat <- n * r2
  list(
    statistic = stat,
    p_value = 1 - stats::pchisq(stat, df = 1)
  )
}

#' Basic residual diagnostics for a canonical model card
#' @keywords internal
m2_compute_card_diagnostics <- function(years, actual, fitted, residuals, n_params = 1L) {
  residuals <- m2_num_vec(residuals)
  years <- m2_num_vec(years)
  fitted <- m2_num_vec(fitted)

  keep <- is.finite(residuals)
  residuals <- residuals[keep]
  fitted <- fitted[keep]
  if (length(years) >= length(keep)) {
    years <- years[keep]
  }

  if (length(residuals) == 0) {
    return(list(
      shapiro_p = NA_real_,
      durbin_watson = NA_real_,
      ljung_box_p = NA_real_,
      breusch_pagan_p = NA_real_,
      breakpoint_count = 0L,
      breakpoint_years = numeric(0),
      max_abs_acf = NA_real_,
      stability_score = NA_real_
    ))
  }

  shapiro_p <- if (length(residuals) >= 3 && length(residuals) <= 5000) {
    tryCatch(stats::shapiro.test(residuals)$p.value, error = function(e) NA_real_)
  } else {
    NA_real_
  }

  dw <- m2_durbin_watson_stat(residuals)

  lag_max <- min(5L, length(residuals) - 1L)
  ljung_p <- if (lag_max >= 1L) {
    tryCatch(stats::Box.test(residuals, lag = lag_max, type = "Ljung-Box")$p.value, error = function(e) NA_real_)
  } else {
    NA_real_
  }

  acf_vals <- if (lag_max >= 1L) {
    tryCatch(stats::acf(residuals, plot = FALSE, lag.max = lag_max)$acf[-1], error = function(e) numeric(0))
  } else {
    numeric(0)
  }
  max_abs_acf <- if (length(acf_vals) > 0) max(abs(acf_vals), na.rm = TRUE) else NA_real_

  bp <- m2_breusch_pagan(fitted, residuals)

  breakpoints <- if (length(residuals) >= 10 && length(years) == length(residuals)) {
    tryCatch(detect_residual_breakpoints(years, residuals), error = function(e) NULL)
  } else {
    NULL
  }
  breakpoint_years <- if (!is.null(breakpoints)) m2_num_vec(breakpoints$breakpoint_years) else numeric(0)
  breakpoint_count <- length(breakpoint_years)

  score_parts <- c(
    if (is.finite(shapiro_p)) min(1, shapiro_p / 0.05) else NA_real_,
    if (is.finite(dw)) 1 - min(1, abs(dw - 2) / 2) else NA_real_,
    if (is.finite(ljung_p)) min(1, ljung_p / 0.05) else NA_real_,
    if (is.finite(bp$p_value)) min(1, bp$p_value / 0.05) else NA_real_,
    if (is.finite(max_abs_acf)) 1 - min(1, max_abs_acf) else NA_real_,
    1 - min(1, breakpoint_count / 3)
  )
  stability_score <- if (all(is.na(score_parts))) NA_real_ else mean(score_parts, na.rm = TRUE)

  list(
    shapiro_p = shapiro_p,
    durbin_watson = dw,
    ljung_box_p = ljung_p,
    breusch_pagan_p = bp$p_value,
    breakpoint_count = breakpoint_count,
    breakpoint_years = breakpoint_years,
    max_abs_acf = max_abs_acf,
    stability_score = stability_score
  )
}

#' Dense curve payload for plotting
#' @keywords internal
m2_build_curve_payload <- function(model_object, years, fitted, residuals) {
  years <- m2_num_vec(years)
  fitted <- m2_num_vec(fitted)
  residuals <- m2_num_vec(residuals)

  observed <- data.frame(
    Year = years,
    Fitted = fitted,
    Lower = fitted - 1.96 * stats::sd(residuals, na.rm = TRUE),
    Upper = fitted + 1.96 * stats::sd(residuals, na.rm = TRUE)
  )

  dense <- observed
  if (!is.null(model_object) && length(years) >= 2) {
    year_seq <- seq(min(years, na.rm = TRUE), max(years, na.rm = TRUE), length.out = max(100L, length(years)))
    pred_vals <- tryCatch(
      suppressWarnings(predict(model_object, newdata = data.frame(Year = year_seq))),
      error = function(e) rep(NA_real_, length(year_seq))
    )

    if (any(is.finite(pred_vals))) {
      se_vals <- tryCatch({
        pred_obj <- suppressWarnings(predict(model_object, newdata = data.frame(Year = year_seq), se.fit = TRUE))
        if (is.list(pred_obj) && "se.fit" %in% names(pred_obj)) {
          as.numeric(pred_obj$se.fit)
        } else {
          rep(stats::sd(residuals, na.rm = TRUE), length(year_seq))
        }
      }, error = function(e) rep(stats::sd(residuals, na.rm = TRUE), length(year_seq)))

      dense <- data.frame(
        Year = year_seq,
        Fitted = m2_num_vec(pred_vals),
        Lower = m2_num_vec(pred_vals) - 1.96 * se_vals,
        Upper = m2_num_vec(pred_vals) + 1.96 * se_vals
      )
    }
  }

  list(observed = observed, dense = dense)
}

#' Forecast payload helper
#' @keywords internal
m2_build_forecast_payload <- function(point = numeric(0),
                                      years = numeric(0),
                                      lower = NULL,
                                      upper = NULL,
                                      method = NA_character_) {
  list(
    point = m2_num_vec(point),
    years = m2_num_vec(years),
    lower = m2_num_vec(lower),
    upper = m2_num_vec(upper),
    method = method
  )
}

#' Approximate future forecast from a fitted regression object
#' @keywords internal
m2_forecast_from_regression_model <- function(model_object, years, residuals, horizon = 5L) {
  years <- m2_num_vec(years)
  residuals <- residuals[is.finite(residuals)]
  if (is.null(model_object) || length(years) == 0 || horizon <= 0) {
    return(m2_build_forecast_payload())
  }

  future_years <- seq(max(years, na.rm = TRUE) + 1, by = 1, length.out = horizon)
  preds <- tryCatch(
    suppressWarnings(predict(model_object, newdata = data.frame(Year = future_years))),
    error = function(e) rep(NA_real_, horizon)
  )
  preds <- m2_num_vec(preds)

  sigma <- stats::sd(residuals, na.rm = TRUE)
  sigma <- if (is.finite(sigma)) sigma else 0
  spread <- sigma * sqrt(seq_len(horizon))

  m2_build_forecast_payload(
    point = preds,
    years = future_years,
    lower = preds - 1.96 * spread,
    upper = preds + 1.96 * spread,
    method = "regression_extrapolation"
  )
}

#' Build canonical model card
#' @keywords internal
m2_new_model_card <- function(name,
                              family,
                              years,
                              actual,
                              fitted,
                              model_object = NULL,
                              forecast = NULL,
                              equation = NA_character_,
                              source = "regression",
                              target = "annual",
                              metadata = list()) {
  years <- m2_num_vec(years)
  actual <- m2_num_vec(actual)
  fitted <- m2_num_vec(fitted)

  keep <- is.finite(years) & is.finite(actual) & is.finite(fitted)
  years <- years[keep]
  actual <- actual[keep]
  fitted <- fitted[keep]
  residuals <- actual - fitted

  n_params <- m2_scalar_num(
    m2_coalesce_null(metadata$n_params, tryCatch(length(stats::coef(model_object)), error = function(e) NA_real_)),
    default = 1
  )

  metrics <- m2_compute_fit_metrics(actual, fitted, n_params = n_params)
  info <- m2_information_criteria(model_object, residuals, n_params = n_params, n_obs = length(actual))
  if (!is.null(metadata$AIC)) {
    info$AIC <- m2_scalar_num(metadata$AIC, info$AIC)
  }
  if (!is.null(metadata$BIC)) {
    info$BIC <- m2_scalar_num(metadata$BIC, info$BIC)
  }
  if (!is.null(metadata$logLik)) {
    info$logLik <- m2_scalar_num(metadata$logLik, info$logLik)
  }
  diag <- m2_compute_card_diagnostics(years, actual, fitted, residuals, n_params = n_params)
  curves <- m2_build_curve_payload(model_object, years, fitted, residuals)

  if (is.null(forecast)) {
    forecast <- m2_build_forecast_payload()
  }

  list(
    name = name,
    family = family,
    source = source,
    target = target,
    status = "success",
    class = if (is.null(model_object)) NA_character_ else class(model_object)[1],
    coefficients = if (is.null(model_object)) list() else tryCatch(as.list(stats::coef(model_object)), error = function(e) list()),
    equation = equation,
    n_params = n_params,
    years = years,
    actual = actual,
    fitted = fitted,
    residuals = residuals,
    metrics = metrics,
    diagnostics = diag,
    AIC = info$AIC,
    BIC = info$BIC,
    logLik = info$logLik,
    curve = curves,
    forecast = forecast,
    metadata = metadata
  )
}

#' Interpretability profile for regression models
#' @keywords internal
m2_model_interpretability_profile <- function(model_name, family = NA_character_) {
  key <- tolower(model_name %||% "")
  fam <- tolower(family %||% "")

  if (key %in% c("logistic", "logistic4p", "gompertz", "gompertzoffset", "richards", "weibull", "mmf", "monod", "asymptotic", "vonbertalanffy", "chapmanrichards", "korf", "hossfeld", "hill", "stannard", "baranyi")) {
    return(list(score = 1.00, headline_eligible = TRUE, saturation_capable = TRUE, benchmark_only = FALSE))
  }
  if (key %in% c("exponential", "exponentialoffset", "power", "logarithmic", "linear", "quadratic")) {
    return(list(score = 0.90, headline_eligible = TRUE, saturation_capable = FALSE, benchmark_only = FALSE))
  }
  if (key %in% c("cubic", "beta", "ratkowsky", "gaussian", "fourier")) {
    return(list(score = 0.65, headline_eligible = FALSE, saturation_capable = FALSE, benchmark_only = FALSE))
  }
  if (fam %in% c("spline", "periodic")) {
    return(list(score = 0.20, headline_eligible = FALSE, saturation_capable = FALSE, benchmark_only = TRUE))
  }

  list(score = 0.50, headline_eligible = FALSE, saturation_capable = FALSE, benchmark_only = FALSE)
}

#' Convert canonical card to comparison row
#' @keywords internal
m2_model_card_to_row <- function(card) {
  profile <- m2_model_interpretability_profile(card$name, card$family)
  data.frame(
    Model = card$name,
    Family = card$family,
    Source = card$source,
    Target = card$target,
    R2 = round(card$metrics$r_squared, 6),
    Adj_R2 = round(card$metrics$adj_r_squared, 6),
    RMSE = round(card$metrics$rmse, 4),
    MAE = round(card$metrics$mae, 4),
    MAPE = round(card$metrics$mape, 2),
    SMAPE = round(card$metrics$smape, 2),
    AIC = round(card$AIC, 4),
    BIC = round(card$BIC, 4),
    N_params = round(card$n_params, 0),
    Shapiro_P = round(card$diagnostics$shapiro_p, 4),
    DW = round(card$diagnostics$durbin_watson, 4),
    LjungBox_P = round(card$diagnostics$ljung_box_p, 4),
    BP_P = round(card$diagnostics$breusch_pagan_p, 4),
    Breakpoints = round(card$diagnostics$breakpoint_count, 0),
    Stability = round(card$diagnostics$stability_score, 4),
    TheilU = round(card$metrics$theil_u, 4),
    Interpretability = round(profile$score, 4),
    HeadlineEligible = isTRUE(profile$headline_eligible),
    SaturationCapable = isTRUE(profile$saturation_capable),
    BenchmarkOnly = isTRUE(profile$benchmark_only),
    Equation = m2_coalesce_null(card$equation, NA_character_),
    stringsAsFactors = FALSE
  )
}

#' Normalize a metric so that larger is always better
#' @keywords internal
m2_normalize_metric <- function(x, higher_is_better = TRUE) {
  x <- m2_num_vec(x)
  valid <- is.finite(x)
  out <- rep(NA_real_, length(x))
  if (!any(valid)) {
    return(out)
  }

  vals <- x[valid]
  if (!higher_is_better) {
    vals <- -vals
  }

  rng <- range(vals, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) <= .Machine$double.eps) {
    out[valid] <- 1
    return(out)
  }

  out[valid] <- (vals - rng[1]) / (rng[2] - rng[1])
  out
}

#' Apply weighted model scoring to a comparison table
#' @keywords internal
m2_score_model_table <- function(comparison, weights = NULL) {
  if (!is.data.frame(comparison) || nrow(comparison) == 0) {
    return(comparison)
  }

  defaults <- c(
    Adj_R2 = 0.20,
    RMSE = 0.15,
    MAE = 0.10,
    SMAPE = 0.10,
    AIC = 0.10,
    BIC = 0.10,
    Stability = 0.15,
    LjungBox_P = 0.05,
    BP_P = 0.05
  )
  if (!is.null(weights) && length(weights) > 0) {
    defaults[names(weights)] <- as.numeric(weights)
  }

  scored <- comparison
  metric_map <- list(
    Adj_R2 = TRUE,
    RMSE = FALSE,
    MAE = FALSE,
    SMAPE = FALSE,
    AIC = FALSE,
    BIC = FALSE,
    Stability = TRUE,
    LjungBox_P = TRUE,
    BP_P = TRUE
  )

  weighted_parts <- list()
  used_weights <- numeric(0)

  for (metric in names(defaults)) {
    if (!metric %in% names(scored)) {
      next
    }
    col_name <- paste0(metric, "_Score")
    scored[[col_name]] <- m2_normalize_metric(scored[[metric]], higher_is_better = metric_map[[metric]])
    weighted_parts[[metric]] <- scored[[col_name]] * defaults[[metric]]
    used_weights <- c(used_weights, defaults[[metric]])
  }

  if (length(weighted_parts) == 0) {
    scored$CompositeScore <- NA_real_
    scored$Rank <- seq_len(nrow(scored))
    return(scored)
  }

  weighted_matrix <- do.call(cbind, weighted_parts)
  if (is.null(dim(weighted_matrix))) {
    weighted_matrix <- matrix(weighted_matrix, ncol = 1L)
  }
  available_matrix <- !is.na(weighted_matrix)
  weight_vector <- used_weights[seq_len(ncol(weighted_matrix))]
  available_weight_sum <- rowSums(t(t(available_matrix) * weight_vector), na.rm = TRUE)
  weighted_sum <- rowSums(weighted_matrix, na.rm = TRUE)
  scored$CompositeScore <- ifelse(available_weight_sum > 0, weighted_sum / available_weight_sum, NA_real_)
  scored$Rank <- rank(-scored$CompositeScore, ties.method = "first", na.last = "keep")
  if ("Interpretability" %in% names(scored)) {
    scored$NarrativeScore <- (0.82 * scored$CompositeScore) + (0.18 * scored$Interpretability)
    scored$NarrativeRank <- rank(-scored$NarrativeScore, ties.method = "first", na.last = "keep")
  }
  scored[order(scored$Rank, scored$RMSE), , drop = FALSE]
}

#' Extract residual diagnostics in renderer-friendly shape
#' @keywords internal
m2_card_residual_payload <- function(card) {
  residuals <- m2_num_vec(card$residuals)
  if (length(residuals) == 0) {
    return(NULL)
  }

  lag_max <- min(10L, length(residuals) - 1L)
  acf_obj <- if (lag_max >= 1L) {
    tryCatch(stats::acf(residuals, plot = FALSE, lag.max = lag_max), error = function(e) NULL)
  } else {
    NULL
  }
  pacf_obj <- if (lag_max >= 1L) {
    tryCatch(stats::pacf(residuals, plot = FALSE, lag.max = lag_max), error = function(e) NULL)
  } else {
    NULL
  }

  list(
    residuals = residuals,
    shapiro_wilk = list(p_value = card$diagnostics$shapiro_p),
    durbin_watson = list(statistic = card$diagnostics$durbin_watson),
    breusch_pagan = list(p_value = card$diagnostics$breusch_pagan_p),
    acf = list(
      lags = if (is.null(acf_obj)) numeric(0) else as.numeric(acf_obj$lag[-1]),
      values = if (is.null(acf_obj)) numeric(0) else as.numeric(acf_obj$acf[-1])
    ),
    pacf = list(
      lags = if (is.null(pacf_obj)) numeric(0) else seq_along(as.numeric(pacf_obj$acf)),
      values = if (is.null(pacf_obj)) numeric(0) else as.numeric(pacf_obj$acf)
    )
  )
}
