# ============================================================================
# m2_compute_regression.R - Comprehensive Regression Analysis for IEEE Q1
# ============================================================================
# Fits 30+ growth models and selects best fit
# Includes linear, polynomial, exponential, sigmoid, saturation, and advanced models
# ============================================================================

#' Compute comprehensive regression analysis
#'
#' Fits multiple growth models and selects the best based on R², AIC, BIC
#'
#' @param input Data frame with Year and Articles columns
#' @param config Configuration list
#' @return List with comparison table, best model, and diagnostics
#' @export
compute_m2_regression <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) < 5) {
    return(structure(list(
      status = "error",
      comparison_table = data.frame(),
      best_model = list(),
      models = list(),
      performance = list()
    ), raw_models = list()))
  }
  
  years <- as.numeric(input$Year)
  articles <- as.numeric(input$Articles)
  data <- data.frame(Year = years, Articles = articles)
  
  raw_models <- m2_fit_all_models(data)
  
  if (length(raw_models) == 0) {
    return(structure(list(
      status = "error: no models converged",
      comparison_table = data.frame(),
      best_model = list(name = "none", R2 = 0),
      models = list(),
      performance = list()
    ), raw_models = list()))
  }

  horizon <- as.integer(m2_scalar_num(config$forecast_horizon, default = 5))
  model_cards <- lapply(names(raw_models), function(model_name) {
    m2_regression_model_to_card(model_name, raw_models[[model_name]], data, horizon = horizon)
  })
  names(model_cards) <- names(raw_models)
  model_cards <- Filter(Negate(is.null), model_cards)

  comparison <- m2_build_comparison_table(model_cards, articles)
  scored <- m2_score_model_table(comparison, weights = config[["m2_selection_weights"]])
  benchmark <- m2_select_best_model(
    scored,
    model_cards,
    years,
    articles,
    config = config,
    prefer_interpretable = FALSE
  )
  best <- m2_select_best_model(
    scored,
    model_cards,
    years,
    articles,
    config = config,
    prefer_interpretable = TRUE
  )
  perf <- m2_analyze_performance(best)

  result <- list(
    status = "success",
    comparison_table = scored,
    best_model = best,
    benchmark_best_model = benchmark,
    linear = if ("Linear" %in% names(model_cards)) model_cards$Linear else NULL,
    models = model_cards,
    performance = perf,
    data = data,
    family_summary = m2_summarize_regression_families(model_cards)
  )
  attr(result, "raw_models") <- raw_models
  result
}

#' Fit all regression models with robust parameter estimation
#' @keywords internal
m2_fit_all_models <- function(data) {
  suppressWarnings({
    models <- list()
    
    # === LINEAR AND POLYNOMIAL MODELS ===
    
    models$Linear <- tryCatch(
      lm(Articles ~ Year, data = data),
      error = function(e) NULL
    )
    
    models$Quadratic <- tryCatch(
      lm(Articles ~ Year + I(Year^2), data = data),
      error = function(e) NULL
    )
    
    models$Cubic <- tryCatch(
      lm(Articles ~ Year + I(Year^2) + I(Year^3), data = data),
      error = function(e) NULL
    )
    
    # === LOGARITHMIC AND POWER MODELS ===
    
    # Logarithmic model requires Year > 0
    if (all(data$Year > 0)) {
      models$Logarithmic <- tryCatch(
        lm(Articles ~ log(Year), data = data),
        error = function(e) NULL
      )
    } else {
      models$Logarithmic <- NULL
    }
    
    sv <- m2_get_start_values("Power", data)
    models$Power <- tryCatch(
      nls(Articles ~ m2_power(Year, a, b), data = data,
          start = list(a = sv$a, b = sv$b)),
      error = function(e) NULL
    )
    
    # === EXPONENTIAL MODELS ===
    
    sv <- m2_get_start_values("Exponential", data)
    models$Exponential <- tryCatch(
      nls(Articles ~ m2_exponential(Year, r, N0, t0), data = data,
          start = list(r = sv$r, N0 = sv$N0, t0 = sv$t0)),
      error = function(e) NULL
    )
    
    sv <- m2_get_start_values("ExponentialOffset", data)
    models$ExponentialOffset <- tryCatch(
      nls(Articles ~ m2_exponential_offset(Year, a, b, r), data = data,
          start = list(a = sv$a, b = sv$b, r = sv$r)),
      error = function(e) NULL
    )
    
    # === SIGMOID MODELS ===
    
    sv <- m2_get_start_values("Logistic", data)
    models$Logistic <- tryCatch(
      nls(Articles ~ m2_logistic(Year, K, r, t0), data = data,
          start = list(K = sv$K, r = sv$r, t0 = sv$t0),
          control = nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL
    )
    
    sv <- m2_get_start_values("Logistic4P", data)
    models$Logistic4P <- tryCatch(
      nls(Articles ~ m2_logistic_4param(Year, a, K, r, t0), data = data,
          start = list(a = sv$a, K = sv$K, r = sv$r, t0 = sv$t0),
          control = nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL
    )
    
    sv <- m2_get_start_values("Gompertz", data)
    models$Gompertz <- tryCatch(
      nls(Articles ~ m2_gompertz(Year, K, r, t0), data = data,
          start = list(K = sv$K, r = sv$r, t0 = sv$t0),
          control = nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL
    )
    
    sv <- m2_get_start_values("GompertzOffset", data)
    models$GompertzOffset <- tryCatch(
      nls(Articles ~ m2_gompertz_offset(Year, a, K, r, t0), data = data,
          start = list(a = sv$a, K = sv$K, r = sv$r, t0 = sv$t0),
          control = nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL
    )
    
    sv <- m2_get_start_values("Richards", data)
    models$Richards <- tryCatch(
      nls(Articles ~ m2_richards(Year, K, r, t0, v), data = data,
          start = list(K = sv$K, r = sv$r, t0 = sv$t0, v = sv$v),
          control = nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL
    )
    
    sv <- m2_get_start_values("Weibull", data)
    models$Weibull <- tryCatch(
      nls(Articles ~ m2_weibull(Year, K, r, t0, alpha), data = data,
          start = list(K = sv$K, r = sv$r, t0 = sv$t0, alpha = sv$alpha),
          control = nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL
    )
    
    # === BIOLOGICAL GROWTH MODELS ===
    
    sv <- m2_get_start_values("VonBertalanffy", data)
    models$VonBertalanffy <- tryCatch(
      nls(Articles ~ m2_vonbertalanffy(Year, Linf, k, t0), data = data,
          start = list(Linf = sv$Linf, k = sv$k, t0 = sv$t0),
          control = nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL
    )
    
    sv <- m2_get_start_values("Beta", data)
    models$Beta <- tryCatch(
      nls(Articles ~ m2_beta(Year, K, alpha, beta, t0), data = data,
          start = list(K = sv$K, alpha = sv$alpha, beta = sv$beta, t0 = sv$t0),
          control = nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL
    )
    
    sv <- m2_get_start_values("ChapmanRichards", data)
    models$ChapmanRichards <- tryCatch(
      nls(Articles ~ m2_chapman_richards(Year, K, r, t0, m), data = data,
          start = list(K = sv$K, r = sv$r, t0 = sv$t0, m = sv$m),
          control = nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL
    )
    
    sv <- m2_get_start_values("Korf", data)
    models$Korf <- tryCatch(
      nls(Articles ~ m2_korf(Year, K, r, beta), data = data,
          start = list(K = sv$K, r = sv$r, beta = sv$beta),
          control = nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL
    )
    
    # === SATURATION MODELS ===
    
    sv <- m2_get_start_values("MMF", data)
    models$MMF <- tryCatch(
      nls(Articles ~ m2_mmf(Year, K, a, b, d), data = data,
          start = list(K = sv$K, a = sv$a, b = sv$b, d = sv$d),
          control = nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL
    )
    
    sv <- m2_get_start_values("Stannard", data)
    models$Stannard <- tryCatch(
      nls(Articles ~ m2_stannard(Year, K, a, b), data = data,
          start = list(K = sv$K, a = sv$a, b = sv$b),
          control = nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL
    )
    
    sv <- m2_get_start_values("Hill", data)
    models$Hill <- tryCatch(
      nls(Articles ~ m2_hill(Year, K, K50, n), data = data,
          start = list(K = sv$K, K50 = sv$K50, n = sv$n),
          control = nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL
    )
    
    sv <- m2_get_start_values("Monod", data)
    models$Monod <- tryCatch(
      nls(Articles ~ m2_monod(Year, K, Ks), data = data,
          start = list(K = sv$K, Ks = sv$Ks),
          control = nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL
    )
    
    sv <- m2_get_start_values("Baranyi", data)
    models$Baranyi <- tryCatch(
      nls(Articles ~ m2_baranyi(Year, y0, y_max, h0, lambda), data = data,
          start = list(y0 = sv$y0, y_max = sv$y_max, h0 = sv$h0, lambda = sv$lambda),
          control = nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL
    )
    
    sv <- m2_get_start_values("Ratkowsky", data)
    models$Ratkowsky <- tryCatch(
      nls(Articles ~ m2_ratkowsky(Year, a, b, t0, c), data = data,
          start = list(a = sv$a, b = sv$b, t0 = sv$t0, c = sv$c),
          control = nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL
    )
    
    sv <- m2_get_start_values("Hossfeld", data)
    models$Hossfeld <- tryCatch(
      nls(Articles ~ m2_hossfeld(Year, K, a, b), data = data,
          start = list(K = sv$K, a = sv$a, b = sv$b),
          control = nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL
    )
    
    sv <- m2_get_start_values("Asymptotic", data)
    models$Asymptotic <- tryCatch(
      nls(Articles ~ m2_asymptotic(Year, a, b, c), data = data,
          start = list(a = sv$a, b = sv$b, c = sv$c)),
      error = function(e) NULL
    )
    
    # === PERIODIC MODELS ===
    
    sv <- m2_get_start_values("Fourier", data)
    models$Fourier <- tryCatch(
      nls(Articles ~ m2_fourier(Year, a0, a1, b1, w), data = data,
          start = list(a0 = sv$a0, a1 = sv$a1, b1 = sv$b1, w = sv$w)),
      error = function(e) NULL
    )
    
    sv <- m2_get_start_values("Gaussian", data)
    models$Gaussian <- tryCatch(
      nls(Articles ~ m2_gaussian(Year, A, mu, sigma), data = data,
          start = list(A = sv$A, mu = sv$mu, sigma = sv$sigma)),
      error = function(e) NULL
    )
    
    # === SPLINE MODELS ===
    
    models$Spline <- tryCatch(
      lm(Articles ~ splines::bs(Year, df = min(5, nrow(data) - 1)), data = data),
      error = function(e) NULL
    )
    
    models$Spline3 <- tryCatch(
      lm(Articles ~ splines::bs(Year, df = 3), data = data),
      error = function(e) NULL
    )
    
    Filter(Negate(is.null), models)
  })
}

#' Build comparison table with all metrics
#' @keywords internal
m2_build_comparison_table <- function(models, y) {
  if (!is.list(models) || length(models) == 0) {
    return(data.frame())
  }

  rows <- lapply(models, m2_model_card_to_row)
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame())
  }

  results <- do.call(rbind, rows)
  results[order(-results$Adj_R2, results$RMSE), , drop = FALSE]
}

#' Select best model using information criteria
#' @keywords internal
m2_select_best_model <- function(comparison,
                                 models = NULL,
                                 years = NULL,
                                 articles = NULL,
                                 config = biblio_config(),
                                 prefer_interpretable = TRUE) {
  if (nrow(comparison) == 0) {
    return(list(name = "none", R2 = 0, Adj_R2 = 0, RMSE = NA, equation = "", predictions = NULL))
  }

  ordered <- comparison[order(comparison$Rank, -comparison$CompositeScore, comparison$RMSE), , drop = FALSE]
  benchmark_row <- ordered[1, , drop = FALSE]
  best <- benchmark_row

  if (isTRUE(prefer_interpretable) && "HeadlineEligible" %in% names(ordered)) {
    tolerance <- m2_scalar_num(config$m2_interpretability_tolerance, default = 0.08)
    rmse_relax <- m2_scalar_num(config$m2_interpretability_rmse_relax, default = 1.35)
    r2_relax <- m2_scalar_num(config$m2_interpretability_r2_relax, default = 0.08)

    eligible <- ordered[ordered$HeadlineEligible %in% TRUE, , drop = FALSE]
    if (nrow(eligible) > 0) {
      near_best <- eligible[
        eligible$CompositeScore >= (benchmark_row$CompositeScore[1] - tolerance) &
          eligible$Adj_R2 >= (benchmark_row$Adj_R2[1] - r2_relax) &
          eligible$RMSE <= (benchmark_row$RMSE[1] * rmse_relax),
        ,
        drop = FALSE
      ]
      if (nrow(near_best) == 0 && isTRUE(benchmark_row$BenchmarkOnly[1])) {
        near_best <- eligible
      }
      if (nrow(near_best) > 0) {
        score_col <- if ("NarrativeScore" %in% names(near_best)) "NarrativeScore" else "CompositeScore"
        best <- near_best[order(-near_best[[score_col]], -near_best$Adj_R2, near_best$RMSE), , drop = FALSE][1, , drop = FALSE]
      }
    }
  }

  model_name <- as.character(best$Model[1])
  card <- if (!is.null(models) && model_name %in% names(models)) models[[model_name]] else NULL
  predictions <- if (!is.null(card)) card$fitted else NULL
  selection_reason <- m2_build_selection_reason(best, benchmark_row, prefer_interpretable)
  parameter_summary <- if (!is.null(card)) m2_summarize_model_parameters(card) else list()

  list(
    name = model_name,
    family = if (!is.null(card)) card$family else NA_character_,
    source = if (!is.null(card)) card$source else "regression",
    R2 = m2_scalar_num(best$R2[1]),
    Adj_R2 = m2_scalar_num(best$Adj_R2[1]),
    RMSE = m2_scalar_num(best$RMSE[1]),
    MAE = m2_scalar_num(best$MAE[1]),
    MAPE = m2_scalar_num(best$MAPE[1]),
    SMAPE = m2_scalar_num(best$SMAPE[1]),
    AIC = m2_scalar_num(best$AIC[1]),
    BIC = m2_scalar_num(best$BIC[1]),
    equation = as.character(best$Equation[1]),
    n_params = m2_scalar_num(best$N_params[1]),
    composite_score = m2_scalar_num(best$CompositeScore[1]),
    rank = m2_scalar_num(best$Rank[1]),
    narrative_score = m2_scalar_num(best$NarrativeScore[1]),
    predictions = predictions,
    years = if (!is.null(card)) card$years else m2_num_vec(years),
    actual = if (!is.null(card)) card$actual else m2_num_vec(articles),
    residuals = if (!is.null(card)) card$residuals else NULL,
    diagnostics = if (!is.null(card)) card$diagnostics else list(),
    forecast = if (!is.null(card)) card$forecast else m2_build_forecast_payload(),
    curve = if (!is.null(card)) card$curve else list(),
    benchmark_name = as.character(benchmark_row$Model[1]),
    benchmark_score = m2_scalar_num(benchmark_row$CompositeScore[1]),
    parameter_summary = parameter_summary,
    selection_reason = selection_reason
  )
}

#' Get equation string for display
#' @keywords internal
m2_get_equation_string <- function(model_name, coefs) {
  eq <- switch(tolower(model_name),
    "linear" = sprintf("y = %.2f + %.4f·t", coefs[1], coefs[2]),
    "quadratic" = sprintf("y = %.2f + %.4f·t + %.6f·t²", coefs[1], coefs[2], coefs[3]),
    "cubic" = "y = a + bt + ct² + dt³",
    "logarithmic" = sprintf("y = %.2f + %.2f·ln(t)", coefs[2], coefs[1]),
    "power" = sprintf("y = %.2f·t^{%.3f}", coefs[1], coefs[2]),
    "exponential" = "y = N₀·e^{r(t-t₀)}",
    "logistic" = "y = K / (1 + e^{-r(t-t₀)})",
    "gompertz" = "y = K·e^{-e^{-r(t-t₀)}}",
    "richards" = "y = K / (1 + ν·e^{-r(t-t₀)})^{1/ν}",
    "weibull" = "y = K·(1 - e^{-((t-t₀)/α)^r})",
    "vonbertalanffy" = "y = L_∞·(1 - e^{-k(t-t₀)})",
    "beta" = "y = K·(1 + (t-t₀)/α)^{-β}",
    "mmf" = "y = (ab + Kt^d)/(b + t^d)",
    "stannard" = "y = K / (1 + e^{-a-b·ln(t)})",
    "monod" = "y = Kt/(Ks + t)",
    "baranyi" = "y = y₀ + (y_max - y₀)·(1 - e^{-...})",
    "fourier" = "y = a₀ + a₁cos(ωt) + b₁sin(ωt)",
    "gaussian" = "y = A·e^{-(t-μ)²/(2σ²)}",
    "spline" = "y = Σ βᵢBᵢ(t)",
    model_name
  )
  
  eq
}

#' Get equation string for display
#' @keywords internal
m2_get_equation_string <- function(model_name, coefs) {
  switch(tolower(model_name),
    "linear" = sprintf("y = %.2f + %.4f*t", coefs[1], coefs[2]),
    "quadratic" = sprintf("y = %.2f + %.4f*t + %.6f*t^2", coefs[1], coefs[2], coefs[3]),
    "cubic" = "y = a + b*t + c*t^2 + d*t^3",
    "logarithmic" = sprintf("y = %.2f + %.2f*ln(t)", coefs[2], coefs[1]),
    "power" = sprintf("y = %.2f*t^(%.3f)", coefs[1], coefs[2]),
    "exponential" = "y = N0*exp(r*(t - t0))",
    "exponentialoffset" = "y = a + b*exp(r*t)",
    "logistic" = "y = K / (1 + exp(-r*(t - t0)))",
    "logistic4p" = "y = a + K / (1 + exp(-r*(t - t0)))",
    "gompertz" = "y = K*exp(-exp(-r*(t - t0)))",
    "gompertzoffset" = "y = a + K*exp(-exp(-r*(t - t0)))",
    "richards" = "y = K / (1 + v*exp(-r*(t - t0)))^(1/v)",
    "weibull" = "y = K*(1 - exp(-((t - t0)/alpha)^r))",
    "vonbertalanffy" = "y = Linf*(1 - exp(-k*(t - t0)))",
    "beta" = "y = K*(1 + (t - t0)/alpha)^(-beta)",
    "mmf" = "y = (a*b + K*t^d) / (b + t^d)",
    "stannard" = "y = K / (1 + exp(-a - b*ln(t)))",
    "monod" = "y = (K*t) / (Ks + t)",
    "baranyi" = "y = y0 + (ymax - y0)*(1 - exp(-g(t)))",
    "fourier" = "y = a0 + a1*cos(w*t) + b1*sin(w*t)",
    "gaussian" = "y = A*exp(-((t - mu)^2)/(2*sigma^2))",
    "spline" = "Flexible spline benchmark",
    "spline3" = "Flexible spline benchmark",
    model_name
  )
}

#' Build a readable selection reason
#' @keywords internal
m2_build_selection_reason <- function(selected_row, benchmark_row, prefer_interpretable = TRUE) {
  if (!isTRUE(prefer_interpretable) || identical(as.character(selected_row$Model[1]), as.character(benchmark_row$Model[1]))) {
    return(sprintf(
      "Selected by composite score %.4f with stability %.4f and adjusted R2 %.4f.",
      m2_scalar_num(selected_row$CompositeScore[1]),
      m2_scalar_num(selected_row$Stability[1]),
      m2_scalar_num(selected_row$Adj_R2[1])
    ))
  }

  sprintf(
    "Selected %s as the main interpretable model. Flexible benchmark %s scored %.4f, while the selected model scored %.4f with adjusted R2 %.4f.",
    as.character(selected_row$Model[1]),
    as.character(benchmark_row$Model[1]),
    m2_scalar_num(benchmark_row$CompositeScore[1]),
    m2_scalar_num(selected_row$CompositeScore[1]),
    m2_scalar_num(selected_row$Adj_R2[1])
  )
}

#' Extract explainable parameters for narrative/reporting
#' @keywords internal
m2_summarize_model_parameters <- function(card) {
  coeffs <- card$coefficients %||% list()
  if (length(coeffs) == 0) {
    return(list())
  }

  coeffs_num <- suppressWarnings(vapply(coeffs, as.numeric, numeric(1)))
  names(coeffs_num) <- names(coeffs)

  pick_param <- function(keys) {
    idx <- match(tolower(keys), tolower(names(coeffs_num)))
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0) {
      return(NA_real_)
    }
    coeffs_num[idx[1]]
  }

  list(
    carrying_capacity = pick_param(c("K", "Linf", "y_max", "a")),
    growth_rate = pick_param(c("r", "k", "b")),
    inflection_year = pick_param(c("t0", "K50", "lambda")),
    shape = pick_param(c("alpha", "beta", "v", "d", "n")),
    coefficients = coeffs_num
  )
}

#' Analyze model performance and residuals
#' @keywords internal
m2_analyze_performance <- function(best) {
  if (is.null(best) || identical(best$name, "none") || is.null(best$residuals)) {
    return(list())
  }

  residuals <- m2_num_vec(best$residuals)
  actual <- m2_num_vec(best$actual)
  fitted <- m2_num_vec(best$predictions)
  n <- min(length(actual), length(fitted), length(residuals))
  if (n == 0) {
    return(list())
  }

  residuals <- residuals[seq_len(n)]
  actual <- actual[seq_len(n)]
  fitted <- fitted[seq_len(n)]
  std_resid <- if (stats::sd(residuals, na.rm = TRUE) > 0) residuals / stats::sd(residuals, na.rm = TRUE) else residuals

  acf_vals <- tryCatch(
    stats::acf(residuals, plot = FALSE, lag.max = min(10L, n - 1L))$acf[-1],
    error = function(e) numeric(0)
  )

  jb_stat <- tryCatch({
    sigma <- sqrt(mean((residuals - mean(residuals, na.rm = TRUE))^2, na.rm = TRUE))
    if (!is.finite(sigma) || sigma <= .Machine$double.eps) {
      return(NA_real_)
    }
    skewness <- mean((residuals - mean(residuals, na.rm = TRUE))^3, na.rm = TRUE) / sigma^3
    kurtosis <- mean((residuals - mean(residuals, na.rm = TRUE))^4, na.rm = TRUE) / sigma^4 - 3
    n * (skewness^2 / 6 + kurtosis^2 / 24)
  }, error = function(e) NA_real_)

  list(
    residuals = round(residuals, 4),
    std_residuals = round(std_resid, 4),
    acf = round(acf_vals, 4),
    shapiro_p = round(m2_scalar_num(best$diagnostics$shapiro_p), 4),
    dw_statistic = round(m2_scalar_num(best$diagnostics$durbin_watson), 4),
    ljung_box_p = round(m2_scalar_num(best$diagnostics$ljung_box_p), 4),
    breusch_pagan_p = round(m2_scalar_num(best$diagnostics$breusch_pagan_p), 4),
    jarque_bera = round(jb_stat, 4),
    breakpoint_years = m2_num_vec(best$diagnostics$breakpoint_years),
    stability_score = round(m2_scalar_num(best$diagnostics$stability_score), 4),
    n_obs = n
  )
}

#' Serialize model for storage
#' @keywords internal
m2_serialize_model <- function(mod) {
  if (is.list(mod) && all(c("name", "metrics", "diagnostics") %in% names(mod))) {
    return(mod)
  }

  r_squared <- tryCatch(suppressWarnings(summary(mod)$r.squared), error = function(e) NA_real_)
  adj_r_squared <- tryCatch(suppressWarnings(summary(mod)$adj.r.squared), error = function(e) NA_real_)
  if (length(r_squared) != 1L || !is.finite(r_squared)) r_squared <- NA_real_
  if (length(adj_r_squared) != 1L || !is.finite(adj_r_squared)) adj_r_squared <- NA_real_

  list(
    class = class(mod)[1],
    coefficients = tryCatch(as.list(coef(mod)), error = function(e) list()),
    r_squared = if (is.finite(r_squared)) max(0, min(1, r_squared)) else NA_real_,
    adj_r_squared = if (is.finite(adj_r_squared)) max(0, min(1, adj_r_squared)) else NA_real_
  )
}

#' Canonicalize a fitted regression model into a model card
#' @keywords internal
m2_regression_model_to_card <- function(model_name, mod, data, horizon = 5L) {
  preds <- tryCatch(suppressWarnings(stats::predict(mod, newdata = data)), error = function(e) NULL)
  if (is.null(preds) || length(preds) != nrow(data) || any(!is.finite(preds))) {
    return(NULL)
  }

  residuals <- data$Articles - preds
  family <- m2_classify_regression_family(model_name)
  equation <- tryCatch(m2_get_equation_string(model_name, stats::coef(mod)), error = function(e) model_name)
  forecast <- m2_forecast_from_regression_model(mod, data$Year, residuals, horizon = horizon)

  m2_new_model_card(
    name = model_name,
    family = family,
    years = data$Year,
    actual = data$Articles,
    fitted = preds,
    model_object = mod,
    forecast = forecast,
    equation = equation,
    source = "regression",
    target = "annual",
    metadata = list(
      n_params = tryCatch(length(stats::coef(mod)), error = function(e) NA_integer_)
    )
  )
}

#' Classify regression models into families
#' @keywords internal
m2_classify_regression_family <- function(model_name) {
  model_name <- tolower(model_name)

  if (model_name %in% c("linear", "quadratic", "cubic")) {
    return("polynomial")
  }
  if (model_name %in% c("logarithmic", "power")) {
    return("transform")
  }
  if (model_name %in% c("exponential", "exponentialoffset")) {
    return("exponential")
  }
  if (model_name %in% c("logistic", "logistic4p", "gompertz", "gompertzoffset", "richards", "weibull")) {
    return("sigmoid")
  }
  if (model_name %in% c("vonbertalanffy", "beta", "chapmanrichards", "korf")) {
    return("biological")
  }
  if (model_name %in% c("mmf", "stannard", "hill", "monod", "baranyi", "ratkowsky", "hossfeld", "asymptotic")) {
    return("saturation")
  }
  if (model_name %in% c("fourier", "gaussian")) {
    return("periodic")
  }
  if (model_name %in% c("spline", "spline3")) {
    return("spline")
  }
  "other"
}

#' Summarize regression performance by family
#' @keywords internal
m2_summarize_regression_families <- function(model_cards) {
  if (!is.list(model_cards) || length(model_cards) == 0) {
    return(data.frame())
  }

  family <- vapply(model_cards, function(card) m2_coalesce_null(card$family, NA_character_), character(1))
  stability <- vapply(model_cards, function(card) m2_scalar_num(card$diagnostics$stability_score), numeric(1))
  rmse <- vapply(model_cards, function(card) m2_scalar_num(card$metrics$rmse), numeric(1))

  stats::aggregate(
    cbind(stability = stability, rmse = rmse),
    by = list(family = family),
    FUN = function(x) round(mean(x, na.rm = TRUE), 4)
  )
}

#' Retrieve raw fitted regression models from compute output
#' @keywords internal
m2_get_raw_regression_models <- function(result) {
  attr(result, "raw_models")
}
