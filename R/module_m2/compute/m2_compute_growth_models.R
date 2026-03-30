# ============================================================================
# m2_compute_growth_models.R - Advanced Growth Models
# ============================================================================
# Bass diffusion, Gompertz, Weibull, Richards, von Bertalanffy, Morgan-Mercer-Flodin

#' Fit Bass diffusion model
#'
#' @param years Vector of years
#' @param articles Vector of article counts
#' @return List with model parameters and predictions
#' @export
fit_bass_model <- function(years, articles) {
  n <- length(articles)
  if (n < 5) {
    return(list(status = "error: insufficient data", model = "bass"))
  }
  
  # Bass model: N(t) = m * (1 - exp(-(p+q)*t)) / (1 + (q/p)*exp(-(p+q)*t))
  # N(t) = cumulative adopters
  # p = innovation coefficient, q = imitation coefficient, m = market potential
  
  cumulative <- cumsum(articles)
  
  # Initial parameter estimates
  m_init <- max(cumulative) * 1.5
  
  # Fit using nonlinear least squares
  bass_fit <- tryCatch({
    nls(cumulative ~ m * (1 - exp(-(p + q) * (years - years[1]))) / 
        (1 + (q / p) * exp(-(p + q) * (years - years[1]))),
        start = list(m = m_init, p = 0.01, q = 0.3),
        control = nls.control(maxiter = 1000, warnOnly = TRUE))
  }, error = function(e) NULL)
  
  if (is.null(bass_fit)) {
    return(list(
      status = "error: bass model convergence failed",
      model = "bass",
      params = list(m = NA, p = NA, q = NA),
      predictions = rep(NA, n),
      fitted = rep(NA, n)
    ))
  }
  
  params <- coef(bass_fit)
  fitted_vals <- predict(bass_fit)
  residuals_val <- cumulative - fitted_vals
  
  # Bass curves types
  if (params["q"] > params["p"]) {
    curve_type <- "imitation-dominant"
  } else {
    curve_type <- "innovation-dominant"
  }
  
  # Peak time
  peak_time <- years[1] - log(params["p"] / params["q"]) / (params["p"] + params["q"])
  
  list(
    status = "success",
    model = "bass",
    params = list(
      m = params["m"],    # Market potential
      p = params["p"],    # Innovation coefficient
      q = params["q"]     # Imitation coefficient
    ),
    fitted = fitted_vals,
    residuals = residuals_val,
    peak_time = peak_time,
    curve_type = curve_type,
    cumsum = cumulative,
    years = years,
    AIC = AIC(bass_fit),
    BIC = BIC(bass_fit),
    R_squared = 1 - sum(residuals_val^2) / sum((cumulative - mean(cumulative))^2)
  )
}

#' Fit Gompertz growth model
#'
#' @param years Vector of years
#' @param articles Vector of article counts
#' @return List with model parameters and predictions
#' @export
fit_gompertz_model <- function(years, articles) {
  n <- length(articles)
  if (n < 4) {
    return(list(status = "error: insufficient data", model = "gompertz"))
  }
  
  # Gompertz: y(t) = K * exp(-exp(a - b*t))
  # Asymmetric S-curve, inflection at y = K/e
  
  cumulative <- cumsum(articles)
  t <- years - years[1]
  
  # Initial estimates
  K_init <- max(cumulative) * 1.5
  a_init <- log(log(K_init / cumulative[1]))
  b_init <- 0.1
  
  gomp_fit <- tryCatch({
    nls(cumulative ~ K * exp(-exp(a - b * t)),
        start = list(K = K_init, a = a_init, b = b_init),
        control = nls.control(maxiter = 1000, warnOnly = TRUE))
  }, error = function(e) NULL)
  
  if (is.null(gomp_fit)) {
    # Simplified fitting
    params <- fit_gompertz_simple(years, cumulative)
  } else {
    params <- coef(gomp_fit)
    fitted_vals <- predict(gomp_fit)
    residuals_val <- cumulative - fitted_vals
  }
  
  if (is.null(gomp_fit)) {
    fitted_vals <- params$K * exp(-exp(params$a - params$b * t))
    residuals_val <- cumulative - fitted_vals
  }
  
  # Inflection point
  inflection_t <- params["a"] / params["b"]
  inflection_y <- params["K"] / exp(1)
  
  list(
    status = "success",
    model = "gompertz",
    params = list(
      K = params["K"],    # Carrying capacity
      a = params["a"],    # Initial growth rate
      b = params["b"]     # Growth rate parameter
    ),
    fitted = fitted_vals,
    residuals = residuals_val,
    inflection_t = years[1] + inflection_t,
    inflection_y = inflection_y,
    years = years,
    cumsum = cumulative,
    AIC = if (!is.null(gomp_fit)) AIC(gomp_fit) else NA,
    BIC = if (!is.null(gomp_fit)) BIC(gomp_fit) else NA,
    R_squared = 1 - sum(residuals_val^2) / sum((cumulative - mean(cumulative))^2)
  )
}

#' Simple Gompertz fitting fallback
#' @keywords internal
fit_gompertz_simple <- function(years, cumulative) {
  t <- years - years[1]
  
  K <- max(cumulative) * 1.5
  
  # Linearize: ln(ln(K/y)) = a - b*t
  y_valid <- cumulative[cumulative >0 & cumulative < K]
  t_valid <- t[cumulative > 0 & cumulative < K]
  
  if (length(y_valid) < 3) {
    return(c(K = K, a = 1, b = 0.1))
  }
  
  y_trans <- log(log(K / y_valid))
  
  fit <- lm(y_trans ~ t_valid)
  
  c(K = K, a = coef(fit)[1], b = -coef(fit)[2])
}

#' Fit Weibull growth model
#'
#' @param years Vector of years
#' @param articles Vector of article counts
#' @return List with model parameters and predictions
#' @export
fit_weibull_model <- function(years, articles) {
  n <- length(articles)
  if (n < 4) {
    return(list(status = "error: insufficient data", model = "weibull"))
  }
  
  # Weibull cumulative: F(t) = 1 - exp(-(t/scale)^shape)
  # For growth: y(t) = K * (1 - exp(-(t/scale)^shape))
  
  cumulative <- cumsum(articles)
  t <- years - years[1]
  
  K_init <- max(cumulative) * 1.5
  scale_init <- max(t)
  shape_init <- 2
  
  weib_fit <- tryCatch({
    nls(cumulative ~ K * (1 - exp(-(t/scale)^shape)),
        start = list(K = K_init, scale = scale_init, shape = shape_init),
        control = nls.control(maxiter = 1000, warnOnly = TRUE))
  }, error = function(e) NULL)
  
  if (is.null(weib_fit)) {
    # Simple estimate
    params <- c(K = K_init, scale = scale_init, shape = shape_init)
    fitted_vals <- K_init * (1 - exp(-(t/scale_init)^shape_init))
    residuals_val <- cumulative - fitted_vals
  } else {
    params <- coef(weib_fit)
    fitted_vals <- predict(weib_fit)
    residuals_val <- cumulative - fitted_vals
  }
  
  list(
    status = "success",
    model = "weibull",
    params = list(
      K = params["K"],
      scale = params["scale"],
      shape = params["shape"]
    ),
    fitted = fitted_vals,
    residuals = residuals_val,
    years = years,
    cumsum = cumulative,
    AIC = if (!is.null(weib_fit)) AIC(weib_fit) else NA,
    BIC = if (!is.null(weib_fit)) BIC(weib_fit) else NA,
    R_squared = 1 - sum(residuals_val^2) / sum((cumulative - mean(cumulative))^2)
  )
}

#' Fit Richards growth model (generalized logistic)
#'
#' @param years Vector of years
#' @param articles Vector of article counts
#' @return List with model parameters and predictions
#' @export
fit_richards_model <- function(years, articles) {
  n <- length(articles)
  if (n < 5) {
    return(list(status = "error: insufficient data", model = "richards"))
  }
  
  # Richards: y(t) = K / (1 + exp(-r*(t-t0)))^(1/v)
  # When v=1, reduces to logistic
  
  cumulative <- cumsum(articles)
  t <- years - years[1]
  
  K_init <- max(cumulative) * 1.5
  r_init <- 0.1
  t0_init <- t[which.max(articles)]
  v_init <- 1
  
  rich_fit <- tryCatch({
    nls(cumulative ~ K / (1 + exp(-r * (t - t0)))^(1/v),
        start = list(K = K_init, r = r_init, t0 = t0_init, v = v_init),
        control = nls.control(maxiter = 1000, warnOnly = TRUE))
  }, error = function(e) NULL)
  
  if (is.null(rich_fit)) {
    params <- c(K = K_init, r = r_init, t0 = t0_init, v = v_init)
    fitted_vals <- K_init / (1 + exp(-r_init * (t - t0_init)))^(1/v_init)
    residuals_val <- cumulative - fitted_vals
  } else {
    params <- coef(rich_fit)
    fitted_vals <- predict(rich_fit)
    residuals_val <- cumulative - fitted_vals
  }
  
  # Inflection point
  inflection_t <- params["t0"] + log(params["v"]) / params["r"]
  
  list(
    status = "success",
    model = "richards",
    params = list(
      K = params["K"],      # Carrying capacity
      r = params["r"],      # Growth rate
      t0 = params["t0"],    # Inflection time parameter
      v = params["v"]       # Shape parameter
    ),
    fitted = fitted_vals,
    residuals = residuals_val,
    inflection_t = years[1] + inflection_t,
    years = years,
    cumsum = cumulative,
    AIC = if (!is.null(rich_fit)) AIC(rich_fit) else NA,
    BIC = if (!is.null(rich_fit)) BIC(rich_fit) else NA,
    R_squared = 1 - sum(residuals_val^2) / sum((cumulative - mean(cumulative))^2),
    is_asymmetric = params["v"] != 1
  )
}

#' Fit von Bertalanffy growth model
#'
#' @param years Vector of years
#' @param articles Vector of article counts
#' @return List with model parameters and predictions
#' @export
fit_von_bertalanffy_model <- function(years, articles) {
  n <- length(articles)
  if (n < 4) {
    return(list(status = "error: insufficient data", model = "von_bertalanffy"))
  }
  
  # Von Bertalanffy: L(t) = L_inf * (1 - exp(-k*(t - t0)))
  # Used for biological growth, applicable to knowledge growth
  
  cumulative <- cumsum(articles)
  t <- years - years[1]
  
  L_inf_init <- max(cumulative) * 1.5
  k_init <- 0.1
  t0_init <- -1
  
  vb_fit <- tryCatch({
    nls(cumulative ~ L_inf * (1 - exp(-k * (t - t0))),
        start = list(L_inf = L_inf_init, k = k_init, t0 = t0_init),
        control = nls.control(maxiter = 1000, warnOnly = TRUE))
  }, error = function(e) NULL)
  
  if (is.null(vb_fit)) {
    params <- c(L_inf = L_inf_init, k = k_init, t0 = t0_init)
    fitted_vals <- L_inf_init * (1 - exp(-k_init * (t - t0_init)))
    residuals_val <- cumulative - fitted_vals
  } else {
    params <- coef(vb_fit)
    fitted_vals <- predict(vb_fit)
    residuals_val <- cumulative - fitted_vals
  }
  
  list(
    status = "success",
    model = "von_bertalanffy",
    params = list(
      L_inf = params["L_inf"],  # Asymptotic maximum
      k = params["k"],         # Growth coefficient
      t0 = params["t0"]        # Theoretical age at size zero
    ),
    fitted = fitted_vals,
    residuals = residuals_val,
    years = years,
    cumsum = cumulative,
    AIC = if (!is.null(vb_fit)) AIC(vb_fit) else NA,
    BIC = if (!is.null(vb_fit)) BIC(vb_fit) else NA,
    R_squared = 1 - sum(residuals_val^2) / sum((cumulative - mean(cumulative))^2)
  )
}

#' Fit Morgan-Mercer-Flodin model
#'
#' @param years Vector of years
#' @param articles Vector of article counts
#' @return List with model parameters and predictions
#' @export
fit_mmf_model <- function(years, articles) {
  n <- length(articles)
  if (n < 5) {
    return(list(status = "error: insufficient data", model = "mmf"))
  }
  
  # MMF: y(t) = (a * t^d + c) / (b + t^d)
  # Flexible sigmoid with parameter d controlling shape
  
  cumulative <- cumsum(articles)
  t <- years - years[1] + 1  # Avoid t=0
  
  a_init <- max(cumulative)
  b_init <- 1
  c_init <- cumulative[1]
  d_init <- 2
  
  mmf_fit <- tryCatch({
    nls(cumulative ~ (a * t^d + c) / (b + t^d),
        start = list(a = a_init, b = b_init, c = c_init, d = d_init),
        control = nls.control(maxiter = 1000, warnOnly = TRUE))
  }, error = function(e) NULL)
  
  if (is.null(mmf_fit)) {
    params <- c(a = a_init, b = b_init, c = c_init, d = d_init)
    fitted_vals <- (a_init * t^d_init + c_init) / (b_init + t^d_init)
    residuals_val <- cumulative - fitted_vals
  } else {
    params <- coef(mmf_fit)
    fitted_vals <- predict(mmf_fit)
    residuals_val <- cumulative - fitted_vals
  }
  
  list(
    status = "success",
    model = "mmf",
    params = list(
      a = params["a"],
      b = params["b"],
      c = params["c"],
      d = params["d"]
    ),
    fitted = fitted_vals,
    residuals = residuals_val,
    years = years,
    cumsum = cumulative,
    AIC = if (!is.null(mmf_fit)) AIC(mmf_fit) else NA,
    BIC = if (!is.null(mmf_fit)) BIC(mmf_fit) else NA,
    R_squared = 1 - sum(residuals_val^2) / sum((cumulative - mean(cumulative))^2)
  )
}

#' Compare all growth models
#'
#' @param years Vector of years
#' @param articles Vector of article counts
#' @return List with all model fits and comparison
#' @export
compare_growth_models <- function(years, articles) {
  n <- length(articles)
  if (n < 5) {
    return(list(status = "error: insufficient data"))
  }
  
  models <- list()
  
  models$bass <- fit_bass_model(years, articles)
  models$gompertz <- fit_gompertz_model(years, articles)
  models$weibull <- fit_weibull_model(years, articles)
  models$richards <- fit_richards_model(years, articles)
  models$von_bertalanffy <- fit_von_bertalanffy_model(years, articles)
  models$mmf <- fit_mmf_model(years, articles)
  
  # Extract AIC/BIC/R² for comparison
  comparison <- data.frame(
    model = names(models),
    AIC = sapply(models, function(m) if(is.list(m)) m$AIC else NA),
    BIC = sapply(models, function(m) if(is.list(m)) m$BIC else NA),
    R_squared = sapply(models, function(m) if(is.list(m)) m$R_squared else NA),
    stringsAsFactors = FALSE
  )
  
  # Remove failed models
  comparison <- comparison[!is.na(comparison$AIC), ]
  
  if (nrow(comparison) == 0) {
    return(list(
      status = "error: all models failed to converge",
      models = models,
      comparison = data.frame()
    ))
  }
  
  comparison <- comparison[order(comparison$AIC), ]
  comparison$delta_AIC <- comparison$AIC - min(comparison$AIC)
  comparison$weight <- exp(-0.5 * comparison$delta_AIC)
  comparison$weight <- comparison$weight / sum(comparison$weight)
  
  best_model <- comparison$model[1]
  
  list(
    status = "success",
    models = models,
    comparison = comparison,
    best_model = best_model,
    best_fit = models[[best_model]],
    n_models = nrow(comparison)
  )
}

#' Forecast using growth model
#'
#' @param model_fit Output from a growth model fit
#' @param years_new Years to forecast
#' @return Forecasted values
#' @export
forecast_growth_model <- function(model_fit, years_new) {
  if (is.null(model_fit) || model_fit$status != "success") {
    return(rep(NA, length(years_new)))
  }
  
  # Calculate t relative to first observed year
  t_new <- years_new - model_fit$years[1]
  
  # Store baseline offset: last observed cumulative value
  # This ensures forecasts start from where data ends, not from zero
  baseline_cumsum <- model_fit$cumsum[length(model_fit$cumsum)]
  
  if (model_fit$model == "bass") {
    p <- model_fit$params$p
    q <- model_fit$params$q
    m <- model_fit$params$m
    # Bass cumulative formula: N(t) = m * (1 - exp(-(p+q)*t)) / (1 + (q/p)*exp(-(p+q)*t))
    forecast_cumsum <- m * (1 - exp(-(p + q) * t_new)) / (1 + (q / p) * exp(-(p + q) * t_new))
    # Add baseline to get absolute cumulative values
    forecast_cumsum <- baseline_cumsum + forecast_cumsum
    # Convert cumulative to period values
    forecast_vals <- c(forecast_cumsum[1], diff(forecast_cumsum))
    # For first forecast, also add the baseline
    if (length(forecast_vals) > 0 && t_new[1] <= 1) {
      forecast_vals[1] <- model_fit$fitted[length(model_fit$fitted)]
    }
  } else if (model_fit$model == "gompertz") {
    K <- model_fit$params$K
    a <- model_fit$params$a
    b <- model_fit$params$b
    forecast_cumsum <- K * exp(-exp(a - b * t_new))
    forecast_cumsum <- baseline_cumsum + forecast_cumsum
    forecast_vals <- c(forecast_cumsum[1], diff(forecast_cumsum))
  } else if (model_fit$model == "weibull") {
    K <- model_fit$params$K
    scale <- model_fit$params$scale
    shape <- model_fit$params$shape
    forecast_cumsum <- K * (1 - exp(-(t_new/scale)^shape))
    forecast_cumsum <- baseline_cumsum + forecast_cumsum
    forecast_vals <- c(forecast_cumsum[1], diff(forecast_cumsum))
  } else if (model_fit$model == "richards") {
    K <- model_fit$params$K
    r <- model_fit$params$r
    t0 <- model_fit$params$t0
    v <- model_fit$params$v
    forecast_cumsum <- K / (1 + exp(-r * (t_new - t0)))^(1/v)
    forecast_cumsum <- baseline_cumsum + forecast_cumsum
    forecast_vals <- c(forecast_cumsum[1], diff(forecast_cumsum))
  } else if (model_fit$model == "von_bertalanffy") {
    L_inf <- model_fit$params$L_inf
    k <- model_fit$params$k
    t0 <- model_fit$params$t0
    forecast_cumsum <- L_inf * (1 - exp(-k * (t_new - t0)))
    forecast_cumsum <- baseline_cumsum + forecast_cumsum
    forecast_vals <- c(forecast_cumsum[1], diff(forecast_cumsum))
  } else if (model_fit$model == "mmf") {
    a <- model_fit$params$a
    b<- model_fit$params$b
    c <- model_fit$params$c
    d <- model_fit$params$d
    t_new_shifted <- t_new + 1
    forecast_cumsum <- (a * t_new_shifted^d + c) / (b + t_new_shifted^d)
    forecast_cumsum <- baseline_cumsum + forecast_cumsum
    forecast_vals <- c(forecast_cumsum[1], diff(forecast_cumsum))
  } else {
    return(rep(NA, length(years_new)))
  }
  
  # Ensure non-negative values
  forecast_vals <- pmax(0, forecast_vals)
  
  list(
    years = years_new,
    cumulative = forecast_cumsum,
    values = forecast_vals,
    model = model_fit$model
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b