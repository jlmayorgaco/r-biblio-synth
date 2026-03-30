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
    return(list(
      status = "error",
      comparison_table = data.frame(),
      best_model = list(),
      models = list(),
      performance = list()
    ))
  }
  
  years <- as.numeric(input$Year)
  articles <- as.numeric(input$Articles)
  data <- data.frame(Year = years, Articles = articles)
  
  models <- m2_fit_all_models(data)
  
  if (length(models) == 0) {
    return(list(
      status = "error: no models converged",
      comparison_table = data.frame(),
      best_model = list(name = "none", R2 = 0),
      models = list(),
      performance = list()
    ))
  }
  
  comparison <- m2_build_comparison_table(models, articles)
  best <- m2_select_best_model(comparison, models, years, articles)
  perf <- m2_analyze_performance(years, articles, models, best)
  
  list(
    status = "success",
    comparison_table = comparison,
    best_model = best,
    models = lapply(models, m2_serialize_model),
    performance = perf,
    data = data
  )
}

#' Fit all regression models with robust parameter estimation
#' @keywords internal
m2_fit_all_models <- function(data) {
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
  
  models$Logarithmic <- tryCatch(
    lm(Articles ~ log(Year), data = data),
    error = function(e) NULL
  )
  
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
  
  # Remove NULL models
  Filter(Negate(is.null), models)
}

#' Build comparison table with all metrics
#' @keywords internal
m2_build_comparison_table <- function(models, y) {
  results <- data.frame(
    Model = character(),
    R2 = numeric(),
    Adj_R2 = numeric(),
    RMSE = numeric(),
    MAE = numeric(),
    MAPE = numeric(),
    AIC = numeric(),
    BIC = numeric(),
    N_params = integer(),
    Equation = character(),
    stringsAsFactors = FALSE
  )
  
  for (name in names(models)) {
    mod <- models[[name]]
    
    preds <- tryCatch(predict(mod), error = function(e) NULL)
    if (is.null(preds) || any(is.na(preds))) next
    
    n <- length(y)
    ss_res <- sum((y - preds)^2)
    ss_tot <- sum((y - mean(y))^2)
    
    r2 <- if (ss_tot > 0) 1 - ss_res / ss_tot else 0
    n_params <- length(coef(mod))
    adj_r2 <- if (n > n_params) 1 - (1 - r2) * (n - 1) / (n - n_params) else r2
    
    rmse <- sqrt(mean((y - preds)^2))
    mae <- mean(abs(y - preds))
    
    mape <- tryCatch({
      nonzero <- y != 0
      if (sum(nonzero) > 0) mean(abs((y[nonzero] - preds[nonzero]) / y[nonzero])) * 100 else NA
    }, error = function(e) NA)
    
    aic <- tryCatch(AIC(mod), error = function(e) NA)
    bic <- tryCatch(BIC(mod), error = function(e) NA)
    
    equation <- m2_get_equation_string(name, coef(mod))
    
    results <- rbind(results, data.frame(
      Model = name,
      R2 = round(r2, 6),
      Adj_R2 = round(adj_r2, 6),
      RMSE = round(rmse, 4),
      MAE = round(mae, 4),
      MAPE = round(mape, 2),
      AIC = round(aic, 4),
      BIC = round(bic, 4),
      N_params = n_params,
      Equation = equation,
      stringsAsFactors = FALSE
    ))
  }
  
  results[order(-results$R2), ]
}

#' Select best model using information criteria
#' @keywords internal
m2_select_best_model <- function(comparison, models = NULL, years = NULL, articles = NULL) {
  if (nrow(comparison) == 0) {
    return(list(name = "none", R2 = 0, Adj_R2 = 0, RMSE = NA, equation = "", predictions = NULL))
  }
  
  # Primary: highest adjusted R²
  best_idx <- which.max(comparison$Adj_R2)
  
  if (length(best_idx) == 0) best_idx <- 1
  
  best <- comparison[best_idx[1], ]
  
  # Compute predictions if years and articles are provided
  predictions <- NULL
  if (!is.null(years) && !is.null(articles) && !is.null(models)) {
    model_name <- best$Model
    if (model_name %in% names(models) && !is.null(models[[model_name]])) {
      predictions <- tryCatch({
        predict(models[[model_name]], newdata = data.frame(Year = years))
      }, error = function(e) NULL)
    }
  }
  
  list(
    name = best$Model,
    R2 = best$R2,
    Adj_R2 = best$Adj_R2,
    RMSE = best$RMSE,
    MAE = best$MAE,
    AIC = best$AIC,
    BIC = best$BIC,
    equation = best$Equation,
    n_params = best$N_params,
    predictions = predictions
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

#' Analyze model performance and residuals
#' @keywords internal
m2_analyze_performance <- function(x, y, models, best) {
  if (is.null(best) || best$name == "none") return(list())
  
  mod <- models[[best$name]]
  if (is.null(mod)) return(list())
  
  preds <- tryCatch(predict(mod), error = function(e) NULL)
  if (is.null(preds)) return(list())
  
  residuals <- y - preds
  n <- length(y)
  
  std_resid <- if (sd(residuals) > 0) residuals / sd(residuals) else residuals
  
  acf_vals <- tryCatch(
    acf(residuals, plot = FALSE, lag.max = min(10, n - 1))$acf[-1],
    error = function(e) numeric(0)
  )
  
  shapiro_p <- tryCatch(
    shapiro.test(residuals)$p.value,
    error = function(e) NA
  )
  
  dw_stat <- tryCatch(
    lmtest::dwtest(mod)$statistic,
    error = function(e) NA
  )
  
  # Jarque-Bera test for normality
  jb_stat <- tryCatch({
    n <- length(residuals)
    skewness <- sum((residuals - mean(residuals))^3) / (n * sd(residuals)^3)
    kurtosis <- sum((residuals - mean(residuals))^4) / (n * sd(residuals)^4) - 3
    n * (skewness^2 / 6 + kurtosis^2 / 24)
  }, error = function(e) NA)
  
  list(
    residuals = round(residuals, 4),
    std_residuals = round(std_resid, 4),
    acf = round(acf_vals, 4),
    shapiro_p = round(shapiro_p, 4),
    dw_statistic = round(dw_stat, 4),
    jarque_bera = round(jb_stat, 4),
    n_obs = n
  )
}

#' Serialize model for storage
#' @keywords internal
m2_serialize_model <- function(mod) {
  list(
    class = class(mod)[1],
    coefficients = tryCatch(as.list(coef(mod)), error = function(e) list()),
    r_squared = tryCatch(summary(mod)$r.squared, error = function(e) NA),
    adj_r_squared = tryCatch(summary(mod)$adj.r.squared, error = function(e) NA)
  )
}