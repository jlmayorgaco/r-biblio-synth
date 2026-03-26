# ============================================================================
# module_m2/compute/m2_compute_regression.R - Regression analysis (FIXED)
# ============================================================================

#' @export
compute_m2_regression <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) < 5) {
    return(list(status = "error", comparison_table = data.frame(), best_model = list(), models = list(), performance = list()))
  }

  years <- as.numeric(input$Year)
  articles <- as.numeric(input$Articles)
  data <- data.frame(Year = years, Articles = articles)

  models <- m2_fit_all_models(data)
  comparison <- m2_build_comparison_table(models, articles)
  best <- m2_select_best_model(comparison)
  perf <- m2_analyze_performance(years, articles, models, best)

  list(
    status = "success",
    comparison_table = comparison,
    best_model = best,
    models = lapply(models, m2_serialize_model),
    performance = perf
  )
}

#' Fit all regression models (FIXED: starting values)
m2_fit_all_models <- function(data) {
  models <- list()

  models$Linear <- tryCatch(lm(Articles ~ Year, data = data), error = function(e) NULL)
  models$Polynomial <- tryCatch(lm(Articles ~ Year + I(Year^2) + I(Year^3), data = data), error = function(e) NULL)

  models$Exponential <- tryCatch(
    nls(Articles ~ m2_exponential(Year, r, N0, t0), data = data,
        start = list(r = 0.05, N0 = min(data$Articles) + 1, t0 = min(data$Year))),
    error = function(e) NULL)

  models$Logarithmic <- tryCatch(lm(Articles ~ log(Year), data = data), error = function(e) NULL)

  models$Logistic <- tryCatch(
    nls(Articles ~ m2_logistic(Year, K, r, t0), data = data,
        start = list(K = max(data$Articles), r = 0.1, t0 = mean(data$Year))),
    error = function(e) NULL)

  # Gompertz (FIXED: Nmax must be > N0 for growth)
  models$Gompertz <- tryCatch(
    nls(Articles ~ m2_gompertz(Year, N0, Nmax, k, t0, y0), data = data,
        start = list(N0 = max(data$Articles), Nmax = max(data$Articles) * 1.5, k = 0.05,
                     t0 = min(data$Year), y0 = min(data$Articles))),
    error = function(e) NULL)

  # Weibull (FIXED: correct parameterization)
  models$Weibull <- tryCatch(
    nls(Articles ~ m2_weibull(Year, K, r, t0, alpha), data = data,
        start = list(K = max(data$Articles), r = 1, t0 = min(data$Year), alpha = 10)),
    error = function(e) NULL)

  models$VonBertalanffy <- tryCatch(
    nls(Articles ~ m2_vonbertalanffy(Year, Linf, k, t0), data = data,
        start = list(Linf = max(data$Articles), k = 0.05, t0 = min(data$Year))),
    error = function(e) NULL)

  models$Normal <- tryCatch(
    nls(Articles ~ m2_normal(Year, mu, sigma, A), data = data,
        start = list(mu = mean(data$Year), sigma = sd(data$Year), A = max(data$Articles))),
    error = function(e) NULL)

  # Spline regression (B-spline with 5 df)
  models$Spline <- tryCatch(
    lm(Articles ~ splines::bs(Year, df = 5), data = data),
    error = function(e) NULL)

  # Richards model (generalized logistic)
  models$Richards <- tryCatch(
    nls(Articles ~ m2_richards(Year, K, r, t0, nu), data = data,
        start = list(K = max(data$Articles), r = 0.1, t0 = mean(data$Year), nu = 1.5)),
    error = function(e) NULL)

  # Fourier model (periodic with 10-year period)
  models$Fourier <- tryCatch(
    lm(Articles ~ sin(2 * pi * Year / 10) + cos(2 * pi * Year / 10), data = data),
    error = function(e) NULL)

  Filter(Negate(is.null), models)
}

#' Build comparison table (FIXED: correct R² for NLS)
m2_build_comparison_table <- function(models, y) {
  results <- data.frame(Model = character(), R2 = numeric(), RMSE = numeric(),
                        AIC = numeric(), BIC = numeric(), Parameters = character(),
                        stringsAsFactors = FALSE)

  for (name in names(models)) {
    mod <- models[[name]]
    preds <- tryCatch(predict(mod), error = function(e) NULL)
    if (is.null(preds) || any(is.na(preds))) next

    # Correct R²: 1 - SS_res/SS_tot (works for both lm and nls)
    # NOTE: Negative R² for NLS indicates model worse than horizontal line - diagnostic!
    ss_res <- sum((y - preds)^2)
    ss_tot <- sum((y - mean(y))^2)
    r2 <- if (ss_tot > 0) 1 - ss_res / ss_tot else 0

    rmse <- sqrt(mean((y - preds)^2))
    aic <- tryCatch(AIC(mod), error = function(e) NA)
    bic <- tryCatch(BIC(mod), error = function(e) NA)
    params <- tryCatch(coef(mod), error = function(e) c())
    param_str <- paste(names(params), "=", round(params, 4), collapse = ", ")

    results <- rbind(results, data.frame(
      Model = name, R2 = round(r2, 6), RMSE = round(rmse, 4),
      AIC = round(aic, 4), BIC = round(bic, 4), Parameters = param_str,
      stringsAsFactors = FALSE))
  }

  results[order(-results$R2), ]
}

m2_select_best_model <- function(comparison) {
  if (nrow(comparison) == 0) return(list(name = "none", R2 = 0, params = ""))
  best <- comparison[1, ]
  list(name = best$Model, R2 = best$R2, params = best$Parameters)
}

#' Analyze model performance (FIXED: use lmtest:: instead of require)
m2_analyze_performance <- function(x, y, models, best) {
  if (best$name == "none") return(list())

  mod <- models[[best$name]]
  preds <- tryCatch(predict(mod), error = function(e) NULL)
  if (is.null(preds)) return(list())

  residuals <- y - preds
  std_resid <- if (sd(residuals) > 0) residuals / sd(residuals) else residuals

  acf_vals <- tryCatch(acf(residuals, plot = FALSE)$acf[-1], error = function(e) numeric(0))
  shapiro_p <- tryCatch(shapiro.test(residuals)$p.value, error = function(e) NA)

  # FIXED: use lmtest:: namespace
  dw_stat <- tryCatch(lmtest::dwtest(mod)$statistic, error = function(e) NA)

  list(
    residuals = round(residuals, 4),
    std_residuals = round(std_resid, 4),
    acf = round(acf_vals, 4),
    shapiro_p = round(shapiro_p, 4),
    dw_statistic = round(dw_stat, 4)
  )
}

m2_serialize_model <- function(mod) {
  list(
    class = class(mod)[1],
    coefficients = tryCatch(as.list(coef(mod)), error = function(e) list()),
    r_squared = tryCatch(summary(mod)$r.squared, error = function(e) NA)
  )
}
