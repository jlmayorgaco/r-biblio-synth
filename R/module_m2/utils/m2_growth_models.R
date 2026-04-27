# ============================================================================
# m2_growth_models.R - Comprehensive Growth Model Functions for IEEE Q1
# ============================================================================
# Contains 22 growth models for bibliometric time series analysis
# Each model is fitted using nls() with robust starting values
# ============================================================================

# ============================================================================
# LINEAR AND POLYNOMIAL MODELS
# ============================================================================

#' Linear model: y = a*t + b
m2_linear <- function(t, a, b) { a * t + b }

#' Polynomial model (degree 2): y = a*t^2 + b*t + c
m2_quadratic <- function(t, a, b, c) { a * t^2 + b * t + c }

#' Polynomial model (degree 3): y = a*t^3 + b*t^2 + c*t + d
m2_cubic <- function(t, a, b, c, d) { a * t^3 + b * t^2 + c * t + d }

# ============================================================================
# LOGARITHMIC AND POWER MODELS
# ============================================================================

#' Logarithmic model: y = a*log(t) + b
m2_logarithmic <- function(t, a, b) { a * log(t) + b }

#' Power model: y = a * t^b
m2_power <- function(t, a, b) { a * t^b }

#' Power with offset: y = a * (t - t0)^b
m2_power_offset <- function(t, a, b, t0) { 
  ifelse(t > t0, a * (t - t0)^b, 0) 
}

# ============================================================================
# EXPONENTIAL MODELS
# ============================================================================

#' Exponential model: y = N0 * exp(r * (t - t0))
m2_exponential <- function(t, r, N0, t0) { N0 * exp(r * (t - t0)) }

#' Modified exponential (offset): y = a + b * exp(r * t)
m2_exponential_offset <- function(t, a, b, r) { a + b * exp(r * t) }

#' Double exponential: y = a * exp(r1 * t) + b * exp(r2 * t)
m2_double_exponential <- function(t, a, b, r1, r2) { 
  a * exp(r1 * t) + b * exp(r2 * t) 
}

# ============================================================================
# SIGMOID/S-CURVE MODELS
# ============================================================================

#' Logistic model: y = K / (1 + exp(-r * (t - t0)))
m2_logistic <- function(t, K, r, t0) { K / (1 + exp(-r * (t - t0))) }

#' 4-Parameter Logistic: y = a + (K - a) / (1 + exp(-r * (t - t0)))
m2_logistic_4param <- function(t, a, K, r, t0) { 
  a + (K - a) / (1 + exp(-r * (t - t0))) 
}

#' Gompertz model: y = K * exp(-exp(-r * (t - t0)))
m2_gompertz <- function(t, K, r, t0, ...) {
  legacy_args <- list(...)

  if (length(legacy_args) >= 2) {
    N0 <- K
    Nmax <- r
    k <- t0
    t0_legacy <- legacy_args[[1]]
    y0 <- legacy_args[[2]]
    scale <- max(Nmax - y0, .Machine$double.eps)
    baseline <- max(y0, .Machine$double.eps)
    return(baseline + scale * exp(-exp(-k * (t - t0_legacy))) + max(N0 - baseline, 0) * 0)
  }

  K * exp(-exp(-r * (t - t0)))
}

#' Modified Gompertz (with offset): y = a + (K - a) * exp(-exp(-r * (t - t0)))
m2_gompertz_offset <- function(t, a, K, r, t0) {
  a + (K - a) * exp(-exp(-r * (t - t0)))
}

#' Richards (generalized logistic): y = K / (1 + v * exp(-r * (t - t0)))^(1/v)
m2_richards <- function(t, K, r, t0, v) { 
  if (abs(v) < .Machine$double.eps) return(K / (1 + exp(-r * (t - t0))))
  K / (1 + v * exp(-r * (t - t0)))^(1/v)
}

#' Weibull growth model: y = K * (1 - exp(-((t - t0) / alpha)^r))
m2_weibull <- function(t, K, r, t0, alpha) { 
  K * (1 - exp(-((pmax(t - t0, 0)) / alpha)^r)) 
}

# ============================================================================
# BIOLOGICAL GROWTH MODELS
# ============================================================================

#' Von Bertalanffy: y = Linf * (1 - exp(-k * (t - t0)))
m2_vonbertalanffy <- function(t, Linf, k, t0) { Linf * (1 - exp(-k * (t - t0))) }

#' Beta growth model: y = K * (1 + (t - t0)/alpha)^(-beta)
m2_beta <- function(t, K, alpha, beta, t0) { 
  K * (1 + (pmax(t - t0, 0.001)) / alpha)^(-beta) 
}

#' Gamma growth model: y = K * ((t - t0) / alpha)^(beta - 1) * exp(-(t - t0) / alpha) / gamma(beta)
m2_gamma_growth <- function(t, K, alpha, beta, t0) { 
  t_adj <- pmax(t - t0, 0.001)
  K * (t_adj / alpha)^(beta - 1) * exp(-t_adj / alpha) / gamma(beta)
}

#' Chapman-Richards: y = K * (1 - exp(-r * (t - t0)))^m
m2_chapman_richards <- function(t, K, r, t0, m) { 
  K * (1 - exp(-r * (t - t0)))^m 
}

#' Korf model: y = K * exp(-r / (t^beta))
m2_korf <- function(t, K, r, beta) { K * exp(-r / (t^beta)) }

# ============================================================================
# SATURATION MODELS
# ============================================================================

#' Morgan-Mercer-Flodin (MMF): y = (a * b + K * t^d) / (b + t^d)
m2_mmf <- function(t, K, a, b, d) { (a * b + K * t^d) / (b + t^d) }

#' Stannard model: y = K / (1 + exp(-a - b * log(t)))
m2_stannard <- function(t, K, a, b) { K / (1 + exp(-a - b * log(pmax(t, 0.001)))) }

#' Hill model: y = K * t^n / (K50^n + t^n)
m2_hill <- function(t, K, K50, n) { K * t^n / (K50^n + t^n) }

#' Monod model: y = K * t / (Ks + t)
m2_monod <- function(t, K, Ks) { K * t / (Ks + t) }

#' Michaelis-Menten (same as Monod): y = Vmax * t / (Km + t)
m2_michaelis_menten <- function(t, Vmax, Km) { Vmax * t / (Km + t) }

# ============================================================================
# ADVANCED MODELS
# ============================================================================

#' Baranyi model: y = y0 + (y_max - y0) * exp(-exp(h0 + lambda*t))
m2_baranyi <- function(t, y0, y_max, h0, lambda) {
  y0 + (y_max - y0) * (1 - exp(-exp(h0 + lambda * t)))
}

#' Ratkowsky model: y = a * (1 - exp(-b * (t - t0)))^c
m2_ratkowsky <- function(t, a, b, t0, c) { 
  a * (1 - exp(-b * (t - t0)))^c 
}

#' Hossfeld model: y = K * t / (1 + a * t + b * t^2)
m2_hossfeld <- function(t, K, a, b) { K * t / (1 + a * t + b * t^2) }

#' Negative exponential: y = K * (1 - exp(-r * (t - t0)))
m2_neg_exponential <- function(t, K, r, t0) { K * (1 - exp(-r * (t - t0))) }

#' Hyperbolic: y = K / (t + a)
m2_hyperbolic <- function(t, K, a) { K / (pmax(t, 0.001) + a) }

#' Asymptotic regression: y = a - b * exp(-c * t)
m2_asymptotic <- function(t, a, b, c) { a - b * exp(-c * t) }

# ============================================================================
# PERIODIC/HARMONIC MODELS
# ============================================================================

#' Fourier series (first harmonic): y = a0 + a1*cos(w*t) + b1*sin(w*t)
m2_fourier <- function(t, a0, a1, b1, w) { a0 + a1 * cos(w * t) + b1 * sin(w * t) }

#' Fourier series (second harmonic): y = a0 + sum of 2 harmonics
m2_fourier2 <- function(t, a0, a1, b1, a2, b2, w) {
  a0 + a1 * cos(w * t) + b1 * sin(w * t) + a2 * cos(2 * w * t) + b2 * sin(2 * w * t)
}

#' Gaussian pulse: y = A * exp(-(t - mu)^2 / (2 * sigma^2))
m2_gaussian <- function(t, A, mu, sigma) { 
  A * exp(-(t - mu)^2 / (2 * sigma^2)) 
}

# ============================================================================
# PARAMETER ESTIMATION HELPERS
# ============================================================================

#' Get robust starting values for a model
#' @param model_name Name of the model
#' @param data Data frame with Year and Articles columns
#' @return Named list of starting parameter values
m2_get_start_values <- function(model_name, data) {
  y <- data$Articles
  t <- data$Year
  
  y_max <- max(y, na.rm = TRUE)
  y_min <- min(y, na.rm = TRUE)
  y_mean <- mean(y, na.rm = TRUE)
  t_min <- min(t, na.rm = TRUE)
  t_max <- max(t, na.rm = TRUE)
  t_range <- t_max - t_min
  
  # Estimate growth rate from first and last values
  y_first <- mean(head(y, min(3, length(y))))
  y_last <- mean(tail(y, min(3, length(y))))
  if (y_first > 0 && y_last > y_first) {
    r_est <- log(y_last / y_first) / t_range
  } else {
    r_est <- 0.01
  }
  
  # Ensure positive values for models that require them
  y_positive <- max(y_max, 0.001)
  y_mean_pos <- max(y_mean, 0.001)
  y_first_pos <- max(y_first, 0.001)
  y_last_pos <- max(y_last, 0.001)
  
  switch(model_name,
    "Linear" = list(a = (y_last - y_first) / t_range, b = y_first),
    "Quadratic" = list(a = 0.001, b = (y_last - y_first) / t_range, c = y_first),
    "Cubic" = list(a = 0.00001, b = 0.001, c = (y_last - y_first) / t_range, d = y_first),
    "Logarithmic" = list(a = (y_last - y_first) / log(t_range + 1), b = y_first),
    "Power" = list(a = y_mean_pos, b = 1),
    "Exponential" = list(r = max(0.001, r_est), N0 = y_first_pos, t0 = t_min),
    "ExponentialOffset" = list(a = 0, b = y_mean_pos, r = r_est),
    "Logistic" = list(K = y_positive * 1.1, r = max(0.01, r_est), t0 = (t_min + t_max) / 2),
    "Logistic4P" = list(a = 0, K = y_positive * 1.1, r = max(0.01, r_est), t0 = (t_min + t_max) / 2),
    "Gompertz" = list(K = y_positive * 1.1, r = max(0.01, r_est * 2), t0 = t_min),
    "GompertzOffset" = list(a = 0, K = y_positive * 1.1, r = max(0.01, r_est * 2), t0 = t_min),
    "Richards" = list(K = y_positive * 1.1, r = max(0.01, r_est), t0 = mean(t), v = 1),
    "Weibull" = list(K = y_positive * 1.1, r = 1, t0 = t_min, alpha = t_range / 3),
    "VonBertalanffy" = list(Linf = y_positive * 1.1, k = max(0.01, r_est), t0 = t_min),
    "Beta" = list(K = y_positive, alpha = t_range / 3, beta = 2, t0 = t_min),
    "Gamma" = list(K = y_positive, alpha = t_range / 3, beta = 2, t0 = t_min),
    "ChapmanRichards" = list(K = y_positive * 1.1, r = max(0.01, r_est), t0 = t_min, m = 2),
    "Korf" = list(K = y_positive * 1.1, r = 1, beta = 0.5),
    "MMF" = list(K = y_positive * 1.1, a = 0.001, b = t_range / 2, d = 2),
    "Stannard" = list(K = y_positive * 1.1, a = 1, b = 0.5),
    "Hill" = list(K = y_positive * 1.1, K50 = mean(t), n = 2),
    "Monod" = list(K = y_positive * 1.1, Ks = mean(t) / 2),
    "Baranyi" = list(y0 = y_first_pos, y_max = y_positive * 1.1, h0 = 0, lambda = r_est),
    "Ratkowsky" = list(a = y_positive * 1.1, b = r_est, t0 = t_min, c = 1),
    "Hossfeld" = list(K = y_positive * 1.1, a = 0.001, b = 0.0001),
    "Asymptotic" = list(a = y_positive * 1.1, b = y_positive, c = r_est),
    "Fourier" = list(a0 = y_mean_pos, a1 = (y_positive - max(y_min, 0.001)) / 4, b1 = 0, w = 2 * pi / 10),
    "Fourier2" = list(a0 = y_mean_pos, a1 = (y_positive - max(y_min, 0.001)) / 4, b1 = 0, a2 = (y_positive - max(y_min, 0.001)) / 8, b2 = 0, w = 2 * pi / 10),
    "Gaussian" = list(A = y_positive, mu = mean(t), sigma = t_range / 4),
    "Spline" = list(df = min(5, length(t) - 1)),
    list()
  )
}
