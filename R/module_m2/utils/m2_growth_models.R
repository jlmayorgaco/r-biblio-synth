# ============================================================================
# module_m2/utils/m2_growth_models.R - Growth model functions (CLEANED)
# ============================================================================

# Linear
m2_linear <- function(t, a, b) { a * t + b }

# Exponential
m2_exponential <- function(t, r, N0, t0) { N0 * exp(r * (t - t0)) }

# Logarithmic
m2_logarithmic <- function(t, a, b) { a * log(t) + b }

# Logistic
m2_logistic <- function(t, K, r, t0) { K / (1 + exp(-r * (t - t0))) }

# Gompertz
m2_gompertz <- function(t, N0, Nmax, k, t0, y0) {
  y0 + N0 * exp(log(Nmax / N0) * exp(-k * (t - t0)))
}

# Weibull (correct parameterization)
m2_weibull <- function(t, K, r, t0, alpha) { K * (1 - exp(-((t - t0) / alpha)^r)) }

# Von Bertalanffy
m2_vonbertalanffy <- function(t, Linf, k, t0) { Linf * (1 - exp(-k * (t - t0))) }

# Normal (Gaussian)
m2_normal <- function(t, mu, sigma, A) { A * exp(-0.5 * ((t - mu) / sigma)^2) }

# Richards (generalized logistic)
m2_richards <- function(t, K, r, t0, nu) { K / (1 + exp(-r * (t - t0)))^nu }
