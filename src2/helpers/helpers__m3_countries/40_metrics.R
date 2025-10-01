# helpers/helpers__m3_countries/40_metrics.R
# Metrics helpers for M3_Countries (pure, testable)

# ---- Safe Theil wrapper ------------------------------------------------------
# Uses ineq::Theil() when available; otherwise a manual implementation:
#   T = (1/n) * sum( (x_i/μ) * log(x_i/μ) )
# Handles zeros by adding a tiny epsilon to avoid log(0).
m3c_theil_safe <- function(x) {
  x <- x[is.finite(x) & !is.na(x)]
  if (!length(x)) return(NA_real_)
  eps <- .Machine$double.eps
  x <- ifelse(x <= 0, eps, x)

  # Preferred path
  if ("Theil" %in% getNamespaceExports("ineq")) {
    return(ineq::Theil(x))
  }

  # Fallback manual calculation
  mu <- mean(x)
  if (!is.finite(mu) || mu <= 0) return(NA_real_)
  y  <- x / mu
  (1 / length(y)) * sum(y * log(y))
}

# ---- Country-level indicators -----------------------------------------------
# Input: country-year dataframe with columns country, TP, TC
# Output: dataframe per country with TP, TC, CPP, RCA, RCR
m3c_compute_indicators <- function(df_cy) {
  agg <- df_cy %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      TP = sum(TP, na.rm = TRUE),
      TC = sum(TC, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(CPP = ifelse(TP > 0, TC / TP, 0))

  total_tp <- sum(agg$TP)
  total_tc <- sum(agg$TC)

  agg %>%
    dplyr::mutate(
      RCA = (TP / total_tp) / mean(TP / total_tp),
      RCR = (TC / total_tc) / mean(TC / total_tc)
    )
}

# ---- Inequality metrics snapshot --------------------------------------------
# Input: output of m3c_compute_indicators()
# Output: named list with gini/theil/hhi for TP and TC
m3c_compute_inequality <- function(ind) {
  list(
    gini_tp  = ineq::Gini(ind$TP),
    gini_tc  = ineq::Gini(ind$TC),
    theil_tp = m3c_theil_safe(ind$TP),
    theil_tc = m3c_theil_safe(ind$TC),
    hhi_tp   = sum((ind$TP / sum(ind$TP))^2),
    hhi_tc   = sum((ind$TC / sum(ind$TC))^2)
  )
}
