# ============================================================================
# safe_division.R - Safe Division Operations
# ============================================================================
# Provides safe division functions that handle edge cases like division by zero

#' Safe division function
#' Handles division by zero and NA values
#'
#' @param numerator Numeric value or vector
#' @param denominator Numeric value or vector
#' @param default Value to return when division by zero occurs (default 0)
#' @return Result of division or default if division by zero
#' @export
safe_divide <- function(numerator, denominator, default = 0) {
  result <- numerator / denominator
  result[is.na(result) | is.nan(result) | is.infinite(result)] <- default
  result[denominator == 0] <- default
  result
}

#' Safe proportion calculation
#' Handles cases where total might be zero
#'
#' @param part Numeric value
#' @param total Numeric value
#' @param default Value to return when total is zero (default 0)
#' @return Proportion or default
#' @export
safe_proportion <- function(part, total, default = 0) {
  if (is.null(total) || is.na(total) || total == 0) {
    return(default)
  }
  result <- part / total
  if (is.na(result) || is.nan(result) || is.infinite(result)) {
    return(default)
  }
  result
}

#' Safe percentage calculation
#'
#' @param part Numeric value
#' @param total Numeric value
#' @param default Value to return when total is zero (default NA)
#' @return Percentage or default
#' @export
safe_percentage <- function(part, total, default = NA) {
  safe_proportion(part, total, default) * 100
}

#' Safe mean with required minimum
#'
#' @param x Numeric vector
#' @param min_n Minimum number of values required (default 1)
#' @param default Value to return if insufficient data (default NA)
#' @return Mean or default
#' @export
safe_mean <- function(x, min_n = 1, default = NA) {
  x <- x[!is.na(x)]
  if (length(x) < min_n) {
    return(default)
  }
  mean(x)
}

#' Safe standard deviation
#'
#' @param x Numeric vector
#' @param min_n Minimum number of values required (default 2)
#' @param default Value to return if insufficient data (default NA)
#' @return Standard deviation or default
#' @export
safe_sd <- function(x, min_n = 2, default = NA) {
  x <- x[!is.na(x)]
  if (length(x) < min_n) {
    return(default)
  }
  sd(x)
}

#' Safe variance
#'
#' @param x Numeric vector
#' @param min_n Minimum number of values required (default 2)
#' @param default Value to return if insufficient data (default NA)
#' @return Variance or default
#' @export
safe_var <- function(x, min_n = 2, default = NA) {
  x <- x[!is.na(x)]
  if (length(x) < min_n) {
    return(default)
  }
  var(x)
}

#' Safe coefficient of variation (CV)
#' Handles cases where mean is zero
#'
#' @param x Numeric vector
#' @param min_n Minimum number of values required (default 2)
#' @param default Value to return if insufficient data (default NA)
#' @return CV or default
#' @export
safe_cv <- function(x, min_n = 2, default = NA) {
  x <- x[!is.na(x)]
  if (length(x) < min_n) {
    return(default)
  }
  m <- mean(x)
  if (m == 0) {
    return(default)
  }
  s <- sd(x)
  s / abs(m)
}

#' Safe log transform
#' Handles zero and negative values
#'
#' @param x Numeric value or vector
#' @param offset Small offset to add before log (default 1)
#' @return Log-transformed value
#' @export
safe_log <- function(x, offset = 1) {
  if (is.null(x) || all(is.na(x))) {
    return(rep(NA, length(x)))
  }
  log(pmax(offset, x))
}

#' Safe log10 transform
#'
#' @param x Numeric value or vector
#' @param offset Small offset to add before log (default 0.001)
#' @return Log10-transformed value
#' @export
safe_log10 <- function(x, offset = 0.001) {
  if (is.null(x) || all(is.na(x))) {
    return(rep(NA, length(x)))
  }
  log10(pmax(offset, x))
}

#' Safe Gini coefficient calculation
#'
#' @param x Numeric vector
#' @param default Value to return if calculation fails (default NA)
#' @param na.rm Logical; if TRUE, remove NA values (default TRUE)
#' @param warn_neg Logical; if TRUE, warn when negative values are removed (default TRUE)
#' @return Gini coefficient
#' @export
safe_gini <- function(x, default = NA, na.rm = TRUE, warn_neg = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (any(is.na(x))) {
    return(default)
  }
  
  n <- length(x)
  
  if (n < 2) {
    return(default)
  }
  
  # Handle negative values: Gini is undefined for negative values
  # We take absolute values but warn since negative values are unusual
  has_neg <- any(x < 0)
  if (has_neg) {
    if (warn_neg) {
      warning("Gini: negative values detected and converted to absolute values. ",
              "Consider filtering your data.")
    }
    x <- abs(x)
  }
  
  if (all(x == 0)) {
    return(0)  # All zeros = perfect equality
  }
  
  if (all(x == x[1])) {
    return(0)  # Perfect equality
  }
  
  x <- sort(x)
  
  # Standard Gini coefficient formula
  # Handle case where sum(x) might be zero due to zeros in data
  sum_x <- sum(x)
  if (sum_x == 0) {
    return(default)
  }
  
  gini <- (n + 1 - 2 * sum((n + 1 - seq_len(n)) * x) / (n * sum_x)) / n
  
  if (is.na(gini) || is.nan(gini) || is.infinite(gini)) {
    return(default)
  }
  
  max(0, min(1, gini))
}

#' Safe correlation
#'
#' @param x Numeric vector
#' @param y Numeric vector
#' @param method Correlation method: "pearson", "kendall", "spearman"
#' @param min_n Minimum number of paired values (default 3)
#' @param default Value to return if calculation fails (default NA)
#' @return Correlation coefficient
#' @export
safe_cor <- function(x, y, method = "pearson", min_n = 3, default = NA) {
  valid <- !is.na(x) & !is.na(y)
  x <- x[valid]
  y <- y[valid]
  
  if (length(x) < min_n) {
    return(default)
  }
  
  # Check for constant values
  if (sd(x) == 0 || sd(y) == 0) {
    return(default)
  }
  
  tryCatch({
    cor(x, y, method = method)
  }, error = function(e) {
    default
  })
}

#' Safe regression
#'
#' @param formula Regression formula
#' @param data Data frame
#' @param min_n Minimum number of observations (default 5)
#' @return List with coefficients, R2, etc., or NULL if fails
#' @export
safe_lm <- function(formula, data, min_n = 5) {
  if (nrow(data) < min_n) {
    return(NULL)
  }
  
  tryCatch({
    fit <- lm(formula, data = data)
    summary_fit <- summary(fit)
    
    list(
      coefficients = coef(fit),
      r_squared = summary_fit$r.squared,
      adj_r_squared = summary_fit$adj.r.squared,
      p_value = summary_fit$coefficients[2, 4],
      residuals = residuals(fit),
      fitted = fitted(fit),
      success = TRUE
    )
  }, error = function(e) {
    NULL
  })
}

#' Safe quantile
#'
#' @param x Numeric vector
#' @param probs Probability values (default c(0.25, 0.5, 0.75))
#' @param default Value to return if insufficient data
#' @return Quantiles or default
#' @export
safe_quantile <- function(x, probs = c(0.25, 0.5, 0.75), default = NA) {
  x <- x[!is.na(x)]
  
  if (length(x) < 2) {
    return(rep(default, length(probs)))
  }
  
  tryCatch({
    quantile(x, probs = probs, na.rm = TRUE)
  }, error = function(e) {
    rep(default, length(probs))
  })
}

#' Safe scale (z-score)
#' Handles cases where SD is zero
#'
#' @param x Numeric vector
#' @param default Value for zero variance cases
#' @return Scaled values
#' @export
safe_scale <- function(x, default = 0) {
  x <- as.numeric(x)
  s <- sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  
  if (is.na(s) || s == 0) {
    return(rep(default, length(x)))
  }
  
  (x - m) / s
}

#' Safe normalization (0-1 scale)
#'
#' @param x Numeric vector
#' @param default Value for constant cases
#' @return Normalized values
#' @export
safe_normalize <- function(x, default = 0.5) {
  x <- as.numeric(x)
  r <- range(x, na.rm = TRUE)
  
  if (is.na(r[1]) || is.na(r[2]) || r[1] == r[2]) {
    return(rep(default, length(x)))
  }
  
  (x - r[1]) / (r[2] - r[1])
}

#' Check if numeric vector has enough variance
#'
#' @param x Numeric vector
#' @param threshold Minimum variance threshold
#' @return TRUE if variance is sufficient, FALSE otherwise
#' @export
has_variance <- function(x, threshold = 1e-10) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(FALSE)
  var(x) > threshold
}

#' Clamp values to range
#'
#' @param x Numeric vector
#' @param lower Lower bound
#' @param upper Upper bound
#' @return Clamped values
#' @export
clamp <- function(x, lower = -Inf, upper = Inf) {
  pmax(lower, pmin(upper, x))
}