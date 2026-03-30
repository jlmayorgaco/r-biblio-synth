# ============================================================================
# statistical_tests.R - Advanced Statistical Testing Framework
# ============================================================================
# Provides hypothesis testing with multiple comparison correction,
# power analysis, and effect size reporting

#' Apply multiple testing correction to hypothesis results
#'
#' @param hypotheses List of hypothesis test results
#' @param method Correction method: "fdr" (BH), "bonferroni", "holm", "none"
#' @return Updated hypotheses with adjusted p-values
#' @export
apply_multiple_testing_correction <- function(hypotheses, method = "fdr") {
  if (!is.list(hypotheses) || length(hypotheses) == 0) {
    return(hypotheses)
  }
  
  # Extract p-values
  p_values <- sapply(hypotheses, function(h) {
    if (is.list(h) && "p_value" %in% names(h)) {
      h$p_value
    } else {
      NA
    }
  })
  
  # Apply correction
  if (method != "none" && length(p_values) > 1) {
    # Remove NAs for correction
    valid_idx <- !is.na(p_values)
    adjusted <- numeric(length(p_values))
    adjusted[!valid_idx] <- NA
    
    if (sum(valid_idx) > 0) {
      adjusted[valid_idx] <- p.adjust(p_values[valid_idx], method = method)
    }
  } else {
    adjusted <- p_values
  }
  
  # Update hypotheses
  for (i in seq_along(hypotheses)) {
    if (is.list(hypotheses[[i]]) && "p_value" %in% names(hypotheses[[i]])) {
      hypotheses[[i]]$p_value_adjusted <- adjusted[i]
      hypotheses[[i]]$correction_method <- method
      
      # Update significance based on adjusted p-value
      if (!is.na(adjusted[i])) {
        hypotheses[[i]]$significant_adjusted <- adjusted[i] < 0.05
      }
    }
  }
  
  hypotheses
}

#' Compute statistical power for a test
#'
#' @param effect_size Cohen's d effect size
#' @param n Sample size
#' @param alpha Significance level
#' @param test_type Type of test: "t", "correlation", "anova"
#' @return Power estimate
#' @export
compute_statistical_power <- function(effect_size, n, alpha = 0.05, test_type = "t") {
  if (effect_size == 0 || n < 2) return(0)
  
  if (test_type == "t") {
    # Power for two-sample t-test
    df <- n - 2
    ncp <- effect_size * sqrt(n / 2)
    crit <- qt(1 - alpha / 2, df)
    power <- 1 - pt(crit, df, ncp) + pt(-crit, df, ncp)
    return(power)
  } else if (test_type == "correlation") {
    # Power for correlation test
    z_r <- atanh(effect_size)  # Fisher's z
    se <- 1 / sqrt(n - 3)
    z_crit <- qnorm(1 - alpha / 2)
    power <- pnorm(z_r / se - z_crit) + pnorm(-z_r / se - z_crit)
    return(power)
  } else if (test_type == "anova") {
    # Power for one-way ANOVA
    # Cohen's f for ANOVA
    f <- effect_size
    df1 <- 1
    df2 <- n - 2
    lambda <- f^2 * n
    crit <- qf(1 - alpha, df1, df2)
    power <- 1 - pf(crit, df1, df2, lambda)
    return(power)
  }
  
  NA
}

#' Kolmogorov-Smirnov test for distribution fitting
#'
#' @param x Data vector
#' @param distribution Distribution to test: "norm", "lnorm", "exp", "gamma", "weibull"
#' @return Test results with p-value
#' @export
ks_distribution_test <- function(x, distribution = "lnorm") {
  x <- x[!is.na(x) & x > 0]
  
  if (length(x) < 10) {
    return(list(
      statistic = NA,
      p_value = NA,
      distribution = distribution,
      parameters = NULL,
      status = "error: insufficient data"
    ))
  }
  
  # Fit distribution
  fit_result <- tryCatch({
    if (distribution == "norm") {
      params <- fitdistrplus::fitdist(x, "norm")
      list(mean = params$estimate["mean"], sd = params$estimate["sd"])
    } else if (distribution == "lnorm") {
      params <- fitdistrplus::fitdist(x, "lnorm")
      list(meanlog = params$estimate["meanlog"], sdlog = params$estimate["sdlog"])
    } else if (distribution == "exp") {
      params <- fitdistrplus::fitdist(x, "exp")
      list(rate = params$estimate["rate"])
    } else if (distribution == "gamma") {
      params <- fitdistrplus::fitdist(x, "gamma")
      list(shape = params$estimate["shape"], rate = params$estimate["rate"])
    } else if (distribution == "weibull") {
      params <- fitdistrplus::fitdist(x, "weibull")
      list(shape = params$estimate["shape"], scale = params$estimate["scale"])
    }
  }, error = function(e) {
    # Fallback to simple estimation
    if (distribution == "norm") {
      list(mean = mean(x), sd = sd(x))
    } else if (distribution == "lnorm") {
      log_x <- log(x)
      list(meanlog = mean(log_x), sdlog = sd(log_x))
    } else if (distribution == "exp") {
      list(rate = 1 / mean(x))
    } else if (distribution == "gamma") {
      m <- mean(x)
      v <- var(x)
      list(shape = m^2 / v, rate = m / v)
    } else if (distribution == "weibull") {
      # Method of moments approximation
      list(shape = 1, scale = mean(x))
    }
  })
  
  # Run KS test
  ks_result <- tryCatch({
    if (distribution == "norm") {
      ks.test(x, "pnorm", mean = fit_result$mean, sd = fit_result$sd)
    } else if (distribution == "lnorm") {
      ks.test(x, "plnorm", meanlog = fit_result$meanlog, sdlog = fit_result$sdlog)
    } else if (distribution == "exp") {
      ks.test(x, "pexp", rate = fit_result$rate)
    } else if (distribution == "gamma") {
      ks.test(x, "pgamma", shape = fit_result$shape, rate = fit_result$rate)
    } else if (distribution == "weibull") {
      ks.test(x, "pweibull", shape = fit_result$shape, scale = fit_result$scale)
    }
  }, error = function(e) NULL)
  
  if (is.null(ks_result)) {
    return(list(
      statistic = NA,
      p_value = NA,
      distribution = distribution,
      parameters = fit_result,
      status = "error: KS test failed"
    ))
  }
  
  list(
    statistic = ks_result$statistic,
    p_value = ks_result$p.value,
    distribution = distribution,
    parameters = fit_result,
    significant = ks_result$p.value < 0.05,
    interpretation = if (ks_result$p.value < 0.05) {
      sprintf("Data does NOT fit %s distribution (p = %.4f)", distribution, ks_result$p.value)
    } else {
      sprintf("Data is consistent with %s distribution (p = %.4f)", distribution, ks_result$p.value)
    },
    status = "success"
  )
}

#' Anderson-Darling normality test
#'
#' @param x Data vector
#' @return Test results
#' @export
anderson_darling_test <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  
  if (n < 8) {
    return(list(
      statistic = NA,
      p_value = NA,
      normal = NA,
      status = "error: insufficient data (n <8)"
    ))
  }
  
  # If nortest is available, use it
  ad_result <- tryCatch({
    if (requireNamespace("nortest", quietly = TRUE)) {
      test <- nortest::ad.test(x)
      list(statistic = test$statistic, p_value = test$p.value)
    } else {
      # Manual calculation
      mean_x <- mean(x)
      sd_x <- sd(x)
      y <- sort((x - mean_x) / sd_x)
      log_p <- pnorm(y, log.p = TRUE)
      log_q <- pnorm(-y, log.p = TRUE)
      AD <- -n - sum((2 * (1:n) - 1) * (log_p + rev(log_q))) / n
      
      # Adjusted p-value (approximation)
      AD_star <- AD * (1 + 0.75/n + 2.25/n^2)
      p_value <- exp(-1.2337141 - 1.096497 * AD_star)
      p_value <- min(1, max(0, p_value))
      
      list(statistic = AD, p_value = p_value)
    }
  }, error = function(e) {
    list(statistic = NA, p_value = NA)
  })
  
  list(
    statistic = ad_result$statistic,
    p_value = ad_result$p_value,
    normal = ad_result$p_value > 0.05,
    interpretation = if (ad_result$p_value > 0.05) {
      "Data is consistent with normal distribution"
    } else {
      "Data deviates significantly from normal distribution"
    },
    status = "success"
  )
}

#' Chi-square goodness of fit test
#'
#' @param observed Vector of observed counts
#' @param expected Vector of expected counts (or "equal" for uniform)
#' @return Test results
#' @export
chi_square_gof_test <- function(observed, expected = "equal") {
  observed <- observed[!is.na(observed)]
  n <- length(observed)
  
  if (n < 2) {
    return(list(
      statistic = NA,
      p_value = NA,
      df = NA,
      status = "error: insufficient categories"
    ))
  }
  
  if (is.character(expected) && expected == "equal") {
    expected <- rep(sum(observed) / n, n)
  }
  
  # Chi-square statistic
  chi_sq <- sum((observed - expected)^2 / expected)
  df <- n - 1
  p_value <- 1 - pchisq(chi_sq, df)
  
  list(
    statistic = chi_sq,
    p_value = p_value,
    df = df,
    observed = observed,
    expected = expected,
    significant = p_value < 0.05,
    interpretation = if (p_value < 0.05) {
      sprintf("Observed distribution differs from expected (chi-sq = %.2f, p = %.4f)", chi_sq, p_value)
    } else {
      sprintf("Observed distribution matches expected (chi-sq = %.2f, p = %.4f)", chi_sq, p_value)
    },
    status = "success"
  )
}

#' Benford's Law test for citation counts
#'
#' @param x Numeric vector (citation counts)
#' @return Test results
#' @export
benford_test <- function(x) {
  x <- x[!is.na(x) & x > 0]
  
  if (length(x) < 30) {
    return(list(
      statistic = NA,
      p_value = NA,
      significant = NA,
      status = "error: insufficient data (n< 30)"
    ))
  }
  
  # Extract first digits
  first_digits <- as.numeric(substr(as.character(x), 1, 1))
  first_digits <- first_digits[first_digits %in% 1:9]
  
  if (length(first_digits) < 30) {
    return(list(
      statistic = NA,
      p_value = NA,
      status = "error: insufficient valid first digits"
    ))
  }
  
  # Observed frequencies
  observed <- table(factor(first_digits, levels = 1:9))
  
  # Benford's Law expected frequencies
  expected <- length(first_digits) * log10(1 + 1 / (1:9))
  
  # Chi-square test
  chi_sq <- sum((observed - expected)^2 / expected)
  df <-8
  p_value <- 1 - pchisq(chi_sq, df)
  
  # Deviation from Benford's Law by digit
  deviation <- (observed - expected) / expected
  
  list(
    statistic = chi_sq,
    p_value = p_value,
    df = df,
    observed = observed,
    expected = expected,
    deviation = deviation,
    significant = p_value < 0.05,
    follows_benford = p_value >= 0.05,
    interpretation = if (p_value >= 0.05) {
      "Citation counts follow Benford's Law (no manipulation detected)"
    } else {
      "Citation counts deviate from Benford's Law (potential anomaly)"
    },
    status = "success"
  )
}

#' Levene's test for homogeneity of variance
#'
#' @param groups List of vectors to compare
#' @return Test results
#' @export
levene_test <- function(groups) {
  if (!is.list(groups) || length(groups) < 2) {
    return(list(
      statistic = NA,
      p_value = NA,
      status = "error: need at least 2 groups"
    ))
  }
  
  # Remove NAs
  groups <- lapply(groups, function(g) g[!is.na(g)])
  
  # Check minimum size
  if (any(sapply(groups, length) < 2)) {
    return(list(
      statistic = NA,
      p_value = NA,
      status = "error: each group needs at least 2 observations"
    ))
  }
  
  # Compute group medians
  medians <- sapply(groups, median)
  
  # Compute absolute deviations from median
  deviations <- lapply(seq_along(groups), function(i) {
    abs(groups[[i]] - medians[i])
  })
  
  # One-way ANOVA on deviations
  all_dev <- unlist(deviations)
  group_labels <- rep(seq_along(groups), sapply(groups, length))
  
  # Calculate ANOVA
  k <- length(groups)
  n_total <- length(all_dev)
  grand_mean <- mean(all_dev)
  
  group_means <- sapply(deviations, mean)
  group_sizes <- sapply(groups, length)
  
  ss_between <- sum(group_sizes * (group_means - grand_mean)^2)
  ss_within <- sum(sapply(seq_along(groups), function(i) {
    sum((deviations[[i]] - group_means[i])^2)
  }))
  
  df_between <- k - 1
  df_within <- n_total - k
  
  ms_between <- ss_between / df_between
  ms_within <- ss_within / df_within
  
  F_stat <- ms_between / ms_within
  p_value <- 1 - pf(F_stat, df_between, df_within)
  
  list(
    statistic = F_stat,
    p_value = p_value,
    df1 = df_between,
    df2 = df_within,
    homogeneous = p_value >= 0.05,
    interpretation = if (p_value >= 0.05) {
      "Variances are homogeneous (Levene's test p >= 0.05)"
    } else {
      "Variances are heterogeneous (Levene's test p <0.05)"
    },
    status = "success"
  )
}

#' Walsh test for multiple outliers
#'
#' @param x Numeric vector
#' @param alpha Significance level
#' @return Indices of outliers
#' @export
walsh_outlier_test <- function(x, alpha = 0.05) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  
  if (n < 10) {
    return(list(
      outliers = integer(0),
      n_outliers = 0,
      status = "error: insufficient data"
    ))
  }
  
  # Walsh test
  k <- ceiling(sqrt(n))
  lower_threshold <- x[k + 1] - (x[n - k] - x[k + 1]) / (n - 2 * k - 1) * qnorm(1 - alpha / 2)
  upper_threshold <- x[n - k] + (x[n - k] - x[k + 1]) / (n - 2 * k - 1) * qnorm(1 - alpha / 2)
  
  outliers <- which(x < lower_threshold | x > upper_threshold)
  
  list(
    outliers = outliers,
    outlier_values = x[outliers],
    lower_threshold = lower_threshold,
    upper_threshold = upper_threshold,
    n_outliers = length(outliers),
    status = "success"
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b