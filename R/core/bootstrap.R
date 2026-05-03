# ============================================================================
# bootstrap.R - Bootstrap and Package Loading for Bibliometric Analysis
# ============================================================================
# Provides bootstrap methods for computing confidence intervals
# for all key metrics across modules
# Also handles package loading and sourcing all project R files on startup

.project_root <- NULL
.bootstrap_dir <- NULL

#' Collect project R files in deterministic order for development workflows
#'
#' Split source files under subdirectories are treated as the editable source of
#' truth. Root-level `zz_*.R` overrides are loaded last so they can patch
#' generated/legacy behavior. Any legacy `R/000_package_flat.R` artifact is
#' excluded by default to avoid sourcing the flat package artifact together with
#' the split tree.
#'
#' @param project_root Project root directory.
#' @param include_flat Logical. Include `R/000_package_flat.R`.
#' @return Character vector of absolute paths.
#' @keywords internal
rbiblio_collect_r_files <- function(project_root = getwd(), include_flat = FALSE) {
  r_dir <- file.path(project_root, "R")
  if (!dir.exists(r_dir)) {
    return(character(0))
  }

  files <- list.files(r_dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  files <- normalizePath(files, winslash = "/", mustWork = FALSE)

  bootstrap_path <- normalizePath(file.path(r_dir, "core", "bootstrap.R"), winslash = "/", mustWork = FALSE)
  flat_path <- normalizePath(file.path(r_dir, "000_package_flat.R"), winslash = "/", mustWork = FALSE)

  files <- files[files != bootstrap_path]
  if (!isTRUE(include_flat)) {
    files <- files[files != flat_path]
  }

  files <- files[!grepl("(^|/)(validate|test)[^/]*\\.R$", files, ignore.case = TRUE)]

  r_dir_norm <- normalizePath(r_dir, winslash = "/", mustWork = FALSE)
  rel <- files
  prefix <- paste0(r_dir_norm, "/")
  rel[startsWith(rel, prefix)] <- substring(rel[startsWith(rel, prefix)], nchar(prefix) + 1L)
  rel <- gsub("\\\\", "/", rel)

  root_files <- files[!grepl("/", rel, fixed = TRUE)]
  subdir_files <- files[grepl("/", rel, fixed = TRUE)]
  root_names <- basename(root_files)

  root_regular <- root_files[!grepl("^zz_.*\\.R$", root_names, ignore.case = TRUE)]
  root_overrides <- root_files[grepl("^zz_.*\\.R$", root_names, ignore.case = TRUE)]

  c(sort(subdir_files), sort(root_regular), sort(root_overrides))
}

#' Bootstrap the project for local development
#'
#' Explicitly installs missing dependencies and sources the `R/` tree. This is
#' intended for contributor workflows and is intentionally not executed when the
#' package is loaded through `library()` or `pkgload::load_all()`.
#'
#' @param project_root Project root directory.
#' @param install_deps Logical. If TRUE, run dependency bootstrap.
#' @param quiet Logical. Forwarded to dependency bootstrap.
#' @return Invisibly returns TRUE on success.
bootstrap_project <- function(project_root = getwd(),
                              install_deps = TRUE,
                              quiet = TRUE) {
  bootstrap_dir <- file.path(project_root, "R", "core")
  auto_install_path <- file.path(project_root, "R", "core", "auto_install.R")
  desc_path <- file.path(project_root, "DESCRIPTION")

  if (install_deps) {
    if (file.exists(auto_install_path)) {
      source(auto_install_path, local = TRUE)
      auto_install_dependencies(desc_path = desc_path, quiet = quiet)
    } else {
      warning("Could not find auto_install.R - packages may not be loaded properly")
    }
  }

  r_files <- rbiblio_collect_r_files(project_root = project_root, include_flat = FALSE)
  if (length(r_files) > 0) {
    for (f in r_files) {
      tryCatch(source(f), error = function(e) {
        warning("Failed to source ", f, ": ", e$message)
      })
    }
  }

  invisible(TRUE)
}

#' Bootstrap confidence interval for a statistic
#'
#' @param data Vector or data frame to bootstrap
#' @param statistic Function to compute statistic
#' @param R Number of bootstrap replicates (default 1000)
#' @param conf_level Confidence level (default 0.95)
#' @param method Bootstrap method: "percentile", "bca", "normal"
#' @param seed Random seed for reproducibility. If NULL (default), no seed is set.
#' @return List with CI bounds, estimate, and bootstrap samples
#' @export
bootstrap_ci <- function(data, statistic, R = 1000, conf_level = 0.95, method = "percentile", seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (!is.data.frame(data)) {
    data <- data[!is.na(data)]
  }
  
  n <- if (is.data.frame(data)) nrow(data) else length(data)
  
  # Compute original estimate
  estimate <- bootstrap_eval_statistic(data, statistic)
  
  if (is.na(estimate) || is.null(estimate)) {
    return(list(
      estimate = NA,
      lower = NA,
      upper = NA,
      se = NA,
      method = method,
      n_boot = 0,
      status = "error: statistic returned NA"
    ))
  }
  
  # Bootstrap replicates
  boot_samples <- numeric(R)
  for (i in 1:R) {
    boot_data <- if (is.data.frame(data)) {
      data[sample(n, replace = TRUE), ]
    } else {
      sample(data, replace = TRUE)
    }
    boot_samples[i] <- bootstrap_eval_statistic(boot_data, statistic)
  }
  
  # Remove NAs
  boot_samples <- boot_samples[!is.na(boot_samples)]
  n_valid <- length(boot_samples)
  
  if (n_valid < 10) {
    return(list(
      estimate = estimate,
      lower = NA,
      upper = NA,
      se = NA,
      method = method,
      n_boot = n_valid,
      status = "error: too few valid bootstrap samples"
    ))
  }
  
  # Compute CI based on method
  alpha <- 1 - conf_level
  
  if (method == "percentile") {
    lower <- quantile(boot_samples, alpha / 2, na.rm = TRUE)
    upper <- quantile(boot_samples, 1 - alpha / 2, na.rm = TRUE)
  } else if (method == "bca") {
    # Bias-corrected and accelerated
    z0 <- qnorm(mean(boot_samples < estimate, na.rm = TRUE))
    
    # Acceleration (jackknife)
    jack_vals <- numeric(min(n, 100))
    for (j in 1:length(jack_vals)) {
      jack_data <- if (is.data.frame(data)) {
        data[-j, ]
      } else {
        data[-j]
      }
      jack_vals[j] <- tryCatch(statistic(jack_data), error = function(e) NA)
    }
    jack_vals <- jack_vals[!is.na(jack_vals)]
    a <- sum((mean(jack_vals) - jack_vals)^3) / (6 * sum((mean(jack_vals) - jack_vals)^2)^1.5)
    
    z_alpha <- qnorm(c(alpha / 2, 1 - alpha / 2))
    z_bounds <- z0 + (z0 + z_alpha) / (1 - a * (z0 + z_alpha))
    lower <- quantile(boot_samples, pnorm(z_bounds[1]), na.rm = TRUE)
    upper <- quantile(boot_samples, pnorm(z_bounds[2]), na.rm = TRUE)
  } else {
    # Normal approximation
    se_boot <- sd(boot_samples, na.rm = TRUE)
    z <- qnorm(1 - alpha / 2)
    lower <- estimate - z * se_boot
    upper <- estimate + z * se_boot
  }
  
  list(
    estimate = estimate,
    lower = as.numeric(lower),
    upper = as.numeric(upper),
    se = sd(boot_samples, na.rm = TRUE),
    bias = mean(boot_samples, na.rm = TRUE) - estimate,
    method = method,
    n_boot = n_valid,
    conf_level = conf_level,
    boot_samples = boot_samples,
    status = "success"
  )
}

#' Evaluate a bootstrap statistic with NA-aware fallback
#' @keywords internal
bootstrap_eval_statistic <- function(data, statistic) {
  value <- tryCatch(statistic(data), error = function(e) NA_real_)

  if (!is.data.frame(data) && length(value) == 1L && is.na(value)) {
    value <- tryCatch(statistic(data, na.rm = TRUE), error = function(e) NA_real_)
  }

  suppressWarnings(as.numeric(value)[1])
}

#' Bootstrap CI for mean
#' @export
bootstrap_mean_ci <- function(x, R = 1000, conf_level = 0.95, seed = NULL) {
  bootstrap_ci(x, mean, R = R, conf_level = conf_level, method = "percentile", seed = seed)
}

#' Bootstrap CI for median
#' @export
bootstrap_median_ci <- function(x, R = 1000, conf_level = 0.95, seed = NULL) {
  bootstrap_ci(x, median, R = R, conf_level = conf_level, method = "percentile", seed = seed)
}

#' Bootstrap CI for proportion
#' @export
bootstrap_proportion_ci <- function(x, R = 1000, conf_level = 0.95, seed = NULL) {
  bootstrap_ci(x, mean, R = R, conf_level = conf_level, method = "percentile", seed = seed)
}

#' Bootstrap CI for mean
#' @export
bootstrap_mean <- function(x, R = 1000, conf_level = 0.95, seed = NULL) {
  bootstrap_mean_ci(x, R = R, conf_level = conf_level, seed = seed)
}

#' Bootstrap CI for median
#' @export
bootstrap_median <- function(x, R = 1000, conf_level = 0.95, seed = NULL) {
  bootstrap_median_ci(x, R = R, conf_level = conf_level, seed = seed)
}

#' Bootstrap CI for proportion
#' @export
bootstrap_proportion <- function(successes, trials = NULL, R = 1000, conf_level = 0.95, seed = NULL) {
  x <- if (is.null(trials)) {
    successes
  } else if (length(successes) == 1L && length(trials) == 1L) {
    c(rep(1, max(0, as.integer(successes))), rep(0, max(0, as.integer(trials - successes))))
  } else {
    suppressWarnings(as.numeric(successes) / as.numeric(trials))
  }

  bootstrap_proportion_ci(x, R = R, conf_level = conf_level, seed = seed)
}

#' Bootstrap CI for correlation
#' @export
bootstrap_correlation_ci <- function(x, y, method = "pearson", R = 1000, conf_level = 0.95, seed = NULL) {
  data <- data.frame(x = x, y = y)
  stat <- function(d) cor(d$x, d$y, method = method, use = "complete.obs")
  bootstrap_ci(data, stat, R = R, conf_level = conf_level, method = "bca", seed = seed)
}

#' Bootstrap CI for regression coefficient
#' @export
bootstrap_regression_ci <- function(formula, data, R = 1000, conf_level = 0.95, seed = NULL) {
  stat <- function(d) {
    fit <- tryCatch(lm(formula, data = d), error = function(e) NULL)
    if (is.null(fit)) return(NA)
    coef(fit)[2]  # Slope coefficient
  }
  bootstrap_ci(data, stat, R = R, conf_level = conf_level, method = "bca", seed = seed)
}

#' Bootstrap CI for Gini coefficient
#' @export
bootstrap_gini_ci <- function(x, R = 1000, conf_level = 0.95, seed = NULL) {
  gini_fn <- function(d) {
    d <- sort(d[!is.na(d)])
    n <- length(d)
    if (n < 2) return(NA)
    sum_d <- sum(d)
    if (!is.finite(sum_d) || sum_d <= 0) return(NA)
    gini <- (2 * sum(seq_len(n) * d) / (n * sum_d)) - ((n + 1) / n)
    max(0, min(1, gini))
  }
  bootstrap_ci(x, gini_fn, R = R, conf_level = conf_level, method = "bca", seed = seed)
}

#' Bootstrap CI for Gini coefficient
#' @export
bootstrap_gini <- function(x, R = 1000, conf_level = 0.95, seed = NULL) {
  bootstrap_gini_ci(x, R = R, conf_level = conf_level, seed = seed)
}

#' Bootstrap CI for h-index
#' @export
bootstrap_hindex_ci <- function(citations, R = 1000, conf_level = 0.95, seed = NULL) {
  h_index_fn <- function(d) {
    d <- sort(d[!is.na(d)], decreasing = TRUE)
    h <- 0
    for (i in seq_along(d)) {
      if (d[i] >= i) h <- i else break
    }
    h
  }
  bootstrap_ci(citations, h_index_fn, R = R, conf_level = conf_level, method = "percentile", seed = seed)
}

#' Bootstrap CI for growth rate
#' @export
bootstrap_growth_rate_ci <- function(years, values, R = 1000, conf_level = 0.95, seed = NULL) {
  data <- data.frame(year = years, value = values)
  
  growth_fn <- function(d) {
    d <- d[order(d$year), ]
    fit <- tryCatch(lm(value ~ year, data = d), error = function(e) NULL)
    if (is.null(fit)) return(NA)
    coef(fit)[2]  # Slope = annual growth
  }
  
  bootstrap_ci(data, growth_fn, R = R, conf_level = conf_level, method = "bca", seed = seed)
}

#' Bootstrap CI for ratio
#' @export
bootstrap_ratio_ci <- function(numerator, denominator, R = 1000, conf_level = 0.95, seed = NULL) {
  data <- data.frame(num = numerator, denom = denominator)
  
  ratio_fn <- function(d) {
    valid <- !is.na(d$num) & !is.na(d$denom) & d$denom != 0
    if (sum(valid) < 2) return(NA)
    sum(d$num[valid]) / sum(d$denom[valid])
  }
  
  bootstrap_ci(data, ratio_fn, R = R, conf_level = conf_level, method = "percentile", seed = seed)
}

#' Multiple testing correction (FDR/Bonferroni)
#'
#' @param p_values Vector of p-values
#' @param method Correction method: "fdr" (BH), "bonferroni", "holm"
#' @return Adjusted p-values
#' @export
correct_multiple_tests <- function(p_values, method = "fdr") {
  p_values <- p_values[!is.na(p_values)]
  n <- length(p_values)
  
  if (n == 0) return(numeric(0))
  
  if (method == "bonferroni") {
    adjusted <- pmin(p_values * n, 1)
  } else if (method == "holm") {
    ordered_p <- order(p_values)
    adjusted <- numeric(n)
    for (i in seq_along(ordered_p)) {
      adjusted[ordered_p[i]] <- min(1, (n - i + 1) * p_values[ordered_p[i]])
    }
    # Enforce monotonicity
    for (i in 2:n) {
      if (adjusted[ordered_p[i]] > adjusted[ordered_p[i - 1]]) {
        adjusted[ordered_p[i]] <- adjusted[ordered_p[i - 1]]
      }
    }
  } else {
    # Benjamini-Hochberg FDR
    ordered_p <- order(p_values)
    adjusted <- numeric(n)
    for (i in n:1) {
      adjusted[ordered_p[i]] <- min(1, p_values[ordered_p[i]] * n / i)
      if (i < n && adjusted[ordered_p[i]] > adjusted[ordered_p[i + 1]]) {
        adjusted[ordered_p[i]] <- adjusted[ordered_p[i + 1]]
      }
    }
  }
  
  adjusted
}

#' Effect size calculation
#'
#' @param effect Effect magnitude
#' @param se Standard error
#' @param type Type: "d" (Cohen's d), "r" (correlation), "or" (odds ratio)
#' @return List with effect size and CI
#' @export
calculate_effect_size <- function(effect, se, type = "d") {
  if (is.na(effect) || is.na(se) || se == 0) {
    return(list(
      estimate = effect,
      se = se,
      lower = NA,
      upper = NA,
      type = type,
      interpretation = "Cannot compute"
    ))
  }
  
  z <- qnorm(0.975)
  lower <- effect - z * se
  upper <- effect + z * se
  
  interpretation <- interpret_effect_size(effect, type)
  
  list(
    estimate = effect,
    se = se,
    lower = lower,
    upper = upper,
    type = type,
    interpretation = interpretation
  )
}

#' Interpret effect size
#' @keywords internal
interpret_effect_size <- function(effect, type = "d") {
  if (type == "d") {
    if (abs(effect) < 0.2) return("Negligible")
    if (abs(effect) < 0.5) return("Small")
    if (abs(effect) < 0.8) return("Medium")
    return("Large")
  } else if (type == "r") {
    if (abs(effect) < 0.1) return("Negligible")
    if (abs(effect) < 0.3) return("Small")
    if (abs(effect) < 0.5) return("Medium")
    return("Large")
  } else if (type == "or") {
    if (abs(log(effect)) < 0.5) return("Negligible")
    if (abs(log(effect)) < 1.0) return("Small")
    if (abs(log(effect)) < 2.0) return("Medium")
    return("Large")
  }
  "Unknown effect type"
}

#' Bootstrap summary for a vector
#' @export
bootstrap_summary <- function(x, R = 1000, conf_level = 0.95, seed = NULL) {
  if (length(x) < 3 || all(is.na(x))) {
    return(list(
      mean = list(estimate = NA, lower = NA, upper = NA),
      median = list(estimate = NA, lower = NA, upper = NA),
      sd = list(estimate = NA, lower = NA, upper = NA),
      quantiles = NULL,
      status = "error: insufficient data"
    ))
  }
  
  mean_ci <- bootstrap_mean_ci(x, R = R, conf_level = conf_level, seed = seed)
  median_ci <- bootstrap_median_ci(x, R = R, conf_level = conf_level, seed = seed)
  
  sd_fn <- function(d) sd(d, na.rm = TRUE)
  sd_ci <- bootstrap_ci(x, sd_fn, R = R, conf_level = conf_level, seed = seed)
  
  quantile_fn <- function(d, probs) {
    quantile(d, probs, na.rm = TRUE)
  }
  
  list(
    mean = mean_ci,
    median = median_ci,
    sd = sd_ci,
    quantiles = list(
      q025 = quantile(x, 0.025, na.rm = TRUE),
      q25 = quantile(x, 0.25, na.rm = TRUE),
      q50 = quantile(x, 0.50, na.rm = TRUE),
      q75 = quantile(x, 0.75, na.rm = TRUE),
      q975 = quantile(x, 0.975, na.rm = TRUE)
    ),
    n = sum(!is.na(x)),
    conf_level = conf_level,
    status = "success"
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
