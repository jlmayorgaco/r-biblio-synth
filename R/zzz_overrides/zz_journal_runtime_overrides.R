# ============================================================================
# zz_journal_runtime_overrides.R - Runtime hardening for M2/M3 and dev tooling
# ============================================================================

rbiblio_has_finite <- function(x) {
  any(is.finite(suppressWarnings(as.numeric(x))))
}

rbiblio_safe_summary_lm <- function(model) {
  if (is.null(model) || !inherits(model, "lm")) {
    return(NULL)
  }
  suppressWarnings(tryCatch(summary(model), error = function(e) NULL))
}

rbiblio_safe_norm01 <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  out <- rep(NA_real_, length(x_num))
  keep <- is.finite(x_num)
  if (!any(keep)) {
    return(out)
  }
  vals <- x_num[keep]
  rng <- range(vals, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) <= .Machine$double.eps) {
    out[keep] <- 1
    return(out)
  }
  out[keep] <- (vals - rng[1]) / (rng[2] - rng[1])
  out
}

rbiblio_safe_rank_cor_test <- function(x, y, method = "spearman") {
  x <- suppressWarnings(as.numeric(x))
  y <- suppressWarnings(as.numeric(y))
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]
  if (length(x) < 4L || length(unique(x)) < 2L || length(unique(y)) < 2L) {
    return(NULL)
  }
  suppressWarnings(tryCatch(
    stats::cor.test(x, y, method = method, exact = FALSE),
    error = function(e) NULL
  ))
}

rbiblio_safe_shapiro <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) < 3L || length(x) > 5000L || length(unique(x)) < 3L) {
    return(NULL)
  }
  suppressWarnings(tryCatch(stats::shapiro.test(x), error = function(e) NULL))
}

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

bootstrap_project <- function(project_root = getwd(),
                              install_deps = TRUE,
                              quiet = TRUE) {
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

rbiblio_m1_cache_env <- local({
  env <- new.env(parent = emptyenv())
  env$cache <- list()
  env
})

get_cached_biblio_analysis <- function(input) {
  input <- m1_prepare_biblio_input(input)
  cache <- rbiblio_m1_cache_env$cache %||% list()
  cache_key <- digest::digest(input, algo = "xxhash32")

  if (!is.null(cache[[cache_key]])) {
    return(cache[[cache_key]])
  }

  result <- tryCatch({
    res <- bibliometrix::biblioAnalysis(input, sep = ";")
    s <- summary(res, pause = FALSE, verbose = FALSE)
    list(res = res, summary = s, status = "success")
  }, error = function(e) {
    list(res = NULL, summary = NULL, status = "error", error = conditionMessage(e))
  })

  cache[[cache_key]] <- result
  rbiblio_m1_cache_env$cache <- cache
  result
}

clear_biblio_cache <- function() {
  rbiblio_m1_cache_env$cache <- list()
  invisible(TRUE)
}

rbiblio_quiet_ggsave <- function(...) {
  suppressMessages(suppressWarnings(ggplot2::ggsave(...)))
}

export_plot_artifact <- function(plot, path,
                                 width = 10,
                                 height = 6,
                                 dpi = 300) {
  if (inherits(plot, "recordedplot")) {
    return(export_recorded_plot_artifact(plot, path, width = width, height = height, dpi = dpi))
  }

  if (!interactive()) {
    old_dev <- grDevices::dev.cur()
    on.exit(if (grDevices::dev.cur() != old_dev) grDevices::dev.off(), add = TRUE)
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  png_path <- paste0(path, ".png")
  svg_path <- paste0(path, ".svg")
  pdf_path <- paste0(path, ".pdf")
  eps_path <- paste0(path, ".eps")

  if (requireNamespace("Cairo", quietly = TRUE)) {
    rbiblio_quiet_ggsave(
      filename = png_path,
      plot = plot,
      width = width,
      height = height,
      dpi = dpi,
      bg = "white",
      type = "cairo"
    )
  } else {
    rbiblio_quiet_ggsave(
      filename = png_path,
      plot = plot,
      width = width,
      height = height,
      dpi = dpi,
      bg = "white"
    )
  }

  tryCatch({
    rbiblio_quiet_ggsave(
      filename = svg_path,
      plot = plot,
      width = width,
      height = height,
      device = "svg",
      bg = "white"
    )
  }, error = function(e) NULL)

  tryCatch({
    rbiblio_quiet_ggsave(
      filename = pdf_path,
      plot = plot,
      width = width,
      height = height,
      device = grDevices::pdf,
      bg = "white"
    )
  }, error = function(e) NULL)

  tryCatch({
    eps_device <- function(filename, width, height, ...) {
      grDevices::cairo_ps(
        filename = filename,
        width = width,
        height = height,
        onefile = FALSE,
        ...
      )
    }
    rbiblio_quiet_ggsave(
      filename = eps_path,
      plot = plot,
      width = width,
      height = height,
      device = eps_device,
      bg = "white"
    )
  }, error = function(e) NULL)

  invisible(c(
    png = png_path,
    svg = if (file.exists(svg_path)) svg_path else NA_character_,
    pdf = if (file.exists(pdf_path)) pdf_path else NA_character_,
    eps = if (file.exists(eps_path)) eps_path else NA_character_
  ))
}

m2_kendall_stat <- function(x) {
  x <- m2_num_vec(x)
  x <- x[is.finite(x)]
  if (length(x) < 3L || length(unique(x)) < 2L) {
    return(list(tau = NA_real_, p_value = NA_real_, status = "degenerate"))
  }
  kt <- suppressWarnings(tryCatch(
    stats::cor.test(seq_along(x), x, method = "kendall", exact = FALSE),
    error = function(e) NULL
  ))
  if (is.null(kt)) {
    return(list(tau = NA_real_, p_value = NA_real_, status = "failed"))
  }
  list(
    tau = m2_scalar_num(unname(kt$estimate)),
    p_value = m2_scalar_num(kt$p.value),
    status = "success"
  )
}

m2_breusch_pagan <- function(fitted, residuals) {
  keep <- is.finite(fitted) & is.finite(residuals)
  fitted <- fitted[keep]
  residuals <- residuals[keep]
  n <- length(residuals)

  if (n < 5L || stats::sd(residuals, na.rm = TRUE) <= .Machine$double.eps) {
    return(list(statistic = NA_real_, p_value = NA_real_))
  }

  aux <- tryCatch(stats::lm(base::I(residuals^2) ~ fitted), error = function(e) NULL)
  aux_summary <- rbiblio_safe_summary_lm(aux)
  if (is.null(aux_summary) || is.null(aux_summary$r.squared) || !is.finite(aux_summary$r.squared)) {
    return(list(statistic = NA_real_, p_value = NA_real_))
  }

  stat <- n * aux_summary$r.squared
  list(
    statistic = stat,
    p_value = 1 - stats::pchisq(stat, df = 1)
  )
}

m2_compute_card_diagnostics <- function(years, actual, fitted, residuals, n_params = 1L) {
  residuals <- m2_num_vec(residuals)
  years <- m2_num_vec(years)
  fitted <- m2_num_vec(fitted)

  keep <- is.finite(residuals)
  residuals <- residuals[keep]
  fitted <- fitted[keep]
  if (length(years) >= length(keep)) {
    years <- years[keep]
  }

  if (length(residuals) == 0) {
    return(list(
      shapiro_p = NA_real_,
      durbin_watson = NA_real_,
      ljung_box_p = NA_real_,
      breusch_pagan_p = NA_real_,
      breakpoint_count = 0L,
      breakpoint_years = numeric(0),
      max_abs_acf = NA_real_,
      stability_score = NA_real_
    ))
  }

  shapiro_p <- if (length(residuals) >= 3 && length(residuals) <= 5000 && length(unique(residuals)) >= 3) {
    suppressWarnings(tryCatch(stats::shapiro.test(residuals)$p.value, error = function(e) NA_real_))
  } else {
    NA_real_
  }

  dw <- m2_durbin_watson_stat(residuals)

  lag_max <- min(5L, length(residuals) - 1L)
  ljung_p <- if (lag_max >= 1L && stats::sd(residuals, na.rm = TRUE) > .Machine$double.eps) {
    suppressWarnings(tryCatch(stats::Box.test(residuals, lag = lag_max, type = "Ljung-Box")$p.value, error = function(e) NA_real_))
  } else {
    NA_real_
  }

  acf_vals <- if (lag_max >= 1L && stats::sd(residuals, na.rm = TRUE) > .Machine$double.eps) {
    tryCatch(stats::acf(residuals, plot = FALSE, lag.max = lag_max)$acf[-1], error = function(e) numeric(0))
  } else {
    numeric(0)
  }
  acf_vals <- m2_num_vec(acf_vals)
  acf_vals <- acf_vals[is.finite(acf_vals)]
  max_abs_acf <- if (length(acf_vals) > 0L) max(abs(acf_vals), na.rm = TRUE) else NA_real_

  bp <- m2_breusch_pagan(fitted, residuals)

  breakpoints <- if (length(residuals) >= 10 && length(years) == length(residuals)) {
    tryCatch(detect_residual_breakpoints(years, residuals), error = function(e) NULL)
  } else {
    NULL
  }
  breakpoint_years <- if (!is.null(breakpoints)) m2_num_vec(breakpoints$breakpoint_years) else numeric(0)
  breakpoint_count <- length(breakpoint_years)

  score_parts <- c(
    if (is.finite(shapiro_p)) min(1, shapiro_p / 0.05) else NA_real_,
    if (is.finite(dw)) 1 - min(1, abs(dw - 2) / 2) else NA_real_,
    if (is.finite(ljung_p)) min(1, ljung_p / 0.05) else NA_real_,
    if (is.finite(bp$p_value)) min(1, bp$p_value / 0.05) else NA_real_,
    if (is.finite(max_abs_acf)) 1 - min(1, max_abs_acf) else NA_real_,
    1 - min(1, breakpoint_count / 3)
  )
  stability_score <- if (all(is.na(score_parts))) NA_real_ else mean(score_parts, na.rm = TRUE)

  list(
    shapiro_p = shapiro_p,
    durbin_watson = dw,
    ljung_box_p = ljung_p,
    breusch_pagan_p = bp$p_value,
    breakpoint_count = breakpoint_count,
    breakpoint_years = breakpoint_years,
    max_abs_acf = max_abs_acf,
    stability_score = stability_score
  )
}

compute_m2_growth_models_wrapper <- function(input, config) {
  year_col <- if ("Year" %in% names(input)) "Year" else names(input)[1]
  articles_col <- if ("Articles" %in% names(input)) "Articles" else names(input)[2]

  years <- input[[year_col]]
  articles <- input[[articles_col]]

  if (length(years) < 5) {
    return(list(status = "error: insufficient data for growth models (need 5+ years)"))
  }

  results <- list()
  results$bass <- suppressWarnings(fit_bass_model(years, articles))
  results$gompertz <- suppressWarnings(fit_gompertz_model(years, articles))
  results$weibull <- suppressWarnings(fit_weibull_model(years, articles))
  results$richards <- suppressWarnings(fit_richards_model(years, articles))
  results$von_bertalanffy <- suppressWarnings(fit_von_bertalanffy_model(years, articles))
  results$mmf <- suppressWarnings(fit_mmf_model(years, articles))
  results$comparison <- suppressWarnings(compare_growth_models(results))
  results$best_model <- select_best_growth_model(results$comparison)
  results$status <- "success"
  results
}

create_diagnostics_model_comparison_plot <- function(comparison) {
  models <- if ("model" %in% names(comparison)) comparison$model else comparison$Model
  aic_values <- if ("AIC" %in% names(comparison)) comparison$AIC else rep(NA_real_, length(models))
  bic_values <- if ("BIC" %in% names(comparison)) comparison$BIC else rep(NA_real_, length(models))
  composite_values <- if ("CompositeScore" %in% names(comparison)) comparison$CompositeScore else rep(NA_real_, length(models))

  df <- rbind(
    data.frame(model = factor(models, levels = models), value = aic_values, metric = "AIC"),
    data.frame(model = factor(models, levels = models), value = bic_values, metric = "BIC")
  )
  if (any(is.finite(composite_values))) {
    df <- rbind(df, data.frame(model = factor(models, levels = models), value = composite_values, metric = "Composite"))
  }

  df$value_norm <- ave(df$value, df$metric, FUN = rbiblio_safe_norm01)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = model, y = value_norm, fill = metric)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7), width = 0.6, color = "black", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = c("AIC" = "#0072BD", "BIC" = "#D95319", "Composite" = "#77AC30")) +
    ggplot2::coord_flip() +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Model Comparison (AIC vs BIC)",
      x = NULL,
      y = "Normalized Score"
    ) +
    ggplot2::theme(legend.position = "bottom")

  ieee_mark_plot_layout(p, "full")
}

create_accuracy_comparison_plot <- function(accuracy) {
  if (is.null(accuracy) || nrow(accuracy) == 0) return(NULL)
  if (!"model" %in% names(accuracy) && "Model" %in% names(accuracy)) accuracy$model <- accuracy$Model

  metrics <- c("MAE", "RMSE", "MAPE", "SMAPE", "MASE", "TheilU", "Stability")
  available <- intersect(metrics, names(accuracy))
  if (length(available) == 0) return(NULL)

  df_list <- lapply(available, function(m) {
    data.frame(model = accuracy$model, metric = m, value = suppressWarnings(as.numeric(accuracy[[m]])))
  })
  df <- do.call(rbind, df_list)
  df$value_norm <- ave(df$value, df$metric, FUN = rbiblio_safe_norm01)

  colors <- c(
    "MAE" = "#0072BD",
    "RMSE" = "#D95319",
    "MAPE" = "#77AC30",
    "SMAPE" = "#A2142F",
    "MASE" = "#4DBBD5",
    "TheilU" = "#E64B35",
    "Stability" = "#3C5488"
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = model, y = value_norm, fill = metric)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7), width = 0.6, color = "black", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = colors[available]) +
    ggplot2::coord_flip() +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Forecast Accuracy Comparison",
      x = NULL,
      y = "Normalized Score"
    ) +
    ggplot2::theme(legend.position = "bottom")

  ieee_mark_plot_layout(p, "full")
}

calculate_kurtosis <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 4L) return(NA_real_)
  s <- stats::sd(x)
  if (!is.finite(s) || s <= .Machine$double.eps) {
    return(NA_real_)
  }
  m <- mean(x)
  sum((x - m)^4) / (n * s^4) - 3
}

fit_country_regression <- function(ts_data, models, country_name) {
  models_to_try <- intersect(models, c("linear", "quadratic", "exponential", "logarithmic", "power"))
  if (nrow(ts_data) < 8) {
    models_to_try <- c("linear", "quadratic")
  }

  fitted_models <- list()
  model_comparison <- data.frame(
    model = character(),
    R2 = numeric(),
    Adj_R2 = numeric(),
    RMSE = numeric(),
    AIC = numeric(),
    BIC = numeric(),
    stringsAsFactors = FALSE
  )

  for (model_name in models_to_try) {
    fit <- suppressWarnings(tryCatch(fit_growth_model(ts_data$Year, ts_data$Articles, model_name), error = function(e) NULL))
    if (!is.null(fit) && !is.null(fit$fit)) {
      fitted_models[[model_name]] <- fit
      model_comparison <- rbind(model_comparison, data.frame(
        model = model_name,
        R2 = fit$R2 %||% NA_real_,
        Adj_R2 = fit$Adj_R2 %||% NA_real_,
        RMSE = fit$RMSE %||% NA_real_,
        AIC = fit$AIC %||% NA_real_,
        BIC = fit$BIC %||% NA_real_,
        stringsAsFactors = FALSE
      ))
    }
  }

  if (nrow(model_comparison) == 0) {
    return(list(status = "no_valid_models", country = country_name))
  }

  best_idx <- which.max(model_comparison$Adj_R2)
  best_model <- model_comparison$model[best_idx]

  linear_fit <- fitted_models[["linear"]]
  slope <- NA_real_
  slope_pvalue <- NA_real_
  trend_direction <- "unknown"
  if (!is.null(linear_fit) && !is.null(linear_fit$fit)) {
    fit_summary <- rbiblio_safe_summary_lm(linear_fit$fit)
    coefs <- fit_summary$coefficients %||% matrix(numeric(0), nrow = 0, ncol = 0)
    if (is.matrix(coefs) && nrow(coefs) >= 2 && ncol(coefs) >= 4) {
      slope <- suppressWarnings(as.numeric(coefs[2, 1]))
      slope_pvalue <- suppressWarnings(as.numeric(coefs[2, 4]))
      trend_direction <- if (!is.finite(slope) || abs(slope) <= .Machine$double.eps) {
        "stable"
      } else if (slope > 0) {
        "increasing"
      } else {
        "decreasing"
      }
    }
  }

  trend_test <- test_linear_trend(ts_data)

  harmonic_analysis <- analyze_country_harmonics(ts_data$Year, ts_data$Articles)
  if (!is.null(fitted_models[[best_model]])) {
    predicted <- fitted_models[[best_model]]$predictions
    residuals <- ts_data$Articles - predicted
    residual_harmonics <- analyze_residual_harmonics_country(ts_data$Year, residuals)
    residual_tests <- test_country_residuals(ts_data$Year, residuals)
  } else {
    residual_harmonics <- list()
    residual_tests <- list()
  }

  list(
    status = "success",
    country = country_name,
    n_years = nrow(ts_data),
    year_range = c(min(ts_data$Year), max(ts_data$Year)),
    total_articles = sum(ts_data$Articles),
    mean_annual = mean(ts_data$Articles),
    sd_annual = stats::sd(ts_data$Articles),
    fitted_models = fitted_models,
    model_comparison = model_comparison,
    best_model = best_model,
    best_fit = fitted_models[[best_model]],
    slope = slope,
    slope_pvalue = slope_pvalue,
    trend_direction = trend_direction,
    trend_significance = trend_test,
    growth_rate = if (!is.na(slope)) slope / mean(ts_data$Articles) * 100 else NA_real_,
    harmonics = harmonic_analysis,
    residual_harmonics = residual_harmonics,
    residual_tests = residual_tests
  )
}

test_linear_trend <- function(ts_data) {
  if (nrow(ts_data) < 4) {
    return(list(test = "insufficient_data", p_value = NA_real_))
  }

  fit <- tryCatch(stats::lm(Articles ~ Year, data = ts_data), error = function(e) NULL)
  fit_summary <- rbiblio_safe_summary_lm(fit)
  coefs <- fit_summary$coefficients %||% matrix(numeric(0), nrow = 0, ncol = 0)

  if (is.null(fit) || !is.matrix(coefs) || nrow(coefs) < 2) {
    return(list(test = "failed", p_value = NA_real_))
  }

  slope <- suppressWarnings(as.numeric(coefs[2, 1]))
  se_slope <- suppressWarnings(as.numeric(coefs[2, 2]))
  if (!is.finite(slope) || !is.finite(se_slope) || se_slope <= .Machine$double.eps) {
    return(list(
      test = "degenerate",
      slope = slope,
      se_slope = se_slope,
      t_statistic = NA_real_,
      p_value = NA_real_,
      is_significant = NA,
      direction = if (is.finite(slope) && slope > 0) "increasing" else if (is.finite(slope) && slope < 0) "decreasing" else "stable"
    ))
  }

  t_stat <- slope / se_slope
  p_value <- 2 * stats::pt(-abs(t_stat), df = fit$df.residual)

  list(
    test = "t_test",
    slope = slope,
    se_slope = se_slope,
    t_statistic = t_stat,
    p_value = p_value,
    is_significant = p_value < 0.05,
    direction = if (slope > 0) "increasing" else "decreasing"
  )
}

test_growth_equality_hypothesis <- function(prepared_data, config) {
  country_summary <- prepared_data$country_summary
  if (is.null(country_summary) || nrow(country_summary) < 4) {
    return(list(
      hyphypothesis = "No difference in growth patterns between countries",
      null = "Growth patterns differ significantly",
      result = "inconclusive",
      interpretation = "Insufficient country data"
    ))
  }

  growth_rates <- country_summary$annual_growth_rate
  growth_rates <- growth_rates[is.finite(growth_rates)]
  if (length(growth_rates) < 4) {
    return(list(
      hyphypothesis = "No difference in growth patterns between countries",
      null = "Growth patterns differ significantly",
      result = "inconclusive",
      interpretation = "Insufficient growth data"
    ))
  }

  mean_growth <- mean(growth_rates)
  sd_growth <- stats::sd(growth_rates)
  cv <- if (is.finite(mean_growth) && abs(mean_growth) > .Machine$double.eps && is.finite(sd_growth)) {
    sd_growth / abs(mean_growth)
  } else {
    NA_real_
  }

  shapiro_res <- rbiblio_safe_shapiro(growth_rates)
  result <- if (is.finite(cv) && cv < 0.5) "fail_to_reject" else "reject"
  interpretation <- if (is.finite(cv)) {
    sprintf("Growth rate CV = %.2f. %s variation in growth patterns.", cv, if (cv < 0.5) "Low" else "High")
  } else {
    "Growth-rate variation could not be summarized robustly because the series was degenerate."
  }

  list(
    hyphypothesis = "No difference in growth patterns between countries",
    null = "Growth patterns differ significantly",
    result = result,
    CV = cv,
    shapiro_pvalue = if (!is.null(shapiro_res)) shapiro_res$p.value else NA_real_,
    mean_growth = mean_growth,
    sd_growth = sd_growth,
    interpretation = interpretation
  )
}

test_country_hypotheses <- function(country_regressions, summary_stats) {
  successful <- Filter(function(x) x$status == "success", country_regressions)
  hypotheses <- list()
  n_successful <- summary_stats$n_successful %||% 0L
  n_increasing <- summary_stats$n_increasing %||% 0L
  declining <- summary_stats$decreasing_countries %||% character()

  if (n_successful == 0) {
    hypotheses$H03_1 <- list(
      hypothesis = "All countries show increasing production trend",
      null = "Not all countries have increasing production",
      result = "inconclusive",
      n_increasing = 0L,
      n_total = 0L,
      proportion = NA_real_,
      interpretation = "No countries met the minimum data requirement for regression analysis"
    )
    hypotheses$H03_3 <- list(
      hypothesis = "Research topic is not declining in any country",
      null = "Research topic is declining in at least one country",
      result = "inconclusive",
      n_declining = 0L,
      declining_countries = character(),
      interpretation = "No countries met the minimum data requirement for decline testing"
    )
    return(list(
      hypotheses = hypotheses,
      hyphypotheses = hypotheses,
      summary = summarize_hypothesis_results(hypotheses),
      n_tests = length(hypotheses),
      status = "inconclusive"
    ))
  }

  hypotheses$H03_1 <- list(
    hypothesis = "All countries show increasing production trend",
    null = "Not all countries have increasing production",
    result = if (n_increasing == n_successful) "fail_to_reject" else "reject",
    n_increasing = n_increasing,
    n_total = n_successful,
    proportion = n_increasing / n_successful,
    interpretation = sprintf("%.1f%% of countries show increasing production (%d/%d)", n_increasing / n_successful * 100, n_increasing, n_successful)
  )

  if (length(successful) > 2) {
    slopes <- sapply(successful, function(x) x$slope)
    slopes <- slopes[is.finite(slopes)]
    mean_slopes <- mean(slopes)
    cv <- if (length(slopes) >= 2 && is.finite(mean_slopes) && abs(mean_slopes) > .Machine$double.eps) stats::sd(slopes) / abs(mean_slopes) else NA_real_
    kurtosis_val <- calculate_kurtosis(slopes)
    hypotheses$H03_2 <- list(
      hypothesis = "Research interest is uniformly distributed across countries",
      null = "Research interest is concentrated in specific country groups",
      result = if (is.finite(cv) && cv < 0.5) "fail_to_reject" else "reject",
      coefficient_of_variation = cv,
      kurtosis = kurtosis_val,
      interpretation = if (is.finite(cv)) {
        sprintf("Growth rate CV = %.2f%s, indicates %s distribution", cv * 100, "%", if (cv < 0.5) "uniform" else "heterogeneous")
      } else {
        "Slope dispersion was not estimable because country slopes were degenerate."
      }
    )
    if (length(slopes) >= 4) {
      clusters <- identify_growth_patterns(slopes, names(successful))
      hypotheses$H03_2$clusters <- clusters
      hypotheses$H03_2$bunches <- list(
        growing = clusters$growing,
        stable = clusters$stable,
        declining = clusters$declining,
        interpretation = sprintf("Countries cluster into %d growing, %d stable, %d declining", length(clusters$growing), length(clusters$stable), length(clusters$declining))
      )
    }
  }

  hypotheses$H03_3 <- list(
    hypothesis = "Research topic is not declining in any country",
    null = "Research topic is declining in at least one country",
    result = if (length(declining) == 0) "fail_to_reject" else "reject",
    n_declining = length(declining),
    declining_countries = declining,
    interpretation = if (length(declining) == 0) {
      "No countries show declining research interest"
    } else {
      sprintf("%d countries show declining interest: %s", length(declining), paste(head(declining, 5), collapse = ", "))
    }
  )

  if (length(successful) >= 4) {
    total_articles <- sapply(successful, function(x) x$total_articles)
    growth_rates <- sapply(successful, function(x) x$growth_rate)
    cor_test <- rbiblio_safe_rank_cor_test(log(total_articles), growth_rates, method = "spearman")
    if (!is.null(cor_test)) {
      hypotheses$H03_4 <- list(
        hypothesis = "No relationship between country production volume and growth rate",
        null = "There is a relationship between volume and growth",
        result = if (cor_test$p.value > 0.05) "fail_to_reject" else "reject",
        correlation = cor_test$estimate,
        p_value = cor_test$p.value,
        interpretation = sprintf("Spearman correlation = %.3f (p = %.4f)", cor_test$estimate, cor_test$p.value)
      )
    }
  }

  if (length(successful) >= 5) {
    gini_trend <- test_concentration_trend(country_regressions)
    hypotheses$H03_5 <- list(
      hypothesis = "Production concentration (Gini) is stable over time",
      null = "Concentration is changing over time",
      result = gini_trend$result,
      gini_start = gini_trend$gini_start,
      gini_end = gini_trend$gini_end,
      gini_change = gini_trend$change,
      interpretation = gini_trend$interpretation
    )
  }

  list(
    hypotheses = hypotheses,
    hyphypotheses = hypotheses,
    n_tests = length(hypotheses),
    summary = summarize_hypothesis_results(hypotheses),
    status = "success"
  )
}

compute_trend <- function(x, y) {
  x <- suppressWarnings(as.numeric(x))
  y <- suppressWarnings(as.numeric(y))
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]
  if (length(x) < 2L) {
    return(list(slope = NA_real_, intercept = NA_real_, r2 = NA_real_))
  }

  model <- tryCatch(stats::lm(y ~ x, na.action = na.exclude), error = function(e) NULL)
  model_summary <- rbiblio_safe_summary_lm(model)
  if (is.null(model) || is.null(model_summary)) {
    return(list(slope = NA_real_, intercept = NA_real_, r2 = NA_real_))
  }

  coefs <- stats::coef(model)
  list(
    slope = suppressWarnings(as.numeric(coefs[2])),
    intercept = suppressWarnings(as.numeric(coefs[1])),
    r2 = suppressWarnings(as.numeric(model_summary$r.squared))
  )
}

m2_compute_trend_statistics <- function(years, articles, changepoint_result = NULL) {
  years <- m2_num_vec(years)
  articles <- m2_num_vec(articles)
  keep <- is.finite(years) & is.finite(articles)
  years <- years[keep]
  articles <- articles[keep]
  n <- length(articles)

  if (n < 3L) {
    return(list(status = "insufficient_data"))
  }

  linear_fit <- tryCatch(stats::lm(articles ~ years), error = function(e) NULL)
  fit_summary <- rbiblio_safe_summary_lm(linear_fit)
  coefs <- if (!is.null(linear_fit)) stats::coef(linear_fit) else c(NA_real_, NA_real_)
  slope <- suppressWarnings(as.numeric(coefs[2]))
  intercept <- suppressWarnings(as.numeric(coefs[1]))
  acceleration <- mean(diff(diff(articles)), na.rm = TRUE)
  growth_rates <- diff(articles) / pmax(abs(articles[-n]), .Machine$double.eps)
  cagr <- if (n >= 2 && articles[1] > 0) (articles[n] / articles[1])^(1 / (n - 1)) - 1 else NA_real_
  recent_window <- min(5L, n)
  recent_cagr <- if (recent_window >= 2 && articles[n - recent_window + 1] > 0) {
    (articles[n] / articles[n - recent_window + 1])^(1 / (recent_window - 1)) - 1
  } else {
    NA_real_
  }

  mk <- m2_mann_kendall_test(articles)
  sen <- m2_sen_slope(years, articles)
  hurst <- m2_hurst_exponent(articles)

  list(
    slope = slope,
    intercept = intercept,
    r_squared = suppressWarnings(as.numeric(fit_summary$r.squared %||% NA_real_)),
    cagr = cagr,
    recent_cagr = recent_cagr,
    mean_growth_rate = mean(growth_rates, na.rm = TRUE),
    median_growth_rate = stats::median(growth_rates, na.rm = TRUE),
    acceleration = acceleration,
    volatility = stats::sd(growth_rates, na.rm = TRUE),
    coefficient_of_variation = stats::sd(articles, na.rm = TRUE) / pmax(mean(articles, na.rm = TRUE), .Machine$double.eps),
    mann_kendall = mk,
    sen_slope = sen,
    hurst_exponent = hurst,
    breakpoint_years = if (!is.null(changepoint_result$summary$changepoint_years)) changepoint_result$summary$changepoint_years else numeric(0),
    n_breakpoints = if (!is.null(changepoint_result$summary$n_changepoints)) changepoint_result$summary$n_changepoints else 0L,
    status = "success"
  )
}

compute_nelsop_patterns <- function(country_year_data) {
  years <- sort(unique(country_year_data$year))
  if (length(years) < 5L) {
    return(list(status = "insufficient years"))
  }

  leadership_index <- numeric(length(years))
  for (i in seq_along(years)) {
    year_data <- country_year_data[country_year_data$year == years[i], ]
    year_data <- year_data[order(-year_data$production), ]
    total <- sum(year_data$production)
    top5_share <- if (is.finite(total) && total > 0) sum(year_data$production[1:min(5, nrow(year_data))]) / total else NA_real_
    leadership_index[i] <- top5_share
  }

  concentration_fit <- tryCatch(stats::lm(leadership_index ~ years), error = function(e) NULL)
  concentration_summary <- rbiblio_safe_summary_lm(concentration_fit)
  concentration_trend <- if (!is.null(concentration_fit)) suppressWarnings(as.numeric(stats::coef(concentration_fit)[2])) else NA_real_
  concentration_p <- if (!is.null(concentration_summary) && is.matrix(concentration_summary$coefficients) && nrow(concentration_summary$coefficients) >= 2) {
    suppressWarnings(as.numeric(concentration_summary$coefficients[2, 4]))
  } else {
    NA_real_
  }

  network_density <- compute_network_density_over_time(country_year_data)
  specialization <- compute_specialization_index(country_year_data)

  list(
    leadership_index = leadership_index,
    concentration_trend = concentration_trend,
    concentration_trend_p = concentration_p,
    interpretation = if (is.finite(concentration_trend) && concentration_trend < -0.001) {
      "Decentralizing: leadership is spreading to more countries"
    } else if (is.finite(concentration_trend) && concentration_trend > 0.001) {
      "Concentrating: leaders are maintaining dominance"
    } else {
      "Stable: no significant change in concentration"
    },
    network_density = network_density,
    specialization = specialization,
    years = years,
    status = "success"
  )
}

create_lisa_plot <- function(lisa_data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (is.null(lisa_data) || !is.list(lisa_data)) return(NULL)

  palette <- c(
    "HH" = "#B2182B",
    "LL" = "#2166AC",
    "HL" = "#EF8A62",
    "LH" = "#67A9CF",
    "Not significant" = "gray70"
  )

  values_df <- lisa_data$lisa_values %||% data.frame()
  if (is.data.frame(values_df) &&
      nrow(values_df) > 0 &&
      all(c("country", "value", "cluster_type") %in% names(values_df))) {
    values_df <- values_df[is.finite(suppressWarnings(as.numeric(values_df$value))), , drop = FALSE]
    if (nrow(values_df) == 0) return(NULL)

    top_n <- config$top_n_countries %||% min(20L, nrow(values_df))
    values_df <- values_df[order(-abs(suppressWarnings(as.numeric(values_df$value)))), , drop = FALSE]
    values_df <- utils::head(values_df, top_n)
    if ("significant" %in% names(values_df)) {
      sig <- as.logical(values_df$significant)
      sig[is.na(sig)] <- FALSE
      values_df$cluster_type <- ifelse(sig, as.character(values_df$cluster_type), "Not significant")
    }
    values_df$cluster_type <- factor(values_df$cluster_type, levels = names(palette))

    p <- ggplot2::ggplot(
      values_df,
      ggplot2::aes(
        x = stats::reorder(country, value),
        y = value,
        fill = cluster_type
      )
    ) +
      ggplot2::geom_col(color = "white", linewidth = 0.15) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
      ggplot2::labs(
        title = "LISA Spatial Clusters by Country",
        subtitle = "Bars show country production values colored by local spatial cluster type.",
        x = "Country",
        y = "Production",
        fill = "Cluster Type"
      ) +
      ieee_theme_bar() +
      ggplot2::theme(legend.position = "bottom")

    return(ieee_mark_plot_layout(p, "full"))
  }

  clusters_df <- lisa_data$clusters %||% data.frame()
  if (is.data.frame(clusters_df) &&
      nrow(clusters_df) > 0 &&
      all(c("type", "count") %in% names(clusters_df))) {
    clusters_df <- clusters_df[is.finite(suppressWarnings(as.numeric(clusters_df$count))), , drop = FALSE]
    if (nrow(clusters_df) == 0) return(NULL)
    clusters_df$type <- factor(as.character(clusters_df$type), levels = names(palette))

    p <- ggplot2::ggplot(
      clusters_df,
      ggplot2::aes(x = type, y = count, fill = type)
    ) +
      ggplot2::geom_col(color = "white", linewidth = 0.15, width = 0.7) +
      ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
      ggplot2::labs(
        title = "LISA Cluster Summary",
        subtitle = "Counts of countries assigned to each local spatial association type.",
        x = "Cluster Type",
        y = "Number of Countries",
        fill = "Cluster Type"
      ) +
      ieee_theme_bar() +
      ggplot2::theme(legend.position = "none")

    return(ieee_mark_plot_layout(p, "single"))
  }

  NULL
}
