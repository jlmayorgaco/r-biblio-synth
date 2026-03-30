# ============================================================================
# m3_compute_country_regressions.R - Per-Country Regression Analysis
# ============================================================================
# Run M2-style regression models for each country's annual production
# Identify which countries have growing/declining/stable research interest

#' Compute regression models for each country's annual production
#'
#' @param prepared_data Output from prepare_m3_country_data
#' @param config Configuration list
#' @return List with per-country regression results
#' @export
m3_compute_country_regressions <- function(prepared_data, config = biblio_config()) {
  if (is.null(prepared_data) || prepared_data$status != "success") {
    return(list(
      country_regressions = list(),
      status = "error: invalid prepared_data"
    ))
  }
  
  doc_country <- prepared_data$country_doc_level
  if (is.null(doc_country) || nrow(doc_country) == 0) {
    return(list(
      country_regressions = list(),
      status = "error: no country data"
    ))
  }
  
  year_col <- get_year_column(prepared_data)
  if (is.null(year_col)) {
    return(list(
      country_regressions = list(),
      status = "error: no year column found"
    ))
  }
  
  country_year_production <- aggregate_production_by_country_year(doc_country, year_col)
  
  if (is.null(country_year_production) || nrow(country_year_production) == 0) {
    return(list(
      country_regressions = list(),
      status = "error: could not compute production by country-year"
    ))
  }
  
  country_regressions <- list()
  countries <- unique(country_year_production$country)
  min_years <- config$min_years_for_regression %||% 5
  
  growth_models <- get_growth_models()
  
  for (country in countries) {
    country_data <- country_year_production[country_year_production$country == country, ]
    country_data <- country_data[order(country_data$year), ]
    
    if (nrow(country_data) < min_years) {
      country_regressions[[country]] <- list(
        status = "insufficient_data",
        n_years = nrow(country_data),
        min_required = min_years
      )
      next
    }
    
    ts_data <- data.frame(
      Year = country_data$year,
      Articles = country_data$n_articles
    )
    
    country_regressions[[country]] <- fit_country_regression(ts_data, growth_models, country)
  }
  
  summary_stats <- summarize_country_regressions(country_regressions)
  
  hypothesis_results <- test_country_hypotheses(country_regressions, summary_stats)
  
  list(
    country_regressions = country_regressions,
    summary = summary_stats,
    hypotheses = hypothesis_results,
    status = "success"
  )
}

#' Get year column from prepared data
#' @keywords internal
get_year_column <- function(prepared_data) {
  if (!is.null(prepared_data$year_column)) {
    return(prepared_data$year_column)
  }
  
  possible_cols <- c("PY", "Year", "year", "PublicationYear")
  for (col in possible_cols) {
    if (!is.null(prepared_data$input_data[[col]])) {
      return(col)
    }
  }
  "PY"
}

#' Aggregate production by country and year
#' @keywords internal
aggregate_production_by_country_year <- function(doc_country, year_col) {
  year_data <- doc_country$year
  if (is.null(year_data)) {
    year_data <- doc_country[[year_col]]
  }
  
  doc_country$year <- as.integer(year_data)
  doc_country <- doc_country[!is.na(doc_country$year), ]
  
  aggregate(doc_id ~ country + year, 
            data = doc_country, 
            FUN = function(x) length(unique(x)))
}

#' Available growth models
#' @keywords internal
get_growth_models <- function() {
  c("linear", "quadratic", "exponential", "logarithmic", "power", 
    "logistic", "gompertz", "vonbertalanffy")
}

#' Fit regression for a single country
#' @keywords internal
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
    fit <- tryCatch({
      fit_growth_model(ts_data$Year, ts_data$Articles, model_name)
    }, error = function(e) NULL)
    
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
    return(list(
      status = "no_valid_models",
      country = country_name
    ))
  }
  
  best_idx <- which.max(model_comparison$Adj_R2)
  best_model <- model_comparison$model[best_idx]
  
  linear_fit <- fitted_models[["linear"]]
  slope <- NA_real_
  slope_pvalue <- NA_real_
  trend_direction <- "unknown"
  
  if (!is.null(linear_fit) && !is.null(linear_fit$fit)) {
    coefs <- summary(linear_fit$fit)$coefficients
    if (nrow(coefs) >= 2 && ncol(coefs) >= 4) {
      slope <- coefs[2, 1]
      slope_pvalue <- coefs[2, 4]
      trend_direction <- if (slope > 0) "increasing" else if (slope < 0) "decreasing" else "stable"
    }
  }
  
  trend_test <- test_linear_trend(ts_data)
  
  # =========================================
  # Harmonic Analysis on Country Data
  # =========================================
  harmonic_analysis <- analyze_country_harmonics(ts_data$Year, ts_data$Articles)
  
  # =========================================
  # Residual Analysis
  # =========================================
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
    sd_annual = sd(ts_data$Articles),
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

#' Analyze harmonics for country data
#' @keywords internal
analyze_country_harmonics <- function(years, articles) {
  n <- length(articles)
  if (n < 8) {
    return(list(
      dominant_period = NA,
      interpretation = "Insufficient data for harmonic analysis"
    ))
  }
  
  dt <- mean(diff(years))
  
  # FFT analysis
  fft_result <- fft(articles - mean(articles))
  frequencies <- (0:(floor(n / 2))) / (n * dt)
  power <- Mod(fft_result[1:length(frequencies)])^2
  
  # Find dominant period
  peak_idx <- which.max(power[2:floor(n/2)]) + 1
  dominant_freq <- frequencies[peak_idx]
  dominant_period <- if (dominant_freq > 0) 1 / dominant_freq else NA
  
  # Variance explained by harmonics
  variance_explained <- sum(power[1:min(3, length(power))]) / sum(power)
  
  # Top periods
  top_idx <- order(-power)[1:min(3, length(power))]
  top_periods <- data.frame(
    period = 1 / frequencies[top_idx[frequencies[top_idx] > 0]],
    power = power[top_idx[frequencies[top_idx] > 0]],
    variance_explained = power[top_idx[frequencies[top_idx] > 0]] / sum(power)
  )
  
  list(
    dominant_period = dominant_period,
    dominant_frequency = dominant_freq,
    variance_explained = variance_explained,
    top_periods = top_periods,
    spectrum = list(
      frequencies = frequencies,
      power = power
    ),
    interpretation = sprintf(
      "Dominant cycle: %.1f years (%.1f%% variance)",
      dominant_period, variance_explained * 100
    )
  )
}

#' Analyze residual harmonics for country
#' @keywords internal
analyze_residual_harmonics_country <- function(years, residuals) {
  n <- length(residuals)
  if (n < 8) {
    return(list(
      has_periodicity = FALSE,
      interpretation = "Insufficient data"
    ))
  }
  
  dt <- mean(diff(years))
  
  # FFT on residuals
  fft_result <- fft(residuals - mean(residuals))
  frequencies <- (0:(floor(n / 2))) / (n * dt)
  power <- Mod(fft_result[1:length(frequencies)])^2
  
  # Check for significant periodicity
  noise_level <- quantile(power, 0.75)
  max_power <- max(power[2:floor(n/2)])
  
  has_periodicity <- max_power > 3 * noise_level
  
  peak_idx <- which.max(power[2:floor(n/2)]) + 1
  dominant_freq <- frequencies[peak_idx]
  dominant_period <- if (dominant_freq > 0) 1 / dominant_freq else NA
  
  variance_explained <- sum(power[1:min(3, length(power))]) / sum(power)
  
  list(
    has_periodicity = has_periodicity,
    dominant_period = dominant_period,
    dominant_frequency = dominant_freq,
    variance_explained = variance_explained,
    max_power_ratio = max_power / noise_level,
    interpretation = if (has_periodicity) {
      sprintf("Significant residual periodicity detected: %.1f-year cycle", dominant_period)
    } else {
      "No significant periodicity in residuals"
    }
  )
}

#' Test country residuals
#' @keywords internal
test_country_residuals <- function(years, residuals) {
  n <- length(residuals)
  if (n < 8) {
    return(list(
      is_normal = NA,
      has_breakpoints = NA,
      interpretation = "Insufficient data"
    ))
  }
  
  # Normality test
  sw_test <- tryCatch(shapiro.test(residuals), error = function(e) NULL)
  is_normal <- if (!is.null(sw_test)) sw_test$p.value > 0.05 else NA
  
  # Breakpoint detection (simplified)
  cumulative <- cumsum(residuals - mean(residuals))
  cusum_max <- max(abs(cumulative))
  threshold <- 1.143 * sqrt(n) * sd(residuals)
  has_breakpoints <- cusum_max > threshold
  
  # Durbin-Watson
  dw_stat <- sum(diff(residuals)^2) / sum(residuals^2)
  has_autocorr <- dw_stat < 1.5 || dw_stat > 2.5
  
  list(
    is_normal = is_normal,
    shapiro_p = if (!is.null(sw_test)) sw_test$p.value else NA,
    has_breakpoints = has_breakpoints,
    cusum_max = cusum_max,
    cusum_threshold = threshold,
    dw_statistic = dw_stat,
    has_autocorrelation = has_autocorr,
    interpretation = sprintf(
      "Normality: %s, Breakpoints: %s, Autocorr: %s",
      if (is.na(is_normal)) "NA" else if (is_normal) "Yes" else "No",
      if (has_breakpoints) "Yes" else "No",
      if (has_autocorr) "Yes" else "No"
    )
  )
}

#' Fit a growth model
#' @keywords internal
fit_growth_model <- function(x, y, model_name) {
  df <- data.frame(x = x, y = y)
  df <- df[!is.na(df$y) & df$y > 0, ]
  
  if (nrow(df) < 3) return(NULL)
  
  fit <- tryCatch({
    switch(model_name,
      "linear" = lm(y ~ x, data = df),
      "quadratic" = lm(y ~ x + I(x^2), data = df),
      "exponential" = lm(log(y) ~ x, data = df),
      "logarithmic" = lm(y ~ log(x), data = df),
      "power" = lm(log(y) ~ log(x), data = df),
      NULL
    )
  }, error = function(e) NULL)
  
  if (is.null(fit)) return(NULL)
  
  pred <- predict(fit)
  if (model_name %in% c("exponential", "power")) {
    pred <- exp(pred)
  }
  
  ss_res <- sum((df$y - pred)^2)
  ss_tot <- sum((df$y - mean(df$y))^2)
  R2 <- 1 - ss_res/ss_tot
  
  n <- nrow(df)
  k <- length(coef(fit))
  Adj_R2 <- 1 - (1 - R2) * (n - 1) / (n - k)
  
  RMSE <- sqrt(mean((df$y - pred)^2))
  
  AIC_val <- AIC(fit)
  BIC_val <- BIC(fit)
  
  list(
    model = model_name,
    fit = fit,
    coefficients = coef(fit),
    R2 = R2,
    Adj_R2 = Adj_R2,
    RMSE = RMSE,
    AIC = AIC_val,
    BIC = BIC_val,
    predictions = pred,
    residuals = df$y - pred
  )
}

#' Test linear trend significance
#' @keywords internal
test_linear_trend <- function(ts_data) {
  if (nrow(ts_data) < 4) {
    return(list(test = "insufficient_data", p_value = NA_real_))
  }
  
  fit <- tryCatch({
    lm(Articles ~ Year, data = ts_data)
  }, error = function(e) NULL)
  
  if (is.null(fit)) {
    return(list(test = "failed", p_value = NA_real_))
  }
  
  coefs <- summary(fit)$coefficients
  if (nrow(coefs) < 2) {
    return(list(test = "failed", p_value = NA_real_))
  }
  
  slope <- coefs[2, 1]
  se_slope <- coefs[2, 2]
  t_stat <- slope / se_slope
  p_value <- 2 * pt(-abs(t_stat), df = fit$df.residual)
  
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

#' Summarize country regressions
#' @keywords internal
summarize_country_regressions <- function(country_regressions) {
  successful <- Filter(function(x) x$status == "success", country_regressions)
  
  if (length(successful) == 0) {
    return(list(
      n_countries = length(country_regressions),
      n_successful = 0,
      increasing = 0,
      decreasing = 0,
      stable = 0,
      status = "no_successful_fits"
    ))
  }
  
  directions <- sapply(successful, function(x) x$trend_direction)
  
  growth_rates <- sapply(successful, function(x) x$growth_rate)
  growth_rates <- growth_rates[!is.na(growth_rates)]
  
  slopes <- sapply(successful, function(x) x$slope)
  slopes <- slopes[!is.na(slopes)]
  
  list(
    n_countries = length(country_regressions),
    n_successful = length(successful),
    n_increasing = sum(directions == "increasing", na.rm = TRUE),
    n_decreasing = sum(directions == "decreasing", na.rm = TRUE),
    n_stable = sum(directions == "stable", na.rm = TRUE),
    mean_growth_rate = if (length(growth_rates) > 0) mean(growth_rates) else NA_real_,
    median_growth_rate = if (length(growth_rates) > 0) median(growth_rates) else NA_real_,
    mean_slope = if (length(slopes) > 0) mean(slopes) else NA_real_,
    median_slope = if (length(slopes) > 0) median(slopes) else NA_real_,
    increasing_countries = names(directions[directions == "increasing"]),
    decreasing_countries = names(directions[directions == "decreasing"])
  )
}

#' Test hypotheses about country production trends
#' @keywords internal
test_country_hypotheses <- function(country_regressions, summary_stats) {
  successful <- Filter(function(x) x$status == "success", country_regressions)
  
  hypotheses <- list()
  
  # H03.1: Are all countries showing increasing production?
  hypotheses$H03_1 <- list(
    hypothesis = "All countries show increasing production trend",
    null = "Not all countries have increasing production",
    result = if (summary_stats$n_increasing == summary_stats$n_successful) {
      "fail_to_reject"
    } else {
      "reject"
    },
    n_increasing = summary_stats$n_increasing,
    n_total = summary_stats$n_successful,
    proportion = summary_stats$n_increasing / summary_stats$n_successful,
    interpretation = sprintf("%.1f%% of countries show increasing production (%d/%d)",
                            summary_stats$n_increasing / summary_stats$n_successful * 100,
                            summary_stats$n_increasing,
                            summary_stats$n_successful)
  )
  
  # H03.2: Is research interest uniform across all countries? (vs concentrated in bunches)
  if (length(successful) > 2) {
    slopes <- sapply(successful, function(x) x$slope)
    slopes <- slopes[!is.na(slopes)]
    
    cv <- sd(slopes) / abs(mean(slopes))
    
    kurtosis_val <- calculate_kurtosis(slopes)
    
    hypotheses$H03_2 <- list(
      hypothesis = "Research interest is uniformly distributed across countries",
      null = "Research interest is concentrated in specific country groups",
      result = if (cv < 0.5) "fail_to_reject" else "reject",
      coefficient_of_variation = cv,
      kurtosis = kurtosis_val,
      interpretation = sprintf("Growth rate CV = %.2f%s, indicates %s distribution",
                              cv * 100, "%",
                              if (cv < 0.5) "uniform" else "heterogeneous")
    )
    
    if (length(slopes) >= 4) {
      clusters <- identify_growth_patterns(slopes, names(successful))
      
      hypotheses$H03_2$clusters <- clusters
      
      hypotheses$H03_2$bunches <- list(
        growing = clusters$growing,
        stable = clusters$stable,
        declining = clusters$declining,
        interpretation = sprintf("Countries cluster into %d growing, %d stable, %d declining",
                               length(clusters$growing),
                               length(clusters$stable),
                               length(clusters$declining))
      )
    }
  }
  
  # H03.3: Is the research topic declining in some countries?
  declining <- summary_stats$decreasing_countries
  hypotheses$H03_3 <- list(
    hypothesis = "Research topic is not declining in any country",
    null = "Research topic is declining in at least one country",
    result = if (length(declining) == 0) "fail_to_reject" else "reject",
    n_declining = length(declining),
    declining_countries = declining,
    interpretation = if (length(declining) == 0) {
      "No countries show declining research interest"
    } else {
      sprintf("%d countries show declining interest: %s",
             length(declining),
             paste(head(declining, 5), collapse = ", "))
    }
  )
  
  # H03.4: Is there a relationship between country size and growth rate?
  if (length(successful) >= 4) {
    total_articles <- sapply(successful, function(x) x$total_articles)
    growth_rates <- sapply(successful, function(x) x$growth_rate)
    valid_idx <- !is.na(growth_rates) & !is.na(total_articles)
    
    if (sum(valid_idx) >= 4) {
      cor_test <- cor.test(log(total_articles[valid_idx]), 
                           growth_rates[valid_idx],
                           method = "spearman")
      
      hypotheses$H03_4 <- list(
        hypothesis = "No relationship between country production volume and growth rate",
        null = "There is a relationship between volume and growth",
        result = if (cor_test$p.value > 0.05) "fail_to_reject" else "reject",
        correlation = cor_test$estimate,
        p_value = cor_test$p.value,
        interpretation = sprintf("Spearman correlation = %.3f (p = %.4f)",
                                cor_test$estimate, cor_test$p.value)
      )
    }
  }
  
  # H03.5: Is production concentration stable over time?
  if (length(successful) >= 5) {
    year_lists <- lapply(successful, function(x) {
      x$fitted_models$linear$fit$model$x
    })
    
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
    n_tests = length(hypotheses),
    summary = summarize_hypothesis_results(hypotheses)
  )
}

#' Calculate kurtosis
#' @keywords internal
calculate_kurtosis <- function(x) {
  n <- length(x)
  if (n < 4) return(NA_real_)
  m <- mean(x)
  s <- sd(x)
  sum((x - m)^4) / (n * s^4) - 3
}

#' Identify growth patterns (clusters)
#' @keywords internal
identify_growth_patterns <- function(slopes, country_names) {
  names(slopes) <- country_names
  
  q33 <- quantile(slopes, 0.33)
  q66 <- quantile(slopes, 0.67)
  
  growing <- names(slopes[slopes > q66])
  declining <- names(slopes[slopes < q33])
  stable <- names(slopes[slopes >= q33 & slopes <= q66])
  
  list(
    growing = growing,
    stable = stable,
    declining = declining,
    thresholds = c(lower = q33, upper = q66)
  )
}

#' Test concentration trend
#' @keywords internal
test_concentration_trend <- function(country_regressions) {
  list(
    result = "insufficient_data",
    gini_start = NA_real_,
    gini_end = NA_real_,
    change = NA_real_,
    interpretation = "Could not compute concentration trend"
  )
}

#' Summarize hypothesis results
#' @keywords internal
summarize_hypothesis_results <- function(hypotheses) {
  n_total <- length(hypotheses)
  n_rejected <- sum(sapply(hypotheses, function(h) h$result == "reject"))
  n_failed_to_reject <- n_total - n_rejected
  
  list(
    n_hypotheses = n_total,
    n_rejected = n_rejected,
    n_failed_to_reject = n_failed_to_reject,
    rejection_rate = n_rejected / n_total
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b