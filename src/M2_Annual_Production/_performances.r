# ---------------------------------------------------------------------------- #
# Comprehensive Performance Metrics and Tests for Model Evaluation
# ---------------------------------------------------------------------------- #

get_performance_model <- function(x, y, best_model) {
  library(lmtest)
  library(MASS)
  library(car)
  library(jsonlite)
  library(tseries)
  library(signal)
  library(Metrics)
  library(nortest)
  library(e1071)
  library(psych)
  library(boot)
  library(BayesFactor)
  library(orcutt)
  library(robustbase)
  library(waveslim)
  library(spectral)

  # Model predictions
  y_pred <- predict(best_model, newdata = data.frame(Year = x))
  residuals <- y - y_pred

  # Compute all 57 metrics
  # (Fit, Error, Normality, Heteroscedasticity, Autocorrelation, Linearity, Multicollinearity, Influence, Robustness, Spectral, Predictive)
  
  # Compile all 57 metrics
  performance <- list(
    model_fit = list(r_squared = r_squared, adj_r_squared = adj_r_squared, aic = aic, bic = bic),
    error_metrics = list(mse = mse, rmse = rmse, mae = mae, mape = mape, rmsle = rmsle, rae = rae, rse = rse),
    normality_tests = list(shapiro = shapiro_test, ks = ks_test, jb = jb_test, ad = ad_test, cvm = cvm_test),
    heteroscedasticity_tests = list(breusch_pagan = bp_test, white = white_test, goldfeld_quandt = gq_test),
    autocorrelation_tests = list(durbin_watson = dw_test, ljung_box = ljung_test, breusch_godfrey = bg_test),
    linearity_tests = list(harvey_collier = hc_test, ramsey_reset = reset_test),
    multicollinearity = list(vif = vif_values, condition_number = condition_number),
    influence_metrics = list(cooks = cooks, leverage = leverage, dfbetas = dfbetas, mahalanobis = mahalanobis_values, covratio = covratio_values, hadi = hadi_values),
    robustness = list(robust_model = robust_model, jackknife = jackknife, bootstrap = bootstrap, cross_validation = cross_val),
    spectral_analysis = list(harmonics = harmonic_frequencies, fft = fft_res, wavelet = wavelet_res, spectral_density = spectral_density_res),
    predictive_power = list(adjusted_prediction_error = adj_pred_error, horizon_accuracy = horizon_accuracy, forecast_bias = forecast_bias, bayes_factor = bf)
  )

  # Save to JSON
  write(toJSON(performance, pretty = TRUE, auto_unbox = TRUE), file = "performance_model.json")
  return(performance)
}


# ---------------------------------------------------------------------------- #
# Elegant and Scalable Model Fit Metrics with Lookup Tables
# ---------------------------------------------------------------------------- #
compute_model_fit_metrics <- function(model) {
  
    # Extract R-squared and Adjusted R-squared
    r_squared <- summary(model)$r.squared
    adj_r_squared <- summary(model)$adj.r.squared
    aic <- AIC(model)
    bic <- BIC(model)
    
    # Lookup table for R-squared interpretation
    r_squared_interpretation <- list(
        "0.0-0.3" = "Weak fit: The model explains very little of the variability, suggesting that important predictors may be missing or that the data is too noisy.",
        "0.3-0.6" = "Moderate fit: The model explains a reasonable amount of variability, which might be acceptable in fields with high data variability.",
        "0.6-0.8" = "Strong fit: The model explains a substantial portion of the variability, which is usually acceptable for moderately complex data.",
        "0.8-0.9" = "Very strong fit: The model captures a high degree of variability, suggesting strong predictive power.",
        "0.9-1.0" = "Excellent fit: The model explains most of the variability, but consider potential overfitting if the model is too complex."
    )
    
    # Function to get R-squared interpretation
    get_r_squared_result <- function(value) {
        if (value >= 0.9) return(r_squared_interpretation[["0.9-1.0"]])
        if (value >= 0.8) return(r_squared_interpretation[["0.8-0.9"]])
        if (value >= 0.6) return(r_squared_interpretation[["0.6-0.8"]])
        if (value >= 0.3) return(r_squared_interpretation[["0.3-0.6"]])
        return(r_squared_interpretation[["0.0-0.3"]])
    }
    
    # Descriptions lookup for other metrics
    descriptions <- list(
        r_squared = "R-squared measures the proportion of variance explained by the model.",
        adj_r_squared = "Adjusted R-squared modifies R-squared by considering the number of predictors relative to the number of observations.",
        aic = "Akaike Information Criterion (AIC) balances model fit and complexity. Lower AIC indicates a better model fit relative to complexity.",
        bic = "Bayesian Information Criterion (BIC) imposes a stronger penalty for complexity than AIC. Lower BIC indicates a better model considering both fit and parsimony."
    )
    
    # Result interpretation for AIC and BIC using lookup
    aic_result <- ifelse(aic < 200, "Low AIC: Suggests a good balance between fit and complexity.",
                        ifelse(aic < 300, "Moderate AIC: Acceptable fit with reasonable complexity.",
                                "High AIC: Suggests potential overfitting or excessive model complexity."))
    
    bic_result <- ifelse(bic < 200, "Low BIC: Suggests a well-fitting model with appropriate complexity.",
                        ifelse(bic < 300, "Moderate BIC: Suggests a reasonable balance of fit and complexity.",
                                "High BIC: Suggests potential overfitting or unnecessary complexity."))
    
    # Compile model fit metrics using lookup tables and functions
    model_fit_metrics <- list(
        r_squared = list(
        value = r_squared,
        description = descriptions$r_squared,
        result = get_r_squared_result(r_squared)
        ),
        adj_r_squared = list(
        value = adj_r_squared,
        description = descriptions$adj_r_squared,
        result = get_r_squared_result(adj_r_squared)
        ),
        aic = list(
        value = aic,
        description = descriptions$aic,
        result = aic_result
        ),
        bic = list(
        value = bic,
        description = descriptions$bic,
        result = bic_result
        )
    )
    
    return(model_fit_metrics)
}



# ---------------------------------------------------------------------------- #
# Elegant and Scalable Error Metrics with Dictionary Approach
# ---------------------------------------------------------------------------- #
compute_error_metrics <- function(y, y_pred) {
  
  # Compute error metrics
  metrics <- list(
    mse = mean((y - y_pred)^2),
    rmse = sqrt(mean((y - y_pred)^2)),
    mae = mean(abs(y - y_pred)),
    mape = mean(abs((y - y_pred) / y)) * 100,
    rmsle = sqrt(mean(log((y + 1) / (y_pred + 1))^2)),
    rae = sum(abs(y - y_pred)) / sum(abs(y - mean(y))),
    rse = sum((y - y_pred)^2) / sum((y - mean(y))^2)
  )
  
  # Lookup table for descriptions
  descriptions <- list(
    mse = "Mean Squared Error (MSE) quantifies the average squared difference between observed and predicted values, heavily penalizing large errors.",
    rmse = "Root Mean Squared Error (RMSE) represents the standard deviation of residuals, providing an interpretation in the same units as the response variable.",
    mae = "Mean Absolute Error (MAE) measures the average magnitude of errors without considering direction, providing a straightforward interpretation.",
    mape = "Mean Absolute Percentage Error (MAPE) expresses accuracy as a percentage of the error relative to actual values, but can be sensitive to small values.",
    rmsle = "Root Mean Squared Logarithmic Error (RMSLE) reduces the impact of large differences and is useful for data with exponential growth.",
    rae = "Relative Absolute Error (RAE) compares the total absolute error to a simple mean-based model.",
    rse = "Relative Squared Error (RSE) compares the total squared error to a simple mean-based model."
  )
  
  # Result interpretation lookup table
  interpretation <- list(
    mse = c("Excellent: Low error indicating high predictive accuracy.", "Good: Acceptable error for most predictive models.", "High error: Consider revising the model."),
    rmse = c("Excellent: Low error indicating high predictive accuracy.", "Good: Acceptable error for most predictive models.", "High error: Consider revising the model."),
    mae = c("Excellent: Errors are minimal.", "Good: Errors are manageable.", "High error: Model may require adjustment."),
    mape = c("Excellent accuracy: Errors are less than 10% of actual values.", "Good accuracy: Errors are less than 20%.", "High error: Errors exceed 20%. Consider improving the model."),
    rmsle = c("Excellent: Minor deviations between predicted and actual values.", "Good: Reasonable deviations.", "High error: Significant deviations detected."),
    rae = c("Excellent: Predictive power is strong compared to a mean-based model.", "Good: Predictive power is adequate.", "High error: Limited predictive power compared to a mean-based model."),
    rse = c("Excellent: Predictive power is strong compared to a mean-based model.", "Good: Predictive power is adequate.", "High error: Limited predictive power compared to a mean-based model.")
  )
  
  # Helper function for result interpretation
  get_result <- function(metric, value) {
    if (metric %in% c("mse", "rmse")) {
      if (value < 1) return(interpretation[[metric]][1])
      if (value < 5) return(interpretation[[metric]][2])
      return(interpretation[[metric]][3])
    }
    if (metric == "mae") {
      if (value < 1) return(interpretation[[metric]][1])
      if (value < 5) return(interpretation[[metric]][2])
      return(interpretation[[metric]][3])
    }
    if (metric == "mape") {
      if (value < 10) return(interpretation[[metric]][1])
      if (value < 20) return(interpretation[[metric]][2])
      return(interpretation[[metric]][3])
    }
    if (metric == "rmsle") {
      if (value < 0.1) return(interpretation[[metric]][1])
      if (value < 0.5) return(interpretation[[metric]][2])
      return(interpretation[[metric]][3])
    }
    if (metric %in% c("rae", "rse")) {
      if (value < 0.5) return(interpretation[[metric]][1])
      if (value < 1) return(interpretation[[metric]][2])
      return(interpretation[[metric]][3])
    }
    return("Interpretation not available.")
  }
  
  # Compile error metrics using dictionary-based approach
  error_metrics <- lapply(names(metrics), function(metric) {
    list(
      value = metrics[[metric]],
      description = descriptions[[metric]],
      result = get_result(metric, metrics[[metric]])
    )
  })
  
  # Name the list elements
  names(error_metrics) <- names(metrics)
  
  return(error_metrics)
}


# ---------------------------------------------------------------------------- #
# Elegant and Scalable Normality Tests with Dictionary Approach
# ---------------------------------------------------------------------------- #
compute_normality_tests <- function(residuals) {
  
  # Compute normality tests
  tests <- list(
    shapiro = shapiro.test(residuals),
    ks = ks.test(residuals, "pnorm", mean(residuals), sd(residuals)),
    jb = jarque.bera.test(residuals),
    ad = ad.test(residuals),
    cvm = cvm.test(residuals)
  )
  
  # Lookup table for descriptions
  descriptions <- list(
    shapiro = "Shapiro-Wilk Test assesses the null hypothesis that the data is normally distributed. Best for small samples (n < 50).",
    ks = "Kolmogorov-Smirnov Test compares the sample distribution with a normal distribution. Sensitive to both the center and tails of the distribution.",
    jb = "Jarque-Bera Test evaluates skewness and kurtosis to assess normality, commonly used for large samples.",
    ad = "Anderson-Darling Test gives more weight to the tails of the distribution, making it powerful for detecting deviations in the tails.",
    cvm = "Cramer-von Mises Test is a variant of the Anderson-Darling Test, emphasizing overall differences between the empirical and normal distributions."
  )
  
  # Result interpretation lookup table
  interpretation <- list(
    shapiro = "p-value < 0.05 suggests non-normality; p-value ≥ 0.05 suggests normality.",
    ks = "p-value < 0.05 suggests the sample differs significantly from a normal distribution.",
    jb = "p-value < 0.05 suggests the data significantly deviates from normality based on skewness and kurtosis.",
    ad = "p-value < 0.05 indicates significant deviation from normality, especially in the tails.",
    cvm = "p-value < 0.05 suggests significant differences between the sample and a normal distribution."
  )
  
  # Helper function for result interpretation
  get_result <- function(test) {
    p_value <- test$p.value
    if (p_value < 0.01) {
      return("Strong evidence against normality (p < 0.01).")
    } else if (p_value < 0.05) {
      return("Moderate evidence against normality (p < 0.05).")
    } else if (p_value < 0.1) {
      return("Weak evidence against normality (p < 0.1).")
    } else {
      return("No evidence against normality (p ≥ 0.1).")
    }
  }
  
  # Compile normality tests using dictionary-based approach
  normality_tests <- lapply(names(tests), function(test) {
    list(
      statistic = tests[[test]]$statistic,
      p_value = tests[[test]]$p.value,
      description = descriptions[[test]],
      interpretation = interpretation[[test]],
      result = get_result(tests[[test]])
    )
  })
  
  # Name the list elements
  names(normality_tests) <- names(tests)
  
  return(normality_tests)
}

# ---------------------------------------------------------------------------- #
# Elegant and Scalable Heteroscedasticity Tests with Dictionary Approach
# ---------------------------------------------------------------------------- #

compute_heteroscedasticity_tests <- function(model) {
  
  # Compute heteroscedasticity tests
  tests <- list(
    breusch_pagan = bptest(model),
    white = bptest(model, ~ fitted(model) + I(fitted(model)^2)),
    goldfeld_quandt = gqtest(model)
  )
  
  # Lookup table for descriptions
  descriptions <- list(
    breusch_pagan = "Breusch-Pagan Test detects heteroscedasticity by checking if residual variance depends on independent variables.",
    white = "White's Test is a generalization of the Breusch-Pagan Test, detecting both linear and nonlinear forms of heteroscedasticity.",
    goldfeld_quandt = "Goldfeld-Quandt Test checks for heteroscedasticity by comparing variances across different parts of the data, assuming a specific order."
  )
  
  # Interpretation lookup table
  interpretation <- list(
    breusch_pagan = "p-value < 0.05 suggests evidence of heteroscedasticity; p-value ≥ 0.05 suggests homoscedasticity.",
    white = "p-value < 0.05 suggests evidence of heteroscedasticity; p-value ≥ 0.05 suggests homoscedasticity.",
    goldfeld_quandt = "p-value < 0.05 suggests evidence of heteroscedasticity; p-value ≥ 0.05 suggests homoscedasticity."
  )
  
  # Helper function for result interpretation
  get_result <- function(test) {
    p_value <- test$p.value
    if (p_value < 0.01) {
      return("Strong evidence of heteroscedasticity (p < 0.01).")
    } else if (p_value < 0.05) {
      return("Moderate evidence of heteroscedasticity (p < 0.05).")
    } else if (p_value < 0.1) {
      return("Weak evidence of heteroscedasticity (p < 0.1).")
    } else {
      return("No evidence of heteroscedasticity (p ≥ 0.1).")
    }
  }
  
  # Compile heteroscedasticity tests using dictionary-based approach
  heteroscedasticity_tests <- lapply(names(tests), function(test) {
    list(
      statistic = tests[[test]]$statistic,
      p_value = tests[[test]]$p.value,
      description = descriptions[[test]],
      interpretation = interpretation[[test]],
      result = get_result(tests[[test]])
    )
  })
  
  # Name the list elements
  names(heteroscedasticity_tests) <- names(tests)
  
  return(heteroscedasticity_tests)
}


# ---------------------------------------------------------------------------- #
# Elegant and Scalable Autocorrelation Tests with Dictionary Approach
# ---------------------------------------------------------------------------- #
compute_autocorrelation_tests <- function(model, residuals) {
  
  # Compute autocorrelation tests
  tests <- list(
    durbin_watson = dwtest(model),
    ljung_box = Box.test(residuals, lag = log(length(residuals)), type = "Ljung-Box"),
    breusch_godfrey = bgtest(model)
  )
  
  # Lookup table for descriptions
  descriptions <- list(
    durbin_watson = "Durbin-Watson Test checks for first-order autocorrelation in residuals. Values near 2 indicate no autocorrelation.",
    ljung_box = "Ljung-Box Test checks if residuals are independently distributed across multiple lags. Used to detect serial correlation.",
    breusch_godfrey = "Breusch-Godfrey Test detects higher-order autocorrelation in residuals, accommodating lagged variables."
  )
  
  # Interpretation lookup table
  interpretation <- list(
    durbin_watson = "Durbin-Watson statistic near 2 suggests no autocorrelation; < 2 suggests positive autocorrelation; > 2 suggests negative autocorrelation.",
    ljung_box = "p-value < 0.05 suggests evidence of autocorrelation; p-value ≥ 0.05 suggests no evidence of autocorrelation.",
    breusch_godfrey = "p-value < 0.05 suggests evidence of higher-order autocorrelation; p-value ≥ 0.05 suggests no evidence of autocorrelation."
  )
  
  # Helper function for result interpretation
  get_result <- function(test, test_name) {
    if (test_name == "durbin_watson") {
      stat <- test$statistic
      if (stat < 1.5) {
        return("Evidence of positive autocorrelation.")
      } else if (stat > 2.5) {
        return("Evidence of negative autocorrelation.")
      } else {
        return("No evidence of autocorrelation (statistic near 2).")
      }
    } else {
      p_value <- test$p.value
      if (p_value < 0.01) {
        return("Strong evidence of autocorrelation (p < 0.01).")
      } else if (p_value < 0.05) {
        return("Moderate evidence of autocorrelation (p < 0.05).")
      } else if (p_value < 0.1) {
        return("Weak evidence of autocorrelation (p < 0.1).")
      } else {
        return("No evidence of autocorrelation (p ≥ 0.1).")
      }
    }
  }
  
  # Compile autocorrelation tests using dictionary-based approach
  autocorrelation_tests <- lapply(names(tests), function(test) {
    list(
      statistic = tests[[test]]$statistic,
      p_value = ifelse("p.value" %in% names(tests[[test]]), tests[[test]]$p.value, NA),
      description = descriptions[[test]],
      interpretation = interpretation[[test]],
      result = get_result(tests[[test]], test)
    )
  })
  
  # Name the list elements
  names(autocorrelation_tests) <- names(tests)
  
  return(autocorrelation_tests)
}

# ---------------------------------------------------------------------------- #
# Elegant and Scalable Linearity Tests with Dictionary Approach
# ---------------------------------------------------------------------------- #
compute_linearity_tests <- function(model) {
  
  # Compute linearity tests
  tests <- list(
    harvey_collier = harvtest(model),
    ramsey_reset = resettest(model)
  )
  
  # Lookup table for descriptions
  descriptions <- list(
    harvey_collier = "Harvey-Collier Test checks if the relationship between dependent and independent variables is linear by testing the null hypothesis that the model is correctly specified.",
    ramsey_reset = "Ramsey's RESET Test detects model misspecification by testing if higher-order terms of fitted values can explain the response variable, suggesting potential non-linearity or omitted variables."
  )
  
  # Interpretation lookup table
  interpretation <- list(
    harvey_collier = "p-value < 0.05 suggests that the model may not be linear; p-value ≥ 0.05 suggests no evidence against linearity.",
    ramsey_reset = "p-value < 0.05 suggests evidence of model misspecification or non-linearity; p-value ≥ 0.05 suggests no evidence of misspecification."
  )
  
  # Helper function for result interpretation
  get_result <- function(test) {
    p_value <- test$p.value
    if (p_value < 0.01) {
      return("Strong evidence of non-linearity or model misspecification (p < 0.01).")
    } else if (p_value < 0.05) {
      return("Moderate evidence of non-linearity or model misspecification (p < 0.05).")
    } else if (p_value < 0.1) {
      return("Weak evidence of non-linearity or model misspecification (p < 0.1).")
    } else {
      return("No evidence of non-linearity or misspecification (p ≥ 0.1).")
    }
  }
  
  # Compile linearity tests using dictionary-based approach
  linearity_tests <- lapply(names(tests), function(test) {
    list(
      statistic = tests[[test]]$statistic,
      p_value = tests[[test]]$p.value,
      description = descriptions[[test]],
      interpretation = interpretation[[test]],
      result = get_result(tests[[test]])
    )
  })
  
  # Name the list elements
  names(linearity_tests) <- names(tests)
  
  return(linearity_tests)
}

# ---------------------------------------------------------------------------- #
# Elegant and Scalable Multicollinearity Metrics with Dictionary Approach
# ---------------------------------------------------------------------------- #
compute_multicollinearity_metrics <- function(model) {
  
  # Compute multicollinearity metrics
  vif_values <- vif(model)
  condition_number <- kappa(model)
  
  # Lookup table for descriptions
  descriptions <- list(
    vif = "Variance Inflation Factor (VIF) measures how much the variance of a regression coefficient is inflated due to multicollinearity:\n  - VIF = 1: No multicollinearity.\n  - VIF > 5: Moderate multicollinearity.\n  - VIF > 10: High multicollinearity.",
    condition_number = "Condition Number assesses multicollinearity by examining the sensitivity of the regression solution to changes in the independent variables:\n  - < 10: No multicollinearity.\n  - 10–30: Moderate multicollinearity.\n  - > 30: High multicollinearity."
  )
  
  # Interpretation lookup table
  interpretation <- list(
    vif = "VIF > 5 suggests moderate multicollinearity; VIF > 10 suggests high multicollinearity.",
    condition_number = "Condition number > 30 suggests high multicollinearity."
  )
  
  # Helper function for result interpretation
  get_vif_result <- function(vif_values) {
    max_vif <- max(vif_values)
    if (max_vif > 10) {
      return("High multicollinearity detected (VIF > 10). Consider removing or combining predictors.")
    } else if (max_vif > 5) {
      return("Moderate multicollinearity detected (5 < VIF ≤ 10). Be cautious of predictor redundancy.")
    } else {
      return("No significant multicollinearity detected (VIF ≤ 5).")
    }
  }
  
  get_condition_number_result <- function(cn) {
    if (cn > 30) {
      return("High multicollinearity detected (Condition Number > 30). Consider dimensionality reduction techniques.")
    } else if (cn > 10) {
      return("Moderate multicollinearity detected (10 < Condition Number ≤ 30).")
    } else {
      return("No significant multicollinearity detected (Condition Number ≤ 10).")
    }
  }
  
  # Compile multicollinearity metrics using dictionary-based approach
  multicollinearity_metrics <- list(
    vif = list(
      value = vif_values,
      description = descriptions$vif,
      interpretation = interpretation$vif,
      result = get_vif_result(vif_values)
    ),
    condition_number = list(
      value = condition_number,
      description = descriptions$condition_number,
      interpretation = interpretation$condition_number,
      result = get_condition_number_result(condition_number)
    )
  )
  
  return(multicollinearity_metrics)
}

# ---------------------------------------------------------------------------- #
# Elegant and Scalable Influence Metrics with Dictionary Approach
# ---------------------------------------------------------------------------- #

compute_influence_metrics <- function(model) {
  
  # Compute influence metrics
  cooks <- cooks.distance(model)
  leverage <- hatvalues(model)
  dfbetas <- dfbetas(model)
  mahalanobis_values <- mahalanobis(model.matrix(model), colMeans(model.matrix(model)), cov(model.matrix(model)))
  covratio_values <- covratio(model)
  hadi_values <- hatvalues(model) * cooks.distance(model)
  
  # Lookup table for descriptions
  descriptions <- list(
    cooks = "Cook's Distance measures the influence of a data point by assessing how much the regression coefficients change when it is removed:\n  - > 1 suggests highly influential points.",
    leverage = "Leverage values measure the influence of individual data points based on their position in the predictor space:\n  - High leverage indicates potential outliers.",
    dfbetas = "DFBETAS quantifies how much each coefficient changes when a data point is excluded:\n  - Absolute values > 2/√n suggest influential points.",
    mahalanobis = "Mahalanobis Distance measures the distance of a point from the mean of the predictor variables considering correlations:\n  - High values suggest outliers.",
    covratio = "COVRATIO evaluates the influence of data points on the covariance matrix of coefficients:\n  - Values outside (1 ± 3p/n) suggest influential points.",
    hadi = "Hadi's Influence Measure detects both leverage points and influential observations by combining leverage and Cook's Distance."
  )
  
  # Interpretation lookup table
  interpretation <- list(
    cooks = "Cook's Distance > 1 suggests high influence.",
    leverage = "Leverage > 2p/n suggests potential outliers.",
    dfbetas = "DFBETAS > 2/√n suggests high influence.",
    mahalanobis = "High Mahalanobis Distance suggests outliers.",
    covratio = "COVRATIO outside (1 ± 3p/n) suggests high influence.",
    hadi = "High Hadi's Measure suggests influential points."
  )
  
  # Helper function for result interpretation
  get_result <- function(metric, values) {
    n <- length(values)
    p <- length(coef(model))
    if (metric == "cooks") {
      if (any(values > 1)) return("High influence detected (Cook's Distance > 1).")
      else return("No high influence detected (Cook's Distance ≤ 1).")
    }
    if (metric == "leverage") {
      threshold <- 2 * p / n
      if (any(values > threshold)) return(sprintf("High leverage detected (Leverage > %.3f).", threshold))
      else return("No high leverage detected.")
    }
    if (metric == "dfbetas") {
      threshold <- 2 / sqrt(n)
      if (any(abs(values) > threshold)) return(sprintf("High influence detected (DFBETAS > %.3f).", threshold))
      else return("No high influence detected (DFBETAS ≤ threshold).")
    }
    if (metric == "mahalanobis") {
      threshold <- qchisq(0.99, df = p)
      if (any(values > threshold)) return(sprintf("Outliers detected (Mahalanobis Distance > %.2f).", threshold))
      else return("No outliers detected.")
    }
    if (metric == "covratio") {
      threshold_low <- 1 - 3 * p / n
      threshold_high <- 1 + 3 * p / n
      if (any(values < threshold_low | values > threshold_high)) return("High influence detected (COVRATIO outside threshold).")
      else return("No high influence detected (COVRATIO within threshold).")
    }
    if (metric == "hadi") {
      if (any(values > 1)) return("High influence detected (Hadi's Measure > 1).")
      else return("No high influence detected.")
    }
    return("Interpretation not available.")
  }
  
  # Compile influence metrics using dictionary-based approach
  influence_metrics <- list(
    cooks = list(
      value = cooks,
      description = descriptions$cooks,
      interpretation = interpretation$cooks,
      result = get_result("cooks", cooks)
    ),
    leverage = list(
      value = leverage,
      description = descriptions$leverage,
      interpretation = interpretation$leverage,
      result = get_result("leverage", leverage)
    ),
    dfbetas = list(
      value = dfbetas,
      description = descriptions$dfbetas,
      interpretation = interpretation$dfbetas,
      result = get_result("dfbetas", dfbetas)
    ),
    mahalanobis = list(
      value = mahalanobis_values,
      description = descriptions$mahalanobis,
      interpretation = interpretation$mahalanobis,
      result = get_result("mahalanobis", mahalanobis_values)
    ),
    covratio = list(
      value = covratio_values,
      description = descriptions$covratio,
      interpretation = interpretation$covratio,
      result = get_result("covratio", covratio_values)
    ),
    hadi = list(
      value = hadi_values,
      description = descriptions$hadi,
      interpretation = interpretation$hadi,
      result = get_result("hadi", hadi_values)
    )
  )
  
  return(influence_metrics)
}



# ---------------------------------------------------------------------------- #
# Elegant and Scalable Robustness Metrics with Dictionary Approach
# ---------------------------------------------------------------------------- #

compute_robustness_metrics <- function(model, data, response) {
  library(boot)
  library(robustbase)
  
  # Compute robust model coefficients
  robust_model <- lmrob(as.formula(paste(response, "~ .")), data = data)
  
  # Compute jackknife estimates
  jackknife <- jackknife(coef(model))
  
  # Bootstrap estimates
  bootstrap_func <- function(data, indices) {
    d <- data[indices, ]
    fit <- lm(as.formula(paste(response, "~ .")), data = d)
    return(coef(fit))
  }
  bootstrap <- boot(data = data, statistic = bootstrap_func, R = 1000)
  
  # Cross-validation (10-fold)
  cross_val <- cv.glm(data, model, K = 10)
  
  # Lookup table for descriptions
  descriptions <- list(
    robust_model = "Robust Model uses M-estimation to reduce the influence of outliers, providing more reliable coefficients.",
    jackknife = "Jackknife Resampling generates multiple sub-samples by leaving one observation out, assessing model stability.",
    bootstrap = "Bootstrap Resampling creates multiple samples with replacement to estimate the variability of coefficients.",
    cross_validation = "Cross-Validation (10-fold) assesses model performance on unseen data by splitting the data into training and validation sets."
  )
  
  # Interpretation lookup table
  interpretation <- list(
    robust_model = "Coefficients from the robust model indicate how sensitive the original model is to outliers.",
    jackknife = "Small variations in jackknife estimates suggest a stable model.",
    bootstrap = "Narrow confidence intervals in bootstrap estimates suggest a stable model.",
    cross_validation = "Low cross-validation error suggests strong predictive performance on unseen data."
  )
  
  # Helper function for result interpretation
  get_result <- function(metric, values) {
    if (metric == "robust_model") {
      if (any(abs(coef(robust_model) - coef(model)) > 0.1)) {
        return("Robust model coefficients differ significantly from original, suggesting sensitivity to outliers.")
      } else {
        return("Robust model coefficients are similar to original, suggesting stability.")
      }
    }
    if (metric == "jackknife") {
      if (max(values) > 0.1) {
        return("High variation in jackknife estimates, suggesting model instability.")
      } else {
        return("Low variation in jackknife estimates, suggesting model stability.")
      }
    }
    if (metric == "bootstrap") {
      if (max(bootstrap$t0 - apply(bootstrap$t, 2, mean)) > 0.1) {
        return("High variability in bootstrap estimates, suggesting model instability.")
      } else {
        return("Low variability in bootstrap estimates, suggesting model stability.")
      }
    }
    if (metric == "cross_validation") {
      if (cross_val$delta[2] < 0.1) {
        return("Low cross-validation error, suggesting strong predictive performance.")
      } else {
        return("High cross-validation error, suggesting potential overfitting or limited predictive performance.")
      }
    }
    return("Interpretation not available.")
  }
  
  # Compile robustness metrics using dictionary-based approach
  robustness_metrics <- list(
    robust_model = list(
      value = coef(robust_model),
      description = descriptions$robust_model,
      interpretation = interpretation$robust_model,
      result = get_result("robust_model", coef(robust_model))
    ),
    jackknife = list(
      value = jackknife$jack.values,
      description = descriptions$jackknife,
      interpretation = interpretation$jackknife,
      result = get_result("jackknife", jackknife$jack.values)
    ),
    bootstrap = list(
      value = apply(bootstrap$t, 2, function(x) c(mean = mean(x), sd = sd(x))),
      description = descriptions$bootstrap,
      interpretation = interpretation$bootstrap,
      result = get_result("bootstrap", bootstrap$t)
    ),
    cross_validation = list(
      value = cross_val$delta,
      description = descriptions$cross_validation,
      interpretation = interpretation$cross_validation,
      result = get_result("cross_validation", cross_val$delta)
    )
  )
  
  return(robustness_metrics)
}
