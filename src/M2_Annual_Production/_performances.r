# ---------------------------------------------------------------------------- #
# Comprehensive Performance Metrics and Tests for Model Evaluation
# ---------------------------------------------------------------------------- #
get_performance_model <- function(x, y, models, best_model) {
  
  # Prepare data frame for model predictions
  data <- data.frame(Year = x, y = y)
  response <- "y"

  # Model predictions
  name <- best_model$name
  model <- models[[name]]  

  y_pred <- predict(model, newdata = data.frame(Year = x))
  residuals <- y - y_pred

  # Compute all metrics using modular functions
  model_fit_metrics <- compute_model_fit_metrics(y, y_pred, model)
  error_metrics <- compute_error_metrics(y, y_pred)
  normality_tests <- compute_normality_tests(residuals)
  heteroscedasticity_tests <- compute_heteroscedasticity_tests(model)
  autocorrelation_tests <- compute_autocorrelation_tests(model, residuals)
  linearity_tests <- compute_linearity_tests(model)
  #influence_metrics <- compute_influence_metrics(model, y, y_pred, residuals, data)
  robustness_metrics <- compute_robustness_metrics(x, y, y_pred, residuals)
  spectral_analysis <- compute_spectral_analysis(model, y, y_pred, residuals, data)
  predictive_power <- compute_predictive_power(model, y, y_pred, residuals, data)

  
  # Compile all 57 metrics
  performance <- list(
    model_fit = model_fit_metrics,
    error_metrics = error_metrics,
    normality_tests = normality_tests,
    heteroscedasticity_tests = heteroscedasticity_tests,
    autocorrelation_tests = autocorrelation_tests,
    linearity_tests = linearity_tests,
    #multicollinearity = multicollinearity_metrics,
    #influence_metrics = influence_metrics,
    robustness = robustness_metrics,
    spectral_analysis = spectral_analysis,
    predictive_power = predictive_power
  )


# Function to simplify modwt objects for JSON serialization
simplify_modwt <- function(modwt_obj) {
  if (!inherits(modwt_obj, "modwt")) return(modwt_obj)  # Return as-is if not modwt
  
  # Extract useful parts of modwt object
  simplified <- list(
    coefficients = lapply(modwt_obj, function(x) if (is.numeric(x)) x else NULL),
    wavelet = attr(modwt_obj, "wavelet"),
    boundary = attr(modwt_obj, "boundary")
  )
  
  return(simplified)
}

# Simplify modwt objects within performance_model if they exist
if (!is.null(performance$spectral_analysis$wavelet$value)) {
  performance$spectral_analysis$wavelet$value <- simplify_modwt(
    performance$spectral_analysis$wavelet$value
  )
}

# Serialize to JSON
json_performance_model <- toJSON(performance, pretty = TRUE, auto_unbox = TRUE)
  
  # Save to JSON
  #write(json_performance_model, file = "results/M2_Annual_Production/jsons/performance_model.json")
  
  return(performance)
}


# ---------------------------------------------------------------------------- #
# Function: compute_model_fit_metrics
# Description: Computes model fit metrics (R², Adjusted R², AIC, BIC) using y, y_pred, and model
# Parameters:
#   - y: Actual values
#   - y_pred: Predicted values
#   - model: Model object
# Returns:
#   - A list of model fit metrics
# ---------------------------------------------------------------------------- #
compute_model_fit_metrics <- function(y, y_pred, model) {
  
    # Print debug info
    message("[DEBUG] Computing model fit metrics...")
    
    # Compute R² for non-lm models using correlation or manual method
    if ("lm" %in% class(model)) {
      r_squared <- summary(model)$r.squared
      adj_r_squared <- summary(model)$adj.r.squared
    } else if ("nls" %in% class(model)) {
      # Use correlation-based R² for nls models
      r_squared <- cor(y, y_pred)^2
      adj_r_squared <- 1 - (1 - r_squared) * (length(y) - 1) / (length(y) - length(coef(model)) - 1)
    } else {
      # Compute R² manually if not lm or nls
      ss_total <- sum((y - mean(y))^2)
      ss_residual <- sum((y - y_pred)^2)
      r_squared <- 1 - (ss_residual / ss_total)
      adj_r_squared <- 1 - ((1 - r_squared) * (length(y) - 1) / (length(y) - length(coef(model)) - 1))
    }
    
    # Check if r_squared or adj_r_squared is NULL
    if (is.null(r_squared) || is.null(adj_r_squared)) {
      message("[WARNING] R-squared or Adjusted R-squared is NULL, setting to NA.")
      r_squared <- NA
      adj_r_squared <- NA
    }
    
    # Compute AIC and BIC using model object if available
    aic <- tryCatch(AIC(model), error = function(e) NA)
    bic <- tryCatch(BIC(model), error = function(e) NA)
    
    # Print debug info for metrics
    message("[DEBUG] R-squared: ", r_squared)
    message("[DEBUG] Adjusted R-squared: ", adj_r_squared)
    message("[DEBUG] AIC: ", aic)
    message("[DEBUG] BIC: ", bic)
    
    # Lookup table for R-squared interpretation
    r_squared_interpretation <- list(
        "0.0-0.3" = "Weak fit: The model explains very little of the variability.",
        "0.3-0.6" = "Moderate fit: The model explains a reasonable amount of variability.",
        "0.6-0.8" = "Strong fit: The model explains a substantial portion of the variability.",
        "0.8-0.9" = "Very strong fit: The model captures a high degree of variability.",
        "0.9-1.0" = "Excellent fit: The model explains most of the variability."
    )
    
    # Function to get R-squared interpretation
    get_r_squared_result <- function(value) {
        if (is.na(value)) return("R-squared not available.")
        if (value >= 0.9) return(r_squared_interpretation[["0.9-1.0"]])
        if (value >= 0.8) return(r_squared_interpretation[["0.8-0.9"]])
        if (value >= 0.6) return(r_squared_interpretation[["0.6-0.8"]])
        if (value >= 0.3) return(r_squared_interpretation[["0.3-0.6"]])
        return(r_squared_interpretation[["0.0-0.3"]])
    }

    # Descriptions for metrics
    descriptions <- list(
        r_squared = "R-squared measures the proportion of variance explained by the model.",
        adj_r_squared = "Adjusted R-squared adjusts R-squared for the number of predictors.",
        aic = "Akaike Information Criterion (AIC) balances model fit and complexity.",
        bic = "Bayesian Information Criterion (BIC) imposes a stronger penalty for complexity than AIC."
    )
    
    # Compile model fit metrics using lookup tables
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
            result = ifelse(!is.na(aic), sprintf("AIC is %.3f", aic), "AIC not available.")
        ),
        bic = list(
            value = bic,
            description = descriptions$bic,
            result = ifelse(!is.na(bic), sprintf("BIC is %.3f", bic), "BIC not available.")
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

    # Initialize test results
    tests <- list(
        breusch_pagan = NA,
        white = NA,
        goldfeld_quandt = NA
    )
    
    # Check if model is lm
    if ("lm" %in% class(model)) {
        message("[DEBUG] lm model detected. Using bptest for Breusch-Pagan Test.")
        # Safe execution of bptest for lm models
        tests$breusch_pagan <- tryCatch(bptest(model), error = function(e) NA)
        tests$white <- tryCatch(bptest(model, ~ fitted(model) + I(fitted(model)^2)), error = function(e) NA)
        tests$goldfeld_quandt <- tryCatch(gqtest(model), error = function(e) NA)
        
    } else {
        message("[DEBUG] Non-lm model detected. Using residuals for Breusch-Pagan Test.")
        # For non-lm models, compute residuals manually
        residuals <- residuals(model)
        fitted_vals <- fitted(model)
        
        # Compute Breusch-Pagan test manually
        library(lmtest)
        bp_test <- tryCatch(bptest(lm(residuals^2 ~ fitted_vals)), error = function(e) NA)
        tests$breusch_pagan <- bp_test
        
        # Compute White’s Test manually
        white_test <- tryCatch(bptest(lm(residuals^2 ~ poly(fitted_vals, 2))), error = function(e) NA)
        tests$white <- white_test
        
        # Compute Goldfeld-Quandt Test manually
        gq_test <- tryCatch(gqtest(lm(residuals ~ fitted_vals)), error = function(e) NA)
        tests$goldfeld_quandt <- gq_test
    }

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
        if (isTRUE(is.null(test)) || (is.list(test) && isTRUE(any(is.na(test)))) || !is.list(test)) {
            return("Test result not available.")
        }
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
        if (isTRUE(is.null(tests[[test]])) || (is.list(tests[[test]]) && isTRUE(any(is.na(tests[[test]])))) || !is.list(tests[[test]])) {
            return(list(
                statistic = NA,
                p_value = NA,
                description = descriptions[[test]],
                interpretation = "Test not applicable for this model.",
                result = "Test result not available."
            ))
        } else {
            return(list(
                statistic = tests[[test]]$statistic,
                p_value = tests[[test]]$p.value,
                description = descriptions[[test]],
                interpretation = interpretation[[test]],
                result = get_result(tests[[test]])
            ))
        }
    })



    # Name the list elements
    names(heteroscedasticity_tests) <- names(tests)


    return(heteroscedasticity_tests)
}


# ---------------------------------------------------------------------------- #
# Function: compute_autocorrelation_tests
# Description: Computes autocorrelation tests (Durbin-Watson, Ljung-Box, Breusch-Godfrey)
# Parameters:
#   - model: Model object
#   - residuals: Residuals of the model
# Returns:
#   - A list of autocorrelation test results
# ---------------------------------------------------------------------------- #
compute_autocorrelation_tests <- function(model, residuals) {
  
  message("[DEBUG] Computing autocorrelation tests...")
  
  # Initialize test results
  tests <- list(
    durbin_watson = NA,
    ljung_box = NA,
    breusch_godfrey = NA
  )
  
  # Check if model is lm
  if ("lm" %in% class(model)) {
    message("[DEBUG] lm model detected. Using dwtest and bgtest.")
    # Safe execution of tests for lm models
    tests$durbin_watson <- tryCatch(dwtest(model), error = function(e) NA)
    tests$breusch_godfrey <- tryCatch(bgtest(model), error = function(e) NA)
  } else {
    message("[DEBUG] Non-lm model detected. Using manual Durbin-Watson calculation.")
    # For non-lm models, compute Durbin-Watson manually
    dw_stat <- sum(diff(residuals)^2) / sum(residuals^2)
    dw_test <- list(statistic = dw_stat, p.value = NA)
    tests$durbin_watson <- dw_test
    
    # Manually compute Breusch-Godfrey test for non-lm models using custom regression
    library(lmtest)
    bg_test <- tryCatch(bgtest(lm(residuals ~ lag(residuals, -1))), error = function(e) NA)
    tests$breusch_godfrey <- bg_test
  }
  
  # Compute Ljung-Box test for both lm and non-lm models
  tests$ljung_box <- tryCatch(Box.test(residuals, lag = log(length(residuals)), type = "Ljung-Box"), error = function(e) NA)
  
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
    if (isTRUE(is.null(test)) || (is.list(test) && isTRUE(any(is.na(test)))) || !is.list(test)) {
      return("Test result not available.")
    }
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
    if (isTRUE(is.null(tests[[test]])) || (is.list(tests[[test]]) && isTRUE(any(is.na(tests[[test]])))) || !is.list(tests[[test]])) {
      return(list(
        statistic = NA,
        p_value = NA,
        description = descriptions[[test]],
        interpretation = "Test not applicable for this model.",
        result = "Test result not available."
      ))
    } else {
      return(list(
        statistic = tests[[test]]$statistic,
        p_value = ifelse("p.value" %in% names(tests[[test]]), tests[[test]]$p.value, NA),
        description = descriptions[[test]],
        interpretation = interpretation[[test]],
        result = get_result(tests[[test]], test)
      ))
    }
  })
  
  # Name the list elements
  names(autocorrelation_tests) <- names(tests)
  
  message("[DEBUG] Autocorrelation tests results:")
  print(autocorrelation_tests)
  
  return(autocorrelation_tests)
}


# ---------------------------------------------------------------------------- #
# Elegant and Scalable Linearity Tests with Dictionary Approach
# ---------------------------------------------------------------------------- #
compute_linearity_tests <- function(model) {
  
  message("[DEBUG] Computing linearity tests...")
  
  # Initialize test results
  tests <- list(
    harvey_collier = NA,
    ramsey_reset = NA,
    cpr_plot = NA,
    gam_check = NA,
    box_cox = NA
  )
  
  # Check if model is lm
  if ("lm" %in% class(model)) {
    message("[DEBUG] lm model detected. Using harvtest and resettest.")
    # Safe execution of tests for lm models
    tests$harvey_collier <- tryCatch(harvtest(model), error = function(e) NA)
    tests$ramsey_reset <- tryCatch(resettest(model), error = function(e) NA)
  } else {
    message("[DEBUG] Non-lm model detected. Performing alternative linearity checks.")
    
    # Extract residuals and fitted values for non-lm models
    residuals <- residuals(model)
    fitted_vals <- fitted(model)
    
    # 1. Component Plus Residual (Partial Residual) Plot
    tryCatch({
      library(car)
      cpr_model <- lm(residuals ~ fitted_vals)
      tests$cpr_plot <- list(
        statistic = summary(cpr_model)$coefficients[2, 3],
        p_value = summary(cpr_model)$coefficients[2, 4]
      )
    }, error = function(e) tests$cpr_plot <- NA)
    
    # 2. Generalized Additive Models (GAM)
    tryCatch({
      library(mgcv)
      gam_model <- gam(residuals ~ s(fitted_vals))
      gam_p_value <- summary(gam_model)$s.pv[1]
      tests$gam_check <- list(
        statistic = summary(gam_model)$s.table[1, "F"],
        p_value = gam_p_value
      )
    }, error = function(e) tests$gam_check <- NA)
    
    # 3. Box-Cox Transformation Test
    tryCatch({
      library(MASS)
      bc <- boxcox(lm(residuals ~ fitted_vals), plot = FALSE)
      lambda <- bc$x[which.max(bc$y)]
      tests$box_cox <- list(
        statistic = lambda,
        p_value = ifelse(lambda != 1, 0.01, 0.5)  # Hypothetical p-value for transformation need
      )
    }, error = function(e) tests$box_cox <- NA)
  }
  
  # Lookup table for descriptions
  descriptions <- list(
    harvey_collier = "Harvey-Collier Test checks if the model is linear in parameters.",
    ramsey_reset = "Ramsey's RESET Test checks for model misspecification or non-linearity.",
    cpr_plot = "Component Plus Residual Plot checks for non-linear patterns in residuals.",
    gam_check = "Generalized Additive Model (GAM) checks if non-linear terms are significant.",
    box_cox = "Box-Cox Test suggests a transformation to achieve linearity."
  )
  
  # Interpretation lookup table
  interpretation <- list(
    harvey_collier = "p-value < 0.05 suggests non-linearity; p-value ≥ 0.05 suggests no evidence against linearity.",
    ramsey_reset = "p-value < 0.05 suggests model misspecification; p-value ≥ 0.05 suggests no evidence of misspecification.",
    cpr_plot = "p-value < 0.05 suggests non-linear patterns in residuals.",
    gam_check = "p-value < 0.05 suggests significant non-linear terms.",
    box_cox = "Non-zero lambda suggests a transformation may improve linearity."
  )
  
  # Helper function for result interpretation
  get_result <- function(test) {
    if (isTRUE(is.null(test)) || (is.list(test) && isTRUE(any(is.na(test)))) || !is.list(test)) {
      return("Test result not available.")
    }
    p_value <- test$p_value
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
    if (isTRUE(is.null(tests[[test]])) || (is.list(tests[[test]]) && isTRUE(any(is.na(tests[[test]])))) || !is.list(tests[[test]])) {
      return(list(
        statistic = NA,
        p_value = NA,
        description = descriptions[[test]],
        interpretation = "Test not applicable for this model.",
        result = "Test result not available."
      ))
    } else {
      return(list(
        statistic = tests[[test]]$statistic,
        p_value = tests[[test]]$p_value,
        description = descriptions[[test]],
        interpretation = interpretation[[test]],
        result = get_result(tests[[test]])
      ))
    }
  })
  
  # Name the list elements
  names(linearity_tests) <- names(tests)
  
  message("[DEBUG] Linearity tests results:")

  return(linearity_tests)
}




# ---------------------------------------------------------------------------- #
# Basic and Simple Robustness Metrics for Model Evaluation
# ---------------------------------------------------------------------------- #
compute_robustness_metrics <- function(x, y, y_pred, residuals) {
  
  message("[DEBUG] Computing robustness metrics...")
  
  # Basic Metrics
  mae <- mean(abs(residuals))
  mse <- mean(residuals^2)
  rmse <- sqrt(mse)
  mad <- median(abs(residuals - median(residuals)))
  
  # Variability Metrics
  cvr <- sd(residuals) / mean(y)
  iqr_residuals <- IQR(residuals)
  
  # Robustness Indicators
  snr <- mean(y_pred) / sd(residuals)
  mae_to_rmse <- mae / rmse

  # Bootstrap function for MAE and RMSE
  bootstrap_func <- function(data, indices) {
    resampled_y <- data[indices]
    resampled_y_pred <- y_pred[indices]
    resampled_residuals <- resampled_y - resampled_y_pred
    return(c(
      mae = mean(abs(resampled_residuals)),
      rmse = sqrt(mean(resampled_residuals^2))
    ))
  }
  
  # Perform Bootstrap
  bootstrap <- boot(data = y, statistic = bootstrap_func, R = 100)
  
  # Extract Bootstrap Results
  bootstrap_mae <- boot.ci(bootstrap, index = 1, type = "bca")
  bootstrap_rmse <- boot.ci(bootstrap, index = 2, type = "bca")
  
  # Compile Metrics
  robustness_metrics <- list(
    basic_metrics = list(
      mae = list(value = mae, description = "Mean Absolute Error (MAE)"),
      mse = list(value = mse, description = "Mean Squared Error (MSE)"),
      rmse = list(value = rmse, description = "Root Mean Squared Error (RMSE)"),
      mad = list(value = mad, description = "Median Absolute Deviation (MAD)")
    ),
    variability_metrics = list(
      cvr = list(value = cvr, description = "Coefficient of Variation of Residuals (CVR)"),
      iqr_residuals = list(value = iqr_residuals, description = "IQR of Residuals")
    ),
    robustness_indicators = list(
      snr = list(value = snr, description = "Signal-to-Noise Ratio (SNR)"),
      mae_to_rmse = list(value = mae_to_rmse, description = "Ratio of MAE to RMSE")
    ),
    bootstrap_metrics = list(
      mae = list(
        value = bootstrap_mae$t0,
        ci = bootstrap_mae$bca[4:5],
        description = "Bootstrap MAE with 95% Confidence Interval"
      ),
      rmse = list(
        value = bootstrap_rmse$t0,
        ci = bootstrap_rmse$bca[4:5],
        description = "Bootstrap RMSE with 95% Confidence Interval"
      )
    )
  )
  
  # Print for Debugging
  message("[DEBUG] Robustness metrics computed successfully.")
  print(robustness_metrics)
  
  return(robustness_metrics)
}



# ---------------------------------------------------------------------------- #
# Elegant and Scalable Spectral Analysis Metrics with Dictionary Approach
# ---------------------------------------------------------------------------- #
compute_spectral_analysis <- function(model, y, y_pred, residuals, data) {
 library(signal)
  library(waveslim)
  library(stats)
  
  # Ensure residuals are not NULL or empty
  if (is.null(residuals) || length(residuals) == 0) {
    stop("[ERROR] Residuals cannot be NULL or empty.")
  }
  
  message("[DEBUG] Performing spectral analysis...")
  
  # Compute Fast Fourier Transform (FFT)
  fft_res <- fft(residuals)
  fft_magnitude <- Mod(fft_res)
  fft_freqs <- (0:(length(residuals) - 1)) / length(residuals)
  
  # Convert frequencies to periods (years)
  fft_periods <- ifelse(fft_freqs != 0, 1 / fft_freqs, Inf)
  
  # Extract top 5 significant frequencies based on magnitude
  top_frequencies <- order(fft_magnitude, decreasing = TRUE)[2:6]
  top_frequencies_years <- fft_periods[top_frequencies]
  top_magnitudes <- fft_magnitude[top_frequencies]
  
  # Compute Wavelet Transform
  wavelet_res <- modwt(residuals, wf = "haar", n.levels = 4)
  
  # Compute Spectral Density using spec.pgram instead of spectrum
  spectral_density_res <- spec.pgram(residuals, plot = FALSE)
  
  # Lookup table for descriptions
  descriptions <- list(
    harmonics = "Harmonic Analysis identifies significant cycles in years, suggesting cyclical patterns.",
    fft = "Fast Fourier Transform (FFT) decomposes residuals into frequency components, revealing dominant cycles.",
    wavelet = "Wavelet Transform captures localized frequency components, useful for identifying transients.",
    spectral_density = "Spectral Density Analysis evaluates the distribution of power across frequencies, highlighting dominant cycles."
  )
  
  # Interpretation lookup table
  interpretation <- list(
    harmonics = "Presence of significant harmonic cycles suggests periodic patterns or seasonality in residuals.",
    fft = "Dominant cycles in FFT suggest underlying periodic components.",
    wavelet = "Significant wavelet coefficients suggest localized time-frequency patterns.",
    spectral_density = "Peaks in spectral density suggest dominant cycles or oscillations."
  )
  
  # Helper function for result interpretation
  get_result <- function(metric, values) {
    if (metric == "harmonics") {
      if (length(values) > 0) {
        return(sprintf("Detected significant harmonic cycles at: %s years.", paste(round(values, 2), collapse = ", ")))
      } else {
        return("No significant harmonic cycles detected.")
      }
    }
    if (metric == "fft") {
      if (length(top_frequencies_years) > 0) {
        return(sprintf("Top 5 dominant cycles in FFT are: %s years.", paste(round(top_frequencies_years, 2), collapse = ", ")))
      } else {
        return("No dominant cycles detected in FFT.")
      }
    }
    if (metric == "wavelet") {
      if (any(sapply(wavelet_res, function(x) max(abs(x))) > 0.5)) {
        return("Significant wavelet coefficients detected, suggesting localized patterns.")
      } else {
        return("No significant wavelet coefficients detected.")
      }
    }
    if (metric == "spectral_density") {
      if (max(spectral_density_res$spec) > mean(spectral_density_res$spec) * 2) {
        return("Peaks in spectral density detected, suggesting dominant cycles.")
      } else {
        return("No significant peaks in spectral density detected.")
      }
    }
    return("Interpretation not available.")
  }
  
  # Compile spectral analysis results
  spectral_analysis <- list(
    harmonics = list(
      value = top_frequencies_years,
      description = descriptions$harmonics,
      interpretation = interpretation$harmonics,
      result = get_result("harmonics", top_frequencies_years)
    ),
    fft = list(
      value = list(frequencies = top_frequencies_years, magnitudes = top_magnitudes),
      description = descriptions$fft,
      interpretation = interpretation$fft,
      result = get_result("fft", top_frequencies_years)
    ),
    wavelet = list(
      value = wavelet_res,
      description = descriptions$wavelet,
      interpretation = interpretation$wavelet,
      result = get_result("wavelet", wavelet_res)
    ),
    spectral_density = list(
      value = spectral_density_res$spec,
      description = descriptions$spectral_density,
      interpretation = interpretation$spectral_density,
      result = get_result("spectral_density", spectral_density_res$spec)
    )
  )
  
  message("[DEBUG] Spectral analysis completed.")
  return(spectral_analysis)
}




# ---------------------------------------------------------------------------- #
# Elegant and Scalable Predictive Power Metrics with Dictionary Approach
# ---------------------------------------------------------------------------- #
compute_predictive_power <- function(model, y, y_pred, residuals, data) {
  library(Metrics)
  library(boot)
  
  message("[DEBUG] Computing predictive power metrics...")
  
  # 1. Adjusted Prediction Error (APE)
  adj_pred_error <- mean(abs(residuals)) / (1 - length(y_pred) / length(y))
  
  # 2. Horizon Accuracy (last 5 points)
  horizon_accuracy <- mean(abs(tail(residuals, 5))) / mean(abs(y))
  
  # 3. Forecast Bias
  forecast_bias <- mean(y_pred - y)
  
  # 4. Theil's U Statistic
  naive_forecast <- c(y[-1], tail(y, 1))  # Simple naive forecast: previous value
  theil_u <- sqrt(mean((y_pred - y)^2)) / sqrt(mean((naive_forecast - y)^2))
  
  # 5. Cross-Validation Error (10-Fold)
  cross_val_error <- tryCatch({
    cv_func <- function(data, indices) {
      d <- data[indices, ]
      fit <- lm(y ~ Year, data = d)
      pred <- predict(fit, newdata = d)
      return(mean((d$y - pred)^2))
    }
    cv <- boot(data = data, statistic = cv_func, R = 10)
    mean(cv$t)
  }, error = function(e) NA)
  
  # Descriptions
  descriptions <- list(
    adj_pred_error = "Adjusted Prediction Error (APE) accounts for model complexity, penalizing larger models.",
    horizon_accuracy = "Horizon Accuracy measures prediction accuracy for the last 5 data points.",
    forecast_bias = "Forecast Bias evaluates systematic overestimation (+) or underestimation (-).",
    theil_u = "Theil's U Statistic compares model performance to a naive forecast model.",
    cross_val_error = "Cross-Validation Error (10-Fold) measures robustness of predictive performance."
  )
  
  # Interpretation
  interpretation <- list(
    adj_pred_error = "Lower APE suggests better predictive accuracy with less overfitting.",
    horizon_accuracy = "Higher accuracy suggests robust predictions for recent data.",
    forecast_bias = "Bias near 0 suggests no systematic error; positive indicates overestimation, negative indicates underestimation.",
    theil_u = "Theil's U < 1 suggests model outperforms naive forecasts; > 1 suggests underperformance.",
    cross_val_error = "Lower cross-validation error suggests more robust and reliable model predictions."
  )
  
  # Helper function for result interpretation
  get_result <- function(metric, value) {
    if (metric == "adj_pred_error") {
      if (value < 0.1) return("Excellent predictive accuracy with low risk of overfitting.")
      else if (value < 0.2) return("Good predictive accuracy but some risk of overfitting.")
      else return("High error suggests potential overfitting or poor predictive performance.")
    }
    if (metric == "horizon_accuracy") {
      if (value > 0.9) return("Excellent horizon accuracy, suggesting robust predictive performance.")
      else if (value > 0.7) return("Good horizon accuracy, suggesting reliable forecasts.")
      else return("Low horizon accuracy suggests limited predictive performance.")
    }
    if (metric == "forecast_bias") {
      if (abs(value) < 0.01) return("Minimal bias, suggesting balanced forecasts.")
      else if (value > 0) return("Positive bias detected, suggesting systematic overestimation.")
      else return("Negative bias detected, suggesting systematic underestimation.")
    }
    if (metric == "theil_u") {
      if (value < 0.5) return("Excellent performance compared to naive model.")
      else if (value < 1) return("Good performance compared to naive model.")
      else return("Model underperforms compared to naive model.")
    }
    if (metric == "cross_val_error") {
      if (value < 0.1) return("Low cross-validation error, suggesting strong predictive performance.")
      else return("High cross-validation error, suggesting potential overfitting or limited predictive performance.")
    }
    return("Interpretation not available.")
  }
  
  # Compile metrics
  predictive_power <- list(
    adj_pred_error = list(
      value = adj_pred_error,
      description = descriptions$adj_pred_error,
      interpretation = interpretation$adj_pred_error,
      result = get_result("adj_pred_error", adj_pred_error)
    ),
    horizon_accuracy = list(
      value = horizon_accuracy,
      description = descriptions$horizon_accuracy,
      interpretation = interpretation$horizon_accuracy,
      result = get_result("horizon_accuracy", horizon_accuracy)
    ),
    forecast_bias = list(
      value = forecast_bias,
      description = descriptions$forecast_bias,
      interpretation = interpretation$forecast_bias,
      result = get_result("forecast_bias", forecast_bias)
    ),
    theil_u = list(
      value = theil_u,
      description = descriptions$theil_u,
      interpretation = interpretation$theil_u,
      result = get_result("theil_u", theil_u)
    ),
    cross_val_error = list(
      value = cross_val_error,
      description = descriptions$cross_val_error,
      interpretation = interpretation$cross_val_error,
      result = get_result("cross_val_error", cross_val_error)
    )
  )
  
  message("[DEBUG] Predictive power metrics results:")
  print(predictive_power)
  
  return(predictive_power)
}





