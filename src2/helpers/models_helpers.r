# helpers/stats_helpers.R

# --- Compute R² safely ---
get_model_r2 <- function(model, df = NULL, response_col = "Value") {
  if (is.null(model)) return(NA_real_)
  
  if (inherits(model, "lm")) {
    return(tryCatch(summary(model)$r.squared, error = function(e) NA_real_))
  }
  
  if (inherits(model, "nls") && !is.null(df) && response_col %in% colnames(df)) {
    rss <- tryCatch(deviance(model), error = function(e) NA_real_)
    tss <- tryCatch(sum((df[[response_col]] - mean(df[[response_col]], na.rm = TRUE))^2, na.rm = TRUE),
                    error = function(e) NA_real_)
    return(ifelse(tss > 0, 1 - rss/tss, NA_real_))
  }
  
  return(NA_real_)
}

# --- Summarize a model (JSON safe) ---
summarize_model <- function(model, df = NULL, response_col = "Value") {
  if (is.null(model)) return(NULL)
  
  r2 <- get_model_r2(model, df, response_col)
  
  if (inherits(model, "lm")) {
    return(list(
      type = "linear",
      formula = deparse(formula(model)),
      coef = as.list(coef(model)),
      stats = c(as.list(broom::glance(model)), list(R2 = r2))
    ))
  }
  
  if (inherits(model, "nls")) {
    return(list(
      type = "logistic",
      formula = deparse(formula(model)),
      coef = as.list(coef(model)),
      stats = list(rss = tryCatch(deviance(model), error = function(e) NA_real_), R2 = r2)
    ))
  }
  
  list(type = class(model)[1], R2 = r2)
}
# helpers/model_helpers.R

safe_fit <- function(expr) tryCatch(expr, error = function(e) NULL)

# ================================================================
# fit_models — Fit all registered models and select the best (by R²)
# ================================================================
fit_models <- function(df, response_col = "Value") {
  cat("[fit_models] Starting model fitting...\n")
  cat("[fit_models] Response column:", response_col, "\n")

  if (!exists("MODEL_REGISTRY") || length(MODEL_REGISTRY) == 0) {
    stop("[fit_models] ERROR: MODEL_REGISTRY is empty. Did you source your models?")
  }

  cat("[fit_models] Models available in registry:", paste(names(MODEL_REGISTRY), collapse = ", "), "\n")

  fitted_models <- list()

  for (m_name in names(MODEL_REGISTRY)) {
    cat("[fit_models] Trying model:", m_name, "\n")
    model_class <- MODEL_REGISTRY[[m_name]]

    if (!inherits(model_class, "R6ClassGenerator")) {
      warning("[fit_models] Skipping", m_name, "— not an R6 class.")
      next
    }

    instance <- tryCatch({
      cat("[fit_models][", m_name, "] Creating instance...\n")
      obj <- model_class$new()
      cat("[fit_models][", m_name, "] Fitting model...\n")
      obj$fit(df)
     obj$robust_check(
      x_col = "Year", y_col = "Value",
      methods = c("RESID_Z", "COOK_LM"),
      thresholds = list(param_rel = 0.20, pred_mape = 0.15, r2_drop = 0.03),
      min_keep_ratio = 0.75
    )
      cat("[fit_models][", m_name, "] Fit completed successfully.\n")
      obj
    }, error = function(e) {
      warning(
        "[fit_models][", m_name, "] ERROR during fitting: ", e$message,
        "\n  Class: ", paste(class(e), collapse = ", "),
        "\n  Call: ", deparse(conditionCall(e))
      )
      
      # Si tienes rlang instalado, usar esto:
      if (requireNamespace("rlang", quietly = TRUE)) {
        cat("[fit_models][", m_name, "] Backtrace:\n")
        print(rlang::trace_back())
      }
      
      NULL
    })


    if (!is.null(instance)) {
      fitted_models[[m_name]] <- instance
      cat("[fit_models][", m_name, "] Model stored in fitted_models.\n")
    } else {
      cat("[fit_models][", m_name, "] Model fitting failed, not stored.\n")
    }
  }

  if (length(fitted_models) == 0) {
    warning("[fit_models] No models could be fitted. Returning empty list.\n")
    return(list(best_model = NULL, other_models = list()))
  }

  cat("[fit_models] Computing R² scores for all fitted models...\n")

  r2_scores <- sapply(names(fitted_models), function(m) {
    tryCatch({
      score <- fitted_models[[m]]$r2()
      cat("[fit_models][", m, "] R² score:", score, "\n")
      score
    }, error = function(e) {
      warning("[fit_models][", m, "] ERROR computing R²:", e$message)
      NA_real_
    })
  })

  cat("[fit_models] R² scores summary:\n")
  print(r2_scores)

  best_name <- names(which.max(r2_scores))
  best_model <- fitted_models[[best_name]]
  best_r2 <- r2_scores[[best_name]]

  cat("[fit_models] Best model selected:", best_name, "with R² =", best_r2, "\n")

  # Remove best from others
  other_models <- fitted_models[names(fitted_models) != best_name]

  cat("[fit_models] Returning best model and other models.\n")

  return(list(
    best_model   = best_model,
    best_name    = best_name,
    best_r2      = best_r2,
    other_models = other_models,
    r2_scores    = r2_scores
  ))
}



