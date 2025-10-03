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
# fit_models — Fit all registered models and select the best
#               (metric: "AIC", "BIC", "AICc", "adjR2", "R2",
#                "RMSE", "MAE", "MAPE")
# ================================================================
fit_models <- function(
  df,
  response_col = "Value",
  metric = c("AIC", "BIC", "AICc", "adjR2", "R2", "RMSE", "MAE", "MAPE"),
  tiebreakers = c("AICc", "BIC", "AIC", "adjR2", "R2")
) {
  metric <- match.arg(metric)
  cat("[fit_models] Starting model fitting...\n")
  cat("[fit_models] Response column:", response_col, "\n")
  cat("[fit_models] Selection metric:", metric, "\n")

  if (!exists("MODEL_REGISTRY") || length(MODEL_REGISTRY) == 0) {
    stop("[fit_models] ERROR: MODEL_REGISTRY is empty. Did you source your models?")
  }

  cat("[fit_models] Models available in registry:", paste(names(MODEL_REGISTRY), collapse = ", "), "\n")

  fitted_models <- list()

  # ---- helpers ---------------------------------------------------------------
  .safe_stat <- function(expr, default = NA_real_) {
    tryCatch(expr, error = function(e) default)
  }

  .n_k <- function(obj) {
    n <- .safe_stat(sum(!is.na(obj$df[[obj$response_col]])))
    k <- .safe_stat(length(coef(obj$model)))
    list(n = n, k = k)
  }

  .AICc <- function(obj) {
    nk <- .n_k(obj); n <- nk$n; k <- nk$k
    aic <- .safe_stat(AIC(obj$model))
    if (is.na(aic) || is.na(n) || is.na(k) || (n - k - 1) <= 0) return(NA_real_)
    aic + (2 * k * (k + 1)) / (n - k - 1)
  }

  .adjR2 <- function(obj) {
    # generic adjR2 from RSS/TSS with df correction
    y <- obj$df[[obj$response_col]]
    rss <- .safe_stat(deviance(obj$model))
    tss <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
    nk <- .n_k(obj); n <- nk$n; k <- nk$k
    if (is.na(rss) || tss <= 0 || is.na(n) || is.na(k) || (n - k - 1) <= 0) return(NA_real_)
    r2 <- 1 - rss / tss
    1 - (1 - r2) * (n - 1) / (n - k - 1)
  }

  .residuals <- function(obj) {
    .safe_stat(residuals(obj$model), default = rep(NA_real_, nrow(obj$df)))
  }

  .rmse <- function(obj) {
    r <- .residuals(obj); r <- r[is.finite(r)]
    if (!length(r)) return(NA_real_)
    sqrt(mean(r^2))
  }

  .mae <- function(obj) {
    r <- .residuals(obj); r <- r[is.finite(r)]
    if (!length(r)) return(NA_real_)
    mean(abs(r))
  }

  .mape <- function(obj) {
    y <- obj$df[[obj$response_col]]
    r <- .residuals(obj)
    if (length(r) != length(y)) return(NA_real_)
    idx <- is.finite(r) & is.finite(y) & y != 0
    if (!any(idx)) return(NA_real_)
    mean(abs(r[idx] / y[idx]))
  }

  .collect_metrics <- function(name, obj) {
    list(
      model = name,
      R2    = .safe_stat(obj$r2(), NA_real_),
      adjR2 = .adjR2(obj),
      AIC   = .safe_stat(AIC(obj$model), NA_real_),
      BIC   = .safe_stat(BIC(obj$model), NA_real_),
      AICc  = .AICc(obj),
      RMSE  = .rmse(obj),
      MAE   = .mae(obj),
      MAPE  = .mape(obj)
    )
  }

  # ---- fit all ---------------------------------------------------------------
  for (m_name in names(MODEL_REGISTRY)) {
    cat("[fit_models] Trying model:", m_name, "\n")
    model_class <- MODEL_REGISTRY[[m_name]]

    if (!inherits(model_class, "R6ClassGenerator")) {
      warning("[fit_models] Skipping ", m_name, " — not an R6 class.")
      next
    }

    instance <- tryCatch({
      cat("[fit_models][", m_name, "] Creating instance...\n", sep = "")
      obj <- model_class$new()
      obj$response_col <- response_col
      cat("[fit_models][", m_name, "] Fitting model...\n", sep = "")
      obj$fit(df)

      # Optional: your robustness routine
      obj$robust_check(
        x_col = "Year", y_col = response_col,
        methods = c("RESID_Z", "COOK_LM"),
        thresholds = list(param_rel = 0.20, pred_mape = 0.15, r2_drop = 0.03),
        min_keep_ratio = 0.75
      )

      cat("[fit_models][", m_name, "] Fit completed successfully.\n", sep = "")
      obj
    }, error = function(e) {
      warning(
        "[fit_models][", m_name, "] ERROR during fitting: ", e$message,
        "\n  Class: ", paste(class(e), collapse = ", ")
      )
      if (requireNamespace("rlang", quietly = TRUE)) {
        cat("[fit_models][", m_name, "] Backtrace:\n")
        print(rlang::trace_back())
      }
      NULL
    })

    if (!is.null(instance)) {
      fitted_models[[m_name]] <- instance
      cat("[fit_models][", m_name, "] Model stored in fitted_models.\n", sep = "")
    } else {
      cat("[fit_models][", m_name, "] Model fitting failed, not stored.\n", sep = "")
    }
  }

  if (length(fitted_models) == 0) {
    warning("[fit_models] No models could be fitted. Returning empty list.\n")
    return(list(best_model = NULL, other_models = list(), metrics = NULL))
  }

  # ---- compute metrics -------------------------------------------------------
  cat("[fit_models] Computing metrics for all fitted models...\n")
  metrics_list <- lapply(names(fitted_models), function(nm) .collect_metrics(nm, fitted_models[[nm]]))
  metrics_df <- do.call(rbind, lapply(metrics_list, as.data.frame))
  rownames(metrics_df) <- NULL
  print(metrics_df)

  # ---- select best according to 'metric' ------------------------------------
  higher_is_better <- metric %in% c("R2", "adjR2")
  cat("[fit_models] Selecting best by", metric, "(higher is better? ", higher_is_better, ")\n", sep = "")

  valid_idx <- which(is.finite(metrics_df[[metric]]))
  if (!length(valid_idx)) {
    warning("[fit_models] All ", metric, " are NA. Falling back to R2.")
    metric <- "R2"; higher_is_better <- TRUE
    valid_idx <- which(is.finite(metrics_df[[metric]]))
    if (!length(valid_idx)) {
      warning("[fit_models] All R2 are NA. Returning first fitted model as fallback.")
      best_name <- names(fitted_models)[1]
      best_model <- fitted_models[[best_name]]
      other_models <- fitted_models[names(fitted_models) != best_name]
      return(list(
        best_model   = best_model,
        best_name    = best_name,
        best_metric  = NA_real_,
        metric_used  = "fallback_first",
        other_models = other_models,
        metrics      = metrics_df
      ))
    }
  }

  scores <- metrics_df[[metric]]
  best_idx <- if (higher_is_better) which.max(scores[valid_idx]) else which.min(scores[valid_idx])
  best_row <- valid_idx[best_idx]

  # tie-breakers (optional & ordered)
  ties <- which(scores[valid_idx] == scores[best_row])
  if (length(ties) > 1 && length(tiebreakers)) {
    cat("[fit_models] Tie detected. Applying tiebreakers:", paste(tiebreakers, collapse = " > "), "\n")
    tie_rows <- valid_idx[ties]
    pick <- tie_rows
    for (tb in tiebreakers) {
      tb_higher <- tb %in% c("R2", "adjR2")
      tb_scores <- metrics_df[[tb]][pick]
      if (all(is.na(tb_scores))) next
      best_tb <- if (tb_higher) which.max(tb_scores) else which.min(tb_scores)
      # keep only those equal to the best tb score
      best_val <- tb_scores[best_tb]
      pick <- pick[which(tb_scores == best_val)]
      if (length(pick) == 1) break
    }
    best_row <- pick[1]
  }

  best_name  <- metrics_df$model[best_row]
  best_model <- fitted_models[[best_name]]
  best_value <- metrics_df[[metric]][best_row]

  cat("[fit_models] Best model selected:", best_name, "with", metric, "=", best_value, "\n")

  other_models <- fitted_models[names(fitted_models) != best_name]

  cat("[fit_models] Returning best model, others, and metrics table.\n")
  return(list(
    best_model   = best_model,
    best_name    = best_name,
    best_metric  = best_value,
    metric_used  = metric,
    other_models = other_models,
    metrics      = metrics_df
  ))
}
