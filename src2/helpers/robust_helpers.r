# ============================================================================ #
# robust_helpers.R — Pure helpers for outlier detection & robustness metrics
# ============================================================================ #
# Provides:
#   - detect_outliers()
#   - compare_params()
#   - compare_predictions()
#   - metrics_from_fit()
#
# Notes:
# - COOK_LM uses a linear surrogate (default: Value ~ Year) to screen influence
#   even when the main model is nonlinear. This is common, transparent practice.
# - RESID_Z uses standardized residuals from YOUR fitted model (supply residuals).
# - If you also want Z-scores on the raw response, add "RESPONSE_Z" to methods.
# ============================================================================ #

suppressPackageStartupMessages({
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- Internal: safe numerics ------------------------------------------------ #
..nz <- function(x, eps = 1e-9) ifelse(abs(x) < eps, sign(x) * eps, x)

# ---- Internal: Cook's D via a simple lm surrogate --------------------------- #
.cooks_from_lm <- function(df, lm_formula, y_col = "Value") {
  out <- list(distance = rep(NA_real_, nrow(df)), threshold = NA_real_, ok = FALSE)
  try({
    m <- stats::lm(formula = lm_formula, data = df)
    cd <- stats::cooks.distance(m)
    # p = number of estimated coefficients
    p  <- length(stats::coef(m))
    n  <- NROW(stats::model.matrix(m))
    thr <- 4 / max(1, (n - p))
    out$distance  <- as.numeric(cd)
    out$threshold <- as.numeric(thr)
    out$ok <- TRUE
  }, silent = TRUE)
  out
}

# ---- Outlier detection (union over chosen methods) -------------------------- #
# methods: any subset of c("IQR","MAD","RESID_Z","COOK_LM","RESPONSE_Z")
detect_outliers <- function(df,
                            x = "Year",
                            y = "Value",
                            methods = c("IQR", "MAD", "RESID_Z", "COOK_LM"),
                            residuals = NULL,
                            lm_formula = stats::as.formula("Value ~ Year"),
                            z_cut = 3.0,
                            mad_cut = 3.5,
                            resid_z_cut = 3.5) {

  stopifnot(is.data.frame(df))
  n <- nrow(df)
  yv <- df[[y]]
  methods <- toupper(methods)

  flags <- tibble::tibble(
    idx       = seq_len(n),
    IQR       = FALSE,
    MAD       = FALSE,
    RESID_Z   = FALSE,
    COOK_LM   = FALSE,
    RESPONSE_Z= FALSE
  )

  thresholds <- list()

  # IQR on response
  if ("IQR" %in% methods) {
    Q1   <- stats::quantile(yv, 0.25, na.rm = TRUE)
    Q3   <- stats::quantile(yv, 0.75, na.rm = TRUE)
    IQRv <- Q3 - Q1
    low  <- Q1 - 1.5 * IQRv
    high <- Q3 + 1.5 * IQRv
    f    <- (yv < low) | (yv > high)
    flags$IQR <- ifelse(is.na(f), FALSE, f)
    thresholds$IQR <- list(Q1 = as.numeric(Q1), Q3 = as.numeric(Q3),
                           IQR = as.numeric(IQRv), low = as.numeric(low), high = as.numeric(high))
  }

  # MAD on response
  if ("MAD" %in% methods) {
    med  <- stats::median(yv, na.rm = TRUE)
    MADv <- stats::mad(yv, center = med, constant = 1, na.rm = TRUE)
    madz <- abs(yv - med) / ..nz(MADv)
    f    <- madz > mad_cut
    flags$MAD <- ifelse(is.na(f), FALSE, f)
    thresholds$MAD <- list(median = as.numeric(med), MAD = as.numeric(MADv), cutoff = mad_cut)
  }

  # Z-score on response
  if ("RESPONSE_Z" %in% methods) {
    mu <- mean(yv, na.rm = TRUE)
    sd <- stats::sd(yv, na.rm = TRUE)
    z  <- abs((yv - mu) / ..nz(sd))
    f  <- z > z_cut
    flags$RESPONSE_Z <- ifelse(is.na(f), FALSE, f)
    thresholds$RESPONSE_Z <- list(mean = as.numeric(mu), sd = as.numeric(sd), cutoff = z_cut)
  }

  # Standardized residuals (from the user's fitted model)
  res_vec <- residuals
  if ("RESID_Z" %in% methods) {
    if (is.null(res_vec) || length(res_vec) != n) {
      warning("[detect_outliers] RESID_Z requested but residuals not supplied or wrong length; skipping RESID_Z")
    } else {
      r_sd <- stats::sd(res_vec, na.rm = TRUE)
      rz   <- abs(res_vec / ..nz(r_sd))
      f    <- rz > resid_z_cut
      flags$RESID_Z <- ifelse(is.na(f), FALSE, f)
      thresholds$RESID_Z <- list(sd = as.numeric(r_sd), cutoff = resid_z_cut)
    }
  }

  # Cook's distance from lm surrogate
  if ("COOK_LM" %in% methods) {
    cook <- .cooks_from_lm(df, lm_formula = lm_formula, y_col = y)
    if (!identical(cook$ok, TRUE)) {
      warning("[detect_outliers] COOK_LM failed; skipping COOK_LM")
    } else {
      f <- cook$distance > cook$threshold
      flags$COOK_LM <- ifelse(is.na(f), FALSE, f)
      thresholds$COOK_LM <- list(threshold = as.numeric(cook$threshold))
    }
  }

  # Union of all active flags
  active_cols <- intersect(colnames(flags), methods)
  mask <- if (length(active_cols)) {
    as.logical(Reduce(`|`, lapply(active_cols, function(cn) flags[[cn]])))
  } else {
    rep(FALSE, n)
  }

  detail <- list(
    per_point = dplyr::bind_cols(
      tibble::tibble(index = seq_len(n),
                     !!x := df[[x]],
                     !!y := yv,
                     residual = res_vec %||% rep(NA_real_, n)),
      dplyr::select(flags, -idx)
    ),
    thresholds = thresholds,
    methods_used = methods
  )

  list(mask = mask, detail = detail)
}

# ---- Parameter comparison (full vs clean) ----------------------------------- #
# Accepts named numeric vectors or lists convertible via unlist()
compare_params <- function(params_full, params_clean, eps = 1e-9) {
  v_full  <- unlist(params_full)
  v_clean <- unlist(params_clean)

  # Align by names; if unnamed, align by position with a warning
  if (is.null(names(v_full)) || is.null(names(v_clean))) {
    warning("[compare_params] Unnamed coefficients; aligning by position")
    len <- min(length(v_full), length(v_clean))
    nm  <- paste0("p", seq_len(len))
    names(v_full)  <- nm
    names(v_clean) <- nm
    v_full  <- v_full[seq_len(len)]
    v_clean <- v_clean[seq_len(len)]
  } else {
    nm <- union(names(v_full), names(v_clean))
    v_full  <- v_full[nm];  v_clean <- v_clean[nm]
  }

  abs_change <- v_clean - v_full
  rel_change <- abs(abs_change) / pmax(eps, abs(v_full))

  tibble::tibble(
    param       = names(v_full),
    full        = as.numeric(v_full),
    clean       = as.numeric(v_clean),
    abs_change  = as.numeric(abs_change),
    rel_change  = as.numeric(rel_change)
  )
}

# ---- Prediction comparison (stability of predictions) ----------------------- #
# Compares two prediction vectors on a common X grid, relative to true y
compare_predictions <- function(pred_full, pred_clean, y,
                                eps = 1e-9,
                                bands = c(0.01, 0.05, 0.10)) {
  pred_full  <- as.numeric(pred_full)
  pred_clean <- as.numeric(pred_clean)
  y          <- as.numeric(y)

  len <- min(length(pred_full), length(pred_clean), length(y))
  pred_full  <- pred_full[seq_len(len)]
  pred_clean <- pred_clean[seq_len(len)]
  y          <- y[seq_len(len)]

  keep <- is.finite(pred_full) & is.finite(pred_clean) & is.finite(y)
  if (!any(keep)) {
    return(list(
      MAPE = NA_real_, RMSE = NA_real_, MAE = NA_real_,
      bands = NULL, n = 0
    ))
  }

  df <- tibble::tibble(
    y     = y[keep],
    d     = pred_full[keep] - pred_clean[keep],
    ad    = abs(pred_full[keep] - pred_clean[keep])
  )

  MAPE <- mean(df$ad / pmax(eps, abs(df$y)), na.rm = TRUE)
  RMSE <- sqrt(mean(df$d^2, na.rm = TRUE))
  MAE  <- mean(df$ad, na.rm = TRUE)

  band_stats <- tibble::tibble(
    band = bands,
    pct_within = vapply(bands, function(b) {
      mean(df$ad <= (b * pmax(eps, abs(df$y))), na.rm = TRUE)
    }, numeric(1))
  )

  list(
    MAPE = as.numeric(MAPE),
    RMSE = as.numeric(RMSE),
    MAE  = as.numeric(MAE),
    bands = band_stats,
    n = sum(keep)
  )
}

# ---- Unified metrics extractor ---------------------------------------------- #
# Works with:
#   - Your R6 model instances (with $predict(), $r2(), and $model field)
#   - Bare model objects (lm/nls/nlrob/rlm/lmrob/etc.)
metrics_from_fit <- function(fit, df, y_col = "Value", pred = NULL) {
  # Helper to detect R6-ish object with $predict()
  is_r6_like <- is.list(fit) && any(names(fit) == "predict")

  # Predictions
  y <- df[[y_col]]
  if (is.null(pred)) {
    pred <- try({
      if (is_r6_like) fit$predict(df) else stats::predict(fit, newdata = df)
    }, silent = TRUE)
    if (inherits(pred, "try-error")) pred <- rep(NA_real_, NROW(df))
  }
  pred <- as.numeric(pred)

  # Residuals
  resid <- y - pred

  # R2: try model-supplied, else compute
  R2 <- NA_real_
  if (is_r6_like && any(names(fit) == "r2")) {
    R2 <- try(fit$r2(), silent = TRUE)
    if (inherits(R2, "try-error")) R2 <- NA_real_
  } else {
    # generic R2 (SSE/SST) if both y & pred are finite
    keep <- is.finite(y) & is.finite(pred)
    if (any(keep)) {
      yk   <- y[keep]
      pk   <- pred[keep]
      ss_t <- sum((yk - mean(yk))^2)
      ss_e <- sum((yk - pk)^2)
      if (is.finite(ss_t) && ss_t > 0) R2 <- 1 - ss_e / ss_t
    }
  }

  # Try to find a pseudo-R2 in a model-supplied summary (if R6)
  PseudoR2 <- NA_real_
  if (is_r6_like && any(names(fit) == "summary")) {
    sm <- try(fit$summary(), silent = TRUE)
    if (!inherits(sm, "try-error") && is.list(sm) && !is.null(sm$stats)) {
      PseudoR2 <- sm$stats$PseudoR2 %||% NA_real_
    }
  }

  # AIC/BIC when available
  AICv <- try(stats::AIC(if (is_r6_like && !is.null(fit$model)) fit$model else fit), silent = TRUE)
  if (inherits(AICv, "try-error")) AICv <- NA_real_

  BICv <- try(stats::BIC(if (is_r6_like && !is.null(fit$model)) fit$model else fit), silent = TRUE)
  if (inherits(BICv, "try-error")) BICv <- NA_real_

  # Error metrics
  keep <- is.finite(y) & is.finite(pred)
  RMSE <- if (any(keep)) sqrt(mean((y[keep] - pred[keep])^2)) else NA_real_
  MAE  <- if (any(keep)) mean(abs(y[keep] - pred[keep])) else NA_real_
  MAPE <- if (any(keep)) mean(abs(y[keep] - pred[keep]) / pmax(1e-9, abs(y[keep]))) else NA_real_

  tibble::tibble(
    R2        = as.numeric(R2),
    PseudoR2  = as.numeric(PseudoR2),
    RMSE      = as.numeric(RMSE),
    MAE       = as.numeric(MAE),
    MAPE      = as.numeric(MAPE),
    AIC       = as.numeric(AICv),
    BIC       = as.numeric(BICv),
    n         = NROW(df)
  )
}
# Convert a model object to a light JSON-safe summary
fit_to_json_safe <- function(fit_obj) {
  if (is.null(fit_obj)) return(NULL)
  cls <- try(class(fit_obj), silent = TRUE)
  co  <- try(stats::coef(fit_obj), silent = TRUE)
  aic <- try(stats::AIC(fit_obj),  silent = TRUE)
  bic <- try(stats::BIC(fit_obj),  silent = TRUE)
  list(
    class = if (inherits(cls, "try-error")) NA_character_ else paste(cls, collapse = "/"),
    coef  = if (inherits(co,  "try-error")) NULL else as.list(co),
    AIC   = if (inherits(aic, "try-error")) NA_real_ else as.numeric(aic),
    BIC   = if (inherits(bic, "try-error")) NA_real_ else as.numeric(bic)
  )
}

# Convert `self$robust` to a JSON-safe copy (drops big model objects)
robust_for_json <- function(robust_list) {
  if (is.null(robust_list)) return(NULL)
  fits    <- robust_list$fits %||% list()
  safe <- list(
    outliers     = robust_list$outliers,
    metrics      = robust_list$metrics,
    param_deltas = robust_list$param_deltas,
    pred_deltas  = robust_list$pred_deltas,
    diagnostics  = robust_list$diagnostics,
    decision     = robust_list$decision,
    fits_summary = list(
      full  = fit_to_json_safe(fits$full),
      clean = fit_to_json_safe(fits$clean),
      alt   = fit_to_json_safe(fits$alt)
    )
  )
  safe
}
