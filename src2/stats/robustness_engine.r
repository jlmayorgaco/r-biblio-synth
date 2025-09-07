# ============================================================================ #
# stats/RobustnessEngine.R
# RobustnessEngine — encapsulates IEEE-grade robustness workflow for BaseModel
# ============================================================================ #
# Dependencies: robust_helpers.R (detect_outliers, compare_params, etc.)
# Works with:   any subclass of BaseModel (has $fit(), $predict(), $r2(), hooks)
# Outputs:      list of artifacts (outliers, fits, metrics, deltas, diagnostics)
#               + helper methods to export and plot.
# ============================================================================ #

suppressPackageStartupMessages({
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("jsonlite", quietly = TRUE)
  requireNamespace("ggplot2", quietly = TRUE)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

RobustnessEngine <- R6::R6Class(
  "RobustnessEngine",
  public = list(
    # ----------------------------- Config ----------------------------------- #
    model          = NULL,     # a fitted BaseModel subclass instance
    x_col          = "Year",
    y_col          = "Value",
    methods        = c("IQR","MAD","RESID_Z","COOK_LM"),
    thresholds     = list(param_rel = 0.10, pred_mape = 0.10, r2_drop = 0.02),
    min_keep_ratio = 0.80,
    # ----------------------------- Artifacts -------------------------------- #
    outliers    = NULL,        # from detect_outliers()
    df_full     = NULL,
    df_clean    = NULL,
    fit_clean   = NULL,        # cloned BaseModel fit on df_clean
    fit_alt     = NULL,        # optional robust alternative fit (object, not R6)
    metrics     = NULL,        # tibble rows: full/clean/alt
    param_deltas= NULL,        # tibble with abs/rel changes
    pred_deltas = NULL,        # list with MAPE/RMSE/MAE + bands
    diagnostics = NULL,        # residual SDs, shapiro p-values (optional)
    decision    = NULL,        # list(is_robust, reasons, keep_ratio)
    # ----------------------------- Initialize --------------------------------#
    initialize = function(model,
                          x_col = "Year",
                          y_col = "Value",
                          methods = c("IQR","MAD","RESID_Z","COOK_LM"),
                          thresholds = list(param_rel = 0.10, pred_mape = 0.10, r2_drop = 0.02),
                          min_keep_ratio = 0.80) {
      stopifnot(!is.null(model))
      self$model          <- model
      self$x_col          <- x_col
      self$y_col          <- y_col
      self$methods        <- toupper(methods)
      self$thresholds     <- thresholds
      self$min_keep_ratio <- min_keep_ratio
    },

    # ----------------------------- Run workflow ----------------------------- #
    run = function() {
      # 1) Ensure fitted + data
      if (is.null(self$model$model)) {
        if (is.null(self$model$df)) stop("[RobustnessEngine$run] model has no data (model$df is NULL).")
        self$model$fit(self$model$df)
      }
      self$df_full <- self$model$df
      if (!is.data.frame(self$df_full)) stop("[RobustnessEngine$run] model$df must be a data.frame.")

      # 2) Full predictions/residuals + metrics
      pred_full  <- try(self$model$predict(self$df_full), silent = TRUE)
      if (inherits(pred_full, "try-error")) pred_full <- rep(NA_real_, nrow(self$df_full))
      pred_full <- as.numeric(pred_full)

      resid_full <- try(self$model$residuals(), silent = TRUE)
      if (inherits(resid_full, "try-error")) resid_full <- self$df_full[[self$y_col]] - pred_full

      metrics_full <- metrics_from_fit(self$model, self$df_full, self$y_col, pred = pred_full)

      # 3) Outlier screening
      lm_formula <- try(self$model$lm_surrogate_formula(), silent = TRUE)
      if (inherits(lm_formula, "try-error") || is.null(lm_formula)) {
        lm_formula <- stats::as.formula(paste0(self$y_col, " ~ ", self$x_col))
      }
      self$outliers <- detect_outliers(
        df         = self$df_full,
        x          = self$x_col,
        y          = self$y_col,
        methods    = self$methods,
        residuals  = as.numeric(resid_full),
        lm_formula = lm_formula
      )
      keep_idx   <- which(!self$outliers$mask)
      keep_ratio <- length(keep_idx) / max(1L, nrow(self$df_full))
      if (keep_ratio < self$min_keep_ratio) {
        warning(sprintf("[RobustnessEngine$run] Only %.1f%% kept (< %.0f%%).",
                        100*keep_ratio, 100*self$min_keep_ratio))
      }
      self$df_clean <- self$df_full[keep_idx, , drop = FALSE]

      # 4) Refit same model on clean
      clone <- self$model$clone(deep = TRUE); clone$model <- NULL; clone$df <- NULL
      clone$fit(self$df_clean)
      self$fit_clean <- clone

      metrics_clean <- metrics_from_fit(self$fit_clean, self$df_clean, self$y_col)

      # 5) Optional robust alt fit
      alt <- try(self$model$robust_alt_fit(self$df_full), silent = TRUE)
      has_alt <- !inherits(alt, "try-error") && !is.null(alt)
      if (has_alt) self$fit_alt <- alt
      metrics_alt <- if (has_alt) try(metrics_from_fit(self$fit_alt, self$df_full, self$y_col), silent = TRUE) else NULL

      # 6) Param stability
      coef_full  <- try(stats::coef(self$model$model),  silent = TRUE)
      coef_clean <- try(stats::coef(self$fit_clean$model), silent = TRUE)
      param_deltas <- if (!inherits(coef_full, "try-error") && !inherits(coef_clean, "try-error")) {
        compare_params(coef_full, coef_clean)
      } else NULL
      self$param_deltas <- param_deltas

      # 7) Prediction stability on same X
      pred_clean_on_full <- try(stats::predict(self$fit_clean$model, newdata = self$df_full), silent = TRUE)
      if (inherits(pred_clean_on_full, "try-error")) pred_clean_on_full <- rep(NA_real_, nrow(self$df_full))
      self$pred_deltas <- compare_predictions(pred_full, as.numeric(pred_clean_on_full), self$df_full[[self$y_col]])

      # 8) Goodness-of-fit stability
      param_ok <- TRUE
      if (!is.null(self$param_deltas) && NROW(self$param_deltas)) {
        thr <- self$thresholds$param_rel %||% 0.10
        param_ok <- !any(self$param_deltas$rel_change > thr, na.rm = TRUE)
      }
      pred_ok <- isTRUE(!is.null(self$pred_deltas$MAPE) &&
                        is.finite(self$pred_deltas$MAPE) &&
                        self$pred_deltas$MAPE <= (self$thresholds$pred_mape %||% 0.10))
      r2_ok <- TRUE
      if (is.finite(metrics_full$R2) && is.finite(metrics_clean$R2)) {
        r2_ok <- ((metrics_full$R2 - metrics_clean$R2) <= (self$thresholds$r2_drop %||% 0.02))
      }

      # 9) Diagnostics (lightweight)
      resid_sd_full  <- stats::sd(resid_full, na.rm = TRUE)
      resid_sd_clean <- try(stats::sd(self$df_clean[[self$y_col]] -
                                as.numeric(stats::predict(self$fit_clean$model, newdata = self$df_clean)), na.rm = TRUE), silent = TRUE)
      shapiro_p_full  <- if (nrow(self$df_full)  <= 5000) try(stats::shapiro.test(resid_full)$p.value,  silent = TRUE) else NA_real_
      shapiro_p_clean <- if (nrow(self$df_clean) <= 5000) try(stats::shapiro.test(self$df_clean[[self$y_col]] -
                                  as.numeric(stats::predict(self$fit_clean$model, newdata = self$df_clean)))$p.value, silent = TRUE) else NA_real_
      self$diagnostics <- list(
        resid_sd_full  = if (inherits(resid_sd_clean, "try-error")) as.numeric(resid_sd_full) else as.numeric(resid_sd_full),
        resid_sd_clean = if (inherits(resid_sd_clean, "try-error")) NA_real_ else as.numeric(resid_sd_clean),
        shapiro_p_full  = if (inherits(shapiro_p_full,  "try-error")) NA_real_ else as.numeric(shapiro_p_full),
        shapiro_p_clean = if (inherits(shapiro_p_clean, "try-error")) NA_real_ else as.numeric(shapiro_p_clean)
      )

      # 10) Decision & metrics table
      is_robust <- (param_ok && pred_ok && r2_ok)
      reasons <- c()
      if (!param_ok) reasons <- c(reasons, "param_rel_change_exceeds_threshold")
      if (!pred_ok)  reasons <- c(reasons, "prediction_mape_exceeds_threshold")
      if (!r2_ok)    reasons <- c(reasons, "r2_drop_exceeds_threshold")

      self$decision <- list(
        is_robust  = is_robust,
        reasons    = reasons,
        keep_ratio = keep_ratio
      )

      mt <- dplyr::bind_rows(
        dplyr::mutate(as.data.frame(metrics_full),  set = "full"),
        dplyr::mutate(as.data.frame(metrics_clean), set = "clean")
      )
      if (has_alt && !inherits(metrics_alt, "try-error")) {
        mt <- dplyr::bind_rows(mt, dplyr::mutate(as.data.frame(metrics_alt), set = "alt"))
      }
      self$metrics <- mt

      invisible(self)
    },

    # ----------------------------- Getters ---------------------------------- #
    get_clean_df   = function() self$df_clean,
    get_decision   = function() self$decision,
    get_metrics    = function() self$metrics,
    get_param_deltas = function() self$param_deltas,
    get_pred_deltas  = function() self$pred_deltas,

    # ----------------------------- Export ----------------------------------- #
    export = function(path) {
      if (is.null(path)) return(invisible(FALSE))
      jsonlite::write_json(
        list(
          outliers    = self$outliers,
          metrics     = self$metrics,
          param_deltas= self$param_deltas,
          pred_deltas = self$pred_deltas,
          diagnostics = self$diagnostics,
          decision    = self$decision
        ),
        path, auto_unbox = TRUE, pretty = TRUE
      )
      invisible(TRUE)
    },

    # ----------------------------- Plot (overlay) --------------------------- #
    plot_overlay = function(title = "Robustness overlay") {
      stopifnot(!is.null(self$model), !is.null(self$fit_clean), !is.null(self$df_full))
      df <- self$df_full
      # mark outliers (any method)
      pp <- self$outliers$detail$per_point
      is_out <- rep(FALSE, nrow(df))
      for (flag in intersect(c("IQR","MAD","RESID_Z","COOK_LM","RESPONSE_Z"), colnames(pp))) {
        is_out <- is_out | as.logical(pp[[flag]])
      }
      df$._out <- is_out

      p_full  <- tryCatch(as.numeric(self$model$predict(df)), error = function(e) rep(NA_real_, nrow(df)))
      p_clean <- tryCatch(as.numeric(stats::predict(self$fit_clean$model, newdata = df)), error = function(e) rep(NA_real_, nrow(df)))

      yv <- df[[self$y_col]]
      y_max <- 1.05 * max(yv, p_full, p_clean, na.rm = TRUE)

      ggplot2::ggplot(df, ggplot2::aes_string(self$x_col, self$y_col)) +
        ggplot2::geom_point(size = 1.6, shape = 16, color = "black") +
        ggplot2::geom_point(data = subset(df, ._out), shape = 1, size = 2, stroke = 0.7) +
        ggplot2::geom_line(ggplot2::aes(y = p_full), linewidth = 0.7) +
        ggplot2::geom_line(ggplot2::aes(y = p_clean), linewidth = 0.7, linetype = "22") +
        ggplot2::coord_cartesian(ylim = c(0, y_max)) +
        ggplot2::labs(x = self$x_col, y = self$y_col, title = title) +
        ggplot2::theme_minimal(base_size = 9, base_family = "Times New Roman") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    }
  )
)
