# =============================================================================
# stats/base_model.R
# =============================================================================

suppressPackageStartupMessages({
  requireNamespace("jsonlite", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
})

BaseModel <- R6::R6Class(
  "BaseModel",
  public = list(
    # --- Core state ---
    model        = NULL,        # fitted model object (lm, nls, etc.)
    df           = NULL,        # training dataframe
    response_col = "Value",     # default response column

    # --- Robustness artifact (filled by robust_check) ---
    robust       = NULL,        # list(outliers, fits, metrics, param_deltas, pred_deltas, decision)

    # -------------------------------------------------------------------------
    # Fit the model (must be implemented in subclass)
    # -------------------------------------------------------------------------
    fit = function(df) {
      stop("[BaseModel] fit() must be implemented in subclass")
    },

    # -------------------------------------------------------------------------
    # Predict on new data
    # -------------------------------------------------------------------------
    predict = function(newdata) {
      if (is.null(self$model)) stop("[BaseModel] No model fitted yet")
      stats::predict(self$model, newdata)
    },

    # -------------------------------------------------------------------------
    # R² score (override in subclass if you have a custom definition)
    # -------------------------------------------------------------------------
    r2 = function() {
      NA_real_
    },

    # -------------------------------------------------------------------------
    # JSON-safe summary (override or extend in subclass)
    # -------------------------------------------------------------------------
    summary = function() {
      list(
        type   = class(self)[1],
        coef   = tryCatch(as.list(stats::coef(self$model)), error = function(e) NULL),
        stats  = list(R2 = self$r2())
      )
    },

    # -------------------------------------------------------------------------
    # Graph annotations (override in subclass if needed)
    # Each: list(param_name, param_latex, param_value, param_axis)
    # -------------------------------------------------------------------------
    graphs = function() {
      list()
    },

    # -------------------------------------------------------------------------
    # Residuals hook (default: y - predict(y))
    # Subclasses may override (e.g., studentized residuals, etc.)
    # -------------------------------------------------------------------------
    residuals = function() {
      if (is.null(self$model) || is.null(self$df)) return(rep(NA_real_, 0))
      y <- self$df[[self$response_col]]
      p <- tryCatch(as.numeric(self$predict(self$df)), error = function(e) rep(NA_real_, length(y)))
      y - p
    },

    # -------------------------------------------------------------------------
    # Optional robust alternative fit hook (default: NULL)
    # Subclasses can return an alternative robust fit (e.g., robustbase::nlrob)
    # -------------------------------------------------------------------------
    robust_alt_fit = function(df) {
      NULL
    },

  


    # -------------------------------------------------------------------------
    # Robustness check:
    #  - Detect outliers (union of methods)
    #  - Refit same model on cleaned data
    #  - (Optional) get robust alternative fit from subclass
    #  - Compare metrics/params/predictions and decide robustness
    # -------------------------------------------------------------------------
      robust_check = function(
        x_col = "Year",
        y_col = self$response_col,
        methods = c("IQR","MAD","RESID_Z","COOK_LM"),
        thresholds = list(param_rel = 0.10, pred_mape = 0.10, r2_drop = 0.02),
        min_keep_ratio = 0.80,
        save_json_path = NULL
      ) {
        engine <- RobustnessEngine$new(
          model          = self,
          x_col          = x_col,
          y_col          = y_col,
          methods        = methods,
          thresholds     = thresholds,
          min_keep_ratio = min_keep_ratio
        )
        engine$run()

        # Copy artifacts into self$robust for backward compatibility
        self$robust <- list(
          outliers     = engine$outliers,
          fits         = list(full = self$model, clean = engine$fit_clean$model, alt = engine$fit_alt),
          metrics      = engine$metrics,
          param_deltas = engine$param_deltas,
          pred_deltas  = engine$pred_deltas,
          diagnostics  = engine$diagnostics,
          decision     = engine$decision
        )

        if (!is.null(save_json_path)) {
          jsonlite::write_json(self$robust, save_json_path, auto_unbox = TRUE, pretty = TRUE)
        }

        invisible(self)
      }

  )
)
