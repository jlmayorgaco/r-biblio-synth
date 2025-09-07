# stats/models/PowerLawModel.R
PowerLawModel <- R6::R6Class(
  "PowerLawModel",
  inherit = BaseModel,
  public = list(

    fit = function(df) {
      self$df <- df

      cat("[PowerLawModel] Starting fit...\n")
      cat("[PowerLawModel] Data summary: ",
          "n=", nrow(df),
          ", Year ∈ [", min(df$Year, na.rm=TRUE), ",", max(df$Year, na.rm=TRUE), "]",
          ", Value ∈ [", min(df$Value, na.rm=TRUE), ",", max(df$Value, na.rm=TRUE), "]\n")

      # --- Ensure positivity (log requires > 0) ---
      df <- df[df$Year > 0 & df$Value > 0, ]
      if (nrow(df) == 0) {
        stop("[PowerLawModel] ERROR: No positive Year/Value for log-log fit.")
      }

      # --- Initial guesses from log-log regression ---
      lm_fit <- lm(log(Value) ~ log(Year), data = df)
      a_start <- exp(coef(lm_fit)[1])
      b_start <- coef(lm_fit)[2]
      start_list <- list(a = a_start, b = b_start)

      cat("[PowerLawModel] Initial parameters guess:\n")
      print(start_list)

      # --- Control for nlsLM ---
      ctrl_lm <- minpack.lm::nls.lm.control(
        maxiter = 5000,
        ftol = 1e-10,
        ptol = 1e-10,
        factor = 100,
        maxfev = 10000
      )

      # --- Try nlsLM first ---
      self$model <- tryCatch({
        minpack.lm::nlsLM(
          Value ~ a * (Year^b),
          data  = df,
          start = start_list,
          control = ctrl_lm,
          trace = TRUE
        )
      }, error = function(e) {
        cat("[PowerLawModel] ERROR in nlsLM fit:", e$message, "\n")
        NULL
      })

      # --- Fallback with nls2 brute-force if nlsLM fails ---
      if (is.null(self$model)) {
        cat("[PowerLawModel] Falling back to nls2 brute-force...\n")
        start_grid <- data.frame(
          a = seq(0.5, 1.5, length.out = 5) * a_start,
          b = seq(0.5, 1.5, length.out = 5) * b_start
        )
        self$model <- tryCatch({
          suppressWarnings(
            nls2::nls2(
              Value ~ a * (Year^b),
              data  = df,
              start = start_grid,
              algorithm = "brute-force",
              control = nls.control(maxiter = 2000, tol = 1e-6, minFactor = 1e-10),
              trace = TRUE
            )
          )
        }, error = function(e) {
          cat("[PowerLawModel] Fallback nls2 also failed:", e$message, "\n")
          NULL
        })
      }

      if (!is.null(self$model)) {
        cat("[PowerLawModel] Fit completed successfully.\n")
        cat("[PowerLawModel] Coefficients:\n")
        print(coef(self$model))
      } else {
        cat("[PowerLawModel] Fit failed completely.\n")
      }

      invisible(self)
    },

    r2 = function() {
      rss <- deviance(self$model)
      y   <- self$df[[self$response_col]]
      tss <- sum((y - mean(y))^2, na.rm = TRUE)
      ifelse(tss > 0, 1 - rss/tss, NA_real_)
    },

    summary = function() {
      list(
        type    = "power-law",
        formula = deparse(formula(self$model)),
        coef    = as.list(coef(self$model)),
        Robustness = robust_for_json(self$robust),
        stats   = list(
      rss  = tryCatch(deviance(self$model), error = function(e) NA_real_),
      R2   = self$r2(),
      AIC  = tryCatch(AIC(self$model), error = function(e) NA_real_),
      BIC  = tryCatch(BIC(self$model), error = function(e) NA_real_)
    )
      )
    },

    graphs = function() {
      coef_vals <- as.list(coef(self$model))
      list(
        list(param_name = "a", param_latex = "$a$", 
             param_value = coef_vals$a, param_axis = NULL),
        list(param_name = "b", param_latex = "$b$", 
             param_value = coef_vals$b, param_axis = NULL)
      )
    }
  )
)

# Register in MODEL_REGISTRY
MODEL_REGISTRY$powerlaw <- PowerLawModel
