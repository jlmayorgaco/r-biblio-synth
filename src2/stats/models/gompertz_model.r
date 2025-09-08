# stats/models/GompertzModel.R
GompertzModel <- R6::R6Class(
  "GompertzModel",
  inherit = BaseModel,
  public = list(

    fit = function(df) {
      self$df <- df

      cat("[GompertzModel] Starting fit...\n")
      cat("[GompertzModel] Data summary: ",
          "n=", nrow(df),
          ", Year ∈ [", min(df$Year, na.rm=TRUE), ",", max(df$Year, na.rm=TRUE), "]",
          ", Value ∈ [", min(df$Value, na.rm=TRUE), ",", max(df$Value, na.rm=TRUE), "]\n")

      # --- Robust initial guesses ---
      a_start <- max(df$Value, na.rm = TRUE)                # asymptote
      y0      <- min(df$Value, na.rm = TRUE)                # initial value
      mid_y   <- median(df$Value, na.rm = TRUE)             # midpoint value

      # b: shift factor
      b_start <- log(a_start / max(y0, 1))                  # avoid log(0)
      if (!is.finite(b_start) || b_start <= 0) b_start <- 1

      # c: growth rate estimate
      slope_est <- diff(range(log(df$Value + 1e-8))) / diff(range(df$Year))
      c_start   <- max(slope_est, 0.01)
      if (!is.finite(c_start) || c_start <= 0) c_start <- 0.05

      start_list <- list(a = a_start, b = b_start, c = c_start)
      cat("[GompertzModel] Initial parameters guess:\n")
      print(start_list)

      # Scale Year for numerical stability
      df$Year_scaled <- df$Year - min(df$Year, na.rm = TRUE)

      # --- Control parameters for nlsLM ---
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
          Value ~ a * exp(-b * exp(-c * Year_scaled)),
          data  = df,
          start = start_list,
          control = ctrl_lm,
          trace = TRUE
        )
      }, error = function(e) {
        cat("[GompertzModel] ERROR in nlsLM fit:", e$message, "\n")
        NULL
      })

      # --- If nlsLM fails, try randomized nls2 ---
      if (is.null(self$model)) {
        cat("[GompertzModel] Falling back to randomized nls2...\n")
        start_grid <- data.frame(
          a = seq(0.8, 1.2, length.out = 5) * a_start,
          b = seq(0.5, 2, length.out = 5) * b_start,
          c = seq(0.5, 2, length.out = 5) * c_start
        )
        self$model <- tryCatch({
          suppressWarnings(
            nls2::nls2(
              Value ~ a * exp(-b * exp(-c * Year_scaled)),
              data  = df,
              start = start_grid,
              algorithm = "brute-force",
              control = nls.control(maxiter = 2000, tol = 1e-6, minFactor = 1e-10),
              trace = TRUE
            )
          )
        }, error = function(e) {
          cat("[GompertzModel] Fallback nls2 also failed:", e$message, "\n")
          NULL
        })
      }

      if (!is.null(self$model)) {
        cat("[GompertzModel] Fit completed successfully.\n")
        cat("[GompertzModel] Coefficients:\n")
        print(coef(self$model))
      } else {
        cat("[GompertzModel] Fit failed completely.\n")
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
        type    = "gompertz",
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
      a <- coef_vals$a
      b <- coef_vals$b
      c <- coef_vals$c

      # Characteristic times (scaled years)
      t0     <- log(b) / c
      t5     <- (1 / c) * log(b / -log(0.05))
      t95    <- (1 / c) * log(b / -log(0.95))
      deltaT <- t95 - t5

      # Re-shift to original Year scale
      year0 <- min(self$df$Year, na.rm = TRUE)
      t0  <- t0 + year0
      t5  <- t5 + year0
      t95 <- t95 + year0

      list(
        list(param_name = "a", param_latex = "$a$", 
             param_value = a, param_axis = "y"),
        list(param_name = "b", param_latex = "$b$", 
             param_value = b, param_axis = NULL),
        list(param_name = "c", param_latex = "$c$", 
             param_value = c, param_axis = NULL),
        list(param_name = "t5", param_latex = "$t_{5\\%}$", 
             param_value = t5, param_axis = "x"),
        list(param_name = "t95", param_latex = "$t_{95\\%}$", 
             param_value = t95, param_axis = "x"),
        list(param_name = "Delta_t", param_latex = "$\\Delta t$", 
             param_value = deltaT, param_axis = NULL)
      )
    }
  )
)

# Register in MODEL_REGISTRY
MODEL_REGISTRY$gompertz <- GompertzModel
