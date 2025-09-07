# stats/models/PolynomialModel.R
PolynomialModel <- R6::R6Class(
  "PolynomialModel",
  inherit = BaseModel,
  public = list(
    degree = 2,  # default: quadratic
    
    fit = function(df, degree = 2) {
      self$df <- df
      self$degree <- degree
      
      cat("[PolynomialModel] Starting fit (degree =", degree, ")...\n")
      cat("[PolynomialModel] Data summary: ",
          "n=", nrow(df),
          ", Year ∈ [", min(df$Year, na.rm=TRUE), ",", max(df$Year, na.rm=TRUE), "]",
          ", Value ∈ [", min(df$Value, na.rm=TRUE), ",", max(df$Value, na.rm=TRUE), "]\n")
      
      # Scale Year for numerical stability
      df$Year_scaled <- df$Year - min(df$Year, na.rm = TRUE)
      
      # --- Build polynomial formula dynamically ---
      terms <- paste0("b", 0:degree, " * (Year_scaled^", 0:degree, ")", collapse = " + ")
      form <- as.formula(paste("Value ~", terms))
      
      # --- Initial guesses ---
      start_list <- as.list(setNames(rep(0.1, degree + 1), paste0("b", 0:degree)))
      start_list$b0 <- mean(df$Value, na.rm = TRUE) # intercept ~ avg value
      
      cat("[PolynomialModel] Initial parameters guess:\n")
      print(start_list)
      
      # --- Control parameters for nlsLM ---
      ctrl_lm <- minpack.lm::nls.lm.control(
        maxiter = 5000,
        ftol = 1e-10,
        ptol = 1e-10,
        factor = 100,
        maxfev = 10000
      )
      
      # --- Fit with nlsLM ---
      self$model <- tryCatch({
        minpack.lm::nlsLM(
          form,
          data  = df,
          start = start_list,
          control = ctrl_lm,
          trace = TRUE
        )
      }, error = function(e) {
        cat("[PolynomialModel] ERROR in nlsLM fit:", e$message, "\n")
        NULL
      })
      
      if (!is.null(self$model)) {
        cat("[PolynomialModel] Fit completed successfully.\n")
        cat("[PolynomialModel] Coefficients:\n")
        print(coef(self$model))
      } else {
        cat("[PolynomialModel] Fit failed.\n")
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
        type   = paste0("polynomial (degree ", self$degree, ")"),
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
      guides <- list()
      i <- 0
      for (name in names(coef_vals)) {
        latex <- if (i == 0) "$b_0$" else paste0("$b_{", i, "}$")
        guides[[length(guides) + 1]] <- list(
          param_name  = name,
          param_latex = latex,
          param_value = coef_vals[[name]],
          param_axis  = NULL
        )
        i <- i + 1
      }
      guides
    }
  )
)

# Register in MODEL_REGISTRY
MODEL_REGISTRY$polynomial <- PolynomialModel
