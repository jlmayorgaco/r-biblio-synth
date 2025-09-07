# stats/models/LogisticModel.R
LogisticModel <- R6::R6Class(
  "LogisticModel",
  inherit = BaseModel,
  public = list(

    fit = function(df) {
      self$df <- df
      self$model <- nls2::nls2(
        Value ~ SSlogis(Year, Asym, xmid, scal),
        data = df,
        start = list(
          Asym = max(df$Value, na.rm = TRUE),
          xmid = mean(df$Year, na.rm = TRUE),
          scal = 1
        )
      )
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
        type    = "logistic",
        formula = deparse(formula(self$model)),
        coef    = as.list(coef(self$model)),
        Robustness = robust_for_json(self$robust),
        confint = tryCatch(as.data.frame(confint(self$model)), error = function(e) NULL),
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
      Asym <- coef_vals$Asym
      xmid <- coef_vals$xmid
      scal <- coef_vals$scal

      # Function for t(alpha)
      t_alpha <- function(alpha) {
        xmid - scal * log((1/alpha) - 1)
      }

      t5     <- t_alpha(0.05)
      t50    <- xmid
      t95    <- t_alpha(0.95)
      deltaT <- t95 - t5

      list(
        list(param_name = "Asym", param_latex = "$N_0$",
             param_value = Asym, param_axis = "y"),
        list(param_name = "xmid", param_latex = "$t_{50}$",
             param_value = t50, param_axis = "x"),
        list(param_name = "t5", param_latex = "$t_{5\\%}$",
             param_value = t5, param_axis = "x"),
        list(param_name = "t95", param_latex = "$t_{95\\%}$",
             param_value = t95, param_axis = "x"),
        list(param_name = "Delta_t", param_latex = "$\\Delta t$",
             param_value = deltaT, param_axis = NULL)
      )
    },

    timing_metrics = function() {
      coef_vals <- as.list(coef(self$model))
      Asym <- coef_vals$Asym
      xmid <- coef_vals$xmid
      scal <- coef_vals$scal

      # Function for t(alpha)
      t_alpha <- function(alpha) {
        xmid - scal * log((1/alpha) - 1)
      }

      t5     <- t_alpha(0.05)
      t50    <- xmid
      t95    <- t_alpha(0.95)
      deltaT <- t95 - t5

      data.frame(
        Asym   = Asym,
        xmid   = xmid,
        scal   = scal,
        t5     = t5,
        t50    = t50,
        t95    = t95,
        deltaT = deltaT
      )
    }
  )
)

# Register in MODEL_REGISTRY
MODEL_REGISTRY$logistic <- LogisticModel
