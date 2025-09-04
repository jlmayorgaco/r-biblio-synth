# stats/models/LogarithmicModel.R
LogarithmicModel <- R6::R6Class(
  "LogarithmicModel",
  inherit = BaseModel,
  public = list(
    fit = function(df) {
      self$df <- df
      # Starting guesses from linear regression on log(Year)
      lm_fit <- lm(Value ~ log(Year), data = df)
      start_list <- list(
        a = coef(lm_fit)[1],
        b = coef(lm_fit)[2]
      )
      self$model <- nls2::nls2(
        Value ~ a + b * log(Year),
        data  = df,
        start = start_list,
        algorithm = "port"
      )
      invisible(self)
    },
    
    r2 = function() {
      rss <- deviance(self$model)
      y <- self$df[[self$response_col]]
      tss <- sum((y - mean(y))^2, na.rm = TRUE)
      ifelse(tss > 0, 1 - rss/tss, NA_real_)
    },
    
    summary = function() {
      list(
        type   = "logarithmic",
         formula = deparse(formula(self$model)),
        coef    = as.list(coef(self$model)),
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
MODEL_REGISTRY$logarithmic <- LogarithmicModel
