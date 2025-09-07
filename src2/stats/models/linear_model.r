# stats/models/LinearModel.R
LinearModel <- R6::R6Class(
  "LinearModel",
  inherit = BaseModel,
  public = list(
    fit = function(df) {
      self$df <- df
      self$model <- lm(Value ~ Year, data = df)
      invisible(self)
    },
    
    r2 = function() summary(self$model)$r.squared,
    
    summary = function() {
      list(
        type   = "linear",
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
      list() # Linear doesn’t expose asymptote or inflection
    }
  )
)

# Register in MODEL_REGISTRY
MODEL_REGISTRY$linear <- LinearModel
