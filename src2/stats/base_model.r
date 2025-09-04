# ================================================================
# stats/base_model.R
# ================================================================

BaseModel <- R6::R6Class(
  "BaseModel",
  public = list(
    model        = NULL,           # fitted model object (lm, nls, etc.)
    df           = NULL,           # training dataframe
    response_col = "Value",        # default response column
    
    # --- Fit the model ---
    fit = function(df) {
      stop("[BaseModel] fit() must be implemented in subclass")
    },
    
    # --- Predict on new data ---
    predict = function(newdata) {
      if (is.null(self$model)) stop("[BaseModel] No model fitted yet")
      stats::predict(self$model, newdata)
    },
    
    # --- R² score ---
    r2 = function() {
      NA_real_ # override in subclass
    },
    
    # --- JSON-safe summary (override or extend in subclass) ---
    summary = function() {
      list(
        type   = class(self)[1],
        coef   = tryCatch(as.list(coef(self$model)), error = function(e) NULL),
        stats  = list(R2 = self$r2())
      )
    },
    
    # --- Graph annotations (override in subclass if needed) ---
    # Each annotation: param_name, param_latex, param_value, param_axis
    graphs = function() {
      list()
    }
  )
)
