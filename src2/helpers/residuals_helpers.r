# ================================================================
# Residual Diagnostics Utilities
# ================================================================

# Helper: robust residual extractor
get_residuals <- function(model_obj) {
  # 1) try model-native residuals
  vals <- try(residuals(model_obj$model), silent = TRUE)
  if (!inherits(vals, "try-error") && !is.null(vals)) {
    vals <- as.numeric(vals)
  } else {
    # 2) fallback: y - yhat
    y <- model_obj$df[[model_obj$response_col]]
    yhat <- try(as.numeric(model_obj$predict(model_obj$df)), silent = TRUE)
    if (inherits(yhat, "try-error") || length(yhat) != length(y)) {
      return(numeric(0))
    }
    vals <- as.numeric(y - yhat)
  }
  vals[is.finite(vals)]
}
