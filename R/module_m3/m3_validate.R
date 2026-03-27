# ============================================================================
# m3_validate.R - Validation for M3 Countries Module
# ============================================================================

#' Validate M3 input
#'
#' @param input A data frame containing bibliographic data.
#' @return A list with \code{ok} (logical) and \code{missing_columns} (character).
#' @export
validate_m3_input <- function(input) {
  if (!is.data.frame(input)) {
    return(list(ok = FALSE, missing_columns = NULL, error = "Input must be a data frame"))
  }
  
  # We require at least AU_CO for country extraction, but we can try to extract from C1 if AU_CO is missing
  # However, for minimal validation we check for AU_CO. If missing, we note that we may try C1.
  required_columns <- c("AU_CO")
  validation <- validate_required_columns(input, required_columns)
  
  if (!validation$ok) {
    # We don't fail immediately because we might extract from C1, but we note the issue.
    # We'll let the preparation function handle the fallback and record in diagnostics.
    # For now, we return the validation but with a warning that we might try C1.
    # We'll change the validation to not fail on missing AU_CO, but we'll record in the preparation step.
    # So we change: we don't require AU_CO, but we require at least one of AU_CO or C1.
    required_columns <- c("AU_CO", "C1")
    validation2 <- validate_required_columns(input, required_columns)
    if (!validation2$ok) {
      return(list(ok = FALSE, missing_columns = validation2$missing_columns, 
                  error = "Neither AU_CO nor C1 columns found. At least one is required for country extraction."))
    }
  }
  
  # We also want PY for temporal analysis, but it's not required for all metrics.
  # We'll let the preparation function handle missing PY and record in diagnostics.
  
  return(list(ok = TRUE, missing_columns = character(0)))
}