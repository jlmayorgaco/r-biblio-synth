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
  
  # Require at least one of AU_CO or C1 for country extraction.
  has_au_co <- "AU_CO" %in% names(input)
  has_c1    <- "C1"    %in% names(input)

  if (!has_au_co && !has_c1) {
    return(list(
      ok              = FALSE,
      missing_columns = c("AU_CO", "C1"),
      error           = "Neither AU_CO nor C1 column found. At least one is required for country extraction.",
      n_rows          = nrow(input)
    ))
  }

  return(list(ok = TRUE, missing_columns = character(0), n_rows = nrow(input)))
}