# ============================================================================
# module_m2/m2_validate.R - Input validation for M2
# ============================================================================

#' Validate M2 input (annual production data)
#'
#' @param input A data frame with year and articles columns.
#' @param config A configuration list.
#' @export
validate_m2_input <- function(input, config = biblio_config()) {
  if (!is.data.frame(input)) {
    return(list(ok = FALSE, missing_columns = character(), n_rows = 0, n_cols = 0, error = "Not a data frame"))
  }

  col_check <- validate_required_columns(input, c("Year", "Articles"))
  list(
    ok = col_check$ok,
    missing_columns = col_check$missing_columns,
    n_rows = nrow(input),
    n_cols = ncol(input)
  )
}
