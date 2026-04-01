# ============================================================================
# m5_validate.R - Input validation for M5 Citation Network
# ============================================================================

#' Validate M5 input (citation network data)
#'
#' @param input A data frame with CR (cited references) field
#' @param config A configuration list
#' @return A list with ok, missing_columns, n_rows, n_cols, error
#' @export
validate_m5_input <- function(input, config = biblio_config()) {
  if (!is.data.frame(input)) {
    return(list(
      ok = FALSE,
      missing_columns = character(),
      n_rows = 0,
      n_cols = 0,
      error = "Input is not a data frame"
    ))
  }
  
  if (nrow(input) == 0) {
    return(list(
      ok = FALSE,
      missing_columns = character(),
      n_rows = 0,
      n_cols = ncol(input),
      error = "Input data frame is empty"
    ))
  }
  
  # CR (Cited References) is required for citation network analysis
  required <- "CR"
  col_check <- validate_required_columns(input, required)
  
  if (!col_check$ok) {
    return(list(
      ok = FALSE,
      missing_columns = col_check$missing_columns,
      n_rows = nrow(input),
      n_cols = ncol(input),
      error = paste("Missing required columns:", paste(col_check$missing_columns, collapse = ", "))
    ))
  }
  
  # Check CR column has data
  cr_col <- input$CR
  non_na_cr <- sum(!is.na(cr_col) & cr_col != "")
  if (non_na_cr < 3) {
    return(list(
      ok = FALSE,
      missing_columns = character(),
      n_rows = nrow(input),
      n_cols = ncol(input),
      error = "Insufficient cited references data (need at least 3 non-empty CR entries)"
    ))
  }
  
  list(
    ok = TRUE,
    missing_columns = character(),
    n_rows = nrow(input),
    n_cols = ncol(input),
    n_cited_refs = non_na_cr,
    error = NULL
  )
}