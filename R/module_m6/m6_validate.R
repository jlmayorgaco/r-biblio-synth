# ============================================================================
# m6_validate.R - Input validation for M6 Topic Evolution
# ============================================================================

#' Validate M6 input (topic evolution data)
#'
#' @param input A data frame with keyword columns (DE or ID)
#' @param config A configuration list
#' @return A list with ok, missing_columns, n_rows, n_cols, error
#' @export
validate_m6_input <- function(input, config = biblio_config()) {
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
  
  # Either DE (author keywords) or ID (keywords plus) is required
  has_de <- "DE" %in% names(input)
  has_id <- "ID" %in% names(input)
  
  if (!has_de && !has_id) {
    return(list(
      ok = FALSE,
      missing_columns = c("DE", "ID"),
      n_rows = nrow(input),
      n_cols = ncol(input),
      error = "Missing keyword columns: need DE (author keywords) or ID (keywords plus)"
    ))
  }
  
  # Determine which column to use
  kw_col <- if (has_de) "DE" else "ID"
  
  # Check keyword column has data
  kw_data <- input[[kw_col]]
  non_na_kw <- sum(!is.na(kw_data) & kw_data != "")
  if (non_na_kw < 5) {
    return(list(
      ok = FALSE,
      missing_columns = character(),
      n_rows = nrow(input),
      n_cols = ncol(input),
      error = "Insufficient keyword data (need at least 5 non-empty keyword entries)"
    ))
  }
  
  # Check for year column (needed for topic evolution)
  if (!"PY" %in% names(input)) {
    return(list(
      ok = FALSE,
      missing_columns = "PY",
      n_rows = nrow(input),
      n_cols = ncol(input),
      error = "Missing PY (publication year) column required for topic evolution"
    ))
  }
  
  list(
    ok = TRUE,
    missing_columns = character(),
    n_rows = nrow(input),
    n_cols = ncol(input),
    keyword_column = kw_col,
    n_keywords = non_na_kw,
    error = NULL
  )
}