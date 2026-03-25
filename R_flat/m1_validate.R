# ============================================================================
# m1_validate.R - Input validation for M1
# ============================================================================

#' Validate M1 input
#'
#' Checks that the input is a data frame and has the minimal required columns.
#'
#' @param input A data frame or tibble.
#' @param config A configuration list.
#' @return A list with \code{ok}, \code{missing_columns}, \code{n_rows}, \code{n_cols}.
#' @export
validate_m1_input <- function(input, config = biblio_config()) {
  if (!is.data.frame(input)) {
    return(list(
      ok              = FALSE,
      missing_columns = character(),
      n_rows          = 0L,
      n_cols          = 0L,
      error           = "Input is not a data frame."
    ))
  }

  required <- m1_required_columns_minimal()
  col_check <- validate_required_columns(input, required)

  list(
    ok              = col_check$ok,
    missing_columns = col_check$missing_columns,
    n_rows          = nrow(input),
    n_cols          = ncol(input)
  )
}
