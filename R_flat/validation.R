# ============================================================================
# validation.R - Shared validation helpers
# ============================================================================

#' Validate that input is a data frame
#'
#' @param data An object to check.
#' @param label Character. Label for error messages.
#' @return TRUE invisibly if valid; stops with error otherwise.
#' @export
validate_is_data_frame <- function(data, label = "data") {
  if (!is.data.frame(data)) {
    cli::cli_abort("{label} must be a data frame, got {class(data)}.")
  }
  invisible(TRUE)
}

#' Validate required columns exist in a data frame
#'
#' @param data A data frame.
#' @param required_columns Character vector of column names.
#' @return A list with \code{ok} (logical) and \code{missing_columns} (character).
#' @export
validate_required_columns <- function(data, required_columns) {
  present <- intersect(required_columns, names(data))
  missing <- setdiff(required_columns, names(data))
  list(
    ok              = length(missing) == 0,
    missing_columns = missing
  )
}
