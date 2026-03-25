# ============================================================================
# m1_required_columns.R - Required column definitions for M1
# ============================================================================

#' Get minimal required columns for M1
#'
#' @return Character vector of minimal required column names.
#' @export
m1_required_columns_minimal <- function() {
  c("AU", "PY", "TI", "SO")
}

#' Get extended required columns for M1
#'
#' @return Character vector of extended required column names.
#' @export
m1_required_columns_extended <- function() {
  c("AU", "PY", "TI", "SO", "TC", "DI", "DT", "DE", "ID")
}
