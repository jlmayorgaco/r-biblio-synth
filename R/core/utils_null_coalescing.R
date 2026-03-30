# ============================================================================
# utils_null_coalescing.R - Null Coalescing Operator
# ============================================================================
# Central definition of null coalescing operator to avoid duplicate definitions

#' Null coalescing operator
#'
#' Returns the left operand if not NULL, otherwise the right operand.
#' This is a central definition to avoid duplicate definitions across modules.
#'
#' @param a First value (can be NULL)
#' @param b Second value (fallback)
#' @return a if not NULL, otherwise b
#' @export
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

#' Null coalescing for strings
#'
#' Returns empty string if value is NA or NULL
#' @param x Value to check
#' @return x if not NA/NULL, otherwise ""
#' @export
null_to_empty <- function(x) {
  if (is.null(x) || is.na(x)) "" else as.character(x)
}

#' Safe get from list
#'
#' Safely get a value from a list with default
#' @param lst List to get from
#' @param key Key to retrieve
#' @param default Default value if not found
#' @return Value or default
#' @export
safe_get <- function(lst, key, default = NULL) {
  if (is.null(lst) || !is.list(lst)) return(default)
  if (!key %in% names(lst)) return(default)
  lst[[key]] %||% default
}