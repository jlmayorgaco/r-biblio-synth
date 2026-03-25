# ============================================================================
# m1_column_accessors.R - Safe column accessors for M1
# ============================================================================

#' @export
m1_get_authors_col <- function(data) {
  if ("AU" %in% names(data)) data[["AU"]] else character()
}

#' @export
m1_get_year_col <- function(data) {
  if ("PY" %in% names(data)) data[["PY"]] else integer()
}

#' @export
m1_get_title_col <- function(data) {
  if ("TI" %in% names(data)) data[["TI"]] else character()
}

#' @export
m1_get_source_col <- function(data) {
  if ("SO" %in% names(data)) data[["SO"]] else character()
}

#' @export
m1_get_doi_col <- function(data) {
  if ("DI" %in% names(data)) data[["DI"]] else character()
}

#' @export
m1_get_keywords_de_col <- function(data) {
  if ("DE" %in% names(data)) data[["DE"]] else character()
}

#' @export
m1_get_keywords_id_col <- function(data) {
  if ("ID" %in% names(data)) data[["ID"]] else character()
}
