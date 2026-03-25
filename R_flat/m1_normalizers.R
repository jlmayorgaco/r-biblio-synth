# ============================================================================
# m1_normalizers.R - Normalization helpers for M1
# ============================================================================

#' Normalize text (trim, collapse whitespace, lowercase)
#'
#' @param x Character vector.
#' @return Normalized character vector.
#' @export
m1_normalize_text <- function(x) {
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  tolower(x)
}

#' Normalize a delimited field
#'
#' @param x Character vector.
#' @param delim Delimiter to split on.
#' @return Character vector with normalized values.
#' @export
m1_normalize_delimited_field <- function(x, delim = ";") {
  parts <- unlist(strsplit(x, delim, fixed = TRUE))
  unique(trimws(parts[!is.na(parts) & parts != ""]))
}

#' Normalize country names to match map dataset
#'
#' @param x Character vector of country names.
#' @return Character vector with normalized country names.
#' @export
m1_normalize_country_names <- function(x) {
  dplyr::case_when(
    x == "USA" ~ "UNITED STATES",
    x == "KOREA" ~ "SOUTH KOREA",
    x == "UK" ~ "UNITED KINGDOM",
    TRUE ~ x
  )
}
