normalize_countries <- function(country_list) {
  if (!requireNamespace("countrycode", quietly = TRUE)) return(country_list)
  lapply(country_list, function(cl) {
    if (is.null(cl) || length(cl) == 0 || all(is.na(cl))) return(NA_character_)
    cl <- as.character(cl)
    out <- suppressWarnings(countrycode::countrycode(cl,
      origin = "country.name", destination = "country.name", warn = FALSE))
    out[!is.na(out)]
  })
}

flatten_list_columns_for_csv <- function(df) {
  for (col in names(df)) {
    if (is.list(df[[col]])) {
      df[[col]] <- vapply(df[[col]], function(x) {
        if (is.null(x) || all(is.na(x))) return("")
        paste(x, collapse = "; ")
      }, FUN.VALUE = character(1))
    }
  }
  df
}
