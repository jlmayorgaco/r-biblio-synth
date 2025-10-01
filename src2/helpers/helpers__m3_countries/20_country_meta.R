# helpers/helpers__m3_countries/20_country_meta.R
# Robust reader + mapping utilities (returns ALL columns from countries.csv)

# Expected headers (case-insensitive; extra columns allowed):
#   CODE;LAT;LON;NAME;LANGUAGE;ISO3;CONTINENT;REGION
m3c_country_meta_read <- function(path) {

  if (!file.exists(path)) {

      print(' ')
      print('path ')
      print(path)
      print(' ')
      print(' ')

      print(' ')
      print('file.exists(path) ')
      print(file.exists(path))
      print(' ')
      print(' ')

    warning(sprintf("[M3] countries.csv not found at: %s (wd: %s)", path, getwd()))
    stop(' ?????????????? ¡¡¡¡¡¡¡¡¡¡¡¡ ')
    return(NULL)
  }

  try_read <- function(sep) {
    tryCatch(
      {
        df <- utils::read.csv(path, sep = sep, stringsAsFactors = FALSE,
                              check.names = FALSE, encoding = "UTF-8")
        # normalize column names (remove spaces)
        names(df) <- gsub("\\s+", "", names(df))

        # Minimal requirement
        if (!all(c("NAME","CONTINENT") %in% names(df))) return(NULL)

        # Coerce LAT/LON numeric if present (handle commas as decimal separators)
        if ("LAT" %in% names(df)) df$LAT <- suppressWarnings(as.numeric(gsub(",", ".", df$LAT)))
        if ("LON" %in% names(df)) df$LON <- suppressWarnings(as.numeric(gsub(",", ".", df$LON)))

        # Trim/UTF-8 normalize on common string cols
        key <- intersect(c("CODE","NAME","LANGUAGE","ISO3","CONTINENT","REGION"), names(df))
        for (k in key) {
          if (!is.numeric(df[[k]]) && !is.integer(df[[k]])) {
            df[[k]] <- trimws(iconv(df[[k]], to = "UTF-8"))
          }
        }
        if ("CODE" %in% names(df)) df$CODE <- toupper(df$CODE)
        if ("ISO3" %in% names(df)) df$ISO3 <- toupper(df$ISO3)

        # Drop empty NAME rows; keep ALL columns
        df <- df[!is.na(df$NAME) & nzchar(df$NAME), , drop = FALSE]

        # Reorder: canonical first (if present), then the rest
        canon <- c("CODE","LAT","LON","NAME","LANGUAGE","ISO3","CONTINENT","REGION")
        rest  <- setdiff(names(df), canon)
        df[, c(intersect(canon, names(df)), rest), drop = FALSE]
      },
      error = function(e) NULL
    )
  }

  df <- try_read(";")
  if (is.null(df)) df <- try_read(",")
  if (is.null(df)) {
    warning(sprintf("[M3] Failed to read countries file at: %s", path))
    return(NULL)
  }
  df
}

# Common name variants → canonical NAMEs found in your CSV
m3c_country_alias <- function(x) {
  x0 <- trimws(x); x1 <- gsub("\\s+", " ", x0); xl <- tolower(x1)
  alias_tbl <- c(
    "usa"="United States","us"="United States","u.s.a."="United States",
    "united states of america"="United States",
    "uk"="United Kingdom","u.k."="United Kingdom","england"="United Kingdom",
    "scotland"="United Kingdom","wales"="United Kingdom",
    "korea, south"="South Korea","republic of korea"="South Korea","korea (republic of)"="South Korea",
    "iran"="Iran, Islamic Rep.","russia"="Russian Federation","czech republic"="Czechia",
    "viet nam"="Vietnam","hong kong"="Hong Kong SAR, China","taiwan"="Taiwan, China"
  )
  if (xl %in% names(alias_tbl)) return(alias_tbl[[xl]])
  x1
}

# Optional fallback using countrycode package (if installed)
.m3c_countrycode_fallback <- function(name) {
  if (requireNamespace("countrycode", quietly = TRUE)) {
    out <- tryCatch(countrycode::countrycode(name, origin = "country.name", destination = "continent"),
                    error = function(e) NA_character_)
    if (!is.na(out) && nzchar(out)) return(out)
  }
  NA_character_
}

# Map single country → continent using meta, alias, and fallback
m3c_country_to_continent <- function(name, meta) {
  if (is.null(meta) || is.na(name) || !nzchar(name)) return(NA_character_)
  hit <- meta$CONTINENT[match(name, meta$NAME)]
  if (!is.na(hit)) return(hit)
  ali <- m3c_country_alias(name)
  idx <- match(tolower(ali), tolower(meta$NAME))
  if (!is.na(idx)) return(meta$CONTINENT[[idx]])
  .m3c_countrycode_fallback(name)
}

# Vectorized mapping (unique, trimmed)
m3c_countries_to_continents <- function(vec, meta) {
  if (length(vec) == 0) return(character(0))
  unq <- unique(trimws(vec[!is.na(vec) & nzchar(vec)]))
  if (!length(unq)) return(character(0))
  res <- vapply(unq, m3c_country_to_continent, meta = meta, FUN.VALUE = character(1))
  unique(res[!is.na(res) & nzchar(res)])
}

# Debug helper to list unmapped country names from a country-year df
m3c_report_unmatched_countries <- function(df_cy, meta) {
  df_cy %>%
    dplyr::mutate(.continent_tmp = vapply(country, m3c_country_to_continent, meta = meta, FUN.VALUE = character(1))) %>%
    dplyr::filter(is.na(.continent_tmp) | !nzchar(.continent_tmp)) %>%
    dplyr::count(country, sort = TRUE, name = "n_rows_unmapped")
}
