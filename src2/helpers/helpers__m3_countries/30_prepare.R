m3c_detect_country_col <- function(df, preferred = NULL) {
  if (!is.null(preferred) && preferred %in% names(df)) return(preferred)
  common <- c("Country_List","Country_Array","country_list","country_array",
              "Countries","Country","country","countries")
  cand <- unique(c(common, names(df)[grepl("country", names(df), ignore.case = TRUE)]))
  cand <- cand[cand %in% names(df)]
  if (!length(cand)) stop("[M3] No country list column found.")
  score <- function(col) {
    x <- df[[col]]
    if (is.null(x)) return(-Inf)
    if (is.list(x)) sum(vapply(x, function(v) length(na.omit(v)) > 0, logical(1)))
    else { x <- as.character(x); x[is.na(x)] <- ""
           parts <- strsplit(x, "\\s*[,;|]\\s*")
           sum(vapply(parts, function(v) any(nzchar(trimws(v))), logical(1))) }
  }
  scores <- vapply(cand, score, numeric(1))
  cand[which.max(scores)]
}

m3c_prepare_documents <- function(df_raw, meta, country_col_guess = "Country_Array") {
  keep_cols <- c("Year","Times_Cited","Country_List","Author_Keywords",
                 "Indexed_Keywords","Abstract","Title", country_col_guess)
  df <- df_raw %>% dplyr::select(dplyr::any_of(keep_cols)) %>%
    dplyr::mutate(
      citations = suppressWarnings(as.numeric(.data[["Times_Cited"]])),
      year      = suppressWarnings(as.integer(.data[["Year"]]))
    )

  # Normalize Country_List (fallback to detected)
  if (!"Country_List" %in% names(df)) {
    col_detected <- m3c_detect_country_col(df, preferred = country_col_guess)
    df$Country_List <- df[[col_detected]]
  }

  df %>%
    dplyr::mutate(
      Country_List = gsub('c\\(|\\)|"', "", .data[["Country_List"]]),
      Country_List = gsub(",", ";", Country_List),
      .countries   = purrr::map(
        .data[["Country_List"]],
        ~ {
          if (is.null(.x)) return(character(0))
          vec <- as.character(.x)
          if (length(vec) == 1 && !is.na(vec)) vec <- strsplit(vec, "\\s*[,;|]\\s*")[[1]]
          vec <- vec[!is.na(vec) & nzchar(vec)]
          unique(trimws(vec))
        }
      ),
      .n_countries   = purrr::map_int(.countries, length),
      mcp_flag       = .n_countries >= 2,
      scp_flag       = .n_countries == 1,
      .continents    = purrr::map(.countries, m3c_countries_to_continents, meta = meta),
      Continent_List = purrr::map_chr(.continents, ~ paste(.x, collapse = "; "))
    )
}

m3c_expand_country_year <- function(df_docs) {
  df_expanded <- df_docs %>%
    tidyr::unnest_longer(.countries, values_to = "country", keep_empty = FALSE) %>%
    dplyr::mutate(country = trimws(country)) %>%
    dplyr::filter(!is.na(country) & country != "")

  df_grouped <- df_expanded %>%
    dplyr::group_by(country, year) %>%
    dplyr::summarise(
      TP  = dplyr::n(),
      TC  = sum(citations, na.rm = TRUE),
      MCP = sum(mcp_flag, na.rm = TRUE),
      SCP = sum(scp_flag, na.rm = TRUE),
      Keywords = {
        kw <- c(.data[["Author_Keywords"]], .data[["Indexed_Keywords"]])
        kw <- unlist(strsplit(kw, "\\s*;\\s*"))
        kw <- unique(trimws(kw[!is.na(kw) & nzchar(kw)]))
        paste(kw, collapse = "; ")
      },
      Titles    = paste(na.omit(.data[["Title"]]),    collapse = " || "),
      Abstracts = paste(na.omit(.data[["Abstract"]]), collapse = " || "),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      MCP_ratio = dplyr::if_else(TP > 0, MCP / TP, NA_real_),
      SCP_ratio = dplyr::if_else(TP > 0, SCP / TP, NA_real_)
    ) %>%
    dplyr::arrange(country, year)

  df_grouped
}

m3c_add_continent_to_grouped <- function(df_grouped, meta) {
  df_grouped %>%
    dplyr::mutate(
      continent = vapply(country, m3c_country_to_continent, meta = meta, FUN.VALUE = character(1))
    )
}

m3c_add_age_adjustments <- function(df_cy) {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  df_cy %>%
    dplyr::mutate(
      year = as.integer(as.character(year)),
      age  = pmax(1L, current_year - year + 1L),
      TC_adj = dplyr::if_else(is.finite(TC), TC / age, NA_real_)
    )
}
