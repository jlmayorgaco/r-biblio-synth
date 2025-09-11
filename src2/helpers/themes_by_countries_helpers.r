# =========================== 9.1 THEMES BY COUNTRY×YEAR ===========================
# What it does:
# - Parses keywords from Author_Keywords + Indexed_Keywords (list or delimited strings)
# - Expands multi-country papers
# - Counts themes per (Country, Year) using document-frequency (one vote per doc)
# - Computes share and TF–IDF to rank "main themes"
# - Returns tidy tables and a simple plotting helper
# ----------------------------------------------------------------------------------

# ---- tiny logger (uses your global .cc_log if present) ----
.tlog <- function(tag, ...) {
  if (exists(".cc_log", mode = "function")) .cc_log(tag, ...) else
    if (isTRUE(getOption("cc.verbose", TRUE))) cat(sprintf("[%s] %s\n", tag, paste0(..., collapse=" ")))
}

# ---- country column detection (uses your cc_detect_country_col if available) ----
.detect_country_col <- function(df, preferred = NULL) {
  if (exists("cc_detect_country_col", mode = "function")) {
    return(cc_detect_country_col(df, preferred))
  }
  # fallback (lightweight)
  cands <- unique(c(
    preferred, "Country_List","Country_Array","Countries","Country",
    "country_list","country_array","countries","country","C1","AU_CO",
    names(df)[grepl("country|countries|C1|AU_CO", names(df), ignore.case = TRUE)]
  ))
  cands <- cands[!is.na(cands) & cands %in% names(df)]
  stopifnot(length(cands) > 0)
  cands[[1]]
}

# ---- keyword parsing + normalization --------------------------------------------
.cc_clean_term <- function(x) {
  x <- tolower(x)
  x <- gsub("[\"'`]", "", x)
  x <- gsub("&", " and ", x)
  x <- gsub("[^[:alnum:][:space:]-]", " ", x)   # keep hyphens
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

.cc_terms_from_row <- function(ak, ik, sep_regex = ";|,|\\|") {
  # Combine Author_Keywords + Indexed_Keywords; accept list or string; NA-safe
  pool <- list()
  if (!is.null(ak)) pool <- c(pool, list(ak))
  if (!is.null(ik)) pool <- c(pool, list(ik))
  if (!length(pool)) return(character(0))
  vec <- unlist(lapply(pool, function(v) {
    if (is.list(v)) unlist(v, use.names = FALSE)
    else if (length(v) > 1) as.character(v)
    else if (is.na(v)) character(0)
    else strsplit(as.character(v), sep_regex)[[1]]
  }), use.names = FALSE)
  vec <- .cc_clean_term(vec)
  vec <- vec[nzchar(vec)]
  unique(vec) # doc-frequency (binary) per document
}

# ---- explode documents to (DocID, Country, Year) rows ---------------------------
.cc_doc_countries <- function(df, country_col, sep_regex = ";|,|\\|") {
  countries <- df[[country_col]]
  rows <- lapply(seq_len(nrow(df)), function(i) {
    ci <- countries[[i]]
    # list or string handling
    if (is.list(countries)) vals <- ci
    else if (is.na(ci)) vals <- character(0)
    else vals <- unlist(strsplit(as.character(ci), sep_regex), use.names = FALSE)
    vals <- trimws(as.character(vals))
    vals <- vals[nzchar(vals)]
    if (!length(vals)) return(NULL)
    data.frame(DocID = i, Country = vals, Year = df$Year[[i]], stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(rows)
}

# ---- main computation -----------------------------------------------------------
themes_country_year <- function(df,
                                country_col = NULL,
                                sep_regex = ";|,|\\|",
                                min_docs_cell = 5,
                                n_top = 10,
                                use_tfidf = TRUE) {
  stopifnot("Year" %in% names(df))
  .tlog("9.1", "START | rows =", nrow(df))

  # pick country column
  cc_col <- .detect_country_col(df, preferred = country_col)
  .tlog("9.1", "country_col =", cc_col)

  # Extract per-document terms
  term_rows <- lapply(seq_len(nrow(df)), function(i) {
    terms <- .cc_terms_from_row(df$Author_Keywords[[i]], df$Indexed_Keywords[[i]], sep_regex)
    if (!length(terms)) return(NULL)
    data.frame(DocID = i, term = terms, stringsAsFactors = FALSE)
  })
  doc_terms <- dplyr::bind_rows(term_rows)
  if (nrow(doc_terms) == 0) {
    warning("[9.1] No keywords found in Author_Keywords/Indexed_Keywords.")
    return(list(groups = dplyr::tibble(), themes = dplyr::tibble(),
                top = dplyr::tibble(), top_wide = dplyr::tibble()))
  }

  # Explode to (DocID, Country, Year)
  doc_cy <- .cc_doc_countries(df, cc_col, sep_regex)
  if (nrow(doc_cy) == 0) {
    warning("[9.1] No countries detected.")
    return(list(groups = dplyr::tibble(), themes = dplyr::tibble(),
                top = dplyr::tibble(), top_wide = dplyr::tibble()))
  }

  # Join: (DocID, term) × (DocID, Country, Year) -> per-doc presence
  dt <- dplyr::inner_join(doc_terms, doc_cy, by = "DocID")

  # Count docs per (Country, Year) for cell filtering
  groups <- dt %>%
    dplyr::distinct(DocID, Country, Year) %>%
    dplyr::count(Country, Year, name = "n_docs")

  groups_kept <- groups %>% dplyr::filter(.data$n_docs >= min_docs_cell)
  .tlog("9.1", "groups_kept =", nrow(groups_kept), "| min_docs_cell =", min_docs_cell)

  # Document frequency of term within (Country, Year)
  themes <- dt %>%
    dplyr::distinct(DocID, Country, Year, term) %>%     # binary within doc
    dplyr::count(Country, Year, term, name = "n_docs_term") %>%
    dplyr::inner_join(groups_kept, by = c("Country","Year")) %>%
    dplyr::mutate(share = .data$n_docs_term / .data$n_docs)

  if (nrow(themes) == 0) {
    warning("[9.1] No (Country, Year) cells met min_docs_cell after join.")
    return(list(groups = groups, themes = dplyr::tibble(),
                top = dplyr::tibble(), top_wide = dplyr::tibble()))
  }

  # Optional TF–IDF across (Country, Year) "documents"
  if (isTRUE(use_tfidf)) {
    # IDF over the set of country-year groups that survived filtering
    G <- dplyr::n_distinct(themes$Country, themes$Year)
    df_term <- themes %>%
      dplyr::group_by(term) %>% dplyr::summarise(df = dplyr::n_distinct(Country, Year), .groups = "drop")
    idf <- dplyr::mutate(df_term, idf = log((G + 1) / (df + 1)) + 1)
    themes <- themes %>% dplyr::left_join(idf[, c("term","idf")], by = "term") %>%
      dplyr::mutate(tfidf = .data$n_docs_term * .data$idf)
  } else {
    themes$tfidf <- themes$share
  }

  # Top themes per (Country, Year)
  top <- themes %>%
    dplyr::group_by(Country, Year) %>%
    dplyr::arrange(dplyr::desc(.data$tfidf), dplyr::desc(.data$share), .by_group = TRUE) %>%
    dplyr::slice_head(n = n_top) %>%
    dplyr::ungroup()

  # Wide, human-friendly: one row per cell with a comma-separated theme list
  top_wide <- top %>%
    dplyr::group_by(Country, Year) %>%
    dplyr::summarise(TopThemes = paste(term, collapse = ", "), .groups = "drop") %>%
    dplyr::arrange(Country, Year)

  .tlog("9.1", "END | themes_rows =", nrow(themes), "| top_rows =", nrow(top))
  list(groups = groups, themes = themes, top = top, top_wide = top_wide)
}

# ---- quick plot helper: heatmap for one country -------------------------------
# Shows the top K terms overall for that country, with intensity by tfidf per year.
themes_country_heatmap <- function(themes_tbl, country, top_k_terms = 15) {
  stopifnot(all(c("Country","Year","term","tfidf") %in% names(themes_tbl)))
  dfc <- themes_tbl %>% dplyr::filter(.data$Country == !!country)

  # Pick the global top-K terms for this country across years
  pick <- dfc %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(score = sum(tfidf, na.rm = TRUE), .groups = "drop") %>%
    dplyr::slice_max(order_by = score, n = top_k_terms, with_ties = FALSE)

  heat <- dfc %>%
    dplyr::semi_join(pick, by = "term") %>%
    dplyr::mutate(term = factor(term, levels = rev(pick$term))) %>%
    dplyr::arrange(Year)

  p <- ggplot2::ggplot(heat, ggplot2::aes(x = Year, y = term, fill = tfidf))
  p <- p + ggplot2::geom_tile()
  p <- p + ggplot2::scale_fill_continuous(name = "TF–IDF")
  p <- p + ggplot2::labs(
    title = paste("Main themes in", country, "by year"),
    x = NULL, y = NULL
  )
  p <- p + ggplot2::theme_minimal(base_size = 10)
  p <- p + ggplot2::theme(
    legend.position = "right",
    panel.grid = ggplot2::element_blank()
  )
  p
}
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr); library(jsonlite)
})

# --- 1) Parse helper (lowercase, trim, split by comma) ----------------------
tw_parse_themes <- function(top_wide) {
  stopifnot(all(c("Country","Year","TopThemes") %in% names(top_wide)))
  top_wide %>%
    mutate(TopThemes = coalesce(as.character(TopThemes), "")) %>%
    mutate(theme = str_split(TopThemes, "\\s*,\\s*")) %>%
    select(Country, Year, theme) %>%
    unnest_longer(theme, values_to = "theme") %>%
    mutate(theme = str_squish(str_to_lower(theme))) %>%
    filter(theme != "")
}

# --- 2) Main: general intersection + specific-by-country-year ----------------
build_general_and_specific <- function(top_wide) {
  themes_long <- tw_parse_themes(top_wide)

  # Theme set per (Country, Year)
  per_cy <- themes_long %>%
    group_by(Country, Year) %>%
    summarise(theme_set = list(sort(unique(theme))), .groups = "drop")

  # (A) GENERAL = intersection across all available country-year sets
  sets <- per_cy$theme_set
  general_terms <- if (length(sets) == 0L) character(0) else Reduce(intersect, sets)
  general_terms <- sort(unique(general_terms))

  # (B) SPECIFIC = themes in a country-year that are NOT in general
  specific_df <- per_cy %>%
    mutate(specific_terms = map(theme_set, ~ setdiff(.x, general_terms))) %>%
    select(Country, Year, specific_terms) %>%
    arrange(Country, Year)

  # Nested list: country -> year -> [specific terms]
  by_country <- split(specific_df, specific_df$Country)
  specific_by_country <- lapply(by_country, function(df) {
    yrs <- as.character(df$Year)
    specs <- df$specific_terms
    # name each year's vector by its year; JSON keys must be strings
    setNames(lapply(specs, unname), yrs)
  })

  list(
    general_terms = unname(general_terms),
    specific_by_country = specific_by_country
  )
}

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr); library(jsonlite)
})

# --- Normalize + (optional) filter -----------------------------------------
themes_normalize <- function(themes, min_share = NULL, min_tfidf = NULL) {
  stopifnot(all(c("Country","Year","term") %in% names(themes)))
  x <- themes %>%
    mutate(
      term = str_squish(str_to_lower(as.character(term)))
    ) %>%
    filter(term != "")
  if (!is.null(min_share))  x <- x %>% filter(!is.na(share),  share  >= min_share)
  if (!is.null(min_tfidf))  x <- x %>% filter(!is.na(tfidf),  tfidf  >= min_tfidf)
  x
}

# --- Build general (intersection across all Country×Year) + specifics -------
build_general_and_specific_from_themes <- function(themes,
                                                   min_share = NULL,
                                                   min_tfidf = NULL,
                                                   top_k_per_cy = NULL) {
  th <- themes_normalize(themes, min_share, min_tfidf)

  # terms per (Country, Year), ranked by tf-idf then share, then alphabetically
  per_cy <- th %>%
    group_by(Country, Year) %>%
    arrange(desc(coalesce(tfidf, 0)), desc(coalesce(share, 0)), term, .by_group = TRUE) %>%
    summarise(terms = list(unique(term)), .groups = "drop")

  if (!is.null(top_k_per_cy) && is.finite(top_k_per_cy)) {
    per_cy <- per_cy %>% mutate(terms = map(terms, ~ head(.x, top_k_per_cy)))
  }

  sets <- per_cy$terms
  general_terms <- if (length(sets) == 0L) character(0) else Reduce(intersect, sets)
  general_terms <- sort(unique(general_terms))

  specific_df <- per_cy %>%
    mutate(specific_terms = map(terms, ~ setdiff(.x, general_terms))) %>%
    select(Country, Year, specific_terms) %>%
    arrange(Country, Year)

  # country -> year -> [specific terms]
  specific_by_country <- specific_df %>%
    group_split(Country) %>%
    set_names(map_chr(., ~ unique(.x$Country))) %>%
    map(~ setNames(lapply(.x$specific_terms, unname), as.character(.x$Year)))

  list(
    general_terms = unname(general_terms),
    specific_by_country = specific_by_country
  )
}

# --- Save JSON --------------------------------------------------------------
save_general_specific_json <- function(payload,
                                       out_path = "M9_general_specific_themes_from_themes.json") {
  write_json(payload, path = out_path, auto_unbox = TRUE, pretty = TRUE)
  message(sprintf("Wrote %s", normalizePath(out_path, mustWork = FALSE)))
}

