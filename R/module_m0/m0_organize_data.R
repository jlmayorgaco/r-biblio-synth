# ============================================================================
# m0_organize_data.R - Organize data for downstream modules M1-M4
# ============================================================================

#' Organize merged data into pre-processed tables for M1-M4
#'
#' Creates auxiliary data frames that downstream modules can use directly
#' without re-parsing raw bibliometrix data.
#'
#' @param merged A unified bibliometrix data frame.
#' @param config Configuration list.
#' @return A named list of organized data frames.
#' @export
m0_organize_for_modules <- function(merged, config = biblio_config()) {
  config <- merge_biblio_config(config)

  organized <- list(
    authors      = m0_org_authors(merged),
    countries    = m0_org_countries(merged),
    sources      = m0_org_sources(merged),
    keywords     = m0_org_keywords(merged),
    citations    = m0_org_citations(merged),
    annual       = m0_org_annual(merged),
    collaborations = m0_org_collaborations(merged),
    doc_types    = m0_org_doc_types(merged)
  )

  if (config$verbose) {
    for (nm in names(organized)) {
      n <- if (is.data.frame(organized[[nm]])) nrow(organized[[nm]]) else length(organized[[nm]])
      cli::cli_alert_info("Organized {nm}: {n} rows")
    }
  }

  organized
}

# ---------------------------------------------------------------------------
# Author-level table
# ---------------------------------------------------------------------------
m0_org_authors <- function(df) {
  if (!"AU" %in% names(df)) return(tibble::tibble())

  # Expand semicolon-separated authors
  au_expanded <- tidyr::separate_rows(df, AU, sep = ";")
  au_expanded$AU <- trimws(au_expanded$AU)
  au_expanded <- au_expanded[nzchar(au_expanded$AU) & !is.na(au_expanded$AU), ]

  if (nrow(au_expanded) == 0) return(tibble::tibble())

  # Per-author aggregation
  au_summary <- au_expanded %>%
    dplyr::group_by(AU) %>%
    dplyr::summarise(
      n_articles     = dplyr::n(),
      total_citations = sum(TC, na.rm = TRUE),
      first_year     = min(PY, na.rm = TRUE),
      last_year      = max(PY, na.rm = TRUE),
      h_index        = m0_calc_h_index(TC),
      .groups        = "drop"
    ) %>%
    dplyr::arrange(desc(n_articles))

  # Add affiliation and country info if available
  if ("AU_UN" %in% names(df) && "AU_CO" %in% names(df)) {
    au_affil <- au_expanded %>%
      dplyr::group_by(AU) %>%
      dplyr::summarise(
        affiliations = paste(unique(na.omit(AU_UN)), collapse = "; "),
        countries    = paste(unique(na.omit(AU_CO)), collapse = "; "),
        .groups      = "drop"
      )
    au_summary <- dplyr::left_join(au_summary, au_affil, by = "AU")
  }

  au_summary
}

# ---------------------------------------------------------------------------
# Country-level table
# ---------------------------------------------------------------------------
m0_org_countries <- function(df) {
  co_col <- NULL
  if ("AU_CO" %in% names(df)) co_col <- "AU_CO"
  else if ("AU1_CO" %in% names(df)) co_col <- "AU1_CO"

  if (is.null(co_col)) return(tibble::tibble())

  co_expanded <- tidyr::separate_rows(df, !!rlang::sym(co_col), sep = ";")
  co_expanded[[co_col]] <- trimws(co_expanded[[co_col]])
  co_expanded <- co_expanded[nzchar(co_expanded[[co_col]]) & !is.na(co_expanded[[co_col]]), ]

  if (nrow(co_expanded) == 0) return(tibble::tibble())

  # Normalize country names
  co_expanded$country_norm <- m0_normalize_country_name(co_expanded[[co_col]])

  # Aggregate
  co_summary <- co_expanded %>%
    dplyr::group_by(country_norm) %>%
    dplyr::summarise(
      n_articles      = dplyr::n(),
      total_citations = sum(TC, na.rm = TRUE),
      .groups         = "drop"
    ) %>%
    dplyr::rename(country = country_norm) %>%
    dplyr::arrange(desc(n_articles))

  co_summary
}

# ---------------------------------------------------------------------------
# Source/Journal-level table
# ---------------------------------------------------------------------------
m0_org_sources <- function(df) {
  if (!"SO" %in% names(df)) return(tibble::tibble())

  so_summary <- df %>%
    dplyr::filter(!is.na(SO) & nzchar(SO)) %>%
    dplyr::group_by(SO) %>%
    dplyr::summarise(
      n_articles     = dplyr::n(),
      total_citations = sum(TC, na.rm = TRUE),
      first_year     = min(PY, na.rm = TRUE),
      last_year      = max(PY, na.rm = TRUE),
      .groups        = "drop"
    ) %>%
    dplyr::arrange(desc(n_articles))

  so_summary
}

# ---------------------------------------------------------------------------
# Keyword-level table
# ---------------------------------------------------------------------------
m0_org_keywords <- function(df) {
  de_col <- NULL
  if ("DE" %in% names(df)) de_col <- "DE"
  else if ("ID" %in% names(df)) de_col <- "ID"

  if (is.null(de_col)) return(tibble::tibble())

  kw_expanded <- tidyr::separate_rows(df, !!rlang::sym(de_col), sep = ";")
  kw_expanded[[de_col]] <- trimws(kw_expanded[[de_col]])
  kw_expanded <- kw_expanded[nzchar(kw_expanded[[de_col]]) & !is.na(kw_expanded[[de_col]]), ]

  if (nrow(kw_expanded) == 0) return(tibble::tibble())

  kw_summary <- kw_expanded %>%
    dplyr::group_by(!!rlang::sym(de_col)) %>%
    dplyr::summarise(
      frequency = dplyr::n(),
      .groups   = "drop"
    ) %>%
    dplyr::rename(keyword = !!rlang::sym(de_col)) %>%
    dplyr::arrange(desc(frequency))

  kw_summary
}

# ---------------------------------------------------------------------------
# Citation-level table
# ---------------------------------------------------------------------------
m0_org_citations <- function(df) {
  if (!"TC" %in% names(df)) return(tibble::tibble())

  cit_df <- df %>%
    dplyr::select(dplyr::any_of(c("M0_DOC_ID", "AU", "TI", "PY", "SO", "TC", "DI"))) %>%
    dplyr::arrange(desc(TC))

  cit_df
}

# ---------------------------------------------------------------------------
# Annual production table
# ---------------------------------------------------------------------------
m0_org_annual <- function(df) {
  if (!"PY" %in% names(df)) return(tibble::tibble())

  annual <- df %>%
    dplyr::filter(!is.na(PY)) %>%
    dplyr::group_by(PY) %>%
    dplyr::summarise(
      articles = dplyr::n(),
      total_citations = sum(TC, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(PY)

  annual
}

# ---------------------------------------------------------------------------
# Collaboration table
# ---------------------------------------------------------------------------
m0_org_collaborations <- function(df) {
  if (!"AU" %in% names(df)) return(tibble::tibble())

  # Count authors per document
  n_authors <- sapply(strsplit(df$AU, ";"), function(x) {
    sum(nzchar(trimws(x)) & !is.na(trimws(x)))
  })

  collab_df <- data.frame(
    M0_DOC_ID = df$M0_DOC_ID,
    n_authors = n_authors,
    is_single = n_authors == 1,
    stringsAsFactors = FALSE
  )

  collab_df
}

# ---------------------------------------------------------------------------
# Document type table
# ---------------------------------------------------------------------------
m0_org_doc_types <- function(df) {
  if (!"DT" %in% names(df)) return(tibble::tibble())

  dt_summary <- df %>%
    dplyr::filter(!is.na(DT) & nzchar(DT)) %>%
    dplyr::group_by(DT) %>%
    dplyr::summarise(
      count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(count))

  dt_summary
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

#' Calculate h-index from citation vector
#' @keywords internal
m0_calc_h_index <- function(citations) {
  citations <- sort(as.numeric(citations), decreasing = TRUE)
  citations[is.na(citations)] <- 0
  sum(citations >= seq_along(citations))
}

#' Normalize country name
#' @keywords internal
m0_normalize_country_name <- function(x) {
  x <- toupper(trimws(as.character(x)))
  # Common normalizations
  x <- gsub("^USA$", "UNITED STATES", x)
  x <- gsub("^U\\.S\\.A\\.$", "UNITED STATES", x)
  x <- gsub("^UNITED STATES OF AMERICA$", "UNITED STATES", x)
  x <- gsub("^UK$", "UNITED KINGDOM", x)
  x <- gsub("^U\\.K\\.$", "UNITED KINGDOM", x)
  x <- gsub("^ENGLAND$", "UNITED KINGDOM", x)
  x <- gsub("^KOREA$", "SOUTH KOREA", x)
  x <- gsub("^RUSSIAN FEDERATION$", "RUSSIA", x)
  x
}
