# ============================================================================
# m0_merge_sources.R - Merge multiple bibliometric sources into unified dataset
# ============================================================================

#' Merge multiple bibliometric data frames into one unified dataset
#'
#' Handles deduplication across sources, column harmonization, and adds
#' provenance tracking.
#'
#' @param raw_list Named list of bibliometrix data frames (one per source).
#' @param config Configuration list.
#' @return A single unified data frame.
#' @export
m0_merge_sources <- function(raw_list, config = biblio_config()) {
  config <- merge_biblio_config(config)

  if (length(raw_list) == 0) {
    cli::cli_warn("No sources to merge")
    return(data.frame())
  }

  if (length(raw_list) == 1) {
    return(raw_list[[1]])
  }

  # Step 1: Harmonize columns across all sources
  harmonized <- lapply(raw_list, m0_harmonize_columns)

  # Step 2: Bind all rows
  merged <- dplyr::bind_rows(harmonized)

  # Step 3: Deduplicate based on DOI first, then title+year
  merged <- m0_deduplicate(merged, config)

  # Step 4: Clean and standardize key columns
  merged <- m0_clean_merged(merged)

  if (config$verbose) {
    total_raw <- sum(sapply(raw_list, nrow))
    cli::cli_alert_success("Merged {total_raw} records -> {nrow(merged)} unique records")
  }

  merged
}

#' Harmonize columns across different bibliometric sources
#'
#' Maps variant column names to standard bibliometrix names.
#'
#' @param df A data frame from one source.
#' @return A data frame with harmonized column names.
#' @keywords internal
m0_harmonize_columns <- function(df) {
  # Standard bibliometrix column names
  std_cols <- c(
    AU = "AU", TI = "TI", PY = "PY", SO = "SO", AB = "AB",
    DE = "DE", ID = "ID", C1 = "C1", RP = "RP", TC = "TC",
    SN = "SN", DI = "DI", DT = "DT", LA = "LA", JI = "JI",
    AU_CO = "AU_CO", AU_UN = "AU_UN", AU1_CO = "AU1_CO",
    SR = "SR", NR = "NR", CR = "CR", PU = "PU", PI = "PI",
    PA = "PA", VOL = "VL", ISSUE = "IS", PAGE = "PG",
    SOURCE_DB = "SOURCE_DB", SOURCE_TAG = "SOURCE_TAG"
  )

  # Common aliases from different databases
  alias_map <- c(
    # WoS variants
    "Authors" = "AU", "Author" = "AU", "Article Title" = "TI",
    "Publication Year" = "PY", "Year" = "PY", "Source Title" = "SO",
    "Abstract" = "AB", "Author Keywords" = "DE", "Keywords Plus" = "ID",
    "Affiliations" = "C1", "Reprint Addresses" = "RP",
    "Times Cited" = "TC", "Cited References" = "CR",
    "ISSN" = "SN", "DOI" = "DI", "Document Type" = "DT",
    "Language" = "LA", "Journal" = "SO", "Volume" = "VL",
    "Issue" = "IS", "Pages" = "PG", "Publisher" = "PU",
    # OpenAlex variants
    "title" = "TI", "publication_year" = "PY", "cited_by_count" = "TC",
    "type" = "DT", "language" = "LA",
    # Generic
    "author" = "AU", "authors" = "AU", "year" = "PY",
    "journal" = "SO", "citations" = "TC", "cited_by" = "TC"
  )

  current_names <- names(df)

  # Apply alias mapping
  for (alias in names(alias_map)) {
    if (alias %in% current_names && !(alias_map[[alias]] %in% current_names)) {
      names(df)[names(df) == alias] <- alias_map[[alias]]
    }
  }

  # Ensure provenance columns exist
  if (!"SOURCE_DB" %in% names(df)) df$SOURCE_DB <- NA_character_
  if (!"SOURCE_TAG" %in% names(df)) df$SOURCE_TAG <- NA_character_

  df
}

#' Deduplicate records across sources
#'
#' Uses DOI as primary key, falls back to normalized title + year.
#'
#' @param df Merged data frame.
#' @param config Configuration list.
#' @return Deduplicated data frame.
#' @keywords internal
m0_deduplicate <- function(df, config = biblio_config()) {
  if (nrow(df) <= 1) return(df)

  n_before <- nrow(df)

  # Strategy 1: DOI-based dedup
  if ("DI" %in% names(df)) {
    has_doi <- !is.na(df$DI) & nzchar(trimws(df$DI))
    if (sum(has_doi) > 1) {
      doi_lower <- tolower(trimws(df$DI))
      doi_dup <- duplicated(doi_lower) & has_doi
      df <- df[!doi_dup, ]
    }
  }

  # Strategy 2: Title + Year dedup for records without DOI
  if ("TI" %in% names(df) && "PY" %in% names(df)) {
    title_norm <- m0_normalize_title(df$TI)
    # Handle NA years by using a placeholder
    year_key <- ifelse(is.na(df$PY), "NA_YEAR", as.character(df$PY))
    key <- paste(title_norm, year_key, sep = "|||")
    dup <- duplicated(key) & !is.na(title_norm) & nzchar(title_norm)
    df <- df[!dup, ]
  }

  n_after <- nrow(df)
  if (config$verbose && n_before != n_after) {
    cli::cli_alert_info("Deduplication: {n_before} -> {n_after} ({n_before - n_after} duplicates removed)")
  }

  df
}

#' Normalize title for deduplication
#'
#' @param titles Character vector of titles.
#' @return Normalized titles.
#' @keywords internal
m0_normalize_title <- function(titles) {
  if (!is.character(titles)) titles <- as.character(titles)
  titles <- tolower(trimws(titles))
  # Remove punctuation and extra spaces
  titles <- gsub("[[:punct:]]", " ", titles)
  titles <- gsub("\\s+", " ", titles)
  trimws(titles)
}

#' Clean and standardize merged data
#'
#' @param df Data frame to clean.
#' @return Cleaned data frame.
#' @keywords internal
m0_clean_merged <- function(df) {
  # Ensure PY is numeric
  if ("PY" %in% names(df)) {
    df$PY <- suppressWarnings(as.integer(df$PY))
  }

  # Ensure TC is numeric
  if ("TC" %in% names(df)) {
    df$TC <- suppressWarnings(as.numeric(df$TC))
    df$TC[is.na(df$TC)] <- 0
  }

  # Standardize AU format: semicolon-separated
  if ("AU" %in% names(df)) {
    df$AU <- gsub("\\s*;\\s*", ";", trimws(df$AU))
  }

  # Add row index
  df$M0_DOC_ID <- seq_len(nrow(df))

  df
}
