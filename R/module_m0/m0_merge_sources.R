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
    merged <- m0_harmonize_columns(raw_list[[1]])
    merged <- m0_deduplicate(merged, config)
    merged <- m0_clean_merged(merged)
    return(m0_finalize_provenance_columns(merged))
  }

  # Step 1: Harmonize columns across all sources
  harmonized <- lapply(raw_list, m0_harmonize_columns)

  # Step 2: Bind all rows
  merged <- dplyr::bind_rows(harmonized)

  # Step 3: Deduplicate based on DOI first, then title+year
  merged <- m0_deduplicate(merged, config)

  # Step 4: Clean and standardize key columns
  merged <- m0_clean_merged(merged)
  merged <- m0_finalize_provenance_columns(merged)

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
  config <- merge_biblio_config(config)

  if (!"M0_SOURCE_DBS" %in% names(df)) {
    df$M0_SOURCE_DBS <- toupper(trimws(as.character(df$SOURCE_DB %||% NA_character_)))
  }
  if (!"M0_SOURCE_TAGS" %in% names(df)) {
    df$M0_SOURCE_TAGS <- trimws(as.character(df$SOURCE_TAG %||% NA_character_))
  }
  if (!"M0_PROVENANCE_COUNT" %in% names(df)) {
    df$M0_PROVENANCE_COUNT <- 1L
  }
  if (!"M0_DEDUP_METHOD" %in% names(df)) {
    df$M0_DEDUP_METHOD <- NA_character_
  }

  dedup_methods <- config$dedup_method %||% c("doi_exact", "title_year_normalized")
  if ("DI" %in% names(df)) {
    df$M0_DOI_KEY <- m0_normalize_doi(df$DI)
  } else if (!"M0_DOI_KEY" %in% names(df)) {
    df$M0_DOI_KEY <- NA_character_
  }

  if ("doi_exact" %in% dedup_methods && "DI" %in% names(df)) {
    df <- m0_collapse_duplicate_groups(df, df$M0_DOI_KEY, "doi_exact", config)
  }

  if ("title_year_normalized" %in% dedup_methods && all(c("TI", "PY") %in% names(df))) {
    no_doi <- is.na(df$M0_DOI_KEY) | !nzchar(trimws(as.character(df$M0_DOI_KEY)))
    title_norm <- m0_normalize_title(df$TI)
    df$M0_TITLE_KEY <- title_norm
    year_key <- ifelse(is.na(df$PY), "NA_YEAR", as.character(df$PY))
    title_key <- ifelse(no_doi & !is.na(title_norm) & nzchar(title_norm),
                        paste(title_norm, year_key, sep = "|||"),
                        NA_character_)
    df <- m0_collapse_duplicate_groups(df, title_key, "title_year_normalized", config)
  }

  if ("title_year_fuzzy" %in% dedup_methods && all(c("TI", "PY") %in% names(df))) {
    df <- m0_deduplicate_fuzzy_titles(df, config)
  }

  n_after <- nrow(df)
  if (config$verbose && n_before != n_after) {
    cli::cli_alert_info("Deduplication: {n_before} -> {n_after} ({n_before - n_after} duplicates removed)")
  }

  df
}

#' Collapse duplicate groups while preserving provenance
#' @keywords internal
m0_collapse_duplicate_groups <- function(df, group_key, method, config = biblio_config()) {
  if (length(group_key) != nrow(df)) {
    stop("group_key must have the same length as df")
  }

  valid <- !is.na(group_key) & nzchar(group_key)
  if (sum(valid) <= 1) {
    return(df)
  }

  grouped <- split(which(valid), group_key[valid], drop = TRUE)
  duplicate_groups <- Filter(function(idx) length(idx) > 1, grouped)
  if (length(duplicate_groups) == 0) {
    return(df)
  }

  for (idx in duplicate_groups) {
    keep <- idx[[1]]
    resolved <- m0_resolve_duplicate_group(df[idx, , drop = FALSE], config, method)
    missing_cols <- setdiff(names(resolved), names(df))
    for (col in missing_cols) {
      df[[col]] <- NA
    }
    df[keep, names(resolved)] <- resolved[1, names(resolved), drop = FALSE]
  }

  drop_idx <- unlist(lapply(duplicate_groups, function(idx) idx[-1]), use.names = FALSE)
  if (length(drop_idx) == 0) {
    return(df)
  }
  df[-drop_idx, , drop = FALSE]
}

#' Deduplicate records by fuzzy title matching within year
#' @keywords internal
m0_deduplicate_fuzzy_titles <- function(df, config = biblio_config()) {
  threshold <- config$dedup_threshold %||% 0.9
  doi_key <- if ("M0_DOI_KEY" %in% names(df)) df$M0_DOI_KEY else m0_normalize_doi(df$DI %||% NA_character_)
  no_doi <- is.na(doi_key) | !nzchar(trimws(as.character(doi_key)))
  valid_title <- !is.na(df$TI) & nzchar(trimws(as.character(df$TI)))
  candidate_idx <- which(no_doi & valid_title)

  if (length(candidate_idx) < 2) {
    return(df)
  }

  years <- as.character(df$PY[candidate_idx])
  years[is.na(years)] <- "NA_YEAR"
  fuzzy_groups <- rep(NA_character_, nrow(df))

  for (year_key in unique(years)) {
    year_idx <- candidate_idx[years == year_key]
    if (length(year_idx) < 2) next

    sim <- m0_fuzzy_title_match(df$TI[year_idx], threshold = threshold)
    assigned <- rep(FALSE, length(year_idx))

    for (i in seq_along(year_idx)) {
      if (assigned[i]) next
      matches <- which(sim[i, ] >= threshold)
      if (length(matches) > 1) {
        group_members <- year_idx[matches]
        fuzzy_key <- paste0("fuzzy::", year_key, "::", min(group_members))
        fuzzy_groups[group_members] <- fuzzy_key
        assigned[matches] <- TRUE
      }
    }
  }

  m0_collapse_duplicate_groups(df, fuzzy_groups, "title_year_fuzzy", config)
}

#' Normalize title for deduplication
#'
#' @param titles Character vector of titles.
#' @return Normalized titles.
#' @keywords internal
m0_normalize_title <- function(titles) {
  if (!is.character(titles)) titles <- as.character(titles)
  titles[is.na(titles)] <- NA_character_
  titles <- enc2utf8(titles)
  titles <- iconv(titles, from = "", to = "ASCII//TRANSLIT", sub = "")
  titles <- tolower(trimws(titles))
  titles <- gsub("&amp;", " and ", titles, fixed = TRUE)
  titles <- gsub("[[:punct:]]", " ", titles)
  titles <- gsub("\\s+", " ", titles)
  trimws(titles)
}

#' Normalize DOI strings for cross-source deduplication
#'
#' @param doi Character vector of DOI strings.
#' @return Normalized DOI keys.
#' @keywords internal
m0_normalize_doi <- function(doi) {
  if (!is.character(doi)) doi <- as.character(doi)
  doi[is.na(doi)] <- NA_character_
  doi <- enc2utf8(doi)
  doi <- tolower(trimws(doi))
  doi <- gsub("^https?://(dx\\.)?doi\\.org/", "", doi)
  doi <- gsub("^doi\\s*[:=]\\s*", "", doi)
  doi <- gsub("^https?://", "", doi)
  doi <- gsub("\\s+", "", doi)
  doi <- gsub("[\\.;,]+$", "", doi)
  doi[is.na(doi) | !nzchar(doi)] <- NA_character_
  doi
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

  # Keep stable deduplication keys for downstream audit and provenance.
  if ("DI" %in% names(df)) {
    doi_key <- m0_normalize_doi(df$DI)
    df$M0_DOI_KEY <- doi_key
    df$DI <- ifelse(!is.na(doi_key) & nzchar(doi_key), doi_key, as.character(df$DI))
  } else if (!"M0_DOI_KEY" %in% names(df)) {
    df$M0_DOI_KEY <- NA_character_
  }

  if ("TI" %in% names(df)) {
    df$M0_TITLE_KEY <- m0_normalize_title(df$TI)
  } else if (!"M0_TITLE_KEY" %in% names(df)) {
    df$M0_TITLE_KEY <- NA_character_
  }

  # Standardize AU format: semicolon-separated
  if ("AU" %in% names(df)) {
    df$AU <- gsub("\\s*;\\s*", ";", trimws(df$AU))
  }

  # Add row index
  df$M0_DOC_ID <- seq_len(nrow(df))

  df
}
