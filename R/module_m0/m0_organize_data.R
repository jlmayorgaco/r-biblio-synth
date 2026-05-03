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
  merged <- m0_org_prepare_input(merged)

  keyword_long <- m0_org_keywords_long(merged)
  affiliations <- m0_org_affiliations(merged)
  references_bundle <- m0_org_references(merged)
  country_documents <- m0_org_country_documents(merged, config)

  organized <- list(
    documents = m0_org_documents(merged, keyword_long, affiliations, references_bundle$references),
    authors = m0_org_authors(merged, config),
    author_documents = m0_org_author_documents(merged, config),
    countries = m0_org_countries(country_documents),
    country_documents = country_documents,
    affiliations = affiliations,
    locations = m0_org_locations(affiliations),
    sources = m0_org_sources(merged),
    keywords = m0_org_keywords(keyword_long),
    keywords_long = keyword_long,
    abstracts = m0_org_abstracts(merged),
    citations = m0_org_citations(merged),
    references = references_bundle$references,
    reference_stats = references_bundle$stats,
    reference_journals = m0_org_reference_journals(references_bundle$references),
    reference_authors = m0_org_reference_authors(references_bundle$references),
    annual = m0_org_annual(merged),
    collaborations = m0_org_collaborations(merged, config),
    doc_types = m0_org_doc_types(merged),
    field_coverage = m0_org_field_coverage(merged),
    funding = m0_org_funding(merged),
    field_provenance = m0_build_field_provenance_table(merged),
    field_conflicts = m0_build_field_conflict_table(merged)
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
# Document master table
# ---------------------------------------------------------------------------
m0_org_documents <- function(df, keyword_long = NULL, affiliations = NULL, references = NULL) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(tibble::tibble())
  }

  df <- m0_org_prepare_input(df)
  doc_countries <- m0_org_extract_doc_countries(df)
  clean_abstracts <- if ("AB" %in% names(df)) m0_clean_abstracts(df$AB) else rep(NA_character_, nrow(df))
  abstract_languages <- ifelse(
    !is.na(clean_abstracts) & nzchar(clean_abstracts),
    vapply(clean_abstracts, detect_language_simple, character(1)),
    NA_character_
  )
  author_counts <- vapply(m0_org_split_values(m0_org_get_column(df, "AU")), length, integer(1))

  keyword_summary <- m0_org_document_keyword_summary(keyword_long)
  affiliation_summary <- m0_org_document_affiliation_summary(affiliations)
  reference_summary <- m0_org_document_reference_summary(references)

  documents <- data.frame(
    M0_DOC_ID = df$M0_DOC_ID,
    title = m0_org_get_column(df, "TI"),
    year = suppressWarnings(as.integer(m0_org_get_column(df, "PY"))),
    source = m0_org_get_column(df, "SO"),
    doi = m0_org_get_column(df, "DI"),
    document_type = m0_org_get_column(df, "DT"),
    total_citations = suppressWarnings(as.numeric(m0_org_get_column(df, "TC"))),
    authors = m0_org_get_column(df, "AU"),
    n_authors = author_counts,
    country = vapply(doc_countries, m0_org_collapse_values, character(1)),
    n_countries = vapply(doc_countries, length, integer(1)),
    abstract_raw = m0_org_get_column(df, "AB"),
    abstract_clean = clean_abstracts,
    abstract_language = abstract_languages,
    corresponding_address = m0_org_get_column(df, "RP"),
    orcid = m0_org_get_column(df, "OI"),
    source_db = m0_org_get_column(df, "SOURCE_DB"),
    source_tag = m0_org_get_column(df, "SOURCE_TAG"),
    dedup_method = m0_org_get_column(df, "M0_DEDUP_METHOD"),
    provenance_count = suppressWarnings(as.integer(m0_org_get_column(df, "M0_PROVENANCE_COUNT"))),
    stringsAsFactors = FALSE
  )

  documents <- dplyr::left_join(documents, keyword_summary, by = "M0_DOC_ID")
  documents <- dplyr::left_join(documents, affiliation_summary, by = "M0_DOC_ID")
  documents <- dplyr::left_join(documents, reference_summary, by = "M0_DOC_ID")

  documents$n_keywords[is.na(documents$n_keywords)] <- 0L
  documents$n_references[is.na(documents$n_references)] <- 0L
  documents$n_authors[is.na(documents$n_authors)] <- 0L
  documents$n_countries[is.na(documents$n_countries)] <- 0L
  documents$provenance_count[is.na(documents$provenance_count)] <- 1L
  documents$status <- ifelse(
    !is.na(documents$title) & nzchar(documents$title) & !is.na(documents$year),
    "ready",
    "partial"
  )

  documents
}

# ---------------------------------------------------------------------------
# Author-level tables
# ---------------------------------------------------------------------------
m0_org_author_documents <- function(df, config = biblio_config()) {
  if (!"AU" %in% names(df)) return(tibble::tibble())

  df <- m0_org_prepare_input(df)
  author_base <- df %>%
    dplyr::select(dplyr::any_of(c("M0_DOC_ID", "AU", "PY", "SO", "TI", "DI", "TC"))) %>%
    tidyr::separate_rows(AU, sep = ";") %>%
    dplyr::mutate(AU = trimws(AU)) %>%
    dplyr::filter(!is.na(AU) & nzchar(AU)) %>%
    dplyr::distinct(M0_DOC_ID, AU, .keep_all = TRUE) %>%
    dplyr::group_by(M0_DOC_ID) %>%
    dplyr::mutate(
      author_position = dplyr::row_number(),
      n_authors = dplyr::n(),
      weight = if (identical(config$counting_mode, "fractional")) 1 / dplyr::n() else 1
    ) %>%
    dplyr::ungroup()

  if (nrow(author_base) == 0) return(tibble::tibble())

  doc_countries <- m0_org_extract_doc_countries(df)
  country_map <- data.frame(
    M0_DOC_ID = df$M0_DOC_ID,
    country = vapply(doc_countries, m0_org_collapse_values, character(1)),
    stringsAsFactors = FALSE
  )

  author_base %>%
    dplyr::left_join(country_map, by = "M0_DOC_ID") %>%
    dplyr::mutate(
      author = AU,
      year = suppressWarnings(as.integer(PY)),
      source = SO,
      title = TI,
      doi = DI,
      total_citations = suppressWarnings(as.numeric(TC))
    ) %>%
    dplyr::select(
      M0_DOC_ID, author, author_position, n_authors, weight,
      year, source, title, doi, total_citations, country
    )
}

m0_org_authors <- function(df, config = biblio_config()) {
  author_documents <- m0_org_author_documents(df, config)
  if (nrow(author_documents) == 0) return(tibble::tibble())

  author_documents %>%
    dplyr::group_by(author) %>%
    dplyr::summarise(
      article_count = sum(weight, na.rm = TRUE),
      total_citations = sum(total_citations * weight, na.rm = TRUE),
      first_year = m0_safe_min(year),
      last_year = m0_safe_max(year),
      h_index = m0_calc_h_index(total_citations),
      countries = m0_org_collapse_values(country),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(article_count), desc(total_citations)) %>%
    dplyr::mutate(
      AU = author,
      n_articles = article_count,
      rank = dplyr::row_number()
    )
}

# ---------------------------------------------------------------------------
# Country-level tables
# ---------------------------------------------------------------------------
m0_org_country_documents <- function(df, config = biblio_config()) {
  df <- m0_org_prepare_input(df)
  doc_countries <- m0_org_extract_doc_countries(df)

  rows <- vector("list", length(doc_countries))
  for (i in seq_along(doc_countries)) {
    countries <- doc_countries[[i]]
    if (length(countries) == 0) next

    weight <- 1
    if (identical(config$counting_mode, "fractional")) {
      weight <- 1 / length(countries)
    }

    rows[[i]] <- data.frame(
      M0_DOC_ID = rep(df$M0_DOC_ID[i], length(countries)),
      country = countries,
      year = suppressWarnings(as.integer(df$PY[i])),
      source = df$SO[i] %||% NA_character_,
      title = df$TI[i] %||% NA_character_,
      doi = df$DI[i] %||% NA_character_,
      total_citations = rep(suppressWarnings(as.numeric(df$TC[i])), length(countries)),
      weight = rep(weight, length(countries)),
      weighted_citations = rep(suppressWarnings(as.numeric(df$TC[i])) * weight, length(countries)),
      stringsAsFactors = FALSE
    )
  }

  out <- dplyr::bind_rows(rows)
  if (nrow(out) == 0) return(tibble::tibble())
  dplyr::distinct(out, M0_DOC_ID, country, .keep_all = TRUE)
}

m0_org_countries <- function(country_documents) {
  if (!is.data.frame(country_documents) || nrow(country_documents) == 0) {
    return(tibble::tibble())
  }

  country_documents %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      article_count = sum(weight, na.rm = TRUE),
      total_citations = sum(weighted_citations, na.rm = TRUE),
      first_year = m0_safe_min(year),
      last_year = m0_safe_max(year),
      n_documents = dplyr::n_distinct(M0_DOC_ID),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(article_count), desc(total_citations)) %>%
    dplyr::mutate(
      share = article_count / sum(article_count),
      rank = dplyr::row_number(),
      n_articles = article_count
    )
}

# ---------------------------------------------------------------------------
# Affiliation/location tables
# ---------------------------------------------------------------------------
m0_org_affiliations <- function(df) {
  df <- m0_org_prepare_input(df)
  affiliation_raw <- m0_org_get_affiliation_strings(df)
  if (length(affiliation_raw) == 0) return(tibble::tibble())

  base <- data.frame(
    M0_DOC_ID = df$M0_DOC_ID,
    title = m0_org_get_column(df, "TI"),
    year = suppressWarnings(as.integer(m0_org_get_column(df, "PY"))),
    source = m0_org_get_column(df, "SO"),
    doi = m0_org_get_column(df, "DI"),
    source_db = m0_org_get_column(df, "SOURCE_DB"),
    source_tag = m0_org_get_column(df, "SOURCE_TAG"),
    affiliation_raw = affiliation_raw,
    stringsAsFactors = FALSE
  ) %>%
    tidyr::separate_rows(affiliation_raw, sep = ";") %>%
    dplyr::mutate(affiliation_raw = trimws(affiliation_raw)) %>%
    dplyr::filter(!is.na(affiliation_raw) & nzchar(affiliation_raw))

  if (nrow(base) == 0) return(tibble::tibble())

  parsed <- m0_parse_affiliations(base$affiliation_raw)
  out <- cbind(
    base,
    parsed[, c("institution", "department", "city", "country"), drop = FALSE],
    stringsAsFactors = FALSE
  )

  out$country <- m0_normalize_countries(out$country)
  out$affiliation_id <- seq_len(nrow(out))

  out %>%
    dplyr::select(
      affiliation_id, M0_DOC_ID, title, year, source, doi,
      affiliation_raw, institution, department, city, country,
      source_db, source_tag
    )
}

m0_org_locations <- function(affiliations) {
  if (!is.data.frame(affiliations) || nrow(affiliations) == 0) {
    return(tibble::tibble())
  }

  affiliations %>%
    dplyr::filter(
      (!is.na(country) & nzchar(country)) |
        (!is.na(city) & nzchar(city)) |
        (!is.na(institution) & nzchar(institution))
    ) %>%
    dplyr::group_by(country, city, institution) %>%
    dplyr::summarise(
      article_count = dplyr::n_distinct(M0_DOC_ID),
      total_mentions = dplyr::n(),
      first_year = m0_safe_min(year),
      last_year = m0_safe_max(year),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(article_count), desc(total_mentions)) %>%
    dplyr::mutate(
      rank = dplyr::row_number(),
      location = mapply(m0_org_location_label, city, country, institution, USE.NAMES = FALSE)
    )
}

# ---------------------------------------------------------------------------
# Source/Journal-level table
# ---------------------------------------------------------------------------
m0_org_sources <- function(df) {
  if (!"SO" %in% names(df)) return(tibble::tibble())
  df <- m0_org_prepare_input(df)

  df %>%
    dplyr::filter(!is.na(SO) & nzchar(SO)) %>%
    dplyr::group_by(SO) %>%
    dplyr::summarise(
      article_count = dplyr::n(),
      total_citations = sum(TC, na.rm = TRUE),
      first_year = m0_safe_min(PY),
      last_year = m0_safe_max(PY),
      doi_coverage = mean(!is.na(DI) & nzchar(DI)),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(article_count), desc(total_citations)) %>%
    dplyr::mutate(
      source = SO,
      n_articles = article_count,
      share = article_count / sum(article_count),
      rank = dplyr::row_number()
    )
}

# ---------------------------------------------------------------------------
# Keyword-level tables
# ---------------------------------------------------------------------------
m0_org_keywords_long <- function(df) {
  df <- m0_org_prepare_input(df)

  build_keyword_rows <- function(column_name, keyword_type) {
    if (!column_name %in% names(df)) return(tibble::tibble())

    keyword_df <- df %>%
      dplyr::select(dplyr::any_of(c("M0_DOC_ID", "PY", "SO", "TI", "DI", column_name)))
    names(keyword_df)[names(keyword_df) == column_name] <- "keyword_raw"

    keyword_df %>%
      tidyr::separate_rows(keyword_raw, sep = ";") %>%
      dplyr::mutate(
        keyword_raw = trimws(keyword_raw),
        keyword = tolower(keyword_raw),
        keyword = gsub("\\s+", " ", keyword),
        keyword = trimws(keyword),
        keyword_type = keyword_type,
        year = suppressWarnings(as.integer(PY)),
        source = SO,
        title = TI,
        doi = DI
      ) %>%
      dplyr::filter(!is.na(keyword) & nzchar(keyword)) %>%
      dplyr::distinct(M0_DOC_ID, keyword, keyword_type, .keep_all = TRUE) %>%
      dplyr::select(M0_DOC_ID, keyword, keyword_raw, keyword_type, year, source, title, doi)
  }

  out <- dplyr::bind_rows(
    build_keyword_rows("DE", "author"),
    build_keyword_rows("ID", "index")
  )

  if (nrow(out) == 0) {
    return(data.frame(
      M0_DOC_ID = integer(0),
      keyword = character(0),
      keyword_raw = character(0),
      keyword_type = character(0),
      year = integer(0),
      source = character(0),
      title = character(0),
      doi = character(0),
      stringsAsFactors = FALSE
    ))
  }

  dplyr::arrange(out, M0_DOC_ID, keyword_type, keyword)
}

m0_org_keywords <- function(keyword_long) {
  if (!is.data.frame(keyword_long) || nrow(keyword_long) == 0) {
    return(tibble::tibble())
  }

  keyword_long %>%
    dplyr::group_by(keyword) %>%
    dplyr::summarise(
      frequency = dplyr::n(),
      article_count = dplyr::n_distinct(M0_DOC_ID),
      author_keyword_hits = sum(keyword_type == "author", na.rm = TRUE),
      index_keyword_hits = sum(keyword_type == "index", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(article_count), desc(frequency)) %>%
    dplyr::mutate(
      n_documents = article_count,
      share = article_count / sum(article_count),
      rank = dplyr::row_number()
    )
}

# ---------------------------------------------------------------------------
# Abstract-level table
# ---------------------------------------------------------------------------
m0_org_abstracts <- function(df) {
  if (!"AB" %in% names(df)) return(tibble::tibble())
  df <- m0_org_prepare_input(df)

  clean <- m0_clean_abstracts(df$AB)
  word_counts <- vapply(clean, function(x) {
    if (is.na(x) || !nzchar(x)) return(0L)
    sum(nzchar(strsplit(x, "\\s+")[[1]]))
  }, integer(1))

  data.frame(
    M0_DOC_ID = df$M0_DOC_ID,
    title = m0_org_get_column(df, "TI"),
    year = suppressWarnings(as.integer(m0_org_get_column(df, "PY"))),
    source = m0_org_get_column(df, "SO"),
    doi = m0_org_get_column(df, "DI"),
    abstract_raw = m0_org_get_column(df, "AB"),
    abstract_clean = clean,
    abstract_char_count = nchar(clean),
    abstract_word_count = word_counts,
    abstract_language = ifelse(
      !is.na(clean) & nzchar(clean),
      vapply(clean, detect_language_simple, character(1)),
      NA_character_
    ),
    has_abstract = !is.na(clean) & nzchar(clean),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Citation-level table
# ---------------------------------------------------------------------------
m0_org_citations <- function(df) {
  if (!"TC" %in% names(df)) return(tibble::tibble())
  df <- m0_org_prepare_input(df)

  df %>%
    dplyr::select(dplyr::any_of(c("M0_DOC_ID", "AU", "TI", "PY", "SO", "TC", "DI", "DT"))) %>%
    dplyr::mutate(
      title = TI,
      year = suppressWarnings(as.integer(PY)),
      source = SO,
      total_citations = suppressWarnings(as.numeric(TC)),
      doi = DI,
      document_type = DT
    ) %>%
    dplyr::arrange(desc(total_citations)) %>%
    dplyr::mutate(citation_rank = dplyr::row_number())
}

# ---------------------------------------------------------------------------
# Reference-level tables
# ---------------------------------------------------------------------------
m0_org_references <- function(df) {
  df <- m0_org_prepare_input(df)
  ref_bundle <- m0_extract_references(df, config = biblio_config(verbose = FALSE))
  ref_metrics <- m0_reference_metrics(ref_bundle)

  reference_rows <- list()
  if (identical(ref_bundle$status, "success") && length(ref_bundle$references) > 0) {
    for (i in seq_along(ref_bundle$references)) {
      parsed_refs <- ref_bundle$references[[i]]
      if (length(parsed_refs) == 0) next

      for (j in seq_along(parsed_refs)) {
        ref <- parsed_refs[[j]]
        if (is.null(ref)) next
        reference_rows[[length(reference_rows) + 1L]] <- data.frame(
          M0_DOC_ID = df$M0_DOC_ID[i],
          citing_title = m0_org_get_scalar(df, "TI", i),
          citing_year = suppressWarnings(as.integer(m0_org_get_scalar(df, "PY", i))),
          citing_source = m0_org_get_scalar(df, "SO", i),
          citing_doi = m0_org_get_scalar(df, "DI", i),
          reference_position = j,
          raw_reference = ref$raw %||% NA_character_,
          reference_authors = ref$authors %||% NA_character_,
          reference_year = suppressWarnings(as.integer(ref$year)),
          reference_title = ref$title %||% NA_character_,
          reference_journal = ref$journal %||% NA_character_,
          reference_volume = ref$volume %||% NA_character_,
          reference_issue = ref$issue %||% NA_character_,
          reference_pages = ref$pages %||% NA_character_,
          reference_doi = ref$doi %||% NA_character_,
          reference_pmid = ref$pmid %||% NA_character_,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  references <- dplyr::bind_rows(reference_rows)
  stats <- m0_org_reference_stats_df(ref_bundle, ref_metrics)

  list(references = references, stats = stats)
}

m0_org_reference_stats_df <- function(ref_bundle, ref_metrics) {
  ref_stats <- ref_bundle$reference_stats %||% list()
  year_range <- ref_stats$year_range %||% c(NA, NA)
  age_distribution <- ref_metrics$reference_age_distribution %||% c(NA, NA, NA)

  data.frame(
    n_total = ref_stats$n_total %||% 0L,
    refs_with_year = ref_stats$refs_with_year %||% 0L,
    refs_with_doi = ref_stats$refs_with_doi %||% 0L,
    refs_with_journal = ref_stats$refs_with_journal %||% 0L,
    year_min = suppressWarnings(as.integer(year_range[1])),
    year_max = suppressWarnings(as.integer(year_range[2])),
    median_year = suppressWarnings(as.numeric(ref_stats$median_year %||% NA_real_)),
    median_reference_age = suppressWarnings(as.numeric(ref_metrics$median_reference_age %||% NA_real_)),
    mean_reference_age = suppressWarnings(as.numeric(ref_metrics$mean_reference_age %||% NA_real_)),
    price_index = suppressWarnings(as.numeric(ref_metrics$price_index %||% NA_real_)),
    reference_diversity = suppressWarnings(as.numeric(ref_metrics$reference_diversity %||% NA_real_)),
    reference_half_life = suppressWarnings(as.numeric(ref_metrics$reference_half_life %||% NA_real_)),
    avg_refs_per_document = suppressWarnings(as.numeric(ref_metrics$avg_refs_per_document %||% NA_real_)),
    age_q25 = suppressWarnings(as.numeric(age_distribution[1] %||% NA_real_)),
    age_q50 = suppressWarnings(as.numeric(age_distribution[2] %||% NA_real_)),
    age_q75 = suppressWarnings(as.numeric(age_distribution[3] %||% NA_real_)),
    top_journals = m0_org_collapse_values(ref_stats$top_journals),
    top_authors = m0_org_collapse_values(ref_stats$top_authors),
    stringsAsFactors = FALSE
  )
}

m0_org_reference_journals <- function(references) {
  if (!is.data.frame(references) || nrow(references) == 0) return(tibble::tibble())

  references %>%
    dplyr::filter(!is.na(reference_journal) & nzchar(reference_journal)) %>%
    dplyr::group_by(reference_journal) %>%
    dplyr::summarise(
      cited_count = dplyr::n(),
      citing_documents = dplyr::n_distinct(M0_DOC_ID),
      first_year = m0_safe_min(reference_year),
      last_year = m0_safe_max(reference_year),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(cited_count), desc(citing_documents)) %>%
    dplyr::mutate(rank = dplyr::row_number())
}

m0_org_reference_authors <- function(references) {
  if (!is.data.frame(references) || nrow(references) == 0) return(tibble::tibble())

  references %>%
    dplyr::filter(!is.na(reference_authors) & nzchar(reference_authors)) %>%
    dplyr::group_by(reference_authors) %>%
    dplyr::summarise(
      cited_count = dplyr::n(),
      citing_documents = dplyr::n_distinct(M0_DOC_ID),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(cited_count), desc(citing_documents)) %>%
    dplyr::mutate(rank = dplyr::row_number())
}

# ---------------------------------------------------------------------------
# Annual production table
# ---------------------------------------------------------------------------
m0_org_annual <- function(df) {
  if (!"PY" %in% names(df)) return(tibble::tibble())

  df %>%
    dplyr::filter(!is.na(PY)) %>%
    dplyr::group_by(PY) %>%
    dplyr::summarise(
      article_count = dplyr::n(),
      total_citations = sum(TC, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(PY) %>%
    dplyr::mutate(
      year = PY,
      articles = article_count,
      Year = year,
      Articles = article_count
    )
}

# ---------------------------------------------------------------------------
# Collaboration table
# ---------------------------------------------------------------------------
m0_org_collaborations <- function(df, config = biblio_config()) {
  if (!"AU" %in% names(df)) return(tibble::tibble())
  df <- m0_org_prepare_input(df)

  doc_countries <- m0_org_extract_doc_countries(df)
  author_counts <- vapply(m0_org_split_values(df$AU), length, integer(1))
  country_counts <- vapply(doc_countries, length, integer(1))

  data.frame(
    M0_DOC_ID = df$M0_DOC_ID,
    title = m0_org_get_column(df, "TI"),
    year = suppressWarnings(as.integer(m0_org_get_column(df, "PY"))),
    source = m0_org_get_column(df, "SO"),
    doi = m0_org_get_column(df, "DI"),
    n_authors = author_counts,
    n_countries = country_counts,
    country = vapply(doc_countries, m0_org_collapse_values, character(1)),
    is_single = author_counts == 1,
    collaboration_scope = ifelse(country_counts > 1, "multi_country", "single_country"),
    collaboration_type = ifelse(author_counts > 1, "multi_author", "single_author"),
    has_international_collaboration = country_counts > 1,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Document type table
# ---------------------------------------------------------------------------
m0_org_doc_types <- function(df) {
  if (!"DT" %in% names(df)) return(tibble::tibble())
  df <- m0_org_prepare_input(df)

  df %>%
    dplyr::filter(!is.na(DT) & nzchar(DT)) %>%
    dplyr::group_by(DT) %>%
    dplyr::summarise(
      article_count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(article_count)) %>%
    dplyr::mutate(
      document_type = DT,
      count = article_count,
      share = article_count / sum(article_count),
      rank = dplyr::row_number()
    )
}

# ---------------------------------------------------------------------------
# Coverage and funding tables
# ---------------------------------------------------------------------------
m0_org_field_coverage <- function(df) {
  df <- m0_org_prepare_input(df)
  coverage <- m0_source_coverage(df)
  coverage$coverage %||% tibble::tibble()
}

m0_org_funding <- function(df) {
  funding_cols <- intersect(c("FX", "FU"), names(df))
  if (length(funding_cols) == 0) return(tibble::tibble())
  df <- m0_org_prepare_input(df)

  rows <- list()
  for (i in seq_len(nrow(df))) {
    funding_values <- as.character(unlist(df[i, funding_cols, drop = TRUE]))
    funding_values <- funding_values[!is.na(funding_values) & nzchar(trimws(funding_values))]
    funding_text <- paste(funding_values, collapse = " ")
    funding_text <- trimws(funding_text)
    if (!nzchar(funding_text)) next

    funding_info <- m0_extract_funding(funding_text)

    if (is.data.frame(funding_info$grant_numbers) && nrow(funding_info$grant_numbers) > 0) {
      for (grant in funding_info$grant_numbers$grant) {
        rows[[length(rows) + 1L]] <- data.frame(
          M0_DOC_ID = df$M0_DOC_ID[i],
          title = m0_org_get_scalar(df, "TI", i),
          year = suppressWarnings(as.integer(m0_org_get_scalar(df, "PY", i))),
          source = m0_org_get_scalar(df, "SO", i),
          doi = m0_org_get_scalar(df, "DI", i),
          funding_type = "grant",
          funding_value = grant,
          stringsAsFactors = FALSE
        )
      }
    }

    if (length(funding_info$funders) > 0) {
      for (funder in funding_info$funders) {
        rows[[length(rows) + 1L]] <- data.frame(
          M0_DOC_ID = df$M0_DOC_ID[i],
          title = m0_org_get_scalar(df, "TI", i),
          year = suppressWarnings(as.integer(m0_org_get_scalar(df, "PY", i))),
          source = m0_org_get_scalar(df, "SO", i),
          doi = m0_org_get_scalar(df, "DI", i),
          funding_type = "funder",
          funding_value = funder,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(rows) == 0) {
    return(tibble::tibble(
      M0_DOC_ID = integer(),
      title = character(),
      year = integer(),
      source = character(),
      doi = character(),
      funding_type = character(),
      funding_value = character()
    ))
  }

  dplyr::bind_rows(rows) %>%
    dplyr::distinct(M0_DOC_ID, funding_type, funding_value, .keep_all = TRUE)
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

m0_org_prepare_input <- function(df) {
  if (!is.data.frame(df)) return(data.frame(stringsAsFactors = FALSE))

  out <- data.frame(df, stringsAsFactors = FALSE)
  if (!"M0_DOC_ID" %in% names(out)) {
    out$M0_DOC_ID <- seq_len(nrow(out))
  }

  if ("PY" %in% names(out)) {
    out$PY <- suppressWarnings(as.integer(out$PY))
  }
  if ("TC" %in% names(out)) {
    out$TC <- suppressWarnings(as.numeric(out$TC))
  }

  char_cols <- intersect(
    c("AU", "TI", "SO", "DT", "DI", "AB", "DE", "ID", "CR", "C1", "AU_UN",
      "AU_CO", "AU1_CO", "RP", "OI", "SOURCE_DB", "SOURCE_TAG", "FX", "FU",
      "M0_DEDUP_METHOD", "M0_SOURCE_DBS", "M0_SOURCE_TAGS"),
    names(out)
  )
  for (col in char_cols) {
    out[[col]] <- as.character(out[[col]])
  }

  out
}

m0_org_get_column <- function(df, column_name) {
  if (column_name %in% names(df)) return(df[[column_name]])
  rep(NA_character_, nrow(df))
}

m0_org_get_scalar <- function(df, column_name, index) {
  if (!column_name %in% names(df)) return(NA_character_)
  value <- df[[column_name]][index]
  if (length(value) == 0) return(NA_character_)
  value
}

m0_org_split_values <- function(values, sep = ";") {
  if (is.null(values)) {
    return(list())
  }

  lapply(as.character(values), function(value) {
    if (is.na(value) || !nzchar(trimws(value))) {
      return(character(0))
    }
    pieces <- strsplit(value, sep, fixed = TRUE)[[1]]
    pieces <- trimws(pieces)
    unique(pieces[!is.na(pieces) & nzchar(pieces)])
  })
}

m0_org_collapse_values <- function(values, sep = "; ") {
  if (is.null(values) || length(values) == 0) return(NA_character_)

  values <- as.character(values)
  values <- trimws(values)
  values <- unique(values[!is.na(values) & nzchar(values)])
  if (length(values) == 0) return(NA_character_)
  paste(values, collapse = sep)
}

m0_org_extract_doc_countries <- function(df) {
  countries_from_affiliations <- if ("C1" %in% names(df)) m0_extract_countries(df$C1) else vector("list", nrow(df))
  countries_from_primary <- if ("AU_CO" %in% names(df)) m0_org_split_values(df$AU_CO) else vector("list", nrow(df))
  countries_from_first <- if ("AU1_CO" %in% names(df)) m0_org_split_values(df$AU1_CO) else vector("list", nrow(df))

  out <- vector("list", nrow(df))
  for (i in seq_len(nrow(df))) {
    combined <- c(
      countries_from_primary[[i]] %||% character(0),
      countries_from_first[[i]] %||% character(0),
      countries_from_affiliations[[i]] %||% character(0)
    )
    combined <- m0_normalize_countries(combined)
    combined <- unique(combined[!is.na(combined) & nzchar(combined)])
    out[[i]] <- combined
  }
  out
}

m0_org_get_affiliation_strings <- function(df) {
  c1 <- if ("C1" %in% names(df)) as.character(df$C1) else rep("", nrow(df))
  au_un <- if ("AU_UN" %in% names(df)) as.character(df$AU_UN) else rep("", nrow(df))

  mapply(function(left, right) {
    m0_org_collapse_values(c(left, right))
  }, c1, au_un, USE.NAMES = FALSE)
}

m0_org_document_keyword_summary <- function(keyword_long) {
  if (!is.data.frame(keyword_long) || nrow(keyword_long) == 0) {
    return(data.frame(
      M0_DOC_ID = integer(0),
      keywords = character(0),
      keywords_author = character(0),
      keywords_index = character(0),
      n_keywords = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  keyword_long %>%
    dplyr::group_by(M0_DOC_ID) %>%
    dplyr::summarise(
      keywords = m0_org_collapse_values(keyword),
      keywords_author = m0_org_collapse_values(keyword[keyword_type == "author"]),
      keywords_index = m0_org_collapse_values(keyword[keyword_type == "index"]),
      n_keywords = dplyr::n_distinct(keyword),
      .groups = "drop"
    )
}

m0_org_document_affiliation_summary <- function(affiliations) {
  if (!is.data.frame(affiliations) || nrow(affiliations) == 0) {
    return(data.frame(
      M0_DOC_ID = integer(0),
      affiliations = character(0),
      institutions = character(0),
      cities = character(0),
      stringsAsFactors = FALSE
    ))
  }

  affiliations %>%
    dplyr::group_by(M0_DOC_ID) %>%
    dplyr::summarise(
      affiliations = m0_org_collapse_values(affiliation_raw),
      institutions = m0_org_collapse_values(institution),
      cities = m0_org_collapse_values(city),
      .groups = "drop"
    )
}

m0_org_document_reference_summary <- function(references) {
  if (!is.data.frame(references) || nrow(references) == 0) {
    return(data.frame(
      M0_DOC_ID = integer(0),
      n_references = integer(0),
      references_with_doi = integer(0),
      reference_year_min = integer(0),
      reference_year_max = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  references %>%
    dplyr::group_by(M0_DOC_ID) %>%
    dplyr::summarise(
      n_references = dplyr::n(),
      references_with_doi = sum(!is.na(reference_doi) & nzchar(reference_doi)),
      reference_year_min = m0_safe_min(reference_year),
      reference_year_max = m0_safe_max(reference_year),
      .groups = "drop"
    )
}

m0_org_location_label <- function(city, country, institution) {
  city <- if (!is.na(city) && nzchar(city)) city else NULL
  country <- if (!is.na(country) && nzchar(country)) country else NULL
  institution <- if (!is.na(institution) && nzchar(institution)) institution else NULL

  location_bits <- c(city, country)
  location <- if (length(location_bits) > 0) paste(location_bits, collapse = ", ") else NA_character_

  if (!is.null(institution) && !is.na(location)) {
    return(paste(institution, location, sep = " | "))
  }
  if (!is.null(institution)) {
    return(institution)
  }
  location
}

m0_safe_min <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  min(x)
}

m0_safe_max <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}
