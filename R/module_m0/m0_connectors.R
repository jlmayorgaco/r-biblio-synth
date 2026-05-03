# ============================================================================
# m0_connectors.R - API connectors and enrichment helpers for M0
# ============================================================================

#' Return available M0 API connectors
#' @export
m0_get_connector_registry <- function() {
  list(
    openalex = list(mode = "source", fetch = m0_fetch_openalex_source),
    crossref = list(mode = "source", fetch = m0_fetch_crossref_source),
    pubmed = list(mode = "source", fetch = m0_fetch_pubmed_source),
    orcid = list(mode = "enrichment", fetch = m0_fetch_orcid_profile),
    ror = list(mode = "enrichment", fetch = m0_fetch_ror_organization)
  )
}

#' Fetch an API source using the connector registry
#' @keywords internal
m0_fetch_api_source <- function(spec, config = biblio_config()) {
  registry <- m0_get_connector_registry()
  db <- tolower(trimws(as.character(spec$db %||% NA_character_)))
  if (!db %in% names(registry)) {
    cli::cli_abort("No API connector registered for db '{db}'.")
  }

  connector <- registry[[db]]
  connector$fetch(spec, config)
}

#' Enrich merged records and affiliations via API providers
#' @export
m0_enrich_merged_records <- function(merged, config = biblio_config(), providers = NULL) {
  config <- merge_biblio_config(config)
  if (is.null(providers) || identical(providers, TRUE)) {
    providers <- config$enrichment_sources
  }
  providers <- unique(tolower(as.character(providers)))
  merged <- m0_org_prepare_input(merged)

  enrichment_log <- list()

  if (any(providers %in% c("crossref", "openalex")) && "DI" %in% names(merged)) {
    lookup_rows <- vector("list", nrow(merged))
    for (i in seq_len(nrow(merged))) {
      doi <- trimws(as.character(merged$DI[i] %||% ""))
      if (!nzchar(doi)) next
      metadata <- m0_lookup_document_metadata(doi, providers, config)
      if (length(metadata) == 0) next
      updated <- m0_apply_document_enrichment(merged[i, , drop = FALSE], metadata)
      merged[i, names(updated)] <- updated[1, names(updated), drop = FALSE]
      lookup_rows[[i]] <- data.frame(
        M0_DOC_ID = merged$M0_DOC_ID[i],
        doi = doi,
        providers = paste(names(metadata), collapse = ";"),
        stringsAsFactors = FALSE
      )
    }
    enrichment_log$document_enrichment <- dplyr::bind_rows(lookup_rows)
  }

  affiliations <- m0_org_affiliations(merged)
  if ("ror" %in% providers && nrow(affiliations) > 0) {
    ror_matches <- m0_match_affiliations_to_ror(affiliations, config)
    enrichment_log$ror_matches <- ror_matches
  }

  if ("orcid" %in% providers && !is.null(config$orcid_access_token) && "OI" %in% names(merged)) {
    orcid_profiles <- m0_collect_orcid_profiles(merged$OI, config)
    enrichment_log$orcid_profiles <- orcid_profiles
  }

  list(merged = merged, enrichment = enrichment_log)
}

m0_is_api_source_spec <- function(spec) {
  mode <- tolower(trimws(as.character(spec$mode %||% "")))
  identical(mode, "api") || (is.null(spec$file) && !is.null(spec$query))
}

m0_fetch_openalex_source <- function(spec, config = biblio_config()) {
  params <- list(
    `per-page` = as.integer(spec$limit %||% config$api_limit),
    search = spec$query %||% NULL,
    filter = spec$filter %||% NULL,
    mailto = config$api_email %||% NULL,
    api_key = config$openalex_api_key %||% NULL
  )
  url <- m0_build_url("https://api.openalex.org/works", params)
  payload <- m0_api_get_json(url)
  records <- payload$results %||% list()
  m0_map_openalex_records(records)
}

m0_fetch_crossref_source <- function(spec, config = biblio_config()) {
  params <- list(
    rows = as.integer(spec$limit %||% config$api_limit),
    mailto = config$api_email %||% NULL
  )

  if (!is.null(spec$doi) && nzchar(trimws(as.character(spec$doi)))) {
    url <- paste0("https://api.crossref.org/works/", utils::URLencode(spec$doi, reserved = TRUE))
  } else {
    params[["query.bibliographic"]] <- spec$query %||% NULL
    if (!is.null(spec$filter)) params$filter <- spec$filter
    url <- m0_build_url("https://api.crossref.org/works", params)
  }

  payload <- m0_api_get_json(url)
  message <- payload$message %||% list()
  records <- if (!is.null(message$items)) message$items else list(message)
  m0_map_crossref_records(records)
}

m0_fetch_pubmed_source <- function(spec, config = biblio_config()) {
  ids <- spec$ids %||% NULL
  if (is.null(ids)) {
    search_params <- list(
      db = "pubmed",
      retmode = "json",
      retmax = as.integer(spec$limit %||% config$api_limit),
      term = spec$query %||% NULL
    )
    search_url <- m0_build_url("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi", search_params)
    search_payload <- m0_api_get_json(search_url)
    ids <- search_payload$esearchresult$idlist %||% character()
  }

  ids <- unique(as.character(unlist(ids)))
  ids <- ids[nzchar(ids)]
  if (length(ids) == 0) return(data.frame())

  summary_params <- list(
    db = "pubmed",
    retmode = "json",
    id = paste(ids, collapse = ",")
  )
  summary_url <- m0_build_url("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi", summary_params)
  summary_payload <- m0_api_get_json(summary_url)
  m0_map_pubmed_records(summary_payload$result %||% list())
}

m0_fetch_orcid_profile <- function(orcid_id, config = biblio_config()) {
  token <- config$orcid_access_token %||% NULL
  if (is.null(token) || !nzchar(token)) {
    cli::cli_warn("ORCID enrichment requires an ORCID access token. Set config$orcid_access_token or ORCID_ACCESS_TOKEN.")
    return(list())
  }

  orcid_id <- gsub("^https?://orcid.org/", "", trimws(as.character(orcid_id)))
  url <- paste0("https://pub.orcid.org/v3.0/", orcid_id, "/record")
  headers <- c(
    "Accept" = "application/json",
    "Authorization" = paste("Bearer", token)
  )
  m0_api_get_json(url, headers = headers)
}

m0_fetch_ror_organization <- function(query, config = biblio_config()) {
  query <- trimws(as.character(query))
  if (!nzchar(query)) return(list())

  base <- "https://api.ror.org/v2/organizations"
  if (grepl("^https?://ror.org/", query, ignore.case = TRUE) || grepl("^[0-9a-z]{9}$", query, ignore.case = TRUE)) {
    clean_id <- gsub("^https?://ror.org/", "", query, ignore.case = TRUE)
    return(m0_api_get_json(paste0(base, "/", clean_id)))
  }

  url <- m0_build_url(base, list(query = query))
  m0_api_get_json(url)
}

m0_lookup_document_metadata <- function(doi, providers, config) {
  out <- list()
  doi <- gsub("^https?://(dx\\.)?doi\\.org/", "", trimws(as.character(doi)), ignore.case = TRUE)
  if (!nzchar(doi)) return(out)

  if ("crossref" %in% providers) {
    crossref <- tryCatch(
      m0_fetch_crossref_source(list(db = "crossref", mode = "api", doi = doi, limit = 1L), config),
      error = function(e) data.frame()
    )
    if (is.data.frame(crossref) && nrow(crossref) > 0) out$crossref <- crossref[1, , drop = FALSE]
  }
  if ("openalex" %in% providers) {
    params <- list(
      filter = paste0("doi:https://doi.org/", doi),
      limit = 1L,
      db = "openalex",
      mode = "api"
    )
    openalex <- tryCatch(m0_fetch_openalex_source(params, config), error = function(e) data.frame())
    if (is.data.frame(openalex) && nrow(openalex) > 0) out$openalex <- openalex[1, , drop = FALSE]
  }

  out
}

m0_apply_document_enrichment <- function(row_df, metadata) {
  out <- data.frame(row_df, stringsAsFactors = FALSE)
  if (length(metadata) == 0) return(out)

  missing_first_fields <- c("AB", "DE", "ID", "SO", "PU", "SN", "C1", "AU_UN", "AU_CO", "OI", "FX", "FU")
  for (provider in names(metadata)) {
    candidate <- metadata[[provider]]
    if (!is.data.frame(candidate) || nrow(candidate) == 0) next
    for (field in intersect(names(candidate), names(out))) {
      if (field %in% missing_first_fields) {
        if (!m0_has_value(out[[field]][1]) && m0_has_value(candidate[[field]][1])) {
          out[[field]][1] <- candidate[[field]][1]
        }
      } else if (field == "TC") {
        tc_vals <- c(
          suppressWarnings(as.numeric(out[[field]][1])),
          suppressWarnings(as.numeric(candidate[[field]][1]))
        )
        tc_vals <- tc_vals[!is.na(tc_vals)]
        out[[field]][1] <- if (length(tc_vals) > 0) max(tc_vals) else NA_real_
      }
    }
  }
  out
}

m0_match_affiliations_to_ror <- function(affiliations, config = biblio_config()) {
  if (!is.data.frame(affiliations) || nrow(affiliations) == 0) return(data.frame())

  institutions <- unique(affiliations$institution[!is.na(affiliations$institution) & nzchar(affiliations$institution)])
  rows <- list()

  for (inst in institutions) {
    payload <- tryCatch(m0_fetch_ror_organization(inst, config), error = function(e) list())
    items <- payload$items %||% payload$items_with_scores %||% list()
    if (length(items) == 0) next

    first_item <- items[[1]]
    item_obj <- first_item$organization %||% first_item
    chosen <- first_item$chosen %||% NA
    rows[[length(rows) + 1L]] <- data.frame(
      institution = inst,
      ror_id = item_obj$id %||% NA_character_,
      ror_name = item_obj$name %||% NA_character_,
      country = (item_obj$locations[[1]]$geonames_details$country_name %||% item_obj$country$country_name %||% NA_character_),
      confidence = suppressWarnings(as.numeric(first_item$score %||% NA_real_)),
      chosen = as.logical(chosen),
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(rows)
}

m0_collect_orcid_profiles <- function(orcid_values, config = biblio_config()) {
  orcid_values <- unique(unlist(strsplit(paste(orcid_values, collapse = ";"), ";", fixed = TRUE)))
  orcid_values <- trimws(orcid_values)
  orcid_values <- orcid_values[!is.na(orcid_values) & nzchar(orcid_values)]

  rows <- list()
  for (orcid_id in orcid_values) {
    payload <- tryCatch(m0_fetch_orcid_profile(orcid_id, config), error = function(e) list())
    if (length(payload) == 0) next

    person <- payload$person %||% list()
    name_obj <- person$name %||% list()
    biography <- person$biography$content %||% NA_character_
    rows[[length(rows) + 1L]] <- data.frame(
      orcid = orcid_id,
      given_names = name_obj$`given-names`$value %||% NA_character_,
      family_name = name_obj$`family-name`$value %||% NA_character_,
      credit_name = name_obj$`credit-name`$value %||% NA_character_,
      biography = biography,
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(rows)
}

m0_map_openalex_records <- function(records) {
  if (length(records) == 0) return(data.frame())

  rows <- lapply(records, function(rec) {
    authorships <- rec$authorships %||% list()
    authors <- vapply(authorships, function(a) a$author$display_name %||% NA_character_, character(1))
    institutions <- unique(unlist(lapply(authorships, function(a) {
      insts <- a$institutions %||% list()
      vapply(insts, function(i) i$display_name %||% NA_character_, character(1))
    })))
    countries <- unique(unlist(lapply(authorships, function(a) {
      if (!is.null(a$countries)) return(unlist(a$countries))
      insts <- a$institutions %||% list()
      vapply(insts, function(i) i$country_code %||% NA_character_, character(1))
    })))
    orcids <- unique(unlist(lapply(authorships, function(a) a$author$orcid %||% NA_character_)))
    concepts <- rec$concepts %||% list()
    keywords <- vapply(concepts, function(x) {
      score <- suppressWarnings(as.numeric(x$score %||% NA_real_))
      if (!is.na(score) && score < 0.3) return(NA_character_)
      x$display_name %||% NA_character_
    }, character(1))

    data.frame(
      TI = rec$display_name %||% NA_character_,
      PY = suppressWarnings(as.integer(rec$publication_year %||% NA_integer_)),
      SO = rec$primary_location$source$display_name %||% NA_character_,
      AB = m0_reconstruct_openalex_abstract(rec$abstract_inverted_index %||% NULL),
      DE = paste(stats::na.omit(unique(keywords)), collapse = ";"),
      ID = paste(stats::na.omit(unique(keywords)), collapse = ";"),
      C1 = paste(stats::na.omit(unique(institutions)), collapse = ";"),
      TC = suppressWarnings(as.numeric(rec$cited_by_count %||% NA_real_)),
      DI = gsub("^https?://doi.org/", "", rec$doi %||% "", ignore.case = TRUE),
      DT = rec$type %||% NA_character_,
      AU = paste(stats::na.omit(authors), collapse = ";"),
      AU_CO = paste(stats::na.omit(countries), collapse = ";"),
      AU_UN = paste(stats::na.omit(unique(institutions)), collapse = ";"),
      OI = paste(stats::na.omit(unique(gsub("^https?://orcid.org/", "", orcids, ignore.case = TRUE))), collapse = ";"),
      SN = paste(stats::na.omit(rec$primary_location$source$issn %||% character()), collapse = ";"),
      PU = rec$primary_location$source$host_organization_name %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })

  m0_standardize_columns(dplyr::bind_rows(rows))
}

m0_map_crossref_records <- function(records) {
  if (length(records) == 0) return(data.frame())

  rows <- lapply(records, function(rec) {
    authors <- rec$author %||% list()
    author_names <- vapply(authors, function(a) {
      family <- a$family %||% ""
      given <- a$given %||% ""
      trimws(paste(family, given))
    }, character(1))
    affiliations <- unique(unlist(lapply(authors, function(a) {
      aff <- a$affiliation %||% list()
      vapply(aff, function(x) x$name %||% NA_character_, character(1))
    })))
    orcids <- unique(unlist(lapply(authors, function(a) {
      orcid <- a$ORCID %||% NA_character_
      gsub("^https?://orcid.org/", "", orcid, ignore.case = TRUE)
    })))
    funders <- rec$funder %||% list()
    funder_names <- vapply(funders, function(f) f$name %||% NA_character_, character(1))
    subjects <- unlist(rec$subject %||% character())
    references <- rec$reference %||% list()
    reference_lines <- vapply(references, function(ref) {
      paste(stats::na.omit(unlist(ref[c("author", "year", "journal-title", "DOI")])), collapse = ", ")
    }, character(1))

    data.frame(
      TI = (rec$title %||% list(NA_character_))[[1]] %||% NA_character_,
      PY = m0_extract_crossref_year(rec),
      SO = (rec$`container-title` %||% list(NA_character_))[[1]] %||% NA_character_,
      AB = rec$abstract %||% NA_character_,
      DE = paste(stats::na.omit(subjects), collapse = ";"),
      ID = paste(stats::na.omit(subjects), collapse = ";"),
      C1 = paste(stats::na.omit(affiliations), collapse = ";"),
      TC = suppressWarnings(as.numeric(rec$`is-referenced-by-count` %||% NA_real_)),
      DI = rec$DOI %||% NA_character_,
      DT = rec$type %||% NA_character_,
      AU = paste(stats::na.omit(author_names), collapse = ";"),
      AU_UN = paste(stats::na.omit(affiliations), collapse = ";"),
      OI = paste(stats::na.omit(orcids), collapse = ";"),
      FU = paste(stats::na.omit(funder_names), collapse = ";"),
      FX = paste(stats::na.omit(funder_names), collapse = ";"),
      CR = paste(stats::na.omit(reference_lines), collapse = ";"),
      SN = paste(stats::na.omit(rec$ISSN %||% character()), collapse = ";"),
      PU = rec$publisher %||% NA_character_,
      VL = rec$volume %||% NA_character_,
      IS = rec$issue %||% NA_character_,
      PG = rec$page %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })

  m0_standardize_columns(dplyr::bind_rows(rows))
}

m0_map_pubmed_records <- function(result_obj) {
  uids <- result_obj$uids %||% character()
  if (length(uids) == 0) return(data.frame())

  rows <- lapply(uids, function(uid) {
    rec <- result_obj[[uid]] %||% list()
    authors <- rec$authors %||% list()
    author_names <- vapply(authors, function(a) a$name %||% NA_character_, character(1))
    article_ids <- rec$articleids %||% list()
    doi <- NA_character_
    if (length(article_ids) > 0) {
      doi_values <- vapply(article_ids, function(a) {
        if (identical(tolower(a$idtype %||% ""), "doi")) a$value %||% NA_character_ else NA_character_
      }, character(1))
      doi <- stats::na.omit(doi_values)[1] %||% NA_character_
    }

    data.frame(
      TI = rec$title %||% NA_character_,
      PY = suppressWarnings(as.integer(gsub("^(\\d{4}).*$", "\\1", rec$pubdate %||% NA_character_))),
      SO = rec$fulljournalname %||% rec$source %||% NA_character_,
      TC = NA_real_,
      DI = doi,
      DT = paste(stats::na.omit(rec$pubtype %||% character()), collapse = ";"),
      AU = paste(stats::na.omit(author_names), collapse = ";"),
      NR = suppressWarnings(as.numeric(rec$references %||% NA_real_)),
      stringsAsFactors = FALSE
    )
  })

  m0_standardize_columns(dplyr::bind_rows(rows))
}

m0_reconstruct_openalex_abstract <- function(index_obj) {
  if (is.null(index_obj) || length(index_obj) == 0) return(NA_character_)
  words <- list()
  for (term in names(index_obj)) {
    positions <- unlist(index_obj[[term]])
    for (pos in positions) {
      words[[as.character(pos)]] <- term
    }
  }
  if (length(words) == 0) return(NA_character_)
  ordered <- words[order(as.integer(names(words)))]
  paste(unlist(ordered), collapse = " ")
}

m0_extract_crossref_year <- function(rec) {
  candidates <- list(
    rec$published$`date-parts`,
    rec$`published-print`$`date-parts`,
    rec$`published-online`$`date-parts`,
    rec$created$`date-parts`
  )
  for (cand in candidates) {
    if (!is.null(cand) && length(cand) > 0) {
      year <- suppressWarnings(as.integer(cand[[1]][1]))
      if (!is.na(year)) return(year)
    }
  }
  NA_integer_
}

m0_build_url <- function(base, params = list()) {
  params <- params[!vapply(params, is.null, logical(1))]
  params <- params[!vapply(params, function(x) length(x) == 0 || all(is.na(x)) || all(!nzchar(as.character(x))), logical(1))]
  if (length(params) == 0) return(base)

  query <- paste(
    sprintf(
      "%s=%s",
      utils::URLencode(names(params), reserved = TRUE),
      vapply(params, function(x) utils::URLencode(as.character(x)[1], reserved = TRUE), character(1))
    ),
    collapse = "&"
  )
  paste0(base, "?", query)
}

m0_api_get_text <- function(url, headers = character(), timeout = 30) {
  old_timeout <- getOption("timeout")
  options(timeout = max(old_timeout, timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)

  con <- base::url(url, open = "r", encoding = "UTF-8", headers = headers)
  on.exit(close(con), add = TRUE)
  paste(readLines(con, warn = FALSE), collapse = "\n")
}

m0_api_get_json <- function(url, headers = character(), timeout = 30) {
  text <- m0_api_get_text(url, headers = headers, timeout = timeout)
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}
