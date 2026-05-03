# ============================================================================
# m3_prepare_country_data.R - Prepare country data for M3
# ============================================================================

#' Prepare country data for M3 analysis
#'
#' Extracts, normalizes, and aggregates country-level information from bibliographic data.
#'
#' @param input A data frame containing bibliographic data (must have AU_CO or C1).
#' @param config A configuration list (see \code{biblio_config}).
#' @return A list containing prepared country data at different levels of aggregation.
#' @export
prepare_m3_country_data <- function(input, config = biblio_config()) {
  config <- merge_biblio_config(config)
  # Validate input (we do a light check; the main validation is in m3_validate)
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(
      country_doc_level = tibble::tibble(),
      country_annual = tibble::tibble(),
      country_annual_citations = tibble::tibble(),
      country_summary = tibble::tibble(),
      status = "error: input is not a valid data frame or is empty"
    ))
  }
  input <- m3_ensure_bibliometrix_countries(input)
  country_col <- m3_select_country_column(input)
  
  # Initialize an empty list to hold document-country pairs
  doc_country_pairs <- list()
  
  # We'll iterate over rows (for simplicity; in practice, we can vectorize)
  for (i in seq_len(nrow(input))) {
    countries <- m3_extract_document_countries(input, i, country_col)

    if (length(countries) == 0) {
      # No country information for this document
      doc_country_pairs[[i]] <- data.frame(doc_id = i, country = NA_character_, stringsAsFactors = FALSE)
    } else {
      weight <- if (identical(config$counting_mode, "fractional")) 1 / length(countries) else 1
      doc_country_pairs[[i]] <- data.frame(
        doc_id = i,
        country = countries,
        weight = rep(weight, length(countries)),
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combine all pairs
  country_doc_level <- dplyr::bind_rows(doc_country_pairs)
  country_doc_level <- dplyr::left_join(
    country_doc_level,
    tibble::tibble(
      doc_id = seq_len(nrow(input)),
      PY = if ("PY" %in% names(input)) as.integer(input$PY) else NA_integer_,
      year = if ("PY" %in% names(input)) as.integer(input$PY) else NA_integer_,
      TC = if ("TC" %in% names(input)) suppressWarnings(as.numeric(input$TC)) else NA_real_
    ),
    by = "doc_id"
  )
  
  # Remove rows where country is NA (if we want to keep them, we can, but for analysis we might drop)
  # We'll keep them for now, but note that they will be dropped in aggregations.
  
  # Prepare annual aggregation if PY is present
  country_annual <- tibble::tibble()
  country_annual_citations <- tibble::tibble()
  if ("PY" %in% names(input)) {
    annual_data <- country_doc_level[, intersect(c("doc_id", "country", "weight", "PY"), names(country_doc_level)), drop = FALSE]
    # Now aggregate by country and year for article count
    country_annual <- annual_data %>%
      dplyr::filter(!is.na(country)) %>%
      dplyr::group_by(country, PY) %>%
      dplyr::summarize(
        article_count = sum(weight, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(year = PY)
    
    # Also aggregate by country and year for citations if TC is present
    if ("TC" %in% names(input)) {
      annual_data_tc <- country_doc_level[, intersect(c("doc_id", "country", "weight", "PY", "TC"), names(country_doc_level)), drop = FALSE]
      # Now aggregate by country and year for total citations
      country_annual_citations <- annual_data_tc %>%
        dplyr::filter(!is.na(country)) %>%
        dplyr::group_by(country, PY) %>%
        dplyr::summarize(
          total_citations = sum(TC * weight, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(year = PY)
    }
  }
  
  # Prepare country summary (total articles per country)
  country_summary <- country_doc_level %>%
    dplyr::filter(!is.na(country)) %>%
    dplyr::group_by(country) %>%
    dplyr::summarize(
      article_count = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(article_count))
  
  # Add citation information to country summary if TC is present
  if ("TC" %in% names(input)) {
    # Now aggregate by country to get total citations
    country_citations <- country_doc_level %>%
      dplyr::filter(!is.na(country)) %>%
      dplyr::group_by(country) %>%
      dplyr::summarize(
        total_citations = sum(TC * weight, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Join with country_summary
    country_summary <- country_summary %>%
      dplyr::left_join(country_citations, by = "country") %>%
      dplyr::mutate(
        total_citations = ifelse(is.na(total_citations), 0, total_citations)
      )
  } else {
    # If TC is not present, set total_citations to 0
    country_summary <- country_summary %>%
      dplyr::mutate(total_citations = 0)
  }

  total_country_articles <- sum(country_summary$article_count, na.rm = TRUE)
  country_summary <- country_summary %>%
    dplyr::mutate(
      share = if (total_country_articles > 0) article_count / total_country_articles else 0,
      rank = dplyr::row_number()
    )
  
  # Return the prepared data
  return(list(
    country_doc_level = country_doc_level,
    country_annual = country_annual,
    country_annual_citations = country_annual_citations,
    country_summary = country_summary,
    input_data = input,
    year_column = if ("PY" %in% names(input)) "PY" else if ("year" %in% names(input)) "year" else NULL,
    status = "success"
  ))
}

#' Ensure bibliometrix country tags are available when possible
#'
#' @param input A bibliographic data frame.
#' @return The input data frame, optionally enriched with AU_CO/AU1_CO.
#' @keywords internal
m3_ensure_bibliometrix_countries <- function(input) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(input)
  }

  if (m3_has_usable_country_column(input)) {
    return(input)
  }

  if (!"C1" %in% names(input) || !requireNamespace("bibliometrix", quietly = TRUE)) {
    return(input)
  }

  tryCatch(
    {
      enriched <- suppressMessages(suppressWarnings(
        bibliometrix::metaTagExtraction(input, Field = "AU_CO", sep = ";")
      ))
      if (is.data.frame(enriched) && m3_has_usable_country_column(enriched)) {
        return(enriched)
      }
      input
    },
    error = function(e) input
  )
}

m3_has_usable_country_column <- function(input) {
  any(vapply(c("AU_CO", "AU1_CO"), function(col) {
    col %in% names(input) &&
      any(!is.na(input[[col]]) & nzchar(trimws(as.character(input[[col]]))))
  }, logical(1)))
}

m3_select_country_column <- function(input) {
  for (col in c("AU_CO", "AU1_CO", "M0_COUNTRIES", "countries", "COUNTRY", "Country")) {
    if (col %in% names(input) &&
        any(!is.na(input[[col]]) & nzchar(trimws(as.character(input[[col]]))))) {
      return(col)
    }
  }
  NULL
}

m3_extract_document_countries <- function(input, row_index, country_col = NULL) {
  from_country_col <- character(0)
  if (!is.null(country_col) && country_col %in% names(input)) {
    from_country_col <- m3_filter_valid_countries(
      m3_split_country_field(input[[country_col]][row_index])
    )
  }

  if (length(from_country_col) > 0) {
    return(from_country_col)
  }

  if ("C1" %in% names(input)) {
    return(m3_extract_countries_from_affiliations(input$C1[row_index]))
  }

  character(0)
}

m3_split_country_field <- function(value) {
  if (length(value) == 0 || is.na(value) || !nzchar(trimws(as.character(value)))) {
    return(character(0))
  }
  parts <- unlist(strsplit(as.character(value), "\\s*;\\s*|\\s*\\|\\s*", perl = TRUE), use.names = FALSE)
  trimws(parts)
}

m3_extract_countries_from_affiliations <- function(c1_value) {
  if (length(c1_value) == 0 || is.na(c1_value) || !nzchar(trimws(as.character(c1_value)))) {
    return(character(0))
  }

  chunks <- unlist(strsplit(as.character(c1_value), "\\s*;\\s*", perl = TRUE), use.names = FALSE)
  tokens <- unlist(strsplit(chunks, "\\s*,\\s*", perl = TRUE), use.names = FALSE)
  candidates <- c(tokens, m3_find_country_aliases_in_text(chunks))
  m3_filter_valid_countries(candidates)
}

m3_filter_valid_countries <- function(countries) {
  if (length(countries) == 0) {
    return(character(0))
  }

  normalized <- m3_normalize_country_names(countries)
  canonical <- m3_canonical_country_names(normalized)
  canonical <- canonical[!is.na(canonical) & nzchar(canonical)]
  unique(canonical)
}

m3_canonical_country_names <- function(countries) {
  if (!is.character(countries)) {
    countries <- as.character(countries)
  }

  cleaned <- toupper(trimws(countries))
  cleaned <- gsub("^\\[[^]]+\\]\\s*", "", cleaned, perl = TRUE)
  cleaned <- gsub("\\.$", "", cleaned)
  cleaned <- gsub("\\s+", " ", cleaned)

  alias_map <- m3_country_alias_map()
  alias_idx <- cleaned %in% names(alias_map)
  cleaned[alias_idx] <- alias_map[cleaned[alias_idx]]

  canonical <- rep(NA_character_, length(cleaned))
  empty_idx <- is.na(cleaned) | !nzchar(cleaned)
  if (all(empty_idx)) {
    return(canonical)
  }

  if (requireNamespace("countrycode", quietly = TRUE)) {
    cc_name <- suppressWarnings(countrycode::countrycode(
      cleaned,
      origin = "country.name",
      destination = "country.name",
      warn = FALSE
    ))
    cc_iso2 <- suppressWarnings(countrycode::countrycode(
      cleaned,
      origin = "iso2c",
      destination = "country.name",
      warn = FALSE
    ))
    cc_iso3 <- suppressWarnings(countrycode::countrycode(
      cleaned,
      origin = "iso3c",
      destination = "country.name",
      warn = FALSE
    ))
    canonical <- ifelse(!is.na(cc_name), cc_name, ifelse(!is.na(cc_iso2), cc_iso2, cc_iso3))
    canonical <- toupper(canonical)
  }

  reference <- m3_country_reference_set()
  reference_idx <- is.na(canonical) & cleaned %in% reference
  canonical[reference_idx] <- cleaned[reference_idx]

  institution_idx <- m3_is_probable_affiliation(cleaned) & !(cleaned %in% reference)
  canonical[institution_idx] <- NA_character_
  canonical
}

m3_country_alias_map <- function() {
  c(
    "USA" = "UNITED STATES",
    "U S A" = "UNITED STATES",
    "U.S.A" = "UNITED STATES",
    "U.S.A." = "UNITED STATES",
    "US" = "UNITED STATES",
    "U.S" = "UNITED STATES",
    "U.S." = "UNITED STATES",
    "UNITED STATES OF AMERICA" = "UNITED STATES",
    "AMERICA" = "UNITED STATES",
    "UK" = "UNITED KINGDOM",
    "U.K" = "UNITED KINGDOM",
    "U.K." = "UNITED KINGDOM",
    "GB" = "UNITED KINGDOM",
    "GBR" = "UNITED KINGDOM",
    "ENGLAND" = "UNITED KINGDOM",
    "SCOTLAND" = "UNITED KINGDOM",
    "WALES" = "UNITED KINGDOM",
    "NORTHERN IRELAND" = "UNITED KINGDOM",
    "KOREA" = "SOUTH KOREA",
    "REPUBLIC OF KOREA" = "SOUTH KOREA",
    "KOREA SOUTH" = "SOUTH KOREA",
    "SOUTH KOREA" = "SOUTH KOREA",
    "RUSSIAN FEDERATION" = "RUSSIA",
    "SYRIAN ARAB REPUBLIC" = "SYRIA",
    "IRAN ISLAMIC REPUBLIC OF" = "IRAN",
    "IRAN, ISLAMIC REPUBLIC OF" = "IRAN",
    "VIET NAM" = "VIETNAM",
    "UAE" = "UNITED ARAB EMIRATES",
    "UNITED ARAB EMIRATES" = "UNITED ARAB EMIRATES",
    "HONG KONG" = "HONG KONG",
    "TAIWAN" = "TAIWAN",
    "PEOPLES R CHINA" = "CHINA",
    "PEOPLE'S R CHINA" = "CHINA",
    "P R CHINA" = "CHINA",
    "PR CHINA" = "CHINA"
  )
}

m3_country_reference_set <- function() {
  ref <- character(0)
  if (requireNamespace("countrycode", quietly = TRUE)) {
    ref <- toupper(unique(stats::na.omit(countrycode::codelist$country.name.en)))
  }
  ref <- unique(c(
    ref,
    unname(m3_country_alias_map()),
    "HONG KONG",
    "TAIWAN",
    "KOSOVO",
    "PALESTINE"
  ))
  ref[!is.na(ref) & nzchar(ref)]
}

m3_is_probable_affiliation <- function(x) {
  grepl(
    paste0(
      "\\b(",
      paste(
        c(
          "ACADEMY", "CENTER", "CENTRE", "COLLEGE", "COMPANY", "CORPORATION",
          "DEPARTMENT", "DIGITAL", "DIVISION", "ELECTRIC", "ENGINEERING",
          "FACULTY", "GROUP", "HOSPITAL", "IEEE", "INC", "INSTITUTE",
          "LAB", "LABORATORY", "LTD", "POWER", "SCHOOL", "TECHNICAL",
          "TECHNOLOGY", "UNIV", "UNIVERSITY"
        ),
        collapse = "|"
      ),
      ")\\b"
    ),
    x,
    perl = TRUE
  )
}

m3_find_country_aliases_in_text <- function(text) {
  if (length(text) == 0) {
    return(character(0))
  }

  aliases <- names(m3_country_alias_map())
  aliases <- aliases[nchar(aliases) >= 3]
  text_upper <- toupper(text)
  found <- character(0)

  for (alias in aliases) {
    pattern <- paste0("(^|[^A-Z])", gsub("([.])", "\\\\\\1", alias), "([^A-Z]|$)")
    if (any(grepl(pattern, text_upper, perl = TRUE))) {
      found <- c(found, alias)
    }
  }

  found
}

#' Normalize country names
#'
#' Applies a set of rules to standardize country names.
#'
#' @param countries A character vector of country names.
#' @return A character vector of normalized country names.
#' @export
m3_normalize_country_names <- function(countries) {
  if (!is.character(countries)) {
    countries <- as.character(countries)
  }
  
  # Trim whitespace
  countries <- trimws(countries)
  
  # Replace common variants using exact whole-string matching to avoid
  # cascade corruption (e.g., "KOREA" matching inside "SOUTH KOREA").
  norm_map <- c(
    "USA"                     = "UNITED STATES",
    "U.S.A."                  = "UNITED STATES",
    "US"                      = "UNITED STATES",
    "U.S."                    = "UNITED STATES",
    "UNITED STATES OF AMERICA" = "UNITED STATES",
    "UK"                      = "UNITED KINGDOM",
    "U.K."                    = "UNITED KINGDOM",
    "GB"                      = "UNITED KINGDOM",
    "GBR"                     = "UNITED KINGDOM",
    "ENGLAND"                 = "UNITED KINGDOM",
    "KOREA"                   = "SOUTH KOREA",
    "KR"                      = "SOUTH KOREA",
    "KOR"                     = "SOUTH KOREA",
    "RUSSIAN FEDERATION"      = "RUSSIA",
    "RU"                      = "RUSSIA",
    "RUS"                     = "RUSSIA",
    "CN"                      = "CHINA",
    "CHN"                     = "CHINA",
    "JP"                      = "JAPAN",
    "JPN"                     = "JAPAN",
    "FR"                      = "FRANCE",
    "FRA"                     = "FRANCE",
    "DE"                      = "GERMANY",
    "DEU"                     = "GERMANY",
    "GER"                     = "GERMANY",
    "IT"                      = "ITALY",
    "ITA"                     = "ITALY",
    "ES"                      = "SPAIN",
    "ESP"                     = "SPAIN",
    "CA"                      = "CANADA",
    "CAN"                     = "CANADA",
    "AU"                      = "AUSTRALIA",
    "AUS"                     = "AUSTRALIA",
    "NL"                      = "NETHERLANDS",
    "NLD"                     = "NETHERLANDS",
    "IR"                      = "IRAN",
    "IRN"                     = "IRAN",
    "PL"                      = "POLAND",
    "POL"                     = "POLAND",
    "SE"                      = "SWEDEN",
    "SWE"                     = "SWEDEN"
  )

  # Apply replacements only when the trimmed, uppercased string matches exactly.
  upper_countries <- toupper(countries)
  for (i in seq_along(norm_map)) {
    match_idx <- upper_countries == toupper(names(norm_map)[i])
    countries[match_idx] <- norm_map[i]
    upper_countries[match_idx] <- norm_map[i]
  }
  
  # Return the normalized vector
  return(countries)
}
