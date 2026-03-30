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
  
  # Extract country information
  # We try AU_CO first, then fall back to C1 if AU_CO is missing or empty
  country_vector <- character(0)
  
  if ("AU_CO" %in% names(input) && !all(is.na(input$AU_CO)) && any(input$AU_CO != "")) {
    # AU_CO is present and has some non-empty values
    country_vector <- input$AU_CO
  } else if ("C1" %in% names(input) && !all(is.na(input$C1)) && any(input$C1 != "")) {
    # Fall back to C1: we need to extract countries from C1 addresses
    # This is a simplified extraction; in reality, C1 parsing is complex.
    # We'll do a basic split by semicolon and then try to catch country patterns.
    # For now, we note that this is a placeholder and we will improve if needed.
    # We'll just return an empty vector and let the user know that C1 parsing is not implemented.
    # However, for the sake of the task, we'll attempt a very basic extraction.
    # We split by semicolon, then look for patterns that might be countries.
    # This is not robust and is only for demonstration.
    c1_list <- strsplit(as.character(input$C1), ";")
    # We'll just take the last element of each split as a potential country (very naive)
    country_vector <- sapply(c1_list, function(x) {
      if (length(x) > 0) {
        # Trim whitespace
        country <- trimws(x[length(x)])
        # If the country is empty or NA, return NA
        if (is.na(country) || country == "") {
          return(NA_character_)
        }
        return(country)
      } else {
        return(NA_character_)
      }
    })
  } else {
    # Neither AU_CO nor C1 is usable
    country_vector <- rep(NA_character_, nrow(input))
  }
  
  # Normalize country names
  country_vector <- m3_normalize_country_names(country_vector)
  
  # Create a data frame with document index and country
  # We allow multiple countries per document? In bibliometrix, AU_CO can have multiple countries separated by semicolon.
  # We need to split AU_CO by semicolon if it contains multiple.
  # We'll do: for each document, split the AU_CO string by semicolon, then normalize each.
  # We'll create a long format: one row per document-country pair.
  
  # Let's redo the extraction to handle multiple countries per document properly.
  
  # We'll start over with a more robust approach.
  
  # Initialize an empty list to hold document-country pairs
  doc_country_pairs <- list()
  
  # We'll iterate over rows (for simplicity; in practice, we can vectorize)
  for (i in seq_len(nrow(input))) {
    # Get the raw country string for this document
    au_co_val <- if ("AU_CO" %in% names(input)) input$AU_CO[i] else NA
    c1_val <- if ("C1" %in% names(input)) input$C1[i] else NA
    
    raw_country <- if (!is.na(au_co_val) && nzchar(as.character(au_co_val))) {
      as.character(au_co_val)
    } else if (!is.na(c1_val) && nzchar(as.character(c1_val))) {
      as.character(c1_val)
    } else {
      NA_character_
    }
    
    if (is.na(raw_country) || !nzchar(raw_country)) {
      # No country information for this document
      doc_country_pairs[[i]] <- data.frame(doc_id = i, country = NA_character_, stringsAsFactors = FALSE)
    } else {
      # Split by semicolon to get individual countries
      countries <- strsplit(raw_country, ";", fixed = TRUE)[[1]]
      # Trim whitespace and normalize
      countries <- trimws(countries)
      countries <- m3_normalize_country_names(countries)
      # Remove empty strings
      countries <- countries[countries != ""]
      if (length(countries) == 0) {
        doc_country_pairs[[i]] <- data.frame(doc_id = i, country = NA_character_, stringsAsFactors = FALSE)
      } else {
        doc_country_pairs[[i]] <- data.frame(
          doc_id = i,
          country = countries,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  # Combine all pairs
  country_doc_level <- dplyr::bind_rows(doc_country_pairs)
  
  # Remove rows where country is NA (if we want to keep them, we can, but for analysis we might drop)
  # We'll keep them for now, but note that they will be dropped in aggregations.
  
  # Prepare annual aggregation if PY is present
  country_annual <- tibble::tibble()
  country_annual_citations <- tibble::tibble()
  if ("PY" %in% names(input)) {
    # We need to expand the annual data to match the country-doc level
    # We'll create an annual data frame by repeating the PY for each country in the document
    annual_data <- data.frame(
      doc_id = rep(seq_len(nrow(input)), times = sapply(doc_country_pairs, nrow)),
      PY = rep(input$PY, times = sapply(doc_country_pairs, nrow))
    )
    # Join with country_doc_level to get country for each row
    annual_data <- dplyr::left_join(annual_data, country_doc_level, by = "doc_id")
    # Now aggregate by country and year for article count
    country_annual <- annual_data %>%
      dplyr::filter(!is.na(country)) %>%
      dplyr::group_by(country, PY) %>%
      dplyr::summarize(
        article_count = dplyr::n(),
        .groups = "drop"
      )
    
    # Also aggregate by country and year for citations if TC is present
    if ("TC" %in% names(input)) {
      # We need to repeat TC for each country in the document
      annual_data_tc <- data.frame(
        doc_id = rep(seq_len(nrow(input)), times = sapply(doc_country_pairs, nrow)),
        PY = rep(input$PY, times = sapply(doc_country_pairs, nrow)),
        TC = rep(input$TC, times = sapply(doc_country_pairs, nrow))
      )
      # Join with country_doc_level to get country for each row
      annual_data_tc <- dplyr::left_join(annual_data_tc, country_doc_level, by = "doc_id")
      # Now aggregate by country and year for total citations
      country_annual_citations <- annual_data_tc %>%
        dplyr::filter(!is.na(country)) %>%
        dplyr::group_by(country, PY) %>%
        dplyr::summarize(
          total_citations = sum(TC, na.rm = TRUE),
          .groups = "drop"
        )
    }
  }
  
  # Prepare country summary (total articles per country)
  country_summary <- country_doc_level %>%
    dplyr::filter(!is.na(country)) %>%
    dplyr::group_by(country) %>%
    dplyr::summarize(
      article_count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(article_count))
  
  # Add citation information to country summary if TC is present
  if ("TC" %in% names(input)) {
    # We need to get the total citations per country.
    # Join country_doc_level (which uses row-index doc_id) with indexed TC values.
    input_indexed <- tibble::tibble(
      doc_id = seq_len(nrow(input)),
      TC     = as.numeric(input$TC)
    )
    doc_country_with_tc <- dplyr::left_join(
      country_doc_level,
      input_indexed,
      by = "doc_id"
    )
    
    # Now aggregate by country to get total citations
    country_citations <- doc_country_with_tc %>%
      dplyr::filter(!is.na(country)) %>%
      dplyr::group_by(country) %>%
      dplyr::summarize(
        total_citations = sum(TC, na.rm = TRUE),
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
  
  # Return the prepared data
  return(list(
    country_doc_level = country_doc_level,
    country_annual = country_annual,
    country_annual_citations = country_annual_citations,
    country_summary = country_summary,
    status = "success"
  ))
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
    "UNITED STATES OF AMERICA" = "UNITED STATES",
    "UK"                      = "UNITED KINGDOM",
    "U.K."                    = "UNITED KINGDOM",
    "ENGLAND"                 = "UNITED KINGDOM",
    "KOREA"                   = "SOUTH KOREA",
    "RUSSIAN FEDERATION"      = "RUSSIA",
    "RUS"                     = "RUSSIA"
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