# ============================================================================
# m4_extract_institutions.R - Extract and Parse Institutional Affiliations
# ============================================================================

#' Extract institutions from bibliographic data
#'
#' Parses affiliation strings (typically from C1 field) to extract
#' institution names, departments, cities, and countries.
#'
#' @param input Bibliographic data frame
#' @param config Configuration list
#' @return List with parsed institutional data
#' @export
m4_extract_institutions <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(status = "error: invalid input"))
  }
  
  # Find affiliation column
  affil_col <- if ("C1" %in% names(input)) "C1" else NULL
  if (is.null(affil_col)) {
    return(list(status = "error: missing affiliation column (C1)"))
  }
  
  log_message("INFO", "Extracting institutions from {n} records", n = nrow(input))
  
  # Parse each affiliation
  parsed_list <- vector("list", nrow(input))
  
  for (i in seq_len(nrow(input))) {
    affil_str <- input[[affil_col]][i]
    
    if (is.na(affil_str) || affil_str == "") {
      parsed_list[[i]] <- NULL
      next
    }
    
    # Parse affiliation
    parsed <- parse_affiliation_string(affil_str)
    parsed$doc_id <- i
    parsed_list[[i]] <- parsed
    
    # Progress logging
    if (i %% 100 == 0) {
      log_progress(i, nrow(input), "Extracting institutions")
    }
  }
  
  # Combine into data frame
  all_institutions <- do.call(rbind, lapply(parsed_list, function(x) {
    if (is.null(x)) return(NULL)
    as.data.frame(x, stringsAsFactors = FALSE)
  }))
  
  if (is.null(all_institutions) || nrow(all_institutions) == 0) {
    return(list(status = "error: no institutions found"))
  }
  
  # Normalize and deduplicate institutions
  all_institutions <- normalize_institutions(all_institutions)
  
  # Create institution lookup
  institution_lookup <- create_institution_lookup(all_institutions)
  
  log_message("INFO", "Extracted {n} unique institutions", 
              n = length(unique(all_institutions$institution_clean)))
  
  list(
    status = "success",
    institutions = all_institutions,
    lookup = institution_lookup,
    n_records = nrow(input),
    n_institutions = length(unique(all_institutions$institution_clean))
  )
}

#' Parse a single affiliation string
#' @keywords internal
parse_affiliation_string <- function(affil_str) {
  # Split by semicolon (multiple affiliations per author)
  affil_parts <- strsplit(affil_str, ";")[[1]]
  affil_parts <- trimws(affil_parts)
  affil_parts <- affil_parts[affil_parts != ""]
  
  results <- list()
  
  for (part in affil_parts) {
    # Extract components
    institution <- extract_institution_name(part)
    department <- extract_department(part)
    city <- extract_city(part)
    country <- extract_country(part)
    
    results[[length(results) + 1]] <- list(
      original = part,
      institution = institution,
      institution_clean = clean_institution_name(institution),
      department = department,
      city = city,
      country = country,
      sector = classify_sector(institution)
    )
  }
  
  # Return as data frame
  do.call(rbind, lapply(results, as.data.frame, stringsAsFactors = FALSE))
}

#' Extract institution name from affiliation
#' @keywords internal
extract_institution_name <- function(affil_part) {
  # Remove email addresses
  affil_part <- gsub("\\S+@\\S+", "", affil_part)
  
  # Common patterns for institution names
  patterns <- c(
    "^([^,]+)",  # Take everything before first comma
    "(University|College|Institute|School|Academy|Center|Centre|Laboratory|Lab|Hospital)[^,]*",
    "(Corp\\.|Inc\\.|Ltd\\.|LLC|GmbH|SA|SAS|BV|NV)[^,]*",
    "(Google|Microsoft|IBM|Amazon|Facebook|Apple|Samsung)[^,]*"
  )
  
  for (pattern in patterns) {
    match <- regmatches(affil_part, regexpr(pattern, affil_part, ignore.case = TRUE))
    if (length(match) > 0 && nchar(match[1]) > 3) {
      return(trimws(match[1]))
    }
  }
  
  # Fallback: first 50 chars
  substr(affil_part, 1, min(50, nchar(affil_part)))
}

#' Extract department from affiliation
#' @keywords internal
extract_department <- function(affil_part) {
  dept_patterns <- c(
    "Dept\\.?( of|\\.\\s+|artment\\s+of)?\\s+([^,]+)",
    "Department\\s+of\\s+([^,]+)",
    "School\\s+of\\s+([^,]+)",
    "Faculty\\s+of\\s+([^,]+)",
    "Division\\s+of\\s+([^,]+)"
  )
  
  for (pattern in dept_patterns) {
    match <- regmatches(affil_part, regexpr(pattern, affil_part, ignore.case = TRUE))
    if (length(match) > 0) {
      # Extract capture group
      dept <- sub(pattern, "\\1", match[1], ignore.case = TRUE)
      if (nchar(dept) > 0) return(trimws(dept))
    }
  }
  
  NA_character_
}

#' Extract city from affiliation
#' @keywords internal
extract_city <- function(affil_part) {
  # Look for city before country or at end
  # Common pattern: "... City, Country" or "... City Country"
  
  # Remove email and institution name
  cleaned <- gsub("\\S+@\\S+", "", affil_part)
  
  # Try to find city (usually before country)
  city_patterns <- c(
    ",\\s*([^,]+),\\s*(USA|UK|China|Japan|Germany|France|Italy|Spain|Canada|Australia|Brazil|India)",
    ",\\s*([^,]+)\\s+(USA|United States|UK|United Kingdom|China|PRC)",
    "([A-Z][a-z]+(?:\\s[A-Z][a-z]+)?)\\s*\\d{5}"  # City before ZIP code
  )
  
  for (pattern in city_patterns) {
    match <- regmatches(cleaned, regexpr(pattern, cleaned))
    if (length(match) > 0) {
      city <- sub(pattern, "\\1", match[1])
      if (nchar(city) > 2) return(trimws(city))
    }
  }
  
  NA_character_
}

#' Extract country from affiliation
#' @keywords internal
extract_country <- function(affil_part) {
  affil_upper <- toupper(affil_part)
  
  # Common country patterns
  country_patterns <- list(
    USA = c("USA", "U.S.A.", "UNITED STATES", "AMERICA"),
    UK = c("UK", "U.K.", "UNITED KINGDOM", "ENGLAND", "SCOTLAND", "WALES"),
    China = c("CHINA", "PRC", "PEOPLE'S REPUBLIC OF CHINA"),
    Japan = c("JAPAN", "JPN"),
    Germany = c("GERMANY", "DEUTSCHLAND"),
    France = c("FRANCE"),
    Italy = c("ITALY", "ITALIA"),
    Canada = c("CANADA"),
    Australia = c("AUSTRALIA"),
    Brazil = c("BRAZIL", "BRASIL"),
    India = c("INDIA"),
    Spain = c("SPAIN", "ESPAÑA"),
    Netherlands = c("NETHERLANDS", "HOLLAND"),
    Switzerland = c("SWITZERLAND", "SCHWEIZ"),
    South_Korea = c("SOUTH KOREA", "KOREA", "REPUBLIC OF KOREA"),
    Sweden = c("SWEDEN", "SVERIGE"),
    Russia = c("RUSSIA", "RUSSIAN FEDERATION"),
    Belgium = c("BELGIUM", "BELGIQUE"),
    Austria = c("AUSTRIA", "ÖSTERREICH"),
    Denmark = c("DENMARK", "DANMARK"),
    Finland = c("FINLAND", "SUOMI"),
    Norway = c("NORWAY", "NORGE"),
    Poland = c("POLAND", "POLSKA"),
    Portugal = c("PORTUGAL"),
    Greece = c("GREECE", "HELLAS"),
    Turkey = c("TURKEY", "TÜRKIYE"),
    Israel = c("ISRAEL"),
    Singapore = c("SINGAPORE"),
    Taiwan = c("TAIWAN"),
    Hong_Kong = c("HONG KONG"),
    Mexico = c("MEXICO", "MÉXICO"),
    Argentina = c("ARGENTINA"),
    Chile = c("CHILE"),
    South_Africa = c("SOUTH AFRICA"),
    Iran = c("IRAN"),
    Saudi_Arabia = c("SAUDI ARABIA"),
    Egypt = c("EGYPT"),
    Pakistan = c("PAKISTAN"),
    Malaysia = c("MALAYSIA"),
    Thailand = c("THAILAND"),
    Indonesia = c("INDONESIA"),
    New_Zealand = c("NEW ZEALAND"),
    Ireland = c("IRELAND", "EIRE"),
    Czech_Republic = c("CZECH REPUBLIC", "CZECHIA"),
    Hungary = c("HUNGARY", "MAGYARORSZÁG")
  )
  
  for (country in names(country_patterns)) {
    for (pattern in country_patterns[[country]]) {
      if (grepl(pattern, affil_upper, fixed = TRUE)) {
        return(gsub("_", " ", country))
      }
    }
  }
  
  NA_character_
}

#' Clean institution name
#' @keywords internal
clean_institution_name <- function(name) {
  if (is.na(name)) return(NA_character_)
  
  # Remove extra whitespace
  name <- trimws(name)
  name <- gsub("\\s+", " ", name)
  
  # Standardize common abbreviations
  name <- gsub("^Univ\\.\\s*", "University ", name, ignore.case = TRUE)
  name <- gsub("^Inst\\.\\s*", "Institute ", name, ignore.case = TRUE)
  name <- gsub("^Dept\\.\\s*", "Department ", name, ignore.case = TRUE)
  name <- gsub("^Lab\\.\\s*", "Laboratory ", name, ignore.case = TRUE)
  
  # Remove trailing punctuation
  name <- gsub("[,.;]+$", "", name)
  
  # Title case
  name <- tools::toTitleCase(tolower(name))
  
  name
}

#' Classify institution sector
#' @keywords internal
classify_sector <- function(institution_name) {
  if (is.na(institution_name)) return(NA_character_)
  
  inst_lower <- tolower(institution_name)
  
  # Academic keywords
  academic <- c("university", "college", "institute of technology", 
                "polytechnic", "academy", "school of")
  
  # Corporate keywords
  corporate <- c("inc", "corp", "ltd", "llc", "gmbh", "sa", "sas", "bv", "nv",
                 "company", "corporation", "industries", "solutions")
  
  # Government keywords
  government <- c("national", "federal", "ministry", "agency", "laboratory",
                  "department of", "institute of", "center for", "centre for")
  
  # Hospital keywords
  hospital <- c("hospital", "medical center", "health center", "clinic")
  
  if (any(sapply(academic, function(k) grepl(k, inst_lower, fixed = TRUE)))) {
    return("Academic")
  } else if (any(sapply(corporate, function(k) grepl(k, inst_lower, fixed = TRUE)))) {
    return("Corporate")
  } else if (any(sapply(government, function(k) grepl(k, inst_lower, fixed = TRUE)))) {
    return("Government")
  } else if (any(sapply(hospital, function(k) grepl(k, inst_lower, fixed = TRUE)))) {
    return("Healthcare")
  } else {
    return("Other")
  }
}

#' Normalize institutions across dataset
#' @keywords internal
normalize_institutions <- function(inst_df) {
  if (is.null(inst_df) || nrow(inst_df) == 0) {
    return(inst_df)
  }
  
  # Group similar institutions using string similarity
  unique_names <- unique(inst_df$institution_clean)
  
  # Create mapping for similar names
  name_mapping <- list()
  
  for (i in seq_along(unique_names)) {
    name <- unique_names[i]
    if (is.na(name)) next
    
    # Check if similar to existing canonical name
    matched <- FALSE
    for (canonical in names(name_mapping)) {
      if (string_similarity(name, canonical) > 0.85) {
        name_mapping[[canonical]] <- c(name_mapping[[canonical]], name)
        matched <- TRUE
        break
      }
    }
    
    if (!matched) {
      name_mapping[[name]] <- name
    }
  }
  
  # Create reverse mapping
  reverse_map <- list()
  for (canonical in names(name_mapping)) {
    for (variant in name_mapping[[canonical]]) {
      reverse_map[[variant]] <- canonical
    }
  }
  
  # Apply mapping
  inst_df$institution_canonical <- sapply(inst_df$institution_clean, function(x) {
    if (is.na(x)) return(NA_character_)
    reverse_map[[x]] %||% x
  })
  
  inst_df
}

#' Calculate string similarity (simplified Jaccard)
#' @keywords internal
string_similarity <- function(s1, s2) {
  if (is.na(s1) || is.na(s2)) return(0)
  
  # Tokenize
  tokens1 <- unlist(strsplit(tolower(s1), "\\s+"))
  tokens2 <- unlist(strsplit(tolower(s2), "\\s+"))
  
  # Jaccard similarity
  intersection <- length(intersect(tokens1, tokens2))
  union <- length(union(tokens1, tokens2))
  
  if (union == 0) return(0)
  intersection / union
}

#' Create institution lookup table
#' @keywords internal
create_institution_lookup <- function(inst_df) {
  inst_df %>%
    dplyr::group_by(institution_canonical) %>%
    dplyr::summarise(
      n_papers = dplyr::n_distinct(doc_id),
      variants = dplyr::n_distinct(institution_clean),
      countries = paste(unique(country[!is.na(country)]), collapse = "; "),
      sectors = paste(unique(sector[!is.na(sector)]), collapse = "; "),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(n_papers))
}