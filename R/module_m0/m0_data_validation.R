# ============================================================================
# m0_data_validation.R - Data Validation and Quality Functions
# ============================================================================
# ORCID/DOI validation, affiliation parsing, email extraction,
# language detection, and comprehensive quality checks

#' Validate and extract ORCID IDs
#'
#' @param orcid_strings Vector of ORCID strings
#' @return List with valid ORCIDs, invalid entries, and statistics
#' @export
m0_validate_orcid <- function(orcid_strings) {
  if (is.null(orcid_strings) || length(orcid_strings) == 0) {
    return(list(
      valid_orcids = character(0),
      invalid_entries = character(0),
      n_valid = 0,
      n_invalid = 0,
      validation_rate = NA,
      status = "error: no input"
    ))
  }
  
  # ORCID pattern: 0000-0000-0000-000X or 000000000000000X
  orcid_pattern <- "^[0-9]{4}-?[0-9]{4}-?[0-9]{4}-?[0-9]{3}[0-9X]$"
  
  orcid_strings <- as.character(orcid_strings)
  valid_mask <- grepl(orcid_pattern, orcid_strings, ignore.case = TRUE)
  
  # Clean valid ORCIDs to standard format
  valid_orcids <- gsub("-", "", orcid_strings[valid_mask])
  valid_orcids <- paste0(
    substr(valid_orcids, 1, 4), "-",
    substr(valid_orcids, 5, 8), "-",
    substr(valid_orcids, 9, 12), "-",
    substr(valid_orcids, 13, 16)
  )
  
  # Calculate checksum
  valid_with_checksum <- sapply(valid_orcids, validate_orcid_checksum)
  
  invalid_entries <- orcid_strings[!valid_mask]
  
  list(
    valid_orcids = valid_orcids,
    valid_with_checksum = valid_orcids[valid_with_checksum],
    invalid_entries = invalid_entries,
    n_valid = length(valid_orcids),
    n_valid_checksum = sum(valid_with_checksum),
    n_invalid = length(invalid_entries),
    validation_rate = length(valid_orcids) / length(orcid_strings),
    status = "success"
  )
}

#' Validate ORCID checksum
#' @keywords internal
validate_orcid_checksum <- function(orcid) {
  orcid_clean <- gsub("-", "", orcid)
  digits <- strsplit(orcid_clean, "")[[1]]
  
  # ISO7064 checksum
  total <- 0
  for (digit in digits[1:15]) {
    total <- (total + as.numeric(digit)) * 2
  }
  
  remainder <- total %% 11
  result <- (12 - remainder) %% 11
  
  check_digit <- if (result == 10) "X" else as.character(result)
  
  check_digit == digits[16]
}

#' Validate and clean DOIs
#'
#' @param doi_strings Vector of DOI strings
#' @return List with cleaned DOIs, invalid entries, and statistics
#' @export
m0_validate_doi <- function(doi_strings) {
  if (is.null(doi_strings) || length(doi_strings) == 0) {
    return(list(
      valid_dois = character(0),
      invalid_entries = character(0),
      n_valid = 0,
      n_invalid = 0,
      status = "error: no input"
    ))
  }
  
  doi_strings <- as.character(doi_strings)
  
  # DOI patterns
  doi_pattern <- "^10\\.[0-9]{4,}(\\.[0-9]+)*/[-._;()/:A-Z0-9]+$"
  
  # Clean DOI strings
  cleaned <- doi_strings
  cleaned <- gsub("^.*?(10\\.[0-9]+)", "\\1", cleaned, ignore.case = TRUE)
  cleaned <- gsub("\\s+", "", cleaned)
  cleaned <- gsub("^doi:\\s*", "", cleaned, ignore.case = TRUE)
  cleaned <- gsub("^https?://(dx\\.)?doi\\.org/", "", cleaned, ignore.case = TRUE)
  
  # Validate
  valid_mask <- grepl(doi_pattern, cleaned, ignore.case = TRUE)
  
  valid_dois <- cleaned[valid_mask]
  invalid_entries <- doi_strings[!valid_mask]
  
  # Fuzzy match for similar DOIs
  similar_pairs <- find_similar_dois(valid_dois)
  
  list(
    valid_dois = valid_dois,
    invalid_entries = invalid_entries,
    similar_pairs = similar_pairs,
    n_valid = length(valid_dois),
    n_invalid = length(invalid_entries),
    n_unique = length(unique(valid_dois)),
    n_potential_duplicates = nrow(similar_pairs),
    validation_rate = length(valid_dois) / length(doi_strings),
    status = "success"
  )
}

#' Find similar DOIs (potential duplicates)
#' @keywords internal
find_similar_dois <- function(dois) {
  if (length(dois) < 2) {
    return(data.frame(doi1 = character(0), doi2 = character(0), similarity = numeric(0)))
  }
  
  dois <- unique(dois)
  n <- length(dois)
  
  pairs <- data.frame(
    doi1 = character(0),
    doi2 = character(0),
    similarity = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      sim <- m0_levenshtein_distance(dois[i], dois[j]) / max(nchar(dois[i]), nchar(dois[j]))
      if (sim > 0.9 && dois[i] != dois[j]) {
        pairs <- rbind(pairs, data.frame(
          doi1 = dois[i],
          doi2 = dois[j],
          similarity = sim
        ))
      }
    }
  }
  
  pairs
}

#' Extract and validate email addresses
#'
#' @param text Vector of text containing emails
#' @return List with emails and statistics
#' @export
m0_extract_emails <- function(text) {
  if (is.null(text) || length(text) == 0) {
    return(list(
      emails = character(0),
      valid_emails = character(0),
      n_emails = 0,
      status = "error: no input"
    ))
  }
  
  text <- as.character(text)
  
  # Email pattern
  email_pattern <- "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"
  
  all_emails <- unlist(regmatches(text, gregexpr(email_pattern, text, ignore.case = TRUE)))
  
  if (length(all_emails) == 0) {
    return(list(
      emails = character(0),
      valid_emails = character(0),
      n_emails = 0,
      status = "success"
    ))
  }
  
  all_emails <- tolower(unique(all_emails))
  
  # Validate emails
  valid_mask <- validate_email_domain(all_emails)
  valid_emails <- all_emails[valid_mask]
  
  list(
    emails = all_emails,
    valid_emails = valid_emails,
    invalid_emails = all_emails[!valid_mask],
    n_emails = length(all_emails),
    n_valid = length(valid_emails),
    domains = unique(gsub(".*@", "", valid_emails)),
    status = "success"
  )
}

#' Validate email domain
#' @keywords internal
validate_email_domain <- function(emails) {
  # Basic validation - domain exists
  domains <- gsub(".*@", "", emails)
  
  # Common TLDs
  valid_tlds <- c("com", "org", "edu", "gov", "net", "io", "ac", "co", "uk", "au", 
                  "de", "fr", "jp", "cn", "in", "br", "ca", "nl", "es", "it", 
                  "se", "ch", "be", "at", "dk", "no", "pl", "tw", "hk", "sg")
  
  tlds <- gsub(".*\\.([^.]+)$", "\\1", domains)
  
  # Check for valid structure
  valid_structure <- grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", emails)
  
  valid_structure & (tlds %in% valid_tlds | nchar(tlds) >= 2)
}

#' Parse affiliation strings
#'
#' @param affiliations Vector of affiliation strings
#' @return Data frame with institution, department, city, country
#' @export
m0_parse_affiliations <- function(affiliations) {
  if (is.null(affiliations) || length(affiliations) == 0) {
    return(data.frame(
      original = character(0),
      institution = character(0),
      department = character(0),
      city = character(0),
      country = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  affiliations <- as.character(affiliations)
  
  parsed <- lapply(affiliations, function(aff) {
    if (is.na(aff) || aff == "") {
      return(data.frame(
        original = aff,
        institution = NA_character_,
        department = NA_character_,
        city = NA_character_,
        country = NA_character_,
        stringsAsFactors = FALSE
      ))
    }
    
    # Split by semicolon for multiple affiliations
    parts <- strsplit(aff, ";")[[1]]
    parts <- trimws(parts)
    
    institution <- extract_institution(parts[1])
    department <- extract_department(parts[1])
    city <- extract_city(parts[1])
    country <- m0_normalize_countries(extract_country(parts[1]))
    
    data.frame(
      original = aff,
      institution = institution,
      department = department,
      city = city,
      country = country,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, parsed)
}

#' Extract institution from affiliation string
#' @keywords internal
extract_institution <- function(aff) {
  if (is.na(aff) || aff == "") return(NA_character_)
  
  # Common patterns
  patterns <- c(
    "University of ([A-Za-z ]+)",
    "([A-Za-z ]+) University",
    "([A-Za-z ]+) Institute",
    "([A-Za-z ]+) College",
    "([A-Za-z ]+) School of"
  )
  
  for (p in patterns) {
    m <- regmatches(aff, regexec(p, aff, ignore.case = TRUE))[[1]]
    if (length(m) > 0 && nchar(m[1]) > 3) {
      return(tools::toTitleCase(tolower(m[1])))
    }
  }
  
  # First part before comma
  first_part <- strsplit(aff, ",")[[1]][1]
  trimws(first_part)
}

#' Extract department from affiliation string
#' @keywords internal
extract_department <- function(aff) {
  if (is.na(aff) || aff == "") return(NA_character_)
  
  patterns <- c(
    "Department of ([A-Za-z ]+)",
    "([A-Za-z ]+) Department",
    "School of ([A-Za-z ]+)",
    "Faculty of ([A-Za-z ]+)"
  )
  
  for (p in patterns) {
    m <- regmatches(aff, regexec(p, aff, ignore.case = TRUE))[[1]]
    if (length(m) > 0) {
      return(tools::toTitleCase(tolower(m[1])))
    }
  }
  
  NA_character_
}

#' Extract city from affiliation string
#' @keywords internal
extract_city <- function(aff) {
  if (is.na(aff) || aff == "") return(NA_character_)
  
  parts <- strsplit(aff, ",")[[1]]
  if (length(parts) >= 3) {
    city <- trimws(parts[length(parts) - 1])
    return(city)
  }
  
  NA_character_
}

#' Extract country from affiliation string
#' @keywords internal
extract_country <- function(aff) {
  if (is.na(aff) || aff == "") return(NA_character_)
  
  parts <- strsplit(aff, ",")[[1]]
  if (length(parts) >= 2) {
    country <- trimws(parts[length(parts)])
    return(country)
  }
  
  NA_character_
}

#' Detect document language
#'
#' @param text Character vector (titles/abstracts)
#' @return Data frame with detected languages
#' @export
m0_detect_language <- function(text) {
  if (is.null(text) || length(text) == 0) {
    return(data.frame(
      language = character(0),
      confidence = numeric(0),
      status = "error: no input"
    ))
  }
  
  text <- as.character(text)
  
  # Try textcat package first
  lang_detected <- tryCatch({
    if (requireNamespace("textcat", quietly = TRUE)) {
      textcat::textcat(text)
    } else {
      # Fallback: character frequency analysis
      sapply(text, detect_language_simple)
    }
  }, error = function(e) {
    sapply(text, detect_language_simple)
  })
  
  # Confidence (simple version)
  confidence <- ifelse(lang_detected %in% c("english", "en", "eng"), 0.9, 0.7)
  
  data.frame(
    language = lang_detected,
    confidence = confidence,
    stringsAsFactors = FALSE
  )
}

#' Simple language detection without external packages
#' @keywords internal
detect_language_simple <- function(text) {
  if (is.na(text) || nchar(text) < 10) return("unknown")
  
  text <- tolower(text)
  
  # English indicators
  english_words <- c("the", "and", "of", "to", "in", "is", "for", "with", "on", "as")
  english_count <- sum(sapply(english_words, function(w) grepl(paste0("\\b", w, "\\b"), text)))
  
  # Spanish indicators
  spanish_words <- c("de", "la", "el", "en", "y", "los", "las", "un", "una", "por")
  spanish_count <- sum(sapply(spanish_words, function(w) grepl(paste0("\\b", w, "\\b"), text)))
  
  # French indicators
  french_words <- c("de", "la", "le", "les", "des", "et", "en", "du", "un", "une")
  french_count <- sum(sapply(french_words, function(w) grepl(paste0("\\b", w, "\\b"), text)))
  
  # German indicators
  german_words <- c("der", "die", "das", "und", "von", "mit", "fur", "auf", "als", "ist")
  german_count <- sum(sapply(german_words, function(w) grepl(paste0("\\b", w, "\\b"), text)))
  
  # Chinese indicators
  chinese_chars <- sum(nchar(gsub("[^\u4e00-\u9fff]", "", text)))
  
  if (chinese_chars > nchar(text) * 0.3) return("chinese")
  
  counts <- c(english = english_count, spanish = spanish_count, french = french_count, german = german_count)
  
  if (max(counts) == 0) return("unknown")
  
  names(which.max(counts))
}

#' Clean abstracts (remove LaTeX, special characters)
#'
#' @param abstracts Character vector of abstracts
#' @return Cleaned abstracts
#' @export
m0_clean_abstracts <- function(abstracts) {
  if (is.null(abstracts)) return(character(0))
  
  abstracts <- as.character(abstracts)
  
  # Remove LaTeX
  abstracts <- gsub("\\\\[a-zA-Z]+\\{[^}]*\\}", "", abstracts)
  abstracts <- gsub("\\\\[a-zA-Z]+", "", abstracts)
  abstracts <- gsub("\\$[^$]*\\$", "", abstracts)
  
  # Remove special characters
  abstracts <- gsub("[\u2018\u2019]", "'", abstracts)  # Smart quotes
  abstracts <- gsub("[\u201C\u201D]", "\"", abstracts)   # Smart double quotes
  abstracts <- gsub("\u2013|\u2014", "-", abstracts)    # Em/en dashes
  abstracts <- gsub("\u2026", "...", abstracts)          # Ellipsis
  abstracts <- gsub("[\u00A0\u2000-\u200B]", " ", abstracts)  # Non-breaking spaces
  
  # Remove URLs
  abstracts <- gsub("https?://[^\\s]+", "", abstracts)
  
  # Remove email addresses
  abstracts <- gsub("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", "", abstracts)
  
  # Remove HTML tags
  abstracts <- gsub("<[^>]+>", "", abstracts)
  
  # Normalize whitespace
  abstracts <- gsub("\\s+", " ", abstracts)
  abstracts <- trimws(abstracts)
  
  abstracts
}

#' Extract funding information
#'
#' @param text Vector containing funding/acknowledgment text
#' @return List with grant numbers and funders
#' @export
m0_extract_funding <- function(text) {
  if (is.null(text) || length(text) == 0) {
    return(list(
      grant_numbers = data.frame(),
      funders = character(0),
      n_grants = 0,
      status = "error: no input"
    ))
  }
  
  text <- paste(text, collapse = " ")
  
  # Common grant patterns
  grant_patterns <- c(
    # NIH
    "R01[A-Za-z0-9]{2}[0-9]{5,6}",
    "R21[A-Za-z0-9]{2}[0-9]{5,6}",
    "U01[A-Za-z0-9]{2}[0-9]{5,6}",
    "P01[A-Za-z0-9]{2}[0-9]{5,6}",
    # EU
    "FP[57]?[-]?[0-9]{6,7}",
    "H2020[-]?[0-9]{6,7}",
    # NSF
    "[A-Z]{2}E[-]?[0-9]{7}",
    "CCF[-]?[0-9]{7}",
    # UK
    "G[0-9]{8}",
    "MC_[A-Z]+_[0-9]+"
  )
  
  all_grants <- character(0)
  for (p in grant_patterns) {
    matches <- regmatches(text, gregexpr(p, text, ignore.case = TRUE))[[1]]
    all_grants <- c(all_grants, matches)
  }
  all_grants <- unique(all_grants)
  
  # Funder extraction
  funder_keywords <- c(
    "National Institutes of Health", "NIH", "National Science Foundation", "NSF",
    "European Research Council", "ERC", "Horizon 2020", "H2020",
    "Wellcome Trust", "Bill and Melinda Gates", "Gates Foundation",
    "National Natural Science Foundation", "NSFC"," Deutsche Forschungsgemeinschaft",
    "Ministry of Science", "Science Foundation"
  )
  
  found_funders <- sapply(funder_keywords, function(f) {
    if (grepl(f, text, ignore.case = TRUE)) f else NA
  })
  found_funders <- found_funders[!is.na(found_funders)]
  
  list(
    grant_numbers = if (length(all_grants) > 0) {
      data.frame(grant = all_grants, stringsAsFactors = FALSE)
    } else {
      data.frame()
    },
    funders = unique(found_funders),
    n_grants = length(all_grants),
    n_funders = length(found_funders),
    status = "success"
  )
}

#' Complete data quality report with all checks
#'
#' @param df Bibliographic data frame
#' @return Comprehensive quality report
#' @export
m0_full_quality_report <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(list(status = "error: invalid data frame"))
  }
  
  report <- list(
    n_records = nrow(df),
    n_columns = ncol(df),
    columns = names(df),
    missing_data = list(),
    quality_checks = list(),
    validations = list()
  )
  
  # Missing data
  for (col in names(df)) {
    n_missing <- sum(is.na(df[[col]]) | df[[col]] == "")
    report$missing_data[[col]] <- list(
      n_missing = n_missing,
      pct_missing = n_missing / nrow(df) * 100
    )
  }
  
  # ORCID validation
  if ("OI" %in% names(df)) {
    report$validations$orcid <- m0_validate_orcid(df$OI)
  }
  
  # DOI validation
  if ("DI" %in% names(df)) {
    report$validations$doi <- m0_validate_doi(df$DI)
  }
  
  # Email extraction
  if ("AU" %in% names(df)) {
    report$validations$emails <- m0_extract_emails(df$AU)
  }
  
  # Language detection (sample)
  if ("AB" %in% names(df)) {
    sample_size <- min(100, nrow(df))
    report$validations$language <- m0_detect_language(df$AB[1:sample_size])
  }
  
  # Funding extraction
  if ("FX" %in% names(df) || "FU" %in% names(df)) {
    funding_cols <- c()
    if ("FX" %in% names(df)) funding_cols <- c(funding_cols, df$FX)
    if ("FU" %in% names(df)) funding_cols <- c(funding_cols, df$FU)
    report$validations$funding <- m0_extract_funding(funding_cols)
  }
  
  # Year validation
  if ("PY" %in% names(df)) {
    years <- as.numeric(df$PY)
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    report$quality_checks$years <- list(
      n_missing = sum(is.na(years)),
      n_future = sum(years > current_year, na.rm = TRUE),
      n_invalid = sum(years < 1800 | years > current_year + 1, na.rm = TRUE),
      range = range(years, na.rm = TRUE)
    )
  }
  
  # Citation validation
  if ("TC" %in% names(df)) {
    citations <- as.numeric(df$TC)
    report$quality_checks$citations <- list(
      n_missing = sum(is.na(citations)),
      n_negative = sum(citations < 0, na.rm = TRUE),
      max_citation = max(citations, na.rm = TRUE),
      median_citation = median(citations, na.rm = TRUE)
    )
  }
  
  report$status <- "success"
  report
}
