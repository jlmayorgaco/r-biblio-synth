# ============================================================================
# m0_normalize_data.R - Data Normalization and Cleaning
# ============================================================================
# Comprehensive country name standardization, author disambiguation,
# and data quality functions

#' Comprehensive country name normalization
#'
#' @param country Vector of country names
#' @return Normalized country names
#' @export
m0_normalize_countries <- function(country) {
  if (is.null(country)) return(character(0))
  
  country <- toupper(trimws(as.character(country)))
  country <- gsub("[[:punct:]]", " ", country)
  country <- gsub("\\s+", " ", country)
  country <- trimws(country)
  
  # Comprehensive mapping dictionary
  country_map <- c(
    # USA variants
    "USA" = "UNITED STATES",
    "U S A" = "UNITED STATES",
    "U.S.A." = "UNITED STATES",
    "U S A" = "UNITED STATES",
    "UNITED STATES OF AMERICA" = "UNITED STATES",
    "US" = "UNITED STATES",
    "U.S." = "UNITED STATES",
    "UNITED STSTE" = "UNITED STATES",
    "UNITED STATE" = "UNITED STATES",
    "AMERICA" = "UNITED STATES",
    
    # UK variants
    "UK" = "UNITED KINGDOM",
    "U.K." = "UNITED KINGDOM",
    "U K" = "UNITED KINGDOM",
    "ENGLAND" = "UNITED KINGDOM",
    "SCOTLAND" = "UNITED KINGDOM",
    "WALES" = "UNITED KINGDOM",
    "NORTHERN IRELAND" = "UNITED KINGDOM",
    "GREAT BRITAIN" = "UNITED KINGDOM",
    "BRITAIN" = "UNITED KINGDOM",
    
    # China variants
    "CHINA" = "CHINA",
    "PEOPLES R CHINA" = "CHINA",
    "PEOPLES REPUBLIC OF CHINA" = "CHINA",
    "P R CHINA" = "CHINA",
    "P.R. CHINA" = "CHINA",
    "PRCHINA" = "CHINA",
    "PRC" = "CHINA",
    "HONG KONG" = "CHINA",
    "HONG KONG SAR" = "CHINA",
    "MACAU" = "CHINA",
    "MACAO" = "CHINA",
    "TAIWAN" = "CHINA",
    "REPUBLIC OF CHINA" = "CHINA",
    
    # Korea variants
    "SOUTH KOREA" = "SOUTH KOREA",
    "KOREA" = "SOUTH KOREA",
    "REPUBLIC OF KOREA" = "SOUTH KOREA",
    "KOREA REPUBLIC" = "SOUTH KOREA",
    "NORTH KOREA" = "NORTH KOREA",
    "DEMOCRATIC PEOPLES REPUBLIC OF KOREA" = "NORTH KOREA",
    "DPRK" = "NORTH KOREA",
    
    # Russia variants
    "RUSSIA" = "RUSSIA",
    "RUSSIAN FEDERATION" = "RUSSIA",
    "RUSSIAN FED" = "RUSSIA",
    "USSR" = "RUSSIA",
    "SOVIET UNION" = "RUSSIA",
    
    # Iran variants
    "IRAN" = "IRAN",
    "IRAN ISLAMIC REPUBLIC" = "IRAN",
    "ISLAMIC REPUBLIC OF IRAN" = "IRAN",
    "PERSIA" = "IRAN",
    
    # Common European variants
    "CZECH REPUBLIC" = "CZECHIA",
    "CZECH" = "CZECHIA",
    "CZECHOSLOVAKIA" = "CZECHIA",
    "SLOVAK REPUBLIC" = "SLOVAKIA",
    "REPUBLIC OF IRELAND" = "IRELAND",
    "EIRE" = "IRELAND",
    "FEDERAL REPUBLIC OF GERMANY" = "GERMANY",
    "WEST GERMANY" = "GERMANY",
    "EAST GERMANY" = "GERMANY",
    "DEUTSCHLAND" = "GERMANY",
    "BOSNIA HERZEGOVINA" = "BOSNIA AND HERZEGOVINA",
    "BOSNIA" = "BOSNIA AND HERZEGOVINA",
    "MACEDONIA" = "NORTH MACEDONIA",
    "REPUBLIC OF MACEDONIA" = "NORTH MACEDONIA",
    "FYROM" = "NORTH MACEDONIA",
    "SERBIA AND MONTENEGRO" = "SERBIA",
    "YUGOSLAVIA" = "SERBIA",
    
    # India variants
    "INDIA" = "INDIA",
    "BHARAT" = "INDIA",
    "HINDUSTAN" = "INDIA",
    
    # Japan variants
    "JAPAN" = "JAPAN",
    "NIPPON" = "JAPAN",
    
    # Brazil variants
    "BRAZIL" = "BRAZIL",
    "BRASIL" = "BRAZIL",
    "FEDERATIVE REPUBLIC OF BRAZIL" = "BRAZIL",
    
    # Australia variants
    "AUSTRALIA" = "AUSTRALIA",
    "COMMONWEALTH OF AUSTRALIA" = "AUSTRALIA",
    
    # Canada variants
    "CANADA" = "CANADA",
    
    # France variants
    "FRANCE" = "FRANCE",
    "FRENCH REPUBLIC" = "FRANCE",
    "REPUBLIQUE FRANCAISE" = "FRANCE",
    
    # Germany variants
    "GERMANY" = "GERMANY",
    "DEUTSCHLAND" = "GERMANY",
    "FEDERAL REPUBLIC OF GERMANY" = "GERMANY",
    
    # Italy variants
    "ITALY" = "ITALY",
    "ITALIA" = "ITALY",
    "REPUBLIC OF ITALY" = "ITALY",
    
    # Spain variants
    "SPAIN" = "SPAIN",
    "ESPANA" = "SPAIN",
    "ESPAÑA" = "SPAIN",
    "KINGDOM OF SPAIN" = "SPAIN",
    
    # Netherlands variants
    "NETHERLANDS" = "NETHERLANDS",
    "THE NETHERLANDS" = "NETHERLANDS",
    "HOLLAND" = "NETHERLANDS",
    "KONINKRIJK DER NEDERLANDEN" = "NETHERLANDS",
    
    # Switzerland variants
    "SWITZERLAND" = "SWITZERLAND",
    "SCHWEIZ" = "SWITZERLAND",
    "SUISSE" = "SWITZERLAND",
    "SVIZZERA" = "SWITZERLAND",
    "HELVETIA" = "SWITZERLAND",
    
    # Sweden variants
    "SWEDEN" = "SWEDEN",
    "SVERIGE" = "SWEDEN",
    "KINGDOM OF SWEDEN" = "SWEDEN",
    
    # Other common variants
    "VIETNAM" = "VIETNAM",
    "VIET NAM" = "VIETNAM",
    "SOCIALIST REPUBLIC OF VIETNAM" = "VIETNAM",
    
    "THAILAND" = "THAILAND",
    "KINGDOM OF THAILAND" = "THAILAND",
    "SIAM" = "THAILAND",
    
    "EGYPT" = "EGYPT",
    "ARAB REPUBLIC OF EGYPT" = "EGYPT",
    
    "SOUTH AFRICA" = "SOUTH AFRICA",
    "REPUBLIC OF SOUTH AFRICA" = "SOUTH AFRICA",
    
    "NEW ZEALAND" = "NEW ZEALAND",
    "AOTEAROA" = "NEW ZEALAND",
    
    "MEXICO" = "MEXICO",
    "ESTADOS UNIDOS MEXICANOS" = "MEXICO",
    
    "ARGENTINA" = "ARGENTINA",
    "ARGENTINE REPUBLIC" = "ARGENTINA",
    
    "SINGAPORE" = "SINGAPORE",
    "REPUBLIC OF SINGAPORE" = "SINGAPORE",
    
    "MALAYSIA" = "MALAYSIA",
    
    "INDONESIA" = "INDONESIA",
    "REPUBLIC OF INDONESIA" = "INDONESIA",
    
    "PHILIPPINES" = "PHILIPPINES",
    "REPUBLIC OF THE PHILIPPINES" = "PHILIPPINES",
    
    "POLAND" = "POLAND",
    "REPUBLIC OF POLAND" = "POLAND",
    "POLSKA" = "POLAND",
    
    "TURKEY" = "TURKEY",
    "TÜRKIYE" = "TURKEY",
    "TURKIYE" = "TURKEY",
    "REPUBLIC OF TURKEY" = "TURKEY",
    
    "SAUDI ARABIA" = "SAUDI ARABIA",
    "KINGDOM OF SAUDI ARABIA" = "SAUDI ARABIA",
    
    "UAE" = "UNITED ARAB EMIRATES",
    "UNITED ARAB EMIRATES" = "UNITED ARAB EMIRATES",
    
    "PAKISTAN" = "PAKISTAN",
    "ISLAMIC REPUBLIC OF PAKISTAN" = "PAKISTAN",
    
    "BANGLADESH" = "BANGLADESH",
    "PEOPLES REPUBLIC OF BANGLADESH" = "BANGLADESH",
    
    "NIGERIA" = "NIGERIA",
    "FEDERAL REPUBLIC OF NIGERIA" = "NIGERIA",
    
    "KENYA" = "KENYA",
    "REPUBLIC OF KENYA" = "KENYA",
    
    "ETHIOPIA" = "ETHIOPIA",
    
    "CHILE" = "CHILE",
    "REPUBLIC OF CHILE" = "CHILE",
    
    "COLOMBIA" = "COLOMBIA",
    "REPUBLIC OF COLOMBIA" = "COLOMBIA",
    
    "PERU" = "PERU",
    "REPUBLIC OF PERU" = "PERU",
    
    "VENEZUELA" = "VENEZUELA",
    "BOLIVARIAN REPUBLIC OF VENEZUELA" = "VENEZUELA",
    
    "GREECE" = "GREECE",
    "HELLENIC REPUBLIC" = "GREECE",
    "ELLADA" = "GREECE",
    
    "PORTUGAL" = "PORTUGAL",
    "PORTUGUESE REPUBLIC" = "PORTUGAL",
    
    "AUSTRIA" = "AUSTRIA",
    "REPUBLIC OF AUSTRIA" = "AUSTRIA",
    "ÖSTERREICH" = "AUSTRIA",
    
    "BELGIUM" = "BELGIUM",
    "KINGDOM OF BELGIUM" = "BELGIUM",
    "BELGIE" = "BELGIUM",
    "BELGIQUE" = "BELGIUM",
    
    "DENMARK" = "DENMARK",
    "KINGDOM OF DENMARK" = "DENMARK",
    "DANMARK" = "DENMARK",
    
    "NORWAY" = "NORWAY",
    "KINGDOM OF NORWAY" = "NORWAY",
    "NORGE" = "NORWAY",
    
    "FINLAND" = "FINLAND",
    "REPUBLIC OF FINLAND" = "FINLAND",
    "SUOMI" = "FINLAND",
    
    "ISRAEL" = "ISRAEL",
    "STATE OF ISRAEL" = "ISRAEL",
    
    "ARABIA SAUDI" = "SAUDI ARABIA",
    
    # Continent corrections
    "AFRICA" = NA_character_,
    "ASIA" = NA_character_,
    "EUROPE" = NA_character_,
    "AMERICA" = NA_character_,
    "SOUTH AMERICA" = NA_character_,
    "NORTH AMERICA" = NA_character_,
    "OCEANIA" = NA_character_,
    
    # Handle empty/invalid
    "N/A" = NA_character_,
    "NA" = NA_character_,
    "UNKNOWN" = NA_character_,
    "NOT SPECIFIED" = NA_character_
  )
  
  # Apply mapping
  for (variant in names(country_map)) {
    country[country == variant] <- country_map[variant]
  }
  
  # Remove extra whitespace
  country <- trimws(country)
  
  # Return empty for invalid
  country[country == "" | is.na(country)] <- NA_character_
  
  country
}

#' Extract countries from affiliation strings
#'
#' @param affiliations Character vector of affiliation strings
#' @return List of extracted countries per affiliation
#' @export
m0_extract_countries <- function(affiliations) {
  if (is.null(affiliations)) return(list())
  
  # Common patterns for country extraction
  country_pattern <- ";\\s*([^,;]+)\\s*$"
  
  countries <- lapply(as.character(affiliations), function(aff) {
    if (is.na(aff) || aff == "") return(character(0))
    
    # Split by semicolon (multiple affiliations)
    parts <- strsplit(aff, ";")[[1]]
    parts <- trimws(parts)
    
    extracted <- character(0)
    for (part in parts) {
      # Try to extract country from end
      matches <- regmatches(part, regexpr(",\\s*([^,]+)\\s*$", part))
      if (length(matches) > 0) {
        country <- gsub("^,\\s*", "", matches)
        country <- trimws(country)
        if (nchar(country) > 0) {
          extracted <- c(extracted, country)
        }
      }
    }
    
    # Normalize
    m0_normalize_countries(extracted)
  })
  
  countries
}

#' Fuzzy title matching for deduplication
#'
#' @param titles Character vector of titles
#' @param threshold Similarity threshold (0-1)
#' @return Matrix of similarity scores
#' @export
m0_fuzzy_title_match <- function(titles, threshold = 0.85) {
  if (length(titles) < 2) return(matrix(1, 1, 1))
  
  # Normalize titles
  titles_norm <- tolower(trimws(as.character(titles)))
  titles_norm <- gsub("[[:punct:]]", " ", titles_norm)
  titles_norm <- gsub("[[:digit:]]", " ", titles_norm)
  titles_norm <- gsub("\\s+", " ", titles_norm)
  titles_norm <- trimws(titles_norm)
  
  n <- length(titles_norm)
  sim_matrix <- matrix(0, n, n)
  
  # Use Levenshtein-like comparison
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      sim <- m0_string_similarity(titles_norm[i], titles_norm[j])
      sim_matrix[i, j] <- sim
      sim_matrix[j, i] <- sim
    }
  }
  diag(sim_matrix) <- 1
  
  sim_matrix
}

#' Calculate string similarity (Jaccard + Levenshtein hybrid)
#' @keywords internal
m0_string_similarity <- function(s1, s2) {
  if (is.na(s1) || is.na(s2)) return(0)
  
  # Tokenize
  tokens1 <- strsplit(s1, "\\s+")[[1]]
  tokens2 <- strsplit(s2, "\\s+")[[1]]
  
  # Jaccard similarity
  intersection <- length(intersect(tokens1, tokens2))
  union <- length(union(tokens1, tokens2))
  jaccard <- if (union > 0) intersection / union else 0
  
  # Prefix similarity (for short strings)
  min_len <- min(nchar(s1), nchar(s2))
  max_len <- max(nchar(s1), nchar(s2))
  if (max_len == 0) return(1)
  
  # Levenshtein-like
  lev_dist <- m0_levenshtein_distance(s1, s2)
  lev_sim <- 1 - lev_dist / max_len
  
  # Hybrid
  0.5 * jaccard + 0.5 * lev_sim
}

#' Levenshtein distance
#' @keywords internal
m0_levenshtein_distance <- function(s1, s2) {
  if (is.na(s1) || is.na(s2)) return(Inf)
  
  n1 <- nchar(s1)
  n2 <- nchar(s2)
  
  if (n1 == 0) return(n2)
  if (n2 == 0) return(n1)
  
  # Create matrix
  d <- matrix(0, nrow = n1 + 1, ncol = n2 + 1)
  d[, 1] <- 0:n1
  d[1, ] <- 0:n2
  
  chars1 <- strsplit(s1, "")[[1]]
  chars2 <- strsplit(s2, "")[[1]]
  
  for (i in 2:(n1 + 1)) {
    for (j in 2:(n2 + 1)) {
      cost <- if (chars1[i - 1] == chars2[j - 1]) 0 else 1
      d[i, j] <- min(
        d[i - 1, j] + 1,      # deletion
        d[i, j - 1] + 1,      # insertion
        d[i - 1, j - 1] + cost # substitution
      )
    }
  }
  
  d[n1 + 1, n2 + 1]
}

#' Author name disambiguation helper
#'
#' @param authors Character vector of author names
#' @return List with cleaned names and clusters
#' @export
m0_disambiguate_authors <- function(authors) {
  if (is.null(authors) || length(authors) == 0) {
    return(list(names = character(0), clusters = list()))
  }
  
  # Normalize author names
  authors_norm <- sapply(authors, function(x) {
    if (is.na(x) || x == "") return(NA_character_)
    
    # Split by semicolon (multiple authors)
    names <- strsplit(as.character(x), ";")[[1]]
    names <- trimws(names)
    names <- names[names != "" & !is.na(names)]
    
    # Normalize each name
    sapply(names, function(n) {
      # Convert to title case
      n <- tools::toTitleCase(tolower(n))
      
      # Standardize initials
      n <- gsub("\\.\\s*", " ", n)
      n <- gsub("\\s+", " ", n)
      
      # Extract last name and initials
      parts <- strsplit(n, "\\s+")[[1]]
      if (length(parts) == 0) return(n)
      
      # Last name is typically the last part
      last_name <- parts[length(parts)]
      first_parts <- parts[-length(parts)]
      
      # Standardize initials
      initials <- paste0(substr(first_parts, 1, 1), collapse = "")
      
      paste(last_name, initials, sep = ", ")
    })
  })
  
  # Flatten
  all_names <- unique(unlist(authors_norm))
  all_names <- all_names[!is.na(all_names)]
  
  # Cluster similar names
  if (length(all_names) > 1) {
    sim_matrix <- m0_fuzzy_author_similarity(all_names)
    clusters <- m0_cluster_authors(all_names, sim_matrix)
  } else {
    clusters <- setNames(as.list(all_names), all_names)
  }
  
  list(
    names = all_names,
    normalized = authors_norm,
    clusters = clusters
  )
}

#' Fuzzy author similarity
#' @keywords internal
m0_fuzzy_author_similarity <- function(names) {
  n <- length(names)
  sim <- matrix(0, n, n)
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      s <- m0_author_name_similarity(names[i], names[j])
      sim[i, j] <- s
      sim[j, i] <- s
    }
  }
  diag(sim) <- 1
  
  sim
}

#' Author name similarity
#' @keywords internal
m0_author_name_similarity <- function(name1, name2) {
  # Parse names
  parts1 <- strsplit(name1, ",\\s*")[[1]]
  parts2 <- strsplit(name2, ",\\s*")[[1]]
  
  if (length(parts1) < 1 || length(parts2) < 1) return(0)
  
  # Last names
  last1 <- parts1[1]
  last2 <- parts2[1]
  
  # Last name similarity (most important)
  last_sim <- if (last1 == last2) 1 else {
    lev <- m0_levenshtein_distance(last1, last2)
    1 - lev / max(nchar(last1), nchar(last2))
  }
  
  if (last_sim < 0.8) return(0)
  
  # Initials
  init1 <- if (length(parts1) > 1) parts1[2] else ""
  init2 <- if (length(parts2) > 1) parts2[2] else ""
  
  init_sim <- if (init1 == "" || init2 == "") 0.5 else {
    # Compare initials
    common_init <- length(intersect(strsplit(init1, "")[[1]], strsplit(init2, "")[[1]]))
    max_init <- max(nchar(init1), nchar(init2))
    common_init / max_init
  }
  
  # Weighted combination
  0.6 * last_sim + 0.4 * init_sim
}

#' Cluster authors
#' @keywords internal
m0_cluster_authors <- function(names, sim_matrix, threshold = 0.85) {
  n <- length(names)
  clusters <- list()
  assigned <- rep(FALSE, n)
  
  for (i in 1:n) {
    if (assigned[i]) next
    
    # Find similar names
    similar <- which(sim_matrix[i, ] >= threshold & !assigned)
    
    # Create cluster
    canonical <- names[i]
    clusters[[canonical]] <- names[similar]
    assigned[similar] <- TRUE
  }
  
  clusters
}

#' Check data quality and report issues
#'
#' @param df Bibliometric data frame
#' @return List with quality metrics and issues
#' @export
m0_check_data_quality <- function(df) {
  issues <- list()
  metrics <- list()
  
  # Check required columns
  required_cols <- c("AU", "TI", "PY", "SO")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    issues$missing_columns <- missing_cols
  }
  
  # Check missing values
  if ("AU" %in% names(df)) {
    metrics$n_missing_authors <- sum(is.na(df$AU) | df$AU == "")
    metrics$pct_missing_authors <- metrics$n_missing_authors / nrow(df) * 100
  }
  
  if ("PY" %in% names(df)) {
    metrics$n_missing_years <- sum(is.na(df$PY))
    metrics$pct_missing_years <- metrics$n_missing_years / nrow(df) * 100
    
    # Check for invalid years
    years <- as.numeric(df$PY)
    invalid_years <- sum(years < 1800 | years > as.numeric(format(Sys.Date(), "%Y")) + 1, na.rm = TRUE)
    metrics$n_invalid_years <- invalid_years
  }
  
  if ("TC" %in% names(df)) {
    metrics$n_missing_citations <- sum(is.na(df$TC))
    metrics$n_negative_citations <- sum(df$TC < 0, na.rm = TRUE)
    metrics$max_citations <- max(df$TC, na.rm = TRUE)
    # Outlier detection
    q <- quantile(df$TC, c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    metrics$n_citation_outliers <- sum(df$TC > q[2] + 3 * iqr, na.rm = TRUE)
  }
  
  if ("DI" %in% names(df)) {
    metrics$n_missing_doi <- sum(is.na(df$DI) | df$DI == "")
    metrics$pct_with_doi <- 100 - (metrics$n_missing_doi / nrow(df) * 100)
  }
  
  # Check for duplicates
  if ("TI" %in% names(df)) {
    titles_norm <- tolower(trimws(df$TI))
    titles_norm <- gsub("[[:punct:]]", "", titles_norm)
    metrics$n_potential_duplicates <- sum(duplicated(titles_norm))
  }
  
  # Country extraction
  if ("C1" %in% names(df)) {
    countries <- m0_extract_countries(df$C1)
    metrics$n_docs_with_countries <- sum(sapply(countries, length) > 0)
    metrics$pct_with_countries <- metrics$n_docs_with_countries / nrow(df) * 100
  }
  
  list(
    metrics = metrics,
    issues = issues,
    summary = data.frame(
      check = names(metrics),
      value = unlist(metrics),
      stringsAsFactors = FALSE
    )
  )
}

#' Normalize citations for citation window bias
#'
#' Older papers have had more time to accumulate citations. This function
#' computes citations per year to enable fair comparison across papers
#' of different ages.
#'
#' @param df Bibliometric data frame with TC and PY columns
#' @param ref_year Reference year for normalization (default: current year)
#' @param method Normalization method: "per_year" (TC/years), "log" (log transform)
#' @return Data frame with additional TC_norm column
#' @export
m0_normalize_citations <- function(df, ref_year = NULL, method = "per_year") {
  if (!"TC" %in% names(df) || !"PY" %in% names(df)) {
    warning("m0_normalize_citations: TC or PY column not found, returning original data")
    return(df)
  }

  if (is.null(ref_year)) {
    ref_year <- as.integer(format(Sys.Date(), "%Y"))
  }

  df <- data.frame(df, stringsAsFactors = FALSE)

  tc <- suppressWarnings(as.numeric(df$TC))
  py <- suppressWarnings(as.integer(df$PY))

  years_since_pub <- ref_year - py
  years_since_pub <- pmax(years_since_pub, 1)

  df$TC_norm <- switch(method,
    per_year = {
      tc_adj <- ifelse(years_since_pub > 0, tc / years_since_pub, NA)
      tc_adj
    },
    log = {
      log1p(tc)
    },
    rank = {
      rank(-tc, ties.method = "average")
    },
    stop("Unknown normalization method: ", method)
  )

  df$TC_years_since_pub <- years_since_pub

  df
}

#' Remove outliers based on citation count
#'
#' @param df Bibliometric data frame
#' @param method Method: "iqr" or "zscore"
#' @param threshold Threshold for outlier detection
#' @return Filtered data frame
#' @export
m0_remove_citation_outliers <- function(df, method = "iqr", threshold = 3) {
  if (!"TC" %in% names(df)) return(df)
  
  tc <- df$TC[!is.na(df$TC)]
  
  if (method == "iqr") {
    q <- quantile(tc, c(0.25, 0.75))
    iqr <- q[2] - q[1]
    upper_bound <- q[2] + threshold * iqr
    df <- df[is.na(df$TC) | df$TC <= upper_bound, ]
  } else if (method == "zscore") {
    mean_tc <- mean(tc)
    sd_tc <- sd(tc)
    z <- (df$TC - mean_tc) / sd_tc
    df <- df[is.na(z) | abs(z) <= threshold, ]
  }
  
  df
}

#' Impute missing years
#'
#' @param df Bibliometric data frame
#' @param method "median" or "mode"
#' @return Data frame with imputed years
#' @export
m0_impute_years <- function(df, method = "median") {
  if (!"PY" %in% names(df)) return(df)
  
  missing <- is.na(df$PY)
  if (sum(missing) == 0) return(df)
  
  if (method == "median") {
    fill_value <- median(df$PY, na.rm = TRUE)
  } else {
    year_counts <- table(df$PY)
    fill_value <- as.numeric(names(year_counts)[which.max(year_counts)])
  }
  
  df$PY[missing] <- fill_value
  
  df
}

#' Standardize journal names
#'
#' @param journal_names Vector of journal names
#' @return Standardized journal names
#' @export
m0_standardize_journals <- function(journal_names) {
  if (is.null(journal_names)) return(character(0))
  
  journals <- toupper(trimws(as.character(journal_names)))
  
  # Remove common abbreviations and periodicals
  journals <- gsub("\\.$", "", journals)
  journals <- gsub("^THE\\s+", "", journals)
  journals <- gsub("\\s*\\(INTERNATIONAL\\)\\s*", " ", journals)
  journals <- gsub("\\s*INTERNATIONAL\\s*", " ", journals)
  journals <- gsub("\\s+JOURNAL\\s+", " J ", journals)
  journals <- gsub("^JOURNAL\\s+", "J ", journals)
  journals <- gsub("\\s+TRANSACTIONS\\s*", " TRANS ", journals)
  journals <- gsub("\\s+PROCEEDINGS\\s*", " PROC ", journals)
  journals <- gsub("\\s+REVIEW\\s*", " REV ", journals)
  journals <- gsub("\\s+LETTERS\\s*", " LETT ", journals)
  journals <- gsub("\\s+RESEARCH\\s*", " RES ", journals)
  journals <- gsub("\\s+", " ", journals)
  journals <- trimws(journals)
  
  journals
}

#' Detect and handle encoding issues
#'
#' @param text Character vector
#' @return Fixed character vector
#' @export
m0_fix_encoding <- function(text) {
  if (is.null(text)) return(character(0))
  
  text <- as.character(text)
  
  # Fix common encoding issues
  text <- gsub("Ã¡", "á", text, fixed = TRUE)
  text <- gsub("Ã©", "é", text, fixed = TRUE)
  text <- gsub("Ã­", "í", text, fixed = TRUE)
  text <- gsub("Ã³", "ó", text, fixed = TRUE)
  text <- gsub("Ãº", "ú", text, fixed = TRUE)
  text <- gsub("Ã±", "ñ", text, fixed = TRUE)
  text <- gsub("Ã¼", "ü", text, fixed = TRUE)
  text <- gsub("â€™", "'", text, fixed = TRUE)
  text <- gsub("â€œ", "\"", text, fixed = TRUE)
  text <- gsub("â€", "\"", text, fixed = TRUE)
  text <- gsub("â€“", "-", text, fixed = TRUE)
  text <- gsub("â€”", "—", text, fixed = TRUE)
  
  # Remove control characters
  text <- gsub("[[:cntrl:]]", "", text)
  
  # Fix whitespace
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  
  text
}

#' Create data quality report
#'
#' @param df Bibliometric data frame
#' @param output_file Path to save report
#' @return List with quality metrics
#' @export
m0_create_quality_report <- function(df, output_file = NULL) {
  quality <- m0_check_data_quality(df)
  
  report_lines <- c(
    "========================================",
    "        DATA QUALITY REPORT",
    "========================================",
    "",
    paste("Total documents:", nrow(df)),
    paste("Total columns:", ncol(df)),
    "",
    "--- Missing Values ---",
    ""
  )
  
  for (nm in names(quality$metrics)) {
    if (grepl("n_missing|n_invalid", nm)) {
      report_lines <- c(report_lines, paste0(nm, ": ", quality$metrics[[nm]]))
    }
  }
  
  report_lines <- c(report_lines, "", "--- Potential Issues ---", "")
  
  if (length(quality$issues) > 0) {
    for (issue_name in names(quality$issues)) {
      report_lines <- c(report_lines, paste0(issue_name, ": ", 
                                              paste(quality$issues[[issue_name]], collapse = ", ")))
    }
  } else {
    report_lines <- c(report_lines, "No major issues detected.")
  }
  
  report_lines <- c(report_lines, "", "--- Recommendations ---", "")
  
  # Add recommendations based on issues
  if (quality$metrics$n_missing_years > nrow(df) * 0.1) {
    report_lines <- c(report_lines, "Consider year imputation for missing values.")
  }
  if (quality$metrics$pct_with_doi < 50) {
    report_lines <- c(report_lines, "Low DOI coverage may affect deduplication accuracy.")
  }
  if (quality$metrics$n_potential_duplicates > nrow(df) * 0.05) {
    report_lines <- c(report_lines, "Consider enabling fuzzy deduplication.")
  }
  
  if (!is.null(output_file)) {
    writeLines(report_lines, output_file)
  }
  
  list(
    report_lines = report_lines,
    metrics = quality$metrics,
    issues = quality$issues
  )
}