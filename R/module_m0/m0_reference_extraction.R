# ============================================================================
# m0_reference_extraction.R - Bibliographic Reference Extraction
# ============================================================================
# Extract and parse references from bibliographic records

#' Extract references from bibliographic data
#'
#' @param input Bibliographic data frame (should have CR column)
#' @param config Configuration list
#' @return List with parsed references
#' @export
m0_extract_references <- function(input, config = biblio_config()) {
  ref_col <- "CR"
  if (!ref_col %in% names(input)) {
    return(list(
      references = list(),
      n_records_with_refs = 0,
      total_refs = 0,
      status = "warning: no reference column found"
    ))
  }
  
  refs_column <- input[[ref_col]]
  refs_column <- as.character(refs_column)
  
  # Parse references
  parsed_refs <- lapply(seq_along(refs_column), function(i) {
    if (is.na(refs_column[i]) || refs_column[i] == "") {
      return(list())
    }
    
    # Split by semicolon (common in Scopus/WoS)
    raw_refs <- strsplit(refs_column[i], ";")[[1]]
    raw_refs <- trimws(raw_refs)
    raw_refs <- raw_refs[nchar(raw_refs) > 10]  # Minimum length for valid ref
    
    # Parse each reference
    lapply(raw_refs, parse_single_reference)
  })
  
  names(parsed_refs) <- seq_len(length(parsed_refs))
  
  # Count references
  n_with_refs <- sum(sapply(parsed_refs, length) > 0)
  total_refs <- sum(sapply(parsed_refs, length))
  
  # Aggregate reference statistics
  ref_stats <- aggregate_reference_stats(parsed_refs)
  
  list(
    references = parsed_refs,
    n_records_with_refs = n_with_refs,
    total_refs = total_refs,
    reference_stats = ref_stats,
    status = "success"
  )
}

#' Parse single reference string
#' @keywords internal
parse_single_reference <- function(ref_string) {
  if (is.na(ref_string) || nchar(ref_string) < 10) {
    return(NULL)
  }
  
  ref_string <- trimws(ref_string)
  
  # Common patterns
  # Pattern 1: Author, Year, Title, Journal, Vol(Issue), Pages
  # Pattern 2: Author (Year) Title. Journal, Vol, Pages.
  # Pattern 3: Author. Title. Journal Year;Vol:Pages.
  
  parsed <- list(
    raw = ref_string,
    authors = NA_character_,
    year = NA_integer_,
    title = NA_character_,
    journal = NA_character_,
    volume = NA_character_,
    issue = NA_character_,
    pages = NA_character_,
    doi = NA_character_,
    pmid = NA_character_
  )
  
  # Extract year (4 digits)
  year_match <- regmatches(ref_string, regexpr("\\b(19|20)\\d{2}\\b", ref_string))
  if (length(year_match) > 0) {
    parsed$year <- as.integer(year_match[1])
  }
  
  # Extract DOI
  doi_match <- regmatches(ref_string, regexpr("10\\.[0-9]{4,}/[^\\s]+", ref_string))
  if (length(doi_match) > 0) {
    parsed$doi <- doi_match[1]
  }
  
  # Extract PMID
  pmid_match <- regmatches(ref_string, regexpr("PMID[:\\s]*(\\d+)", ref_string))
  if (length(pmid_match) > 0) {
    parsed$pmid <- gsub("PMID[:\\s]*", "", pmid_match[1])
  }
  
  # Extract pages
  pages_match <- regmatches(ref_string, regexpr("\\b\\d{1,4}[-\u2013]\\d{1,4}\\b", ref_string))
  if (length(pages_match) > 0) {
    parsed$pages <- pages_match[1]
  }
  
  # Extract volume
  vol_match <- regmatches(ref_string, regexpr("\\bV(\\d+)|\\b(\\d+)\\s*\\(", ref_string))
  if (length(vol_match) > 0) {
    parsed$volume <- gsub("[^0-9]", "", vol_match[1])
  }
  
  # Extract journal (often in italics or quoted)
  journal_patterns <- c(
    "([A-Z][A-Za-z\\s]+\\s*(?:Journal|Review|Letters|Proceedings|Transactions|Magazine|Quarterly))",
    "([A-Z][A-Za-z\\s]+\\s*(?:Academy|Society|Association)\\s+[A-Za-z]+)",
    "(PNAS|Nature|Science|Cell|Lancet|BMJ|JAMA|NEJM)"
  )
  
  for (p in journal_patterns) {
    journal_match <- regmatches(ref_string, regexpr(p, ref_string))
    if (length(journal_match) > 0) {
      parsed$journal <- journal_match[1]
      break
    }
  }
  
  # Extract authors (first part before year or title)
  author_patterns <- c(
    "^([A-Z][a-z]+,? [A-Z][A-Za-z\\-]+(?:,? [A-Z][A-Za-z\\-]+)*)",
    "^([A-Z][A-Za-z\\-]+(?:,? [A-Z][A-Za-z\\-]+)+(?:,? et al)?)"
  )
  
  for (p in author_patterns) {
    author_match <- regmatches(ref_string, regexpr(p, ref_string))
    if (length(author_match) > 0) {
      parsed$authors <- author_match[1]
      break
    }
  }
  
  parsed
}

#' Aggregate reference statistics
#' @keywords internal
aggregate_reference_stats <- function(parsed_refs) {
  # Flatten references
  all_refs <- unlist(parsed_refs, recursive = FALSE)
  all_refs <- all_refs[!sapply(all_refs, is.null)]
  
  if (length(all_refs) == 0) {
    return(list(
      n_total = 0,
      refs_with_year = 0,
      refs_with_doi = 0,
      refs_with_journal = 0,
      year_range = c(NA, NA),
      top_journals = character(0),
      top_authors = character(0)
    ))
  }
  
  # Count complete references
  n_with_year <- sum(!sapply(all_refs, function(r) is.null(r) || is.na(r$year)))
  n_with_doi <- sum(!sapply(all_refs, function(r) is.null(r) || is.na(r$doi)))
  n_with_journal <- sum(!sapply(all_refs, function(r) is.null(r) || is.na(r$journal)))
  
  # Year distribution
  years <- sapply(all_refs, function(r) if (!is.null(r)) r$year else NA)
  years <- years[!is.na(years)]
  
  # Journal frequency
  journals <- sapply(all_refs, function(r) if (!is.null(r)) r$journal else NA)
  journals <- journals[!is.na(journals) & journals != ""]
  journal_freq <- sort(table(journals), decreasing = TRUE)
  
  # Author frequency
  authors <- sapply(all_refs, function(r) if (!is.null(r)) r$authors else NA)
  authors <- authors[!is.na(authors) & authors != ""]
  author_freq <- sort(table(authors), decreasing = TRUE)
  
  list(
    n_total = length(all_refs),
    refs_with_year = n_with_year,
    refs_with_doi = n_with_doi,
    refs_with_journal = n_with_journal,
    year_range = if (length(years) > 0) range(years) else c(NA, NA),
    median_year = if (length(years) > 0) median(years) else NA,
    top_journals = names(journal_freq)[1:min(10, length(journal_freq))],
    top_authors = names(author_freq)[1:min(10, length(author_freq))],
    journal_counts = head(journal_freq, 20),
    author_counts = head(author_freq, 20)
  )
}

#' Calculate reference-based metrics
#'
#' @param references Parsed references from m0_extract_references
#' @return List with reference metrics
#' @export
m0_reference_metrics <- function(references) {
  if (is.null(references) || references$status != "success") {
    return(list(
      median_reference_age = NA,
      reference_age_distribution = numeric(0),
      price_index = NA,
      reference_diversity = NA,
      self_citation_rate = NA,
      status = "error"
    ))
  }
  
  all_refs <- unlist(references$references, recursive = FALSE)
  all_refs <- all_refs[!sapply(all_refs, is.null)]
  
  if (length(all_refs) == 0) {
    return(list(status = "error: no references"))
  }
  
  # Extract years
  ref_years <- sapply(all_refs, function(r) r$year)
  ref_years <- ref_years[!is.na(ref_years)]
  
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  # Reference age
  ref_ages <- current_year - ref_years
  median_age <- median(ref_ages, na.rm = TRUE)
  
  # Price Index (proportion of references < 5 years old)
  price_index <- mean(ref_ages < 5, na.rm = TRUE)
  
  # Reference diversity (unique journals / total refs)
  journals <- sapply(all_refs, function(r) r$journal)
  journals <- journals[!is.na(journals) & journals != ""]
  ref_diversity <- length(unique(journals)) / max(1, length(journals))
  
  # Reference half-life
  ref_ages_sorted <- sort(ref_ages)
  half_life_ind <- which(cumsum(rep(1, length(ref_ages_sorted))) / length(ref_ages_sorted) >= 0.5)[1]
  half_life <- if (!is.na(half_life_ind)) ref_ages_sorted[half_life_ind] else NA
  
  # Synonymy index (average references per citing document)
  avg_refs_per_doc <- length(all_refs) / length(references$references)
  
  list(
    median_reference_age = median_age,
    mean_reference_age = mean(ref_ages, na.rm = TRUE),
    reference_age_distribution = quantile(ref_ages, c(0.25, 0.5, 0.75)),
    price_index = price_index,
    reference_diversity = ref_diversity,
    reference_half_life = half_life,
    avg_refs_per_document = avg_refs_per_doc,
    n_unique_journals = length(unique(journals)),
    n_references = length(all_refs),
    reference_years = ref_years,
    status = "success"
  )
}

#' Calculate inter-rater reliability for screening
#'
#' @param ratings Matrix where rows are papers and columns are reviewers
#' @return List with Cohen's Kappa and other agreement measures
#' @export
m0_inter_rater_reliability <- function(ratings) {
  if (!is.matrix(ratings) && !is.data.frame(ratings)) {
    return(list(
      kappa = NA,
      percent_agreement = NA,
      status = "error: ratings must be matrix or data frame"
    ))
  }
  
  if (ncol(ratings) < 2) {
    return(list(
      kappa = NA,
      percent_agreement = NA,
      status = "error: need at least 2 reviewers"
    ))
  }
  
  ratings <- as.matrix(ratings)
  
  # Convert to binary if needed
  ratings <- if (is.factor(ratings[1, 1])) {
    ratings
  } else {
    as.matrix(ratings)
  }
  
  n <- nrow(ratings)
  
  if (n < 2) {
    return(list(
      kappa = NA,
      percent_agreement = NA,
      status = "error: need at least 2 papers"
    ))
  }
  
  # Pairwise Cohen's Kappa
  n_reviewers <- ncol(ratings)
  kappas <- numeric(n_reviewers * (n_reviewers - 1) / 2)
  idx <- 1
  
  for (i in 1:(n_reviewers - 1)) {
    for (j in (i + 1):n_reviewers) {
      kappas[idx] <- calculate_cohens_kappa(ratings[, i], ratings[, j])
      idx <- idx + 1
    }
  }
  
  # Overall percent agreement
  agree_count <- 0
  total_pairs <- 0
  
  for (i in 1:n) {
    for (r1 in 1:(n_reviewers - 1)) {
      for (r2 in (r1 + 1):n_reviewers) {
        if (!is.na(ratings[i, r1]) && !is.na(ratings[i, r2])) {
          total_pairs <- total_pairs + 1
          if (ratings[i, r1] == ratings[i, r2]) {
            agree_count <- agree_count + 1
          }
        }
      }
    }
  }
  
  percent_agreement <- if (total_pairs > 0) agree_count / total_pairs else NA
  
  # Fleiss' Kappa for multiple raters
  fleiss_kappa <- calculate_fleiss_kappa(ratings)
  
  list(
    cohens_kappa_mean = mean(kappas, na.rm = TRUE),
    cohens_kappa_range = range(kappas, na.rm = TRUE),
    fleiss_kappa = fleiss_kappa,
    percent_agreement = percent_agreement,
    n_papers = n,
    n_reviewers = n_reviewers,
    interpretation = interpret_kappa(mean(kappas, na.rm = TRUE)),
    status = "success"
  )
}

#' Calculate Cohen's Kappa
#' @keywords internal
calculate_cohens_kappa <- function(r1, r2) {
  # Remove NAs
  valid <- !is.na(r1) & !is.na(r2)
  r1 <- r1[valid]
  r2 <- r2[valid]
  
  if (length(r1) < 2) return(NA)
  
  # Contingency table
  categories <- sort(unique(c(r1, r2)))
  n_categories <- length(categories)
  
  contingency <- matrix(0, n_categories, n_categories)
  rownames(contingency) <- colnames(contingency) <- categories
  
  for (i in seq_along(r1)) {
    contingency[as.character(r1[i]), as.character(r2[i])] <- 
      contingency[as.character(r1[i]), as.character(r2[i])] + 1
  }
  
  n <- sum(contingency)
  
  # Observed agreement
  po <- sum(diag(contingency)) / n
  
  # Expected agreement
  row_marginals <- rowSums(contingency) / n
  col_marginals <- colSums(contingency) / n
  pe <- sum(row_marginals * col_marginals)
  
  # Kappa
  if (pe == 1) return(1)
  kappa <- (po - pe) / (1 - pe)
  
  kappa
}

#' Calculate Fleiss' Kappa
#' @keywords internal
calculate_fleiss_kappa <- function(ratings) {
  n <- nrow(ratings)
  m <- ncol(ratings)
  
  # Get unique categories
  all_cats <- unique(as.vector(ratings))
  all_cats <- all_cats[!is.na(all_cats)]
  k <- length(all_cats)
  
  if (k ==0 || n ==0) return(NA)
  
  # Calculate n_ij for each subject and category
  n_ij <- matrix(0, n, k)
  colnames(n_ij) <- all_cats
  
  for (i in 1:n) {
    for (cat in all_cats) {
      n_ij[i, as.character(cat)] <- sum(ratings[i, ] == cat, na.rm = TRUE)
    }
  }
  
  # P_i for each subject
  P_i <- (sum(n_ij^2) - n * m) / (m * (m - 1) * n / m)
  P_i <- rowSums(n_ij^2 - m) / (m * (m - 1))
  P_bar <- mean(P_i)
  
  # P_j for each category
  p_j <- colSums(n_ij) / (n * m)
  
  # P_e
  P_e <- sum(p_j^2)
  
  # Kappa
  if (P_e == 1) return(1)
  kappa <- (P_bar - P_e) / (1 - P_e)
  
  kappa
}

#' Interpret Kappa value
#' @keywords internal
interpret_kappa <- function(kappa) {
  if (is.na(kappa)) return("Cannot calculate")
  if (kappa < 0) return("Poor agreement")
  if (kappa < 0.20) return("Slight agreement")
  if (kappa < 0.40) return("Fair agreement")
  if (kappa < 0.60) return("Moderate agreement")
  if (kappa < 0.80) return("Substantial agreement")
  return("Almost perfect agreement")
}

#' Source coverage analysis
#'
#' @param input Bibliographic data frame
#' @param sources Character vector of source columns to analyze
#' @return List with coverage statistics
#' @export
m0_source_coverage <- function(input, sources = NULL) {
  if (is.null(sources)) {
    sources <- c("AU", "TI", "PY", "SO", "DI", "AB", "CR", "C1", "KW", "TC")
  }
  
  n_records <- nrow(input)
  
  coverage <- data.frame(
    field = sources,
    n_present = sapply(sources, function(col) {
      if (col %in% names(input)) {
        sum(!is.na(input[[col]]) & input[[col]] != "")
      } else {
        0
      }
    }),
    pct_present = NA,
    n_complete = NA,
    stringsAsFactors = FALSE
  )
  
  coverage$pct_present <- coverage$n_present / n_records * 100
  
  # Records with all fields complete
  complete_count <- sum(sapply(1:n_records, function(i) {
    all(sapply(sources, function(col) {
      if (col %in% names(input)) {
        !is.na(input[i, col]) && input[i, col] != ""
      } else {
        FALSE
      }
    }))
  }))
  
  coverage$n_complete <- complete_count
  
  # Field combinations
  combinations <- list()
  
  # Essential fields
  essential <- c("AU", "TI", "PY")
  essential_count <- sum(sapply(1:n_records, function(i) {
    all(sapply(essential, function(col) {
      if (col %in% names(input)) {
        !is.na(input[i, col]) && input[i, col] != ""
      } else {
        FALSE
      }
    }))
  }))
  
  combinations$essential <- essential_count
  
  # With DOI
  with_doi <- if ("DI" %in% names(input)) {
    sum(!is.na(input$DI) & input$DI != "")
  } else {
    0}
  
  # With abstract
  with_abstract <- if ("AB" %in% names(input)) {
    sum(!is.na(input$AB) & input$AB != "")
  } else {
    0
  }
  
  # With keywords
  with_keywords <- if ("KW" %in% names(input) || "DE" %in% names(input)) {
    kw_col <- if ("KW" %in% names(input)) "KW" else "DE"
    sum(!is.na(input[[kw_col]]) & input[[kw_col]] != "")
  } else {
    0
  }
  
  list(
    coverage = coverage,
    n_records = n_records,
    n_complete = complete_count,
    pct_complete = complete_count / n_records * 100,
    combinations = combinations,
    with_doi = with_doi,
    with_abstract = with_abstract,
    with_keywords = with_keywords,
    essential_coverage = essential_count / n_records * 100,
    status = "success"
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b