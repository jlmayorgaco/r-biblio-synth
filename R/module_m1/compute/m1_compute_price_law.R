# ============================================================================
# m1_compute_price_law.R - Price's Law and Literature Half-Life Analysis
# ============================================================================
# Price's Law: Half of the significant contributions come from the square root
# of the number of authors (Price's square root law).
#
# Also computes:
# - Literature half-life (citations/age)
# - Price's Index (proportion of recent references)
# - Median citation age
# - Bibliometric aging indicators
#
# IEEE Q1 Enhancement: Comprehensive analysis of literature dynamics
# following Derek J. de Solla Price's foundational work.

#' Compute Price's Law and literature half-life analysis
#'
#' @param input Bibliographic data frame
#' @param config Configuration list
#' @return List with Price's Law results
#' @export
compute_m1_price_law <- function(input, config = biblio_config()) {
  validate_is_data_frame(input)
  
  tc_col <- "TC"
  py_col <- "PY"
  au_col <- "AU"
  if (!au_col %in% names(input)) au_col <- "AF"
  
  cr_col <- "CR"
  
  price_law <- compute_price_square_root_law(input, au_col)
  
  half_life <- compute_literature_half_life(input, tc_col, py_col)
  
  price_index <- if (cr_col %in% names(input)) {
    compute_price_index(input, cr_col, py_col)
  } else {
    list(index = NA_real_, n_references = 0, n_recent = 0, status = "no references column")
  }
  
  author_concentration <- compute_price_author_concentration(input, au_col)
  
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  summary_stats <- list(
    total_authors = price_law$n_authors,
    sqrt_authors = price_law$sqrt_n_authors,
    publications_top_sqrt = price_law$publications_top_sqrt,
    actual_proportion = price_law$actual_proportion,
    price_law_holds = price_law$price_law_holds,
    half_life = half_life$median_age,
    citation_half_life = half_life$citation_half_life,
    price_index = price_index$index,
    mean_citation_age = half_life$mean_citation_age,
    max_citation_age = half_life$max_citation_age,
    interpretation = interpret_price_results(price_law, half_life, price_index)
  )
  
  list(
    price_law = price_law,
    half_life = half_life,
    price_index = price_index,
    author_concentration = author_concentration,
    summary = summary_stats,
    status = "success"
  )
}

#' Compute Price's square root law for authors
#' @keywords internal
compute_price_square_root_law <- function(input, au_col) {
  if (!au_col %in% names(input)) {
    return(list(
      n_authors = 0,
      sqrt_n_authors = 0,
      publications_top_sqrt = 0,
      actual_proportion = NA_real_,
      price_law_holds = NA,
      expected_proportion = 0.5,
      author_distribution = data.frame()
    ))
  }
  
  authors_list <- strsplit(as.character(input[[au_col]]), ";")
  authors_list <- lapply(authors_list, trimws)
  authors_list <- lapply(authors_list, function(x) x[x != ""])
  
  author_counts <- table(unlist(authors_list))
  
  author_df <- data.frame(
    author = names(author_counts),
    n_papers = as.integer(author_counts),
    stringsAsFactors = FALSE
  )
  
  author_df <- author_df[order(-author_df$n_papers), ]
  
  n_authors <- nrow(author_df)
  if (n_authors == 0) {
    return(list(
      n_authors = 0,
      sqrt_n_authors = 0,
      publications_top_sqrt = 0,
      actual_proportion = NA_real_,
      price_law_holds = NA,
      expected_proportion = 0.5,
      author_distribution = author_df
    ))
  }
  
  sqrt_n_authors <- ceiling(sqrt(n_authors))
  
  total_papers <- author_df$n_papers[1]
  for (i in 2:n_authors) {
    total_papers <- total_papers + author_df$n_papers[i]
  }
  
  top_sqrt_papers <- 0
  for (i in 1:min(sqrt_n_authors, n_authors)) {
    top_sqrt_papers <- top_sqrt_papers + author_df$n_papers[i]
  }
  
  actual_proportion <- top_sqrt_papers / total_papers
  
  expected_proportion <- 0.5
  
  price_law_holds <- actual_proportion >= 0.4 && actual_proportion <= 0.6
  
  list(
    n_authors = n_authors,
    sqrt_n_authors = sqrt_n_authors,
    publications_top_sqrt = sum(author_df$n_papers[1:min(sqrt_n_authors, n_authors)]),
    actual_proportion = actual_proportion,
    price_law_holds = price_law_holds,
    expected_proportion = expected_proportion,
    deviation_from_expected = actual_proportion - expected_proportion,
    top_authors = head(author_df, min(10, n_authors)),
    author_distribution = author_df
  )
}

#' Compute literature half-life
#' @keywords internal
compute_literature_half_life <- function(input, tc_col, py_col) {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  if (!tc_col %in% names(input) || !py_col %in% names(input)) {
    return(list(
      median_age = NA_real_,
      mean_citation_age = NA_real_,
      citation_half_life = NA_real_,
      max_citation_age = NA_real_
    ))
  }
  
  citations <- as.numeric(input[[tc_col]])
  years <- as.numeric(input[[py_col]])
  
  valid_idx <- !is.na(citations) & !is.na(years) & years > 1900 & years <= current_year
  
  if (sum(valid_idx) == 0) {
    return(list(
      median_age = NA_real_,
      mean_citation_age = NA_real_,
      citation_half_life = NA_real_,
      max_citation_age = NA_real_
    ))
  }
  
  ages <- current_year - years[valid_idx]
  valid_citations <- citations[valid_idx]
  
  median_age <- median(ages, na.rm = TRUE)
  mean_age <- mean(ages, na.rm = TRUE)
  max_age <- max(ages, na.rm = TRUE)
  
  citation_half_life <- compute_citation_half_life(valid_citations, ages)
  
  list(
    median_age = median_age,
    mean_citation_age = mean_age,
    citation_half_life = citation_half_life,
    max_citation_age = max_age,
    age_distribution = data.frame(
      age = ages,
      citations = valid_citations
    )
  )
}

#' Compute citation half-life
#' @keywords internal
compute_citation_half_life <- function(citations, ages) {
  if (length(citations) == 0 || sum(citations, na.rm = TRUE) == 0) {
    return(NA_real_)
  }
  
  cum_citations <- cumsum(sort(citations, decreasing = TRUE))
  total_citations <- sum(citations, na.rm = TRUE)
  half_citations <- total_citations / 2
  
  half_idx <- which(cum_citations >= half_citations)[1]
  
  if (is.na(half_idx)) return(NA_real_)
  
  sorted_ages <- sort(ages, decreasing = FALSE)
  sorted_citations <- sort(citations, decreasing = FALSE)
  
  cumulative_citations <- cumsum(sorted_citations)
  
  sorted_ages[cumulative_citations >= half_citations][1]
}

#' Compute Price's Index
#' @keywords internal
compute_price_index <- function(input, cr_col, py_col) {
  if (!cr_col %in% names(input)) {
    return(list(
      index = NA_real_,
      n_references = 0,
      n_recent = 0,
      status = "no references column"
    ))
  }
  
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  references <- input[[cr_col]]
  pub_years <- input[[py_col]]
  
  valid_idx <- !is.na(pub_years) & !is.na(references) & references != ""
  
  if (length(valid_idx) == 0 || sum(valid_idx) == 0) {
    return(list(
      index = NA_real_,
      n_references = 0,
      n_recent = 0,
      status = "no valid references"
    ))
  }
  
  ref_years <- extract_reference_years(references[valid_idx])
  
  n_refs <- length(ref_years)
  if (n_refs == 0) {
    return(list(
      index = NA_real_,
      n_references = 0,
      n_recent = 0,
      status = "could not extract reference years"
    ))
  }
  
  n_recent <- sum(ref_years >= (current_year - 5), na.rm = TRUE)
  
  price_index <- n_recent / n_refs
  
  list(
    index = price_index,
    n_references = n_refs,
    n_recent = n_recent,
    recency_window = 5,
    status = "success"
  )
}

#' Extract publication years from references
#' @keywords internal
extract_reference_years <- function(references) {
  years <- lapply(references, function(ref_string) {
    if (is.na(ref_string) || ref_string == "") return(integer(0))
    
    year_matches <- regmatches(ref_string, gregexpr("\\b(19|20)\\d{2}\\b", ref_string))
    
    as.integer(unlist(year_matches))
  })
  
  unlist(years)
}

#' Compute Price's author concentration
#' @keywords internal
compute_price_author_concentration <- function(input, au_col) {
  if (!au_col %in% names(input)) {
    return(list(
      gini = NA_real_,
      top_1_pct_share = NA_real_,
      top_5_pct_share = NA_real_,
      top_10_pct_share = NA_real_,
      entropy = NA_real_,
      concentration_type = NA_character_
    ))
  }
  
  authors_list <- strsplit(as.character(input[[au_col]]), ";")
  authors_list <- lapply(authors_list, trimws)
  authors_list <- lapply(authors_list, function(x) x[x != ""])
  
  author_counts <- table(unlist(authors_list))
  sorted_counts <- sort(author_counts, decreasing = TRUE)
  n_authors <- length(sorted_counts)
  total_papers <- sum(sorted_counts)
  
  if (n_authors == 0 || total_papers == 0) {
    return(list(
      gini = NA_real_,
      top_1_pct_share = NA_real_,
      top_5_pct_share = NA_real_,
      top_10_pct_share = NA_real_,
      entropy = NA_real_,
      concentration_type = NA_character_
    ))
  }
  
  gini <- compute_gini(sorted_counts)
  
  top_1_pct <- max(1, floor(n_authors * 0.01))
  top_5_pct <- max(1, floor(n_authors * 0.05))
  top_10_pct <- max(1, floor(n_authors * 0.10))
  
  top_1_pct_share <- sum(sorted_counts[1:top_1_pct]) / total_papers
  top_5_pct_share <- sum(sorted_counts[1:top_5_pct]) / total_papers
  top_10_pct_share <- sum(sorted_counts[1:top_10_pct]) / total_papers
  
  proportions <- sorted_counts / total_papers
  entropy <- -sum(proportions * log2(proportions + 1e-10))
  
  max_entropy <- log2(n_authors)
  normalized_entropy <- entropy / max_entropy
  
  if (top_10_pct_share > 0.5) {
    concentration_type <- "highly concentrated"
  } else if (top_10_pct_share > 0.3) {
    concentration_type <- "moderately concentrated"
  } else {
    concentration_type <- "evenly distributed"
  }
  
  list(
    gini = gini,
    top_1_pct_share = top_1_pct_share,
    top_5_pct_share = top_5_pct_share,
    top_10_pct_share = top_10_pct_share,
    entropy = entropy,
    normalized_entropy = normalized_entropy,
    concentration_type = concentration_type
  )
}

#' Compute Gini coefficient
#' @keywords internal
compute_gini <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  n <- length(x)
  
  if (n == 0 || sum(x) == 0) return(NA_real_)
  
  x <- sort(x)
  
  n <- length(x)
  cum_x <- cumsum(x)
  area_under_curve <- sum(cum_x) / (n * sum(x))
  gini <- 1 - 2 * area_under_curve
  
  gini
}

#' Interpret Price's Law results
#' @keywords internal
interpret_price_results <- function(price_law, half_life, price_index) {
  interpretations <- character(0)
  
  if (!is.na(price_law$price_law_holds) && price_law$price_law_holds) {
    interpretations <- c(interpretations, 
      sprintf("Price's Law holds: top sqrt(n)=%.0f authors account for %.1f%% of publications.",
              price_law$sqrt_n_authors, price_law$actual_proportion * 100))
  } else if (!is.na(price_law$actual_proportion)) {
    if (price_law$actual_proportion > 0.6) {
      interpretations <- c(interpretations,
        sprintf("Higher concentration than Price's Law predicts: top sqrt(n)=%.0f authors account for %.1f%% of publications (> 50%%).",
                price_law$sqrt_n_authors, price_law$actual_proportion * 100))
    } else {
      interpretations <- c(interpretations,
        sprintf("Lower concentration than Price's Law predicts: top sqrt(n)=%.0f authors account for %.1f%% of publications (< 50%%).",
                price_law$sqrt_n_authors, price_law$actual_proportion * 100))
    }
  }
  
  if (!is.na(half_life$median_age)) {
    interpretations <- c(interpretations,
      sprintf("Literature half-life: median citation age is %.1f years.", half_life$median_age))
  }
  
  if (!is.na(price_index$index) && price_index$index > 0) {
    interpretations <- c(interpretations,
      sprintf("Price's Index: %.1f%% of references are from the last 5 years.", price_index$index * 100))
  }
  
  paste(interpretations, collapse = " ")
}