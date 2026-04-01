# ============================================================================
# m1_compute_hypotheses.R - Hypothesis Testing for M1
# ============================================================================
# Tests key bibliometric hypotheses for main information analysis

#' Compute hypothesis tests for M1
#'
#' Tests fundamental bibliometric hypotheses:
#' - Lotka's Law conformity
#' - Bradford's Law conformity
#' - Price's Law conformity
#' - Citation distribution patterns
#' - Collaboration patterns
#' - Concentration metrics
#'
#' @param input Bibliographic data frame
#' @param config Configuration list
#' @return List with hypothesis test results
#' @export
compute_m1_hypotheses <- function(input, config = biblio_config()) {
  validate_is_data_frame(input)
  
  hypotheses <- list()
  
  # H01.1: Author productivity follows Lotka's Law
  hypotheses$H01_1 <- test_lotka_law_hypothesis(input, config)
  
  # H01.2: Citation distribution follows power law
  hypotheses$H01_2 <- test_citation_power_law_hypothesis(input, config)
  
  # H01.3: Bradford's Law applies to source distribution
  hypotheses$H01_3 <- test_bradford_hypothesis(input, config)
  
  # H01.4: Collaboration rate has increased over time
  hypotheses$H01_4 <- test_collaboration_trend_hypothesis(input, config)
  
  # H01.5: Author productivity inequality follows Price's Law
  hypotheses$H01_5 <- test_price_law_hypothesis(input, config)
  
  # H01.6: Keywords follow Zipf's distribution
  hypotheses$H01_6 <- test_zipf_keywords_hypothesis(input, config)
  
  # H01.7: Citation distribution is highly skewed (Gini > 0.7)
  hypotheses$H01_7 <- test_citation_gini_hypothesis(input, config)
  
  # H01.8: Document types follow expected proportions for the field
  hypotheses$H01_8 <- test_document_type_hypothesis(input, config)
  
  # H01.9: International collaboration correlates with citation impact
  hypotheses$H01_9 <- test_intl_collab_citation_hypothesis(input, config)
  
  # H01.10: Source concentration follows Bradford zones (1/3 in core)
  hypotheses$H01_10 <- test_bradford_zones_hypothesis(input, config)
  
  # Additional tests if data permits
  hypotheses$H01_11 <- test_keyword_concentration_hypothesis(input, config)
  hypotheses$H01_12 <- test_author_dominance_hypothesis(input, config)
  
  summary_stats <- summarize_hypothesis_results(hypotheses)
  
  list(
    hypotheses = hypotheses,
    n_hypotheses = length(hypotheses),
    n_rejected = summary_stats$n_rejected,
    n_not_rejected = summary_stats$n_failed_to_reject,
    rejection_rate = summary_stats$rejection_rate,
    summary = summary_stats,
    status = "success"
  )
}

# ============================================================================
# Individual Hypothesis Tests
# ============================================================================

test_lotka_law_hypothesis <- function(input, config) {
  lotka_result <- tryCatch({
    compute_m1_lotka(input, config)
  }, error = function(e) NULL)
  
  if (is.null(lotka_result) || !is.list(lotka_result)) {
    return(list(
      hyphypothesis = "Author productivity follows Lotka's Law (alpha = 2)",
      null = "Author productivity does NOT follow Lotka's Law",
      result = "inconclusive",
      interpretation = "Could not compute Lotka's Law parameters"
    ))
  }
  
  if (is.null(lotka_result$status) || lotka_result$status != "success") {
    return(list(
      hyphypothesis = "Author productivity follows Lotka's Law (alpha = 2)",
      null = "Author productivity does NOT follow Lotka's Law",
      result = "inconclusive",
      interpretation = "Lotka computation was not successful"
    ))
  }
  
  # Safely extract lotka sub-list
  lotka <- lotka_result$lotka
  if (is.null(lotka) || !is.list(lotka)) {
    return(list(
      hyphypothesis = "Author productivity follows Lotka's Law (alpha = 2)",
      null = "Author productivity does NOT follow Lotka's Law",
      result = "inconclusive",
      interpretation = "Lotka results not available"
    ))
  }
  
  # Safely extract alpha
  alpha <- lotka$alpha
  if (is.null(alpha) || length(alpha) == 0) {
    alpha <- NA
  } else if (is.list(alpha)) {
    alpha <- NA
  } else {
    alpha <- as.numeric(alpha)[1]
  }
  
  if (is.na(alpha)) {
    return(list(
      hyphypothesis = "Author productivity follows Lotka's Law (alpha = 2)",
      null = "Author productivity does NOT follow Lotka's Law",
      result = "inconclusive",
      interpretation = "Could not estimate alpha parameter"
    ))
  }
  
  alpha_diff <- abs(alpha - 2)
  
  # Safely extract gof_pvalue
  gof_pvalue <- lotka$gof_pvalue
  if (is.null(gof_pvalue) || length(gof_pvalue) == 0 || is.list(gof_pvalue)) {
    gof_pvalue <- NA_real_
  } else {
    gof_pvalue <- as.numeric(gof_pvalue)[1]
  }
  
  result <- if (alpha_diff < 0.3 && (is.na(gof_pvalue) || gof_pvalue > 0.05)) {
    "fail_to_reject"
  } else {
    "reject"
  }
  
  pval_str <- if (is.na(gof_pvalue)) "NA" else sprintf("%.4f", gof_pvalue)
  
  interpretation <- sprintf(
    "Power-law exponent alpha = %.2f (classical = 2.0). %s K-S p-value = %s.",
    alpha,
    if (alpha_diff < 0.3) "Close to theoretical value." else "Deviates from theoretical value.",
    pval_str
  )
  
  # Safely extract other values
  gof_ks <- lotka$gof_ks
  if (is.null(gof_ks) || length(gof_ks) == 0 || is.list(gof_ks)) gof_ks <- NA_real_
  else gof_ks <- as.numeric(gof_ks)[1]
  
  n_authors <- lotka$n_authors
  if (is.null(n_authors) || length(n_authors) == 0 || is.list(n_authors)) n_authors <- NA_integer_
  else n_authors <- as.integer(n_authors)[1]
  
  list(
    hyphypothesis = "Author productivity follows Lotka's Law (alpha = 2)",
    null = "Author productivity does NOT follow Lotka's Law",
    result = result,
    alpha = alpha,
    classical_alpha = 2,
    alpha_difference = alpha_diff,
    ks_statistic = gof_ks,
    ks_pvalue = gof_pvalue,
    n_authors = n_authors,
    interpretation = interpretation
  )
}

test_citation_power_law_hypothesis <- function(input, config) {
  tc_col <- get_citation_column(input)
  if (is.null(tc_col)) {
    return(list(
      hyphypothesis = "Citation distribution follows power law",
      null = "Citations do not follow power law",
      result = "inconclusive",
      interpretation = "No citation data available"
    ))
  }
  
  citations <- input[[tc_col]]
  citations <- citations[!is.na(citations) & citations > 0]
  
  if (length(citations) < 50) {
    return(list(
      hyphypothesis = "Citation distribution follows power law",
      null = "Citations do not follow power law",
      result = "inconclusive",
      interpretation = "Insufficient citation data"
    ))
  }
  
  log_citations <- log(citations)
  fit <- lm(log_citations ~ seq_along(citations))
  r2 <- summary(fit)$r.squared
  
  sorted_cites <- sort(citations, decreasing = TRUE)
  rank <- seq_along(sorted_cites)
  
  log_fit <- lm(log(sorted_cites) ~ log(rank))
  power_exp <- coef(log_fit)[2]
  if (length(power_exp) > 1) power_exp <- power_exp[1]
  
  result <- if (abs(power_exp + 1) < 0.5) "fail_to_reject" else "reject"
  
  interpretation <- sprintf(
    "Fitted power exponent = %.2f (expected ~-1 for power law). R² = %.3f",
    as.numeric(power_exp)[1],
    r2[1]
  )
  
  list(
    hyphypothesis = "Citation distribution follows power law",
    null = "Citations do not follow power law",
    result = result,
    power_exponent = power_exp,
    r_squared = r2,
    n_citations = length(citations),
    interpretation = interpretation
  )
}

test_bradford_hypothesis <- function(input, config) {
  bradford_result <- tryCatch({
    compute_m1_bradford(input, config)
  }, error = function(e) NULL)
  
  if (is.null(bradford_result) || bradford_result$status != "success") {
    return(list(
      hyphypothesis = "Bradford's Law applies to source distribution",
      null = "Source distribution does not follow Bradford's Law",
      result = "inconclusive",
      interpretation = "Could not compute Bradford zones"
    ))
  }
  
  n_sources <- bradford_result$n_sources %||% NA
  bradford_coeff <- bradford_result$bradford_coefficient %||% NA
  
  # Ensure scalars
  if (length(n_sources) > 1) n_sources <- n_sources[1]
  if (length(bradford_coeff) > 1) bradford_coeff <- bradford_coeff[1]
  
  result <- if (!is.na(bradford_coeff[1]) && bradford_coeff[1] < 2) "fail_to_reject" else "reject"
  
  interpretation <- sprintf(
    "Bradford coefficient = %.2f. %s",
    as.numeric(bradford_coeff)[1],
    if (result == "fail_to_reject") "Distribution follows Bradford pattern."
    else "Distribution deviates from Bradford pattern."
  )
  
  list(
    hyphypothesis = "Bradford's Law applies to source distribution",
    null = "Source distribution does not follow Bradford's Law",
    result = result,
    bradford_coefficient = bradford_coeff[1],
    n_sources = n_sources[1],
    interpretation = interpretation
  )
}

test_collaboration_trend_hypothesis <- function(input, config) {
  collab_result <- tryCatch({
    compute_m1_collaboration(input, config)
  }, error = function(e) NULL)
  
  if (is.null(collab_result) || collab_result$status != "success") {
    return(list(
      hyphypothesis = "Collaboration rate has increased over time",
      null = "Collaboration rate is stable or decreasing",
      result = "inconclusive",
      interpretation = "Could not compute collaboration trends"
    ))
  }
  
  collaboration_by_year <- collab_result$by_year
  
  if (is.null(collaboration_by_year) || nrow(collaboration_by_year) < 5) {
    return(list(
      hyphypothesis = "Collaboration rate has increased over time",
      null = "Collaboration rate is stable or decreasing",
      result = "inconclusive",
      interpretation = "Insufficient temporal data"
    ))
  }
  
  # Check for required columns
  if (!"collaboration_rate" %in% names(collaboration_by_year)) {
    # Try to use alternative column names
    if ("mcp_ratio" %in% names(collaboration_by_year)) {
      collaboration_by_year$collaboration_rate <- collaboration_by_year$mcp_ratio
    } else {
      return(list(
        hyphypothesis = "Collaboration rate has increased over time",
        null = "Collaboration rate is stable or decreasing",
        result = "inconclusive",
        interpretation = "Collaboration rate data not available"
      ))
    }
  }
  
  # Check minimum sample size for regression
  if (nrow(collaboration_by_year) < 3) {
    return(list(
      hyphypothesis = "Collaboration rate has increased over time",
      null = "Collaboration rate is stable or decreasing",
      result = "inconclusive",
      interpretation = "Insufficient data points for trend analysis (need at least 3 years)"
    ))
  }
  
  fit <- lm(collaboration_rate ~ year, data = collaboration_by_year)
  
  # Check model convergence and coefficient availability
  if (is.null(fit) || length(coef(fit)) < 2 || is.na(coef(fit)[2])) {
    return(list(
      hyphypothesis = "Collaboration rate has increased over time",
      null = "Collaboration rate is stable or decreasing",
      result = "inconclusive",
      interpretation = "Could not fit regression model"
    ))
  }
  
  slope <- coef(fit)[2]
  p_value <- summary(fit)$coefficients[2, 4]
  
  # Ensure scalar values
  slope <- slope[1]
  p_value <- p_value[1]
  
  result <- if (slope > 0 && p_value < 0.05) "reject_null" 
            else if (slope <= 0 && p_value < 0.05) "reject"
            else "fail_to_reject"
  
  if (result == "reject_null") result <- "fail_to_reject"
  
  interpretation <- sprintf(
    "Collaboration trend slope = %.4f articles/year (p = %.4f). %s",
    slope, p_value,
    if (slope > 0) "Increasing trend detected." else "No significant increasing trend."
  )
  
  list(
    hyphypothesis = "Collaboration rate has increased over time",
    null = "Collaboration rate is stable or decreasing",
    result = result,
    slope = slope,
    p_value = p_value,
    interpretation = interpretation
  )
}

test_price_law_hypothesis <- function(input, config) {
  price_result <- tryCatch({
    compute_m1_price_law(input, config)
  }, error = function(e) NULL)
  
  if (is.null(price_result) || price_result$status != "success") {
    return(list(
      hyphypothesis = "Author productivity inequality follows Price's Law",
      null = "Productivity distribution does not follow Price's Law",
      result = "inconclusive",
      interpretation = "Could not compute Price's Law metrics"
    ))
  }
  
  price_index <- price_result$price_index$index %||% NA
  core_prop <- price_result$author_concentration$top_10_pct_share %||% NA
  
  # Ensure scalars
  if (length(price_index) > 1) price_index <- price_index[1]
  if (length(core_prop) > 1) core_prop <- core_prop[1]
  
  n_authors_val <- price_result$price_law$n_authors %||% 100
  if (length(n_authors_val) > 1) n_authors_val <- n_authors_val[1]
  expected_core_prop <- 1 / sqrt(n_authors_val)
  
  result <- if (!is.na(price_index[1]) && abs(price_index[1] - 0.5) < 0.2) "fail_to_reject" else "reject"
  
  interpretation <- sprintf(
    "Price index = %.2f (expected ~0.5). Core authors = %.1f%% (sqrt(N) = %.1f%%).",
    as.numeric(price_index)[1],
    as.numeric(core_prop)[1] * 100,
    as.numeric(expected_core_prop)[1] * 100
  )
  
  list(
    hyphypothesis = "Author productivity inequality follows Price's Law",
    null = "Productivity distribution does not follow Price's Law",
    result = result,
    price_index = price_index,
    core_author_proportion = core_prop,
    expected_core = expected_core_prop,
    interpretation = interpretation
  )
}

test_zipf_keywords_hypothesis <- function(input, config) {
  kw_cols <- c("KW", "DE", "ID", "Keywords")
  kw_col <- NULL
  for (col in kw_cols) {
    if (col %in% names(input)) {
      kw_col <- col
      break
    }
  }
  
  if (is.null(kw_col)) {
    return(list(
      hyphypothesis = "Keywords follow Zipf's distribution",
      null = "Keywords do not follow Zipf's distribution",
      result = "inconclusive",
      interpretation = "No keyword data available"
    ))
  }
  
  keywords <- unlist(strsplit(as.character(input[[kw_col]]), ";"))
  keywords <- trimws(keywords)
  keywords <- keywords[keywords != ""]
  
  if (length(keywords) < 50) {
    return(list(
      hyphypothesis = "Keywords follow Zipf's distribution",
      null = "Keywords do not follow Zipf's distribution",
      result = "inconclusive",
      interpretation = "Insufficient keyword data"
    ))
  }
  
  freq <- sort(table(keywords), decreasing = TRUE)
  rank <- seq_along(freq)
  
  log_fit <- lm(log(as.numeric(freq)) ~ log(rank))
  zipf_exp <- coef(log_fit)[2]
  if (length(zipf_exp) > 1) zipf_exp <- zipf_exp[1]
  
  result <- if (abs(zipf_exp + 1) < 0.5) "fail_to_reject" else "reject"
  
  interpretation <- sprintf(
    "Zipf exponent = %.2f (expected -1). %s",
    as.numeric(zipf_exp)[1],
    if (result == "fail_to_reject") "Distribution closely follows Zipf's Law."
    else "Distribution deviates from Zipf's Law."
  )
  
  list(
    hyphypothesis = "Keywords follow Zipf's distribution",
    null = "Keywords do not follow Zipf's distribution",
    result = result,
    zipf_exponent = zipf_exp,
    n_unique_keywords = length(freq),
    interpretation = interpretation
  )
}

test_citation_gini_hypothesis <- function(input, config) {
  tc_col <- get_citation_column(input)
  if (is.null(tc_col)) {
    return(list(
      hyphypothesis = "Citation distribution is highly concentrated (Gini > 0.7)",
      null = "Citations are relatively evenly distributed (Gini < 0.7)",
      result = "inconclusive",
      interpretation = "No citation data available"
    ))
  }
  
  citations <- input[[tc_col]]
  citations <- citations[!is.na(citations)]
  
  if (length(citations) < 20) {
    return(list(
      hyphypothesis = "Citation distribution is highly concentrated (Gini > 0.7)",
      null = "Citations are relatively evenly distributed (Gini < 0.7)",
      result = "inconclusive",
      interpretation = "Insufficient citation data"
    ))
  }
  
  citations <- sort(citations)
  n <- length(citations)
  index <- 2:n
  gini <- (n + 1 - 2 * sum((n + 1 - index) * citations[index]) / (n * sum(citations))) / n
  
  result <- if (gini > 0.7) "reject_null" else "fail_to_reject"
  if (result == "reject_null") result <- "reject" else result <- "fail_to_reject"
  
  interpretation <- sprintf(
    "Gini coefficient = %.3f. %s",
    gini,
    if (gini > 0.7) "High concentration detected (>0.7)."
    else "Moderate concentration detected."
  )
  
  list(
    hyphypothesis = "Citation distribution is highly concentrated (Gini > 0.7)",
    null = "Citations are relatively evenly distributed (Gini < 0.7)",
    result = result,
    gini = gini,
    threshold = 0.7,
    interpretation = interpretation
  )
}

test_document_type_hypothesis <- function(input, config) {
  dt_result <- tryCatch({
    compute_m1_doc_types(input, config)
  }, error = function(e) NULL)
  
  if (is.null(dt_result) || dt_result$status != "success") {
    return(list(
      hyphypothesis = "Document types follow expected proportions",
      null = "Document type distribution is unusual",
      result = "inconclusive",
      interpretation = "Could not analyze document types"
    ))
  }
  
  dt_table <- dt_result$doc_type_table
  
  if (is.null(dt_table) || nrow(dt_table) == 0) {
    return(list(
      hyphypothesis = "Document types follow expected proportions",
      null = "Document type distribution is unusual",
      result = "inconclusive",
      interpretation = "No document type data"
    ))
  }
  
  article_prop <- dt_table$proportion[dt_table$type == "Article"]
  if (length(article_prop) == 0) article_prop <- NA
  if (length(article_prop) > 1) article_prop <- article_prop[1]
  
  result <- if (!is.na(article_prop[1]) && article_prop[1] > 0.5) "fail_to_reject" else "reject"
  
  interpretation <- sprintf(
    "Articles = %.1f%% of documents. %s",
    as.numeric(article_prop)[1] * 100,
    if (result == "fail_to_reject") "Distribution appears normal."
    else "Unusual document type distribution."
  )
  
  list(
    hyphypothesis = "Document types follow expected proportions",
    null = "Document type distribution is unusual",
    result = result,
    article_proportion = article_prop,
    interpretation = interpretation
  )
}

test_intl_collab_citation_hypothesis <- function(input, config) {
  tc_col <- get_citation_column(input)
  c1_col <- "C1"
  
  if (!tc_col %in% names(input) || !c1_col %in% names(input)) {
    return(list(
      hyphypothesis = "International collaboration correlates with higher citations",
      null = "No relationship between collaboration and citations",
      result = "inconclusive",
      interpretation = "Insufficient data for collaboration-citation analysis"
    ))
  }
  
  citations <- input[[tc_col]]
  c1 <- input[[c1_col]]
  
  is_intl <- sapply(as.character(c1), function(x) {
    if (is.na(x) || x == "") return(NA)
    countries <- unique(extract_countries_from_c1(x))
    length(countries) > 1
  })
  
  valid_idx <- !is.na(is_intl) & !is.na(citations)
  if (sum(valid_idx) < 20) {
    return(list(
      hyphypothesis = "International collaboration correlates with higher citations",
      null = "No relationship between collaboration and citations",
      result = "inconclusive",
      interpretation = "Insufficient international collaboration data"
    ))
  }
  
  intl_cites <- citations[valid_idx & is_intl]
  domestic_cites <- citations[valid_idx & !is_intl]
  
  if (length(intl_cites) < 5 || length(domestic_cites) < 5) {
    return(list(
      hyphypothesis = "International collaboration correlates with higher citations",
      null = "No relationship between collaboration and citations",
      result = "inconclusive",
      interpretation = "Insufficient data in one group"
    ))
  }
  
  test_result <- tryCatch(wilcox.test(intl_cites, domestic_cites), error = function(e) NULL)
  
  mean_intl <- mean(intl_cites, na.rm = TRUE)
  mean_dom <- mean(domestic_cites, na.rm = TRUE)
  p_val <- if (!is.null(test_result) && !is.null(test_result$p.value)) test_result$p.value[1] else NA
  
  result <- if (!is.na(p_val) && mean_intl > mean_dom && p_val < 0.05) "fail_to_reject" else "reject"
  
  interpretation <- sprintf(
    "Intl collab mean citations = %.1f, Domestic = %.1f. Wilcox p = %.4f. %s",
    mean_intl, mean_dom, p_val,
    if (result == "fail_to_reject") "Intl collaboration associated with higher citations."
    else "No significant difference detected."
  )
  
  list(
    hyphypothesis = "International collaboration correlates with higher citations",
    null = "No relationship between collaboration and citations",
    result = result,
    mean_intl_citations = mean_intl,
    mean_domestic_citations = mean_dom,
    p_value = p_val,
    n_intl = length(intl_cites),
    n_domestic = length(domestic_cites),
    interpretation = interpretation
  )
}

test_bradford_zones_hypothesis <- function(input, config) {
  bradford_result <- tryCatch({
    compute_m1_bradford(input, config)
  }, error = function(e) NULL)
  
  if (is.null(bradford_result)) {
    return(list(
      hyphypothesis = "Core sources (top 1/3) contain 1/3 of articles",
      null = "Core sources do not contain 1/3 of articles",
      result = "inconclusive",
      interpretation = "Could not compute Bradford zones"
    ))
  }
  
  core_prop <- bradford_result$core_proportion %||% NA
  article_prop <- bradford_result$core_article_proportion %||% NA
  
  # Ensure scalars
  if (length(core_prop) > 1) core_prop <- core_prop[1]
  if (length(article_prop) > 1) article_prop <- article_prop[1]
  
  result <- if (!is.na(article_prop[1]) && abs(article_prop[1] - 1/3) < 0.15) "fail_to_reject" else "reject"
  
  interpretation <- sprintf(
    "Core sources (1/3) contain %.1f%% of articles (expected ~33%%).",
    as.numeric(article_prop)[1] * 100
  )
  
  list(
    hyphypothesis = "Core sources (top 1/3) contain 1/3 of articles",
    null = "Core sources do not contain 1/3 of articles",
    result = result,
    core_proportion = core_prop,
    core_article_proportion = article_prop,
    interpretation = interpretation
  )
}

test_keyword_concentration_hypothesis <- function(input, config) {
  kw_cols <- c("KW", "DE", "ID", "Keywords")
  kw_col <- NULL
  for (col in kw_cols) {
    if (col %in% names(input)) {
      kw_col <- col
      break
    }
  }
  
  if (is.null(kw_col)) {
    return(list(
      hyphypothesis = "Keywords are concentrated (top 20% = 80% of occurrences)",
      null = "Keywords are distributed more evenly",
      result = "inconclusive",
      interpretation = "No keyword data"
    ))
  }
  
  keywords <- unlist(strsplit(as.character(input[[kw_col]]), ";"))
  keywords <- trimws(keywords)
  keywords <- keywords[keywords != ""]
  
  if (length(keywords) < 50) {
    return(list(
      hyphypothesis = "Keywords are concentrated (top 20% = 80% of occurrences)",
      null = "Keywords are distributed more evenly",
      result = "inconclusive",
      interpretation = "Insufficient keywords"
    ))
  }
  
  freq <- sort(table(keywords), decreasing = TRUE)
  cumsum_freq <- cumsum(freq)
  total <- sum(freq)
  
  top_20_pct <- ceiling(length(freq) * 0.2)
  # Ensure index is within bounds
  top_20_pct <- min(top_20_pct, length(cumsum_freq))
  prop_top_20 <- cumsum_freq[top_20_pct] / total
  if (length(prop_top_20) > 1) prop_top_20 <- prop_top_20[1]
  
  result <- if (prop_top_20 > 0.7) "reject_null" else "fail_to_reject"
  if (result == "reject_null") result <- "reject" else result <- "fail_to_reject"
  
  interpretation <- sprintf(
    "Top 20%% keywords account for %.1f%% of occurrences.",
    as.numeric(prop_top_20)[1] * 100
  )
  
  list(
    hyphypothesis = "Keywords are concentrated (top 20% = 80% of occurrences)",
    null = "Keywords are distributed more evenly",
    result = result,
    top_20_prop = prop_top_20,
    n_unique = length(freq),
    interpretation = interpretation
  )
}

test_author_dominance_hypothesis <- function(input, config) {
  au_col <- "AU"
  if (!au_col %in% names(input)) {
    return(list(
      hyphypothesis = "Top 10% authors produce > 50% of articles",
      null = "Authorship is more distributed",
      result = "inconclusive",
      interpretation = "No author data"
    ))
  }
  
  authors <- unlist(strsplit(as.character(input[[au_col]]), ";"))
  authors <- trimws(authors)
  authors <- authors[authors != ""]
  
  if (length(authors) < 50) {
    return(list(
      hyphypothesis = "Top 10% authors produce > 50% of articles",
      null = "Authorship is more distributed",
      result = "inconclusive",
      interpretation = "Insufficient author data"
    ))
  }
  
  freq <- sort(table(authors), decreasing = TRUE)
  cumsum_freq <- cumsum(freq)
  total <- sum(freq)
  
  top_10_pct <- ceiling(length(freq) * 0.1)
  # Ensure index is within bounds
  top_10_pct <- min(top_10_pct, length(cumsum_freq))
  prop_top_10 <- cumsum_freq[top_10_pct] / total
  if (length(prop_top_10) > 1) prop_top_10 <- prop_top_10[1]
  
  result <- if (prop_top_10 > 0.4) "reject_null" else "fail_to_reject"
  if (result == "reject_null") result <- "reject" else result <- "fail_to_reject"
  
  interpretation <- sprintf(
    "Top 10%% authors produce %.1f%% of articles.",
    as.numeric(prop_top_10)[1] * 100
  )
  
  list(
    hyphypothesis = "Top 10% authors produce > 50% of articles",
    null = "Authorship is more distributed",
    result = result,
    top_10_prop = prop_top_10,
    n_unique = length(freq),
    interpretation = interpretation
  )
}

# ============================================================================
# Helper Functions
# ============================================================================

get_citation_column <- function(input) {
  tc_cols <- c("TC", "Times_Cited", "Citations", "citation_count")
  for (col in tc_cols) {
    if (col %in% names(input)) return(col)
  }
  NULL
}

extract_countries_from_c1 <- function(c1_string) {
  if (is.na(c1_string) || c1_string == "") return(character(0))
  affiliations <- strsplit(as.character(c1_string), ";")[[1]]
  countries <- gsub(".*;\\s*([^;]+)$", "\\1", affiliations)
  countries <- gsub(".*;\\s*([^,]+)$", "\\1", countries)
  countries <- trimws(countries)
  countries[countries != ""]
}

summarize_hypothesis_results <- function(hypotheses) {
  hyp_list <- hypotheses
  n_total <- length(hyp_list)
  n_rejected <- sum(vapply(hyp_list, function(h) isTRUE(h$result == "reject"), logical(1)), na.rm = TRUE)
  n_failed_to_reject <- sum(vapply(hyp_list, function(h) isTRUE(h$result == "fail_to_reject"), logical(1)), na.rm = TRUE)
  n_inconclusive <- n_total - n_rejected - n_failed_to_reject
  
  list(
    n_total = n_total,
    n_rejected = n_rejected,
    n_failed_to_reject = n_failed_to_reject,
    n_inconclusive = n_inconclusive,
    rejection_rate = if (n_total > 0) n_rejected / n_total else NA_real_
  )
}