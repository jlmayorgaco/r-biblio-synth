# ============================================================================
# m3_compute_hypotheses.R - Hypothesis Testing for M3 (Countries)
# ============================================================================
# Tests key hypotheses about country production patterns
# Focuses on: growth/decline patterns, concentration, relationships

#' Compute hypothesis tests for M3
#'
#' @param prepared_data Output from prepare_m3_country_data
#' @param config Configuration list
#' @return List with hypothesis test results
#' @export
m3_compute_hypotheses <- function(prepared_data, config = biblio_config()) {
  if (is.null(prepared_data) || prepared_data$status != "success") {
    return(list(
      hyphypotheses = list(),
      status = "error: invalid prepared_data"
    ))
  }
  
  hypotheses <- list()
  
  # H03.1: Research interest is increasing in ALL countries
  hypotheses$H03_1 <- test_universal_growth_hypothesis(prepared_data, config)
  
  # H03.2: No difference in growth patterns between countries
  hypotheses$H03_2 <- test_growth_equality_hypothesis(prepared_data, config)
  
  # H03.3: Production concentration is stable over time
  hypotheses$H03_3 <- test_concentration_stability_hypothesis(prepared_data, config)
  
  # H03.4: International collaboration is uniformly distributed
  hypotheses$H03_4 <- test_collab_distribution_hypothesis(prepared_data, config)
  
  # H03.5: GDP correlates with publication output (uses proxy)
  hypotheses$H03_5 <- test_size_output_correlation_hypothesis(prepared_data, config)
  
  # H03.6: Citation impact correlates with production volume
  hypotheses$H03_6 <- test_production_citation_hypothesis(prepared_data, config)
  
  # H03.7: Leading countries maintain dominance over time
  hypotheses$H03_7 <- test_leader_persistence_hypothesis(prepared_data, config)
  
  # H03.8: New entrants have different growth patterns
  hypotheses$H03_8 <- test_entrant_growth_hypothesis(prepared_data, config)
  
  # H03.9: SCP/MCP ratio is consistent across countries
  hypotheses$H03_9 <- test_scp_mcp_consistency_hypothesis(prepared_data, config)
  
  # H03.10: Geographic distance affects collaboration strength
  hypotheses$H03_10 <- test_distance_collab_hypothesis(prepared_data, config)
  
  # H03.11: Research topic declining in some countries
  hypotheses$H03_11 <- test_declining_countries_hypothesis(prepared_data, config)
  
  # H03.12: Growth clusters (bunches) exist
  hypotheses$H03_12 <- test_growth_clusters_hypothesis(prepared_data, config)
  
  summary_stats <- summarize_h3_hypothesis_results(hypotheses)
  
  list(
    hyphypotheses = hypotheses,
    n_hypotheses = length(hypotheses),
    n_rejected = summary_stats$n_rejected,
    n_not_rejected = summary_stats$n_failed_to_reject,
    rejection_rate = summary_stats$rejection_rate,
    summary = summary_stats,
    status = "success"
  )
}

# Individual tests

test_universal_growth_hypothesis <- function(prepared_data, config) {
  country_summary <- prepared_data$country_summary
  
  if (is.null(country_summary) || nrow(country_summary) == 0) {
    return(list(
      hyphypothesis = "All countries show increasing research interest",
      null = "Not all countries have increasing production",
      result = "inconclusive",
      interpretation = "No country summary data available"
    ))
  }
  
  n_countries <- nrow(country_summary)
  n_increasing <- sum(country_summary$trend_direction == "increasing", na.rm = TRUE)
  n_declining <- sum(country_summary$trend_direction == "decreasing", na.rm = TRUE)
  n_stable <- sum(country_summary$trend_direction == "stable", na.rm = TRUE)
  
  prop_increasing <- n_increasing / n_countries
  
  result <- if (n_increasing == n_countries) "fail_to_reject" else "reject"
  
  interpretation <- sprintf(
    "Only %d/%d (%.1f%%) countries show increasing production. %d declining, %d stable.",
    n_increasing, n_countries, prop_increasing * 100, n_declining, n_stable
  )
  
  list(
    hyphypothesis = "All countries show increasing research interest",
    null = "Not all countries have increasing production",
    result = result,
    n_total = n_countries,
    n_increasing = n_increasing,
    n_declining = n_declining,
    n_stable = n_stable,
    proportion_increasing = prop_increasing,
    increasing_countries = country_summary$country[country_summary$trend_direction == "increasing"],
    declining_countries = country_summary$country[country_summary$trend_direction == "decreasing"],
    interpretation = interpretation
  )
}

test_growth_equality_hypothesis <- function(prepared_data, config) {
  country_summary <- prepared_data$country_summary
  
  if (is.null(country_summary) || nrow(country_summary) < 4) {
    return(list(
      hyphypothesis = "No difference in growth patterns between countries",
      null = "Growth patterns differ significantly",
      result = "inconclusive",
      interpretation = "Insufficient country data"
    ))
  }
  
  growth_rates <- country_summary$annual_growth_rate
  growth_rates <- growth_rates[!is.na(growth_rates)]
  
  if (length(growth_rates) < 4) {
    return(list(
      hyphypothesis = "No difference in growth patterns between countries",
      null = "Growth patterns differ significantly",
      result = "inconclusive",
      interpretation = "Insufficient growth data"
    ))
  }
  
  cv <- sd(growth_rates) / abs(mean(growth_rates))
  
  # Kolmogorov-Smirnov test against normal
  ks_test <- tryCatch(ks.test(growth_rates, "pnorm", mean = mean(growth_rates), sd = sd(growth_rates)),
                     error = function(e) NULL)
  
  result <- if (cv < 0.5) "fail_to_reject" else "reject"
  
  interpretation <- sprintf(
    "Growth rate CV = %.2f. %s variation in growth patterns.",
    cv,
    if (cv < 0.5) "Low" else "High"
  )
  
  list(
    hyphypothesis = "No difference in growth patterns between countries",
    null = "Growth patterns differ significantly",
    result = result,
    CV = cv,
    ks_pvalue = if (!is.null(ks_test)) ks_test$p.value else NA,
    mean_growth = mean(growth_rates),
    sd_growth = sd(growth_rates),
    interpretation = interpretation
  )
}

test_concentration_stability_hypothesis <- function(prepared_data, config) {
  gini_over_time <- prepared_data$gini_over_time
  
  if (is.null(gini_over_time) || length(gini_over_time) < 5) {
    return(list(
      hyphypothesis = "Production concentration is stable over time",
      null = "Concentration is changing",
      result = "inconclusive",
      interpretation = "Insufficient temporal data"
    ))
  }
  
  years <- as.numeric(names(gini_over_time))
  gini_vals <- as.numeric(gini_over_time)
  
  fit <- tryCatch(lm(gini_vals ~ years), error = function(e) NULL)
  if (is.null(fit)) return(list(hyphypothesis = "Concentration stability", result = "inconclusive"))
  
  slope <- coef(fit)[2]
  p_value <- summary(fit)$coefficients[2, 4]
  
  result <- if (abs(slope) < 0.01 && p_value > 0.05) "fail_to_reject" else "reject"
  
  interpretation <- sprintf(
    "Gini trend slope = %.4f/year (p = %.4f). %s",
    slope, p_value,
    if (result == "fail_to_reject") "Concentration is stable." else "Concentration is changing."
  )
  
  list(
    hyphypothesis = "Production concentration is stable over time",
    null = "Concentration is changing",
    result = result,
    slope = slope,
    p_value = p_value,
    gini_start = gini_vals[1],
    gini_end = gini_vals[length(gini_vals)],
    interpretation = interpretation
  )
}

test_collab_distribution_hypothesis <- function(prepared_data, config) {
  collab_data <- prepared_data$collaboration_indices
  
  if (is.null(collab_data) || nrow(collab_data) < 5) {
    return(list(
      hyphypothesis = "International collaboration is uniformly distributed",
      null = "Collaboration is concentrated",
      result = "inconclusive",
      interpretation = "Insufficient collaboration data"
    ))
  }
  
  collab_centrality <- collab_data$collaboration_centrality
  
  gini <- calculate_gini(collab_centrality)
  
  result <- if (gini < 0.4) "fail_to_reject" else "reject"
  
  interpretation <- sprintf(
    "Collaboration centrality Gini = %.3f. %s",
    gini,
    if (gini < 0.4) "Relatively uniform distribution." else "Concentrated in few countries."
  )
  
  list(
    hyphypothesis = "International collaboration is uniformly distributed across countries",
    null = "Collaboration is concentrated in specific countries",
    result = result,
    gini = gini,
    top_5_share = sum(sort(collab_centrality, decreasing = TRUE)[1:5]) / sum(collab_centrality),
    interpretation = interpretation
  )
}

test_size_output_correlation_hypothesis <- function(prepared_data, config) {
  country_summary <- prepared_data$country_summary
  
  if (is.null(country_summary) || nrow(country_summary) < 5) {
    return(list(
      hyphypothesis = "Country size correlates with publication output",
      null = "No correlation between size and output",
      result = "inconclusive",
      interpretation = "Insufficient data"
    ))
  }
  
  # Use total production as size proxy since GDP isn't available
  # This tests whether past productivity correlates with current
  
  total_articles <- country_summary$total_articles
  mean_citations <- country_summary$mean_citations
  
  valid_idx <- !is.na(total_articles) & !is.na(mean_citations) & total_articles > 0 & mean_citations > 0
  
  if (sum(valid_idx) < 5) {
    return(list(
      hyphypothesis = "Country size correlates with publication output",
      null = "No correlation",
      result = "inconclusive",
      interpretation = "Insufficient valid data"
    ))
  }
  
  # Correlation between log(articles) and citations
  cor_test <- tryCatch(cor.test(log(total_articles[valid_idx]), 
                                mean_citations[valid_idx],
                                method = "spearman"),
                      error = function(e) NULL)
  
  if (is.null(cor_test)) return(list(hyphypothesis = "Size-output correlation", result = "inconclusive"))
  
  result <- if (cor_test$p.value > 0.05) "fail_to_reject" else "reject"
  
  list(
    hyphypothesis = "Country production volume correlates with citation impact",
    null = "No correlation between volume and impact",
    result = result,
    correlation = cor_test$estimate,
    p_value = cor_test$p.value,
    interpretation = sprintf("Spearman rho = %.3f (p = %.4f). %s",
                            cor_test$estimate, cor_test$p.value,
                            if (result == "fail_to_reject") "No significant correlation." else "Significant correlation.")
  )
}

test_production_citation_hypothesis <- function(prepared_data, config) {
  country_summary <- prepared_data$country_summary
  
  if (is.null(country_summary) || nrow(country_summary) < 5) {
    return(list(
      hyphypothesis = "Production volume correlates with citation impact",
      null = "No correlation",
      result = "inconclusive",
      interpretation = "Insufficient data"
    ))
  }
  
  total_articles <- country_summary$total_articles
  total_citations <- country_summary$total_citations
  
  valid_idx <- !is.na(total_articles) & !is.na(total_citations) & total_articles > 0
  
  if (sum(valid_idx) < 5) {
    return(list(hyphypothesis = "Production-citation correlation", result = "inconclusive"))
  }
  
  cor_test <- tryCatch(cor.test(log(total_articles[valid_idx]),
                                log(total_citations[valid_idx]),
                                method = "spearman"),
                      error = function(e) NULL)
  
  if (is.null(cor_test)) return(list(hyphypothesis = "Production-citation correlation", result = "inconclusive"))
  
  result <- if (cor_test$p.value > 0.05) "fail_to_reject" else "reject"
  
  list(
    hyphypothesis = "Higher production correlates with higher citation impact",
    null = "No correlation between production and impact",
    result = result,
    correlation = cor_test$estimate,
    p_value = cor_test$p.value,
    interpretation = sprintf("Spearman rho = %.3f (p = %.4f)",
                            cor_test$estimate, cor_test$p.value)
  )
}

test_leader_persistence_hypothesis <- function(prepared_data, config) {
  country_year_prod <- prepared_data$country_year_production
  
  if (is.null(country_year_prod) || nrow(country_year_prod) < 10) {
    return(list(
      hyphypothesis = "Leading countries maintain dominance over time",
      null = "Leadership changes over time",
      result = "inconclusive",
      interpretation = "Insufficient temporal data"
    ))
  }
  
  years <- sort(unique(country_year_prod$year))
  if (length(years) < 3) {
    return(list(hyphypothesis = "Leader persistence", result = "inconclusive"))
  }
  
  first_year <- years[1]
  last_year <- years[length(years)]
  
  first_data <- country_year_prod[country_year_prod$year == first_year, ]
  last_data <- country_year_prod[country_year_prod$year == last_year, ]
  
  top_5_first <- head(first_data[order(-first_data$n_articles), "country"], 5)
  top_5_last <- head(last_data[order(-last_data$n_articles), "country"], 5)
  
  overlap <- length(intersect(top_5_first, top_5_last))
  
  result <- if (overlap >= 3) "fail_to_reject" else "reject"
  
  list(
    hyphypothesis = "Leading countries maintain dominance over time",
    null = "Leadership changes over time",
    result = result,
    top_5_first_year = top_5_first,
    top_5_last_year = top_5_last,
    n_persisting = overlap,
    persistence_rate = overlap / 5,
    interpretation = sprintf("%d/5 top countries remained in top 5 over entire period.", overlap)
  )
}

test_entrant_growth_hypothesis <- function(prepared_data, config) {
  country_summary <- prepared_data$country_summary
  
  if (is.null(country_summary) || nrow(country_summary) < 5) {
    return(list(
      hyphypothesis = "New entrants have different growth patterns",
      null = "No difference between new and established countries",
      result = "inconclusive",
      interpretation = "Insufficient data"
    ))
  }
  
  established <- country_summary[country_summary$n_years >= median(country_summary$n_years), ]
  new_entrants <- country_summary[country_summary$n_years < median(country_summary$n_years), ]
  
  if (nrow(established) < 3 || nrow(new_entrants) < 3) {
    return(list(hyphypothesis = "Entrant growth patterns", result = "inconclusive"))
  }
  
  established_growth <- established$annual_growth_rate[!is.na(established$annual_growth_rate)]
  new_growth <- new_entrants$annual_growth_rate[!is.na(new_entrants$annual_growth_rate)]
  
  if (length(established_growth) < 2 || length(new_growth) < 2) {
    return(list(hyphypothesis = "Entrant growth patterns", result = "inconclusive"))
  }
  
  t_test <- tryCatch(t.test(established_growth, new_growth), error = function(e) NULL)
  
  if (is.null(t_test)) return(list(hyphypothesis = "Entrant growth patterns", result = "inconclusive"))
  
  result <- if (t_test$p.value > 0.05) "fail_to_reject" else "reject"
  
  list(
    hyphypothesis = "New entrants have different growth patterns than established countries",
    null = "No difference in growth patterns",
    result = result,
    established_mean_growth = mean(established_growth),
    new_entrant_mean_growth = mean(new_growth),
    p_value = t_test$p.value,
    interpretation = sprintf("Established: %.1f%% growth, New entrants: %.1f%% growth (p=%.4f)",
                            mean(established_growth) * 100, mean(new_growth) * 100, t_test$p.value)
  )
}

test_scp_mcp_consistency_hypothesis <- function(prepared_data, config) {
  scp_mcp <- prepared_data$scp_mcp_by_country
  
  if (is.null(scp_mcp) || nrow(scp_mcp) < 5) {
    return(list(
      hyphypothesis = "SCP/MCP ratio is consistent across countries",
      null = "SCP/MCP ratio varies significantly",
      result = "inconclusive",
      interpretation = "Insufficient SCP/MCP data"
    ))
  }
  
  mcp_ratios <- scp_mcp$mcp_ratio
  mcp_ratios <- mcp_ratios[!is.na(mcp_ratios)]
  
  if (length(mcp_ratios) < 5) {
    return(list(hyphypothesis = "SCP/MCP consistency", result = "inconclusive"))
  }
  
  cv <- sd(mcp_ratios) / mean(mcp_ratios)
  
  result <- if (cv < 0.3) "fail_to_reject" else "reject"
  
  list(
    hyphypothesis = "SCP/MCP ratio is consistent across countries",
    null = "SCP/MCP ratio varies significantly",
    result = result,
    mean_mcp_ratio = mean(mcp_ratios),
    sd_mcp_ratio = sd(mcp_ratios),
    CV = cv,
    interpretation = sprintf("Mean MCP = %.1f%%, CV = %.2f. %s",
                            mean(mcp_ratios) * 100, cv,
                            if (cv < 0.3) "Consistent across countries." else "High variation.")
  )
}

test_distance_collab_hypothesis <- function(prepared_data, config) {
  collab_matrix <- prepared_data$collaboration_matrix
  
  if (is.null(collab_matrix) || nrow(collab_matrix) < 5) {
    return(list(
      hyphypothesis = "Geographic distance affects collaboration strength",
      null = "No relationship between distance and collaboration",
      result = "inconclusive",
      interpretation = "Insufficient collaboration data"
    ))
  }
  
  # Simplified test: check if top collaborators are geographically close
  # This is a placeholder - real implementation would use geographic coordinates
  
  list(
    hyphypothesis = "Geographic distance affects collaboration strength",
    null = "No relationship between distance and collaboration",
    result = "inconclusive",
    interpretation = "Requires geographic coordinates for full analysis"
  )
}

test_declining_countries_hypothesis <- function(prepared_data, config) {
  country_summary <- prepared_data$country_summary
  
  if (is.null(country_summary) || nrow(country_summary) == 0) {
    return(list(
      hyphypothesis = "Research topic is not declining in any country",
      null = "Research topic is declining in at least one country",
      result = "inconclusive",
      interpretation = "No country summary data"
    ))
  }
  
  declining <- country_summary[country_summary$trend_direction == "decreasing", ]
  
  n_declining <- nrow(declining)
  total <- nrow(country_summary)
  
  result <- if (n_declining == 0) "fail_to_reject" else "reject"
  
  interpretation <- sprintf(
    "%d/%d (%.1f%%) countries show declining research interest.",
    n_declining, total, n_declining / total * 100
  )
  
  if (n_declining > 0) {
    interpretation <- paste0(interpretation, " Declining countries: ", 
                            paste(head(declining$country, 5), collapse = ", "))
  }
  
  list(
    hyphypothesis = "Research topic is not declining in any country",
    null = "Research topic is declining in at least one country",
    result = result,
    n_declining = n_declining,
    n_total = total,
    declining_countries = declining$country,
    interpretation = interpretation
  )
}

test_growth_clusters_hypothesis <- function(prepared_data, config) {
  country_summary <- prepared_data$country_summary
  
  if (is.null(country_summary) || nrow(country_summary) < 5) {
    return(list(
      hyphypothesis = "Research interest is uniformly distributed (no clusters)",
      null = "Countries cluster into distinct growth patterns",
      result = "inconclusive",
      interpretation = "Insufficient country data"
    ))
  }
  
  growth_rates <- country_summary$annual_growth_rate
  growth_rates <- growth_rates[!is.na(growth_rates)]
  
  if (length(growth_rates) < 5) {
    return(list(hyphypothesis = "Growth clusters", result = "inconclusive"))
  }
  
  # Use k-means to identify clusters
  k_result <- tryCatch(kmeans(growth_rates, centers = 3, nstart = 10), error = function(e) NULL)
  
  if (is.null(k_result)) return(list(hyphypothesis = "Growth clusters", result = "inconclusive"))
  
  cluster_sizes <- k_result$size
  
  # Bunches identified
  growing_idx <- which.max(k_result$centers)
  growing_bunch <- sum(cluster_sizes[k_result$centers == max(k_result$centers)])
  declining_bunch <- sum(cluster_sizes[k_result$centers == min(k_result$centers)])
  stable_bunch <- sum(cluster_sizes) - growing_bunch - declining_bunch
  
  # If largest cluster has > 70% of countries, distribution is roughly uniform
  max_cluster_prop <- max(cluster_sizes) / sum(cluster_sizes)
  
  result <- if (max_cluster_prop > 0.7) "fail_to_reject" else "reject"
  
  list(
    hyphypothesis = "Research interest is uniformly distributed across countries",
    null = "Countries cluster into distinct growth patterns (bunches)",
    result = result,
    n_clusters = 3,
    cluster_sizes = cluster_sizes,
    cluster_centers = k_result$centers,
    growing_bunch = growing_bunch,
    declining_bunch = declining_bunch,
    stable_bunch = stable_bunch,
    within_ss = k_result$tot.withinss,
    interpretation = sprintf("Identified %d clusters: Growing=%d, Stable=%d, Declining=%d countries.",
                            3, growing_bunch, stable_bunch, declining_bunch)
  )
}

calculate_gini <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2) return(0)
  
  x <- sort(x)
  gini <- (n + 1 - 2 * sum((n + 1 - seq_len(n)) * x) / (n * sum(x))) / n
  gini
}

summarize_h3_hypothesis_results <- function(hypotheses) {
  n_total <- length(hypotheses)
  n_rejected <- sum(sapply(hypotheses, function(h) h$result == "reject"), na.rm = TRUE)
  n_failed <- sum(sapply(hypotheses, function(h) h$result == "fail_to_reject"), na.rm = TRUE)
  
  list(
    n_total = n_total,
    n_rejected = n_rejected,
    n_failed_to_reject = n_failed,
    rejection_rate = n_rejected / n_total
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b