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
      hypotheses = list(),
      hyphypotheses = list(),
      status = "error: invalid prepared_data"
    ))
  }

  prepared_data <- m3_prepare_hypothesis_inputs(prepared_data, config)
  
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

  # H03.23: Regional/country-group effects after controlling productivity
  hypotheses$H03_23 <- test_region_adjusted_impact_hypothesis(prepared_data, config)

  # H03.24: Country bibliometric role clusters differ in impact/collaboration
  hypotheses$H03_24 <- test_country_role_cluster_difference_hypothesis(prepared_data, config)

  # H03.25: MCP share mediates production-impact association
  hypotheses$H03_25 <- test_mcp_mediation_hypothesis(prepared_data, config)

  # H03.26: Citation inequality exceeds production inequality
  hypotheses$H03_26 <- test_citation_vs_production_inequality_hypothesis(prepared_data, config)

  # H03.27: Emerging countries accelerate faster than established countries
  hypotheses$H03_27 <- test_emerging_country_acceleration_hypothesis(prepared_data, config)

  # H03.28: Collaboration network modularity exceeds random structure
  hypotheses$H03_28 <- test_collaboration_modularity_hypothesis(prepared_data, config)

  # H03.29: Country trajectory vectors show a non-random common direction
  hypotheses$H03_29 <- test_country_trajectory_direction_hypothesis(prepared_data, config)

  # H03.30: Geographic Matthew effect / cumulative advantage
  hypotheses$H03_30 <- test_geographic_matthew_effect_hypothesis(prepared_data, config)

  # H03.31: Collaboration reduces impact inequality
  hypotheses$H03_31 <- test_collaboration_impact_gap_hypothesis(prepared_data, config)

  # H03.32: Peripheral countries depend more on MCP
  hypotheses$H03_32 <- test_peripheral_mcp_dependency_hypothesis(prepared_data, config)

  # H03.33: Initial productivity predicts future leadership
  hypotheses$H03_33 <- test_initial_productivity_future_leadership_hypothesis(prepared_data, config)

  # H03.34: Beta convergence between countries
  hypotheses$H03_34 <- test_country_beta_convergence_hypothesis(prepared_data, config)

  # H03.35: Sigma convergence between countries
  hypotheses$H03_35 <- test_country_sigma_convergence_hypothesis(prepared_data, config)

  # H03.36: Citation impact is decoupled from productivity for some countries
  hypotheses$H03_36 <- test_residual_impact_decoupling_hypothesis(prepared_data, config)

  # H03.37: New entrants alter field concentration
  hypotheses$H03_37 <- test_new_entrant_concentration_shift_hypothesis(prepared_data, config)

  # H03.38: Collaboration bridge centrality predicts impact
  hypotheses$H03_38 <- test_bridge_centrality_impact_hypothesis(prepared_data, config)

  # H03.39: Regional assortativity in collaboration
  hypotheses$H03_39 <- test_regional_collaboration_assortativity_hypothesis(prepared_data, config)
  
  summary_stats <- summarize_h3_hypothesis_results(hypotheses)
  
  list(
    hypotheses = hypotheses,
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
                                method = "spearman",
                                exact = FALSE),
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
                                method = "spearman",
                                exact = FALSE),
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

test_region_adjusted_impact_hypothesis <- function(prepared_data, config) {
  country_summary <- prepared_data$country_summary %||% tibble::tibble()
  if (!is.data.frame(country_summary) || nrow(country_summary) < 8) {
    return(m3_inconclusive_hypothesis("Regional group has no effect on citation impact after controlling production", "Insufficient country data"))
  }
  df <- m3_country_hypothesis_features(prepared_data)
  if (!"region" %in% names(df) || length(unique(stats::na.omit(df$region))) < 2) {
    df$region <- m3_proxy_region(df$country)
  }
  df <- df[is.finite(df$total_articles) & is.finite(df$total_citations) & df$total_articles > 0 & !is.na(df$region), , drop = FALSE]
  if (nrow(df) < 8 || length(unique(df$region)) < 2) {
    return(m3_inconclusive_hypothesis("Regional group has no effect on citation impact after controlling production", "At least two regional groups are required"))
  }
  fit0 <- tryCatch(stats::lm(log1p(total_citations) ~ log1p(total_articles), data = df), error = function(e) NULL)
  fit1 <- tryCatch(stats::lm(log1p(total_citations) ~ log1p(total_articles) + region, data = df), error = function(e) NULL)
  cmp <- if (!is.null(fit0) && !is.null(fit1)) tryCatch(stats::anova(fit0, fit1), error = function(e) NULL) else NULL
  if (is.null(cmp) || nrow(cmp) < 2) {
    return(m3_inconclusive_hypothesis("Regional group has no effect on citation impact after controlling production", "ANCOVA comparison failed"))
  }
  p_value <- suppressWarnings(as.numeric(cmp[["Pr(>F)"]][2]))
  statistic <- suppressWarnings(as.numeric(cmp[["F"]][2]))
  r2_gain <- tryCatch(summary(fit1)$r.squared - summary(fit0)$r.squared, error = function(e) NA_real_)
  m3_stat_hypothesis(
    "Regional group has no effect on citation impact after controlling production",
    "Region does not improve log citation impact beyond log production",
    "ANCOVA nested model comparison",
    p_value,
    statistic,
    r2_gain,
    sprintf("Regional adjusted-impact test: F = %.3f (p = %.4f), delta R2 = %.3f.", statistic, p_value, r2_gain)
  )
}

test_country_role_cluster_difference_hypothesis <- function(prepared_data, config) {
  df <- m3_country_hypothesis_features(prepared_data)
  if (!is.data.frame(df) || nrow(df) < 8) {
    return(m3_inconclusive_hypothesis("Country role clusters have equal impact and collaboration profiles", "Insufficient country data"))
  }
  features <- df[, intersect(c("total_articles", "total_citations", "mean_citations", "mcp_ratio", "annual_growth_rate"), names(df)), drop = FALSE]
  features[] <- lapply(features, function(x) suppressWarnings(as.numeric(x)))
  features[!is.finite(as.matrix(features))] <- 0
  variable <- vapply(features, function(x) stats::sd(x, na.rm = TRUE) > 0, logical(1))
  features <- features[, variable, drop = FALSE]
  if (ncol(features) < 2 || nrow(features) < 8) {
    return(m3_inconclusive_hypothesis("Country role clusters have equal impact and collaboration profiles", "Insufficient variable features for clustering"))
  }
  k <- max(2L, min(4L, floor(sqrt(nrow(features)))))
  set.seed(as.integer(config$seed %||% 1234L))
  km <- tryCatch(stats::kmeans(scale(features), centers = k, nstart = 25), error = function(e) NULL)
  if (is.null(km)) {
    return(m3_inconclusive_hypothesis("Country role clusters have equal impact and collaboration profiles", "K-means failed"))
  }
  df$role_cluster <- factor(km$cluster)
  metric <- if ("mean_citations" %in% names(df)) "mean_citations" else "total_citations"
  test <- tryCatch(stats::kruskal.test(df[[metric]] ~ df$role_cluster), error = function(e) NULL)
  if (is.null(test)) {
    return(m3_inconclusive_hypothesis("Country role clusters have equal impact and collaboration profiles", "Cluster impact test failed"))
  }
  m3_stat_hypothesis(
    "Country role clusters have equal impact and collaboration profiles",
    "K-means country roles do not differ in citation impact",
    "Kruskal-Wallis across role clusters",
    test$p.value,
    suppressWarnings(as.numeric(test$statistic)),
    m3_epsilon_squared(test$statistic, nrow(df), length(unique(df$role_cluster))),
    sprintf("Role clusters differ on %s with chi-square = %.3f (p = %.4f).", metric, as.numeric(test$statistic), test$p.value)
  )
}

test_mcp_mediation_hypothesis <- function(prepared_data, config) {
  df <- m3_country_hypothesis_features(prepared_data)
  df <- df[is.finite(df$total_articles) & is.finite(df$total_citations) & is.finite(df$mcp_ratio) & df$total_articles > 0, , drop = FALSE]
  if (nrow(df) < 8 || stats::sd(df$mcp_ratio, na.rm = TRUE) <= 0) {
    return(m3_inconclusive_hypothesis("MCP share does not mediate the production-impact relationship", "Insufficient MCP variation"))
  }
  fit_a <- tryCatch(stats::lm(mcp_ratio ~ log1p(total_articles), data = df), error = function(e) NULL)
  fit_b <- tryCatch(stats::lm(log1p(total_citations) ~ log1p(total_articles) + mcp_ratio, data = df), error = function(e) NULL)
  if (is.null(fit_a) || is.null(fit_b)) {
    return(m3_inconclusive_hypothesis("MCP share does not mediate the production-impact relationship", "Mediation regressions failed"))
  }
  a <- suppressWarnings(as.numeric(stats::coef(fit_a)[2]))
  b <- suppressWarnings(as.numeric(stats::coef(fit_b)["mcp_ratio"]))
  se_a <- tryCatch(summary(fit_a)$coefficients[2, 2], error = function(e) NA_real_)
  se_b <- tryCatch(summary(fit_b)$coefficients["mcp_ratio", 2], error = function(e) NA_real_)
  indirect <- a * b
  se <- sqrt((b^2 * se_a^2) + (a^2 * se_b^2))
  z <- indirect / pmax(se, .Machine$double.eps)
  p_value <- 2 * stats::pnorm(-abs(z))
  m3_stat_hypothesis(
    "MCP share does not mediate the production-impact relationship",
    "The indirect production -> MCP -> citation pathway equals zero",
    "Sobel mediation test",
    p_value,
    z,
    indirect,
    sprintf("Indirect effect = %.4f (z = %.3f, p = %.4f).", indirect, z, p_value)
  )
}

test_citation_vs_production_inequality_hypothesis <- function(prepared_data, config) {
  df <- m3_country_hypothesis_features(prepared_data)
  if (!is.data.frame(df) || nrow(df) < 5) {
    return(m3_inconclusive_hypothesis("Citation inequality does not exceed production inequality", "Insufficient country data"))
  }
  tp <- suppressWarnings(as.numeric(df$total_articles))
  tc <- suppressWarnings(as.numeric(df$total_citations))
  keep <- is.finite(tp) & is.finite(tc) & tp >= 0 & tc >= 0
  tp <- tp[keep]
  tc <- tc[keep]
  if (length(tp) < 5 || sum(tp) <= 0 || sum(tc) <= 0) {
    return(m3_inconclusive_hypothesis("Citation inequality does not exceed production inequality", "Insufficient positive TP/TC totals"))
  }
  diff_obs <- calculate_gini(tc) - calculate_gini(tp)
  set.seed(as.integer(config$seed %||% 1234L))
  b <- replicate(499L, {
    idx <- sample(seq_along(tp), replace = TRUE)
    calculate_gini(tc[idx]) - calculate_gini(tp[idx])
  })
  p_value <- mean(b <= 0, na.rm = TRUE)
  m3_stat_hypothesis(
    "Citation inequality does not exceed production inequality",
    "Gini(TC) - Gini(TP) is less than or equal to zero",
    "Bootstrap paired Gini difference",
    p_value,
    diff_obs,
    diff_obs,
    sprintf("Gini(TC) - Gini(TP) = %.3f (one-sided bootstrap p = %.4f).", diff_obs, p_value)
  )
}

test_emerging_country_acceleration_hypothesis <- function(prepared_data, config) {
  df <- m3_country_hypothesis_features(prepared_data)
  if (!is.data.frame(df) || nrow(df) < 8 || !"n_years" %in% names(df)) {
    return(m3_inconclusive_hypothesis("Emerging countries do not accelerate faster than established countries", "Insufficient country growth features"))
  }
  df$group <- ifelse(df$n_years < stats::median(df$n_years, na.rm = TRUE), "Emerging", "Established")
  metric <- suppressWarnings(as.numeric(df$annual_growth_rate))
  keep <- is.finite(metric) & !is.na(df$group)
  df <- df[keep, , drop = FALSE]
  if (nrow(df) < 6 || length(unique(df$group)) < 2) {
    return(m3_inconclusive_hypothesis("Emerging countries do not accelerate faster than established countries", "Both emerging and established groups are required"))
  }
  test <- tryCatch(stats::wilcox.test(annual_growth_rate ~ group, data = df, alternative = "greater", exact = FALSE), error = function(e) NULL)
  if (is.null(test)) {
    return(m3_inconclusive_hypothesis("Emerging countries do not accelerate faster than established countries", "Growth comparison failed"))
  }
  delta <- stats::median(df$annual_growth_rate[df$group == "Emerging"], na.rm = TRUE) -
    stats::median(df$annual_growth_rate[df$group == "Established"], na.rm = TRUE)
  m3_stat_hypothesis(
    "Emerging countries do not accelerate faster than established countries",
    "Emerging-country growth is not greater than established-country growth",
    "One-sided Wilcoxon rank-sum test",
    test$p.value,
    suppressWarnings(as.numeric(test$statistic)),
    delta,
    sprintf("Emerging minus established median growth = %.3f (p = %.4f).", delta, test$p.value)
  )
}

test_collaboration_modularity_hypothesis <- function(prepared_data, config) {
  mat <- prepared_data$collaboration_matrix
  if (is.null(mat) || !is.matrix(mat) || nrow(mat) < 5) {
    return(m3_inconclusive_hypothesis("Country collaboration network modularity does not exceed random structure", "Insufficient collaboration matrix"))
  }
  mat[!is.finite(mat)] <- 0
  diag(mat) <- 0
  if (sum(mat) <= 0) {
    return(m3_inconclusive_hypothesis("Country collaboration network modularity does not exceed random structure", "No collaboration edge weight"))
  }
  clusters <- stats::kmeans(scale(rowSums(mat) + mat), centers = max(2L, min(4L, floor(sqrt(nrow(mat))))), nstart = 10)$cluster
  observed <- m3_weighted_modularity(mat, clusters)
  set.seed(as.integer(config$seed %||% 1234L))
  random_q <- replicate(199L, m3_weighted_modularity(mat, sample(clusters)))
  p_value <- mean(random_q >= observed, na.rm = TRUE)
  m3_stat_hypothesis(
    "Country collaboration network modularity does not exceed random structure",
    "Observed weighted modularity is no greater than random label permutations",
    "Permutation modularity test",
    p_value,
    observed,
    observed - mean(random_q, na.rm = TRUE),
    sprintf("Observed modularity = %.3f; random mean = %.3f (p = %.4f).", observed, mean(random_q, na.rm = TRUE), p_value)
  )
}

test_country_trajectory_direction_hypothesis <- function(prepared_data, config) {
  annual <- prepared_data$country_annual %||% tibble::tibble()
  if (!is.data.frame(annual) || nrow(annual) < 10 || !all(c("country", "year", "article_count") %in% names(annual))) {
    return(m3_inconclusive_hypothesis("Country trajectory vectors have no common direction", "Insufficient country-year production data"))
  }
  split_rows <- split(annual, annual$country)
  slopes <- vapply(split_rows, function(dat) {
    dat <- dat[is.finite(dat$year) & is.finite(dat$article_count), , drop = FALSE]
    if (nrow(dat) < 3 || length(unique(dat$year)) < 2) return(NA_real_)
    suppressWarnings(as.numeric(stats::coef(stats::lm(article_count ~ year, data = dat))[2]))
  }, numeric(1))
  slopes <- slopes[is.finite(slopes)]
  if (length(slopes) < 5) {
    return(m3_inconclusive_hypothesis("Country trajectory vectors have no common direction", "Insufficient finite country slopes"))
  }
  positive <- sum(slopes > 0)
  test <- tryCatch(stats::binom.test(positive, length(slopes), p = 0.5, alternative = "greater"), error = function(e) NULL)
  if (is.null(test)) {
    return(m3_inconclusive_hypothesis("Country trajectory vectors have no common direction", "Directional binomial test failed"))
  }
  m3_stat_hypothesis(
    "Country trajectory vectors have no common direction",
    "The share of positive country production slopes is not greater than 0.5",
    "One-sided binomial direction test",
    test$p.value,
    positive,
    positive / length(slopes),
    sprintf("%d/%d country slopes are positive (p = %.4f).", positive, length(slopes), test$p.value)
  )
}

test_geographic_matthew_effect_hypothesis <- function(prepared_data, config) {
  shares <- m3_country_window_shares(prepared_data)
  if (!is.data.frame(shares) || nrow(shares) < 6) {
    return(m3_inconclusive_hypothesis("Initial country share does not predict later share gains", "Insufficient country-window share data"))
  }
  fit <- tryCatch(stats::lm(share_change ~ initial_share, data = shares), error = function(e) NULL)
  if (is.null(fit)) {
    return(m3_inconclusive_hypothesis("Initial country share does not predict later share gains", "Matthew-effect regression failed"))
  }
  coefs <- tryCatch(summary(fit)$coefficients, error = function(e) NULL)
  if (is.null(coefs) || nrow(coefs) < 2) {
    return(m3_inconclusive_hypothesis("Initial country share does not predict later share gains", "Initial-share coefficient unavailable"))
  }
  beta <- suppressWarnings(as.numeric(coefs[2, 1]))
  t_stat <- suppressWarnings(as.numeric(coefs[2, 3]))
  p_two <- suppressWarnings(as.numeric(coefs[2, 4]))
  p_value <- if (is.finite(beta) && beta > 0) p_two / 2 else 1 - p_two / 2
  m3_stat_hypothesis(
    "Initial country share does not predict later share gains",
    "Initial country production share has no positive association with subsequent share change",
    "One-sided share-change regression",
    p_value,
    t_stat,
    beta,
    sprintf("Initial-share beta = %.3f (one-sided p = %.4f). Positive beta supports a geographic Matthew effect.", beta, p_value)
  )
}

test_collaboration_impact_gap_hypothesis <- function(prepared_data, config) {
  df <- m3_country_hypothesis_features(prepared_data)
  if (!is.data.frame(df) || nrow(df) < 8 || stats::sd(df$mcp_ratio, na.rm = TRUE) <= 0) {
    return(m3_inconclusive_hypothesis("MCP share is not associated with smaller citation-impact gaps", "Insufficient MCP variation"))
  }
  df$cpp <- suppressWarnings(as.numeric(df$total_citations)) / pmax(suppressWarnings(as.numeric(df$total_articles)), .Machine$double.eps)
  median_cpp <- stats::median(df$cpp, na.rm = TRUE)
  df$impact_gap <- abs(log1p(df$cpp) - log1p(median_cpp))
  keep <- is.finite(df$mcp_ratio) & is.finite(df$impact_gap)
  df <- df[keep, , drop = FALSE]
  if (nrow(df) < 8) {
    return(m3_inconclusive_hypothesis("MCP share is not associated with smaller citation-impact gaps", "Insufficient finite impact gaps"))
  }
  test <- tryCatch(stats::cor.test(df$mcp_ratio, -df$impact_gap, method = "spearman", exact = FALSE), error = function(e) NULL)
  if (is.null(test)) {
    return(m3_inconclusive_hypothesis("MCP share is not associated with smaller citation-impact gaps", "Spearman test failed"))
  }
  rho <- suppressWarnings(as.numeric(test$estimate))
  p_value <- if (is.finite(rho) && rho > 0) test$p.value / 2 else 1 - test$p.value / 2
  m3_stat_hypothesis(
    "MCP share is not associated with smaller citation-impact gaps",
    "MCP ratio has no association with smaller deviation from median CPP",
    "Spearman MCP versus negative impact-gap test",
    p_value,
    rho,
    rho,
    sprintf("MCP ratio versus negative CPP gap Spearman rho = %.3f (one-sided p = %.4f).", rho, p_value)
  )
}

test_peripheral_mcp_dependency_hypothesis <- function(prepared_data, config) {
  df <- m3_country_hypothesis_features(prepared_data)
  if (!is.data.frame(df) || nrow(df) < 8 || stats::sd(df$mcp_ratio, na.rm = TRUE) <= 0) {
    return(m3_inconclusive_hypothesis("Peripheral countries do not have higher MCP dependence than core countries", "Insufficient MCP variation"))
  }
  threshold <- stats::median(df$total_articles, na.rm = TRUE)
  df$core_status <- ifelse(df$total_articles >= threshold, "Core", "Peripheral")
  df <- df[is.finite(df$mcp_ratio) & !is.na(df$core_status), , drop = FALSE]
  if (nrow(df) < 8 || length(unique(df$core_status)) < 2) {
    return(m3_inconclusive_hypothesis("Peripheral countries do not have higher MCP dependence than core countries", "Both core and peripheral groups are required"))
  }
  test <- tryCatch(stats::wilcox.test(
    df$mcp_ratio[df$core_status == "Peripheral"],
    df$mcp_ratio[df$core_status == "Core"],
    alternative = "greater",
    exact = FALSE
  ), error = function(e) NULL)
  if (is.null(test)) {
    return(m3_inconclusive_hypothesis("Peripheral countries do not have higher MCP dependence than core countries", "MCP group comparison failed"))
  }
  delta <- stats::median(df$mcp_ratio[df$core_status == "Peripheral"], na.rm = TRUE) -
    stats::median(df$mcp_ratio[df$core_status == "Core"], na.rm = TRUE)
  m3_stat_hypothesis(
    "Peripheral countries do not have higher MCP dependence than core countries",
    "Peripheral-country MCP ratios are not greater than core-country MCP ratios",
    "One-sided Wilcoxon core-periphery MCP test",
    test$p.value,
    suppressWarnings(as.numeric(test$statistic)),
    delta,
    sprintf("Peripheral minus core median MCP ratio = %.3f (p = %.4f).", delta, test$p.value)
  )
}

test_initial_productivity_future_leadership_hypothesis <- function(prepared_data, config) {
  shares <- m3_country_window_shares(prepared_data)
  if (!is.data.frame(shares) || nrow(shares) < 6) {
    return(m3_inconclusive_hypothesis("Initial productivity does not predict future leadership", "Insufficient country-window share data"))
  }
  test <- tryCatch(stats::cor.test(shares$initial_share, shares$final_share, method = "spearman", exact = FALSE), error = function(e) NULL)
  if (is.null(test)) {
    return(m3_inconclusive_hypothesis("Initial productivity does not predict future leadership", "Lagged rank correlation failed"))
  }
  rho <- suppressWarnings(as.numeric(test$estimate))
  p_value <- if (is.finite(rho) && rho > 0) test$p.value / 2 else 1 - test$p.value / 2
  m3_stat_hypothesis(
    "Initial productivity does not predict future leadership",
    "Initial production share is not positively associated with final production share",
    "Spearman lagged leadership correlation",
    p_value,
    rho,
    rho,
    sprintf("Initial versus final share Spearman rho = %.3f (one-sided p = %.4f).", rho, p_value)
  )
}

test_country_beta_convergence_hypothesis <- function(prepared_data, config) {
  shares <- m3_country_window_shares(prepared_data)
  if (!is.data.frame(shares) || nrow(shares) < 8) {
    return(m3_inconclusive_hypothesis("Countries do not show beta convergence in production", "Insufficient country-window share data"))
  }
  shares <- shares[shares$initial_articles > 0, , drop = FALSE]
  if (nrow(shares) < 8) {
    return(m3_inconclusive_hypothesis("Countries do not show beta convergence in production", "At least 8 countries active in the initial window are required"))
  }
  shares$growth <- (shares$final_articles - shares$initial_articles) / pmax(shares$initial_articles, .Machine$double.eps)
  shares$log_initial <- log1p(shares$initial_articles)
  shares <- shares[is.finite(shares$growth) & is.finite(shares$log_initial), , drop = FALSE]
  if (nrow(shares) < 8 || stats::sd(shares$log_initial, na.rm = TRUE) <= 0) {
    return(m3_inconclusive_hypothesis("Countries do not show beta convergence in production", "Insufficient initial productivity variation"))
  }
  fit <- tryCatch(stats::lm(growth ~ log_initial, data = shares), error = function(e) NULL)
  coefs <- if (!is.null(fit)) tryCatch(summary(fit)$coefficients, error = function(e) NULL) else NULL
  if (is.null(coefs) || nrow(coefs) < 2) {
    return(m3_inconclusive_hypothesis("Countries do not show beta convergence in production", "Beta-convergence coefficient unavailable"))
  }
  beta <- suppressWarnings(as.numeric(coefs[2, 1]))
  t_stat <- suppressWarnings(as.numeric(coefs[2, 3]))
  p_two <- suppressWarnings(as.numeric(coefs[2, 4]))
  p_value <- if (is.finite(beta) && beta < 0) p_two / 2 else 1 - p_two / 2
  m3_stat_hypothesis(
    "Countries do not show beta convergence in production",
    "Initial production does not negatively predict subsequent growth",
    "One-sided beta-convergence regression",
    p_value,
    t_stat,
    beta,
    sprintf("Beta-convergence coefficient = %.3f (one-sided p = %.4f). Negative beta supports catch-up.", beta, p_value)
  )
}

test_country_sigma_convergence_hypothesis <- function(prepared_data, config) {
  gini <- prepared_data$gini_over_time
  if (is.null(gini) || length(gini) < 6) {
    return(m3_inconclusive_hypothesis("Cross-country production dispersion is not decreasing over time", "Insufficient annual inequality data"))
  }
  df <- data.frame(year = suppressWarnings(as.numeric(names(gini))), gini = suppressWarnings(as.numeric(gini)))
  df <- df[is.finite(df$year) & is.finite(df$gini), , drop = FALSE]
  if (nrow(df) < 6 || stats::sd(df$gini, na.rm = TRUE) <= 0) {
    return(m3_inconclusive_hypothesis("Cross-country production dispersion is not decreasing over time", "Insufficient Gini variation"))
  }
  fit <- tryCatch(stats::lm(gini ~ year, data = df), error = function(e) NULL)
  coefs <- if (!is.null(fit)) tryCatch(summary(fit)$coefficients, error = function(e) NULL) else NULL
  if (is.null(coefs) || nrow(coefs) < 2) {
    return(m3_inconclusive_hypothesis("Cross-country production dispersion is not decreasing over time", "Sigma-convergence coefficient unavailable"))
  }
  slope <- suppressWarnings(as.numeric(coefs[2, 1]))
  t_stat <- suppressWarnings(as.numeric(coefs[2, 3]))
  p_two <- suppressWarnings(as.numeric(coefs[2, 4]))
  p_value <- if (is.finite(slope) && slope < 0) p_two / 2 else 1 - p_two / 2
  m3_stat_hypothesis(
    "Cross-country production dispersion is not decreasing over time",
    "Annual production Gini slope is not negative",
    "One-sided sigma-convergence trend test",
    p_value,
    t_stat,
    slope,
    sprintf("Annual production Gini slope = %.4f/year (one-sided p = %.4f).", slope, p_value)
  )
}

test_residual_impact_decoupling_hypothesis <- function(prepared_data, config) {
  df <- m3_country_hypothesis_features(prepared_data)
  df <- df[is.finite(df$total_articles) & is.finite(df$total_citations) & df$total_articles > 0, , drop = FALSE]
  if (!is.data.frame(df) || nrow(df) < 8 || stats::sd(df$total_citations, na.rm = TRUE) <= 0) {
    return(m3_inconclusive_hypothesis("Citation impact is fully coupled to productivity", "Insufficient TP/TC variation"))
  }
  fit <- tryCatch(stats::lm(log1p(total_citations) ~ log1p(total_articles), data = df), error = function(e) NULL)
  if (is.null(fit)) {
    return(m3_inconclusive_hypothesis("Citation impact is fully coupled to productivity", "Impact-productivity model failed"))
  }
  std_res <- tryCatch(suppressWarnings(stats::rstandard(fit)), error = function(e) rep(NA_real_, nrow(df)))
  p_raw <- 2 * stats::pnorm(-abs(std_res))
  p_adj <- stats::p.adjust(p_raw, method = "BH")
  outliers <- which(is.finite(p_adj) & p_adj <= 0.05)
  min_p <- if (any(is.finite(p_adj))) min(p_adj, na.rm = TRUE) else NA_real_
  m3_stat_hypothesis(
    "Citation impact is fully coupled to productivity",
    "No country has an FDR-significant residual impact after controlling production",
    "Residual impact outlier scan",
    min_p,
    if (length(outliers) > 0) max(abs(std_res[outliers]), na.rm = TRUE) else max(abs(std_res), na.rm = TRUE),
    length(outliers),
    sprintf("%d country impact outlier(s) detected after FDR correction; strongest adjusted p = %.4f.", length(outliers), min_p)
  )
}

test_new_entrant_concentration_shift_hypothesis <- function(prepared_data, config) {
  annual <- prepared_data$country_annual %||% tibble::tibble()
  gini <- prepared_data$gini_over_time
  if (!is.data.frame(annual) || nrow(annual) < 10 || is.null(gini) || length(gini) < 6) {
    return(m3_inconclusive_hypothesis("New country entry waves do not alter production concentration", "Insufficient annual country-entry data"))
  }
  annual <- annual[is.finite(annual$year) & !is.na(annual$country), , drop = FALSE]
  first_year <- stats::aggregate(year ~ country, data = annual, FUN = min)
  entrants <- as.data.frame(table(first_year$year), stringsAsFactors = FALSE)
  names(entrants) <- c("year", "new_entrants")
  entrants$year <- suppressWarnings(as.numeric(as.character(entrants$year)))
  entrants$new_entrants <- suppressWarnings(as.numeric(entrants$new_entrants))
  df <- data.frame(year = suppressWarnings(as.numeric(names(gini))), gini = suppressWarnings(as.numeric(gini)))
  df <- merge(df, entrants, by = "year", all.x = TRUE)
  df$new_entrants[is.na(df$new_entrants)] <- 0
  df <- df[is.finite(df$year) & is.finite(df$gini) & is.finite(df$new_entrants), , drop = FALSE]
  if (nrow(df) < 6 || stats::sd(df$new_entrants, na.rm = TRUE) <= 0) {
    return(m3_inconclusive_hypothesis("New country entry waves do not alter production concentration", "Insufficient entrant variation"))
  }
  fit <- tryCatch(stats::lm(gini ~ year + new_entrants, data = df), error = function(e) NULL)
  coefs <- if (!is.null(fit)) tryCatch(summary(fit)$coefficients, error = function(e) NULL) else NULL
  if (is.null(coefs) || !"new_entrants" %in% rownames(coefs)) {
    return(m3_inconclusive_hypothesis("New country entry waves do not alter production concentration", "Entrant coefficient unavailable"))
  }
  beta <- suppressWarnings(as.numeric(coefs["new_entrants", 1]))
  t_stat <- suppressWarnings(as.numeric(coefs["new_entrants", 3]))
  p_value <- suppressWarnings(as.numeric(coefs["new_entrants", 4]))
  m3_stat_hypothesis(
    "New country entry waves do not alter production concentration",
    "Annual new-entrant count has no association with production Gini after controlling year",
    "Entrant-adjusted Gini regression",
    p_value,
    t_stat,
    beta,
    sprintf("New-entrant beta on production Gini = %.4f (p = %.4f).", beta, p_value)
  )
}

test_bridge_centrality_impact_hypothesis <- function(prepared_data, config) {
  mat <- prepared_data$collaboration_matrix
  df <- m3_country_hypothesis_features(prepared_data)
  if (is.null(mat) || !is.matrix(mat) || nrow(mat) < 5 || !is.data.frame(df) || nrow(df) < 8) {
    return(m3_inconclusive_hypothesis("Collaboration bridge centrality does not predict citation impact", "Insufficient collaboration matrix or country impact data"))
  }
  mat[!is.finite(mat)] <- 0
  diag(mat) <- 0
  centrality <- rowSums(mat, na.rm = TRUE)
  cent_df <- data.frame(country = names(centrality), bridge_centrality = as.numeric(centrality))
  df <- dplyr::left_join(df, cent_df, by = "country")
  df <- df[is.finite(df$total_articles) & is.finite(df$total_citations) & is.finite(df$bridge_centrality) & df$total_articles > 0, , drop = FALSE]
  if (nrow(df) < 8 || stats::sd(df$bridge_centrality, na.rm = TRUE) <= 0) {
    return(m3_inconclusive_hypothesis("Collaboration bridge centrality does not predict citation impact", "Insufficient bridge-centrality variation"))
  }
  fit <- tryCatch(stats::lm(log1p(total_citations) ~ log1p(total_articles) + log1p(bridge_centrality), data = df), error = function(e) NULL)
  coefs <- if (!is.null(fit)) tryCatch(summary(fit)$coefficients, error = function(e) NULL) else NULL
  term <- "log1p(bridge_centrality)"
  if (is.null(coefs) || !term %in% rownames(coefs)) {
    return(m3_inconclusive_hypothesis("Collaboration bridge centrality does not predict citation impact", "Centrality coefficient unavailable"))
  }
  beta <- suppressWarnings(as.numeric(coefs[term, 1]))
  t_stat <- suppressWarnings(as.numeric(coefs[term, 3]))
  p_value <- suppressWarnings(as.numeric(coefs[term, 4]))
  m3_stat_hypothesis(
    "Collaboration bridge centrality does not predict citation impact",
    "Bridge centrality has zero coefficient after controlling production",
    "Production-adjusted bridge-centrality regression",
    p_value,
    t_stat,
    beta,
    sprintf("Bridge-centrality beta = %.3f (t = %.3f, p = %.4f).", beta, t_stat, p_value)
  )
}

test_regional_collaboration_assortativity_hypothesis <- function(prepared_data, config) {
  mat <- prepared_data$collaboration_matrix
  if (is.null(mat) || !is.matrix(mat) || nrow(mat) < 5) {
    return(m3_inconclusive_hypothesis("Collaboration is not regionally assortative", "Insufficient collaboration matrix"))
  }
  mat[!is.finite(mat)] <- 0
  diag(mat) <- 0
  if (sum(mat) <= 0) {
    return(m3_inconclusive_hypothesis("Collaboration is not regionally assortative", "No collaboration edge weight"))
  }
  countries <- rownames(mat)
  regions <- m3_country_continent(countries)
  if (length(unique(stats::na.omit(regions))) < 2) {
    return(m3_inconclusive_hypothesis("Collaboration is not regionally assortative", "At least two regions are required"))
  }
  observed <- m3_intra_region_edge_share(mat, regions)
  set.seed(as.integer(config$seed %||% 1234L))
  random <- replicate(299L, m3_intra_region_edge_share(mat, sample(regions)))
  p_value <- mean(random >= observed, na.rm = TRUE)
  m3_stat_hypothesis(
    "Collaboration is not regionally assortative",
    "Intra-region collaboration share is no greater than random region permutations",
    "Permutation regional assortativity test",
    p_value,
    observed,
    observed - mean(random, na.rm = TRUE),
    sprintf("Observed intra-region collaboration share = %.3f; random mean = %.3f (p = %.4f).", observed, mean(random, na.rm = TRUE), p_value)
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

m3_inconclusive_hypothesis <- function(label, interpretation = "Insufficient data", null = NULL) {
  list(
    hyphypothesis = label,
    hypothesis = label,
    null = null %||% label,
    result = "inconclusive",
    evidence_class = "statistical",
    test = "not estimable",
    interpretation = interpretation
  )
}

m3_stat_hypothesis <- function(label, null, test, p_value, statistic, effect_size, interpretation) {
  p_value <- suppressWarnings(as.numeric(p_value)[1])
  list(
    hyphypothesis = label,
    hypothesis = label,
    null = null,
    test = test,
    evidence_class = "statistical",
    result = if (is.finite(p_value) && p_value <= 0.05) "reject" else if (is.finite(p_value)) "fail_to_reject" else "inconclusive",
    statistic = suppressWarnings(as.numeric(statistic)[1]),
    p_value = p_value,
    effect_size = suppressWarnings(as.numeric(effect_size)[1]),
    interpretation = interpretation
  )
}

m3_country_hypothesis_features <- function(prepared_data) {
  df <- prepared_data$country_summary %||% tibble::tibble()
  if (!is.data.frame(df) || nrow(df) == 0) return(tibble::tibble())
  df <- tibble::as_tibble(df)
  if (!"total_articles" %in% names(df) && "article_count" %in% names(df)) df$total_articles <- df$article_count
  if (!"total_citations" %in% names(df)) df$total_citations <- 0
  if (!"mean_citations" %in% names(df)) df$mean_citations <- df$total_citations / pmax(df$total_articles, .Machine$double.eps)
  scp <- prepared_data$scp_mcp_by_country %||% tibble::tibble()
  if (is.data.frame(scp) && nrow(scp) > 0 && "country" %in% names(scp)) {
    keep_cols <- intersect(c("country", "scp", "mcp", "mcp_ratio"), names(scp))
    df <- dplyr::left_join(df, scp[, keep_cols, drop = FALSE], by = "country")
  }
  if (!"mcp_ratio" %in% names(df)) df$mcp_ratio <- 0
  df$mcp_ratio <- dplyr::coalesce(suppressWarnings(as.numeric(df$mcp_ratio)), 0)
  df
}

m3_proxy_region <- function(country) {
  first <- toupper(substr(as.character(country), 1, 1))
  factor(dplyr::case_when(
    first %in% c("A", "B", "C", "D", "E", "F") ~ "Group A-F",
    first %in% c("G", "H", "I", "J", "K", "L", "M") ~ "Group G-M",
    TRUE ~ "Group N-Z"
  ))
}

m3_epsilon_squared <- function(statistic, n, k) {
  h <- suppressWarnings(as.numeric(statistic)[1])
  if (!is.finite(h) || !is.finite(n) || !is.finite(k) || n <= k) return(NA_real_)
  max(0, (h - k + 1) / (n - k))
}

m3_weighted_modularity <- function(mat, clusters) {
  mat <- as.matrix(mat)
  mat[!is.finite(mat)] <- 0
  m <- sum(mat) / 2
  if (!is.finite(m) || m <= 0) return(NA_real_)
  k <- rowSums(mat)
  q <- 0
  for (i in seq_len(nrow(mat))) {
    for (j in seq_len(ncol(mat))) {
      if (clusters[i] == clusters[j]) {
        q <- q + (mat[i, j] - (k[i] * k[j]) / (2 * m))
      }
    }
  }
  q / (2 * m)
}

m3_country_window_shares <- function(prepared_data) {
  annual <- prepared_data$country_annual %||% tibble::tibble()
  if (!is.data.frame(annual) || nrow(annual) == 0 || !all(c("country", "year", "article_count") %in% names(annual))) {
    return(tibble::tibble())
  }
  annual <- annual[is.finite(annual$year) & is.finite(annual$article_count) & !is.na(annual$country), , drop = FALSE]
  years <- sort(unique(annual$year))
  if (length(years) < 4) {
    return(tibble::tibble())
  }
  window_n <- max(2L, floor(length(years) / 3))
  initial_years <- utils::head(years, window_n)
  final_years <- utils::tail(years, window_n)
  initial <- stats::aggregate(article_count ~ country, data = annual[annual$year %in% initial_years, , drop = FALSE], FUN = sum)
  final <- stats::aggregate(article_count ~ country, data = annual[annual$year %in% final_years, , drop = FALSE], FUN = sum)
  names(initial)[2] <- "initial_articles"
  names(final)[2] <- "final_articles"
  out <- merge(initial, final, by = "country", all = TRUE)
  out$initial_articles[is.na(out$initial_articles)] <- 0
  out$final_articles[is.na(out$final_articles)] <- 0
  total_initial <- sum(out$initial_articles, na.rm = TRUE)
  total_final <- sum(out$final_articles, na.rm = TRUE)
  out$initial_share <- out$initial_articles / pmax(total_initial, .Machine$double.eps)
  out$final_share <- out$final_articles / pmax(total_final, .Machine$double.eps)
  out$share_change <- out$final_share - out$initial_share
  tibble::as_tibble(out)
}

m3_country_continent <- function(country) {
  country_norm <- toupper(trimws(as.character(country)))
  out <- rep("Unassigned", length(country_norm))
  continent_groups <- REGIONAL_GROUPS[c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")]
  for (region in names(continent_groups)) {
    out[country_norm %in% continent_groups[[region]]] <- region
  }
  out
}

m3_intra_region_edge_share <- function(mat, regions) {
  mat <- as.matrix(mat)
  if (!is.matrix(mat) || nrow(mat) < 2 || length(regions) != nrow(mat)) return(NA_real_)
  idx <- upper.tri(mat)
  weights <- mat[idx]
  same_region <- outer(regions, regions, FUN = "==")[idx]
  valid <- is.finite(weights) & !is.na(same_region) & regions[row(mat)[idx]] != "Unassigned" & regions[col(mat)[idx]] != "Unassigned"
  weights <- weights[valid]
  same_region <- same_region[valid]
  if (length(weights) == 0 || sum(weights, na.rm = TRUE) <= 0) return(NA_real_)
  sum(weights[same_region], na.rm = TRUE) / sum(weights, na.rm = TRUE)
}

summarize_h3_hypothesis_results <- function(hypotheses) {
  n_total <- length(hypotheses)
  if (n_total == 0) {
    return(list(
      n_total = 0L,
      n_rejected = 0L,
      n_failed_to_reject = 0L,
      rejection_rate = NA_real_
    ))
  }
  n_rejected <- sum(sapply(hypotheses, function(h) h$result == "reject"), na.rm = TRUE)
  n_failed <- sum(sapply(hypotheses, function(h) h$result == "fail_to_reject"), na.rm = TRUE)
  
  list(
    n_total = n_total,
    n_rejected = n_rejected,
    n_failed_to_reject = n_failed,
    rejection_rate = n_rejected / n_total
  )
}

m3_prepare_hypothesis_inputs <- function(prepared_data, config = biblio_config()) {
  output <- prepared_data
  country_annual <- output$country_annual

  if (is.null(country_annual)) {
    country_annual <- tibble::tibble()
  } else {
    country_annual <- tibble::as_tibble(country_annual)
  }

  if (nrow(country_annual) > 0) {
    if (!"year" %in% names(country_annual) && "PY" %in% names(country_annual)) {
      country_annual$year <- as.integer(country_annual$PY)
    }
    if (!"article_count" %in% names(country_annual) && "n_articles" %in% names(country_annual)) {
      country_annual$article_count <- suppressWarnings(as.numeric(country_annual$n_articles))
    }
    if (!"n_articles" %in% names(country_annual) && "article_count" %in% names(country_annual)) {
      country_annual$n_articles <- suppressWarnings(as.numeric(country_annual$article_count))
    }
  }

  output$country_annual <- country_annual
  output$country_year_production <- country_annual

  output$country_summary <- m3_prepare_country_summary_for_hypotheses(
    output$country_summary,
    country_annual
  )

  if (is.null(output$gini_over_time) && nrow(country_annual) > 0) {
    output$gini_over_time <- m3_build_country_gini_over_time(country_annual)
  }

  if (is.null(output$scp_mcp_by_country) && !is.null(output$country_doc_level) && nrow(output$country_doc_level) > 0) {
    output$scp_mcp_by_country <- m3_build_scp_mcp_by_country(output$country_doc_level)
  }

  if (is.null(output$collaboration_matrix) && !is.null(output$country_doc_level) && nrow(output$country_doc_level) > 0) {
    output$collaboration_matrix <- build_collaboration_matrix(output$country_doc_level)
  }

  if (is.null(output$collaboration_indices) && !is.null(output$country_doc_level) && nrow(output$country_doc_level) > 0) {
    collab <- compute_m3_collaboration_indices(output, config)
    output$collaboration_indices <- collab$indices %||% tibble::tibble()
  }

  output
}

m3_prepare_country_summary_for_hypotheses <- function(country_summary, country_annual) {
  if (is.null(country_summary)) {
    country_summary <- tibble::tibble()
  } else {
    country_summary <- tibble::as_tibble(country_summary)
  }

  if (nrow(country_summary) == 0) {
    return(country_summary)
  }

  if (!"article_count" %in% names(country_summary) && "total_articles" %in% names(country_summary)) {
    country_summary$article_count <- suppressWarnings(as.numeric(country_summary$total_articles))
  }
  if (!"total_articles" %in% names(country_summary) && "article_count" %in% names(country_summary)) {
    country_summary$total_articles <- suppressWarnings(as.numeric(country_summary$article_count))
  }
  if (!"total_citations" %in% names(country_summary)) {
    country_summary$total_citations <- 0
  }
  if (!"mean_citations" %in% names(country_summary)) {
    denom <- suppressWarnings(as.numeric(country_summary$total_articles))
    numer <- suppressWarnings(as.numeric(country_summary$total_citations))
    country_summary$mean_citations <- ifelse(!is.na(denom) & denom > 0, numer / denom, NA_real_)
  }

  if (nrow(country_annual) > 0 && all(c("country", "year", "article_count") %in% names(country_annual))) {
    trend_features <- m3_derive_country_trend_features(country_annual)
    country_summary <- dplyr::left_join(country_summary, trend_features, by = "country")
  } else {
    if (!"n_years" %in% names(country_summary)) {
      country_summary$n_years <- 1L
    }
    if (!"annual_growth_rate" %in% names(country_summary)) {
      country_summary$annual_growth_rate <- NA_real_
    }
    if (!"trend_direction" %in% names(country_summary)) {
      country_summary$trend_direction <- "stable"
    }
  }

  country_summary
}

m3_derive_country_trend_features <- function(country_annual) {
  split_rows <- split(country_annual, country_annual$country)

  rows <- lapply(names(split_rows), function(country_name) {
    dat <- split_rows[[country_name]]
    dat <- dat[order(dat$year), , drop = FALSE]
    values <- suppressWarnings(as.numeric(dat$article_count))
    years <- suppressWarnings(as.numeric(dat$year))
    n_years <- sum(!is.na(years) & !is.na(values))

    slope <- NA_real_
    growth_rate <- NA_real_
    trend_direction <- "stable"

    if (n_years >= 2) {
      fit <- tryCatch(lm(values ~ years), error = function(e) NULL)
      if (!is.null(fit)) {
        slope <- suppressWarnings(as.numeric(coef(fit)[2]))
        mean_val <- mean(values, na.rm = TRUE)
        growth_rate <- if (is.finite(mean_val) && mean_val > 0 && is.finite(slope)) {
          slope / mean_val * 100
        } else {
          NA_real_
        }
        trend_direction <- if (!is.finite(slope) || abs(slope) < 1e-8) {
          "stable"
        } else if (slope > 0) {
          "increasing"
        } else {
          "decreasing"
        }
      }
    }

    tibble::tibble(
      country = country_name,
      n_years = n_years,
      annual_growth_rate = growth_rate,
      trend_direction = trend_direction
    )
  })

  dplyr::bind_rows(rows)
}

m3_build_country_gini_over_time <- function(country_annual) {
  yearly <- split(country_annual, country_annual$year)
  gini_values <- vapply(yearly, function(dat) {
    x <- suppressWarnings(as.numeric(dat$article_count))
    x <- x[is.finite(x) & x >= 0]
    if (length(x) < 2 || sum(x) <= 0) {
      return(0)
    }
    calculate_gini(x)
  }, numeric(1))
  gini_values[order(as.numeric(names(gini_values)))]
}

m3_build_scp_mcp_by_country <- function(country_doc_level) {
  valid <- country_doc_level[!is.na(country_doc_level$doc_id) & !is.na(country_doc_level$country), , drop = FALSE]
  if (nrow(valid) == 0) {
    return(tibble::tibble())
  }

  doc_groups <- split(valid$country, valid$doc_id)
  rows <- lapply(names(doc_groups), function(doc_id) {
    countries <- unique(doc_groups[[doc_id]])
    if (length(countries) == 0) {
      return(NULL)
    }
    tibble::tibble(
      country = countries,
      scp = as.integer(length(countries) == 1),
      mcp = as.integer(length(countries) > 1)
    )
  })

  summary <- dplyr::bind_rows(rows) %>%
    dplyr::group_by(country) %>%
    dplyr::summarize(
      scp = sum(scp, na.rm = TRUE),
      mcp = sum(mcp, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      total = scp + mcp,
      mcp_ratio = ifelse(total > 0, mcp / total, NA_real_)
    )

  summary
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
