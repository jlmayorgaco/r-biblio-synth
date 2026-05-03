# ============================================================================
# m3_compute_advanced_journal.R - Optional journal-grade M3 analytics
# ============================================================================

m3_compute_advanced_journal <- function(prepared_data, data = list(), config = biblio_config()) {
  config <- merge_biblio_config(config)

  if (!isTRUE(config$advanced_analytics)) {
    return(m3_advanced_journal_empty("disabled", "advanced_analytics is FALSE."))
  }

  summary <- prepared_data$country_summary %||% tibble::tibble()
  min_countries <- as.integer(config$min_countries_for_advanced_geo %||% 8)
  n_countries <- if (is.data.frame(summary)) sum(!is.na(summary$country)) else 0L
  if (n_countries < min_countries) {
    return(m3_advanced_journal_empty(
      "insufficient_data",
      sprintf("Advanced geographic analytics require at least %d countries; received %d.", min_countries, n_countries)
    ))
  }

  tryCatch({
    collaboration_premium <- m3_compute_collaboration_premium(prepared_data, config)
    mobility <- m3_compute_country_mobility_advanced(prepared_data, data$temporal_dynamics %||% list(), config)
    trajectories <- m3_compute_country_trajectories_advanced(prepared_data, mobility, config)
    geo_concentration <- m3_compute_geo_concentration_advanced(prepared_data, config)
    regional_decomposition <- m3_compute_regional_decomposition_advanced(prepared_data, collaboration_premium, config)
    normalization_optional <- m3_compute_normalization_optional(prepared_data, config)
    country_metrics <- m3_compute_country_advanced_metrics(prepared_data, collaboration_premium, mobility, trajectories, config)
    collaboration_network <- m3_compute_country_collaboration_network_advanced(prepared_data, country_metrics, config)
    scp_mcp_trends <- m3_compute_scp_mcp_trends_advanced(prepared_data, config)
    uncertainty <- m3_compute_uncertainty_advanced(prepared_data, collaboration_premium, mobility, geo_concentration, config)
    robustness <- m3_compute_robustness_advanced(prepared_data, country_metrics, config)
    inequality_decomposition <- m3_compute_inequality_decomposition_advanced(prepared_data, country_metrics, config)
    trend_models <- m3_compute_country_trend_models_advanced(country_metrics, config)
    outliers <- m3_compute_geographic_outliers_advanced(country_metrics, config)
    spatial_autocorrelation <- m3_compute_spatial_autocorrelation_advanced(country_metrics, config)
    collaboration_backbone <- m3_compute_collaboration_backbone_advanced(collaboration_network, country_metrics, config)
    gravity_model <- m3_compute_gravity_collaboration_model(country_metrics, collaboration_network, config)
    country_roles <- m3_compute_country_role_labels_advanced(country_metrics, collaboration_network, trend_models, outliers, config)
    hypotheses <- m3_advanced_hypotheses(
      collaboration_premium,
      mobility,
      trajectories,
      geo_concentration,
      regional_decomposition,
      country_metrics,
      scp_mcp_trends,
      uncertainty,
      robustness,
      trend_models,
      outliers,
      spatial_autocorrelation,
      collaboration_backbone,
      gravity_model,
      country_roles
    )

    list(
      status = "success",
      reason = NA_character_,
      collaboration_premium = collaboration_premium,
      mobility = mobility,
      trajectories = trajectories,
      geo_concentration = geo_concentration,
      regional_decomposition = regional_decomposition,
      normalization_optional = normalization_optional,
      country_metrics = country_metrics,
      collaboration_network = collaboration_network,
      scp_mcp_trends = scp_mcp_trends,
      uncertainty = uncertainty,
      robustness = robustness,
      inequality_decomposition = inequality_decomposition,
      trend_models = trend_models,
      outliers = outliers,
      spatial_autocorrelation = spatial_autocorrelation,
      collaboration_backbone = collaboration_backbone,
      gravity_model = gravity_model,
      country_roles = country_roles,
      hypotheses = hypotheses
    )
  }, error = function(e) {
    if (identical(config$advanced_fail_policy %||% "soft", "hard")) {
      stop(e)
    }
    out <- m3_advanced_journal_empty("error", e$message)
    out$error <- e$message
    out
  })
}

m3_advanced_journal_empty <- function(status, reason) {
  hypotheses <- m3_advanced_hypothesis_bundle(list(
    M3_H09 = m3_advanced_hypothesis("M3_H09", "Does MCP have a citation premium over SCP?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M3_H10 = m3_advanced_hypothesis("M3_H10", "Is geographic production more concentrated than a uniform distribution?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M3_H11 = m3_advanced_hypothesis("M3_H11", "Does country leadership change significantly between temporal windows?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M3_H12 = m3_advanced_hypothesis("M3_H12", "Are there emerging countries with statistically relevant share growth?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M3_H13 = m3_advanced_hypothesis("M3_H13", "Do regions differ in international collaboration?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M3_H14 = m3_advanced_hypothesis("M3_H14", "Are country rankings robust to full versus fractional counting?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M3_H15 = m3_advanced_hypothesis("M3_H15", "Are there countries with disproportionate citation impact relative to production?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M3_H16 = m3_advanced_hypothesis("M3_H16", "Is international collaboration increasing over time?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M3_H17 = m3_advanced_hypothesis("M3_H17", "Did top-country dominance change between temporal windows?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M3_H18 = m3_advanced_hypothesis("M3_H18", "Do leading countries show statistically positive production trends?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M3_H19 = m3_advanced_hypothesis("M3_H19", "Does country impact show spatial or regional autocorrelation?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M3_H20 = m3_advanced_hypothesis("M3_H20", "Is the collaboration backbone concentrated in a small set of country pairs?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M3_H21 = m3_advanced_hypothesis("M3_H21", "Does output mass explain collaboration intensity in an exploratory gravity model?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason),
    M3_H22 = m3_advanced_hypothesis("M3_H22", "Can countries be assigned interpretable bibliometric roles?", "not_estimable", NA_real_, NA_character_, NA_real_, "inconclusive", reason)
  ))

  list(
    status = status,
    reason = reason,
    collaboration_premium = list(status = status, reason = reason, table = m3_empty_collaboration_premium_table(), bootstrap = tibble::tibble()),
    mobility = list(status = status, reason = reason, rank_windows = m3_empty_rank_mobility_table(), rank_volatility = tibble::tibble(), leadership_persistence = tibble::tibble(), rank_stability = tibble::tibble()),
    trajectories = list(status = status, reason = reason, table = m3_empty_country_trajectory_table()),
    geo_concentration = list(status = status, reason = reason, table = m3_empty_geo_concentration_table(), lorenz = tibble::tibble()),
    regional_decomposition = list(status = status, reason = reason, table = m3_empty_regional_decomposition_table(), region_year = tibble::tibble()),
    normalization_optional = list(status = "not_available", reason = "No external country normalization dataset was provided.", table = tibble::tibble()),
    country_metrics = list(status = status, reason = reason, country_table = tibble::tibble(), country_year = tibble::tibble(), contribution = tibble::tibble(), country_premium = tibble::tibble(), windowed_mcp = tibble::tibble(), world_map_metrics = tibble::tibble()),
    collaboration_network = list(status = status, reason = reason, nodes = tibble::tibble(), edges = tibble::tibble(), layout = tibble::tibble()),
    scp_mcp_trends = list(status = status, reason = reason, annual = tibble::tibble(), trend = tibble::tibble(), country_windows = tibble::tibble()),
    uncertainty = list(status = status, reason = reason, intervals = tibble::tibble(), share_change_bootstrap = tibble::tibble(), dominance_bootstrap = tibble::tibble()),
    robustness = list(status = status, reason = reason, rank_sensitivity = tibble::tibble(), topn_sensitivity = tibble::tibble(), quadrant_robustness = tibble::tibble()),
    inequality_decomposition = list(status = status, reason = reason, window_metrics = tibble::tibble(), year_metrics = tibble::tibble(), regional_concentration = tibble::tibble(), regional_inequality_contribution = tibble::tibble()),
    trend_models = list(status = status, reason = reason, table = tibble::tibble()),
    outliers = list(status = status, reason = reason, table = tibble::tibble()),
    spatial_autocorrelation = list(status = status, reason = reason, global = tibble::tibble(), lisa = tibble::tibble()),
    collaboration_backbone = list(status = status, reason = reason, nodes = tibble::tibble(), edges = tibble::tibble(), communities = tibble::tibble()),
    gravity_model = list(status = status, reason = reason, table = tibble::tibble(), coefficients = tibble::tibble()),
    country_roles = list(status = status, reason = reason, table = tibble::tibble()),
    hypotheses = hypotheses
  )
}

m3_compute_collaboration_premium <- function(prepared_data, config) {
  doc_level <- prepared_data$country_doc_level %||% tibble::tibble()
  if (!is.data.frame(doc_level) || nrow(doc_level) == 0 || !"doc_id" %in% names(doc_level)) {
    return(list(status = "insufficient_data", reason = "Document-country data were unavailable.", table = m3_empty_collaboration_premium_table(), bootstrap = tibble::tibble(), doc_table = tibble::tibble()))
  }

  doc_table <- doc_level |>
    dplyr::filter(!is.na(.data$country), nzchar(as.character(.data$country))) |>
    dplyr::group_by(.data$doc_id) |>
    dplyr::summarise(
      countries = dplyr::n_distinct(.data$country),
      citations = suppressWarnings(max(as.numeric(.data$TC), na.rm = TRUE)),
      year = suppressWarnings(min(as.numeric(.data$year %||% .data$PY), na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      collaboration_type = ifelse(.data$countries > 1, "MCP", "SCP"),
      citations = ifelse(is.finite(.data$citations), .data$citations, NA_real_)
    )

  if (nrow(doc_table) == 0 || length(unique(doc_table$collaboration_type)) < 2) {
    return(list(status = "insufficient_data", reason = "Both SCP and MCP document groups are required.", table = m3_empty_collaboration_premium_table(), bootstrap = tibble::tibble(), doc_table = doc_table))
  }

  scp <- doc_table$citations[doc_table$collaboration_type == "SCP"]
  mcp <- doc_table$citations[doc_table$collaboration_type == "MCP"]
  scp <- scp[is.finite(scp)]
  mcp <- mcp[is.finite(mcp)]
  if (length(scp) < 2 || length(mcp) < 2) {
    return(list(status = "insufficient_data", reason = "Citation data were too sparse for SCP/MCP comparison.", table = m3_empty_collaboration_premium_table(), bootstrap = tibble::tibble(), doc_table = doc_table))
  }

  bootstrap_n <- as.integer(config$bootstrap_n %||% 500)
  set.seed(314159)
  boot <- replicate(bootstrap_n, {
    mean(sample(mcp, length(mcp), replace = TRUE), na.rm = TRUE) -
      mean(sample(scp, length(scp), replace = TRUE), na.rm = TRUE)
  })
  ci <- stats::quantile(boot, probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE)
  pooled_sd <- sqrt(((length(mcp) - 1) * stats::var(mcp) + (length(scp) - 1) * stats::var(scp)) / max(1, length(mcp) + length(scp) - 2))
  cohen_d <- if (is.finite(pooled_sd) && pooled_sd > 0) (mean(mcp) - mean(scp)) / pooled_sd else NA_real_
  p_value <- tryCatch(stats::wilcox.test(mcp, scp, exact = FALSE)$p.value, error = function(e) NA_real_)

  table <- tibble::tibble(
    comparison = "MCP_vs_SCP",
    n_mcp = length(mcp),
    n_scp = length(scp),
    mean_mcp_citations = mean(mcp, na.rm = TRUE),
    mean_scp_citations = mean(scp, na.rm = TRUE),
    mean_difference = mean(mcp, na.rm = TRUE) - mean(scp, na.rm = TRUE),
    ratio = mean(mcp, na.rm = TRUE) / max(mean(scp, na.rm = TRUE), .Machine$double.eps),
    cohen_d = cohen_d,
    ci_low = ci[1],
    ci_high = ci[2],
    p_value = p_value,
    method = "wilcoxon_bootstrap_ci"
  )

  list(
    status = "success",
    reason = NA_character_,
    table = table,
    bootstrap = tibble::tibble(iteration = seq_along(boot), mean_difference = as.numeric(boot)),
    doc_table = doc_table
  )
}

m3_compute_country_mobility_advanced <- function(prepared_data, temporal_dynamics, config) {
  annual <- prepared_data$country_annual %||% tibble::tibble()
  if (!is.data.frame(annual) || nrow(annual) == 0 || !all(c("country", "year", "article_count") %in% names(annual))) {
    return(list(status = "insufficient_data", reason = "Country-year production data were unavailable.", rank_windows = m3_empty_rank_mobility_table(), rank_volatility = tibble::tibble(), leadership_persistence = tibble::tibble(), rank_stability = tibble::tibble(), annual_ranks = tibble::tibble()))
  }

  annual <- annual |>
    dplyr::mutate(year = suppressWarnings(as.numeric(.data$year)), article_count = suppressWarnings(as.numeric(.data$article_count))) |>
    dplyr::filter(is.finite(.data$year), is.finite(.data$article_count), !is.na(.data$country))

  years <- sort(unique(annual$year))
  if (length(years) < 3) {
    return(list(status = "insufficient_data", reason = "At least three years are needed for mobility analysis.", rank_windows = m3_empty_rank_mobility_table(), rank_volatility = tibble::tibble(), leadership_persistence = tibble::tibble(), rank_stability = tibble::tibble(), annual_ranks = tibble::tibble()))
  }

  annual_ranks <- annual |>
    dplyr::group_by(.data$year) |>
    dplyr::mutate(
      year_total = sum(.data$article_count, na.rm = TRUE),
      share = ifelse(.data$year_total > 0, .data$article_count / .data$year_total, 0),
      rank = dplyr::min_rank(dplyr::desc(.data$article_count))
    ) |>
    dplyr::ungroup()

  k <- max(2L, floor(length(years) / 3))
  first_years <- utils::head(years, k)
  last_years <- utils::tail(years, k)
  window_rank <- function(target_years, suffix) {
    out <- annual_ranks |>
      dplyr::filter(.data$year %in% target_years) |>
      dplyr::group_by(.data$country) |>
      dplyr::summarise(
        articles = sum(.data$article_count, na.rm = TRUE),
        share = mean(.data$share, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(rank = dplyr::min_rank(dplyr::desc(.data$articles)))
    names(out)[names(out) == "articles"] <- paste0("articles_", suffix)
    names(out)[names(out) == "share"] <- paste0("share_", suffix)
    names(out)[names(out) == "rank"] <- paste0("rank_", suffix)
    out
  }

  first <- window_rank(first_years, "first")
  last <- window_rank(last_years, "last")
  rank_windows <- dplyr::full_join(first, last, by = "country") |>
    dplyr::mutate(
      rank_first = ifelse(is.na(.data$rank_first), max(.data$rank_first, .data$rank_last, na.rm = TRUE) + 1, .data$rank_first),
      rank_last = ifelse(is.na(.data$rank_last), max(.data$rank_first, .data$rank_last, na.rm = TRUE) + 1, .data$rank_last),
      rank_change = .data$rank_first - .data$rank_last,
      share_change = (.data$share_last %||% 0) - (.data$share_first %||% 0),
      direction = dplyr::case_when(.data$rank_change > 0 ~ "improved", .data$rank_change < 0 ~ "declined", TRUE ~ "stable")
    ) |>
    dplyr::arrange(.data$rank_last)

  rank_volatility <- annual_ranks |>
    dplyr::group_by(.data$country) |>
    dplyr::summarise(
      active_years = dplyr::n_distinct(.data$year),
      rank_volatility = stats::sd(.data$rank, na.rm = TRUE),
      mean_rank = mean(.data$rank, na.rm = TRUE),
      .groups = "drop"
    )

  leaders <- annual_ranks |>
    dplyr::group_by(.data$year) |>
    dplyr::slice_min(.data$rank, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
  leadership_persistence <- leaders |>
    dplyr::count(.data$country, name = "leader_years") |>
    dplyr::mutate(leadership_share = .data$leader_years / length(years)) |>
    dplyr::arrange(dplyr::desc(.data$leader_years))

  paired <- rank_windows |>
    dplyr::filter(is.finite(.data$rank_first), is.finite(.data$rank_last))
  spearman <- if (nrow(paired) >= 3) tryCatch(stats::cor.test(paired$rank_first, paired$rank_last, method = "spearman", exact = FALSE), error = function(e) NULL) else NULL
  kendall <- if (nrow(paired) >= 3) tryCatch(stats::cor.test(paired$rank_first, paired$rank_last, method = "kendall", exact = FALSE), error = function(e) NULL) else NULL
  rank_stability <- tibble::tibble(
    method = c("spearman", "kendall"),
    estimate = c(if (!is.null(spearman)) unname(spearman$estimate) else NA_real_, if (!is.null(kendall)) unname(kendall$estimate) else NA_real_),
    p_value = c(if (!is.null(spearman)) spearman$p.value else NA_real_, if (!is.null(kendall)) kendall$p.value else NA_real_),
    n_countries = nrow(paired)
  )

  list(
    status = "success",
    reason = NA_character_,
    rank_windows = rank_windows,
    rank_volatility = rank_volatility,
    leadership_persistence = leadership_persistence,
    rank_stability = rank_stability,
    annual_ranks = annual_ranks
  )
}

m3_compute_country_trajectories_advanced <- function(prepared_data, mobility, config) {
  rank_windows <- mobility$rank_windows %||% tibble::tibble()
  volatility <- mobility$rank_volatility %||% tibble::tibble()
  annual <- mobility$annual_ranks %||% tibble::tibble()
  if (!is.data.frame(rank_windows) || nrow(rank_windows) == 0) {
    return(list(status = "insufficient_data", reason = "Rank-window mobility table was unavailable.", table = m3_empty_country_trajectory_table()))
  }

  first_active <- if (is.data.frame(annual) && nrow(annual) > 0) {
    annual |>
      dplyr::group_by(.data$country) |>
      dplyr::summarise(first_active_year = min(.data$year, na.rm = TRUE), .groups = "drop")
  } else {
    tibble::tibble(country = character(), first_active_year = numeric())
  }
  min_year <- if (nrow(annual) > 0) min(annual$year, na.rm = TRUE) else NA_real_
  max_year <- if (nrow(annual) > 0) max(annual$year, na.rm = TRUE) else NA_real_
  late_cut <- if (is.finite(min_year) && is.finite(max_year)) min_year + 0.6 * (max_year - min_year) else Inf

  table <- rank_windows |>
    dplyr::left_join(volatility, by = "country") |>
    dplyr::left_join(first_active, by = "country") |>
    dplyr::mutate(
      share_first = dplyr::coalesce(.data$share_first, 0),
      share_last = dplyr::coalesce(.data$share_last, 0),
      share_change = .data$share_last - .data$share_first,
      relative_share_growth = ifelse(.data$share_first > 0, .data$share_change / .data$share_first, NA_real_),
      late_entry = is.finite(.data$first_active_year) & .data$first_active_year >= late_cut,
      trajectory_class = dplyr::case_when(
        .data$late_entry & .data$share_last > 0 ~ "late_entry",
        .data$share_change > 0.01 & (.data$relative_share_growth > 0.25 | is.na(.data$relative_share_growth)) ~ "emerging",
        .data$share_change < -0.01 ~ "declining",
        is.finite(.data$rank_volatility) & .data$rank_volatility >= stats::quantile(.data$rank_volatility, 0.75, na.rm = TRUE) ~ "volatile",
        TRUE ~ "stable"
      )
    ) |>
    dplyr::arrange(dplyr::desc(abs(.data$share_change)))

  list(status = "success", reason = NA_character_, table = table)
}

m3_compute_geo_concentration_advanced <- function(prepared_data, config) {
  summary <- prepared_data$country_summary %||% tibble::tibble()
  if (!is.data.frame(summary) || nrow(summary) == 0 || !"article_count" %in% names(summary)) {
    return(list(status = "insufficient_data", reason = "Country summary was unavailable.", table = m3_empty_geo_concentration_table(), lorenz = tibble::tibble()))
  }

  build_metric <- function(values, metric_name) {
    values <- suppressWarnings(as.numeric(values))
    values <- values[is.finite(values) & values >= 0]
    n <- length(values)
    total <- sum(values)
    if (n == 0 || total <= 0) {
      return(tibble::tibble(metric = metric_name, n_countries = n, total = total, gini = NA_real_, theil = NA_real_, hhi = NA_real_, palma_ratio = NA_real_, top5_share = NA_real_, top10_share = NA_real_, uniform_hhi = NA_real_, chisq_p_value = NA_real_))
    }
    shares <- values / total
    sorted <- sort(values)
    top_desc <- sort(values, decreasing = TRUE)
    bottom_n <- max(1, floor(0.4 * n))
    top_n_palma <- max(1, ceiling(0.1 * n))
    expected <- rep(total / n, n)
    chisq_p <- tryCatch(
      suppressWarnings(stats::chisq.test(values, p = rep(1 / n, n), rescale.p = TRUE)$p.value),
      error = function(e) NA_real_
    )
    tibble::tibble(
      metric = metric_name,
      n_countries = n,
      total = total,
      gini = m3_gini(values),
      theil = m3_theil(values),
      hhi = sum(shares^2),
      palma_ratio = sum(utils::head(top_desc, top_n_palma)) / max(sum(utils::head(sorted, bottom_n)), .Machine$double.eps),
      top5_share = sum(utils::head(top_desc, min(5, n))) / total,
      top10_share = sum(utils::head(top_desc, min(10, n))) / total,
      uniform_hhi = 1 / n,
      chisq_p_value = chisq_p
    )
  }

  table <- dplyr::bind_rows(
    build_metric(summary$article_count, "production"),
    build_metric(summary$total_citations %||% rep(0, nrow(summary)), "citations")
  )

  lorenz <- summary |>
    dplyr::arrange(.data$article_count) |>
    dplyr::mutate(
      cumulative_countries = dplyr::row_number() / dplyr::n(),
      cumulative_production = cumsum(.data$article_count) / sum(.data$article_count, na.rm = TRUE)
    ) |>
    dplyr::select("country", "cumulative_countries", "cumulative_production")

  list(status = "success", reason = NA_character_, table = table, lorenz = lorenz)
}

m3_compute_regional_decomposition_advanced <- function(prepared_data, collaboration_premium, config) {
  summary <- prepared_data$country_summary %||% tibble::tibble()
  annual <- prepared_data$country_annual %||% tibble::tibble()
  if (!is.data.frame(summary) || nrow(summary) == 0) {
    return(list(status = "insufficient_data", reason = "Country summary was unavailable.", table = m3_empty_regional_decomposition_table(), region_year = tibble::tibble()))
  }

  country_region <- tibble::tibble(
    country = summary$country,
    region = vapply(summary$country, m3_region_for_country, character(1))
  )

  table <- summary |>
    dplyr::left_join(country_region, by = "country") |>
    dplyr::group_by(.data$region) |>
    dplyr::summarise(
      n_countries = dplyr::n_distinct(.data$country),
      article_count = sum(.data$article_count, na.rm = TRUE),
      total_citations = sum(.data$total_citations, na.rm = TRUE),
      citations_per_article = .data$total_citations / pmax(.data$article_count, .Machine$double.eps),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      production_share = .data$article_count / sum(.data$article_count, na.rm = TRUE),
      citation_share = .data$total_citations / sum(.data$total_citations, na.rm = TRUE)
    ) |>
    dplyr::arrange(dplyr::desc(.data$article_count))

  doc_table <- collaboration_premium$doc_table %||% tibble::tibble()
  if (is.data.frame(doc_table) && nrow(doc_table) > 0) {
    country_doc <- prepared_data$country_doc_level |>
      dplyr::select("doc_id", "country") |>
      dplyr::distinct() |>
      dplyr::left_join(country_region, by = "country") |>
      dplyr::left_join(doc_table |> dplyr::select("doc_id", "collaboration_type"), by = "doc_id")
    regional_collab <- country_doc |>
      dplyr::filter(!is.na(.data$region), !is.na(.data$collaboration_type)) |>
      dplyr::group_by(.data$region) |>
      dplyr::summarise(mcp_ratio = mean(.data$collaboration_type == "MCP", na.rm = TRUE), .groups = "drop")
    table <- table |> dplyr::left_join(regional_collab, by = "region")
  } else {
    table$mcp_ratio <- NA_real_
  }

  region_year <- tibble::tibble()
  if (is.data.frame(annual) && nrow(annual) > 0) {
    region_year <- annual |>
      dplyr::left_join(country_region, by = "country") |>
      dplyr::filter(!is.na(.data$region)) |>
      dplyr::group_by(.data$region, .data$year) |>
      dplyr::summarise(article_count = sum(.data$article_count, na.rm = TRUE), .groups = "drop") |>
      dplyr::group_by(.data$year) |>
      dplyr::mutate(share = .data$article_count / sum(.data$article_count, na.rm = TRUE)) |>
      dplyr::ungroup()
  }

  list(status = "success", reason = NA_character_, table = table, region_year = region_year)
}

m3_compute_normalization_optional <- function(prepared_data, config) {
  external <- config$country_normalization_data %||% NULL
  if (!is.data.frame(external) || !"country" %in% names(external)) {
    return(list(status = "not_available", reason = "No external country normalization dataset was provided.", table = tibble::tibble()))
  }
  summary <- prepared_data$country_summary %||% tibble::tibble()
  optional_cols <- c("population", "gdp", "rd_expenditure", "researchers_per_million", "university_output")
  for (col in optional_cols) {
    if (!col %in% names(external)) external[[col]] <- NA_real_
  }
  table <- summary |>
    dplyr::left_join(external, by = "country") |>
    dplyr::mutate(
      articles_per_million = ifelse(is.finite(.data$population) & .data$population > 0, .data$article_count / (.data$population / 1e6), NA_real_),
      citations_per_billion_gdp = ifelse(is.finite(.data$gdp) & .data$gdp > 0, .data$total_citations / (.data$gdp / 1e9), NA_real_),
      articles_per_billion_gdp = ifelse(is.finite(.data$gdp) & .data$gdp > 0, .data$article_count / (.data$gdp / 1e9), NA_real_),
      articles_per_billion_rd = ifelse(is.finite(.data$rd_expenditure) & .data$rd_expenditure > 0, .data$article_count / (.data$rd_expenditure / 1e9), NA_real_),
      citations_per_billion_rd = ifelse(is.finite(.data$rd_expenditure) & .data$rd_expenditure > 0, .data$total_citations / (.data$rd_expenditure / 1e9), NA_real_),
      articles_per_1000_researchers = ifelse(is.finite(.data$researchers_per_million) & .data$researchers_per_million > 0 & is.finite(.data$population) & .data$population > 0,
                                             .data$article_count / ((.data$researchers_per_million * .data$population / 1e6) / 1000), NA_real_),
      articles_per_university_output = ifelse(is.finite(.data$university_output) & .data$university_output > 0, .data$article_count / .data$university_output, NA_real_)
    )
  list(status = "success", reason = NA_character_, table = table)
}

m3_compute_country_advanced_metrics <- function(prepared_data, collaboration_premium, mobility, trajectories, config) {
  doc_country <- m3_doc_country_table(prepared_data)
  annual <- prepared_data$country_annual %||% tibble::tibble()
  annual_citations <- prepared_data$country_annual_citations %||% tibble::tibble()

  if (nrow(doc_country) == 0) {
    return(list(
      status = "insufficient_data",
      reason = "Document-country data were unavailable.",
      country_table = tibble::tibble(),
      country_year = tibble::tibble(),
      contribution = tibble::tibble(),
      country_premium = tibble::tibble(),
      windowed_mcp = tibble::tibble(),
      world_map_metrics = tibble::tibble()
    ))
  }

  doc_counts <- doc_country |>
    dplyr::count(.data$doc_id, name = "n_countries")
  doc_country <- doc_country |>
    dplyr::left_join(doc_counts, by = "doc_id") |>
    dplyr::mutate(
      fractional_weight = ifelse(.data$n_countries > 0, 1 / .data$n_countries, 0),
      collaboration_type = ifelse(.data$n_countries > 1, "MCP", "SCP")
    )

  max_year <- suppressWarnings(max(doc_country$year, na.rm = TRUE))
  if (!is.finite(max_year)) max_year <- NA_real_

  full_counts <- doc_country |>
    dplyr::distinct(.data$doc_id, .data$country, .keep_all = TRUE) |>
    dplyr::group_by(.data$country) |>
    dplyr::summarise(
      full_articles = dplyr::n(),
      total_citations = sum(.data$TC, na.rm = TRUE),
      .groups = "drop"
    )

  fractional_counts <- doc_country |>
    dplyr::group_by(.data$country) |>
    dplyr::summarise(
      fractional_articles = sum(.data$fractional_weight, na.rm = TRUE),
      fractional_citations = sum(.data$TC * .data$fractional_weight, na.rm = TRUE),
      .groups = "drop"
    )

  citation_age <- doc_country |>
    dplyr::distinct(.data$doc_id, .data$country, .keep_all = TRUE) |>
    dplyr::mutate(
      paper_age = if (is.finite(max_year)) pmax(1, max_year - .data$year + 1) else NA_real_,
      citations_per_year_doc = ifelse(is.finite(.data$paper_age), .data$TC / .data$paper_age, NA_real_)
    ) |>
    dplyr::group_by(.data$country) |>
    dplyr::summarise(
      citations_per_year = mean(.data$citations_per_year_doc, na.rm = TRUE),
      age_normalized_impact = sum(.data$citations_per_year_doc, na.rm = TRUE) / dplyr::n_distinct(.data$doc_id),
      .groups = "drop"
    )

  scp_mcp_country <- doc_country |>
    dplyr::distinct(.data$doc_id, .data$country, .data$collaboration_type, .data$TC, .keep_all = TRUE) |>
    dplyr::group_by(.data$country, .data$collaboration_type) |>
    dplyr::summarise(
      articles = dplyr::n(),
      mean_citations = mean(.data$TC, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = "collaboration_type",
      values_from = c("articles", "mean_citations"),
      values_fill = list(articles = 0, mean_citations = NA_real_)
    )
  for (nm in c("articles_SCP", "articles_MCP")) {
    if (!nm %in% names(scp_mcp_country)) scp_mcp_country[[nm]] <- 0
  }
  for (nm in c("mean_citations_SCP", "mean_citations_MCP")) {
    if (!nm %in% names(scp_mcp_country)) scp_mcp_country[[nm]] <- NA_real_
  }
  scp_mcp_country <- scp_mcp_country |>
    dplyr::transmute(
      country = .data$country,
      scp_articles = .data$articles_SCP,
      mcp_articles = .data$articles_MCP,
      scp_citations_mean = .data$mean_citations_SCP,
      mcp_citations_mean = .data$mean_citations_MCP
    )

  country_premium <- m3_compute_country_collaboration_premium_table(doc_country, config)
  mobility_table <- mobility$rank_windows %||% tibble::tibble()
  if (is.data.frame(mobility_table) && nrow(mobility_table) > 0) {
    mobility_table <- mobility_table |>
      dplyr::select(dplyr::any_of(c("country", "articles_first", "share_first", "rank_first", "articles_last", "share_last", "rank_last", "rank_change", "share_change", "direction")))
  }
  trajectory_table <- trajectories$table %||% tibble::tibble()
  if (is.data.frame(trajectory_table) && nrow(trajectory_table) > 0) {
    trajectory_table <- trajectory_table |>
      dplyr::select(dplyr::any_of(c("country", "relative_share_growth", "rank_volatility", "mean_rank", "first_active_year", "late_entry", "trajectory_class")))
  }
  leadership <- m3_compute_leadership_windows(mobility$annual_ranks %||% tibble::tibble())

  pieces <- Filter(function(x) is.data.frame(x) && nrow(x) > 0 && "country" %in% names(x), list(
    full_counts,
    fractional_counts,
    citation_age,
    scp_mcp_country,
    country_premium,
    mobility_table,
    trajectory_table,
    leadership
  ))
  country_table <- Reduce(function(x, y) dplyr::full_join(x, y, by = "country"), pieces)

  for (col in c("age_normalized_impact", "citations_per_year", "relative_share_growth", "share_first", "share_last", "rank_first", "rank_last")) {
    if (!col %in% names(country_table)) country_table[[col]] <- NA_real_
  }
  zero_cols <- c("full_articles", "total_citations", "fractional_articles", "fractional_citations", "scp_articles", "mcp_articles", "leader_years_top5", "leader_years_top10")
  for (col in zero_cols) {
    if (col %in% names(country_table)) country_table[[col]][is.na(country_table[[col]])] <- 0
  }

  country_table <- country_table |>
    dplyr::mutate(
      citations_per_article = ifelse(.data$full_articles > 0, .data$total_citations / .data$full_articles, NA_real_),
      production_share = ifelse(sum(.data$full_articles, na.rm = TRUE) > 0, .data$full_articles / sum(.data$full_articles, na.rm = TRUE), 0),
      citation_share = ifelse(sum(.data$total_citations, na.rm = TRUE) > 0, .data$total_citations / sum(.data$total_citations, na.rm = TRUE), 0),
      fractional_share = ifelse(sum(.data$fractional_articles, na.rm = TRUE) > 0, .data$fractional_articles / sum(.data$fractional_articles, na.rm = TRUE), 0),
      mcp_ratio = ifelse((.data$scp_articles + .data$mcp_articles) > 0, .data$mcp_articles / (.data$scp_articles + .data$mcp_articles), NA_real_),
      scp_share = ifelse(sum(.data$scp_articles, na.rm = TRUE) > 0, .data$scp_articles / sum(.data$scp_articles, na.rm = TRUE), 0),
      mcp_share = ifelse(sum(.data$mcp_articles, na.rm = TRUE) > 0, .data$mcp_articles / sum(.data$mcp_articles, na.rm = TRUE), 0),
      full_rank = dplyr::min_rank(dplyr::desc(.data$full_articles)),
      fractional_rank = dplyr::min_rank(dplyr::desc(.data$fractional_articles)),
      rank_sensitivity = .data$full_rank - .data$fractional_rank
    )

  window_shares <- m3_compute_country_window_shares(doc_country)
  if (nrow(window_shares) > 0) {
    country_table <- country_table |>
      dplyr::left_join(window_shares, by = "country") |>
      dplyr::mutate(
        share_acceleration = dplyr::coalesce(.data$share_late, 0) - 2 * dplyr::coalesce(.data$share_mid, 0) + dplyr::coalesce(.data$share_early, 0),
        top_entry = dplyr::coalesce(.data$rank_first > 10 & .data$rank_last <= 10, FALSE),
        top_exit = dplyr::coalesce(.data$rank_first <= 10 & .data$rank_last > 10, FALSE)
      )
  } else {
    country_table$share_acceleration <- NA_real_
    country_table$top_entry <- FALSE
    country_table$top_exit <- FALSE
  }

  country_year <- tibble::tibble()
  if (is.data.frame(annual) && nrow(annual) > 0 && all(c("country", "year", "article_count") %in% names(annual))) {
    country_year <- annual |>
      dplyr::select("country", "year", "article_count") |>
      dplyr::mutate(year = suppressWarnings(as.numeric(.data$year)), article_count = suppressWarnings(as.numeric(.data$article_count)))
    if (is.data.frame(annual_citations) && nrow(annual_citations) > 0 && all(c("country", "year", "total_citations") %in% names(annual_citations))) {
      country_year <- country_year |>
        dplyr::left_join(
          annual_citations |> dplyr::select("country", "year", "total_citations"),
          by = c("country", "year")
        )
    } else {
      country_year$total_citations <- NA_real_
    }
    country_year <- country_year |>
      dplyr::group_by(.data$year) |>
      dplyr::mutate(
        year_total = sum(.data$article_count, na.rm = TRUE),
        share = ifelse(.data$year_total > 0, .data$article_count / .data$year_total, 0)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        citations_per_article = ifelse(.data$article_count > 0, .data$total_citations / .data$article_count, NA_real_)
      )
  }

  contribution <- country_table |>
    dplyr::select("country", "production_share", "citation_share", "scp_share", "mcp_share", "fractional_share") |>
    tidyr::pivot_longer(-"country", names_to = "component", values_to = "share") |>
    dplyr::mutate(component = gsub("_", " ", .data$component, fixed = TRUE))

  windowed_mcp <- m3_compute_country_windowed_mcp(doc_country)
  world_map_metrics <- country_table |>
    dplyr::transmute(
      country = .data$country,
      article_count = .data$full_articles,
      normalized_impact = .data$age_normalized_impact,
      mcp_ratio = .data$mcp_ratio,
      growth = dplyr::coalesce(.data$relative_share_growth, .data$share_acceleration),
      total_citations = .data$total_citations
    )

  list(
    status = "success",
    reason = NA_character_,
    country_table = country_table |> dplyr::arrange(dplyr::desc(.data$full_articles)),
    country_year = country_year,
    contribution = contribution,
    country_premium = country_premium,
    windowed_mcp = windowed_mcp,
    world_map_metrics = world_map_metrics
  )
}

m3_compute_country_collaboration_network_advanced <- function(prepared_data, country_metrics, config) {
  doc_country <- m3_doc_country_table(prepared_data)
  if (nrow(doc_country) == 0) {
    return(list(status = "insufficient_data", reason = "Document-country data were unavailable.", nodes = tibble::tibble(), edges = tibble::tibble(), layout = tibble::tibble()))
  }

  countries_by_doc <- split(doc_country$country, doc_country$doc_id)
  edges <- dplyr::bind_rows(lapply(countries_by_doc, function(countries) {
    countries <- sort(unique(countries[!is.na(countries) & nzchar(countries)]))
    if (length(countries) < 2) return(tibble::tibble())
    pairs <- utils::combn(countries, 2)
    tibble::tibble(from = pairs[1, ], to = pairs[2, ], documents = 1)
  }))

  if (!is.data.frame(edges) || nrow(edges) == 0) {
    return(list(status = "insufficient_data", reason = "No international country pairs were observed.", nodes = tibble::tibble(), edges = tibble::tibble(), layout = tibble::tibble()))
  }

  edges <- edges |>
    dplyr::group_by(.data$from, .data$to) |>
    dplyr::summarise(weight = sum(.data$documents, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$weight))

  metrics <- country_metrics$country_table %||% tibble::tibble()
  nodes <- tibble::tibble(country = sort(unique(c(edges$from, edges$to)))) |>
    dplyr::left_join(metrics, by = "country") |>
    dplyr::mutate(
      full_articles = dplyr::coalesce(.data$full_articles, 0),
      mcp_ratio = dplyr::coalesce(.data$mcp_ratio, 0)
    )

  degree <- dplyr::bind_rows(
    edges |> dplyr::transmute(country = .data$from, degree_weight = .data$weight),
    edges |> dplyr::transmute(country = .data$to, degree_weight = .data$weight)
  ) |>
    dplyr::group_by(.data$country) |>
    dplyr::summarise(degree_weight = sum(.data$degree_weight, na.rm = TRUE), degree = dplyr::n(), .groups = "drop")

  nodes <- nodes |>
    dplyr::left_join(degree, by = "country") |>
    dplyr::mutate(
      degree_weight = dplyr::coalesce(.data$degree_weight, 0),
      degree = dplyr::coalesce(.data$degree, 0L)
    ) |>
    dplyr::arrange(dplyr::desc(.data$degree_weight), dplyr::desc(.data$full_articles))

  n <- nrow(nodes)
  layout <- nodes |>
    dplyr::mutate(
      angle = 2 * pi * (dplyr::row_number() - 1) / max(1, n),
      x = cos(.data$angle),
      y = sin(.data$angle)
    )

  list(status = "success", reason = NA_character_, nodes = nodes, edges = edges, layout = layout)
}

m3_compute_scp_mcp_trends_advanced <- function(prepared_data, config) {
  doc_country <- m3_doc_country_table(prepared_data)
  if (nrow(doc_country) == 0 || !any(is.finite(doc_country$year))) {
    return(list(status = "insufficient_data", reason = "Document-year country data were unavailable.", annual = tibble::tibble(), trend = tibble::tibble(), country_windows = tibble::tibble()))
  }

  doc_type <- doc_country |>
    dplyr::filter(is.finite(.data$year)) |>
    dplyr::group_by(.data$doc_id, .data$year) |>
    dplyr::summarise(n_countries = dplyr::n_distinct(.data$country), .groups = "drop") |>
    dplyr::mutate(collaboration_type = ifelse(.data$n_countries > 1, "MCP", "SCP"))

  annual <- doc_type |>
    dplyr::count(.data$year, .data$collaboration_type, name = "documents") |>
    tidyr::pivot_wider(names_from = "collaboration_type", values_from = "documents", values_fill = 0)
  if (!"SCP" %in% names(annual)) annual$SCP <- 0
  if (!"MCP" %in% names(annual)) annual$MCP <- 0
  annual <- annual |>
    dplyr::mutate(
      SCP = dplyr::coalesce(.data$SCP, 0),
      MCP = dplyr::coalesce(.data$MCP, 0),
      total = .data$SCP + .data$MCP,
      mcp_share = ifelse(.data$total > 0, .data$MCP / .data$total, NA_real_)
    ) |>
    dplyr::arrange(.data$year)

  trend <- tibble::tibble()
  valid <- annual[is.finite(annual$year) & is.finite(annual$mcp_share), , drop = FALSE]
  if (nrow(valid) >= 3 && stats::sd(valid$mcp_share, na.rm = TRUE) > 0) {
    fit <- stats::lm(mcp_share ~ year, data = valid)
    sm <- summary(fit)
    trend <- tibble::tibble(
      metric = "mcp_share",
      slope = unname(stats::coef(fit)[["year"]]),
      p_value = sm$coefficients["year", "Pr(>|t|)"],
      r_squared = sm$r.squared,
      n_years = nrow(valid)
    )
  }

  country_windows <- m3_compute_country_windowed_mcp(doc_country)
  list(status = "success", reason = NA_character_, annual = annual, trend = trend, country_windows = country_windows)
}

m3_compute_uncertainty_advanced <- function(prepared_data, collaboration_premium, mobility, geo_concentration, config) {
  doc_country <- m3_doc_country_table(prepared_data)
  bootstrap_n <- as.integer(config$bootstrap_n %||% 500)
  if (nrow(doc_country) == 0 || bootstrap_n < 10) {
    return(list(status = "insufficient_data", reason = "Bootstrap requires document-country rows and at least 10 iterations.", intervals = tibble::tibble(), share_change_bootstrap = tibble::tibble(), dominance_bootstrap = tibble::tibble()))
  }

  windows <- m3_country_window_years(doc_country$year)
  if (length(windows$early) == 0 || length(windows$late) == 0) {
    return(list(status = "insufficient_data", reason = "Early and late windows could not be defined.", intervals = tibble::tibble(), share_change_bootstrap = tibble::tibble(), dominance_bootstrap = tibble::tibble()))
  }

  early <- doc_country$country[doc_country$year %in% windows$early]
  late <- doc_country$country[doc_country$year %in% windows$late]
  early <- early[!is.na(early) & nzchar(early)]
  late <- late[!is.na(late) & nzchar(late)]
  if (length(early) < 5 || length(late) < 5) {
    return(list(status = "insufficient_data", reason = "Window samples were too small for bootstrap uncertainty.", intervals = tibble::tibble(), share_change_bootstrap = tibble::tibble(), dominance_bootstrap = tibble::tibble()))
  }

  rank_windows <- mobility$rank_windows %||% tibble::tibble()
  target_countries <- if (is.data.frame(rank_windows) && nrow(rank_windows) > 0) {
    rank_windows |>
      dplyr::arrange(dplyr::desc(abs(.data$share_change))) |>
      utils::head(12) |>
      dplyr::pull("country")
  } else {
    names(sort(table(c(early, late)), decreasing = TRUE))[seq_len(min(12, length(unique(c(early, late)))))]
  }

  set.seed(271828)
  share_boot <- dplyr::bind_rows(lapply(target_countries, function(country) {
    boot <- replicate(bootstrap_n, {
      late_s <- sample(late, length(late), replace = TRUE)
      early_s <- sample(early, length(early), replace = TRUE)
      mean(late_s == country) - mean(early_s == country)
    })
    tibble::tibble(iteration = seq_along(boot), country = country, share_change = as.numeric(boot))
  }))

  share_intervals <- share_boot |>
    dplyr::group_by(.data$country) |>
    dplyr::summarise(
      entity = dplyr::first(.data$country),
      metric = "share_change",
      estimate = mean(.data$share_change, na.rm = TRUE),
      ci_low = stats::quantile(.data$share_change, 0.025, na.rm = TRUE, names = FALSE),
      ci_high = stats::quantile(.data$share_change, 0.975, na.rm = TRUE, names = FALSE),
      method = "bootstrap_country_row_resampling",
      .groups = "drop"
    ) |>
    dplyr::select("entity", "metric", "estimate", "ci_low", "ci_high", "method")

  dominance_boot <- replicate(bootstrap_n, {
    late_s <- sample(late, length(late), replace = TRUE)
    early_s <- sample(early, length(early), replace = TRUE)
    max(prop.table(table(late_s))) - max(prop.table(table(early_s)))
  })
  dominance_interval <- tibble::tibble(
    entity = "top_country",
    metric = "top_country_dominance_change",
    estimate = max(prop.table(table(late))) - max(prop.table(table(early))),
    ci_low = stats::quantile(dominance_boot, 0.025, na.rm = TRUE, names = FALSE),
    ci_high = stats::quantile(dominance_boot, 0.975, na.rm = TRUE, names = FALSE),
    method = "bootstrap_country_row_resampling"
  )

  cp <- collaboration_premium$table %||% tibble::tibble()
  premium_interval <- if (is.data.frame(cp) && nrow(cp) > 0 && all(c("mean_difference", "ci_low", "ci_high") %in% names(cp))) {
    tibble::tibble(
      entity = "MCP_vs_SCP",
      metric = "citation_premium",
      estimate = cp$mean_difference[1],
      ci_low = cp$ci_low[1],
      ci_high = cp$ci_high[1],
      method = cp$method[1] %||% "bootstrap_ci"
    )
  } else {
    tibble::tibble()
  }

  intervals <- dplyr::bind_rows(premium_interval, dominance_interval, share_intervals)
  list(
    status = "success",
    reason = NA_character_,
    intervals = intervals,
    share_change_bootstrap = share_boot,
    dominance_bootstrap = tibble::tibble(iteration = seq_along(dominance_boot), dominance_change = as.numeric(dominance_boot))
  )
}

m3_compute_robustness_advanced <- function(prepared_data, country_metrics, config) {
  country_table <- country_metrics$country_table %||% tibble::tibble()
  if (!is.data.frame(country_table) || nrow(country_table) == 0 || !all(c("full_articles", "fractional_articles") %in% names(country_table))) {
    return(list(status = "insufficient_data", reason = "Country full/fractional metrics were unavailable.", rank_sensitivity = tibble::tibble(), topn_sensitivity = tibble::tibble(), quadrant_robustness = tibble::tibble()))
  }

  valid <- country_table |>
    dplyr::filter(is.finite(.data$full_articles), is.finite(.data$fractional_articles)) |>
    dplyr::mutate(
      full_rank = dplyr::min_rank(dplyr::desc(.data$full_articles)),
      fractional_rank = dplyr::min_rank(dplyr::desc(.data$fractional_articles))
    )
  spearman <- if (nrow(valid) >= 3) tryCatch(stats::cor.test(valid$full_rank, valid$fractional_rank, method = "spearman", exact = FALSE), error = function(e) NULL) else NULL
  kendall <- if (nrow(valid) >= 3) tryCatch(stats::cor.test(valid$full_rank, valid$fractional_rank, method = "kendall", exact = FALSE), error = function(e) NULL) else NULL
  rank_sensitivity <- tibble::tibble(
    method = c("spearman", "kendall"),
    estimate = c(if (!is.null(spearman)) unname(spearman$estimate) else NA_real_, if (!is.null(kendall)) unname(kendall$estimate) else NA_real_),
    p_value = c(if (!is.null(spearman)) spearman$p.value else NA_real_, if (!is.null(kendall)) kendall$p.value else NA_real_),
    mean_abs_rank_change = mean(abs(valid$full_rank - valid$fractional_rank), na.rm = TRUE),
    n_countries = nrow(valid)
  )

  topn_values <- unique(pmin(c(5, 10, 20), nrow(valid)))
  topn_sensitivity <- dplyr::bind_rows(lapply(topn_values, function(n) {
    full_top <- valid |> dplyr::arrange(.data$full_rank) |> utils::head(n)
    frac_top <- valid |> dplyr::arrange(.data$fractional_rank) |> utils::head(n)
    tibble::tibble(
      top_n = n,
      full_share = sum(full_top$production_share, na.rm = TRUE),
      fractional_share = sum(frac_top$fractional_share, na.rm = TRUE),
      overlap = length(intersect(full_top$country, frac_top$country)),
      overlap_share = length(intersect(full_top$country, frac_top$country)) / max(1, n),
      full_hhi = sum((full_top$full_articles / max(sum(full_top$full_articles, na.rm = TRUE), .Machine$double.eps))^2, na.rm = TRUE),
      fractional_hhi = sum((frac_top$fractional_articles / max(sum(frac_top$fractional_articles, na.rm = TRUE), .Machine$double.eps))^2, na.rm = TRUE)
    )
  }))

  quadrant_robustness <- valid |>
    dplyr::mutate(
      full_quadrant = m3_quadrant_label(.data$full_articles, .data$age_normalized_impact),
      fractional_quadrant = m3_quadrant_label(.data$fractional_articles, .data$age_normalized_impact),
      quadrant_changed = .data$full_quadrant != .data$fractional_quadrant
    ) |>
    dplyr::select("country", "full_rank", "fractional_rank", "rank_sensitivity", "full_quadrant", "fractional_quadrant", "quadrant_changed")

  list(status = "success", reason = NA_character_, rank_sensitivity = rank_sensitivity, topn_sensitivity = topn_sensitivity, quadrant_robustness = quadrant_robustness)
}

m3_compute_inequality_decomposition_advanced <- function(prepared_data, country_metrics, config) {
  country_year <- country_metrics$country_year %||% tibble::tibble()
  country_table <- country_metrics$country_table %||% tibble::tibble()
  if (!is.data.frame(country_table) || nrow(country_table) == 0) {
    return(list(status = "insufficient_data", reason = "Country metrics were unavailable.", window_metrics = tibble::tibble(), year_metrics = tibble::tibble(), regional_concentration = tibble::tibble(), regional_inequality_contribution = tibble::tibble()))
  }

  window_metrics <- tibble::tibble()
  year_metrics <- tibble::tibble()
  if (is.data.frame(country_year) && nrow(country_year) > 0 && all(c("country", "year", "article_count") %in% names(country_year))) {
    windows <- m3_country_window_years(country_year$year)
    window_metrics <- dplyr::bind_rows(lapply(names(windows), function(window) {
      yrs <- windows[[window]]
      if (length(yrs) == 0) return(tibble::tibble())
      tmp <- country_year |>
        dplyr::filter(.data$year %in% yrs) |>
        dplyr::group_by(.data$country) |>
        dplyr::summarise(article_count = sum(.data$article_count, na.rm = TRUE), total_citations = sum(.data$total_citations, na.rm = TRUE), .groups = "drop")
      dplyr::bind_rows(
        m3_concentration_row(tmp$article_count, "production", window),
        m3_concentration_row(tmp$total_citations, "citations", window)
      )
    }))

    year_metrics <- dplyr::bind_rows(lapply(sort(unique(country_year$year)), function(year) {
      tmp <- country_year[country_year$year == year, , drop = FALSE]
      dplyr::bind_rows(
        m3_concentration_row(tmp$article_count, "production", as.character(year)),
        m3_concentration_row(tmp$total_citations, "citations", as.character(year))
      ) |>
        dplyr::mutate(year = year)
    }))
  }

  regional_concentration <- country_table |>
    dplyr::mutate(region = vapply(.data$country, m3_region_for_country, character(1))) |>
    dplyr::group_by(.data$region) |>
    dplyr::summarise(
      n_countries = dplyr::n_distinct(.data$country),
      production = sum(.data$full_articles, na.rm = TRUE),
      citations = sum(.data$total_citations, na.rm = TRUE),
      within_region_gini_production = m3_gini(.data$full_articles),
      within_region_gini_citations = m3_gini(.data$total_citations),
      within_region_hhi_production = {
        x <- .data$full_articles / max(sum(.data$full_articles, na.rm = TRUE), .Machine$double.eps)
        sum(x^2, na.rm = TRUE)
      },
      .groups = "drop"
    ) |>
    dplyr::mutate(
      production_share = .data$production / max(sum(.data$production, na.rm = TRUE), .Machine$double.eps),
      citation_share = .data$citations / max(sum(.data$citations, na.rm = TRUE), .Machine$double.eps)
    )

  regional_inequality_contribution <- regional_concentration |>
    dplyr::transmute(
      region = .data$region,
      production_share = .data$production_share,
      citation_share = .data$citation_share,
      production_gini_contribution = .data$production_share * .data$within_region_gini_production,
      citation_gini_contribution = .data$citation_share * .data$within_region_gini_citations
    )

  list(status = "success", reason = NA_character_, window_metrics = window_metrics, year_metrics = year_metrics, regional_concentration = regional_concentration, regional_inequality_contribution = regional_inequality_contribution)
}

m3_compute_country_trend_models_advanced <- function(country_metrics, config) {
  country_year <- country_metrics$country_year %||% tibble::tibble()
  if (!is.data.frame(country_year) || nrow(country_year) == 0 || !all(c("country", "year", "article_count", "share") %in% names(country_year))) {
    return(list(status = "insufficient_data", reason = "Country-year production data were unavailable.", table = tibble::tibble()))
  }

  pieces <- split(country_year, country_year$country)
  table <- dplyr::bind_rows(lapply(names(pieces), function(country) {
    df <- pieces[[country]]
    df <- df[is.finite(df$year) & is.finite(df$article_count), , drop = FALSE]
    if (nrow(df) < 3 || length(unique(df$year)) < 3) return(tibble::tibble())
    prod_fit <- tryCatch(stats::lm(article_count ~ year, data = df), error = function(e) NULL)
    share_fit <- tryCatch(stats::lm(share ~ year, data = df), error = function(e) NULL)
    if (is.null(prod_fit)) return(tibble::tibble())
    sm <- summary(prod_fit)
    first_articles <- df$article_count[which.min(df$year)]
    last_articles <- df$article_count[which.max(df$year)]
    breakpoint <- m3_simple_country_breakpoint(df)
    tibble::tibble(
      country = country,
      n_years = nrow(df),
      production_slope = unname(stats::coef(prod_fit)[["year"]]),
      production_slope_p = sm$coefficients["year", "Pr(>|t|)"],
      production_r_squared = sm$r.squared,
      share_slope = if (!is.null(share_fit)) unname(stats::coef(share_fit)[["year"]]) else NA_real_,
      share_slope_p = if (!is.null(share_fit)) summary(share_fit)$coefficients["year", "Pr(>|t|)"] else NA_real_,
      relative_growth = (last_articles - first_articles) / max(first_articles, .Machine$double.eps),
      breakpoint_year = breakpoint$breakpoint_year,
      breakpoint_sse_gain = breakpoint$sse_gain
    )
  })) |>
    dplyr::arrange(dplyr::desc(.data$production_slope))

  list(status = if (nrow(table) > 0) "success" else "insufficient_data", reason = if (nrow(table) > 0) NA_character_ else "No country had enough years for trend modeling.", table = table)
}

m3_compute_geographic_outliers_advanced <- function(country_metrics, config) {
  country_table <- country_metrics$country_table %||% tibble::tibble()
  if (!is.data.frame(country_table) || nrow(country_table) < 5 || !all(c("full_articles", "age_normalized_impact") %in% names(country_table))) {
    return(list(status = "insufficient_data", reason = "Country impact and production metrics were unavailable.", table = tibble::tibble()))
  }

  valid <- country_table |>
    dplyr::filter(is.finite(.data$full_articles), .data$full_articles > 0, is.finite(.data$age_normalized_impact))
  if (nrow(valid) < 5) {
    return(list(status = "insufficient_data", reason = "Too few finite country observations for outlier modeling.", table = tibble::tibble()))
  }

  fit <- tryCatch(stats::lm(log1p(age_normalized_impact) ~ log1p(full_articles), data = valid), error = function(e) NULL)
  if (is.null(fit)) {
    return(list(status = "error", reason = "Outlier model failed.", table = tibble::tibble()))
  }

  residuals <- stats::residuals(fit)
  z <- as.numeric(scale(residuals))
  table <- valid |>
    dplyr::mutate(
      expected_log_impact = as.numeric(stats::predict(fit, newdata = valid)),
      impact_residual = residuals,
      residual_z = z,
      outlier_type = dplyr::case_when(
        .data$residual_z >= 2 ~ "high_impact_outlier",
        .data$residual_z <= -2 ~ "low_impact_outlier",
        TRUE ~ "within_expected_range"
      )
    ) |>
    dplyr::select("country", "full_articles", "age_normalized_impact", "expected_log_impact", "impact_residual", "residual_z", "outlier_type") |>
    dplyr::arrange(dplyr::desc(abs(.data$residual_z)))

  list(status = "success", reason = NA_character_, table = table)
}

m3_compute_spatial_autocorrelation_advanced <- function(country_metrics, config) {
  country_table <- country_metrics$country_table %||% tibble::tibble()
  if (!is.data.frame(country_table) || nrow(country_table) < 5 || !"country" %in% names(country_table)) {
    return(list(status = "insufficient_data", reason = "Country metrics unavailable.", global = tibble::tibble(), lisa = tibble::tibble()))
  }
  metric <- if ("age_normalized_impact" %in% names(country_table)) "age_normalized_impact" else "full_articles"
  df <- country_table |>
    dplyr::mutate(
      value = suppressWarnings(as.numeric(.data[[metric]])),
      region = vapply(.data$country, m3_region_for_country, character(1))
    ) |>
    dplyr::filter(is.finite(.data$value))
  if (nrow(df) < 5 || stats::sd(df$value, na.rm = TRUE) <= 0) {
    return(list(status = "insufficient_data", reason = "Too few finite country values for spatial autocorrelation.", global = tibble::tibble(), lisa = tibble::tibble()))
  }

  coords <- config$country_spatial_data %||% NULL
  if (is.data.frame(coords) && all(c("country", "lon", "lat") %in% names(coords))) {
    df <- df |>
      dplyr::left_join(coords[, c("country", "lon", "lat")], by = "country")
    weights <- m3_distance_weight_matrix(df)
    method <- "inverse_distance_weights"
  } else {
    weights <- outer(df$region, df$region, FUN = function(a, b) as.numeric(a == b))
    diag(weights) <- 0
    method <- "region_adjacency_proxy"
  }
  if (sum(weights, na.rm = TRUE) <= 0) {
    return(list(status = "insufficient_data", reason = "No spatial/proxy neighbor structure was available.", global = tibble::tibble(), lisa = tibble::tibble()))
  }

  z <- as.numeric(scale(df$value))
  wz <- as.numeric(weights %*% z) / pmax(rowSums(weights), .Machine$double.eps)
  moran_i <- (length(z) / sum(weights)) * sum(weights * outer(z, z), na.rm = TRUE) / sum(z^2, na.rm = TRUE)
  perm_n <- as.integer(config$spatial_permutations %||% 199)
  set.seed(161803)
  perm <- replicate(max(20, perm_n), {
    zp <- sample(z)
    (length(zp) / sum(weights)) * sum(weights * outer(zp, zp), na.rm = TRUE) / sum(zp^2, na.rm = TRUE)
  })
  p_value <- mean(abs(perm) >= abs(moran_i), na.rm = TRUE)
  lisa <- df |>
    dplyr::mutate(
      local_i = z * wz,
      lisa_cluster = dplyr::case_when(
        z >= 0 & wz >= 0 ~ "high_high",
        z < 0 & wz < 0 ~ "low_low",
        z >= 0 & wz < 0 ~ "high_low",
        TRUE ~ "low_high"
      ),
      method = method
    ) |>
    dplyr::select("country", "region", "value", "local_i", "lisa_cluster", "method")
  global <- tibble::tibble(metric = metric, morans_i = moran_i, p_value = p_value, n_countries = nrow(df), method = method)
  list(status = "success", reason = NA_character_, global = global, lisa = lisa)
}

m3_distance_weight_matrix <- function(df) {
  if (!all(c("lon", "lat") %in% names(df)) || any(!is.finite(df$lon) | !is.finite(df$lat))) {
    return(matrix(0, nrow(df), nrow(df)))
  }
  coords <- as.matrix(df[, c("lon", "lat")])
  d <- as.matrix(stats::dist(coords))
  w <- 1 / pmax(d, 1e-6)
  diag(w) <- 0
  w
}

m3_compute_collaboration_backbone_advanced <- function(collaboration_network, country_metrics, config) {
  edges <- collaboration_network$edges %||% tibble::tibble()
  nodes <- collaboration_network$nodes %||% tibble::tibble()
  if (!is.data.frame(edges) || nrow(edges) == 0 || !"weight" %in% names(edges)) {
    return(list(status = "insufficient_data", reason = "Country collaboration edges unavailable.", nodes = tibble::tibble(), edges = tibble::tibble(), communities = tibble::tibble()))
  }
  threshold <- stats::quantile(edges$weight, probs = 0.75, na.rm = TRUE, names = FALSE)
  backbone <- edges |>
    dplyr::filter(.data$weight >= threshold) |>
    dplyr::arrange(dplyr::desc(.data$weight))
  if (nrow(backbone) == 0) {
    backbone <- utils::head(edges |> dplyr::arrange(dplyr::desc(.data$weight)), min(10, nrow(edges)))
  }
  community <- m3_backbone_components(backbone)
  node_out <- if (is.data.frame(nodes) && nrow(nodes) > 0) {
    nodes |>
      dplyr::left_join(community, by = "country")
  } else {
    tibble::tibble(country = unique(c(backbone$from, backbone$to))) |>
      dplyr::left_join(community, by = "country")
  }
  list(status = "success", reason = NA_character_, nodes = node_out, edges = backbone, communities = community, all_edge_weight = sum(edges$weight, na.rm = TRUE))
}

m3_backbone_components <- function(edges) {
  countries <- sort(unique(c(edges$from, edges$to)))
  comp <- stats::setNames(seq_along(countries), countries)
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (i in seq_len(nrow(edges))) {
      a <- edges$from[i]
      b <- edges$to[i]
      min_comp <- min(comp[[a]], comp[[b]])
      if (comp[[a]] != min_comp || comp[[b]] != min_comp) {
        comp[[a]] <- min_comp
        comp[[b]] <- min_comp
        changed <- TRUE
      }
    }
  }
  tibble::tibble(country = names(comp), community = paste0("C", match(comp, sort(unique(comp)))))
}

m3_compute_gravity_collaboration_model <- function(country_metrics, collaboration_network, config) {
  country_table <- country_metrics$country_table %||% tibble::tibble()
  edges <- collaboration_network$edges %||% tibble::tibble()
  if (!is.data.frame(country_table) || nrow(country_table) == 0 || !is.data.frame(edges) || nrow(edges) < 5) {
    return(list(status = "insufficient_data", reason = "Country metrics or collaboration edges unavailable.", table = tibble::tibble(), coefficients = tibble::tibble()))
  }
  masses <- country_table |>
    dplyr::select(dplyr::any_of(c("country", "full_articles", "total_citations", "mcp_ratio"))) |>
    dplyr::mutate(full_articles = dplyr::coalesce(.data$full_articles, 0))
  external <- config$country_normalization_data %||% NULL
  if (is.data.frame(external) && "country" %in% names(external)) {
    masses <- masses |> dplyr::left_join(external, by = "country")
  }
  for (col in c("gdp", "population")) if (!col %in% names(masses)) masses[[col]] <- NA_real_
  model_df <- edges |>
    dplyr::left_join(masses, by = c("from" = "country")) |>
    dplyr::rename(from_articles = "full_articles", from_gdp = "gdp", from_population = "population") |>
    dplyr::left_join(masses, by = c("to" = "country")) |>
    dplyr::rename(to_articles = "full_articles", to_gdp = "gdp", to_population = "population") |>
    dplyr::mutate(
      log_weight = log1p(.data$weight),
      log_output_mass = log1p(.data$from_articles) + log1p(.data$to_articles),
      log_gdp_mass = log1p(dplyr::coalesce(.data$from_gdp, 0)) + log1p(dplyr::coalesce(.data$to_gdp, 0)),
      log_population_mass = log1p(dplyr::coalesce(.data$from_population, 0)) + log1p(dplyr::coalesce(.data$to_population, 0))
    )
  predictors <- c("log_output_mass")
  if (any(model_df$log_gdp_mass > 0, na.rm = TRUE)) predictors <- c(predictors, "log_gdp_mass")
  if (any(model_df$log_population_mass > 0, na.rm = TRUE)) predictors <- c(predictors, "log_population_mass")
  form <- stats::as.formula(paste("log_weight ~", paste(predictors, collapse = " + ")))
  fit <- tryCatch(stats::lm(form, data = model_df), error = function(e) NULL)
  if (is.null(fit)) {
    return(list(status = "error", reason = "Gravity-style model failed.", table = model_df, coefficients = tibble::tibble()))
  }
  coefs <- summary(fit)$coefficients
  coefficients <- tibble::tibble(
    term = rownames(coefs),
    estimate = coefs[, "Estimate"],
    std_error = coefs[, "Std. Error"],
    p_value = coefs[, "Pr(>|t|)"],
    model = "exploratory_gravity_style_lm"
  )
  model_df$fitted_log_weight <- as.numeric(stats::predict(fit, newdata = model_df))
  list(status = "success", reason = NA_character_, table = model_df, coefficients = coefficients)
}

m3_compute_country_role_labels_advanced <- function(country_metrics, collaboration_network, trend_models, outliers, config) {
  country_table <- country_metrics$country_table %||% tibble::tibble()
  nodes <- collaboration_network$nodes %||% tibble::tibble()
  trends <- trend_models$table %||% tibble::tibble()
  outlier_table <- outliers$table %||% tibble::tibble()
  if (!is.data.frame(country_table) || nrow(country_table) == 0) {
    return(list(status = "insufficient_data", reason = "Country metrics unavailable.", table = tibble::tibble()))
  }
  df <- country_table |>
    dplyr::left_join(nodes |> dplyr::select(dplyr::any_of(c("country", "degree_weight", "degree"))), by = "country")
  if (is.data.frame(trends) && nrow(trends) > 0) {
    df <- df |> dplyr::left_join(trends |> dplyr::select(dplyr::any_of(c("country", "production_slope", "production_p_value", "breakpoint_year"))), by = "country")
  }
  if (is.data.frame(outlier_table) && nrow(outlier_table) > 0) {
    df <- df |> dplyr::left_join(outlier_table |> dplyr::select(dplyr::any_of(c("country", "residual_z", "outlier_type"))), by = "country")
  }
  for (col in c("full_articles", "age_normalized_impact", "mcp_ratio", "degree_weight", "production_slope", "relative_share_growth", "share_acceleration", "residual_z")) {
    if (!col %in% names(df)) df[[col]] <- NA_real_
  }
  q <- function(x, p) suppressWarnings(stats::quantile(x, p, na.rm = TRUE, names = FALSE))
  leader_cut <- q(df$full_articles, 0.80)
  impact_cut <- q(df$age_normalized_impact, 0.80)
  broker_cut <- q(df$degree_weight, 0.80)
  df <- df |>
    dplyr::mutate(
      role_label = dplyr::case_when(
        is.finite(.data$full_articles) & .data$full_articles >= leader_cut & is.finite(.data$age_normalized_impact) & .data$age_normalized_impact >= impact_cut ~ "leader",
        is.finite(.data$degree_weight) & .data$degree_weight >= broker_cut & is.finite(.data$mcp_ratio) & .data$mcp_ratio >= stats::median(.data$mcp_ratio, na.rm = TRUE) ~ "broker",
        dplyr::coalesce(.data$relative_share_growth, .data$share_acceleration, 0) > 0 & dplyr::coalesce(.data$production_slope, 0) > 0 ~ "emerging_accelerator",
        is.finite(.data$age_normalized_impact) & .data$age_normalized_impact >= impact_cut & dplyr::coalesce(.data$full_articles, 0) < leader_cut ~ "high_impact_niche",
        dplyr::coalesce(.data$production_slope, 0) < 0 | dplyr::coalesce(.data$relative_share_growth, 0) < 0 ~ "declining_incumbent",
        dplyr::coalesce(.data$degree_weight, 0) == 0 & dplyr::coalesce(.data$full_articles, 0) > 0 ~ "isolated_producer",
        TRUE ~ "stable_participant"
      ),
      role_evidence = paste0(
        "articles=", round(dplyr::coalesce(.data$full_articles, 0), 2),
        "; impact=", round(dplyr::coalesce(.data$age_normalized_impact, NA_real_), 3),
        "; mcp_ratio=", round(dplyr::coalesce(.data$mcp_ratio, NA_real_), 3),
        "; network_weight=", round(dplyr::coalesce(.data$degree_weight, 0), 2)
      )
    ) |>
    dplyr::select(dplyr::any_of(c("country", "role_label", "role_evidence", "full_articles", "age_normalized_impact", "mcp_ratio", "degree_weight", "production_slope", "relative_share_growth", "share_acceleration", "residual_z", "outlier_type"))) |>
    dplyr::arrange(.data$role_label, dplyr::desc(.data$full_articles))
  list(status = "success", reason = NA_character_, table = df)
}

m3_advanced_hypotheses <- function(collaboration_premium,
                                   mobility,
                                   trajectories,
                                   geo_concentration,
                                   regional_decomposition,
                                   country_metrics = list(),
                                   scp_mcp_trends = list(),
                                   uncertainty = list(),
                                   robustness = list(),
                                   trend_models = list(),
                                   outliers = list(),
                                   spatial_autocorrelation = list(),
                                   collaboration_backbone = list(),
                                   gravity_model = list(),
                                   country_roles = list()) {
  cp <- collaboration_premium$table %||% tibble::tibble()
  premium_effect <- if (nrow(cp) > 0) suppressWarnings(as.numeric(cp$cohen_d[1])) else NA_real_
  premium_ci <- if (nrow(cp) > 0) sprintf("[%.3f, %.3f]", cp$ci_low[1], cp$ci_high[1]) else NA_character_
  premium_p <- if (nrow(cp) > 0) suppressWarnings(as.numeric(cp$p_value[1])) else NA_real_

  conc <- geo_concentration$table %||% tibble::tibble()
  prod <- conc[conc$metric == "production", , drop = FALSE]
  concentration_effect <- if (nrow(prod) > 0) suppressWarnings(as.numeric(prod$gini[1])) else NA_real_
  concentration_p <- if (nrow(prod) > 0) suppressWarnings(as.numeric(prod$chisq_p_value[1])) else NA_real_

  stability <- mobility$rank_stability %||% tibble::tibble()
  spearman <- stability[stability$method == "spearman", , drop = FALSE]
  rho <- if (nrow(spearman) > 0) suppressWarnings(as.numeric(spearman$estimate[1])) else NA_real_
  rho_p <- if (nrow(spearman) > 0) suppressWarnings(as.numeric(spearman$p_value[1])) else NA_real_

  tr <- trajectories$table %||% tibble::tibble()
  emerging <- if (is.data.frame(tr) && nrow(tr) > 0) tr[tr$trajectory_class %in% c("emerging", "late_entry"), , drop = FALSE] else tibble::tibble()
  max_share_gain <- if (nrow(emerging) > 0) max(emerging$share_change, na.rm = TRUE) else NA_real_

  regional <- regional_decomposition$table %||% tibble::tibble()
  region_p <- NA_real_
  region_effect <- NA_real_
  if (is.data.frame(regional) && nrow(regional) >= 3 && "mcp_ratio" %in% names(regional)) {
    valid <- regional[is.finite(regional$mcp_ratio), , drop = FALSE]
    if (nrow(valid) >= 3) {
      region_effect <- stats::sd(valid$mcp_ratio, na.rm = TRUE)
      region_p <- tryCatch(stats::kruskal.test(valid$mcp_ratio, valid$region)$p.value, error = function(e) NA_real_)
    }
  }

  rank_robust <- robustness$rank_sensitivity %||% tibble::tibble()
  rank_spearman <- rank_robust[rank_robust$method == "spearman", , drop = FALSE]
  full_fractional_rho <- if (nrow(rank_spearman) > 0) suppressWarnings(as.numeric(rank_spearman$estimate[1])) else NA_real_
  full_fractional_p <- if (nrow(rank_spearman) > 0) suppressWarnings(as.numeric(rank_spearman$p_value[1])) else NA_real_

  outlier_table <- outliers$table %||% tibble::tibble()
  max_outlier_z <- if (is.data.frame(outlier_table) && nrow(outlier_table) > 0) max(abs(outlier_table$residual_z), na.rm = TRUE) else NA_real_

  trend <- scp_mcp_trends$trend %||% tibble::tibble()
  mcp_trend_slope <- if (is.data.frame(trend) && nrow(trend) > 0) suppressWarnings(as.numeric(trend$slope[1])) else NA_real_
  mcp_trend_p <- if (is.data.frame(trend) && nrow(trend) > 0) suppressWarnings(as.numeric(trend$p_value[1])) else NA_real_

  intervals <- uncertainty$intervals %||% tibble::tibble()
  dominance <- if (is.data.frame(intervals) && nrow(intervals) > 0) intervals[intervals$metric == "top_country_dominance_change", , drop = FALSE] else tibble::tibble()
  dominance_effect <- if (nrow(dominance) > 0) suppressWarnings(as.numeric(dominance$estimate[1])) else NA_real_
  dominance_ci <- if (nrow(dominance) > 0) sprintf("[%.3f, %.3f]", dominance$ci_low[1], dominance$ci_high[1]) else NA_character_
  dominance_supported <- nrow(dominance) > 0 && is.finite(dominance$ci_low[1]) && is.finite(dominance$ci_high[1]) && (dominance$ci_low[1] > 0 || dominance$ci_high[1] < 0)

  trend_table <- trend_models$table %||% tibble::tibble()
  leading_trends <- if (is.data.frame(trend_table) && nrow(trend_table) > 0) {
    trend_table |> dplyr::arrange(dplyr::desc(.data$n_years), dplyr::desc(.data$production_slope)) |> utils::head(10)
  } else {
    tibble::tibble()
  }
  positive_leading_share <- if (nrow(leading_trends) > 0) mean(leading_trends$production_slope > 0 & leading_trends$production_slope_p < 0.05, na.rm = TRUE) else NA_real_
  leading_trend_p <- if (nrow(leading_trends) > 0 && sum(is.finite(leading_trends$production_slope_p)) > 0) {
    min(leading_trends$production_slope_p, na.rm = TRUE)
  } else {
    NA_real_
  }

  spatial_global <- spatial_autocorrelation$global %||% tibble::tibble()
  morans_i <- if (is.data.frame(spatial_global) && nrow(spatial_global) > 0) suppressWarnings(as.numeric(spatial_global$morans_i[1])) else NA_real_
  morans_p <- if (is.data.frame(spatial_global) && nrow(spatial_global) > 0) suppressWarnings(as.numeric(spatial_global$p_value[1])) else NA_real_

  backbone_edges <- collaboration_backbone$edges %||% tibble::tibble()
  backbone_share <- if (is.data.frame(backbone_edges) && nrow(backbone_edges) > 0 && "weight" %in% names(backbone_edges)) {
    total_proxy <- collaboration_backbone$all_edge_weight %||% sum(backbone_edges$weight, na.rm = TRUE)
    sum(backbone_edges$weight, na.rm = TRUE) / max(total_proxy, .Machine$double.eps)
  } else {
    NA_real_
  }

  gravity_coefs <- gravity_model$coefficients %||% tibble::tibble()
  gravity_output <- if (is.data.frame(gravity_coefs) && nrow(gravity_coefs) > 0 && "term" %in% names(gravity_coefs)) {
    gravity_coefs[gravity_coefs$term == "log_output_mass", , drop = FALSE]
  } else {
    tibble::tibble()
  }
  gravity_effect <- if (nrow(gravity_output) > 0) suppressWarnings(as.numeric(gravity_output$estimate[1])) else NA_real_
  gravity_p <- if (nrow(gravity_output) > 0) suppressWarnings(as.numeric(gravity_output$p_value[1])) else NA_real_

  roles_tbl <- country_roles$table %||% tibble::tibble()
  role_coverage <- if (is.data.frame(roles_tbl) && nrow(roles_tbl) > 0 && "role_label" %in% names(roles_tbl)) {
    mean(!is.na(roles_tbl$role_label) & nzchar(roles_tbl$role_label), na.rm = TRUE)
  } else {
    NA_real_
  }

  m3_advanced_hypothesis_bundle(list(
    M3_H09 = m3_advanced_hypothesis(
      "M3_H09",
      "MCP has a citation premium over SCP.",
      "wilcoxon_bootstrap_ci",
      premium_effect,
      premium_ci,
      premium_p,
      if (is.finite(premium_p) && premium_p < 0.05 && is.finite(premium_effect) && premium_effect > 0) "supported" else if (is.finite(premium_p)) "not_supported" else "inconclusive",
      if (is.finite(premium_effect) && premium_effect > 0) "Internationally collaborative papers show higher citation impact in this dataset." else "The available evidence does not show a positive MCP citation premium."
    ),
    M3_H10 = m3_advanced_hypothesis(
      "M3_H10",
      "Geographic production is concentrated beyond a uniform distribution.",
      "chi_square_uniform_gini_hhi",
      concentration_effect,
      NA_character_,
      concentration_p,
      if (is.finite(concentration_p) && concentration_p < 0.05 && is.finite(concentration_effect) && concentration_effect > 0.20) "supported" else if (is.finite(concentration_p)) "not_supported" else "inconclusive",
      "Production concentration is evaluated using Gini, HHI, top-share, and a uniform-distribution test."
    ),
    M3_H11 = m3_advanced_hypothesis(
      "M3_H11",
      "Country leadership changes significantly between temporal windows.",
      "rank_stability_spearman",
      if (is.finite(rho)) 1 - rho else NA_real_,
      NA_character_,
      rho_p,
      if (is.finite(rho) && rho < 0.70) "supported" else if (is.finite(rho)) "not_supported" else "inconclusive",
      if (is.finite(rho) && rho < 0.70) "First-window and last-window rankings diverge enough to support a leadership-change narrative." else "Country leadership appears comparatively persistent across windows."
    ),
    M3_H12 = m3_advanced_hypothesis(
      "M3_H12",
      "Emerging countries have statistically relevant share growth.",
      "windowed_share_growth",
      max_share_gain,
      NA_character_,
      NA_real_,
      if (is.finite(max_share_gain) && max_share_gain > 0.01) "supported" else if (is.finite(max_share_gain)) "not_supported" else "inconclusive",
      if (is.finite(max_share_gain) && max_share_gain > 0.01) "At least one country shows a meaningful increase in production share." else "No emerging-country share gain exceeded the default relevance threshold."
    ),
    M3_H13 = m3_advanced_hypothesis(
      "M3_H13",
      "Regions differ in international collaboration.",
      "regional_mcp_ratio_dispersion",
      region_effect,
      NA_character_,
      region_p,
      if (is.finite(region_effect) && region_effect > 0.05) "supported" else if (is.finite(region_effect)) "not_supported" else "inconclusive",
      "Regional collaboration differences are reported as exploratory association, not causal inference."
    ),
    M3_H14 = m3_advanced_hypothesis(
      "M3_H14",
      "Country rankings are robust to full versus fractional counting.",
      "full_fractional_rank_spearman",
      full_fractional_rho,
      NA_character_,
      full_fractional_p,
      if (is.finite(full_fractional_rho) && full_fractional_rho >= 0.90) "supported" else if (is.finite(full_fractional_rho)) "not_supported" else "inconclusive",
      if (is.finite(full_fractional_rho) && full_fractional_rho >= 0.90) "The leading-country ordering is stable under both counting schemes." else "Counting-mode sensitivity is material enough to report explicitly."
    ),
    M3_H15 = m3_advanced_hypothesis(
      "M3_H15",
      "Some countries have disproportionate citation impact relative to production volume.",
      "log_impact_vs_production_outlier_model",
      max_outlier_z,
      NA_character_,
      NA_real_,
      if (is.finite(max_outlier_z) && max_outlier_z >= 2) "supported" else if (is.finite(max_outlier_z)) "not_supported" else "inconclusive",
      if (is.finite(max_outlier_z) && max_outlier_z >= 2) "At least one country departs strongly from the expected impact-production relationship." else "No country crosses the default residual outlier threshold."
    ),
    M3_H16 = m3_advanced_hypothesis(
      "M3_H16",
      "International collaboration share increases over time.",
      "mcp_share_linear_trend",
      mcp_trend_slope,
      NA_character_,
      mcp_trend_p,
      if (is.finite(mcp_trend_slope) && mcp_trend_slope > 0 && is.finite(mcp_trend_p) && mcp_trend_p < 0.05) "supported" else if (is.finite(mcp_trend_slope)) "not_supported" else "inconclusive",
      if (is.finite(mcp_trend_slope) && mcp_trend_slope > 0) "The annual MCP share trends upward; statistical support depends on the reported p-value." else "The annual MCP share does not show a positive trend in the available data."
    ),
    M3_H17 = m3_advanced_hypothesis(
      "M3_H17",
      "Top-country dominance changes between temporal windows.",
      "bootstrap_top_country_dominance_change",
      dominance_effect,
      dominance_ci,
      NA_real_,
      if (dominance_supported) "supported" else if (is.finite(dominance_effect)) "not_supported" else "inconclusive",
      if (dominance_supported) "The bootstrap interval excludes zero, supporting a dominance-shift interpretation." else "Dominance change is not robust enough to state as a strong finding."
    ),
    M3_H18 = m3_advanced_hypothesis(
      "M3_H18",
      "Leading countries show statistically positive production trends.",
      "country_level_linear_trend_screen",
      positive_leading_share,
      NA_character_,
      leading_trend_p,
      if (is.finite(positive_leading_share) && positive_leading_share >= 0.50) "supported" else if (is.finite(positive_leading_share)) "not_supported" else "inconclusive",
      "Country trend models screen whether the main producers are expanding rather than relying only on aggregate growth."
    ),
    M3_H19 = m3_advanced_hypothesis(
      "M3_H19",
      "Country impact shows spatial or regional autocorrelation.",
      "moran_i_permutation_or_region_proxy",
      morans_i,
      NA_character_,
      morans_p,
      if (is.finite(morans_p) && morans_p < 0.05) "supported" else if (is.finite(morans_p)) "not_supported" else "inconclusive",
      if (is.finite(morans_p) && morans_p < 0.05) "Moran-style evidence indicates non-random geographic clustering." else "Spatial/proxy clustering evidence is not strong enough for a main claim."
    ),
    M3_H20 = m3_advanced_hypothesis(
      "M3_H20",
      "The collaboration backbone concentrates a substantial share of country-pair collaboration.",
      "backbone_edge_weight_share",
      backbone_share,
      NA_character_,
      NA_real_,
      if (is.finite(backbone_share) && backbone_share >= 0.50) "supported" else if (is.finite(backbone_share)) "not_supported" else "inconclusive",
      if (is.finite(backbone_share) && backbone_share >= 0.50) "Backbone country pairs carry at least half of observed international collaboration weight." else "Backbone concentration is not high enough to support a strong concentration claim."
    ),
    M3_H21 = m3_advanced_hypothesis(
      "M3_H21",
      "Output mass explains collaboration intensity in an exploratory gravity-style model.",
      "gravity_style_lm",
      gravity_effect,
      NA_character_,
      gravity_p,
      if (is.finite(gravity_p) && gravity_p < 0.05 && is.finite(gravity_effect) && gravity_effect > 0) "supported" else if (is.finite(gravity_p)) "not_supported" else "inconclusive",
      if (is.finite(gravity_p) && gravity_p < 0.05 && is.finite(gravity_effect) && gravity_effect > 0) "Country-pair output mass is positively associated with collaboration intensity." else "The exploratory gravity-style model does not support a strong collaboration-mass claim."
    ),
    M3_H22 = m3_advanced_hypothesis(
      "M3_H22",
      "Countries can be assigned interpretable bibliometric roles.",
      "country_role_taxonomy_coverage",
      role_coverage,
      NA_character_,
      NA_real_,
      if (is.finite(role_coverage) && role_coverage >= 0.90) "supported" else if (is.finite(role_coverage)) "inconclusive" else "inconclusive",
      if (is.finite(role_coverage) && role_coverage >= 0.90) "Role labels cover nearly all countries and can guide narrative interpretation." else "Role taxonomy coverage or inputs are incomplete; use labels as exploratory."
    )
  ))
}

m3_doc_country_table <- function(prepared_data) {
  doc_level <- prepared_data$country_doc_level %||% tibble::tibble()
  if (!is.data.frame(doc_level) || nrow(doc_level) == 0 || !"country" %in% names(doc_level)) {
    return(tibble::tibble(doc_id = integer(), country = character(), year = numeric(), TC = numeric()))
  }

  year_col <- if ("year" %in% names(doc_level)) "year" else if ("PY" %in% names(doc_level)) "PY" else NULL
  doc_id <- if ("doc_id" %in% names(doc_level)) doc_level$doc_id else seq_len(nrow(doc_level))
  year <- if (!is.null(year_col)) suppressWarnings(as.numeric(doc_level[[year_col]])) else rep(NA_real_, nrow(doc_level))
  tc <- if ("TC" %in% names(doc_level)) suppressWarnings(as.numeric(doc_level$TC)) else rep(NA_real_, nrow(doc_level))

  tibble::tibble(
    doc_id = doc_id,
    country = as.character(doc_level$country),
    year = year,
    TC = tc
  ) |>
    dplyr::filter(!is.na(.data$country), nzchar(trimws(.data$country))) |>
    dplyr::mutate(
      country = trimws(.data$country),
      TC = ifelse(is.finite(.data$TC), .data$TC, 0)
    ) |>
    dplyr::distinct(.data$doc_id, .data$country, .keep_all = TRUE)
}

m3_compute_country_collaboration_premium_table <- function(doc_country, config) {
  if (!is.data.frame(doc_country) || nrow(doc_country) == 0 || !"collaboration_type" %in% names(doc_country)) {
    return(tibble::tibble(country = character(), country_citation_premium = numeric(), country_premium_ci_low = numeric(), country_premium_ci_high = numeric(), country_premium_p_value = numeric(), country_premium_method = character()))
  }

  bootstrap_n <- as.integer(config$bootstrap_n %||% 500)
  pieces <- split(doc_country, doc_country$country)
  dplyr::bind_rows(lapply(names(pieces), function(country) {
    df <- pieces[[country]]
    mcp <- df$TC[df$collaboration_type == "MCP"]
    scp <- df$TC[df$collaboration_type == "SCP"]
    mcp <- mcp[is.finite(mcp)]
    scp <- scp[is.finite(scp)]
    if (length(mcp) < 2 || length(scp) < 2) {
      return(tibble::tibble(
        country = country,
        country_citation_premium = NA_real_,
        country_premium_ci_low = NA_real_,
        country_premium_ci_high = NA_real_,
        country_premium_p_value = NA_real_,
        country_premium_method = "insufficient_scp_mcp"
      ))
    }
    boot <- replicate(bootstrap_n, mean(sample(mcp, length(mcp), replace = TRUE), na.rm = TRUE) - mean(sample(scp, length(scp), replace = TRUE), na.rm = TRUE))
    p <- tryCatch(stats::wilcox.test(mcp, scp, exact = FALSE)$p.value, error = function(e) NA_real_)
    tibble::tibble(
      country = country,
      country_citation_premium = mean(mcp, na.rm = TRUE) - mean(scp, na.rm = TRUE),
      country_premium_ci_low = stats::quantile(boot, 0.025, na.rm = TRUE, names = FALSE),
      country_premium_ci_high = stats::quantile(boot, 0.975, na.rm = TRUE, names = FALSE),
      country_premium_p_value = p,
      country_premium_method = "wilcoxon_bootstrap_ci"
    )
  }))
}

m3_compute_leadership_windows <- function(annual_ranks) {
  if (!is.data.frame(annual_ranks) || nrow(annual_ranks) == 0 || !all(c("country", "rank") %in% names(annual_ranks))) {
    return(tibble::tibble(country = character(), leader_years_top5 = integer(), leader_years_top10 = integer(), leadership_persistence_top5 = numeric(), leadership_persistence_top10 = numeric()))
  }
  n_years <- max(1, dplyr::n_distinct(annual_ranks$year))
  annual_ranks |>
    dplyr::group_by(.data$country) |>
    dplyr::summarise(
      leader_years_top5 = sum(.data$rank <= 5, na.rm = TRUE),
      leader_years_top10 = sum(.data$rank <= 10, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      leadership_persistence_top5 = .data$leader_years_top5 / n_years,
      leadership_persistence_top10 = .data$leader_years_top10 / n_years
    )
}

m3_country_window_years <- function(years) {
  years <- sort(unique(suppressWarnings(as.numeric(years))))
  years <- years[is.finite(years)]
  if (length(years) == 0) {
    return(list(early = numeric(), mid = numeric(), late = numeric()))
  }
  if (length(years) < 3) {
    return(list(early = utils::head(years, 1), mid = years, late = utils::tail(years, 1)))
  }
  k <- max(1, floor(length(years) / 3))
  list(
    early = utils::head(years, k),
    mid = years[(k + 1):max(k + 1, length(years) - k)],
    late = utils::tail(years, k)
  )
}

m3_compute_country_window_shares <- function(doc_country) {
  if (!is.data.frame(doc_country) || nrow(doc_country) == 0 || !any(is.finite(doc_country$year))) {
    return(tibble::tibble())
  }
  windows <- m3_country_window_years(doc_country$year)
  dplyr::bind_rows(lapply(names(windows), function(window) {
    yrs <- windows[[window]]
    if (length(yrs) == 0) return(tibble::tibble())
    doc_country |>
      dplyr::filter(.data$year %in% yrs) |>
      dplyr::count(.data$country, name = "articles") |>
      dplyr::mutate(share = .data$articles / max(sum(.data$articles, na.rm = TRUE), .Machine$double.eps), window = window)
  })) |>
    dplyr::select("country", "window", "share") |>
    tidyr::pivot_wider(names_from = "window", values_from = "share", names_prefix = "share_", values_fill = 0)
}

m3_compute_country_windowed_mcp <- function(doc_country) {
  if (!is.data.frame(doc_country) || nrow(doc_country) == 0 || !any(is.finite(doc_country$year))) {
    return(tibble::tibble(country = character(), window = character(), scp_articles = numeric(), mcp_articles = numeric(), mcp_ratio = numeric()))
  }
  windows <- m3_country_window_years(doc_country$year)
  if (!"n_countries" %in% names(doc_country)) {
    doc_counts <- doc_country |>
      dplyr::count(.data$doc_id, name = "n_countries")
    doc_country <- doc_country |>
      dplyr::left_join(doc_counts, by = "doc_id")
  }
  tmp <- doc_country |>
    dplyr::mutate(
      collaboration_type = ifelse(.data$n_countries > 1, "MCP", "SCP"),
      window = dplyr::case_when(
        .data$year %in% windows$early ~ "early",
        .data$year %in% windows$mid ~ "mid",
        .data$year %in% windows$late ~ "late",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(.data$window))

  out <- tmp |>
    dplyr::count(.data$country, .data$window, .data$collaboration_type, name = "articles") |>
    tidyr::pivot_wider(names_from = "collaboration_type", values_from = "articles", values_fill = 0)
  if (!"SCP" %in% names(out)) out$SCP <- 0
  if (!"MCP" %in% names(out)) out$MCP <- 0
  out |>
    dplyr::mutate(
      scp_articles = .data$SCP,
      mcp_articles = .data$MCP,
      mcp_ratio = ifelse((.data$scp_articles + .data$mcp_articles) > 0, .data$mcp_articles / (.data$scp_articles + .data$mcp_articles), NA_real_)
    ) |>
    dplyr::select("country", "window", "scp_articles", "mcp_articles", "mcp_ratio")
}

m3_concentration_row <- function(values, metric, period) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values) & values >= 0]
  total <- sum(values, na.rm = TRUE)
  n <- length(values)
  if (n == 0 || total <= 0) {
    return(tibble::tibble(period = period, metric = metric, n_countries = n, total = total, gini = NA_real_, theil = NA_real_, hhi = NA_real_, top5_share = NA_real_, top10_share = NA_real_))
  }
  shares <- values / total
  top <- sort(values, decreasing = TRUE)
  tibble::tibble(
    period = period,
    metric = metric,
    n_countries = n,
    total = total,
    gini = m3_gini(values),
    theil = m3_theil(values),
    hhi = sum(shares^2, na.rm = TRUE),
    top5_share = sum(utils::head(top, min(5, n)), na.rm = TRUE) / total,
    top10_share = sum(utils::head(top, min(10, n)), na.rm = TRUE) / total
  )
}

m3_quadrant_label <- function(x, y) {
  x <- suppressWarnings(as.numeric(x))
  y <- suppressWarnings(as.numeric(y))
  x_cut <- stats::median(x, na.rm = TRUE)
  y_cut <- stats::median(y, na.rm = TRUE)
  dplyr::case_when(
    x >= x_cut & y >= y_cut ~ "high_output_high_impact",
    x >= x_cut & y < y_cut ~ "high_output_low_impact",
    x < x_cut & y >= y_cut ~ "low_output_high_impact",
    TRUE ~ "low_output_low_impact"
  )
}

m3_simple_country_breakpoint <- function(df) {
  df <- df[order(df$year), , drop = FALSE]
  years <- sort(unique(df$year))
  if (length(years) < 6) {
    return(list(breakpoint_year = NA_real_, sse_gain = NA_real_))
  }
  full_fit <- tryCatch(stats::lm(article_count ~ year, data = df), error = function(e) NULL)
  if (is.null(full_fit)) return(list(breakpoint_year = NA_real_, sse_gain = NA_real_))
  full_sse <- sum(stats::residuals(full_fit)^2, na.rm = TRUE)
  candidates <- years[3:(length(years) - 3)]
  scores <- vapply(candidates, function(candidate) {
    left <- df[df$year <= candidate, , drop = FALSE]
    right <- df[df$year > candidate, , drop = FALSE]
    if (nrow(left) < 3 || nrow(right) < 3) return(Inf)
    lf <- tryCatch(stats::lm(article_count ~ year, data = left), error = function(e) NULL)
    rf <- tryCatch(stats::lm(article_count ~ year, data = right), error = function(e) NULL)
    if (is.null(lf) || is.null(rf)) return(Inf)
    sum(stats::residuals(lf)^2, na.rm = TRUE) + sum(stats::residuals(rf)^2, na.rm = TRUE)
  }, numeric(1))
  if (!any(is.finite(scores))) return(list(breakpoint_year = NA_real_, sse_gain = NA_real_))
  best <- which.min(scores)
  list(breakpoint_year = candidates[best], sse_gain = (full_sse - scores[best]) / max(full_sse, .Machine$double.eps))
}

m3_region_for_country <- function(country) {
  country_norm <- toupper(trimws(as.character(country)))
  continents <- intersect(names(REGIONAL_GROUPS), c("Africa", "Asia", "Europe", "North America", "South America", "Oceania"))
  for (region in continents) {
    if (country_norm %in% REGIONAL_GROUPS[[region]]) return(region)
  }
  "Unassigned"
}

m3_gini <- function(x) {
  x <- sort(suppressWarnings(as.numeric(x)))
  x <- x[is.finite(x) & x >= 0]
  n <- length(x)
  if (n == 0 || sum(x) == 0) return(NA_real_)
  (2 * sum(seq_len(n) * x) / (n * sum(x))) - (n + 1) / n
}

m3_theil <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x) & x >= 0]
  if (length(x) == 0 || mean(x) <= 0) return(NA_real_)
  ratio <- x / mean(x)
  ratio <- ratio[ratio > 0]
  mean(ratio * log(ratio), na.rm = TRUE)
}

m3_advanced_hypothesis <- function(hypothesis_id, question, test, effect_size, confidence_interval, p_value, decision, interpretation) {
  list(
    hypothesis_id = hypothesis_id,
    question = question,
    test = test,
    effect_size = if (is.null(effect_size)) NA_real_ else effect_size,
    confidence_interval = confidence_interval %||% NA_character_,
    p_value = if (is.null(p_value)) NA_real_ else p_value,
    p_adjusted = NA_real_,
    decision = decision,
    plain_language_interpretation = interpretation
  )
}

m3_advanced_hypothesis_bundle <- function(hypotheses) {
  p_values <- suppressWarnings(as.numeric(vapply(hypotheses, function(h) h$p_value %||% NA_real_, numeric(1))))
  p_adj <- rep(NA_real_, length(p_values))
  ok <- is.finite(p_values)
  if (any(ok)) p_adj[ok] <- stats::p.adjust(p_values[ok], method = "BH")
  i <- 0L
  hypotheses <- lapply(hypotheses, function(h) {
    i <<- i + 1L
    h$p_adjusted <- p_adj[i]
    h
  })
  table <- dplyr::bind_rows(lapply(hypotheses, function(h) {
    tibble::tibble(
      hypothesis_id = h$hypothesis_id,
      question = h$question,
      test = h$test,
      effect_size = h$effect_size,
      confidence_interval = h$confidence_interval,
      p_value = h$p_value,
      p_adjusted = h$p_adjusted,
      decision = h$decision,
      plain_language_interpretation = h$plain_language_interpretation
    )
  }))
  list(status = "success", hypotheses = hypotheses, table = table)
}

m3_empty_collaboration_premium_table <- function() {
  tibble::tibble(comparison = character(), n_mcp = integer(), n_scp = integer(), mean_mcp_citations = numeric(), mean_scp_citations = numeric(), mean_difference = numeric(), ratio = numeric(), cohen_d = numeric(), ci_low = numeric(), ci_high = numeric(), p_value = numeric(), method = character())
}

m3_empty_rank_mobility_table <- function() {
  tibble::tibble(country = character(), articles_first = numeric(), share_first = numeric(), rank_first = numeric(), articles_last = numeric(), share_last = numeric(), rank_last = numeric(), rank_change = numeric(), share_change = numeric(), direction = character())
}

m3_empty_country_trajectory_table <- function() {
  tibble::tibble(country = character(), share_first = numeric(), share_last = numeric(), share_change = numeric(), relative_share_growth = numeric(), rank_change = numeric(), rank_volatility = numeric(), first_active_year = numeric(), late_entry = logical(), trajectory_class = character())
}

m3_empty_geo_concentration_table <- function() {
  tibble::tibble(metric = character(), n_countries = integer(), total = numeric(), gini = numeric(), theil = numeric(), hhi = numeric(), palma_ratio = numeric(), top5_share = numeric(), top10_share = numeric(), uniform_hhi = numeric(), chisq_p_value = numeric())
}

m3_empty_regional_decomposition_table <- function() {
  tibble::tibble(region = character(), n_countries = integer(), article_count = numeric(), total_citations = numeric(), citations_per_article = numeric(), production_share = numeric(), citation_share = numeric(), mcp_ratio = numeric())
}
