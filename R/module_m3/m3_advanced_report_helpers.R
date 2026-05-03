# ============================================================================
# m3_advanced_report_helpers.R - Report fragments for optional M3 analytics
# ============================================================================

.m3_report_advanced_journal <- function(data) {
  advanced <- data$advanced_journal %||% list()
  if (!is.list(advanced) || !identical(advanced$status %||% "", "success")) {
    return(c(
      "--- Advanced Journal Analytics ---",
      sprintf("  Status: %s", advanced$status %||% "unavailable"),
      sprintf("  Reason: %s", advanced$reason %||% "Advanced M3 analytics were not available.")
    ))
  }

  premium <- advanced$collaboration_premium$table %||% tibble::tibble()
  concentration <- advanced$geo_concentration$table %||% tibble::tibble()
  mobility <- advanced$mobility$rank_stability %||% tibble::tibble()
  trajectories <- advanced$trajectories$table %||% tibble::tibble()
  country_metrics <- advanced$country_metrics$country_table %||% tibble::tibble()
  scp_mcp_trend <- advanced$scp_mcp_trends$trend %||% tibble::tibble()
  robustness <- advanced$robustness$rank_sensitivity %||% tibble::tibble()
  uncertainty <- advanced$uncertainty$intervals %||% tibble::tibble()
  outliers <- advanced$outliers$table %||% tibble::tibble()
  hypotheses <- advanced$hypotheses$table %||% tibble::tibble()

  premium_line <- if (is.data.frame(premium) && nrow(premium) > 0) {
    sprintf(
      "  MCP citation premium: %.3f mean citations (95%% CI %.3f to %.3f; d=%.3f).",
      .safe_num(premium$mean_difference[1]),
      .safe_num(premium$ci_low[1]),
      .safe_num(premium$ci_high[1]),
      .safe_num(premium$cohen_d[1])
    )
  } else {
    "  MCP citation premium: not estimable."
  }

  prod <- if (is.data.frame(concentration)) concentration[concentration$metric == "production", , drop = FALSE] else tibble::tibble()
  concentration_line <- if (nrow(prod) > 0) {
    sprintf(
      "  Production concentration: Gini %.3f, Theil %.3f, HHI %.3f, Top-5 %.1f%%.",
      .safe_num(prod$gini[1]),
      .safe_num(prod$theil[1]),
      .safe_num(prod$hhi[1]),
      100 * .safe_num(prod$top5_share[1])
    )
  } else {
    "  Production concentration: not estimable."
  }

  stability <- if (is.data.frame(mobility)) mobility[mobility$method == "spearman", , drop = FALSE] else tibble::tibble()
  mobility_line <- if (nrow(stability) > 0) {
    sprintf("  Rank stability: Spearman rho %.3f (p=%.4f).", .safe_num(stability$estimate[1]), .safe_num(stability$p_value[1]))
  } else {
    "  Rank stability: not estimable."
  }

  trajectory_line <- if (is.data.frame(trajectories) && nrow(trajectories) > 0) {
    counts <- table(trajectories$trajectory_class)
    paste0("  Trajectory classes: ", paste(sprintf("%s=%d", names(counts), as.integer(counts)), collapse = ", "), ".")
  } else {
    "  Trajectory classes: not estimable."
  }

  impact_line <- if (is.data.frame(country_metrics) && nrow(country_metrics) > 0 && "age_normalized_impact" %in% names(country_metrics)) {
    top_impact <- country_metrics |>
      dplyr::filter(is.finite(.data$age_normalized_impact)) |>
      dplyr::arrange(dplyr::desc(.data$age_normalized_impact)) |>
      utils::head(1)
    if (nrow(top_impact) > 0) {
      sprintf("  Age-normalized impact leader: %s (%.3f citations/year/article).", m3_report_title_case(top_impact$country[1]), .safe_num(top_impact$age_normalized_impact[1]))
    } else {
      "  Age-normalized impact: not estimable."
    }
  } else {
    "  Age-normalized impact: not estimable."
  }

  mcp_trend_line <- if (is.data.frame(scp_mcp_trend) && nrow(scp_mcp_trend) > 0) {
    sprintf("  MCP trend: annual MCP-share slope %.4f (p=%.4f; R2=%.3f).", .safe_num(scp_mcp_trend$slope[1]), .safe_num(scp_mcp_trend$p_value[1]), .safe_num(scp_mcp_trend$r_squared[1]))
  } else {
    "  MCP trend: not estimable."
  }

  robustness_line <- if (is.data.frame(robustness) && nrow(robustness) > 0) {
    rho <- robustness[robustness$method == "spearman", , drop = FALSE]
    if (nrow(rho) > 0) {
      sprintf("  Full/fractional robustness: Spearman rho %.3f; mean absolute rank shift %.2f.", .safe_num(rho$estimate[1]), .safe_num(rho$mean_abs_rank_change[1]))
    } else {
      "  Full/fractional robustness: not estimable."
    }
  } else {
    "  Full/fractional robustness: not estimable."
  }

  uncertainty_line <- if (is.data.frame(uncertainty) && nrow(uncertainty) > 0) {
    sprintf("  Uncertainty layer: %d bootstrap intervals exported for premium, dominance, and share-change claims.", nrow(uncertainty))
  } else {
    "  Uncertainty layer: no finite bootstrap intervals were available."
  }

  outlier_line <- if (is.data.frame(outliers) && nrow(outliers) > 0) {
    flagged <- sum(outliers$outlier_type != "within_expected_range", na.rm = TRUE)
    sprintf("  Geographic outliers: %d countries exceeded the residual impact-production threshold.", flagged)
  } else {
    "  Geographic outliers: not estimable."
  }

  hypothesis_line <- if (is.data.frame(hypotheses) && nrow(hypotheses) > 0) {
    sprintf(
      "  Advanced hypotheses: %d supported / %d not supported / %d inconclusive.",
      sum(hypotheses$decision == "supported", na.rm = TRUE),
      sum(hypotheses$decision == "not_supported", na.rm = TRUE),
      sum(hypotheses$decision == "inconclusive", na.rm = TRUE)
    )
  } else {
    "  Advanced hypotheses: not available."
  }

  c(
    "--- Advanced Journal Analytics ---",
    premium_line,
    concentration_line,
    mobility_line,
    trajectory_line,
    impact_line,
    mcp_trend_line,
    robustness_line,
    uncertainty_line,
    outlier_line,
    hypothesis_line
  )
}

m3_report_title_case <- function(x) {
  tools::toTitleCase(tolower(as.character(x)))
}
