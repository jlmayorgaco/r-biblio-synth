# ============================================================================
# m1_compute_narrative.R - M1 narrative evidence metrics
# ============================================================================

compute_m1_narrative <- function(data, config = biblio_config()) {
  citation_summary <- data$citation_analysis$summary %||% data$citations$citation_summary %||% list()
  collaboration_summary <- data$collaboration$summary %||% list()
  keyword_metrics <- data$keyword_cooccurrence$metrics %||% list()
  bradford <- data$bradford$zone_summary %||% list()

  mcp_ratio <- NA_real_
  scp_mcp <- data$countries$scp_mcp_summary %||% data.frame()
  if (is.data.frame(scp_mcp) && nrow(scp_mcp) > 0 && "mcp_ratio" %in% names(scp_mcp)) {
    mcp_ratio <- mean(suppressWarnings(as.numeric(scp_mcp$mcp_ratio)), na.rm = TRUE)
  }

  zone1_share <- NA_real_
  if (is.list(bradford) && is.list(bradford$zone1)) {
    total_articles <- sum(
      ieee_safe_num(bradford$zone1$n_articles, 0),
      ieee_safe_num(bradford$zone2$n_articles, 0),
      ieee_safe_num(bradford$zone3$n_articles, 0)
    )
    if (is.finite(total_articles) && total_articles > 0) {
      zone1_share <- ieee_safe_num(bradford$zone1$n_articles, 0) / total_articles
    }
  }

  mean_citations <- ieee_safe_num(citation_summary$mean_citations)
  citation_h <- ieee_safe_num(citation_summary$h_index)

  metrics <- ieee_bind_metric_rows(
    ieee_metric_row(
      "M1", "Concentration", "Author productivity Gini",
      data$authors$author_gini %||% NA_real_,
      interpretation = "Identifies whether production is concentrated in a small author core.",
      digits = 3
    ),
    ieee_metric_row(
      "M1", "Concentration", "Country production Gini",
      data$countries$country_gini_articles %||% NA_real_,
      interpretation = "Shows whether geographic contribution is balanced or dominated by few countries.",
      digits = 3
    ),
    ieee_metric_row(
      "M1", "Concentration", "Bradford core share",
      zone1_share * 100,
      score = zone1_share,
      units = "%",
      digits = 1,
      interpretation = "Measures how much of the corpus is captured by core journals."
    ),
    ieee_metric_row(
      "M1", "Impact", "Mean citations",
      mean_citations,
      score = log1p(mean_citations) / log1p(max(mean_citations, 100, na.rm = TRUE)),
      interpretation = "Summarizes citation maturity of the corpus.",
      digits = 2
    ),
    ieee_metric_row(
      "M1", "Impact", "Bibliometric h-index",
      citation_h,
      score = log1p(citation_h) / log1p(max(citation_h, 50, na.rm = TRUE)),
      interpretation = "Captures whether impact is distributed across multiple cited papers.",
      digits = 1
    ),
    ieee_metric_row(
      "M1", "Collaboration", "Collaboration index",
      collaboration_summary$collaboration_index %||% NA_real_,
      score = ieee_safe_num(collaboration_summary$collaboration_index) / 5,
      interpretation = "Approximates author-team intensity in the corpus.",
      digits = 2
    ),
    ieee_metric_row(
      "M1", "Collaboration", "Mean MCP ratio",
      mcp_ratio,
      score = mcp_ratio / 100,
      units = "%",
      digits = 1,
      interpretation = "Indicates cross-country collaboration among productive countries."
    ),
    ieee_metric_row(
      "M1", "Conceptual Structure", "Keyword network density",
      keyword_metrics$density %||% NA_real_,
      score = keyword_metrics$density %||% NA_real_,
      interpretation = "Shows whether topic terms form a connected conceptual structure.",
      digits = 3
    )
  )

  list(
    status = if (nrow(metrics) > 0) "success" else "empty",
    metrics = metrics,
    narrative = ieee_narrative_lines(metrics, max_lines = 8)
  )
}
