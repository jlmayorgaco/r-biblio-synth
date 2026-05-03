# ============================================================================
# m3_compute_narrative.R - M3 narrative evidence metrics
# ============================================================================

m3_compute_narrative <- function(data, prepared_data = list(), config = biblio_config()) {
  prod <- data$production$production_summary %||% list()
  cit <- data$citations$citation_summary %||% list()
  ineq <- data$inequality$inequality_summary %||% list()
  scp_mcp <- data$scp_mcp$scp_mcp_summary %||% list()
  growth <- data$growth_dynamics$growth_summary %||% list()

  mcp_ratio <- ieee_safe_num(scp_mcp$mcp_ratio)
  mcp_score <- if (is.finite(mcp_ratio) && mcp_ratio > 1) mcp_ratio / 100 else mcp_ratio

  prod_top5 <- ieee_safe_num(ineq$production$top5_share)
  cit_top5 <- ieee_safe_num(ineq$citations$top5_share)
  prod_gini <- ieee_safe_num(ineq$production$gini, ieee_safe_num(prod$gini_articles))
  cit_gini <- ieee_safe_num(ineq$citations$gini, ieee_safe_num(cit$gini_citations))
  prod_cagr <- ieee_safe_num(growth$productivity$median_cagr)
  cit_cagr <- ieee_safe_num(growth$citations$median_cagr)

  n_countries <- ieee_safe_num(prod$total_countries)
  coverage_score <- log1p(n_countries) / log1p(max(n_countries, 50, na.rm = TRUE))

  metrics <- ieee_bind_metric_rows(
    ieee_metric_row(
      "M3", "Coverage", "Active countries",
      n_countries,
      score = coverage_score,
      digits = 0,
      interpretation = "Shows the geographic breadth of the corpus."
    ),
    ieee_metric_row(
      "M3", "Concentration", "Production Gini",
      prod_gini,
      score = prod_gini,
      digits = 3,
      interpretation = "Quantifies geographic inequality in publication output."
    ),
    ieee_metric_row(
      "M3", "Concentration", "Production top-5 share",
      prod_top5 * 100,
      score = prod_top5,
      units = "%",
      digits = 1,
      interpretation = "Measures how strongly the leading countries dominate publication volume."
    ),
    ieee_metric_row(
      "M3", "Impact", "Citation Gini",
      cit_gini,
      score = cit_gini,
      digits = 3,
      interpretation = "Shows whether citation impact is geographically concentrated."
    ),
    ieee_metric_row(
      "M3", "Impact", "Citation top-5 share",
      cit_top5 * 100,
      score = cit_top5,
      units = "%",
      digits = 1,
      interpretation = "Highlights whether global citation visibility is led by a small country group."
    ),
    ieee_metric_row(
      "M3", "Collaboration", "Overall MCP ratio",
      if (is.finite(mcp_ratio) && mcp_ratio <= 1) mcp_ratio * 100 else mcp_ratio,
      score = mcp_score,
      units = "%",
      digits = 1,
      interpretation = "Indicates the international collaboration share among country-level records."
    ),
    ieee_metric_row(
      "M3", "Growth", "Median production CAGR",
      prod_cagr * 100,
      score = min(abs(prod_cagr) * 5, 1),
      units = "%",
      digits = 2,
      interpretation = "Summarizes the typical country-level publication growth trajectory."
    ),
    ieee_metric_row(
      "M3", "Growth", "Median citation CAGR",
      cit_cagr * 100,
      score = min(abs(cit_cagr) * 5, 1),
      units = "%",
      digits = 2,
      interpretation = "Summarizes whether country citation visibility is expanding over time."
    )
  )

  list(
    status = if (nrow(metrics) > 0) "success" else "empty",
    metrics = metrics,
    narrative = ieee_narrative_lines(metrics, max_lines = 8)
  )
}
