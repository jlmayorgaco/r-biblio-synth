# ============================================================================
# m3_report.R - Structured report builder for M3
# ============================================================================

#' Build M3 text report
#'
#' @param result A \code{biblio_module_result} object (or list with \code{data} slot).
#' @param config A configuration list.
#' @return A list with \code{status}, \code{title}, \code{sections}, \code{lines}.
#' @export
build_m3_report <- function(result, config = biblio_config()) {
  stub <- list(status = "stub", title = "M3 Countries Report",
               sections = list(), lines = character(), tex = character())

  if (!inherits(result, "biblio_module_result")) return(stub)

  data <- result$data

  lines <- c(
    "==========================================",
    "M3 Countries Module Report",
    "==========================================",
    "",
    paste("Generated:", Sys.time()),
    paste("Status   :", result$status),
    sprintf("Countries: %s", .safe_int(result$inputs$n_countries)),
    "",
    .m3_report_narrative(data),
    "",
    .m3_report_overview(data),
    "",
    .m3_report_production(data),
    "",
    .m3_report_citations(data),
    "",
    .m3_report_scp_mcp(data),
    "",
    .m3_report_positioning(data),
    "",
    .m3_report_hypotheses(data),
    "",
    .m3_report_inequality(data),
    "",
    .m3_report_growth(data),
    "",
    .m3_report_change_points(data),
    "",
    .m3_report_profiles(data),
    "",
    .m3_report_spatial(data),
    "",
    .m3_report_regional(data),
    "",
    .m3_report_economic(data),
    "",
    .m3_report_temporal_dynamics(data),
    "",
    .m3_report_advanced_journal(data),
    "",
    .m3_report_experiments(data),
    "",
    .m3_report_caveats(data)
  )

  tex <- c(
    "\\section*{M3 Countries Module Summary}",
    sprintf("\\textbf{Generated}: %s\\\\", format(Sys.time(), tz = "", usetz = FALSE)),
    sprintf("\\textbf{Status}: %s\\\\", result$status),
    sprintf("\\textbf{Countries analyzed}: %s\\\\", .safe_int(result$inputs$n_countries)),
    "",
    "\\subsection*{Narrative Evidence}",
    paste(.m3_report_narrative(data), collapse = "\\\\"),
    "",
    "\\subsection*{Executive Overview}",
    paste(.m3_report_overview(data), collapse = "\\\\"),
    "",
    "\\subsection*{Production}",
    paste(.m3_report_production(data), collapse = "\\\\"),
    "",
    "\\subsection*{Citations}",
    paste(.m3_report_citations(data), collapse = "\\\\"),
    "",
    "\\subsection*{Collaboration}",
    paste(.m3_report_scp_mcp(data), collapse = "\\\\"),
    "",
    "\\subsection*{Country Positioning and Trajectories}",
    paste(.m3_report_positioning(data), collapse = "\\\\"),
    "",
    "\\subsection*{Statistical Hypotheses}",
    paste(.m3_report_hypotheses(data), collapse = "\\\\"),
    "",
    "\\subsection*{Inequality}",
    paste(.m3_report_inequality(data), collapse = "\\\\"),
    "",
    "\\subsection*{Growth and Change Points}",
    paste(c(.m3_report_growth(data), .m3_report_change_points(data)), collapse = "\\\\"),
    "",
    "\\subsection*{Profiles, Spatial and Regional Structure}",
    paste(c(.m3_report_profiles(data), .m3_report_spatial(data), .m3_report_regional(data)), collapse = "\\\\"),
    "",
    "\\subsection*{Economic Correlates}",
    paste(.m3_report_economic(data), collapse = "\\\\"),
    "",
    "\\subsection*{Temporal Dynamics}",
    paste(.m3_report_temporal_dynamics(data), collapse = "\\\\"),
    "",
    "\\subsection*{Advanced Journal Analytics}",
    paste(.m3_report_advanced_journal(data), collapse = "\\\\"),
    "",
    "\\subsection*{Caveats}",
    paste(.m3_report_caveats(data), collapse = "\\\\")
  )

  list(
    status   = "success",
    title    = "M3 Countries Module Report",
    sections = c("narrative", "overview", "production", "citations", "scp_mcp", "positioning", "hypotheses", "inequality",
                 "growth", "change_points", "profiles", "spatial",
                 "regional", "economic", "temporal_dynamics", "advanced_journal", "experiments", "caveats"),
    lines    = lines,
    tex      = tex
  )
}

# ---------------------------------------------------------------------------
# Section builders
# ---------------------------------------------------------------------------

.m3_report_overview <- function(data) {
  top_prod <- .m3_extract_ranked_item(
    data$production$top_countries_by_production,
    value_candidates = c("value", "article_count", "n_articles")
  )
  top_cit <- .m3_extract_ranked_item(
    data$citations$top_countries_by_citations,
    value_candidates = c("value", "total_citations", "citations")
  )

  c(
    "--- Executive Overview ---",
    sprintf("  Leading producer  : %s", .m3_format_ranked_item(top_prod, digits = 0)),
    sprintf("  Leading citer     : %s", .m3_format_ranked_item(top_cit, digits = 0))
  )
}

.m3_report_narrative <- function(data) {
  narrative <- data$narrative %||% list()
  metrics <- narrative$metrics %||% data.frame()
  if (!is.data.frame(metrics) || nrow(metrics) == 0) {
    return(c(
      "--- Narrative Evidence ---",
      "  Not available: geographic evidence metrics could not be normalized."
    ))
  }
  c(
    "--- Narrative Evidence ---",
    paste0("  ", ieee_narrative_lines(metrics, max_lines = 8))
  )
}

.m3_report_production <- function(data) {
  if (!"production" %in% names(data) ||
      length(data$production$production_summary) == 0) {
    return("--- Production: Not available ---")
  }
  s <- data$production$production_summary
  top_prod <- .m3_extract_ranked_item(
    data$production$top_countries_by_production,
    value_candidates = c("value", "article_count", "n_articles")
  )
  c(
    "--- Production ---",
    sprintf("  Total articles   : %d", s$total_articles),
    sprintf("  Countries active : %d", s$total_countries),
    sprintf("  Gini (articles)  : %.4f", .safe_num(s$gini_articles)),
    sprintf("  Top producer     : %s", .m3_format_ranked_item(top_prod, digits = 0))
  )
}

.m3_report_citations <- function(data) {
  if (!"citations" %in% names(data) ||
      length(data$citations$citation_summary) == 0) {
    return("--- Citations: Not available ---")
  }
  s <- data$citations$citation_summary
  top_cit <- .m3_extract_ranked_item(
    data$citations$top_countries_by_citations,
    value_candidates = c("value", "total_citations", "citations")
  )
  c(
    "--- Citations ---",
    sprintf("  Total citations  : %d", as.integer(s$total_citations)),
    sprintf("  Countries        : %d", s$total_countries),
    sprintf("  Gini (citations) : %.4f", .safe_num(s$gini_citations)),
    sprintf("  Top cited        : %s", .m3_format_ranked_item(top_cit, digits = 0))
  )
}

.m3_report_scp_mcp <- function(data) {
  if (!"scp_mcp" %in% names(data) ||
      length(data$scp_mcp$scp_mcp_summary) == 0) {
    return("--- SCP/MCP: Not available ---")
  }
  s <- data$scp_mcp$scp_mcp_summary
  top_collab <- .m3_extract_ranked_item(
    data$scp_mcp$top_collaborators,
    label_candidates = c("country", "label"),
    value_candidates = c("mcp_ratio", "value")
  )
  top_collab_value <- .safe_num(top_collab$value)
  c(
    "--- Collaboration (SCP/MCP) ---",
    sprintf("  Total SCP        : %d", as.integer(s$total_scp)),
    sprintf("  Total MCP        : %d", as.integer(s$total_mcp)),
    sprintf("  Overall MCP ratio: %.1f%%", .safe_num(s$mcp_ratio)),
    sprintf(
      "  Top collaborator : %s",
      if (!is.na(top_collab_value)) {
        sprintf("%s (%.1f%% MCP)", top_collab$label, 100 * top_collab_value)
      } else {
        "N/A"
      }
    )
  )
}

.m3_report_positioning <- function(data) {
  pos <- data$positioning %||% list()
  table <- pos$table %||% tibble::tibble()
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(c(
      "--- Country Positioning and Trajectories ---",
      "  Not available: country positioning metrics could not be computed."
    ))
  }

  notes <- pos$quadrant_notes %||% m3_positioning_quadrant_notes()
  top_mcp <- table[order(-table$mcp_slope, -table$mcp), , drop = FALSE]
  top_tc <- table[order(-table$tc_slope, -table$tc), , drop = FALSE]
  clusters <- table$cluster[!is.na(table$cluster)]

  c(
    "--- Country Positioning and Trajectories ---",
    sprintf("  Projection horizon: %s years.", .safe_int(pos$horizon, default = 3L)),
    sprintf("  SCP/MCP quadrant split: median SCP and median MCP among displayed countries."),
    paste0("  - ", notes$scp_mcp),
    sprintf("  TC/TP quadrant split: median TP and median TC among displayed countries."),
    paste0("  - ", notes$tc_tp),
    if (nrow(top_mcp) > 0) sprintf(
      "  Strongest MCP trajectory: %s (MCP slope %.3f/year; SCP slope %.3f/year).",
      top_mcp$country[1], .safe_num(top_mcp$mcp_slope[1]), .safe_num(top_mcp$scp_slope[1])
    ) else NULL,
    if (nrow(top_tc) > 0) sprintf(
      "  Strongest TC trajectory: %s (TC slope %.3f/year; TP slope %.3f/year).",
      top_tc$country[1], .safe_num(top_tc$tc_slope[1]), .safe_num(top_tc$tp_slope[1])
    ) else NULL,
    if (length(clusters) > 0) sprintf(
      "  K-means retained %d country clusters from standardized TP, TC, SCP, MCP, MCP share, and TC/TP features.",
      length(unique(clusters))
    ) else "  K-means clusters: not available."
  )
}

.m3_report_inequality <- function(data) {
  if (!"inequality" %in% names(data) ||
      length(data$inequality$inequality_summary) == 0) {
    return("--- Inequality: Not available ---")
  }
  s <- data$inequality$inequality_summary
  c(
    "--- Inequality / Concentration ---",
    sprintf("  Production Gini  : %.4f", .safe_num(s$production$gini)),
    sprintf("  Production HHI   : %.4f", .safe_num(s$production$hhi)),
    sprintf("  Production Top-5 : %.1f%%",
            .safe_num(s$production$top5_share) * 100),
    sprintf("  Citations  Gini  : %.4f", .safe_num(s$citations$gini)),
    sprintf("  Citations  HHI   : %.4f", .safe_num(s$citations$hhi)),
    sprintf("  Citations  Top-5 : %.1f%%",
            .safe_num(s$citations$top5_share) * 100)
  )
}

.m3_report_hypotheses <- function(data) {
  hyp <- data$hypotheses$hypotheses %||% data$hypotheses$hyphypotheses %||% list()
  if (!is.list(hyp) || length(hyp) == 0) {
    return(c(
      "--- Statistical Hypotheses ---",
      "  Not available: hypothesis tests were not computed."
    ))
  }
  rows <- Filter(function(x) is.list(x), hyp)
  statistical <- rows[vapply(rows, function(x) identical(x$evidence_class %||% if (!is.null(x$p_value)) "statistical" else "heuristic", "statistical"), logical(1))]
  rejected <- statistical[vapply(statistical, function(x) identical(x$result %||% "", "reject"), logical(1))]
  p_values <- vapply(statistical, function(x) {
    value <- suppressWarnings(as.numeric(x$p_value))
    if (length(value) == 1L && is.finite(value)) value else Inf
  }, numeric(1))
  strongest <- if (length(statistical) > 0 && any(is.finite(p_values))) {
    statistical[[which.min(p_values)]]
  } else {
    NULL
  }
  c(
    "--- Statistical Hypotheses ---",
    sprintf("  Statistical tests: %d; rejected nulls: %d.", length(statistical), length(rejected)),
    if (!is.null(strongest)) {
      sprintf("  Strongest finding: %s", strongest$interpretation %||% strongest$hypothesis %||% strongest$hyphypothesis)
    } else {
      "  Strongest finding: not estimable."
    },
    "  Added tests cover regional adjusted impact, role-cluster differences, MCP mediation, inequality gaps, emerging-country acceleration, network modularity, and trajectory direction."
  )
}

.m3_report_growth <- function(data) {
  if (!"growth_dynamics" %in% names(data)) return("--- Growth Dynamics: Not available ---")
  gs <- data$growth_dynamics$growth_summary

  if (is.null(gs) || gs$productivity$available_years == 0) {
    return(c(
      "--- Growth Dynamics ---",
      "  Not available: no annual publication data in input."
    ))
  }

  c(
    "--- Growth Dynamics ---",
    sprintf("  Years covered    : %d", gs$productivity$available_years),
    sprintf("  Median prod CAGR : %.2f%%",
            .safe_num(gs$productivity$median_cagr) * 100),
    sprintf("  Median prod slope: %.3f", .safe_num(gs$productivity$median_slope)),
    if (!is.null(gs$citations) && gs$citations$available_years > 0)
      sprintf("  Median cit  CAGR : %.2f%%",
              .safe_num(gs$citations$median_cagr) * 100)
    else "  Citation CAGR    : N/A (no annual citation data)"
  )
}

.m3_report_change_points <- function(data) {
  if (!"change_points" %in% names(data) ||
      is.null(data$change_points$change_point_summary)) {
    return("--- Change Points: Not available ---")
  }

  cps <- data$change_points$change_point_summary
  c(
    "--- Structural Breaks ---",
    sprintf("  Productivity CPs : %d", .safe_int(cps$productivity$n_change_points)),
    sprintf("  Citation CPs     : %d", .safe_int(cps$citations$n_change_points)),
    sprintf(
      "  Interpretive note: %s",
      if (.safe_int(cps$productivity$n_change_points, default = 0L) + .safe_int(cps$citations$n_change_points, default = 0L) == 0L) {
        "no stable country-level breakpoints were retained under the current thresholds."
      } else {
        "detected breaks are exploratory and should be triangulated with M2 before strong causal interpretation."
      }
    )
  )
}

.m3_report_profiles <- function(data) {
  if (!"profiles" %in% names(data)) return("--- Profiles: Not available ---")
  ps <- data$profiles$profile_summary
  if (is.null(ps)) return("--- Profiles: Not available ---")
  c(
    "--- Country Profiles ---",
    sprintf("  Countries profiled  : %d", .safe_int(ps$n_countries)),
    sprintf("  Features used       : %d", .safe_int(ps$n_features)),
    sprintf("  PCA performed       : %s", if (isTRUE(ps$pca_performed)) "Yes" else "No"),
    sprintf("  Clustering performed: %s", if (isTRUE(ps$clustering_performed)) "Yes" else "No"),
    if (isTRUE(ps$clustering_performed))
      sprintf("  Clusters (k)        : %d", .safe_int(ps$k_for_clustering))
    else character()
  )
}

.m3_report_spatial <- function(data) {
  spatial <- data$spatial
  if (is.null(spatial) || !identical(spatial$status, "success")) {
    return("--- Spatial Structure: Not available ---")
  }

  morans_i <- .safe_num(spatial$morans_i$statistic)
  morans_p <- .safe_num(spatial$morans_i$p_value)
  hotspots <- if (!is.null(spatial$getis_ord$hotspots)) nrow(spatial$getis_ord$hotspots) else 0L

  c(
    "--- Spatial Structure ---",
    sprintf("  Moran's I        : %s", .m3_format_metric(morans_i, digits = 4)),
    sprintf("  Moran p-value    : %s", .m3_format_metric(morans_p, digits = 4, fallback = "not estimable")),
    sprintf("  Hotspots flagged : %d", hotspots),
    sprintf(
      "  Interpretive note: %s",
      if (is.finite(morans_p)) {
        if (morans_p <= 0.05) "spatial clustering is statistically supported." else "spatial clustering is weak under the current country sample."
      } else {
        "global spatial significance was not estimable under the current neighborhood structure."
      }
    )
  )
}

.m3_report_regional <- function(data) {
  regional <- data$regional
  if (is.null(regional) || !identical(regional$status, "success")) {
    return("--- Regional Structure: Not available ---")
  }

  continent_summary <- regional$by_continent$summary
  lead_region <- .m3_extract_ranked_item(
    continent_summary,
    label_candidates = c("region"),
    value_candidates = c("share", "total_production")
  )
  c(
    "--- Regional Structure ---",
    sprintf("  Leading region   : %s", .m3_format_ranked_item(lead_region, digits = 2))
  )
}

.m3_report_economic <- function(data) {
  economic <- data$economic
  if (is.null(economic) || !identical(economic$status, "success")) {
    return("--- Economic Correlates: Not available ---")
  }

  regression <- economic$regression
  matched_n <- if (!is.null(economic$matched_data)) nrow(economic$matched_data) else 0L

  c(
    "--- Economic Correlates ---",
    sprintf("  Matched countries: %d", matched_n),
    sprintf("  Regression R2    : %s", .m3_format_metric(.safe_num(regression$R_squared), digits = 4)),
    sprintf("  GDP beta         : %s", .m3_format_metric(.safe_num(regression$coefficients["gdp_usd"]), digits = 4, fallback = "not estimable")),
    sprintf("  HDI beta         : %s", .m3_format_metric(.safe_num(regression$coefficients["hdi"]), digits = 4, fallback = "not estimable")),
    sprintf(
      "  Interpretive note: %s",
      if (matched_n < 8 || !is.finite(.safe_num(regression$R_squared))) {
        "economic relationships remain exploratory because the matched sample is limited."
      } else {
        "economic coefficients are directional associations and should not be interpreted causally."
      }
    )
  )
}

.m3_report_experiments <- function(data) {
  if (!"experiments" %in% names(data)) return("--- Experiments: Not available ---")
  es <- data$experiments$experiments_summary
  if (is.null(es)) return("--- Experiments (Exploratory): Not available ---")
  lines <- c(
    "--- Experiments (Exploratory) ---",
    sprintf("  Quadrant analysis       : %s",
            if (isTRUE(es$quadrant_performed)) "Yes" else "No"),
    sprintf("  Concentration sensitivity: %s",
            if (isTRUE(es$concentration_performed)) "Yes" else "No"),
    sprintf("  Momentum scoring        : %s",
            if (isTRUE(es$momentum_performed)) "Yes" else "No")
  )
  if (length(es$notes) > 0) {
    lines <- c(lines, "  Notes:", paste0("    - ", es$notes))
  }
  lines
}

.m3_report_temporal_dynamics <- function(data) {
  temporal <- data$temporal_dynamics
  if (is.null(temporal) || !identical(temporal$status, "success")) {
    return("--- Temporal Dynamics: Not available ---")
  }

  share_trends <- temporal$share_evolution$share_trends
  lead_gainer <- if (is.data.frame(share_trends) && nrow(share_trends) > 0) {
    share_trends[which.max(share_trends$change), , drop = FALSE]
  } else {
    NULL
  }

  lead_decliner <- if (is.data.frame(share_trends) && nrow(share_trends) > 0) {
    share_trends[which.min(share_trends$change), , drop = FALSE]
  } else {
    NULL
  }

  mobility <- temporal$rank_mobility$volatility$overall %||% NA_real_
  emergence_counts <- temporal$emergence$emergence_counts
  peak_emergence <- if (!is.null(emergence_counts) && length(emergence_counts) > 0) {
    idx <- which.max(as.numeric(emergence_counts))
    sprintf("%s (%d new countries)", names(emergence_counts)[idx], as.integer(emergence_counts[idx]))
  } else {
    "N/A"
  }

  c(
    "--- Temporal Dynamics ---",
    sprintf("  Rank mobility    : %s", .m3_format_metric(.safe_num(mobility), digits = 2, fallback = "not estimable")),
    sprintf(
      "  Largest share gain: %s",
      if (!is.null(lead_gainer) && nrow(lead_gainer) == 1) {
        sprintf("%s (%.2f pp)", .m3_pretty_label(lead_gainer$country[1]), .safe_num(lead_gainer$change[1]))
      } else {
        "N/A"
      }
    ),
    sprintf(
      "  Largest share loss: %s",
      if (!is.null(lead_decliner) && nrow(lead_decliner) == 1) {
        sprintf("%s (%.2f pp)", .m3_pretty_label(lead_decliner$country[1]), .safe_num(lead_decliner$change[1]))
      } else {
        "N/A"
      }
    ),
    sprintf("  Peak entry wave  : %s", peak_emergence),
    sprintf(
      "  Trajectory signal: %s",
      if (!is.finite(.safe_num(mobility))) {
        "rank redistribution was not estimable under the current time windows."
      } else if (.safe_num(mobility) >= 3) {
        "leadership is fluid, with substantial rank turnover across countries."
      } else if (.safe_num(mobility) >= 1) {
        "leadership changed moderately between the initial and final windows."
      } else {
        "leadership remained comparatively stable across the observation windows."
      }
    )
  )
}

.m3_report_caveats <- function(data) {
  c(
    "--- Limitations & Caveats ---",
    "  - Country normalization applies a fixed rule set; edge cases may exist.",
    "  - Multi-country results should be interpreted alongside the selected",
    "    counting mode and SCP/MCP breakdown.",
    "  - Change-point detection in M3 is exploratory and should be confirmed",
    "    with the richer breakpoint engine available in M2.",
    "  - Clustering uses k=2 by default; interpret with care for small samples.",
    "  - Shapiro-Wilk tests are informative only; p-values should not be",
    "    over-interpreted given non-random bibliometric samples."
  )
}

# ---------------------------------------------------------------------------
# Utilities
# ---------------------------------------------------------------------------

.safe_num <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0) default else as.numeric(x[[1]])
}

.safe_int <- function(x, default = NA_integer_) {
  if (is.null(x) || length(x) == 0) default else as.integer(x[[1]])
}

.m3_extract_ranked_item <- function(df,
                                    label_candidates = c("label", "country", "region"),
                                    value_candidates = c("value", "article_count", "total_citations", "share")) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(list(label = "N/A", value = NA_real_))
  }

  label_col <- label_candidates[label_candidates %in% names(df)][1]
  value_col <- value_candidates[value_candidates %in% names(df)][1]
  if (is.na(label_col) || is.na(value_col)) {
    return(list(label = "N/A", value = NA_real_))
  }

  values <- suppressWarnings(as.numeric(df[[value_col]]))
  idx <- if (all(is.na(values))) 1L else which.max(values)[1]

  list(
    label = .m3_pretty_label(df[[label_col]][idx]),
    value = values[idx],
    value_name = value_col
  )
}

.m3_format_ranked_item <- function(item, digits = 1) {
  if (is.null(item) || is.na(item$value)) {
    return("N/A")
  }
  value <- item$value
  value_name <- item$value_name %||% ""
  if (identical(value_name, "share")) {
    pct <- if (is.finite(value) && abs(value) <= 1) 100 * value else value
    return(sprintf("%s (%.1f%%)", item$label, pct))
  }
  sprintf("%s (%s)", item$label, format(round(value, digits), trim = TRUE, nsmall = digits))
}

.m3_pretty_label <- function(x) {
  x <- trimws(as.character(x %||% "N/A"))
  if (!nzchar(x) || identical(toupper(x), "NA")) {
    return("N/A")
  }
  tools::toTitleCase(tolower(gsub("\\s+", " ", x)))
}

.m3_format_metric <- function(x, digits = 2, fallback = "N/A") {
  value <- suppressWarnings(as.numeric(x[[1]]))
  if (!length(value) || !is.finite(value)) {
    return(fallback)
  }
  format(round(value, digits), trim = TRUE, nsmall = digits)
}
