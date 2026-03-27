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
               sections = list(), lines = character())

  if (!inherits(result, "biblio_module_result")) return(stub)

  data <- result$data

  lines <- c(
    "==========================================",
    "M3 Countries Module Report",
    "==========================================",
    "",
    paste("Generated:", Sys.time()),
    paste("Status   :", result$status),
    "",
    .m3_report_production(data),
    "",
    .m3_report_citations(data),
    "",
    .m3_report_scp_mcp(data),
    "",
    .m3_report_inequality(data),
    "",
    .m3_report_growth(data),
    "",
    .m3_report_profiles(data),
    "",
    .m3_report_experiments(data),
    "",
    .m3_report_caveats(data)
  )

  list(
    status   = "success",
    title    = "M3 Countries Module Report",
    sections = c("production", "citations", "scp_mcp", "inequality",
                 "growth", "profiles", "experiments", "caveats"),
    lines    = lines
  )
}

# ---------------------------------------------------------------------------
# Section builders
# ---------------------------------------------------------------------------

.m3_report_production <- function(data) {
  if (!"production" %in% names(data) ||
      length(data$production$production_summary) == 0) {
    return("--- Production: Not available ---")
  }
  s <- data$production$production_summary
  c(
    "--- Production ---",
    sprintf("  Total articles   : %d", s$total_articles),
    sprintf("  Countries active : %d", s$total_countries),
    sprintf("  Gini (articles)  : %.4f", .safe_num(s$gini_articles))
  )
}

.m3_report_citations <- function(data) {
  if (!"citations" %in% names(data) ||
      length(data$citations$citation_summary) == 0) {
    return("--- Citations: Not available ---")
  }
  s <- data$citations$citation_summary
  c(
    "--- Citations ---",
    sprintf("  Total citations  : %d", as.integer(s$total_citations)),
    sprintf("  Countries        : %d", s$total_countries),
    sprintf("  Gini (citations) : %.4f", .safe_num(s$gini_citations))
  )
}

.m3_report_scp_mcp <- function(data) {
  if (!"scp_mcp" %in% names(data) ||
      length(data$scp_mcp$scp_mcp_summary) == 0) {
    return("--- SCP/MCP: Not available ---")
  }
  s <- data$scp_mcp$scp_mcp_summary
  c(
    "--- Collaboration (SCP/MCP) ---",
    sprintf("  Total SCP        : %d", as.integer(s$total_scp)),
    sprintf("  Total MCP        : %d", as.integer(s$total_mcp)),
    sprintf("  Overall MCP ratio: %.1f%%", .safe_num(s$mcp_ratio))
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
    if (!is.null(gs$citations) && gs$citations$available_years > 0)
      sprintf("  Median cit  CAGR : %.2f%%",
              .safe_num(gs$citations$median_cagr) * 100)
    else "  Citation CAGR    : N/A (no annual citation data)"
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

.m3_report_caveats <- function(data) {
  c(
    "--- Limitations & Caveats ---",
    "  - Country normalization applies a fixed rule set; edge cases may exist.",
    "  - Multi-country documents are counted once per country (fractional",
    "    counting is not implemented).",
    "  - Change-point detection uses a simple mean-shift heuristic and is",
    "    exploratory only.",
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
