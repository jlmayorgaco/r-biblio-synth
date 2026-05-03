# ============================================================================
# m4_report.R - M4 Sources / Journals & Venues report
# ============================================================================

build_m4_report <- function(result, config = biblio_config()) {
  stub <- list(status = "stub", title = "M4 Sources Report", sections = character(), lines = character(), tex = character())
  if (!inherits(result, "biblio_module_result")) return(stub)
  data <- result$data
  lines <- c(
    "==========================================",
    "M4 Sources / Journals & Venues Report",
    "==========================================",
    "",
    paste("Generated:", Sys.time()),
    paste("Status   :", result$status),
    sprintf("Sources  : %s", result$inputs$n_sources %||% NA_integer_),
    "",
    .m4_report_narrative(data),
    "",
    .m4_report_overview(data),
    "",
    .m4_report_bradford(data),
    "",
    .m4_report_growth(data),
    "",
    .m4_report_advanced_analytics(data),
    "",
    .m4_report_clusters(data)
  )
  tex <- c(
    "\\section*{M4 Sources / Journals \\& Venues Summary}",
    paste(lines, collapse = "\\\\")
  )
  list(
    status = "success",
    title = "M4 Sources / Journals & Venues Report",
    sections = c("narrative", "overview", "bradford", "growth", "advanced_analytics", "clusters"),
    lines = lines,
    tex = tex
  )
}

.m4_report_narrative <- function(data) {
  metrics <- data$narrative$metrics %||% data.frame()
  if (!is.data.frame(metrics) || nrow(metrics) == 0) return("--- Narrative Evidence: Not available ---")
  c("--- Narrative Evidence ---", paste0("  ", ieee_narrative_lines(metrics, max_lines = 8)))
}

.m4_report_overview <- function(data) {
  impact <- data$impact$impact %||% tibble::tibble()
  totals <- data$sources$totals %||% list()
  if (!is.data.frame(impact) || nrow(impact) == 0) return("--- Overview: Not available ---")
  top_tp <- impact[order(-impact$tp), , drop = FALSE][1, , drop = FALSE]
  top_tc <- impact[order(-impact$tc), , drop = FALSE][1, , drop = FALSE]
  c(
    "--- Source Overview ---",
    sprintf("  Total sources      : %s", totals$total_sources %||% nrow(impact)),
    sprintf("  Total documents    : %s", totals$total_documents %||% sum(impact$tp, na.rm = TRUE)),
    sprintf("  Total citations    : %s", totals$total_citations %||% sum(impact$tc, na.rm = TRUE)),
    sprintf("  Leading source TP  : %s (%s papers)", top_tp$source, top_tp$tp),
    sprintf("  Leading source TC  : %s (%s citations)", top_tc$source, top_tc$tc)
  )
}

.m4_report_bradford <- function(data) {
  zs <- data$bradford$zone_summary %||% tibble::tibble()
  if (!is.data.frame(zs) || nrow(zs) == 0) return("--- Bradford Zones: Not available ---")
  c(
    "--- Bradford Zones ---",
    paste0("  ", zs$bradford_zone, ": ", zs$n_sources, " sources, ", zs$tp, " papers, CPP ", round(zs$cpp, 2))
  )
}

.m4_report_growth <- function(data) {
  growth <- data$growth$growth %||% tibble::tibble()
  if (!is.data.frame(growth) || nrow(growth) == 0) return("--- Source Growth: Not available ---")
  top <- growth[order(-growth$tp_slope, -growth$tp), , drop = FALSE][1:min(5, nrow(growth)), , drop = FALSE]
  c(
    "--- Source Growth ---",
    paste0("  ", seq_len(nrow(top)), ". ", top$source, " - TP slope ", round(top$tp_slope, 3), "/year; CPP ", round(top$cpp, 2))
  )
}

.m4_report_advanced_analytics <- function(data) {
  advanced <- data$advanced_analytics %||% list()
  if (!is.list(advanced) || !identical(advanced$status %||% "", "success")) {
    return(c(
      "--- Advanced Analytics / ML ---",
      sprintf("  Status: %s", advanced$status %||% "unavailable"),
      sprintf("  Reason: %s", advanced$reason %||% "Advanced source analytics were not available.")
    ))
  }
  patterns <- advanced$patterns %||% character()
  svm <- advanced$svm %||% list()
  regression <- advanced$regression %||% list()
  silhouette <- advanced$silhouette %||% list()
  c(
    "--- Advanced Analytics / ML ---",
    paste0("  ", patterns),
    sprintf("  Classifier model: %s (%s).", svm$model %||% "unavailable", svm$status %||% "unknown"),
    sprintf("  Impact regression R-squared: %.3f.", ieee_safe_num(regression$r_squared)),
    sprintf("  Cluster silhouette: %.3f.", ieee_safe_num(silhouette$mean_silhouette))
  )
}

.m4_report_clusters <- function(data) {
  clusters <- data$clusters$clusters %||% tibble::tibble()
  if (!is.data.frame(clusters) || nrow(clusters) == 0) return("--- Source Clusters: Not available ---")
  counts <- as.data.frame(table(clusters$cluster), stringsAsFactors = FALSE)
  c(
    "--- Source K-means Clusters ---",
    paste0("  ", counts$Var1, ": ", counts$Freq, " sources")
  )
}
