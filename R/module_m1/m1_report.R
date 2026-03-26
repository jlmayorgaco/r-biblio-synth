# ============================================================================
# m1_report.R - Complete report builder for M1
# ============================================================================

#' @export
build_m1_report <- function(result, config = biblio_config()) {
  if (!inherits(result, "biblio_module_result")) {
    return(list(status = "stub", title = "M1 Report", sections = list(), lines = character(), tex = character()))
  }

  data <- result$data
  lines <- c(
    "==========================================",
    "M1 Main Information Report",
    "==========================================",
    "",
    paste("Generated:", Sys.time()),
    paste("Status:", result$status),
    "",
    m1_report_overview(data),
    "",
    m1_report_doc_types(data),
    "",
    m1_report_authors(data),
    "",
    m1_report_citations(data),
    "",
    m1_report_countries(data),
    "",
    m1_report_sources(data),
    "",
    m1_report_keywords(data),
    "",
    m1_report_bradford(data)
  )

  tex_lines <- build_m1_doc_types_tex(result)

  list(
    status = "success",
    title = "M1 Main Information Report",
    sections = c("overview", "doc_types", "authors", "citations", "countries", "sources", "keywords", "bradford"),
    lines = lines,
    tex = tex_lines
  )
}

m1_report_overview <- function(data) {
  if (!"overview" %in% names(data) || !"summary_table" %in% names(data$overview)) {
    return("--- Overview: Not available ---")
  }
  c("--- Overview ---", paste(data$overview$summary_table$metric, ":", data$overview$summary_table$value))
}

m1_report_doc_types <- function(data) {
  if (!"doc_types" %in% names(data) || !"doc_type_table" %in% names(data$doc_types)) {
    return("--- Document Types: Not available ---")
  }
  dt <- data$doc_types$doc_type_table
  c("--- Document Types ---", paste(dt$label, ":", dt$value, sprintf("(%.1f%%", dt$percentage), ")"))
}

m1_report_authors <- function(data) {
  if (!"authors" %in% names(data) || !"top_authors" %in% names(data$authors)) {
    return("--- Authors: Not available ---")
  }
  au <- data$authors$top_authors
  gini <- if (!is.null(data$authors$author_gini)) sprintf("Gini: %.3f", data$authors$author_gini) else ""
  c("--- Top Authors ---", gini, paste(seq_len(nrow(au)), ".", au$value, "publications"))
}

m1_report_citations <- function(data) {
  if (!"citations" %in% names(data) || !"top_cited_documents" %in% names(data$citations)) {
    return("--- Citations: Not available ---")
  }
  cit <- data$citations$top_cited_documents
  c("--- Most Cited Papers ---", paste(seq_len(nrow(cit)), ".", cit$label, "-", cit$value, "citations"))
}

m1_report_countries <- function(data) {
  if (!"countries" %in% names(data) || !"top_countries_by_articles" %in% names(data$countries)) {
    return("--- Countries: Not available ---")
  }
  co <- data$countries$top_countries_by_articles
  gini <- if (!is.null(data$countries$country_gini_articles)) sprintf("Gini: %.3f", data$countries$country_gini_articles) else ""
  c("--- Top Countries ---", gini, paste(seq_len(nrow(co)), ".", trimws(co$label), "-", co$value, "articles"))
}

m1_report_sources <- function(data) {
  if (!"sources" %in% names(data) || !"top_sources" %in% names(data$sources)) {
    return("--- Sources: Not available ---")
  }
  so <- data$sources$top_sources
  gini <- if (!is.null(data$sources$source_gini)) sprintf("Gini: %.3f", data$sources$source_gini) else ""
  c("--- Top Sources ---", gini, paste(seq_len(nrow(so)), ".", trimws(so$label), "-", so$value, "articles"))
}

m1_report_keywords <- function(data) {
  if (!"keywords" %in% names(data) || !"top_keywords" %in% names(data$keywords)) {
    return("--- Keywords: Not available ---")
  }
  kw <- data$keywords$top_keywords
  c("--- Top Keywords ---", paste(seq_len(nrow(kw)), ".", kw$label, "-", kw$value))
}

m1_report_bradford <- function(data) {
  if (!"bradford" %in% names(data) || !"zone_summary" %in% names(data$bradford)) {
    return("--- Bradford: Not available ---")
  }
  zs <- data$bradford$zone_summary
  c(
    "--- Bradford Zones ---",
    paste("Core:", zs$zone1$n_sources, "sources,", zs$zone1$n_articles, "articles"),
    paste("Moderate:", zs$zone2$n_sources, "sources,", zs$zone2$n_articles, "articles"),
    paste("Peripheral:", zs$zone3$n_sources, "sources,", zs$zone3$n_articles, "articles")
  )
}

#' @export
build_m1_doc_types_tex <- function(result) {
  if (!inherits(result, "biblio_module_result") || !"doc_types" %in% names(result$data)) return(character())
  dt <- result$data$doc_types
  if (!"doc_type_table" %in% names(dt)) return(character())
  tbl <- dt$doc_type_table

  c(
    "\\documentclass[12pt]{article}",
    "\\usepackage{geometry}",
    "\\usepackage{graphicx}",
    "\\geometry{a4paper, margin=1in}",
    "\\begin{document}",
    "\\title{M1 Main Information Report}",
    "\\author{RBiblioSynth}",
    "\\date{\\today}",
    "\\maketitle",
    "",
    "\\section*{Document Types}",
    "\\begin{itemize}",
    paste0("\\item \\textbf{", tbl$label, "}: ", tbl$value, " (", sprintf("%.2f", tbl$percentage), "\\%)"),
    "\\end{itemize}",
    "",
    "\\end{document}"
  )
}
