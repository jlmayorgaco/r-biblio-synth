# ============================================================================
# m1_report.R - Report builder for M1
# ============================================================================

#' Build M1 report payload
#'
#' Creates a structured report payload from an M1 result.
#'
#' @param result A \code{biblio_module_result} object.
#' @param config A configuration list.
#' @return A list with \code{status}, \code{title}, \code{sections}, \code{lines}, \code{tex}.
#' @export
build_m1_report <- function(result, config = biblio_config()) {
  if (!inherits(result, "biblio_module_result")) {
    return(list(
      status   = "stub",
      title    = "M1 Main Information",
      sections = list(),
      lines    = character(),
      tex      = character()
    ))
  }

  # Build plain text report
  lines <- c(
    "==========================================",
    "M1 Main Information Report",
    "==========================================",
    "",
    paste("Generated:", Sys.time()),
    paste("Status:", result$status),
    "",
    "--- Overview ---",
    if ("overview" %in% names(result$data) && "summary_table" %in% names(result$data$overview)) {
      tbl <- result$data$overview$summary_table
      paste(tbl$metric, ":", tbl$value)
    } else {
      "Overview data not available"
    },
    "",
    "--- Document Types ---",
    if ("doc_types" %in% names(result$data) && "doc_type_table" %in% names(result$data$doc_types)) {
      dt <- result$data$doc_types$doc_type_table
      paste(dt$label, ":", dt$value, sprintf("(%.1f%%)", dt$percentage))
    } else {
      "Document types data not available"
    }
  )

  # Build LaTeX report for doc types
  tex_lines <- build_m1_doc_types_tex(result)

  list(
    status   = "success",
    title    = "M1 Main Information Report",
    sections = c("overview", "doc_types", "authors", "citations", "countries", "sources", "keywords", "bradford"),
    lines    = lines,
    tex      = tex_lines
  )
}

#' Build LaTeX document types report
#' @param result A \code{biblio_module_result} object.
#' @return Character vector of LaTeX lines.
build_m1_doc_types_tex <- function(result) {
  if (!inherits(result, "biblio_module_result")) return(character())
  if (!"doc_types" %in% names(result$data)) return(character())

  dt <- result$data$doc_types
  if (!"doc_type_table" %in% names(dt)) return(character())

  tbl <- dt$doc_type_table

  c(
    "\\documentclass[12pt]{article}",
    "\\usepackage{geometry}",
    "\\usepackage{graphicx}",
    "\\geometry{a4paper, margin=1in}",
    "\\begin{document}",
    "\\title{Document Types Distribution Report}",
    "\\author{RBiblioSynth}",
    "\\date{\\today}",
    "\\maketitle",
    "",
    "\\section*{Overview}",
    paste0("The total number of articles analyzed is \\textbf{", sum(tbl$value), "}."),
    "",
    "\\section*{Breakdown by Document Type}",
    "\\begin{itemize}",
    paste0(
      "\\item \\textbf{", tbl$label, "}: ",
      tbl$value, " (", sprintf("%.2f", tbl$percentage), "\\%)"
    ),
    "\\end{itemize}",
    "",
    "\\end{document}"
  )
}
