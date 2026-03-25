# ============================================================================
# report_service.R - Text report export service
# ============================================================================

#' Write a text report file
#'
#' @param lines Character vector. Lines of text.
#' @param path Character. Output file path.
#' @return Invisibly returns the path.
#' @export
write_text_report <- function(lines, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path)
  cli::cli_alert_info("Report saved: {path}")
  invisible(path)
}
