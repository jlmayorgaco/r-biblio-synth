# ============================================================================
# report_service.R - Text report export service
# ============================================================================

#' Write a text report file
#'
#' @param lines Character vector. Lines of text.
#' @param path Character. Output file path.
#' @return Invisibly returns the path on success, NULL on failure.
#' @export
write_text_report <- function(lines, path) {
  # Validate input
  if (!is.character(lines)) {
    cli::cli_warn("lines must be a character vector")
    return(invisible(NULL))
  }
  
  if (length(lines) == 0) {
    cli::cli_warn("lines is empty, nothing to write")
    return(invisible(NULL))
  }
  
  # Validate path
  if (!is.character(path) || length(path) != 1) {
    cli::cli_warn("path must be a single character string")
    return(invisible(NULL))
  }
  
  # Create directory if needed
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir_created <- dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    if (!dir_created && !dir.exists(dir_path)) {
      cli::cli_warn("Failed to create directory: {dir_path}")
      return(invisible(NULL))
    }
  }
  
  # Write with error handling
  tryCatch({
    writeLines(lines, path)
    cli::cli_alert_info("Report saved: {path}")
    invisible(path)
  }, error = function(e) {
    cli::cli_warn("Failed to write report to {path}: {e$message}")
    invisible(NULL)
  })
}
