# ============================================================================
# json_service.R - JSON export service
# ============================================================================

#' Write a JSON artifact file
#'
#' @param x Object to serialize.
#' @param path Character. Output file path.
#' @param auto_unbox Logical. Auto-unbox single-element vectors.
#' @param pretty Logical. Pretty-print JSON.
#' @return Invisibly returns the path.
#' @export
write_json_artifact <- function(x, path,
                                auto_unbox = TRUE,
                                pretty = TRUE) {
  # Create directory if needed
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir_created <- dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    if (!dir_created && !dir.exists(dir_path)) {
      cli::cli_warn("Failed to create directory: {dir_path}")
      return(invisible(NULL))
    }
  }
  
  # Validate input is serializable
  if (is.null(x)) {
    cli::cli_warn("Cannot write NULL object to JSON")
    return(invisible(NULL))
  }
  
  # Write with error handling
  tryCatch({
    jsonlite::write_json(x, path, auto_unbox = auto_unbox, pretty = pretty)
    cli::cli_alert_info("JSON saved: {path}")
  }, error = function(e) {
    cli::cli_warn("Failed to write JSON to {path}: {e$message}")
  })
  
  invisible(path)
}
