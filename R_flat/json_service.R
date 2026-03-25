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
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x, path, auto_unbox = auto_unbox, pretty = pretty)
  cli::cli_alert_info("JSON saved: {path}")
  invisible(path)
}
