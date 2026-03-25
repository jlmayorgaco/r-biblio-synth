# ============================================================================
# paths.R - Path building helpers
# ============================================================================

#' Build module output directory path
#'
#' @param module_id Character. Module identifier (e.g., "m1").
#' @param kind Character. Subdirectory kind ("plots", "json", "reports", "tables").
#' @param config A configuration list from \code{\link{biblio_config}}.
#' @param create Logical. Create the directory if it does not exist.
#' @return Character. The resolved path.
#' @export
build_output_path <- function(module_id, kind,
                              config = biblio_config(),
                              create = TRUE) {
  path <- file.path(config$output_dir, module_id, kind)
  if (create && !dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

#' Build a specific artifact file path
#'
#' @param module_id Character. Module identifier.
#' @param kind Character. Artifact kind ("plots", "json", "reports").
#' @param filename Character. Base filename without extension.
#' @param ext Character. File extension (e.g., "png", "json").
#' @param config A configuration list from \code{\link{biblio_config}}.
#' @param create Logical. Create parent directory if needed.
#' @return Character. Full file path.
#' @export
build_artifact_path <- function(module_id, kind, filename, ext,
                                config = biblio_config(),
                                create = TRUE) {
  dir_path <- build_output_path(module_id, kind, config, create)
  file.path(dir_path, paste0(filename, ".", ext))
}
