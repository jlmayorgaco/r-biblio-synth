# ============================================================================
# contracts.R - Standard result contracts for all modules
# ============================================================================

#' Create a new module result object
#'
#' Constructs a standardized result list for any module output.
#'
#' @param module_id Character. Module identifier (e.g., "m1").
#' @param module_name Character. Human-readable module name.
#' @param status Character. One of "success", "stub", "error", "warning".
#' @param inputs List. Inputs used.
#' @param data List. Computed results.
#' @param diagnostics List. Warnings, checks, notes.
#' @param artifacts List. Plots, tables, files, reports.
#' @param metadata List. Additional metadata.
#' @return An object of class \code{c("biblio_module_result", "list")}.
#' @export
new_module_result <- function(module_id   = "unknown",
                              module_name = "Unknown Module",
                              status      = "stub",
                              inputs      = list(),
                              data        = list(),
                              diagnostics = list(
                                warnings = character(),
                                checks   = list(),
                                notes    = character()
                              ),
                              artifacts   = list(
                                plots   = list(),
                                tables  = list(),
                                files   = list(),
                                reports = list()
                              ),
                              metadata    = list()) {
  structure(
    list(
      module_id   = module_id,
      module_name = module_name,
      status      = status,
      inputs      = inputs,
      data        = data,
      diagnostics = diagnostics,
      artifacts   = artifacts,
      metadata    = metadata
    ),
    class = c("biblio_module_result", "list")
  )
}

#' Create a new artifact manifest
#'
#' @param module_id Character. Module identifier.
#' @param generated_at Character or POSIXct. Timestamp.
#' @param files Character vector. Exported file paths.
#' @param plots Character vector. Exported plot paths.
#' @param tables Character vector. Exported table paths.
#' @param reports Character vector. Exported report paths.
#' @param status Character. "success", "stub", "error".
#' @return An object of class \code{c("biblio_artifact_manifest", "list")}.
#' @export
new_artifact_manifest <- function(module_id   = "unknown",
                                  generated_at = Sys.time(),
                                  files        = character(),
                                  plots        = character(),
                                  tables       = character(),
                                  reports      = character(),
                                  status       = "stub") {
  structure(
    list(
      module_id    = module_id,
      generated_at = generated_at,
      files        = files,
      plots        = plots,
      tables       = tables,
      reports      = reports,
      status       = status
    ),
    class = c("biblio_artifact_manifest", "list")
  )
}

#' Validate a module result object
#'
#' Checks that a module result has the required structure.
#'
#' @param x An object to validate.
#' @return Logical. TRUE if valid, FALSE otherwise.
#' @export
validate_module_result <- function(x) {
  if (!inherits(x, "biblio_module_result")) {
    return(FALSE)
  }
  required <- c("module_id", "module_name", "status", "data", "diagnostics", "artifacts")
  all(required %in% names(x))
}
