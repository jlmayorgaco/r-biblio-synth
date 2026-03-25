# ============================================================================
# m1_manifest.R - Artifact manifest for M1
# ============================================================================

#' Build M1 artifact manifest
#'
#' Creates a standardized artifact manifest from an M1 result.
#'
#' @param result A \code{biblio_module_result} object.
#' @param exported A list of exported file paths (plots, tables, files, reports).
#' @param config A configuration list.
#' @return A \code{biblio_artifact_manifest} object.
#' @export
build_m1_manifest <- function(result, exported = list(), config = biblio_config()) {
  status <- if (inherits(result, "biblio_module_result")) result$status else "stub"

  new_artifact_manifest(
    module_id    = "m1",
    generated_at = Sys.time(),
    files        = if (is.null(exported$files)) character() else exported$files,
    plots        = if (is.null(exported$plots)) character() else exported$plots,
    tables       = if (is.null(exported$tables)) character() else exported$tables,
    reports      = if (is.null(exported$reports)) character() else exported$reports,
    status       = status
  )
}
