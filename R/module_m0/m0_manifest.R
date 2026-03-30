# ============================================================================
# m0_manifest.R - Artifact manifest for M0
# ============================================================================

#' Build M0 artifact manifest
#'
#' @param result Module result object.
#' @param exported List of exported file paths.
#' @param config Configuration list.
#' @return A \code{biblio_artifact_manifest} object.
#' @export
m0_build_manifest <- function(result, exported, config = biblio_config()) {
  config <- merge_biblio_config(config)

  new_artifact_manifest(
    module_id    = "m0",
    generated_at = Sys.time(),
    files        = exported$files,
    plots        = exported$plots,
    tables       = exported$tables,
    reports      = exported$reports,
    status       = result$status
  )
}
