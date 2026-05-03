# ============================================================================
# result_builders.R - Helpers to assemble result objects
# ============================================================================

#' Attach artifacts to a module result
#'
#' @param result A \code{biblio_module_result} object.
#' @param artifacts A list with slots: plots, tables, files, reports.
#' @return The updated result object.
#' @export
attach_artifacts_to_result <- function(result, artifacts = list()) {
  if (!inherits(result, "biblio_module_result")) {
    return(result)
  }
  for (nm in names(artifacts)) {
    result$artifacts[[nm]] <- c(result$artifacts[[nm]], artifacts[[nm]])
  }
  result
}

#' Attach a report payload to a module result
#'
#' @param result A \code{biblio_module_result} object.
#' @param report_payload A list representing the report.
#' @return The updated result object.
#' @export
attach_report_to_result <- function(result, report_payload = list()) {
  if (!inherits(result, "biblio_module_result")) {
    return(result)
  }
  slot_name <- report_payload$report_id %||% report_payload$name %||%
    if (length(result$artifacts$reports) == 0) "report" else paste0("report_", length(result$artifacts$reports) + 1L)
  result$artifacts$reports[[slot_name]] <- report_payload
  result
}

#' Attach a manifest to a module result
#'
#' @param result A \code{biblio_module_result} object.
#' @param manifest A \code{biblio_artifact_manifest} object.
#' @return The updated result object.
#' @export
attach_manifest_to_result <- function(result, manifest) {
  if (!inherits(result, "biblio_module_result")) {
    return(result)
  }
  result$artifacts$manifest <- manifest
  result
}
