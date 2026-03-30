# ============================================================================
# module_registry.R - Module discovery and metadata
# ============================================================================

#' Get available module identifiers
#'
#' @return Character vector of registered module ids.
#' @export
get_available_modules <- function() {
  c("m0", "m1", "m2", "m3")
}

#' Get metadata for a specific module
#'
#' @param module_id Character. Module identifier.
#' @return A named list with module metadata, or NULL if not found.
#' @export
get_module_metadata <- function(module_id) {
  registry <- list(
    m0 = list(
      id   = "m0",
      name = "Data Orchestrator",
      description = "Load, merge, and organize bibliographic sources (Scopus, WoS, OpenAlex). Generate PRISMA flow diagram and report."
    ),
    m1 = list(
      id   = "m1",
      name = "Main Information",
      description = "Overview, document types, authors, citations, countries, sources, keywords, Bradford analysis."
    ),
    m2 = list(
      id   = "m2",
      name = "Annual Production",
      description = "EDA, regression models, harmonic analysis for annual publication trends."
    ),
    m3 = list(
      id   = "m3",
      name = "Countries",
      description = "Country-level production, citations, SCP/MCP, inequality, clustering, collaboration indices."
    )
  )
  registry[[module_id]]
}
