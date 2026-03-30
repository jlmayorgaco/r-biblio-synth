# ============================================================================
# m0_helpers.R - Public API for downstream modules to access M0 data
# ============================================================================

#' Get organized data from M0 result for a specific module
#'
#' Downstream modules (M1-M4) can use this to fetch pre-processed data
#' instead of re-parsing raw bibliometrix data.
#'
#' @param m0_result The result object from \code{run_m0()}.
#' @param slot Character. One of: "authors", "countries", "sources",
#'   "keywords", "citations", "annual", "collaborations", "doc_types", "bib_raw", "bib_merged".
#' @return The requested data frame, or NULL if not found.
#' @export
m0_get <- function(m0_result, slot = "bib_merged") {
  if (!inherits(m0_result, "biblio_module_result") || m0_result$module_id != "m0") {
    cli::cli_warn("m0_get expects a result from run_m0()")
    return(NULL)
  }

  if (slot == "bib_merged") return(m0_result$data$bib_merged)
  if (slot == "bib_raw")    return(m0_result$data$bib_raw)

  org <- m0_result$data$organized
  if (slot %in% names(org)) return(org[[slot]])

  cli::cli_warn("Unknown slot: {slot}. Available: {paste(names(org), collapse=', ')}")
  NULL
}

#' Check if M0 result is valid and has data
#'
#' @param m0_result The result object from \code{run_m0()}.
#' @return Logical.
#' @export
m0_is_valid <- function(m0_result) {
  inherits(m0_result, "biblio_module_result") &&
    m0_result$module_id == "m0" &&
    m0_result$status == "success" &&
    !is.null(m0_result$data$bib_merged) &&
    nrow(m0_result$data$bib_merged) > 0
}

#' Get the raw bibliometrix data frame from M0
#'
#' Equivalent to \code{m0_get(m0_result, "bib_merged")} but with validation.
#'
#' @param m0_result The result object from \code{run_m0()}.
#' @return A data frame or NULL.
#' @export
m0_get_bib_data <- function(m0_result) {
  if (!m0_is_valid(m0_result)) {
    cli::cli_warn("M0 result is not valid or has no data")
    return(NULL)
  }
  m0_result$data$bib_merged
}

#' Get pre-organized country data from M0
#'
#' @param m0_result The result object from \code{run_m0()}.
#' @return A data frame with country-level aggregation.
#' @export
m0_get_countries <- function(m0_result) {
  m0_get(m0_result, "countries")
}

#' Get pre-organized author data from M0
#'
#' @param m0_result The result object from \code{run_m0()}.
#' @return A data frame with author-level aggregation.
#' @export
m0_get_authors <- function(m0_result) {
  m0_get(m0_result, "authors")
}

#' Get pre-organized source/journal data from M0
#'
#' @param m0_result The result object from \code{run_m0()}.
#' @return A data frame with source-level aggregation.
#' @export
m0_get_sources <- function(m0_result) {
  m0_get(m0_result, "sources")
}

#' Get PRISMA data from M0 result
#'
#' @param m0_result The result object from \code{run_m0()}.
#' @return PRISMA data list or NULL.
#' @export
m0_get_prisma <- function(m0_result) {
  if (!inherits(m0_result, "biblio_module_result")) return(NULL)
  m0_result$data$prisma
}

#' Create a default PRISMA spec template
#'
#' Generates a list structure that can be filled with counts and saved as JSON/YAML.
#'
#' @param title Optional review title.
#' @return A list with PRISMA structure.
#' @export
m0_prisma_template <- function(title = NULL) {
  list(
    title = title,
    identification = list(
      records_database  = 0,
      records_other     = 0,
      duplicates_removed = 0
    ),
    screening = list(
      records_screened  = 0,
      excluded_screening = 0
    ),
    eligibility = list(
      fulltext_assessed = 0,
      excluded_fulltext = 0,
      excluded_reasons = list(
        "Not relevant" = 0,
        "Duplicate"    = 0,
        "No full text" = 0,
        "Low quality"  = 0
      )
    ),
    included = list(
      studies_included = 0,
      by_type = list()
    ),
    quality = list(
      tool       = NULL,
      low_risk   = 0,
      high_risk  = 0,
      unclear    = 0
    )
  )
}
