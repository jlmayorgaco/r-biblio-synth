# ============================================================================
# m0_prisma_report.R - PRISMA 2020 report generation from JSON/YAML spec
# ============================================================================

# Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Read PRISMA specification from JSON or YAML file
#'
#' @param spec Either a file path (JSON or YAML) or a list with PRISMA data.
#' @return A list with PRISMA flow counts.
#' @export
m0_read_prisma_spec <- function(spec) {
  if (is.character(spec) && file.exists(spec)) {
    ext <- tolower(tools::file_ext(spec))
    if (ext == "json") {
      return(jsonlite::fromJSON(spec, simplifyVector = FALSE))
    } else if (ext %in% c("yml", "yaml")) {
      if (!requireNamespace("yaml", quietly = TRUE)) {
        cli::cli_abort("Package 'yaml' required to read YAML PRISMA specs")
      }
      return(yaml::read_yaml(spec))
    } else {
      cli::cli_abort("Unsupported PRISMA spec format: {ext}. Use .json or .yml/.yaml")
    }
  }
  if (is.list(spec)) return(spec)
  cli::cli_abort("prisma_spec must be a file path or a list")
}

#' Build PRISMA report text from specification
#'
#' Generates a structured text report following PRISMA 2020 guidelines.
#'
#' @param prisma_data A list with PRISMA flow counts.
#' @param config Configuration list.
#' @return A list with \code{lines} (character vector for text report) and
#'   \code{tex} (LaTeX snippet if applicable).
#' @export
m0_build_prisma_report <- function(prisma_data, config = biblio_config()) {
  config <- merge_biblio_config(config)

  lines <- character()

  # Header
  lines <- c(lines, "============================================================")
  lines <- c(lines, "PRISMA 2020 Flow Diagram - Systematic Review Report")
  lines <- c(lines, paste("Generated:", Sys.time()))
  lines <- c(lines, "============================================================")
  lines <- c(lines, "")

  # Study title/identification if provided
  if (!is.null(prisma_data$title)) {
    lines <- c(lines, paste("Review:", prisma_data$title))
    lines <- c(lines, "")
  }

  # --- IDENTIFICATION ---
  lines <- c(lines, "--- IDENTIFICATION ---")
  id <- prisma_data$identification %||% list()
  if (!is.null(id$records_database)) {
    lines <- c(lines, paste("  Records from databases:", id$records_database))
  }
  if (!is.null(id$records_other)) {
    lines <- c(lines, paste("  Records from other sources:", id$records_other))
  }
  id_total <- (id$records_database %||% 0) + (id$records_other %||% 0)
  lines <- c(lines, paste("  Total records identified:", id_total))
  lines <- c(lines, paste("  Duplicate records removed:", id$duplicates_removed %||% 0))
  lines <- c(lines, paste("  Records after dedup:", id_total - (id$duplicates_removed %||% 0)))
  lines <- c(lines, "")

  # --- SCREENING ---
  lines <- c(lines, "--- SCREENING ---")
  sc <- prisma_data$screening %||% list()
  lines <- c(lines, paste("  Records screened (titles/abstracts):", sc$records_screened %||% 0))
  lines <- c(lines, paste("  Records excluded at title/abstract:", sc$excluded_screening %||% 0))
  lines <- c(lines, "")

  # --- ELIGIBILITY ---
  lines <- c(lines, "--- ELIGIBILITY ---")
  el <- prisma_data$eligibility %||% list()
  lines <- c(lines, paste("  Full-text articles assessed:", el$fulltext_assessed %||% 0))
  if (!is.null(el$excluded_reasons)) {
    lines <- c(lines, "  Exclusion reasons:")
    for (reason in names(el$excluded_reasons)) {
      lines <- c(lines, paste0("    - ", reason, ": ", el$excluded_reasons[[reason]]))
    }
  }
  lines <- c(lines, paste("  Total excluded at full-text:", el$excluded_fulltext %||% 0))
  lines <- c(lines, "")

  # --- INCLUDED ---
  lines <- c(lines, "--- INCLUDED ---")
  inc <- prisma_data$included %||% list()
  lines <- c(lines, paste("  Studies included in review:", inc$studies_included %||% 0))
  if (!is.null(inc$by_type)) {
    lines <- c(lines, "  By type:")
    for (tp in names(inc$by_type)) {
      lines <- c(lines, paste0("    - ", tp, ": ", inc$by_type[[tp]]))
    }
  }
  lines <- c(lines, "")

  # --- QUALITY ASSESSMENT (optional) ---
  if (!is.null(prisma_data$quality)) {
    qa <- prisma_data$quality
    lines <- c(lines, "--- QUALITY ASSESSMENT ---")
    if (!is.null(qa$tool)) lines <- c(lines, paste("  Tool:", qa$tool))
    if (!is.null(qa$low_risk)) lines <- c(lines, paste("  Low risk:", qa$low_risk))
    if (!is.null(qa$high_risk)) lines <- c(lines, paste("  High risk:", qa$high_risk))
    if (!is.null(qa$unclear)) lines <- c(lines, paste("  Unclear:", qa$unclear))
    lines <- c(lines, "")
  }

  lines <- c(lines, "============================================================")

  # Build LaTeX version
  tex <- m0_prisma_to_tex(prisma_data, id_total)

  list(lines = lines, tex = tex)
}

#' Generate LaTeX PRISMA table snippet
#' @keywords internal
m0_prisma_to_tex <- function(prisma_data, id_total) {
  id <- prisma_data$identification %||% list()
  sc <- prisma_data$screening %||% list()
  el <- prisma_data$eligibility %||% list()
  inc <- prisma_data$included %||% list()

  tex <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{PRISMA 2020 Flow Diagram Summary}",
    "\\begin{tabular}{llr}",
    "\\hline",
    "Stage & Item & Count \\\\",
    "\\hline",
    paste0("Identification & Records from databases & ", id$records_database %||% 0, " \\\\"),
    paste0("Identification & Records from other sources & ", id$records_other %||% 0, " \\\\"),
    paste0("Identification & Duplicates removed & ", id$duplicates_removed %||% 0, " \\\\"),
    paste0("Screening & Records screened & ", sc$records_screened %||% 0, " \\\\"),
    paste0("Screening & Excluded at screening & ", sc$excluded_screening %||% 0, " \\\\"),
    paste0("Eligibility & Full-text assessed & ", el$fulltext_assessed %||% 0, " \\\\"),
    paste0("Eligibility & Excluded at full-text & ", el$excluded_fulltext %||% 0, " \\\\"),
    paste0("Included & Studies in review & ", inc$studies_included %||% 0, " \\\\"),
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  )

  tex
}
