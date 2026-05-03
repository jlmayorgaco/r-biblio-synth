# ============================================================================
# m0_prisma_report.R - PRISMA 2020 report generation from JSON/YAML spec
# ============================================================================

#' Read PRISMA specification from JSON or YAML file
#'
#' @param spec Either a file path (JSON or YAML) or a list with PRISMA data.
#' @return A list with PRISMA flow counts.
#' @export
m0_read_prisma_spec <- function(spec) {
  if (is.character(spec) && file.exists(spec)) {
    ext <- tolower(tools::file_ext(spec))
    if (ext == "json") {
      tryCatch({
        jsonlite::fromJSON(spec, simplifyVector = FALSE)
      }, error = function(e) {
        cli::cli_abort("Failed to parse JSON file '{spec}': {e$message}")
      })
    } else if (ext %in% c("yml", "yaml")) {
      if (!requireNamespace("yaml", quietly = TRUE)) {
        cli::cli_abort("Package 'yaml' required to read YAML PRISMA specs. Install with: install.packages('yaml')")
      }
      tryCatch({
        yaml::read_yaml(spec)
      }, error = function(e) {
        cli::cli_abort("Failed to parse YAML file '{spec}': {e$message}")
      })
    } else {
      cli::cli_abort("Unsupported PRISMA spec format: {ext}. Use .json or .yml/.yaml")
    }
  } else if (is.list(spec)) {
    spec
  } else {
    cli::cli_abort("prisma_spec must be a file path (JSON or YAML) or a list with PRISMA data")
  }
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
  excluded_reasons <- m0_filter_named_counts(el$excluded_reasons)
  if (length(excluded_reasons) > 0) {
    lines <- c(lines, "  Exclusion reasons:")
    for (reason in names(excluded_reasons)) {
      lines <- c(lines, paste0("    - ", reason, ": ", excluded_reasons[[reason]]))
    }
  }
  lines <- c(lines, paste("  Total excluded at full-text:", el$excluded_fulltext %||% 0))
  lines <- c(lines, "")

  # --- INCLUDED ---
  lines <- c(lines, "--- INCLUDED ---")
  inc <- prisma_data$included %||% list()
  lines <- c(lines, paste("  Studies included in review:", inc$studies_included %||% 0))
  if (!is.null(inc$by_type)) {
    by_type <- m0_filter_named_counts(inc$by_type)
    if (length(by_type) > 0) {
      lines <- c(lines, "  By type:")
      for (tp in names(by_type)) {
        lines <- c(lines, paste0("    - ", tp, ": ", by_type[[tp]]))
      }
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

#' Build a complete PRISMA bundle from M0 runtime data and optional user spec
#' @keywords internal
m0_build_prisma_bundle <- function(raw_list,
                                   merged,
                                   sources,
                                   prisma_spec = NULL,
                                   screening_summary = NULL,
                                   search_metadata = NULL,
                                   config = biblio_config()) {
  user_spec <- if (!is.null(prisma_spec) && !identical(prisma_spec, "auto")) prisma_spec else NULL
  source_summary <- m0_build_source_summary(raw_list, sources)
  dedup_summary <- m0_build_dedup_summary(raw_list, merged, config)
  prisma_data <- m0_autobuild_prisma_spec(raw_list, merged, sources, user_spec, screening_summary, config)
  if (!is.null(search_metadata)) {
    prisma_data$methods <- m0_recursive_fill(prisma_data$methods %||% list(), list(search_metadata = search_metadata))
  }
  validation <- m0_validate_prisma_flow(prisma_data)
  report <- m0_build_prisma_report(prisma_data, config)
  methodology <- m0_build_prisma_methodology(
    prisma_data,
    source_summary,
    dedup_summary,
    screening_summary = screening_summary,
    search_metadata = search_metadata,
    config = config
  )
  diagram <- m0_render_prisma_diagram(prisma_data, config)

  list(
    spec = prisma_data,
    validation = validation,
    report = report,
    methodology = methodology,
    diagram = diagram
  )
}

#' Auto-build and complete a PRISMA specification
#' @keywords internal
m0_autobuild_prisma_spec <- function(raw_list,
                                     merged,
                                     sources,
                                     user_spec = NULL,
                                     screening_summary = NULL,
                                     config = biblio_config()) {
  auto_flags <- list(
    identification = is.null(user_spec$identification),
    screening = is.null(user_spec$screening) || is.null(user_spec$screening$records_screened),
    eligibility = is.null(user_spec$eligibility) || is.null(user_spec$eligibility$fulltext_assessed),
    included = is.null(user_spec$included) || is.null(user_spec$included$studies_included)
  )

  total_by_role <- m0_build_source_summary(raw_list, sources)
  total_database <- sum(total_by_role$n_records[total_by_role$prisma_role == "database"], na.rm = TRUE)
  total_other <- sum(total_by_role$n_records[total_by_role$prisma_role == "other"], na.rm = TRUE)
  total_raw <- sum(total_by_role$n_records, na.rm = TRUE)
  total_unique <- if (is.data.frame(merged)) nrow(merged) else 0L
  duplicates_removed <- max(total_raw - total_unique, 0L)

  auto <- list(
    title = user_spec$title %||% NULL,
    identification = list(
      records_database = total_database,
      records_other = total_other,
      duplicates_removed = duplicates_removed
    ),
    screening = list(
      records_screened = total_unique,
      excluded_screening = 0
    ),
    eligibility = list(
      fulltext_assessed = total_unique,
      excluded_fulltext = 0,
      excluded_reasons = list()
    ),
    included = list(
      studies_included = total_unique,
      by_type = m0_build_prisma_type_counts(merged)
    ),
    quality = list(
      tool = NULL,
      low_risk = 0,
      high_risk = 0,
      unclear = 0
    ),
    methods = list(
      auto_generated = TRUE,
      deduplication = paste(config$dedup_method %||% character(), collapse = "; "),
      source_registry = split(total_by_role, seq_len(nrow(total_by_role))),
      notes = character()
    )
  )

  prisma_data <- m0_recursive_fill(auto, user_spec %||% list())
  prisma_data <- m0_apply_screening_to_prisma(prisma_data, screening_summary)

  # Fill downstream counts from any user-provided exclusions.
  if (isTRUE(auto_flags$screening)) {
    prisma_data$screening$records_screened <- total_unique
  }
  records_screened <- prisma_data$screening$records_screened %||% total_unique
  excluded_screening <- prisma_data$screening$excluded_screening %||% 0
  if (isTRUE(auto_flags$eligibility)) {
    prisma_data$eligibility$fulltext_assessed <- max(records_screened - excluded_screening, 0)
  }

  fulltext_assessed <- prisma_data$eligibility$fulltext_assessed %||% 0
  excluded_fulltext <- prisma_data$eligibility$excluded_fulltext %||% 0
  if (isTRUE(auto_flags$included)) {
    prisma_data$included$studies_included <- max(fulltext_assessed - excluded_fulltext, 0)
  }

  if (is.null(prisma_data$included$by_type) || length(prisma_data$included$by_type) == 0) {
    prisma_data$included$by_type <- m0_build_prisma_type_counts(merged)
  }

  runtime_counts <- list(
    records_database = total_database,
    records_other = total_other,
    duplicates_removed = duplicates_removed,
    records_after_dedup = total_unique
  )
  prisma_data$methods$runtime_counts <- runtime_counts

  consistency_warnings <- character()
  if (!is.null(user_spec$identification)) {
    user_id <- user_spec$identification
    if (!is.null(user_id$records_database) && !identical(as.integer(user_id$records_database), as.integer(total_database))) {
      consistency_warnings <- c(
        consistency_warnings,
        sprintf("User PRISMA records_database (%s) differs from runtime import count (%s).", user_id$records_database, total_database)
      )
    }
    if (!is.null(user_id$records_other) && !identical(as.integer(user_id$records_other), as.integer(total_other))) {
      consistency_warnings <- c(
        consistency_warnings,
        sprintf("User PRISMA records_other (%s) differs from runtime import count (%s).", user_id$records_other, total_other)
      )
    }
    if (!is.null(user_id$duplicates_removed) && !identical(as.integer(user_id$duplicates_removed), as.integer(duplicates_removed))) {
      consistency_warnings <- c(
        consistency_warnings,
        sprintf("User PRISMA duplicates_removed (%s) differs from runtime dedup count (%s).", user_id$duplicates_removed, duplicates_removed)
      )
    }
  }
  prisma_data$methods$consistency_warnings <- unique(consistency_warnings)

  prisma_data$methods$auto_derived_stages <- auto_flags
  prisma_data$methods$auto_generated <- any(unlist(auto_flags))

  prisma_data
}

#' Recursively fill a list with user overrides
#' @keywords internal
m0_recursive_fill <- function(base, override) {
  if (!is.list(base)) {
    return(override %||% base)
  }

  if (!is.list(override) || length(override) == 0) {
    return(base)
  }

  out <- base
  for (nm in names(override)) {
    if (is.list(base[[nm]]) && is.list(override[[nm]])) {
      out[[nm]] <- m0_recursive_fill(base[[nm]], override[[nm]])
    } else if (!is.null(override[[nm]])) {
      out[[nm]] <- override[[nm]]
    }
  }

  out
}

#' Build PRISMA document-type counts
#' @keywords internal
m0_build_prisma_type_counts <- function(merged) {
  if (!is.data.frame(merged) || !"DT" %in% names(merged) || nrow(merged) == 0) {
    return(list())
  }

  values <- trimws(as.character(stats::na.omit(merged$DT)))
  values <- values[nzchar(values)]
  if (length(values) == 0) {
    return(list())
  }

  counts <- sort(table(values), decreasing = TRUE)
  stats::setNames(as.list(as.integer(counts)), names(counts))
}

#' Validate PRISMA flow consistency
#' @keywords internal
m0_validate_prisma_flow <- function(prisma_data) {
  id <- prisma_data$identification %||% list()
  sc <- prisma_data$screening %||% list()
  el <- prisma_data$eligibility %||% list()
  inc <- prisma_data$included %||% list()

  warnings <- character()
  checks <- list()

  total_identified <- (id$records_database %||% 0) + (id$records_other %||% 0)
  after_dedup <- total_identified - (id$duplicates_removed %||% 0)
  if (after_dedup < 0) {
    warnings <- c(warnings, "Duplicates removed exceed total identified records.")
  }

  records_screened <- sc$records_screened %||% 0
  fulltext_assessed <- el$fulltext_assessed %||% 0
  studies_included <- inc$studies_included %||% 0

  if (records_screened > max(after_dedup, 0)) {
    warnings <- c(warnings, "Records screened exceed records remaining after deduplication.")
  }
  if (fulltext_assessed > records_screened) {
    warnings <- c(warnings, "Full-text assessed exceeds records screened.")
  }
  if (studies_included > fulltext_assessed) {
    warnings <- c(warnings, "Included studies exceed full-text assessed.")
  }

  checks$total_identified <- total_identified
  checks$after_dedup <- after_dedup
  checks$records_screened <- records_screened
  checks$fulltext_assessed <- fulltext_assessed
  checks$studies_included <- studies_included

  list(
    ok = length(warnings) == 0,
    warnings = warnings,
    checks = checks
  )
}

#' Build PRISMA methodology text and LaTeX
#' @keywords internal
m0_build_prisma_methodology <- function(prisma_data,
                                        source_summary,
                                        dedup_summary,
                                        screening_summary = NULL,
                                        search_metadata = NULL,
                                        config = biblio_config()) {
  source_lines <- if (is.data.frame(source_summary) && nrow(source_summary) > 0) {
    apply(source_summary, 1, function(row) {
      paste0(
        "  - ", row[["source_name"]], " [", row[["db"]], ", ", row[["prisma_role"]], ", ", row[["mode"]] %||% "file", "]: ",
        row[["n_records"]], " records",
        if (!is.na(row[["search_date"]]) && nzchar(row[["search_date"]])) paste0(", search date ", row[["search_date"]]) else "",
        if (!is.na(row[["query"]]) && nzchar(row[["query"]])) paste0(", query: ", row[["query"]]) else "",
        if (!is.na(row[["endpoint"]]) && nzchar(row[["endpoint"]])) paste0(", endpoint: ", row[["endpoint"]]) else ""
      )
    })
  } else {
    character()
  }

  dd <- if (is.data.frame(dedup_summary) && nrow(dedup_summary) > 0) dedup_summary[1, , drop = FALSE] else data.frame()
  id <- prisma_data$identification %||% list()
  sc <- prisma_data$screening %||% list()
  el <- prisma_data$eligibility %||% list()
  inc <- prisma_data$included %||% list()
  methods <- prisma_data$methods %||% list()
  search_metadata <- search_metadata %||% methods$search_metadata %||% list()
  search_queries <- search_metadata$queries %||% list()
  primary_query <- search_queries$primary %||% NULL
  query_map <- search_queries$by_database %||% list()
  secondary_databases <- paste(search_metadata$secondary_databases %||% character(), collapse = ", ")
  doc_types <- paste(search_metadata$document_types %||% character(), collapse = ", ")
  reviewers <- paste(search_metadata$reviewers %||% character(), collapse = ", ")

  lines <- c(
    "============================================================",
    "PRISMA 2020 Methodology Narrative",
    paste("Generated:", Sys.time()),
    "============================================================",
    ""
  )

  if (!is.null(prisma_data$title)) {
    lines <- c(lines, paste("Review:", prisma_data$title), "")
  }

  if (length(search_metadata) > 0) {
    lines <- c(
      lines,
      "Review Perimeter",
      paste0("  Topic: ", search_metadata$review_topic %||% "NA"),
      paste0("  Research question: ", search_metadata$research_question %||% "NA"),
      if (!bslr_is_empty_field(search_metadata$research_gap %||% NULL)) paste0("  Research gap: ", search_metadata$research_gap) else NULL,
      paste0("  Inclusion criteria: ", paste(search_metadata$inclusion_criteria %||% character(), collapse = " | ")),
      paste0("  Exclusion criteria: ", paste(search_metadata$exclusion_criteria %||% character(), collapse = " | ")),
      ""
    )
  }

  lines <- c(
    lines,
    "Data Sources",
    if (length(source_lines) > 0) source_lines else "  - No source registry available.",
    "",
    "Workflow",
    paste0(
      "  Records were identified from database sources (n = ", id$records_database %||% 0,
      ") and other sources (n = ", id$records_other %||% 0, ")."
    ),
    paste0(
      "  Deduplication removed ", id$duplicates_removed %||% 0,
      " records using: ", methods$deduplication %||% "unspecified", "."
    ),
    if (nrow(dd) > 0) paste0(
      "  Raw records: ", dd$total_raw_records,
      "; unique records after deduplication: ", dd$unique_records,
      "; multi-source records retained: ", dd$multi_source_records, "."
    ) else "  Deduplication summary unavailable.",
    paste0(
      "  Screening considered ", sc$records_screened %||% 0,
      " records, with ", sc$excluded_screening %||% 0,
      " exclusions at title/abstract level."
    ),
    paste0(
      "  Eligibility review assessed ", el$fulltext_assessed %||% 0,
      " full-text records and excluded ", el$excluded_fulltext %||% 0, "."
    ),
    paste0(
      "  The final synthesis included ", inc$studies_included %||% 0, " studies."
    ),
    ""
  )

  if (length(search_metadata) > 0) {
    lines <- c(
      lines,
      "Search Metadata",
      paste0("  Primary database: ", search_metadata$primary_database %||% "NA"),
      paste0("  Secondary databases: ", if (nzchar(secondary_databases)) secondary_databases else "NA"),
      paste0("  Search date: ", search_metadata$first_run_date %||% "NA"),
      paste0("  Cross-check date: ", search_metadata$crosscheck_date %||% "NA"),
      paste0("  Time span: ", search_metadata$time_span$start %||% "NA", " to ", search_metadata$time_span$end %||% "NA"),
      paste0("  Language restriction: ", search_metadata$language %||% "NA"),
      paste0("  Document types: ", if (nzchar(doc_types)) doc_types else "NA"),
      paste0("  Primary query: ", primary_query %||% "NA"),
      if (length(query_map) > 0) paste0("  Database-specific queries: ", paste(sprintf("%s=%s", names(query_map), unlist(query_map)), collapse = " | ")) else NULL,
      ""
    )
  }

  if (!is.null(el$excluded_reasons) && length(el$excluded_reasons) > 0) {
    lines <- c(lines, "Exclusion Reasons")
    for (reason in names(el$excluded_reasons)) {
      lines <- c(lines, paste0("  - ", reason, ": ", el$excluded_reasons[[reason]]))
    }
    lines <- c(lines, "")
  }

  if (!is.null(prisma_data$quality) && !is.null(prisma_data$quality$tool) && nzchar(prisma_data$quality$tool %||% "")) {
    qa <- prisma_data$quality
    lines <- c(
      lines,
      "Quality Assessment",
      paste0("  Tool: ", qa$tool),
      paste0("  Low risk: ", qa$low_risk %||% 0, "; High risk: ", qa$high_risk %||% 0, "; Unclear: ", qa$unclear %||% 0),
      ""
    )
  }

  if (isTRUE(methods$auto_generated)) {
    auto_flags <- methods$auto_derived_stages %||% list()
    auto_stages <- names(auto_flags)[unlist(auto_flags)]
    if (length(auto_stages) == 0) auto_stages <- "none"
    lines <- c(
      lines,
      paste0(
        "Note: the following PRISMA stages were auto-derived from M0 runtime counts: ",
        paste(auto_stages, collapse = ", "), "."
      ),
      ""
    )
  }

  runtime_counts <- methods$runtime_counts %||% list()
  if (length(runtime_counts) > 0) {
    lines <- c(
      lines,
      "Runtime Import Counts",
      paste0("  Database records loaded by M0: ", runtime_counts$records_database %||% 0, "."),
      paste0("  Other-source records loaded by M0: ", runtime_counts$records_other %||% 0, "."),
      paste0("  Unique records after runtime deduplication: ", runtime_counts$records_after_dedup %||% 0, "."),
      ""
    )
  }

  consistency_warnings <- methods$consistency_warnings %||% character()
  if (length(consistency_warnings) > 0) {
    lines <- c(lines, "Consistency Notes", paste0("  - ", consistency_warnings), "")
  }

  if (isTRUE(screening_summary$ok)) {
    reliability <- screening_summary$reliability %||% list()
    lines <- c(
      lines,
      "Screening Ledger",
      paste0("  Screening decisions recorded: ", screening_summary$n_decisions %||% 0, "."),
      paste0("  Documents with explicit screening decisions: ", screening_summary$n_documents %||% 0, "."),
      paste0("  Reviewers: ", if (nzchar(reviewers)) reviewers else "NA"),
      paste0("  Conflict rule: ", search_metadata$conflict_rule %||% "NA"),
      paste0("  Screening tool: ", search_metadata$screening_tool %||% "NA"),
      paste0("  Quality tool: ", search_metadata$quality_tool %||% "NA")
    )
    if (identical(reliability$status %||% "", "success")) {
      lines <- c(
        lines,
        paste0("  Reviewer agreement (mean Cohen's kappa): ", round(reliability$cohens_kappa_mean %||% NA_real_, 3), "."),
        paste0("  Fleiss' kappa: ", round(reliability$fleiss_kappa %||% NA_real_, 3), "."),
        paste0("  Percent agreement: ", round(100 * (reliability$percent_agreement %||% NA_real_), 1), "%.")
      )
    }
    lines <- c(lines, "")
  }

  tex <- c(
    "\\section*{PRISMA 2020 Methodology}",
    paste0(
      "Records were identified from database sources (n = ", id$records_database %||% 0,
      ") and other sources (n = ", id$records_other %||% 0, "). "
    ),
    paste0(
      "Deduplication removed ", id$duplicates_removed %||% 0,
      " records using ", m0_escape_latex(methods$deduplication %||% "unspecified"), "."
    ),
    paste0(
      "Screening considered ", sc$records_screened %||% 0, " records; ",
      sc$excluded_screening %||% 0, " were excluded at title/abstract level. "
    ),
    paste0(
      "Eligibility assessment reviewed ", el$fulltext_assessed %||% 0,
      " full-text records and excluded ", el$excluded_fulltext %||% 0, ". "
    ),
    paste0(
      "The final synthesis included ", inc$studies_included %||% 0, " studies."
    )
  )

  if (length(search_metadata) > 0) {
    tex <- c(
      tex,
      paste0(
        " The primary database was ", m0_escape_latex(search_metadata$primary_database %||% "NA"),
        " and the main search string was ", m0_escape_latex(primary_query %||% "NA"), "."
      ),
      paste0(
        " The search covered ", m0_escape_latex(search_metadata$time_span$start %||% "NA"),
        " to ", m0_escape_latex(search_metadata$time_span$end %||% "NA"),
        ", restricted to ", m0_escape_latex(search_metadata$language %||% "NA"),
        " and document types ", m0_escape_latex(doc_types), "."
      )
    )
  }

  if (isTRUE(screening_summary$ok)) {
    reliability <- screening_summary$reliability %||% list()
    tex <- c(
      tex,
      paste0(
        " Explicit screening decisions were recorded for ",
        screening_summary$n_documents %||% 0,
        " documents."
      ),
      if (identical(reliability$status %||% "", "success")) {
        paste0(
          " Reviewer agreement yielded a mean Cohen's $\\kappa$ of ",
          round(reliability$cohens_kappa_mean %||% NA_real_, 3),
          " and Fleiss' $\\kappa$ of ",
          round(reliability$fleiss_kappa %||% NA_real_, 3),
          "."
        )
      } else {
        NULL
      }
    )
  }

  list(lines = lines, tex = tex)
}

#' Filter named count vectors/lists to non-empty entries
#' @keywords internal
m0_filter_named_counts <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(list())
  }

  nm <- names(x)
  if (is.null(nm)) {
    return(list())
  }

  keep <- !is.na(nm) & nzchar(trimws(nm))
  x[keep]
}

#' Escape special characters for LaTeX
#' @param x Character string to escape
#' @return Escaped string
#' @keywords internal
m0_escape_latex <- function(x) {
  if (is.null(x) || !is.character(x)) return(as.character(x %||% ""))
  x <- gsub("\\\\", "\\\\\\\\", x)  # Backslash (must be first)
  x <- gsub("&", "\\\\&", x)         # Ampersand
  x <- gsub("%", "\\\\%", x)         # Percent
  x <- gsub("\\$", "\\\\\\$", x)     # Dollar
  x <- gsub("#", "\\\\#", x)         # Hash
  x <- gsub("_", "\\\\_", x)         # Underscore
  x <- gsub("\\{", "\\\\{", x)        # Left brace
  x <- gsub("\\}", "\\\\}", x)        # Right brace
  x <- gsub("~", "\\\\textasciitilde{}", x)  # Tilde
  x <- gsub("\\^", "\\\\textasciicircum{}", x)  # Caret
  x
}

#' Generate LaTeX PRISMA table snippet
#' @keywords internal
m0_prisma_to_tex <- function(prisma_data, id_total) {
  id <- prisma_data$identification %||% list()
  sc <- prisma_data$screening %||% list()
  el <- prisma_data$eligibility %||% list()
  inc <- prisma_data$included %||% list()
  qa <- prisma_data$quality %||% NULL

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

  # Add quality assessment section if provided
  if (!is.null(qa)) {
    qa_lines <- c(
      "\\begin{table}[htbp]",
      "\\centering",
      "\\caption{Quality Assessment Summary}",
      "\\begin{tabular}{lr}",
      "\\hline",
      "Category & Count \\\\",
      "\\hline",
      paste0("Low risk & ", qa$low_risk %||% 0, " \\\\"),
      paste0("High risk & ", qa$high_risk %||% 0, " \\\\"),
      paste0("Unclear & ", qa$unclear %||% 0, " \\\\"),
      "\\hline",
      "\\end{tabular}",
      "\\end{table}"
    )
    tex <- c(tex, "", qa_lines)
  }

  tex
}
