# ============================================================================
# m0_run.R - M0 Data Orchestrator: Load, Merge, Organize, PRISMA
# ============================================================================

#' Run M0 Data Orchestrator
#'
#' Main entry point for M0. Loads bibliographic sources, merges them into a
#' unified dataset, organizes data for downstream modules (M1-M4), and
#' optionally generates PRISMA flow diagram and report.
#'
#' @param sources A named list of source specifications. Each element must be a
#'   list with \code{file} (path), \code{db} ("scopus", "wos", "openalex", "generic"),
#'   and optionally \code{format} ("bibtex", "plaintext", "csv").
#' @param config A configuration list (see \code{biblio_config}).
#' @param prisma_spec Path to a JSON or YAML file with PRISMA flow data, or a
#'   list with PRISMA counts. If NULL, PRISMA generation is skipped.
#' @param screening_ledger Optional screening decisions ledger used to derive
#'   PRISMA counts and reviewer-agreement summaries.
#' @param search_metadata Optional structured metadata describing the human
#'   review perimeter, search strings, reviewers, and quality appraisal logic.
#' @param enrich Logical or character vector. Enable optional API enrichment
#'   using configured providers.
#' @param export Logical. If TRUE, exports artifacts.
#' @return A \code{biblio_module_result} with slots \code{data$bib_raw} (original
#'   bibliometrix objects per source), \code{data$bib_merged} (unified bibliometrix
#'   data frame), \code{data$organized} (pre-processed tables for M1-M4), and
#'   optionally PRISMA artifacts.
#' @export
run_m0 <- function(sources,
                   config     = biblio_config(),
                   prisma_spec = NULL,
                   screening_ledger = NULL,
                   search_metadata = NULL,
                   enrich = FALSE,
                   export     = TRUE) {
  config <- merge_biblio_config(config)

  # 1. Validate inputs
  validation <- m0_validate_sources(sources)
  if (!validation$ok) {
    cli::cli_abort("M0 source validation failed: {validation$error}")
  }

  # 2. Load each source into a bibliometrix data frame
  raw_list <- m0_load_all_sources(sources, config)
  loaded_validation <- m0_validate_loaded(raw_list)
  if (!loaded_validation$ok) {
    cli::cli_abort("M0 loading failed: {loaded_validation$error}")
  }

  # 3. Merge into one unified data frame
  merged <- m0_merge_sources(raw_list, config)
  merged_before_enrichment <- merged
  merged_validation <- m0_validate_merged(merged)
  type_validation <- m0_validate_column_types(merged)

  enrichment <- list()
  enrichment_enabled <- isTRUE(config$enable_enrichment) || !identical(enrich, FALSE)
  if (enrichment_enabled) {
    enrichment_bundle <- m0_enrich_merged_records(
      merged,
      config,
      providers = if (isTRUE(enrich)) NULL else enrich
    )
    merged <- enrichment_bundle$merged
    enrichment <- enrichment_bundle$enrichment %||% list()
  }
  enrichment_audit <- m0_build_enrichment_audit(merged_before_enrichment, merged, enrichment)

  screening_df <- m0_read_screening_ledger(screening_ledger, merged)
  screening_summary <- m0_build_screening_summary(screening_df, merged)

  # 4. Organize auxiliary data frames for downstream modules
  organized <- m0_organize_for_modules(merged, config)
  organized$provenance <- m0_build_provenance_table(merged)
  if (nrow(screening_df) > 0) {
    organized$screening_ledger <- screening_df
  }
  if (isTRUE(screening_summary$ok) && is.data.frame(screening_summary$consensus)) {
    organized$screening_consensus <- screening_summary$consensus
  }
  if (length(enrichment) > 0) {
    for (nm in names(enrichment)) {
      if (is.data.frame(enrichment[[nm]]) || is.list(enrichment[[nm]])) {
        organized[[nm]] <- enrichment[[nm]]
      }
    }
  }

  source_summary <- m0_build_source_summary(raw_list, sources)
  dedup_summary <- m0_build_dedup_summary(raw_list, merged, config)
  quality <- m0_check_data_quality(merged)
  quality_report <- m0_create_quality_report(quality)

  # 5. Build the module result
  result <- m0_build_result(raw_list, merged, organized, validation)
  result$data$source_summary <- source_summary
  result$data$dedup_summary <- dedup_summary
  result$data$quality <- quality
  result$data$quality_report <- quality_report
  result$data$screening_ledger <- screening_df
  result$data$screening_summary <- screening_summary
  result$data$search_metadata <- search_metadata
  result$data$enrichment <- enrichment
  result$data$enrichment_audit <- enrichment_audit
  result$diagnostics$checks$loaded <- loaded_validation
  result$diagnostics$checks$merged <- merged_validation
  result$diagnostics$checks$column_types <- type_validation
  result$diagnostics$checks$quality <- quality
  if (enrichment_enabled) {
    result$diagnostics$notes <- c(result$diagnostics$notes, "API enrichment applied")
  }
  if (nrow(screening_df) > 0) {
    result$diagnostics$checks$screening <- screening_summary
  }
  if (!isTRUE(type_validation$ok)) {
    result$diagnostics$warnings <- c(
      result$diagnostics$warnings,
      paste("M0 column type issues:", paste(names(type_validation$issues), collapse = ", "))
    )
  }
  if (!isTRUE(merged_validation$ok)) {
    result$diagnostics$warnings <- c(
      result$diagnostics$warnings,
      paste("Merged data missing columns:", paste(merged_validation$missing, collapse = ", "))
    )
  }
  if (!is.null(loaded_validation$warning)) {
    result$diagnostics$warnings <- c(result$diagnostics$warnings, loaded_validation$warning)
  }
  if (isTRUE(screening_summary$ok)) {
    result$artifacts$reports <- c(
      result$artifacts$reports,
      list(screening = m0_build_screening_report(screening_summary))
    )
  }

  # 6. PRISMA report & diagram (optional)
  if (!is.null(prisma_spec)) {
    user_prisma <- if (!identical(prisma_spec, "auto")) m0_read_prisma_spec(prisma_spec) else NULL
    prisma_bundle <- m0_build_prisma_bundle(
      raw_list = raw_list,
      merged = merged,
      sources = sources,
      prisma_spec = user_prisma,
      screening_summary = screening_summary,
      search_metadata = search_metadata,
      config = config
    )
    result$artifacts$reports <- c(
      result$artifacts$reports,
      list(
        prisma = prisma_bundle$report,
        prisma_methodology = prisma_bundle$methodology
      )
    )
    result$artifacts$plots <- c(result$artifacts$plots, list(prisma = prisma_bundle$diagram))
    result$data$prisma <- prisma_bundle$spec
    result$data$prisma_validation <- prisma_bundle$validation
    result$diagnostics$checks$prisma <- prisma_bundle$validation
    if (!isTRUE(prisma_bundle$validation$ok)) {
      result$diagnostics$warnings <- c(result$diagnostics$warnings, prisma_bundle$validation$warnings)
    }
  }

  # 7. Export
  if (export) {
    exported <- m0_export(result, config)
    manifest <- m0_build_manifest(result, exported, config)
    result <- attach_manifest_to_result(result, manifest)
  }

  result
}

# ---------------------------------------------------------------------------
# Internal: Build result object
# ---------------------------------------------------------------------------
m0_build_result <- function(raw_list, merged, organized, validation) {
  src_names <- names(raw_list)
  n_total   <- if (is.data.frame(merged)) nrow(merged) else 0L

  new_module_result(
    module_id   = "m0",
    module_name = "Data Orchestrator",
    status      = if (validation$ok && n_total > 0) "success" else "warning",
    inputs      = list(
      n_sources  = length(raw_list),
      sources    = src_names,
      n_total    = n_total
    ),
    data = list(
      bib_raw     = raw_list,
      bib_merged  = merged,
      organized   = organized
    ),
    diagnostics = list(
      warnings = character(),
      checks   = list(validation = validation),
      notes    = character()
    )
  )
}

# ---------------------------------------------------------------------------
# Export
# ---------------------------------------------------------------------------
#' @export
m0_export <- function(result, config = biblio_config()) {
  config <- merge_biblio_config(config)

  exported_files   <- character()
  exported_reports <- character()
  exported_plots   <- character()

  # Export merged dataset as RDS
  if (config$export_json) {
    rds_path <- build_artifact_path("m0", "files", "bib_merged", "rds", config)
    tryCatch({
      saveRDS(result$data$bib_merged, rds_path)
      exported_files <- c(exported_files, rds_path)
    }, error = function(e) cli::cli_warn("M0 RDS export failed: {e$message}"))

    # Export organized data as JSON
    for (nm in names(result$data$organized)) {
      j <- build_artifact_path("m0", "json", paste0("m0_org_", nm), "json", config)
      tryCatch({
        write_json_artifact(result$data$organized[[nm]], j)
        exported_files <- c(exported_files, j)
      }, error = function(e) cli::cli_warn("M0 JSON export failed [{nm}]: {e$message}"))
    }

    optional_payloads <- list(
      source_summary = result$data$source_summary,
      dedup_summary = result$data$dedup_summary,
      quality = result$data$quality,
      prisma = result$data$prisma,
      prisma_validation = result$data$prisma_validation,
      screening_summary = result$data$screening_summary,
      search_metadata = result$data$search_metadata,
      enrichment_audit = result$data$enrichment_audit
    )

    for (nm in names(optional_payloads)) {
      payload <- optional_payloads[[nm]]
      if (!is.null(payload)) {
        j <- build_artifact_path("m0", "json", paste0("m0_", nm), "json", config)
        tryCatch({
          write_json_artifact(payload, j)
          exported_files <- c(exported_files, j)
        }, error = function(e) cli::cli_warn("M0 JSON export failed [{nm}]: {e$message}"))
      }
    }
  }

  if (config$export_reports && !is.null(result$data$quality_report$report_lines)) {
    qp <- build_artifact_path("m0", "reports", "m0_quality_report", "txt", config)
    write_text_report(result$data$quality_report$report_lines, qp)
    exported_reports <- c(exported_reports, qp)
  }

  if (config$export_reports && "screening" %in% names(result$artifacts$reports)) {
    screening_lines <- result$artifacts$reports$screening$lines
    if (!is.null(screening_lines) && length(screening_lines) > 0) {
      sp <- build_artifact_path("m0", "reports", "m0_screening_summary", "txt", config)
      write_text_report(screening_lines, sp)
      exported_reports <- c(exported_reports, sp)
    }
  }

  # Export PRISMA report
  if (config$export_reports && "prisma" %in% names(result$artifacts$reports)) {
    prisma_lines <- result$artifacts$reports$prisma$lines
    if (!is.null(prisma_lines) && length(prisma_lines) > 0) {
      rp <- build_artifact_path("m0", "reports", "m0_prisma_report", "txt", config)
      write_text_report(prisma_lines, rp)
      exported_reports <- c(exported_reports, rp)
    }

    prisma_tex <- result$artifacts$reports$prisma$tex
    if (!is.null(prisma_tex) && length(prisma_tex) > 0) {
      rp_tex <- build_artifact_path("m0", "reports", "m0_prisma_report", "tex", config)
      writeLines(prisma_tex, rp_tex)
      exported_reports <- c(exported_reports, rp_tex)
    }
  }

  if (config$export_reports && "prisma_methodology" %in% names(result$artifacts$reports)) {
    meth_lines <- result$artifacts$reports$prisma_methodology$lines
    if (!is.null(meth_lines) && length(meth_lines) > 0) {
      mp <- build_artifact_path("m0", "reports", "m0_prisma_methodology", "txt", config)
      write_text_report(meth_lines, mp)
      exported_reports <- c(exported_reports, mp)
    }

    meth_tex <- result$artifacts$reports$prisma_methodology$tex
    if (!is.null(meth_tex) && length(meth_tex) > 0) {
      mp_tex <- build_artifact_path("m0", "reports", "m0_prisma_methodology", "tex", config)
      writeLines(meth_tex, mp_tex)
      exported_reports <- c(exported_reports, mp_tex)
    }
  }

  # Export PRISMA diagram
  if (config$export_plots && "prisma" %in% names(result$artifacts$plots)) {
    prisma_plot <- result$artifacts$plots$prisma
    if (!is.null(prisma_plot)) {
      pp <- build_artifact_path("m0", "plots", "m0_prisma_diagram", "png", config)
      tryCatch({
        plot_paths <- export_plot_artifact(
          prisma_plot,
          tools::file_path_sans_ext(pp),
          width = 8,
          height = 10,
          dpi = config$dpi
        )
        exported_plots <- c(exported_plots, stats::na.omit(unname(plot_paths)))
      }, error = function(e) cli::cli_warn("M0 PRISMA diagram export failed: {e$message}"))
    }
  }

  list(plots = exported_plots, tables = character(),
       reports = exported_reports, files = exported_files)
}
