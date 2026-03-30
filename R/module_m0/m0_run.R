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
#' @param export Logical. If TRUE, exports artifacts.
#' @return A \code{biblio_module_result} with slots \code{data$bib_raw} (original
#'   bibliometrix objects per source), \code{data$bib_merged} (unified bibliometrix
#'   data frame), \code{data$organized} (pre-processed tables for M1-M4), and
#'   optionally PRISMA artifacts.
#' @export
run_m0 <- function(sources,
                   config     = biblio_config(),
                   prisma_spec = NULL,
                   export     = TRUE) {
  config <- merge_biblio_config(config)

  # 1. Validate inputs
  validation <- m0_validate_sources(sources)
  if (!validation$ok) {
    cli::cli_abort("M0 source validation failed: {validation$error}")
  }

  # 2. Load each source into a bibliometrix data frame
  raw_list <- m0_load_all_sources(sources, config)

  # 3. Merge into one unified data frame
  merged <- m0_merge_sources(raw_list, config)

  # 4. Organize auxiliary data frames for downstream modules
  organized <- m0_organize_for_modules(merged, config)

  # 5. Build the module result
  result <- m0_build_result(raw_list, merged, organized, validation)

  # 6. PRISMA report & diagram (optional)
  if (!is.null(prisma_spec)) {
    prisma_data <- m0_read_prisma_spec(prisma_spec)
    prisma_report <- m0_build_prisma_report(prisma_data, config)
    prisma_diagram <- m0_render_prisma_diagram(prisma_data, config)
    result$artifacts$reports <- c(result$artifacts$reports, list(prisma = prisma_report))
    result$artifacts$plots   <- c(result$artifacts$plots,   list(prisma = prisma_diagram))
    result$data$prisma <- prisma_data
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
  }

  # Export PRISMA report
  if (config$export_reports && "prisma" %in% names(result$artifacts$reports)) {
    prisma_lines <- result$artifacts$reports$prisma$lines
    if (!is.null(prisma_lines) && length(prisma_lines) > 0) {
      rp <- build_artifact_path("m0", "reports", "m0_prisma_report", "txt", config)
      write_text_report(prisma_lines, rp)
      exported_reports <- c(exported_reports, rp)
    }
  }

  # Export PRISMA diagram
  if (config$export_plots && "prisma" %in% names(result$artifacts$plots)) {
    prisma_plot <- result$artifacts$plots$prisma
    if (!is.null(prisma_plot)) {
      pp <- build_artifact_path("m0", "plots", "m0_prisma_diagram", "png", config)
      tryCatch({
        export_plot_artifact(prisma_plot, tools::file_path_sans_ext(pp),
                             width = 8, height = 10, dpi = config$dpi)
        exported_plots <- c(exported_plots, pp)
      }, error = function(e) cli::cli_warn("M0 PRISMA diagram export failed: {e$message}"))
    }
  }

  list(plots = exported_plots, tables = character(),
       reports = exported_reports, files = exported_files)
}
