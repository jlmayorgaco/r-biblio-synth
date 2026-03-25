# ============================================================================
# m1_run.R - Orchestrator for M1
# ============================================================================

#' Run M1 module
#'
#' Orchestrates the full M1 pipeline:
#' validate -> compute -> render -> table -> report -> export -> manifest.
#'
#' @param input A data frame or tibble of bibliographic data.
#' @param config A configuration list from \code{\link{biblio_config}}.
#' @param export Logical. If TRUE, exports artifacts to disk.
#' @return A \code{biblio_module_result} object.
#' @export
run_m1 <- function(input, config = biblio_config(), export = TRUE) {
  config <- merge_biblio_config(config)

  # 1. Validate
  validation <- validate_m1_input(input, config)

  # 2. Compute all metrics
  data <- list(
    overview    = compute_m1_overview(input, config),
    doc_types   = compute_m1_doc_types(input, config),
    authors     = compute_m1_authors(input, config),
    citations   = compute_m1_citations(input, config),
    countries   = compute_m1_countries(input, config),
    sources     = compute_m1_sources(input, config),
    keywords    = compute_m1_keywords(input, config),
    bradford    = compute_m1_bradford(input, config)
  )

  # 3. Build initial result
  result <- new_module_result(
    module_id   = "m1",
    module_name = "Main Information",
    status      = if (validation$ok) "success" else "warning",
    inputs      = list(
      n_rows = validation$n_rows,
      n_cols = validation$n_cols
    ),
    data        = data,
    diagnostics = list(
      warnings = character(),
      checks   = list(validation = validation),
      notes    = character()
    )
  )

  # 4. Render (plots)
  plots <- list(
    overview    = render_m1_overview(data$overview, config),
    doc_types   = render_m1_doc_types(data$doc_types, config),
    authors     = render_m1_authors(data$authors, config),
    citations   = render_m1_citations(data$citations, config),
    countries   = render_m1_countries(data$countries, config),
    sources     = render_m1_sources(data$sources, config),
    keywords    = render_m1_keywords(data$keywords, config),
    bradford    = render_m1_bradford(data$bradford, config)
  )
  result$artifacts$plots <- plots

  # 5. Tables
  tables <- list(
    overview    = build_m1_overview_table(data$overview, config),
    doc_types   = build_m1_doc_types_table(data$doc_types, config),
    authors     = build_m1_authors_table(data$authors, config),
    citations   = build_m1_citations_table(data$citations, config),
    countries   = build_m1_countries_table(data$countries, config),
    sources     = build_m1_sources_table(data$sources, config),
    keywords    = build_m1_keywords_table(data$keywords, config),
    bradford    = build_m1_bradford_table(data$bradford, config)
  )
  result$artifacts$tables <- tables

  # 6. Report
  report <- build_m1_report(result, config)
  result <- attach_report_to_result(result, report)

  # 7. Export (if requested)
  if (export) {
    exported <- export_m1(result, config)
    manifest <- build_m1_manifest(result, exported, config)
    result <- attach_manifest_to_result(result, manifest)
  }

  result
}

#' Export M1 artifacts
#'
#' Exports plots, tables, and reports to disk.
#'
#' @param result A \code{biblio_module_result} object.
#' @param config A configuration list.
#' @return A list of exported file paths.
#' @export
export_m1 <- function(result, config = biblio_config()) {
  config <- merge_biblio_config(config)

  exported_plots   <- character()
  exported_tables  <- character()
  exported_reports <- character()
  exported_jsons   <- character()

  # Export plots
  if (config$export_plots) {
    for (nm in names(result$artifacts$plots)) {
      plot_section <- result$artifacts$plots[[nm]]
      for (pnm in names(plot_section$plots)) {
        p <- build_artifact_path("m1", "plots", paste0("m1_", nm, "_", pnm), "png", config)
        tryCatch({
          export_plot_artifact(plot_section$plots[[pnm]], tools::file_path_sans_ext(p),
                               width = 8, height = 6, dpi = config$dpi)
          exported_plots <- c(exported_plots, p)
        }, error = function(e) NULL)
      }
    }
  }

  # Export JSON
  if (config$export_json) {
    for (nm in names(result$data)) {
      j <- build_artifact_path("m1", "json", paste0("m1_", nm), "json", config)
      tryCatch({
        write_json_artifact(result$data[[nm]], j)
        exported_jsons <- c(exported_jsons, j)
      }, error = function(e) NULL)
    }
  }

  # Export reports
  if (config$export_reports && length(result$artifacts$reports) > 0) {
    report <- result$artifacts$reports[[1]]
    if (!is.null(report$lines) && length(report$lines) > 0) {
      r <- build_artifact_path("m1", "reports", "m1_report", "txt", config)
      write_text_report(report$lines, r)
      exported_reports <- c(exported_reports, r)
    }
    if (!is.null(report$tex) && length(report$tex) > 0) {
      t <- build_artifact_path("m1", "reports", "m1_doc_types_report", "tex", config)
      writeLines(report$tex, t)
      exported_reports <- c(exported_reports, t)
    }
  }

  list(
    plots   = exported_plots,
    tables  = exported_tables,
    reports = exported_reports,
    files   = exported_jsons
  )
}
