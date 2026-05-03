# ============================================================================
# pipeline.R - End-to-end pipeline entrypoint for RBiblioSynth
# ============================================================================

#' Run the official RBiblioSynth pipeline
#'
#' Executes the public core modules (`M0`-`M4`) in a consistent order and
#' returns a single pipeline result object. The pipeline always runs `M0`
#' first to create the merged bibliometric dataset used by downstream modules.
#'
#' @param sources Named list of source specifications accepted by
#'   \code{\link{run_m0}}.
#' @param modules Character vector selecting downstream modules to run.
#'   Supported values are \code{"m0"}, \code{"m1"}, \code{"m2"}, \code{"m3"}, \code{"m4"}.
#' @param config Configuration list created with \code{\link{biblio_config}}.
#' @param prisma_spec Optional PRISMA specification passed to \code{\link{run_m0}}.
#' @param export Logical. If TRUE, module artifacts and pipeline bundle are
#'   written to disk.
#' @param screening_ledger Optional screening ledger passed to \code{\link{run_m0}}
#'   and, when enabled, \code{\link{run_bslr}}.
#' @param search_metadata Optional methodological search metadata passed to
#'   \code{\link{run_m0}} in core mode.
#' @param bslr_protocol Optional B-SLR protocol list or JSON/YAML path. When
#'   supplied, the pipeline runs the B-SLR workflow layer and exposes M0-M3 plus
#'   the B-SLR result in one object.
#' @param bslr Logical. Force B-SLR mode. Defaults to TRUE when a protocol is
#'   supplied or \code{"bslr"} is included in \code{modules}.
#'
#' @return An object of class \code{biblio_pipeline_result}.
#' @export
run_pipeline <- function(sources,
                         modules = c("m0", "m1", "m2", "m3", "m4"),
                         config = biblio_config(),
                         prisma_spec = NULL,
                         export = TRUE,
                         screening_ledger = NULL,
                         search_metadata = NULL,
                         bslr_protocol = NULL,
                         bslr = !is.null(bslr_protocol) || "bslr" %in% tolower(as.character(modules))) {
  config <- merge_biblio_config(config)
  modules <- unique(tolower(as.character(modules)))

  if (length(modules) == 0) {
    cli::cli_abort("run_pipeline requires at least one module")
  }

  valid_modules <- c(get_available_modules(), "bslr")
  invalid_modules <- setdiff(modules, valid_modules)
  if (length(invalid_modules) > 0) {
    cli::cli_abort("Unknown pipeline module(s): {paste(invalid_modules, collapse = ', ')}")
  }

  if (!"m0" %in% modules) {
    modules <- c("m0", modules)
  }

  if (isTRUE(bslr)) {
    if (is.null(bslr_protocol)) {
      cli::cli_abort("run_pipeline(..., bslr = TRUE) requires `bslr_protocol`.")
    }
    bslr_result <- run_bslr(
      sources = sources,
      protocol = bslr_protocol,
      screening_ledger = screening_ledger,
      prisma_spec = if (is.null(prisma_spec)) "auto" else prisma_spec,
      modules = setdiff(modules, c("m0", "bslr")),
      config = config,
      export = export
    )
    module_results <- bslr_result$data$modules %||% list()
    module_results$bslr <- bslr_result
    module_status <- vapply(module_results, function(x) x$status %||% "unknown", character(1))
    pipeline_result <- m0_build_pipeline_result(
      module_results = module_results,
      module_status = module_status,
      config = config,
      workflow = "bslr",
      artifacts = m0_collect_pipeline_artifacts(module_results),
      metadata_extra = list(
        methodological_mode = bslr_result$data$journal_assessment$summary$methodological_mode %||% NA_character_,
        protocol_ok = isTRUE(bslr_result$data$protocol_validation$ok),
        bslr_status = bslr_result$status
      )
    )
    if (export && !identical(config$report_format, "none")) {
      bundle <- build_pipeline_report_bundle(pipeline_result, config)
      pipeline_result$artifacts$reports <- bundle$reports
      pipeline_result$artifacts$files <- unique(c(pipeline_result$artifacts$files, bundle$files))
    }
    return(pipeline_result)
  }

  module_results <- list()
  module_status <- character()

  module_results$m0 <- run_m0(
    sources = sources,
    config = config,
    prisma_spec = prisma_spec,
    screening_ledger = screening_ledger,
    search_metadata = search_metadata,
    export = export
  )
  module_status[["m0"]] <- module_results$m0$status

  bib_data <- m0_get_bib_data(module_results$m0)
  if (is.null(bib_data)) {
    cli::cli_abort("Pipeline aborted: M0 did not produce merged bibliographic data")
  }

  if ("m1" %in% modules) {
    module_results$m1 <- run_m1(bib_data, config = config, export = export)
    module_status[["m1"]] <- module_results$m1$status
  }

  if ("m2" %in% modules) {
    annual_input <- m0_prepare_m2_input(module_results$m0)
    module_results$m2 <- run_m2(annual_input, config = config, export = export)
    module_status[["m2"]] <- module_results$m2$status
  }

  if ("m3" %in% modules) {
    module_results$m3 <- run_m3(bib_data, config = config, export = export)
    module_status[["m3"]] <- module_results$m3$status
  }

  if ("m4" %in% modules) {
    module_results$m4 <- run_m4(bib_data, config = config, export = export)
    module_status[["m4"]] <- module_results$m4$status
  }

  pipeline_result <- m0_build_pipeline_result(
    module_results = module_results,
    module_status = module_status,
    config = config,
    workflow = "core",
    artifacts = m0_collect_pipeline_artifacts(module_results)
  )

  if (export && !identical(config$report_format, "none")) {
    bundle <- build_pipeline_report_bundle(pipeline_result, config)
    pipeline_result$artifacts$reports <- bundle$reports
    pipeline_result$artifacts$files <- bundle$files
  }

  pipeline_result
}

#' Build a standardized pipeline result
#' @keywords internal
m0_build_pipeline_result <- function(module_results,
                                     module_status,
                                     config,
                                     workflow = "core",
                                     artifacts = list(reports = list(), files = character()),
                                     metadata_extra = list()) {
  structure(
    list(
      status = m0_pipeline_status(module_status),
      modules = module_results,
      config = config,
      summary = m0_build_pipeline_summary(module_results),
      artifacts = artifacts,
      metadata = c(
        list(
          workflow = workflow,
          executed_modules = names(module_results),
          generated_at = Sys.time()
        ),
        metadata_extra
      )
    ),
    class = c("biblio_pipeline_result", "list")
  )
}

#' Collect exported artifacts from nested module manifests
#' @keywords internal
m0_collect_pipeline_artifacts <- function(module_results) {
  files <- character()
  plots <- character()
  tables <- character()
  reports <- character()

  for (module_result in module_results) {
    manifest <- module_result$artifacts$manifest %||% NULL
    if (inherits(manifest, "biblio_artifact_manifest") || is.list(manifest)) {
      files <- c(files, manifest$files %||% character())
      plots <- c(plots, manifest$plots %||% character())
      tables <- c(tables, manifest$tables %||% character())
      reports <- c(reports, manifest$reports %||% character())
    }
  }

  list(
    reports = list(),
    files = unique(c(files, plots, tables, reports)),
    plots = unique(plots),
    tables = unique(tables),
    module_reports = unique(reports)
  )
}

#' Validate a pipeline result object
#'
#' @param x Object to validate.
#' @return Logical. TRUE when \code{x} has the expected pipeline structure.
#' @export
validate_pipeline_result <- function(x) {
  inherits(x, "biblio_pipeline_result") &&
    all(c("status", "modules", "config", "summary", "artifacts", "metadata") %in% names(x))
}

#' Prepare annual production input for M2 from M0 output
#' @keywords internal
m0_prepare_m2_input <- function(m0_result) {
  annual <- m0_get(m0_result, "annual")
  if (is.null(annual) || !is.data.frame(annual) || nrow(annual) == 0) {
    return(data.frame(Year = numeric(0), Articles = numeric(0)))
  }

  annual_out <- annual

  if ("PY" %in% names(annual_out) && !"Year" %in% names(annual_out)) {
    annual_out$Year <- annual_out$PY
  }
  if ("year" %in% names(annual_out) && !"Year" %in% names(annual_out)) {
    annual_out$Year <- annual_out$year
  }
  if ("article_count" %in% names(annual_out) && !"Articles" %in% names(annual_out)) {
    annual_out$Articles <- annual_out$article_count
  }
  if ("articles" %in% names(annual_out) && !"Articles" %in% names(annual_out)) {
    annual_out$Articles <- annual_out$articles
  }

  annual_out[, intersect(c("Year", "Articles", "article_count", "year", "total_citations"), names(annual_out)), drop = FALSE]
}

#' Compute aggregate pipeline status
#' @keywords internal
m0_pipeline_status <- function(module_status) {
  statuses <- unname(module_status)
  if (any(statuses == "error")) {
    return("error")
  }
  if (any(statuses == "warning")) {
    return("warning")
  }
  "success"
}

#' Build pipeline summary statistics
#' @keywords internal
m0_build_pipeline_summary <- function(module_results) {
  m0_result <- module_results$m0
  bib_data <- if (!is.null(m0_result)) m0_get_bib_data(m0_result) else NULL

  year_values <- if (!is.null(bib_data) && "PY" %in% names(bib_data)) {
    suppressWarnings(as.integer(bib_data$PY))
  } else {
    integer()
  }
  year_values <- year_values[!is.na(year_values)]

  list(
    n_documents = if (!is.null(bib_data)) nrow(bib_data) else 0L,
    year_range = if (length(year_values) > 0) range(year_values) else c(NA_integer_, NA_integer_),
    total_citations = if (!is.null(bib_data) && "TC" %in% names(bib_data)) sum(as.numeric(bib_data$TC), na.rm = TRUE) else 0,
    n_authors = nrow(m0_get(m0_result, "authors") %||% data.frame()),
    n_sources = nrow(m0_get(m0_result, "sources") %||% data.frame()),
    module_status = vapply(module_results, function(x) x$status %||% "unknown", character(1))
  )
}

#' Build a publication-ready pipeline report bundle
#' @keywords internal
build_pipeline_report_bundle <- function(pipeline_result, config) {
  report_dir <- build_output_path("pipeline", "reports", config = config, create = TRUE)
  manifest_path <- file.path(report_dir, "pipeline_manifest.json")
  summary_path <- file.path(report_dir, "pipeline_summary.json")
  qmd_path <- file.path(report_dir, "pipeline_summary.qmd")
  appendix_path <- file.path(report_dir, "pipeline_appendix.qmd")
  tex_path <- file.path(report_dir, "pipeline_ieee.tex")

  summary_payload <- pipeline_result$summary
  write_json_artifact(summary_payload, summary_path)

  qmd_lines <- c(
    "---",
    sprintf("title: \"%s\"", "RBiblioSynth Pipeline Report"),
    "format:",
    if (identical(config$report_format, "quarto_pdf")) "  pdf: default" else "  html: default",
    "execute:",
    "  echo: false",
    "---",
    "",
    "# Overview",
    sprintf("- Workflow: `%s`", pipeline_result$metadata$workflow %||% "core"),
    sprintf("- Documents: %s", summary_payload$n_documents),
    sprintf("- Year range: %s - %s", summary_payload$year_range[1], summary_payload$year_range[2]),
    sprintf("- Total citations: %s", summary_payload$total_citations),
    sprintf("- Modules executed: %s", paste(names(pipeline_result$modules), collapse = ", ")),
    sprintf("- Exported artifact files indexed: %s", length(pipeline_result$artifacts$files %||% character())),
    "",
    "# Module Status",
    ""
  )
  qmd_lines <- c(
    qmd_lines,
    vapply(
      names(summary_payload$module_status),
      function(nm) sprintf("- `%s`: %s", nm, summary_payload$module_status[[nm]]),
      character(1)
    )
  )
  if (identical(pipeline_result$metadata$workflow %||% "", "bslr")) {
    qmd_lines <- c(
      qmd_lines,
      "",
      "# B-SLR Integration",
      "",
      sprintf("- Methodological mode: `%s`", pipeline_result$metadata$methodological_mode %||% "unknown"),
      sprintf("- Protocol validation: `%s`", if (isTRUE(pipeline_result$metadata$protocol_ok)) "passed" else "requires human completion"),
      sprintf("- B-SLR status: `%s`", pipeline_result$metadata$bslr_status %||% "unknown"),
      "- The B-SLR module contains the protocol, checkpoints, bibliometric map, SLR scaffolds, and paper assembly bundle."
    )
  }
  write_text_report(qmd_lines, qmd_path)

  appendix_lines <- c(
    "---",
    "title: \"Methodological Appendix\"",
    "format:",
    if (identical(config$report_format, "quarto_pdf")) "  pdf: default" else "  html: default",
    "---",
    "",
    "# Reproducibility",
    "",
    "## Configuration",
    sprintf("- Counting mode: `%s`", config$counting_mode),
    sprintf("- Deduplication: `%s`", paste(config$dedup_method, collapse = ", ")),
    sprintf("- Strict validation: `%s`", config$validate_strict),
    "",
    "## Artifact Manifest",
    sprintf("- Generated at: `%s`", pipeline_result$metadata$generated_at),
    sprintf("- Output directory: `%s`", normalizePath(config$output_dir, winslash = "/", mustWork = FALSE)),
    sprintf("- Workflow: `%s`", pipeline_result$metadata$workflow %||% "core")
  )
  write_text_report(appendix_lines, appendix_path)

  tex_lines <- c(
    "\\documentclass[conference]{IEEEtran}",
    "\\usepackage{graphicx}",
    "\\usepackage{booktabs}",
    "\\begin{document}",
    "\\title{RBiblioSynth Bibliometric Pipeline Report}",
    "\\author{RBiblioSynth}",
    "\\maketitle",
    "\\begin{abstract}",
    "Automated bibliometric report generated from the official RBiblioSynth M0-M3 pipeline.",
    "\\end{abstract}",
    "\\section{Overview}",
    sprintf("Documents analysed: %s. Total citations: %s.", summary_payload$n_documents, summary_payload$total_citations),
    sprintf("Workflow: %s.", m0_escape_latex(pipeline_result$metadata$workflow %||% "core")),
    "\\section{Reproducibility}",
    sprintf("Counting mode: %s. Deduplication strategy: %s.", config$counting_mode, paste(config$dedup_method, collapse = ", ")),
    if (identical(pipeline_result$metadata$workflow %||% "", "bslr")) {
      sprintf(
        "B-SLR methodological mode: %s. B-SLR status: %s.",
        m0_escape_latex(pipeline_result$metadata$methodological_mode %||% "unknown"),
        m0_escape_latex(pipeline_result$metadata$bslr_status %||% "unknown")
      )
    } else {
      character()
    },
    "\\end{document}"
  )
  write_text_report(tex_lines, tex_path)

  manifest_payload <- list(
    status = pipeline_result$status,
    reports = c(qmd_path, appendix_path, tex_path),
    summary = summary_path,
    modules = names(pipeline_result$modules),
    workflow = pipeline_result$metadata$workflow %||% "core",
    bslr = if (identical(pipeline_result$metadata$workflow %||% "", "bslr")) {
      list(
        methodological_mode = pipeline_result$metadata$methodological_mode %||% NA_character_,
        protocol_ok = isTRUE(pipeline_result$metadata$protocol_ok),
        status = pipeline_result$metadata$bslr_status %||% "unknown"
      )
    } else {
      NULL
    },
    module_manifests = lapply(pipeline_result$modules, function(x) {
      if (inherits(x$artifacts$manifest, "biblio_artifact_manifest")) {
        unclass(x$artifacts$manifest)
      } else {
        NULL
      }
    }),
    config = list(
      counting_mode = config$counting_mode,
      dedup_method = config$dedup_method,
      report_format = config$report_format,
      validate_strict = config$validate_strict
    )
  )
  write_json_artifact(manifest_payload, manifest_path)

  list(
    reports = list(
      summary = list(path = qmd_path, format = config$report_format),
      appendix = list(path = appendix_path, format = config$report_format),
      ieee_latex = list(path = tex_path, format = "latex_bundle")
    ),
    files = c(summary_path, manifest_path, qmd_path, appendix_path, tex_path)
  )
}
