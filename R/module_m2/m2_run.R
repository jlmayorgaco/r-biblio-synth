# ============================================================================
# module_m2/m2_run.R - Orchestrator for M2
# ============================================================================

#' Run M2 module (Annual Production Analysis)
#'
#' @param input A data frame with Year and Articles columns.
#' @param config A configuration list.
#' @param export Logical. If TRUE, exports artifacts to disk.
#' @export
run_m2 <- function(input, config = biblio_config(), export = TRUE) {
  config <- merge_biblio_config(config)

  # 1. Validate
  validation <- validate_m2_input(input, config)

  # 2. Compute all metrics
  data <- list(
    eda        = compute_m2_eda(input, config),
    regression = compute_m2_regression(input, config),
    harmonics  = compute_m2_harmonics(input, config)
  )

  # 3. Build result
  result <- new_module_result(
    module_id   = "m2",
    module_name = "Annual Production",
    status      = if (validation$ok) "success" else "warning",
    inputs      = list(n_rows = validation$n_rows, n_cols = validation$n_cols),
    data        = data,
    diagnostics = list(warnings = character(), checks = list(validation = validation), notes = character())
  )

  # 4. Render
  plots <- list(
    eda        = render_m2_eda(data$eda, config),
    regression = render_m2_regression(data$regression, config),
    harmonics  = render_m2_harmonics(data$harmonics, config)
  )
  result$artifacts$plots <- plots

  # 5. Tables
  tables <- list(
    eda        = build_m2_eda_table(data$eda, config),
    regression = build_m2_regression_table(data$regression, config)
  )
  result$artifacts$tables <- tables

  # 6. Export
  if (export) {
    exported <- export_m2(result, config)
    manifest <- build_m2_manifest(result, exported, config)
    result <- attach_manifest_to_result(result, manifest)
  }

  result
}

#' Export M2 artifacts
#' @export
export_m2 <- function(result, config = biblio_config()) {
  config <- merge_biblio_config(config)

  exported_plots <- character()
  exported_jsons <- character()

  w <- config$plot_width
  h <- config$plot_height

  if (config$export_plots) {
    for (nm in names(result$artifacts$plots)) {
      plot_section <- result$artifacts$plots[[nm]]
      for (pnm in names(plot_section$plots)) {
        p <- build_artifact_path("m2", "plots", paste0("m2_", nm, "_", pnm), "png", config)
        tryCatch({
          export_plot_artifact(plot_section$plots[[pnm]], tools::file_path_sans_ext(p),
                               width = w, height = h, dpi = config$dpi)
          exported_plots <- c(exported_plots, p)
        }, error = function(e) NULL)
      }
    }
  }

  if (config$export_json) {
    for (nm in names(result$data)) {
      j <- build_artifact_path("m2", "json", paste0("m2_", nm), "json", config)
      tryCatch({
        write_json_artifact(result$data[[nm]], j)
        exported_jsons <- c(exported_jsons, j)
      }, error = function(e) NULL)
    }
  }

  list(plots = exported_plots, tables = character(), reports = character(), files = exported_jsons)
}

#' Build M2 manifest
#' @export
build_m2_manifest <- function(result, exported = list(), config = biblio_config()) {
  status <- if (inherits(result, "biblio_module_result")) result$status else "stub"
  new_artifact_manifest(
    module_id = "m2",
    generated_at = Sys.time(),
    files = if (is.null(exported$files)) character() else exported$files,
    plots = if (is.null(exported$plots)) character() else exported$plots,
    tables = if (is.null(exported$tables)) character() else exported$tables,
    reports = if (is.null(exported$reports)) character() else exported$reports,
    status = status
  )
}
