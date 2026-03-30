# ============================================================================
# m4_run.R - M4 Institutional Analysis Orchestrator
# ============================================================================

#' Run M4 module (Institutional Analysis)
#'
#' Analyzes institutional affiliations, productivity, and collaboration networks.
#'
#' @param input Bibliographic data frame
#' @param config Configuration list
#' @param export Logical. If TRUE, exports artifacts to disk.
#' @return Module result object
#' @export
run_m4 <- function(input, config = biblio_config(), export = TRUE) {
  config <- merge_biblio_config(config)
  
  # Initialize logging
  log_message("INFO", "Starting M4: Institutional Analysis")
  
  # 1. Validate input
  validation <- validate_m4_input(input, config)
  if (!validation$ok) {
    log_message("ERROR", "M4 validation failed: {msg}", msg = validation$message)
    return(create_error_result("m4", "Institutional Analysis", validation$message))
  }
  
  # 2. Extract and parse institutions
  log_message("INFO", "Extracting institutions from affiliations...")
  parsed_data <- m4_extract_institutions(input, config)
  
  if (parsed_data$status != "success") {
    log_message("ERROR", "Institution extraction failed")
    return(create_error_result("m4", "Institutional Analysis", "Extraction failed"))
  }
  
  # 3. Compute all metrics
  log_message("INFO", "Computing institutional metrics...")
  data <- m4_compute_all(parsed_data, config)
  
  # 4. Build result
  result <- new_module_result(
    module_id   = "m4",
    module_name = "Institutional Analysis",
    status      = "success",
    inputs      = list(n_records = nrow(input)),
    data        = data,
    diagnostics = list(warnings = character(), checks = list())
  )
  
  # 5. Render visualizations
  log_message("INFO", "Rendering institutional visualizations...")
  result <- m4_render_all(result, data, config)
  
  # 6. Build tables
  result <- m4_build_tables(result, data, config)
  
  # 7. Export if requested
  if (export) {
    log_message("INFO", "Exporting M4 artifacts...")
    exported <- export_m4(result, config)
    manifest <- build_m4_manifest(result, exported, config)
    result <- attach_manifest_to_result(result, manifest)
  }
  
  log_message("INFO", "M4: Institutional Analysis completed successfully")
  result
}

#' Validate M4 input
#' @keywords internal
validate_m4_input <- function(input, config) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(ok = FALSE, message = "Input must be a non-empty data frame"))
  }
  
  # Check for affiliation column
  affil_col <- if ("C1" %in% names(input)) "C1" else NULL
  if (is.null(affil_col)) {
    return(list(ok = FALSE, message = "Missing affiliation column (C1)"))
  }
  
  list(ok = TRUE, n_rows = nrow(input), n_cols = ncol(input))
}

#' Compute all M4 metrics
#' @keywords internal
m4_compute_all <- function(parsed_data, config) {
  list(
    extraction       = parsed_data,
    production       = m4_compute_institutional_production(parsed_data, config),
    collaboration    = m4_compute_institutional_collaboration(parsed_data, config),
    ranking          = m4_compute_institutional_ranking(parsed_data, config),
    networks         = m4_compute_institutional_networks(parsed_data, config),
    sector_analysis  = m4_compute_sector_analysis(parsed_data, config),
    geography        = m4_compute_institutional_geography(parsed_data, config)
  )
}

#' Render all M4 visualizations
#' @keywords internal
m4_render_all <- function(result, data, config) {
  result$artifacts$plots <- list(
    production    = render_m4_production(data$production, config),
    collaboration = render_m4_collaboration(data$collaboration, config),
    networks      = render_m4_networks(data$networks, config),
    geography     = render_m4_geography(data$geography, config)
  )
  result
}

#' Build all M4 tables
#' @keywords internal
m4_build_tables <- function(result, data, config) {
  result$artifacts$tables <- list(
    production    = build_m4_production_table(data$production, config),
    collaboration = build_m4_collaboration_table(data$collaboration, config),
    ranking       = build_m4_ranking_table(data$ranking, config)
  )
  result
}

#' Export M4 results
#' @keywords internal
export_m4 <- function(result, config) {
  exported <- character()
  
  # Export plots
  if (config$export_plots) {
    for (plot_name in names(result$artifacts$plots)) {
      if (!is.null(result$artifacts$plots[[plot_name]])) {
        path <- export_plot_artifact(
          result$artifacts$plots[[plot_name]],
          file.path(config$output_dir, "m4", plot_name),
          width = config$plot_width,
          height = config$plot_height,
          dpi = config$dpi
        )
        exported <- c(exported, path)
      }
    }
  }
  
  # Export tables
  if (config$export_json) {
    for (table_name in names(result$artifacts$tables)) {
      path <- file.path(config$output_dir, "m4", paste0(table_name, ".json"))
      write_json_artifact(result$artifacts$tables[[table_name]], path)
      exported <- c(exported, path)
    }
  }
  
  exported
}

#' Build M4 manifest
#' @keywords internal
build_m4_manifest <- function(result, exported, config) {
  list(
    module = "m4",
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    n_records = result$inputs$n_records,
    n_institutions = nrow(result$data$production$institutions),
    exported_files = exported,
    status = result$status
  )
}

#' Create error result helper
#' @keywords internal
create_error_result <- function(module_id, module_name, message) {
  new_module_result(
    module_id = module_id,
    module_name = module_name,
    status = "error",
    inputs = list(),
    data = list(),
    diagnostics = list(errors = message)
  )
}