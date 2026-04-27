# ============================================================================
# m3_run.R - Orchestrator for M3 Countries Module
# ============================================================================

#' Run the M3 Countries Analysis Module
#'
#' Orchestrates validation, data preparation, computation, rendering,
#' table building, reporting, and optional export for the Countries module.
#'
#' @param input A bibliographic data frame (must contain AU_CO or C1).
#' @param config A configuration list (see \code{biblio_config}).
#' @param export Logical. If TRUE, exports plots, tables, and JSON artifacts.
#' @return A \code{biblio_module_result} object.
#' @export
run_m3 <- function(input, config = biblio_config(), export = TRUE) {
  config <- merge_biblio_config(config)

  # 1. Validate
  validation <- validate_m3_input(input)
  if (!validation$ok) {
    if (config$validate_strict) {
      cli::cli_abort("M3 validation failed: {validation$error}")
    }
    cli::cli_warn("M3 validation failed: {validation$error}")
  }

  # 2. Prepare data
  prepared_data <- prepare_m3_country_data(input, config)
  if (prepared_data$status != "success") {
    cli::cli_warn("M3 data preparation issue: {prepared_data$status}")
  }

  # 3. Compute all metrics
  data <- m3_compute_all(prepared_data, config)

  # 4. Build result shell
  result <- m3_build_result(data, validation, prepared_data)

  # 5. Render plots
  result <- m3_render_all(result, data, config)

  # 6. Build tables
  result <- m3_build_tables(result, data, config)

  # 7. Build and attach report
  report <- build_m3_report(result, config)
  result <- attach_report_to_result(result, report)

  # 8. Export and manifest
  if (export) {
    exported <- export_m3(result, config)
    manifest <- build_m3_manifest(result, exported, config)
    result   <- attach_manifest_to_result(result, manifest)
  }

  result
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

m3_compute_all <- function(prepared_data, config) {
  list(
    production            = m3_compute_production(prepared_data, config),
    citations             = m3_compute_citations(prepared_data, config),
    scp_mcp               = m3_compute_scp_mcp(prepared_data, config),
    inequality            = m3_compute_inequality(prepared_data, config),
    rankings              = m3_compute_rankings(prepared_data, config),
    distribution_tests    = m3_compute_distribution_tests(prepared_data, config),
    growth_dynamics       = m3_compute_growth_dynamics(prepared_data, config),
    change_points         = m3_compute_change_points(prepared_data, config),
    profiles              = m3_compute_country_profiles(prepared_data, config),
    similarity_clustering  = m3_compute_similarity_clustering(prepared_data, config),
    collaboration_indices = compute_m3_collaboration_indices(prepared_data, config),
    country_regressions   = m3_compute_country_regressions(prepared_data, config),
    hypotheses            = m3_compute_hypotheses(prepared_data, config),
    experiments           = m3_compute_experiments(prepared_data, config),
    spatial               = m3_compute_spatial(prepared_data, config),
    regional              = m3_compute_regional_wrapper(prepared_data, config),
    economic              = m3_compute_economic_correlation(prepared_data$country_summary, config),
    temporal_dynamics     = m3_compute_temporal_dynamics(prepared_data$country_annual, config)
  )
}

m3_build_result <- function(data, validation, prepared_data) {
  n_countries <- if (nrow(prepared_data$country_summary) > 0)
    nrow(prepared_data$country_summary) else 0L

  new_module_result(
    module_id   = "m3",
    module_name = "Countries",
    status      = if (validation$ok) "success" else "warning",
    inputs      = list(
      n_rows      = if (is.list(validation)) validation$n_rows %||% NA_integer_ else NA_integer_,
      n_countries = n_countries
    ),
    data        = data,
    diagnostics = list(
      warnings = character(),
      checks   = list(validation = validation),
      notes    = character()
    )
  )
}

m3_render_all <- function(result, data, config) {
  result$artifacts$plots <- list(
    productivity           = m3_render_productivity(data$production, config),
    citations              = m3_render_citations(data$citations, config),
    scp_mcp                = m3_render_scp_mcp(data$scp_mcp, config),
    lorenz                 = m3_render_lorenz(data$inequality, config),
    rankings               = m3_render_rankings(data$rankings, config),
    growth                 = m3_render_growth_dynamics(data$growth_dynamics, config),
    change_points          = m3_render_change_points(data$change_points,
                                                     data$growth_dynamics, config),
    similarity             = m3_render_similarity(data$profiles, config),
    experiments            = m3_render_experiments(data$experiments, config),
    collaboration_indices  = render_m3_collaboration_indices(data$collaboration_indices, config),
    country_regressions     = render_m3_country_regressions(data$country_regressions, config),
    world_map               = render_m3_world_map(data, config),
    spatial                = render_m3_spatial(data$spatial, config),
    regional               = render_m3_regional(data$regional, config),
    economic               = render_m3_economic(data$economic, config),
    network                = render_m3_collaboration_network(data$collaboration_indices, config)
  )
  result
}

m3_build_tables <- function(result, data, config) {
  result$artifacts$tables <- list(
    productivity            = m3_table_productivity(data$production, config),
    citations               = m3_table_citations(data$citations, config),
    scp_mcp                 = m3_table_scp_mcp(data$scp_mcp, config),
    inequality              = m3_table_inequality(data$inequality, config),
    rankings                = m3_table_rankings(data$rankings, config),
    distribution_tests      = m3_table_distribution_tests(
                               data$distribution_tests, config),
    growth_dynamics         = m3_table_growth_dynamics(data$growth_dynamics, config),
    profiles                = m3_table_profiles(data$profiles, config),
    experiments             = m3_table_experiments(data$experiments, config),
    collaboration_indices   = m3_table_collaboration_indices(data$collaboration_indices, config),
    country_regressions     = m3_table_country_regressions(data$country_regressions, config),
    spatial                 = build_m3_spatial_table(data$spatial, config),
    regional                = build_m3_regional_table(data$regional, config),
    economic                = build_m3_economic_table(data$economic, config)
  )
  result
}

#' Export M3 artifacts
#' @export
export_m3 <- function(result, config = biblio_config()) {
  config <- merge_biblio_config(config)

  exported_plots   <- character()
  exported_reports <- character()
  exported_jsons   <- character()

  w <- config$plot_width
  h <- config$plot_height

  if (config$export_plots) {
    for (section_nm in names(result$artifacts$plots)) {
      section <- result$artifacts$plots[[section_nm]]
      if (!is.list(section) || !is.list(section$plots)) next
      for (plot_nm in names(section$plots)) {
        p_obj <- section$plots[[plot_nm]]
        if (is.null(p_obj)) next
        out <- build_artifact_path(
          "m3", "plots",
          paste0("m3_", section_nm, "_", plot_nm), "png", config
        )
        tryCatch(
          {
            export_plot_artifact(p_obj, tools::file_path_sans_ext(out),
                                 width = w, height = h, dpi = config$dpi)
            exported_plots <- c(exported_plots, out)
          },
          error = function(e) cli::cli_warn("M3 plot export failed [{plot_nm}]: {e$message}")
        )
      }
    }
  }

  if (config$export_json) {
    for (nm in names(result$data)) {
      j <- build_artifact_path("m3", "json", paste0("m3_", nm), "json", config)
      tryCatch(
        {
          write_json_artifact(result$data[[nm]], j)
          exported_jsons <- c(exported_jsons, j)
        },
        error = function(e) cli::cli_warn("M3 JSON export failed [{nm}]: {e$message}")
      )
    }
  }

  if (config$export_reports && length(result$artifacts$reports) > 0) {
    report <- result$artifacts$reports[[1]]
    if (!is.null(report$lines) && length(report$lines) > 0) {
      r <- build_artifact_path("m3", "reports", "m3_report", "txt", config)
      write_text_report(report$lines, r)
      exported_reports <- c(exported_reports, r)
    }
  }

  list(plots = exported_plots, tables = character(),
       reports = exported_reports, files = exported_jsons)
}

# Null-coalescing helper (avoid rlang dependency for one symbol)
`%||%` <- function(a, b) if (!is.null(a)) a else b
