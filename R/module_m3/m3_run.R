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
  result <- m3_apply_ieee_plot_standards(result, config)

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
  data <- list(
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
  data$advanced_journal <- m3_compute_advanced_journal(prepared_data, data, config)
  data$narrative <- m3_compute_narrative(data, prepared_data, config)
  data
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
    temporal_dynamics      = render_m3_temporal_dynamics(data$temporal_dynamics, config),
    network                = render_m3_collaboration_network(data$collaboration_indices, config),
    advanced_journal       = render_m3_advanced_journal(data$advanced_journal, config),
    narrative              = render_m3_narrative(data$narrative, config)
  )
  result$artifacts$plots <- m3_fill_core_plot_placeholders(result$artifacts$plots, data, config)
  result
}

m3_fill_core_plot_placeholders <- function(plot_sections, data, config) {
  core_specs <- list(
    productivity = list(
      title = "Country productivity overview unavailable",
      message = "Insufficient data remained after validation to render the core production comparison."
    ),
    citations = list(
      title = "Country citation overview unavailable",
      message = "Citation totals were not sufficient to render the core impact comparison reliably."
    ),
    scp_mcp = list(
      title = "Collaboration structure unavailable",
      message = "Domestic versus international collaboration could not be summarized from the available records."
    ),
    lorenz = list(
      title = "Concentration profile unavailable",
      message = "The inequality profile could not be estimated robustly for the available country sample."
    ),
    growth = list(
      title = "Country growth dynamics unavailable",
      message = "Annual country trajectories were too sparse to support a stable growth-dynamics figure."
    ),
    experiments = list(
      title = "Quadrant and momentum view unavailable",
      message = "The exploratory quadrant layer requires a richer country profile to remain interpretable."
    ),
    spatial = list(
      title = "Spatial structure unavailable",
      message = "Spatial diagnostics were not estimable under the current country coverage and neighborhood structure."
    ),
    regional = list(
      title = "Regional summary unavailable",
      message = "Regional aggregation did not yield enough signal to support a publication-grade comparison."
    ),
    economic = list(
      title = "Economic correlates unavailable",
      message = "Economic matching or regression evidence was too limited to support a journal-ready figure."
    ),
    temporal_dynamics = list(
      title = "Temporal country dynamics unavailable",
      message = "Windowed share, rank, or emergence dynamics were too sparse to render a stable temporal narrative."
    ),
    advanced_journal = list(
      title = "Advanced geographic journal analytics unavailable",
      message = "The optional advanced M3 layer did not have enough country, citation, collaboration, or temporal evidence to render premium, mobility, trajectory, concentration, and regional plots."
    ),
    narrative = list(
      title = "Geographic narrative evidence unavailable",
      message = "Coverage, concentration, impact, collaboration, and growth metrics could not be normalized into a narrative dashboard."
    )
  )

  for (section_nm in names(core_specs)) {
    section <- plot_sections[[section_nm]]
    has_content <- FALSE
    if (is.list(section) && is.list(section$plots)) {
      has_content <- length(m3_flatten_plot_collection(section$plots)) > 0
    }

    if (!has_content) {
      spec <- core_specs[[section_nm]]
      layout <- if (section_nm %in% c("productivity", "citations", "experiments", "regional", "temporal_dynamics")) "full" else "single"
      plot_sections[[section_nm]] <- list(
        status = "placeholder",
        plots = list(
          insufficient_data = m3_placeholder_plot(
            title = spec$title,
            message = spec$message,
            layout = layout
          )
        )
      )
    }
  }

  plot_sections
}

m3_placeholder_plot <- function(title, message, layout = "single") {
  ieee_no_data_plot(title = title, message = message, layout = layout)
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
    economic                = build_m3_economic_table(data$economic, config),
    temporal_dynamics       = m3_table_temporal_dynamics(data$temporal_dynamics, config),
    advanced_journal        = m3_table_advanced_journal(data$advanced_journal, config),
    narrative               = m3_table_narrative(data$narrative, config)
  )
  result
}

#' Export M3 artifacts
#' @export
export_m3 <- function(result, config = biblio_config()) {
  config <- merge_biblio_config(config)

  exported_plots   <- character()
  exported_tables  <- character()
  exported_reports <- character()
  exported_jsons   <- character()

  if (config$export_plots) {
    for (section_nm in names(result$artifacts$plots)) {
      section <- result$artifacts$plots[[section_nm]]
      if (!is.list(section) || !is.list(section$plots)) next
      flat_plots <- m3_flatten_plot_collection(section$plots)
      for (plot_nm in names(flat_plots)) {
        p_obj <- ieee_prepare_plot_for_export(
          flat_plots[[plot_nm]],
          module_id = "m3",
          section_id = section_nm,
          plot_id = plot_nm,
          config = config
        )
        if (is.null(p_obj)) next
        spec <- ieee_get_plot_export_spec(
          p_obj,
          config = config,
          section_id = section_nm,
          plot_id = plot_nm
        )
        out <- build_artifact_path(
          "m3", "plots",
          paste0("m3_", section_nm, "_", plot_nm), "png", config
        )
        tryCatch(
          {
            exported_paths <- export_plot_artifact(
              p_obj,
              tools::file_path_sans_ext(out),
              width = spec$width,
              height = spec$height,
              dpi = spec$dpi
            )
            exported_plots <- c(exported_plots, unname(exported_paths[!is.na(exported_paths)]))
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

  for (section_nm in names(result$artifacts$tables)) {
    table_section <- result$artifacts$tables[[section_nm]]
    flat_tables <- m3_flatten_table_collection(table_section)
    for (table_nm in names(flat_tables)) {
      csv_path <- build_artifact_path(
        "m3", "tables",
        paste0("m3_", section_nm, "_", table_nm), "csv", config
      )
      dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
      tryCatch(
        {
          utils::write.csv(flat_tables[[table_nm]], csv_path, row.names = FALSE, na = "")
          exported_tables <- c(exported_tables, csv_path)
        },
        error = function(e) cli::cli_warn("M3 table export failed [{table_nm}]: {e$message}")
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
    if (!is.null(report$tex) && length(report$tex) > 0) {
      t <- build_artifact_path("m3", "reports", "m3_report", "tex", config)
      writeLines(report$tex, t)
      exported_reports <- c(exported_reports, t)
    }
  }

  list(plots = exported_plots, tables = exported_tables,
       reports = exported_reports, files = exported_jsons)
}

#' Apply IEEE-ready styling metadata to all rendered M3 plots
#' @keywords internal
m3_apply_ieee_plot_standards <- function(result, config = biblio_config()) {
  if (!inherits(result, "biblio_module_result") || !is.list(result$artifacts$plots)) {
    return(result)
  }

  for (section_nm in names(result$artifacts$plots)) {
    section <- result$artifacts$plots[[section_nm]]
    if (!is.list(section) || !is.list(section$plots)) {
      next
    }

    for (plot_nm in names(section$plots)) {
      plot_obj <- section$plots[[plot_nm]]
      if (inherits(plot_obj, "ggplot")) {
        plot_caption <- plot_obj$labels$caption %||% ""
        if (!nzchar(plot_caption)) {
          default_caption <- sprintf(
            "M3 %s figure: %s.",
            gsub("_", " ", section_nm),
            gsub("_", " ", plot_nm)
          )
          plot_obj <- plot_obj + ggplot2::labs(caption = default_caption)
        }
        section$plots[[plot_nm]] <- ieee_prepare_plot_for_export(
          plot_obj,
          module_id = "m3",
          section_id = section_nm,
          plot_id = plot_nm,
          config = config
        )
      }
    }

    result$artifacts$plots[[section_nm]] <- section
  }

  result
}

#' Flatten nested M3 plot collections for export
#' @keywords internal
m3_flatten_plot_collection <- function(x, prefix = NULL) {
  if (inherits(x, "ggplot") || inherits(x, "recordedplot")) {
    plot_name <- if (is.null(prefix) || identical(prefix, "")) "plot" else prefix
    out <- list(x)
    names(out) <- plot_name
    return(out)
  }

  if (!is.list(x) || length(x) == 0) {
    return(list())
  }

  flattened <- list()
  child_names <- names(x)
  if (is.null(child_names)) {
    child_names <- as.character(seq_along(x))
  }

  for (i in seq_along(x)) {
    child_prefix <- if (is.null(prefix) || identical(prefix, "")) {
      child_names[i]
    } else {
      paste(prefix, child_names[i], sep = "_")
    }
    flattened <- c(flattened, m3_flatten_plot_collection(x[[i]], child_prefix))
  }

  flattened
}

#' Flatten nested M3 tables for export
#' @keywords internal
m3_flatten_table_collection <- function(x, prefix = NULL) {
  if (is.data.frame(x)) {
    table_name <- if (is.null(prefix) || identical(prefix, "")) "table" else prefix
    out <- list(x)
    names(out) <- table_name
    return(out)
  }

  if (!is.list(x) || length(x) == 0) {
    return(list())
  }

  flattened <- list()
  child_names <- names(x)
  if (is.null(child_names)) {
    child_names <- as.character(seq_along(x))
  }

  for (i in seq_along(x)) {
    child_prefix <- if (is.null(prefix) || identical(prefix, "")) {
      child_names[i]
    } else {
      paste(prefix, child_names[i], sep = "_")
    }
    flattened <- c(flattened, m3_flatten_table_collection(x[[i]], child_prefix))
  }

  flattened
}

# Null-coalescing helper (avoid rlang dependency for one symbol)
`%||%` <- function(a, b) if (!is.null(a)) a else b
