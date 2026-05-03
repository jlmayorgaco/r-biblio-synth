# ============================================================================
# m4_run.R - M4 Sources / Journals & Venues Orchestrator
# ============================================================================

#' Run M4 module (Sources / Journals & Venues)
#'
#' Analyzes journal and venue productivity, citation impact, Bradford zones,
#' source growth, concentration, clustering, and narrative evidence.
#'
#' @param input Bibliographic data frame with at least a source column (`SO`,
#'   `Source`, `Journal`, or `Publication.Source`).
#' @param config Configuration list.
#' @param export Logical. If TRUE, exports artifacts to disk.
#' @return A `biblio_module_result`.
#' @export
run_m4 <- function(input, config = biblio_config(), export = TRUE) {
  config <- merge_biblio_config(config)

  validation <- validate_m4_input(input, config)
  if (!validation$ok && isTRUE(config$validate_strict)) {
    cli::cli_abort("M4 validation failed: {validation$error}")
  }
  if (!validation$ok) {
    cli::cli_warn("M4 validation failed: {validation$error}")
  }

  prepared <- m4_prepare_source_data(input, config)
  data <- m4_compute_all(prepared, config)

  result <- new_module_result(
    module_id = "m4",
    module_name = "Sources / Journals & Venues",
    status = if (validation$ok && identical(prepared$status, "success")) "success" else "warning",
    inputs = list(
      n_rows = validation$n_rows %||% if (is.data.frame(input)) nrow(input) else 0L,
      n_cols = validation$n_cols %||% if (is.data.frame(input)) ncol(input) else 0L,
      n_sources = nrow(prepared$source_summary %||% data.frame())
    ),
    data = data,
    diagnostics = list(
      warnings = c(validation$warning %||% character(), prepared$warning %||% character()),
      checks = list(validation = validation, preparation = prepared$status),
      notes = character()
    )
  )

  result <- m4_render_all(result, data, config)
  result <- m4_build_tables(result, data, config)
  report <- build_m4_report(result, config)
  result <- attach_report_to_result(result, report)

  if (export) {
    exported <- export_m4(result, config)
    manifest <- build_m4_manifest(result, exported, config)
    result <- attach_manifest_to_result(result, manifest)
  }

  result
}

validate_m4_input <- function(input, config = biblio_config()) {
  if (!is.data.frame(input)) {
    return(list(ok = FALSE, error = "Input must be a data frame", n_rows = 0L, n_cols = 0L))
  }
  if (nrow(input) == 0) {
    return(list(ok = FALSE, error = "Input must contain at least one record", n_rows = 0L, n_cols = ncol(input)))
  }
  source_col <- m4_select_source_column(input)
  if (is.null(source_col)) {
    return(list(
      ok = FALSE,
      error = "No source column found. Expected one of SO, Source, Journal, Publication.Source, JI, or J9.",
      n_rows = nrow(input),
      n_cols = ncol(input)
    ))
  }
  list(ok = TRUE, source_col = source_col, n_rows = nrow(input), n_cols = ncol(input))
}

m4_compute_all <- function(prepared, config) {
  data <- list(
    prepared = prepared,
    sources = m4_compute_sources(prepared, config),
    impact = m4_compute_source_impact(prepared, config),
    bradford = m4_compute_bradford(prepared, config),
    growth = m4_compute_source_growth(prepared, config),
    concentration = m4_compute_source_concentration(prepared, config),
    keywords = m4_compute_source_keywords(prepared, config)
  )
  data$similarity <- m4_compute_source_similarity(prepared, config)
  data$specialization <- m4_compute_source_specialization(prepared, data$similarity, config)
  data$lifecycle <- m4_compute_source_lifecycle(prepared, data$growth, config)
  data$clusters <- m4_compute_source_clusters(data, config)
  data$advanced_analytics <- m4_compute_advanced_analytics(data, config)
  data$narrative <- m4_compute_narrative(data, config)
  data
}

m4_render_all <- function(result, data, config) {
  result$artifacts$plots <- list(
    sources = render_m4_sources(data, config),
    bradford = render_m4_bradford(data$bradford, data$impact, config),
    growth = render_m4_growth(data$growth, data$impact, config),
    similarity = render_m4_similarity(data$similarity, config),
    specialization = render_m4_specialization(data$specialization, config),
    lifecycle = render_m4_lifecycle(data$lifecycle, config),
    clusters = render_m4_clusters(data$clusters, data$impact, config),
    advanced_analytics = render_m4_advanced_analytics(data$advanced_analytics, config),
    narrative = render_m4_narrative(data$narrative, config)
  )
  result$artifacts$plots <- m4_fill_plot_placeholders(result$artifacts$plots)
  result
}

m4_fill_plot_placeholders <- function(plot_sections) {
  core_specs <- list(
    sources = list(title = "Source productivity unavailable", message = "No valid journal or venue source data were available.", layout = "full"),
    bradford = list(title = "Bradford source zones unavailable", message = "The source distribution was too sparse for Bradford zoning.", layout = "full"),
    growth = list(title = "Source growth unavailable", message = "Source-year data were insufficient for growth and emergence plots.", layout = "full"),
    similarity = list(title = "Source similarity unavailable", message = "Keyword similarity between sources could not be estimated.", layout = "full"),
    specialization = list(title = "Source specialization unavailable", message = "Specialist-generalist source profiles could not be estimated.", layout = "full"),
    lifecycle = list(title = "Source lifecycle unavailable", message = "Venue lifecycle stages and trajectory forecasts could not be estimated.", layout = "full"),
    clusters = list(title = "Source clustering unavailable", message = "K-means source clusters could not be estimated from productivity, impact, and growth features.", layout = "full"),
    advanced_analytics = list(title = "Advanced source analytics unavailable", message = "ML, anomaly, archetype, or regression evidence could not be estimated from the source feature table.", layout = "full"),
    narrative = list(title = "Source narrative evidence unavailable", message = "Source evidence metrics could not be normalized into a narrative dashboard.", layout = "full")
  )

  for (section_nm in names(core_specs)) {
    section <- plot_sections[[section_nm]]
    flat <- if (is.list(section) && !is.null(section$plots)) m4_flatten_plot_collection(section$plots) else list()
    if (length(Filter(Negate(is.null), flat)) == 0) {
      spec <- core_specs[[section_nm]]
      plot_sections[[section_nm]] <- list(
        status = "placeholder",
        plots = list(insufficient_data = ieee_no_data_plot(spec$title, spec$message, layout = spec$layout))
      )
    }
  }
  plot_sections
}

m4_build_tables <- function(result, data, config) {
  result$artifacts$tables <- list(
    sources = build_m4_sources_table(data$sources, config),
    impact = build_m4_impact_table(data$impact, config),
    bradford = build_m4_bradford_table(data$bradford, config),
    growth = build_m4_growth_table(data$growth, config),
    concentration = build_m4_concentration_table(data$concentration, config),
    keywords = build_m4_keywords_table(data$keywords, config),
    similarity = build_m4_similarity_table(data$similarity, config),
    specialization = build_m4_specialization_table(data$specialization, config),
    lifecycle = build_m4_lifecycle_table(data$lifecycle, config),
    clusters = build_m4_clusters_table(data$clusters, config),
    advanced_analytics = build_m4_advanced_analytics_table(data$advanced_analytics, config),
    narrative = build_m4_narrative_table(data$narrative, config)
  )
  result
}

export_m4 <- function(result, config = biblio_config()) {
  config <- merge_biblio_config(config)
  exported_plots <- character()
  exported_tables <- character()
  exported_reports <- character()
  exported_jsons <- character()

  if (config$export_plots) {
    for (section_nm in names(result$artifacts$plots)) {
      section <- result$artifacts$plots[[section_nm]]
      if (!is.list(section) || !is.list(section$plots)) next
      flat_plots <- m4_flatten_plot_collection(section$plots)
      for (plot_nm in names(flat_plots)) {
        plot_obj <- ieee_prepare_plot_for_export(
          flat_plots[[plot_nm]],
          module_id = "m4",
          section_id = section_nm,
          plot_id = plot_nm,
          config = config
        )
        if (!inherits(plot_obj, "ggplot") && !inherits(plot_obj, "recordedplot")) next
        spec <- ieee_get_plot_export_spec(plot_obj, config, section_id = section_nm, plot_id = plot_nm)
        base <- build_artifact_path(config, "m4", "plots", paste(section_nm, plot_nm, sep = "_"))
        exported_plots <- c(exported_plots, export_plot_artifact(plot_obj, base, width = spec$width, height = spec$height, dpi = spec$dpi))
      }
    }
  }

  if (config$export_tables) {
    for (table_nm in names(result$artifacts$tables)) {
      tables <- m4_flatten_table_collection(result$artifacts$tables[[table_nm]])
      for (sub_nm in names(tables)) {
        path <- build_artifact_path(config, "m4", "tables", paste(table_nm, sub_nm, sep = "_"), ext = "json")
        write_json_artifact(tables[[sub_nm]], path)
        exported_tables <- c(exported_tables, path)
      }
    }
  }

  if (config$export_json) {
    path <- build_artifact_path(config, "m4", "data", "m4_result_data", ext = "json")
    write_json_artifact(result$data, path)
    exported_jsons <- c(exported_jsons, path)
  }

  if (config$export_reports && length(result$artifacts$reports) > 0) {
    path <- build_artifact_path(config, "m4", "reports", "m4_report", ext = "txt")
    write_text_report(result$artifacts$reports[[1]]$lines %||% result$artifacts$reports[[1]], path)
    exported_reports <- c(exported_reports, path)
  }

  list(plots = exported_plots, tables = exported_tables, reports = exported_reports, files = exported_jsons)
}

build_m4_manifest <- function(result, exported = list(), config = biblio_config()) {
  new_artifact_manifest(
    module_id = "m4",
    generated_at = Sys.time(),
    files = exported$files %||% character(),
    plots = exported$plots %||% character(),
    tables = exported$tables %||% character(),
    reports = exported$reports %||% character(),
    status = result$status %||% "unknown"
  )
}

m4_flatten_plot_collection <- function(x) {
  if (is.null(x)) return(list())
  if (inherits(x, "ggplot") || inherits(x, "recordedplot")) return(list(plot = x))
  if (!is.list(x)) return(list())
  out <- list()
  for (nm in names(x)) {
    item <- x[[nm]]
    if (inherits(item, "ggplot") || inherits(item, "recordedplot")) {
      out[[nm]] <- item
    } else if (is.list(item)) {
      nested <- m4_flatten_plot_collection(item)
      for (sub_nm in names(nested)) out[[paste(nm, sub_nm, sep = "_")]] <- nested[[sub_nm]]
    }
  }
  out
}

m4_flatten_table_collection <- function(x) {
  if (is.null(x)) return(list())
  if (is.data.frame(x)) return(list(table = x))
  if (is.list(x) && is.data.frame(x$table)) return(list(table = x$table))
  if (!is.list(x)) return(list())
  out <- list()
  for (nm in names(x)) {
    item <- x[[nm]]
    if (is.data.frame(item)) out[[nm]] <- item
    else if (is.list(item) && is.data.frame(item$table)) out[[nm]] <- item$table
  }
  out
}
