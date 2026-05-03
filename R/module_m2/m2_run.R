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
  input <- m2_normalize_input(input)

  # 1. Validate
  validation <- validate_m2_input(input, config)
  if (!validation$ok && config$validate_strict) {
    cli::cli_abort("M2 validation failed: {paste(validation$missing_columns, collapse = ', ')}")
  }

  # 2. Compute all metrics
  data <- list(
    eda         = compute_m2_eda(input, config),
    regression  = compute_m2_regression(input, config),
    ridge       = compute_m2_ridge(input, config),
    changepoint = compute_m2_changepoint(input, config),
    stl         = compute_m2_stl(input, config),
    hypotheses  = compute_m2_hypotheses(input, config),
    forecasting = compute_m2_forecasting(input, config),
    advanced_ts = compute_m2_advanced_ts(input, config),
    growth_models = compute_m2_growth_models_wrapper(input, config)
  )
  
  # 2b. Compute harmonics AFTER regression (to enable residual analysis)
  data$harmonics <- compute_m2_harmonics(input, data$regression, config)
  
  # 2c. Compute residual analysis (requires regression)
  data$residual_analysis <- compute_m2_residual_analysis(input, data$regression, config)

  # 2d. Build unified model registry and diagnostics
  data$model_registry <- m2_build_model_registry(data, input, config)
  data$diagnostics <- compute_m2_diagnostics(
    input,
    data$model_registry,
    config,
    changepoint_result = data$changepoint,
    forecasting_result = data$forecasting
  )
  data$advanced_journal <- compute_m2_advanced_journal(input, data, config)

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
    harmonics  = render_m2_harmonics(data$harmonics, config),
    residual_analysis = render_m2_residual_analysis(data$residual_analysis, config),
    forecasting = render_m2_forecasting(data$forecasting, config),
    wavelet    = if (!is.null(data$harmonics) && !is.null(data$harmonics$wavelet)) render_m2_wavelet(data$harmonics$wavelet, config) else list(plots = list()),
    advanced_ts = render_m2_advanced_ts(data$advanced_ts, config),
    growth_models = render_m2_growth_models(data$growth_models, config),
    diagnostics = render_m2_diagnostics(data$diagnostics, config),
    advanced_journal = render_m2_advanced_journal(data$advanced_journal, config)
  )
  result$artifacts$plots <- m2_fill_core_plot_placeholders(plots)

  # 5. Tables
  tables <- list(
    eda        = build_m2_eda_table(data$eda, config),
    regression = build_m2_regression_table(data$regression, config),
    forecasting = build_m2_forecasting_table(data$forecasting, config),
    growth_models = build_m2_growth_models_table(data$growth_models, config),
    hypotheses = build_m2_hypotheses_table(data$hypotheses, config),
    diagnostics = build_m2_diagnostics_table(data$diagnostics, config),
    advanced_journal = build_m2_advanced_journal_table(data$advanced_journal, config)
  )
  result$artifacts$tables <- tables

  # 5b. Report
  report <- build_m2_report(data, config)
  result <- attach_report_to_result(result, report)

  # 6. Export
  if (export) {
    exported <- export_m2(result, config)
    manifest <- build_m2_manifest(result, exported, config)
    result <- attach_manifest_to_result(result, manifest)
  }

  result
}

m2_fill_core_plot_placeholders <- function(plot_sections) {
  core_specs <- list(
    eda = list(
      title = "Annual production overview unavailable",
      message = "The annual series did not contain enough valid observations to render the descriptive time-series overview.",
      layout = "full"
    ),
    regression = list(
      title = "Interpretable growth model unavailable",
      message = "No stable interpretable regression or growth curve could be fitted from the available annual production series.",
      layout = "full"
    ),
    forecasting = list(
      title = "Forecasting evidence unavailable",
      message = "Forecast models could not be estimated or benchmarked reliably against the available annual series.",
      layout = "full"
    ),
    growth_models = list(
      title = "Parametric growth-model comparison unavailable",
      message = "The series was too short or degenerate to compare Gompertz, logistic, Richards, Weibull, MMF, and related models.",
      layout = "full"
    ),
    diagnostics = list(
      title = "Residual diagnostics unavailable",
      message = "Residual diagnostics were not estimable because no stable headline model was available.",
      layout = "full"
    ),
    harmonics = list(
      title = "Harmonic structure unavailable",
      message = "The residual or annual series was too short to estimate a meaningful frequency-domain signal.",
      layout = "full"
    ),
    residual_analysis = list(
      title = "Residual analysis unavailable",
      message = "Residuals were missing or insufficient after model fitting, so no residual structure plot was generated.",
      layout = "full"
    ),
    advanced_journal = list(
      title = "Advanced temporal journal analytics unavailable",
      message = "The optional advanced M2 layer did not have enough evidence to render model uncertainty, regime, forecast-validation, or changepoint-consensus plots.",
      layout = "full"
    )
  )

  for (section_nm in names(core_specs)) {
    section <- plot_sections[[section_nm]]
    flat <- if (is.list(section) && !is.null(section$plots)) {
      m2_flatten_plot_collection(section$plots)
    } else {
      list()
    }
    if (length(flat) == 0) {
      spec <- core_specs[[section_nm]]
      plot_sections[[section_nm]] <- list(
        status = "placeholder",
        plots = list(
          insufficient_data = ieee_no_data_plot(
            title = spec$title,
            message = spec$message,
            layout = spec$layout
          )
        )
      )
    }
  }

  plot_sections
}

#' Normalize supported M2 input column variants
#' @keywords internal
m2_normalize_input <- function(input) {
  if (!is.data.frame(input)) {
    return(input)
  }

  output <- input
  if ("PY" %in% names(output) && !"Year" %in% names(output)) {
    output$Year <- output$PY
  }
  if ("year" %in% names(output) && !"Year" %in% names(output)) {
    output$Year <- output$year
  }
  if ("article_count" %in% names(output) && !"Articles" %in% names(output)) {
    output$Articles <- output$article_count
  }
  if ("articles" %in% names(output) && !"Articles" %in% names(output)) {
    output$Articles <- output$articles
  }

  output
}

#' Collect models from computed data for diagnostics
#' @keywords internal
collect_models_for_diagnostics <- function(data) {
  models <- list()
  
  # From regression - extract all fitted models
  if (!is.null(data$regression) && data$regression$status == "success") {
    if (!is.null(data$regression$models)) {
      models <- c(models, data$regression$models)
    }
  }
  
  # From forecasting
  if (!is.null(data$forecasting) && data$forecasting$status == "success") {
    if (!is.null(data$forecasting$arima)) models$arima <- data$forecasting$arima
    if (!is.null(data$forecasting$ets)) models$ets <- data$forecasting$ets
  }
  
  # From growth models
  if (!is.null(data$growth_models) && data$growth_models$status == "success") {
    model_names <- c("bass", "gompertz", "weibull", "richards", "von_bertalanffy", "mmf")
    for (nm in model_names) {
      if (!is.null(data$growth_models[[nm]])) {
        models[[nm]] <- data$growth_models[[nm]]
      }
    }
  }
  
  models
}

#' Wrapper for growth models computation
#' @keywords internal
compute_m2_growth_models_wrapper <- function(input, config) {
  year_col <- if ("Year" %in% names(input)) "Year" else names(input)[1]
  articles_col <- if ("Articles" %in% names(input)) "Articles" else names(input)[2]
  
  years <- input[[year_col]]
  articles <- input[[articles_col]]
  
  if (length(years) < 5) {
    return(list(status = "error: insufficient data for growth models (need 5+ years)"))
  }
  
  results <- list()
  
  results$bass <- fit_bass_model(years, articles)
  results$gompertz <- fit_gompertz_model(years, articles)
  results$weibull <- fit_weibull_model(years, articles)
  results$richards <- fit_richards_model(years, articles)
  results$von_bertalanffy <- fit_von_bertalanffy_model(years, articles)
  results$mmf <- fit_mmf_model(years, articles)
  
  results$comparison <- compare_growth_models(results)
  results$best_model <- select_best_growth_model(results$comparison)
  results$status <- "success"
  
  results
}

#' Select the best fitted growth model from a comparison table
#' @keywords internal
select_best_growth_model <- function(comparison) {
  if (!is.data.frame(comparison) || nrow(comparison) == 0 || !"model" %in% names(comparison)) {
    return(NA_character_)
  }

  ranking_col <- if ("AIC" %in% names(comparison)) "AIC" else if ("R_squared" %in% names(comparison)) "R_squared" else NULL
  if (is.null(ranking_col)) {
    return(comparison$model[1])
  }

  values <- suppressWarnings(as.numeric(comparison[[ranking_col]]))
  if (ranking_col == "R_squared") {
    idx <- which.max(values)
  } else {
    idx <- which.min(values)
  }

  comparison$model[idx][1]
}

#' Export M2 artifacts
#' @export
export_m2 <- function(result, config = biblio_config()) {
  config <- merge_biblio_config(config)

  exported_plots <- character()
  exported_jsons <- character()
  exported_tables <- character()

  if (config$export_plots) {
    for (nm in names(result$artifacts$plots)) {
      plot_section <- result$artifacts$plots[[nm]]
      flat_plots <- m2_flatten_plot_collection(plot_section$plots)
      for (pnm in names(flat_plots)) {
        plot_obj <- ieee_prepare_plot_for_export(
          flat_plots[[pnm]],
          module_id = "m2",
          section_id = nm,
          plot_id = pnm,
          config = config
        )
        spec <- ieee_get_plot_export_spec(
          plot_obj,
          config = config,
          section_id = nm,
          plot_id = pnm
        )
        p <- build_artifact_path("m2", "plots", paste0("m2_", nm, "_", pnm), "png", config)
        tryCatch({
          exported_paths <- export_plot_artifact(plot_obj, tools::file_path_sans_ext(p),
                               width = spec$width, height = spec$height, dpi = spec$dpi)
          exported_plots <- c(exported_plots, unname(exported_paths[!is.na(exported_paths)]))
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

  for (nm in names(result$artifacts$tables)) {
    table_section <- result$artifacts$tables[[nm]]
    flat_tables <- m2_flatten_table_collection(table_section)
    if (length(flat_tables) == 0) {
      next
    }
    for (tnm in names(flat_tables)) {
      csv_path <- build_artifact_path("m2", "tables", paste0("m2_", nm, "_", tnm), "csv", config)
      dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
      tryCatch({
        utils::write.csv(flat_tables[[tnm]], csv_path, row.names = FALSE, na = "")
        exported_tables <- c(exported_tables, csv_path)
      }, error = function(e) NULL)
    }
  }

  exported_reports <- character()
  if (config$export_reports && length(result$artifacts$reports) > 0) {
    report <- result$artifacts$reports[[1]]
    if (!is.null(report$lines) && length(report$lines) > 0) {
      r <- build_artifact_path("m2", "reports", "m2_report", "txt", config)
      write_text_report(report$lines, r)
      exported_reports <- c(exported_reports, r)
    }
    if (!is.null(report$tex) && length(report$tex) > 0) {
      t <- build_artifact_path("m2", "reports", "m2_report", "tex", config)
      writeLines(report$tex, t)
      exported_reports <- c(exported_reports, t)
    }
  }

  list(plots = exported_plots, tables = exported_tables, reports = exported_reports, files = exported_jsons)
}

#' Flatten nested plot collections for export
#' @keywords internal
m2_flatten_plot_collection <- function(x, prefix = NULL) {
  if (inherits(x, "ggplot")) {
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
    flattened <- c(flattened, m2_flatten_plot_collection(x[[i]], child_prefix))
  }

  flattened
}

#' Flatten nested table collections for export
#' @keywords internal
m2_flatten_table_collection <- function(x, prefix = NULL) {
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
    flattened <- c(flattened, m2_flatten_table_collection(x[[i]], child_prefix))
  }

  flattened
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

#' Build M2 report payload
#' @keywords internal
build_m2_report <- function(data, config = biblio_config()) {
  best_reg <- data$regression$best_model
  benchmark_reg <- data$regression$benchmark_best_model
  benchmark_name <- if (is.list(benchmark_reg) && !is.null(benchmark_reg$name)) benchmark_reg$name else NULL
  best_name <- if (is.list(best_reg) && !is.null(best_reg$name)) best_reg$name else "NA"
  best_forecast <- if (!is.null(data$forecasting$model_comparison$best_model)) {
    data$forecasting$model_comparison$best_model
  } else {
    NA_character_
  }
  trend_stats <- data$diagnostics$trend_statistics
  changepoints <- if (!is.null(data$changepoint$summary$changepoint_years)) data$changepoint$summary$changepoint_years else numeric(0)
  param <- best_reg$parameter_summary %||% list()
  hyp <- data$hypotheses$hypotheses %||% list()
  hyp_rows <- Filter(function(x) is.list(x), hyp)
  hyp_statistical <- hyp_rows[vapply(hyp_rows, function(x) identical(x$evidence_class %||% "", "statistical"), logical(1))]
  hyp_supported <- hyp_statistical[vapply(hyp_statistical, function(x) identical(x$result %||% "", "reject"), logical(1))]
  hyp_summary <- if (length(hyp_statistical) > 0) {
    sprintf("%d/%d statistical hypotheses showed evidence against their null.", length(hyp_supported), length(hyp_statistical))
  } else {
    "No statistical hypotheses were available."
  }
  top_hypothesis <- if (length(hyp_supported) > 0) {
    p_values <- vapply(hyp_supported, function(x) {
      value <- suppressWarnings(as.numeric(x$p_value))
      if (length(value) == 1L && is.finite(value)) value else Inf
    }, numeric(1))
    hyp_supported[[which.min(p_values)]]
  } else {
    NULL
  }

  parameter_lines <- character()
  if (is.finite(m2_scalar_num(param$carrying_capacity))) {
    parameter_lines <- c(parameter_lines, sprintf("Estimated saturation/capacity: %.2f", m2_scalar_num(param$carrying_capacity)))
  }
  if (is.finite(m2_scalar_num(param$growth_rate))) {
    parameter_lines <- c(parameter_lines, sprintf("Estimated growth-rate parameter: %.4f", m2_scalar_num(param$growth_rate)))
  }
  if (is.finite(m2_scalar_num(param$inflection_year))) {
    parameter_lines <- c(parameter_lines, sprintf("Estimated inflection year: %.2f", m2_scalar_num(param$inflection_year)))
  }

  lines <- c(
    "M2 Annual Production Analysis",
    "",
    sprintf("Selected interpretable regression model: %s", best_name),
    if (!is.null(benchmark_name) && !identical(benchmark_name, best_name)) {
      sprintf("Flexible benchmark winner: %s", benchmark_name)
    } else {
      NULL
    },
    sprintf("Composite selection score: %.4f", m2_scalar_num(best_reg$composite_score)),
    if (!is.null(best_reg$selection_reason)) paste("Selection rationale:", best_reg$selection_reason) else NULL,
    sprintf("Best forecasting model: %s", if (!is.null(best_forecast)) best_forecast else "NA"),
    sprintf("Estimated CAGR: %.2f%%", 100 * m2_scalar_num(trend_stats$cagr)),
    sprintf("Recent CAGR: %.2f%%", 100 * m2_scalar_num(trend_stats$recent_cagr)),
    sprintf("Sen slope: %.4f", m2_scalar_num(trend_stats$sen_slope)),
    sprintf("Mann-Kendall p-value: %.4f", m2_scalar_num(trend_stats$mann_kendall$p_value)),
    sprintf("Hurst exponent: %.4f", m2_scalar_num(trend_stats$hurst_exponent)),
    sprintf("Detected breakpoints: %s", if (length(changepoints) > 0) paste(changepoints, collapse = ", ") else "None"),
    paste("Hypothesis summary:", hyp_summary),
    "",
    m2_advanced_report_lines(data$advanced_journal %||% list()),
    if (!is.null(top_hypothesis)) {
      paste("Strongest statistical finding:", top_hypothesis$interpretation %||% top_hypothesis$hypothesis)
    } else {
      NULL
    },
    parameter_lines
  )

  tex <- c(
    "\\section*{M2 Annual Production Summary}",
    sprintf("\\textbf{Selected interpretable regression model}: %s\\\\", best_name),
    if (!is.null(benchmark_name) && !identical(benchmark_name, best_name)) {
      sprintf("\\textbf{Flexible benchmark winner}: %s\\\\", benchmark_name)
    } else {
      NULL
    },
    sprintf("\\textbf{Composite selection score}: %.4f\\\\", m2_scalar_num(best_reg$composite_score)),
    if (!is.null(best_reg$selection_reason)) sprintf("\\textbf{Selection rationale}: %s\\\\", best_reg$selection_reason) else NULL,
    sprintf("\\textbf{Best forecasting model}: %s\\\\", if (!is.null(best_forecast)) best_forecast else "NA"),
    sprintf("\\textbf{Estimated CAGR}: %.2f\\%%\\\\", 100 * m2_scalar_num(trend_stats$cagr)),
    sprintf("\\textbf{Recent CAGR}: %.2f\\%%\\\\", 100 * m2_scalar_num(trend_stats$recent_cagr)),
    sprintf("\\textbf{Sen slope}: %.4f\\\\", m2_scalar_num(trend_stats$sen_slope)),
    sprintf("\\textbf{Mann-Kendall p-value}: %.4f\\\\", m2_scalar_num(trend_stats$mann_kendall$p_value)),
    sprintf("\\textbf{Hurst exponent}: %.4f\\\\", m2_scalar_num(trend_stats$hurst_exponent)),
    sprintf("\\textbf{Detected breakpoints}: %s\\\\", if (length(changepoints) > 0) paste(changepoints, collapse = ", ") else "None"),
    sprintf("\\textbf{Hypothesis summary}: %s\\\\", hyp_summary),
    m2_advanced_report_tex(data$advanced_journal %||% list()),
    if (!is.null(top_hypothesis)) sprintf("\\textbf{Strongest statistical finding}: %s\\\\", top_hypothesis$interpretation %||% top_hypothesis$hypothesis) else NULL,
    if (length(parameter_lines) > 0) sprintf("\\textbf{Model parameters}: %s", paste(parameter_lines, collapse = "; ")) else NULL
  )

  list(lines = lines, tex = tex)
}
