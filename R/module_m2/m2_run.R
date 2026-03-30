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
    eda         = compute_m2_eda(input, config),
    regression  = compute_m2_regression(input, config),
    harmonics   = compute_m2_harmonics(input, config),
    ridge       = compute_m2_ridge(input, config),
    changepoint = compute_m2_changepoint(input, config),
    stl         = compute_m2_stl(input, config),
    hypotheses  = compute_m2_hypotheses(input, config),
    forecasting = compute_m2_forecasting(input, config),
    advanced_ts = compute_m2_advanced_ts(input, config),
    growth_models = compute_m2_growth_models_wrapper(input, config)
  )
  
  # 2b. Compute residual analysis (requires regression)
  data$residual_analysis <- compute_m2_residual_analysis(input, data$regression, config)
  
  # 2c. Harmonics already computed in step 2, includes wavelet analysis if available

  # 2d. Compute diagnostics (requires fitted models)
  models_for_diagnostics <- collect_models_for_diagnostics(data)
  data$diagnostics <- compute_m2_diagnostics(input, models_for_diagnostics, config)

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
    diagnostics = render_m2_diagnostics(data$diagnostics, config)
  )
  result$artifacts$plots <- plots

  # 5. Tables
  tables <- list(
    eda        = build_m2_eda_table(data$eda, config),
    regression = build_m2_regression_table(data$regression, config),
    forecasting = build_m2_forecasting_table(data$forecasting, config),
    growth_models = build_m2_growth_models_table(data$growth_models, config)
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
