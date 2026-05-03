# ============================================================================
# m2_table_advanced_journal.R - Advanced M2 journal tables
# ============================================================================

build_m2_advanced_journal_table <- function(result, config = biblio_config()) {
  if (!is.list(result)) {
    return(list(status = "stub", table = tibble::tibble(), tables = list()))
  }

  tables <- list(
    model_uncertainty = result$model_uncertainty$table %||% m2_empty_model_uncertainty_table(),
    growth_regime_summary = result$growth_regimes$summary %||% m2_empty_growth_regime_summary(),
    growth_regime_curve = result$growth_regimes$curve %||% m2_empty_growth_regime_curve(),
    forecast_leaderboard = result$forecast_validation$leaderboard %||% m2_empty_forecast_leaderboard(),
    forecast_interval_calibration = result$forecast_validation$interval_calibration %||% m2_empty_interval_calibration(),
    forecast_fold_results = result$forecast_validation$fold_results %||% tibble::tibble(),
    ensemble_weights = result$ensemble_forecast$weights %||% tibble::tibble(),
    ensemble_forecast = result$ensemble_forecast$forecast %||% tibble::tibble(),
    bootstrap_forecast_intervals = result$bootstrap_forecast_intervals$table %||% tibble::tibble(),
    model_confidence_set = result$model_confidence_set$table %||% tibble::tibble(),
    segmented_regression = result$segmented_regression$table %||% tibble::tibble(),
    saturation_uncertainty = result$saturation_uncertainty$table %||% tibble::tibble(),
    early_warning = result$early_warning$table %||% tibble::tibble(),
    changepoint_consensus = result$changepoint_consensus$table %||% m2_empty_changepoint_consensus(),
    hypotheses = result$hypotheses$table %||% tibble::tibble()
  )

  main_table <- dplyr::bind_rows(lapply(names(tables), function(nm) {
    tbl <- tables[[nm]]
    tibble::tibble(
      table_name = nm,
      rows = if (is.data.frame(tbl)) nrow(tbl) else 0L,
      status = if (is.data.frame(tbl) && nrow(tbl) > 0) "available" else "empty"
    )
  }))

  list(
    status = result$status %||% "success",
    reason = result$reason %||% NA_character_,
    table = main_table,
    tables = tables
  )
}
