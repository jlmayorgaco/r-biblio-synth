# ============================================================================
# zz_m2_journal_overrides.R - Journal-grade overrides for M2
# ============================================================================

m2_compute_hypotheses_legacy <- compute_m2_hypotheses
build_m2_regression_table_legacy <- build_m2_regression_table
build_m2_forecasting_table_legacy <- build_m2_forecasting_table
build_m2_hypotheses_table_legacy <- build_m2_hypotheses_table
compute_m2_forecasting_legacy <- compute_m2_forecasting
compare_forecast_models_legacy <- compare_forecast_models
build_m2_report_legacy <- build_m2_report
export_m2_legacy <- export_m2
run_m2_legacy <- run_m2
create_forecast_plot_legacy <- create_forecast_plot
create_forecast_model_comparison_plot_legacy <- create_forecast_model_comparison_plot

m2_ensure_regression_comparison_columns <- function(comparison) {
  if (!is.data.frame(comparison)) {
    comparison <- data.frame(stringsAsFactors = FALSE)
  }

  n <- nrow(comparison)
  defaults <- list(
    Model = rep(NA_character_, n),
    Rank = if (n > 0) seq_len(n) else numeric(0),
    NarrativeRank = if (n > 0) seq_len(n) else numeric(0),
    CompositeScore = rep(NA_real_, n),
    NarrativeScore = rep(NA_real_, n),
    RMSE = rep(NA_real_, n),
    Adj_R2 = rep(NA_real_, n),
    AIC = rep(NA_real_, n),
    SaturationCapable = rep(FALSE, n),
    HeadlineEligible = rep(FALSE, n),
    BenchmarkOnly = rep(FALSE, n)
  )

  for (nm in names(defaults)) {
    if (!nm %in% names(comparison)) {
      comparison[[nm]] <- defaults[[nm]]
    }
  }

  if (n > 0) {
    if (all(is.na(comparison$Rank))) {
      comparison$Rank <- seq_len(n)
    }
    if (all(is.na(comparison$NarrativeRank))) {
      comparison$NarrativeRank <- comparison$Rank
    }
    if (all(is.na(comparison$Model))) {
      comparison$Model <- paste0("model_", seq_len(n))
    }
  }

  comparison$SaturationCapable <- as.logical(comparison$SaturationCapable %||% FALSE)
  comparison$HeadlineEligible <- as.logical(comparison$HeadlineEligible %||% FALSE)
  comparison$BenchmarkOnly <- as.logical(comparison$BenchmarkOnly %||% FALSE)
  comparison
}

compute_m2_forecasting <- function(input, config = biblio_config()) {
  result <- compute_m2_forecasting_legacy(input, config)
  if (!inherits(result, "list") || !identical(result$status, "success")) {
    return(result)
  }

  years <- m2_num_vec(result$years %||% if ("Year" %in% names(input)) input$Year else numeric(0))
  articles <- m2_num_vec(result$articles %||% if ("Articles" %in% names(input)) input$Articles else numeric(0))
  enhanced_cv <- m2_enhanced_time_series_validation(years, articles, config)
  if (identical(enhanced_cv$status, "success")) {
    result$cv_results <- enhanced_cv
    result$model_comparison <- compare_forecast_models(result$arima, result$ets, result$naive, result$cv_results)
    result$interval_validation <- enhanced_cv$interval_coverage
    result$ensemble$cv_metrics <- enhanced_cv$cv_aggregated[enhanced_cv$cv_aggregated$model == "Ensemble", , drop = FALSE]
  }

  result
}

#' Run M2 module (Annual Production Analysis)
#'
#' Journal-grade override that computes contextual hypotheses after the
#' regression, forecasting, and diagnostics layers are available.
#'
#' @param input A data frame with Year and Articles columns.
#' @param config A configuration list.
#' @param export Logical. If TRUE, exports artifacts to disk.
#' @export
run_m2 <- function(input, config = biblio_config(), export = TRUE) {
  config <- merge_biblio_config(config)
  input <- m2_normalize_input(input)

  validation <- validate_m2_input(input, config)
  if (!validation$ok && config$validate_strict) {
    cli::cli_abort("M2 validation failed: {paste(validation$missing_columns, collapse = ', ')}")
  }

  data <- list(
    eda = compute_m2_eda(input, config),
    regression = compute_m2_regression(input, config),
    ridge = compute_m2_ridge(input, config),
    changepoint = compute_m2_changepoint(input, config),
    stl = compute_m2_stl(input, config),
    forecasting = compute_m2_forecasting(input, config),
    advanced_ts = compute_m2_advanced_ts(input, config),
    growth_models = compute_m2_growth_models_wrapper(input, config)
  )

  data$harmonics <- compute_m2_harmonics(input, data$regression, config)
  data$residual_analysis <- compute_m2_residual_analysis(input, data$regression, config)
  data$model_registry <- m2_build_model_registry(data, input, config)
  data$diagnostics <- compute_m2_diagnostics(
    input,
    data$model_registry,
    config,
    changepoint_result = data$changepoint,
    forecasting_result = data$forecasting
  )
  data$advanced_journal <- compute_m2_advanced_journal(input, data, config)
  data$narrative <- compute_m2_narrative(input, data, config)
  data$hypotheses <- suppressWarnings(compute_m2_hypotheses(
    input,
    config,
    context = list(
      regression = data$regression,
      forecasting = data$forecasting,
      diagnostics = data$diagnostics,
      changepoint = data$changepoint
    )
  ))

  result <- new_module_result(
    module_id = "m2",
    module_name = "Annual Production",
    status = if (validation$ok) "success" else "warning",
    inputs = list(n_rows = validation$n_rows, n_cols = validation$n_cols),
    data = data,
    diagnostics = list(
      warnings = character(),
      checks = list(validation = validation),
      notes = character()
    )
  )

  plots <- list(
    eda = render_m2_eda(data$eda, config),
    regression = render_m2_regression(data$regression, config),
    harmonics = render_m2_harmonics(data$harmonics, config),
    residual_analysis = render_m2_residual_analysis(data$residual_analysis, config),
    forecasting = render_m2_forecasting(data$forecasting, config),
    wavelet = if (!is.null(data$harmonics) && !is.null(data$harmonics$wavelet)) {
      render_m2_wavelet(data$harmonics$wavelet, config)
    } else {
      list(plots = list())
    },
    advanced_ts = render_m2_advanced_ts(data$advanced_ts, config),
    growth_models = render_m2_growth_models(data$growth_models, config),
    diagnostics = render_m2_diagnostics(data$diagnostics, config),
    advanced_journal = render_m2_advanced_journal(data$advanced_journal, config),
    narrative = render_m2_narrative(data$narrative, config)
  )
  result$artifacts$plots <- m2_fill_core_plot_placeholders(plots)

  tables <- list(
    eda = build_m2_eda_table(data$eda, config),
    regression = build_m2_regression_table(data$regression, config),
    forecasting = build_m2_forecasting_table(data$forecasting, config),
    growth_models = build_m2_growth_models_table(data$growth_models, config),
    hypotheses = build_m2_hypotheses_table(data$hypotheses, config),
    diagnostics = build_m2_diagnostics_table(data$diagnostics, config),
    advanced_journal = build_m2_advanced_journal_table(data$advanced_journal, config),
    narrative = build_m2_narrative_table(data$narrative, config)
  )
  result$artifacts$tables <- tables

  report <- build_m2_report(data, config)
  result <- attach_report_to_result(result, report)

  if (export) {
    exported <- export_m2(result, config)
    manifest <- build_m2_manifest(result, exported, config)
    result <- attach_manifest_to_result(result, manifest)
  }

  result
}

#' Compute hypothesis tests for M2 with a journal-grade contract
#'
#' @param input Time series data (Year, Articles)
#' @param config Configuration list
#' @param context Optional computed context from M2.
#' @return List with hypothesis results and curated summary.
#' @export
compute_m2_hypotheses <- function(input, config = biblio_config(), context = list()) {
  base <- m2_compute_hypotheses_legacy(input, config)
  hypotheses <- base$hypotheses %||% list()
  hypotheses <- c(hypotheses, m2_contextual_hypotheses(input, config, context))
  curated <- m2_curate_hypothesis_contract(hypotheses)
  summary <- m2_summarize_curated_hypotheses(curated)

  list(
    hypotheses = curated,
    hyphypotheses = curated,
    n_hypotheses = length(curated),
    n_supported = summary$n_supported,
    n_not_supported = summary$n_not_supported,
    n_inconclusive = summary$n_inconclusive,
    summary = summary,
    table = dplyr::bind_rows(lapply(curated, as.data.frame.list, stringsAsFactors = FALSE)),
    status = "success"
  )
}

m2_contextual_hypotheses <- function(input = NULL, config = biblio_config(), context = list()) {
  series_payload <- m2_build_series_payload(input)
  break_info <- m2_detect_dominant_break(context$changepoint %||% list(), series_payload$years)
  bootstrap_profile <- m2_bootstrap_headline_profile(input, context$regression %||% list(), config)
  driver_payload <- m2_extract_external_driver(input, config)
  cluster_payload <- m2_extract_cluster_payload(input, config)
  scenario_payload <- m2_extract_scenario_payload(input, config)

  out <- list()
  out$H02_13 <- m2_test_forecast_superiority(context$forecasting %||% list())
  out$H02_14 <- m2_test_interpretable_headline_competitiveness(context$regression %||% list())
  out$H02_15 <- m2_test_consensus_structural_break(context$changepoint %||% list(), break_info)
  out$H02_16 <- m2_test_post_break_slope_change(series_payload, break_info)
  out$H02_17 <- m2_test_saturating_model_superiority(context$regression %||% list(), config)
  out$H02_18 <- m2_test_carrying_capacity_identifiability(series_payload, context$regression %||% list(), bootstrap_profile)
  out$H02_19 <- m2_test_diffusion_asymmetry(context$regression %||% list(), config)
  out$H02_20 <- m2_test_long_memory_persistence(context$diagnostics %||% list(), length(series_payload$articles))
  out$H02_21 <- m2_test_trend_stationarity(series_payload)
  out$H02_22 <- m2_test_variance_regime_shift(series_payload, break_info)
  out$H02_23 <- m2_test_early_warning_signals(series_payload, break_info)
  out$H02_24 <- m2_test_model_selection_stability(context$regression %||% list(), bootstrap_profile)
  out$H02_25 <- m2_test_ensemble_superiority(context$forecasting %||% list())
  out$H02_26 <- m2_test_forecast_interval_calibration(context$forecasting %||% list())
  out$H02_27 <- m2_test_cointegration_with_driver(series_payload, driver_payload)
  out$H02_28 <- m2_test_granger_with_driver(series_payload, driver_payload)
  out$H02_29 <- m2_test_interrupted_time_series(series_payload, config)
  out$H02_30 <- m2_test_cluster_trajectory_heterogeneity(cluster_payload)
  out$H02_31 <- m2_test_cluster_divergence(cluster_payload)
  out$H02_32 <- m2_test_cluster_lifecycle_differential(cluster_payload)
  out$H02_33 <- m2_test_hierarchical_forecast_gain(cluster_payload, config)
  out$H02_34 <- m2_test_source_sensitivity_robustness(scenario_payload)
  out$H02_35 <- m2_test_multiple_growth_regimes(series_payload, context$changepoint %||% list(), break_info)
  out$H02_36 <- m2_test_forecast_probability_of_superiority(context$forecasting %||% list())
  out
}

m2_curate_hypothesis_contract <- function(hypotheses) {
  ids <- names(hypotheses)
  ids <- ids %||% paste0("H", seq_along(hypotheses))
  curated <- Map(m2_curate_single_hypothesis, ids, hypotheses)
  names(curated) <- ids

  statistical_ids <- ids[vapply(curated, function(x) identical(x$evidence_class, "statistical") && is.finite(x$p_value), logical(1))]
  if (length(statistical_ids) > 0) {
    padj <- stats::p.adjust(vapply(curated[statistical_ids], `[[`, numeric(1), "p_value"), method = "BH")
    for (i in seq_along(statistical_ids)) {
      curated[[statistical_ids[i]]]$p_adjusted <- padj[i]
    }
  }

  for (id in ids) {
    curated[[id]]$reporting_scope <- m2_hypothesis_reporting_scope(
      id,
      decision = curated[[id]]$decision,
      evidence_class = curated[[id]]$evidence_class
    )
    curated[[id]]$confidence_interval <- m2_format_confidence_interval(curated[[id]]$ci_lower, curated[[id]]$ci_upper)
    curated[[id]]$plain_language_interpretation <- curated[[id]]$plain_language_interpretation %||%
      curated[[id]]$interpretation %||%
      "No interpretation available."
  }

  curated
}

m2_curate_single_hypothesis <- function(id, h) {
  if (!is.list(h)) {
    return(m2_curated_inconclusive_hypothesis(id, "Invalid hypothesis payload."))
  }

  spec <- m2_hypothesis_spec(id)
  p_value <- m2_extract_first_numeric(h$p_value, h$P_value)
  effect_size <- m2_extract_first_numeric(
    h$effect_size,
    h$sen_slope,
    h$kendall_tau,
    h$R_squared,
    h$Adj_R_squared,
    h$R2_exponential - h$R2_linear,
    h$quadratic_coefficient,
    h$delta_mae,
    h$relative_improvement_mae
  )
  if (!is.finite(effect_size) && identical(id, "H02_11")) {
    effect_size <- m2_extract_kendall_from_text(h$interpretation %||% "")
  }
  ci_lower <- m2_extract_first_numeric(h$ci_lower, h$ci_l, h$lower)
  ci_upper <- m2_extract_first_numeric(h$ci_upper, h$ci_u, h$upper)
  if (!is.finite(ci_lower) || !is.finite(ci_upper)) {
    ci_lower <- NA_real_
    ci_upper <- NA_real_
  }

  decision <- m2_hypothesis_decision_from_raw(id, h)
  list(
    hypothesis_id = id,
    question = spec$question,
    test = h$test %||% spec$test %||% "heuristic diagnostic",
    evidence_class = h$evidence_class %||% if (is.finite(p_value)) "statistical" else "heuristic",
    raw_result = h$result %||% "inconclusive",
    decision = decision,
    effect_size = effect_size,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    confidence_interval = NA_character_,
    p_value = if (is.finite(p_value)) p_value else NA_real_,
    p_adjusted = NA_real_,
    statistic = m2_extract_first_numeric(
      h$statistic,
      h$W_statistic,
      h$BP_statistic,
      h$t_statistic,
      h$F_statistic
    ),
    plain_language_interpretation = h$interpretation %||% spec$default_interpretation,
    interpretation = h$interpretation %||% spec$default_interpretation,
    null = h$null %||% spec$null,
    claim_supported_when = spec$supported_when,
    label = h$hypothesis %||% spec$question
  )
}

m2_extract_kendall_from_text <- function(text) {
  if (!is.character(text) || length(text) == 0L || is.na(text[1])) {
    return(NA_real_)
  }
  match <- regexec("Kendall tau\\s*=\\s*([-+]?[0-9]*\\.?[0-9]+)", text[1], perl = TRUE)
  parts <- regmatches(text[1], match)[[1]]
  if (length(parts) < 2L) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(parts[2]))
}

m2_curated_inconclusive_hypothesis <- function(id, message) {
  spec <- m2_hypothesis_spec(id)
  list(
    hypothesis_id = id,
    question = spec$question,
    test = spec$test %||% "heuristic diagnostic",
    evidence_class = "heuristic",
    raw_result = "inconclusive",
    decision = "inconclusive",
    effect_size = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    confidence_interval = NA_character_,
    p_value = NA_real_,
    p_adjusted = NA_real_,
    statistic = NA_real_,
    plain_language_interpretation = message,
    interpretation = message,
    null = spec$null,
    claim_supported_when = spec$supported_when,
    label = spec$question
  )
}

m2_hypothesis_spec <- function(id) {
  spec <- switch(
    id,
    H02_1 = list(
      question = "Does annual production follow a simple linear trend?",
      null = "A linear trend is sufficient to describe annual production.",
      supported_when = "fail_to_reject",
      test = "Linear fit adequacy"
    ),
    H02_2 = list(
      question = "Is there evidence of at least one structural break in annual production?",
      null = "No structural break is present.",
      supported_when = "reject",
      test = "Pettitt change-point test"
    ),
    H02_3 = list(
      question = "Does exponential growth outperform a linear alternative?",
      null = "Exponential growth does not improve over a linear model.",
      supported_when = "fail_to_reject",
      test = "Competing fit comparison"
    ),
    H02_4 = list(
      question = "Are linear-model residuals approximately normal?",
      null = "Residuals are approximately normal.",
      supported_when = "fail_to_reject",
      test = "Shapiro-Wilk"
    ),
    H02_5 = list(
      question = "Are residuals approximately homoscedastic?",
      null = "Residual variance is constant.",
      supported_when = "fail_to_reject",
      test = "Breusch-Pagan"
    ),
    H02_6 = list(
      question = "Is the year-over-year growth rate approximately constant?",
      null = "The year-over-year growth rate is approximately constant.",
      supported_when = "fail_to_reject",
      test = "Coefficient of variation heuristic"
    ),
    H02_7 = list(
      question = "Are there no substantial annual outliers in the series?",
      null = "No substantial annual outliers are present.",
      supported_when = "fail_to_reject",
      test = "Standardized residual screening"
    ),
    H02_8 = list(
      question = "Is there evidence of saturation or logistic dynamics?",
      null = "No saturation pattern is evident.",
      supported_when = "fail_to_reject",
      test = "Early-vs-late growth comparison"
    ),
    H02_9 = list(
      question = "Is the year-over-year growth-rate process stationary?",
      null = "The year-over-year growth-rate process is stationary.",
      supported_when = "fail_to_reject",
      test = "Split-sample t-test"
    ),
    H02_10 = list(
      question = "Is there evidence of acceleration or deceleration in the trend?",
      null = "Quadratic acceleration is zero.",
      supported_when = "reject",
      test = "Quadratic trend coefficient"
    ),
    H02_11 = list(
      question = "Is annual production monotonic over time?",
      null = "No monotonic trend is present.",
      supported_when = "reject",
      test = "Kendall tau"
    ),
    H02_12 = list(
      question = "Is the robust annual slope positive?",
      null = "The robust annual slope is not positive.",
      supported_when = "reject",
      test = "Theil-Sen interval"
    ),
    H02_13 = list(
      question = "Does the selected forecasting model outperform the naive baseline under temporal cross-validation?",
      null = "The selected forecasting model does not outperform the naive baseline.",
      supported_when = "reject",
      test = "Paired temporal CV contrast"
    ),
    H02_14 = list(
      question = "Is the interpretable headline model competitive with the flexible benchmark?",
      null = "Only the flexible benchmark is competitive enough for reporting.",
      supported_when = "reject",
      test = "Headline-vs-benchmark competitiveness rule"
    ),
    H02_15 = list(
      question = "Is there a robust consensus structural break in annual production?",
      null = "No robust consensus structural break is present.",
      supported_when = "reject",
      test = "Consensus across Pettitt, PELT, CUSUM, and binary segmentation"
    ),
    H02_16 = list(
      question = "Does the annual slope change materially after the dominant structural break?",
      null = "The pre-break and post-break slopes are equal.",
      supported_when = "reject",
      test = "Segmented regression interaction"
    ),
    H02_17 = list(
      question = "Do saturating interpretable models outperform simple non-saturating growth alternatives?",
      null = "Saturating interpretable models do not outperform simple growth alternatives.",
      supported_when = "reject",
      test = "Composite model comparison with interpretability constraints"
    ),
    H02_18 = list(
      question = "Is a finite carrying capacity identifiable for the field?",
      null = "No finite carrying capacity can be identified with sufficient stability.",
      supported_when = "reject",
      test = "Bootstrap-supported carrying-capacity assessment"
    ),
    H02_19 = list(
      question = "Is the diffusion trajectory asymmetric rather than symmetric logistic?",
      null = "A symmetric logistic diffusion is sufficient.",
      supported_when = "reject",
      test = "Asymmetric-versus-symmetric sigmoidal comparison"
    ),
    H02_20 = list(
      question = "Does the annual-production series show long-memory persistence?",
      null = "The series behaves like short-memory noise around the trend (H = 0.5).",
      supported_when = "reject",
      test = "Hurst exponent deviation from 0.5"
    ),
    H02_21 = list(
      question = "Is the series better characterized as trend-stationary than unit-root dominated?",
      null = "Trend-stationarity is not supported.",
      supported_when = "reject",
      test = "Approximate ADF + KPSS pair"
    ),
    H02_22 = list(
      question = "Did the volatility of annual production change after the dominant break?",
      null = "Pre-break and post-break variance are equal.",
      supported_when = "reject",
      test = "Pre/post-break variance ratio test"
    ),
    H02_23 = list(
      question = "Are there early-warning signals before the dominant structural break?",
      null = "No early-warning signal is evident before the break.",
      supported_when = "reject",
      test = "Rolling autocorrelation and variance trend diagnostics"
    ),
    H02_24 = list(
      question = "Is the selected headline model stable under bootstrap perturbations?",
      null = "The selected headline model is not stable under bootstrap perturbations.",
      supported_when = "reject",
      test = "Bootstrap selection-frequency test"
    ),
    H02_25 = list(
      question = "Does the ensemble forecast outperform the best individual forecast model?",
      null = "The ensemble forecast does not outperform the best individual model.",
      supported_when = "reject",
      test = "Temporal CV comparison of ensemble versus best single model"
    ),
    H02_26 = list(
      question = "Are the forecast intervals calibrated and informative?",
      null = "Forecast intervals are not demonstrably calibrated.",
      supported_when = "reject",
      test = "Interval coverage and sharpness backtest"
    ),
    H02_27 = list(
      question = "Is annual production cointegrated with an external driver?",
      null = "No long-run cointegrating relation with the external driver is supported.",
      supported_when = "reject",
      test = "Approximate Engle-Granger residual-stationarity test"
    ),
    H02_28 = list(
      question = "Does the external driver Granger-cause annual production?",
      null = "Lagged values of the external driver do not improve annual-production forecasts.",
      supported_when = "reject",
      test = "Lag-1 Granger-style nested regression"
    ),
    H02_29 = list(
      question = "Did a specified external shock alter the level or slope of annual production?",
      null = "The specified shock did not alter the level or slope of the series.",
      supported_when = "reject",
      test = "Interrupted time-series regression"
    ),
    H02_30 = list(
      question = "Do cluster-specific trajectories differ from one another?",
      null = "Cluster trajectories do not differ materially over time.",
      supported_when = "reject",
      test = "Cluster-by-time interaction model"
    ),
    H02_31 = list(
      question = "Are clusters diverging over time rather than converging?",
      null = "Cross-cluster dispersion is not increasing over time.",
      supported_when = "reject",
      test = "Kendall trend on yearly cross-cluster dispersion"
    ),
    H02_32 = list(
      question = "Do clusters occupy statistically different lifecycle dynamics?",
      null = "Clusters do not differ in lifecycle dynamics.",
      supported_when = "reject",
      test = "Cross-cluster growth-dynamics comparison"
    ),
    H02_33 = list(
      question = "Does hierarchical forecasting improve over direct aggregate forecasting?",
      null = "Hierarchical forecasting does not improve over direct aggregate forecasting.",
      supported_when = "reject",
      test = "Hierarchy-versus-direct forecast comparison"
    ),
    H02_34 = list(
      question = "Are the temporal conclusions robust across source or deduplication scenarios?",
      null = "Temporal conclusions are robust across scenarios.",
      supported_when = "fail_to_reject",
      test = "Scenario-by-time interaction robustness test"
    ),
    H02_35 = list(
      question = "Does the series exhibit multiple growth regimes rather than a single-phase evolution?",
      null = "A single growth regime is sufficient.",
      supported_when = "reject",
      test = "Multi-segment versus single-trend comparison"
    ),
    H02_36 = list(
      question = "Is the selected forecast more likely than not to outperform the naive baseline across temporal folds?",
      null = "The selected forecast is not more likely than not to outperform naive across folds.",
      supported_when = "reject",
      test = "Binomial probability-of-superiority test"
    ),
    H02_37 = list(
      question = "Do annual production means differ across early, middle, and recent temporal regimes?",
      null = "Mean annual production is equal across temporal regimes.",
      supported_when = "reject",
      test = "One-way ANOVA"
    ),
    H02_38 = list(
      question = "Do annual production slopes differ across temporal regimes?",
      null = "The Year by Regime interaction is zero.",
      supported_when = "reject",
      test = "ANCOVA interaction test"
    ),
    H02_39 = list(
      question = "Are forecast residuals independent and unbiased?",
      null = "One-step residuals have zero mean and no autocorrelation.",
      supported_when = "fail_to_reject",
      test = "Residual t-test and Ljung-Box"
    ),
    H02_40 = list(
      question = "Did growth volatility shift between early and recent windows?",
      null = "Year-over-year growth variance is equal across windows.",
      supported_when = "reject",
      test = "Fligner-Killeen variance test"
    ),
    H02_41 = list(
      question = "Do forecast model families have equivalent one-step errors?",
      null = "Linear, exponential, and quadratic one-step absolute errors are equal.",
      supported_when = "reject",
      test = "Kruskal-Wallis rolling-error comparison"
    ),
    H02_42 = list(
      question = "Does the recent production slope exceed the historical baseline?",
      null = "Recent annual slope is not greater than the historical baseline slope.",
      supported_when = "reject",
      test = "One-sided Wilcoxon slope comparison"
    ),
    H02_43 = list(
      question = "Is annual production compatible with a stable-intensity Poisson process?",
      null = "The Poisson intensity does not change with year.",
      supported_when = "reject",
      test = "Poisson log-linear trend likelihood-ratio test"
    ),
    H02_44 = list(
      question = "Is annual production overdispersed relative to a Poisson trend model?",
      null = "The Poisson dispersion ratio is less than or equal to one.",
      supported_when = "reject",
      test = "Pearson dispersion test"
    ),
    H02_45 = list(
      question = "Are there statistically significant burst years after FDR correction?",
      null = "No positive annual residual remains significant after FDR correction.",
      supported_when = "reject",
      test = "Poisson residual burst scan"
    ),
    H02_46 = list(
      question = "Does annual production show temporal momentum after trend removal?",
      null = "Lag-1 residual autocorrelation equals zero.",
      supported_when = "reject",
      test = "Lag-1 residual autocorrelation test"
    ),
    H02_47 = list(
      question = "Has recent annual production entered a plateau?",
      null = "The recent annual slope remains positive.",
      supported_when = "reject",
      test = "One-sided recent-window slope test"
    ),
    H02_48 = list(
      question = "Does an abrupt intervention model outperform a gradual temporal curve?",
      null = "A smooth gradual curve is not improved by an abrupt breakpoint intervention.",
      supported_when = "reject",
      test = "Quadratic curve versus interrupted time-series comparison"
    ),
    H02_49 = list(
      question = "Is acceleration evidence robust to leaving out individual years?",
      null = "The acceleration sign is unstable when individual years are removed.",
      supported_when = "reject",
      test = "Leave-one-year-out acceleration sign-stability test"
    ),
    H02_50 = list(
      question = "Does annual production show a diffusion-style growth signature?",
      null = "Diffusion-style growth models do not improve over a simple polynomial trend.",
      supported_when = "reject",
      test = "Logistic/Gompertz versus polynomial AIC contrast"
    ),
    list(
      question = id,
      null = NA_character_,
      supported_when = "fail_to_reject",
      test = "heuristic diagnostic"
    )
  )
  spec$default_interpretation <- spec$question
  spec
}

m2_hypothesis_decision_from_raw <- function(id, h) {
  raw_result <- h$result %||% "inconclusive"
  spec <- m2_hypothesis_spec(id)
  if (identical(raw_result, "inconclusive")) {
    return("inconclusive")
  }
  supported_when <- spec$supported_when %||% "fail_to_reject"
  if (identical(raw_result, supported_when)) {
    return("supported")
  }
  if (raw_result %in% c("reject", "fail_to_reject")) {
    return("not_supported")
  }
  "inconclusive"
}

m2_test_forecast_superiority <- function(forecasting) {
  comparison <- forecasting$model_comparison$comparison %||% data.frame()
  best_model <- forecasting$model_comparison$best_model %||% NA_character_
  cv_scores <- forecasting$cv_results$cv_scores %||% data.frame()

  if (!is.data.frame(comparison) || nrow(comparison) == 0 || !is.character(best_model) || is.na(best_model) || identical(best_model, "Naive")) {
    return(list(
      hypothesis = "Selected forecast does not outperform the naive baseline",
      null = "The selected forecast does not outperform the naive baseline",
      result = "inconclusive",
      test = "Paired temporal CV contrast",
      evidence_class = "statistical",
      interpretation = "Forecast superiority could not be assessed because the best model was unavailable or identical to the naive baseline."
    ))
  }

  if (!is.data.frame(cv_scores) || nrow(cv_scores) == 0 || !all(c("fold", "model", "mae") %in% names(cv_scores))) {
    return(list(
      hypothesis = "Selected forecast outperforms the naive baseline",
      null = "The selected forecast does not outperform the naive baseline",
      result = "inconclusive",
      test = "Paired temporal CV contrast",
      evidence_class = "statistical",
      interpretation = "Cross-validation fold errors were not available for a paired comparison."
    ))
  }

  best_rows <- cv_scores[cv_scores$model == best_model, c("fold", "mae"), drop = FALSE]
  naive_rows <- cv_scores[cv_scores$model == "Naive", c("fold", "mae"), drop = FALSE]
  paired <- merge(best_rows, naive_rows, by = "fold", suffixes = c("_best", "_naive"))
  if (nrow(paired) < 3) {
    return(list(
      hypothesis = "Selected forecast outperforms the naive baseline",
      null = "The selected forecast does not outperform the naive baseline",
      result = "inconclusive",
      test = "Paired temporal CV contrast",
      evidence_class = "statistical",
      interpretation = "Fewer than three paired CV folds were available for a superiority test."
    ))
  }

  paired$delta_mae <- paired$mae_naive - paired$mae_best
  delta_mean <- mean(paired$delta_mae, na.rm = TRUE)
  delta_sd <- stats::sd(paired$delta_mae, na.rm = TRUE)
  stderr <- delta_sd / sqrt(nrow(paired))
  ci <- if (is.finite(stderr)) {
    delta_mean + c(-1, 1) * stats::qt(0.975, df = max(1, nrow(paired) - 1)) * stderr
  } else {
    c(NA_real_, NA_real_)
  }
  p_value <- tryCatch(
    stats::t.test(paired$delta_mae, alternative = "greater")$p.value,
    error = function(e) NA_real_
  )

  result <- if (is.finite(p_value) && p_value < 0.05 && delta_mean > 0) "reject" else "fail_to_reject"
  list(
    hypothesis = "Selected forecast outperforms the naive baseline",
    null = "The selected forecast does not outperform the naive baseline",
    result = result,
    test = "Paired temporal CV contrast",
    evidence_class = "statistical",
    p_value = p_value,
    delta_mae = delta_mean,
    ci_lower = ci[1],
    ci_upper = ci[2],
    relative_improvement_mae = 100 * safe_divide(delta_mean, mean(paired$mae_naive, na.rm = TRUE), default = NA_real_),
    interpretation = sprintf(
      "Mean MAE improvement versus naive = %.3f articles per fold (95%% CI %s).",
      delta_mean,
      m2_format_confidence_interval(ci[1], ci[2])
    )
  )
}

m2_test_interpretable_headline_competitiveness <- function(regression) {
  best <- regression$best_model %||% list()
  benchmark <- regression$benchmark_best_model %||% list()
  if (length(best) == 0 || length(benchmark) == 0) {
    return(list(
      hypothesis = "The interpretable headline model is competitive with the flexible benchmark",
      null = "Only the flexible benchmark is competitive enough for reporting",
      result = "inconclusive",
      test = "Headline-vs-benchmark competitiveness rule",
      evidence_class = "heuristic",
      interpretation = "Regression benchmark information was unavailable."
    ))
  }

  same_model <- identical(best$name %||% NA_character_, benchmark$name %||% "__different__")
  score_gap <- m2_scalar_num(benchmark$composite_score) - m2_scalar_num(best$composite_score)
  rmse_ratio <- safe_divide(m2_scalar_num(best$RMSE), m2_scalar_num(benchmark$RMSE), default = NA_real_)
  supported <- isFALSE(same_model) || isTRUE(best$name == benchmark$name)
  result <- if (supported && (!is.finite(score_gap) || score_gap <= 0.08)) "reject" else "fail_to_reject"

  list(
    hypothesis = "The interpretable headline model is competitive with the flexible benchmark",
    null = "Only the flexible benchmark is competitive enough for reporting",
    result = result,
    test = "Headline-vs-benchmark competitiveness rule",
    evidence_class = "heuristic",
    delta_mae = score_gap,
    effect_size = score_gap,
    relative_rmse = rmse_ratio,
    interpretation = sprintf(
      "Headline model %s scored %.3f versus benchmark %s at %.3f (RMSE ratio %.3f).",
      best$name %||% "NA",
      m2_scalar_num(best$composite_score),
      benchmark$name %||% "NA",
      m2_scalar_num(benchmark$composite_score),
      rmse_ratio
    )
  )
}

m2_make_inconclusive_result <- function(hypothesis, null, test, evidence_class, interpretation) {
  list(
    hypothesis = hypothesis,
    null = null,
    result = "inconclusive",
    test = test,
    evidence_class = evidence_class,
    interpretation = interpretation
  )
}

m2_build_series_payload <- function(input) {
  if (!is.data.frame(input) || !all(c("Year", "Articles") %in% names(input))) {
    return(list(years = numeric(0), articles = numeric(0), data = data.frame()))
  }

  data <- data.frame(
    Year = m2_num_vec(input$Year),
    Articles = m2_num_vec(input$Articles),
    stringsAsFactors = FALSE
  )
  keep <- is.finite(data$Year) & is.finite(data$Articles)
  data <- data[keep, , drop = FALSE]
  data <- data[order(data$Year), , drop = FALSE]

  list(
    years = data$Year,
    articles = data$Articles,
    data = data
  )
}

m2_detect_dominant_break <- function(changepoint, years) {
  cp <- changepoint$changepoints %||% list()
  summary <- changepoint$summary %||% list()
  candidate_years <- sort(unique(c(
    m2_num_vec(cp$consensus),
    m2_num_vec(cp$pelt),
    m2_num_vec(cp$cusum),
    m2_num_vec(cp$binseg),
    m2_num_vec(cp$pettitt)
  )))

  if (length(candidate_years) == 0) {
    return(list(
      year = NA_real_,
      index = NA_integer_,
      methods = character(0),
      n_methods = 0L,
      agreement_rate = m2_scalar_num(summary$agreement_rate, default = 0)
    ))
  }

  support <- lapply(candidate_years, function(y) {
    methods <- character(0)
    if (any(abs(m2_num_vec(cp$consensus) - y) <= 2, na.rm = TRUE)) methods <- c(methods, "consensus")
    if (any(abs(m2_num_vec(cp$pelt) - y) <= 2, na.rm = TRUE)) methods <- c(methods, "PELT")
    if (any(abs(m2_num_vec(cp$cusum) - y) <= 2, na.rm = TRUE)) methods <- c(methods, "CUSUM")
    if (any(abs(m2_num_vec(cp$binseg) - y) <= 2, na.rm = TRUE)) methods <- c(methods, "BinSeg")
    if (any(abs(m2_num_vec(cp$pettitt) - y) <= 2, na.rm = TRUE)) methods <- c(methods, "Pettitt")
    list(year = y, methods = unique(methods), n_methods = length(unique(methods)))
  })

  support_df <- do.call(rbind, lapply(support, function(x) {
    data.frame(
      year = x$year,
      n_methods = x$n_methods,
      methods = paste(x$methods, collapse = ", "),
      stringsAsFactors = FALSE
    )
  }))
  support_df <- support_df[order(-support_df$n_methods, abs(support_df$year - stats::median(candidate_years))), , drop = FALSE]
  chosen <- support[[match(support_df$year[1], candidate_years)]]
  index <- match(chosen$year, years)

  list(
    year = chosen$year,
    index = index,
    methods = chosen$methods,
    n_methods = chosen$n_methods,
    agreement_rate = m2_scalar_num(summary$agreement_rate, default = 0)
  )
}

m2_test_consensus_structural_break <- function(changepoint, break_info) {
  pettitt_p <- m2_scalar_num(changepoint$pettitt_result$p_value)
  if (!is.finite(break_info$year)) {
    return(list(
      hypothesis = "A robust consensus structural break is present",
      null = "No robust consensus structural break is present",
      result = "fail_to_reject",
      test = "Consensus across Pettitt, PELT, CUSUM, and binary segmentation",
      evidence_class = "statistical",
      p_value = if (is.finite(pettitt_p)) pettitt_p else NA_real_,
      effect_size = 0,
      interpretation = "No consensus structural break was retained across the available detectors."
    ))
  }

  result <- if (break_info$n_methods >= 2) "reject" else "fail_to_reject"
  list(
    hypothesis = "A robust consensus structural break is present",
    null = "No robust consensus structural break is present",
    result = result,
    test = "Consensus across Pettitt, PELT, CUSUM, and binary segmentation",
    evidence_class = "statistical",
    p_value = if (is.finite(pettitt_p)) pettitt_p else NA_real_,
    effect_size = break_info$agreement_rate,
    interpretation = sprintf(
      "A dominant break around %.0f is supported by %d detectors (%s); agreement rate = %.2f.",
      break_info$year,
      break_info$n_methods,
      paste(break_info$methods, collapse = ", "),
      break_info$agreement_rate
    )
  )
}

m2_test_post_break_slope_change <- function(series_payload, break_info) {
  data <- series_payload$data
  if (!is.finite(break_info$year) || nrow(data) < 8) {
    return(list(
      hypothesis = "The annual slope changes after the dominant structural break",
      null = "The pre-break and post-break slopes are equal",
      result = if (nrow(data) >= 8) "fail_to_reject" else "inconclusive",
      test = "Segmented regression interaction",
      evidence_class = "statistical",
      interpretation = if (nrow(data) >= 8) {
        "No dominant breakpoint was available to evaluate a segmented slope change."
      } else {
        "Too few observations were available to estimate a segmented slope change."
      }
    ))
  }

  data$post_break <- as.integer(data$Year >= break_info$year)
  if (sum(data$post_break == 0) < 4 || sum(data$post_break == 1) < 4) {
    return(m2_make_inconclusive_result(
      "The annual slope changes after the dominant structural break",
      "The pre-break and post-break slopes are equal",
      "Segmented regression interaction",
      "statistical",
      "At least four observations were required on each side of the dominant breakpoint."
    ))
  }

  fit <- tryCatch(stats::lm(Articles ~ Year * post_break, data = data), error = function(e) NULL)
  if (is.null(fit)) {
    return(m2_make_inconclusive_result(
      "The annual slope changes after the dominant structural break",
      "The pre-break and post-break slopes are equal",
      "Segmented regression interaction",
      "statistical",
      "Segmented regression could not be estimated."
    ))
  }

  coef_table <- summary(fit)$coefficients
  term <- grep("Year:post_break", rownames(coef_table), value = TRUE)[1]
  if (!is.character(term) || is.na(term)) {
    return(m2_make_inconclusive_result(
      "The annual slope changes after the dominant structural break",
      "The pre-break and post-break slopes are equal",
      "Segmented regression interaction",
      "statistical",
      "The slope-change coefficient could not be identified in the segmented regression."
    ))
  }

  delta_slope <- m2_scalar_num(coef_table[term, "Estimate"])
  p_value <- m2_scalar_num(coef_table[term, "Pr(>|t|)"])
  ci <- tryCatch(stats::confint(fit, parm = term), error = function(e) c(NA_real_, NA_real_))
  result <- if (is.finite(p_value) && p_value < 0.05) "reject" else "fail_to_reject"

  list(
    hypothesis = "The annual slope changes after the dominant structural break",
    null = "The pre-break and post-break slopes are equal",
    result = result,
    test = "Segmented regression interaction",
    evidence_class = "statistical",
    p_value = p_value,
    effect_size = delta_slope,
    ci_lower = m2_scalar_num(ci[1]),
    ci_upper = m2_scalar_num(ci[2]),
    interpretation = sprintf(
      "The post-break slope changed by %.3f articles/year relative to the pre-break slope around %.0f.",
      delta_slope,
      break_info$year
    )
  )
}

m2_test_saturating_model_superiority <- function(regression, config) {
  comparison <- m2_ensure_regression_comparison_columns(regression$comparison_table %||% data.frame())
  if (!is.data.frame(comparison) || nrow(comparison) == 0 || !all(c("Model", "CompositeScore", "SaturationCapable") %in% names(comparison))) {
    return(m2_make_inconclusive_result(
      "Saturating interpretable models outperform simple growth alternatives",
      "Saturating interpretable models do not outperform simple growth alternatives",
      "Composite model comparison with interpretability constraints",
      "heuristic",
      "The regression comparison table was unavailable."
    ))
  }

  sat <- comparison[comparison$SaturationCapable %in% TRUE & comparison$HeadlineEligible %in% TRUE, , drop = FALSE]
  simple <- comparison[comparison$SaturationCapable %in% FALSE & comparison$HeadlineEligible %in% TRUE & !(comparison$BenchmarkOnly %in% TRUE), , drop = FALSE]
  if (nrow(sat) == 0 || nrow(simple) == 0) {
    return(m2_make_inconclusive_result(
      "Saturating interpretable models outperform simple growth alternatives",
      "Saturating interpretable models do not outperform simple growth alternatives",
      "Composite model comparison with interpretability constraints",
      "heuristic",
      "Comparable saturating and simple candidate models were not both available."
    ))
  }

  sat_best <- sat[order(-sat$CompositeScore, sat$RMSE), , drop = FALSE][1, , drop = FALSE]
  simple_best <- simple[order(-simple$CompositeScore, simple$RMSE), , drop = FALSE][1, , drop = FALSE]
  tolerance <- m2_scalar_num(config$m2_interpretability_tolerance, default = 0.08)
  delta_score <- m2_scalar_num(sat_best$CompositeScore[1]) - m2_scalar_num(simple_best$CompositeScore[1])
  delta_aic <- m2_scalar_num(sat_best$AIC[1]) - m2_scalar_num(simple_best$AIC[1])
  selected_name <- regression$best_model$name %||% NA_character_
  selected_is_saturating <- identical(selected_name, sat_best$Model[1]) || (selected_name %in% sat$Model)
  supported <- isTRUE(selected_is_saturating) && (
    (is.finite(delta_score) && delta_score >= (-tolerance)) ||
      (is.finite(delta_aic) && delta_aic <= -2)
  )

  list(
    hypothesis = "Saturating interpretable models outperform simple growth alternatives",
    null = "Saturating interpretable models do not outperform simple growth alternatives",
    result = if (supported) "reject" else "fail_to_reject",
    test = "Composite model comparison with interpretability constraints",
    evidence_class = "heuristic",
    effect_size = delta_score,
    interpretation = sprintf(
      "Best saturating candidate %s scored %.3f versus best simple candidate %s at %.3f (delta composite %.3f; delta AIC %.3f).",
      sat_best$Model[1],
      m2_scalar_num(sat_best$CompositeScore[1]),
      simple_best$Model[1],
      m2_scalar_num(simple_best$CompositeScore[1]),
      delta_score,
      delta_aic
    )
  )
}

m2_bootstrap_headline_profile <- function(input, regression, config) {
  data <- m2_build_series_payload(input)$data
  best <- regression$best_model %||% list()
  fitted <- m2_num_vec(best$predictions)
  residuals <- m2_num_vec(best$residuals)
  if (nrow(data) < 8 || length(fitted) != nrow(data) || length(residuals) != nrow(data)) {
    return(list(status = "insufficient_data"))
  }

  reps <- max(4L, as.integer(m2_scalar_num(config$m2_hypothesis_bootstrap_reps, default = 6)))
  model_names <- character(0)
  capacities <- numeric(0)
  for (i in seq_len(reps)) {
    pseudo <- data
    pseudo$Articles <- pmax(0, fitted + sample(residuals, length(residuals), replace = TRUE))
    refit <- tryCatch(compute_m2_regression(pseudo, config), error = function(e) NULL)
    if (is.null(refit) || !identical(refit$status, "success")) {
      next
    }
    model_names <- c(model_names, refit$best_model$name %||% NA_character_)
    cap <- m2_scalar_num(refit$best_model$parameter_summary$carrying_capacity)
    if (is.finite(cap)) {
      capacities <- c(capacities, cap)
    }
  }

  freqs <- if (length(model_names) > 0) {
    as.data.frame(table(model_names), stringsAsFactors = FALSE)
  } else {
    data.frame(model_names = character(0), Freq = integer(0), stringsAsFactors = FALSE)
  }

  list(
    status = if (length(model_names) > 0) "success" else "insufficient_data",
    n_success = length(model_names),
    frequencies = freqs,
    capacities = capacities
  )
}

m2_test_carrying_capacity_identifiability <- function(series_payload, regression, bootstrap_profile) {
  best <- regression$best_model %||% list()
  params <- best$parameter_summary %||% list()
  capacity <- m2_scalar_num(params$carrying_capacity)
  max_obs <- max(series_payload$articles, na.rm = TRUE)
  if (!is.finite(capacity) || !is.finite(max_obs) || max_obs <= 0) {
    return(m2_make_inconclusive_result(
      "A finite carrying capacity is identifiable for the field",
      "No finite carrying capacity can be identified with sufficient stability",
      "Bootstrap-supported carrying-capacity assessment",
      "statistical",
      "The selected model did not expose a finite carrying-capacity parameter."
    ))
  }

  ratio <- safe_divide(capacity, max_obs, default = NA_real_)
  caps <- m2_num_vec(bootstrap_profile$capacities)
  ci <- if (length(caps) >= 4) stats::quantile(caps, probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE) else c(NA_real_, NA_real_)
  p_value <- if (length(caps) >= 4) mean(caps <= max_obs, na.rm = TRUE) else NA_real_
  supported <- if (all(is.finite(ci))) {
    ci[1] > (0.95 * max_obs) && ratio <= 6
  } else {
    is.finite(ratio) && ratio > 1.02 && ratio <= 6
  }

  list(
    hypothesis = "A finite carrying capacity is identifiable for the field",
    null = "No finite carrying capacity can be identified with sufficient stability",
    result = if (supported) "reject" else "fail_to_reject",
    test = "Bootstrap-supported carrying-capacity assessment",
    evidence_class = "statistical",
    p_value = if (is.finite(p_value)) p_value else NA_real_,
    effect_size = ratio,
    ci_lower = if (all(is.finite(ci))) m2_scalar_num(ci[1]) else NA_real_,
    ci_upper = if (all(is.finite(ci))) m2_scalar_num(ci[2]) else NA_real_,
    interpretation = sprintf(
      "Estimated carrying capacity = %.2f articles, equivalent to %.2fx the current maximum observed annual output.",
      capacity,
      ratio
    )
  )
}

m2_test_diffusion_asymmetry <- function(regression, config) {
  comparison <- m2_ensure_regression_comparison_columns(regression$comparison_table %||% data.frame())
  if (!is.data.frame(comparison) || nrow(comparison) == 0) {
    return(m2_make_inconclusive_result(
      "The diffusion trajectory is asymmetric rather than symmetric logistic",
      "A symmetric logistic diffusion is sufficient",
      "Asymmetric-versus-symmetric sigmoidal comparison",
      "heuristic",
      "The regression comparison table was unavailable."
    ))
  }

  asym_models <- c("Gompertz", "GompertzOffset", "Richards", "Weibull", "ChapmanRichards", "Korf", "Baranyi")
  sym_models <- c("Logistic", "Logistic4P")
  asym <- comparison[comparison$Model %in% asym_models, , drop = FALSE]
  sym <- comparison[comparison$Model %in% sym_models, , drop = FALSE]
  if (nrow(asym) == 0 || nrow(sym) == 0) {
    return(m2_make_inconclusive_result(
      "The diffusion trajectory is asymmetric rather than symmetric logistic",
      "A symmetric logistic diffusion is sufficient",
      "Asymmetric-versus-symmetric sigmoidal comparison",
      "heuristic",
      "Asymmetric and symmetric sigmoidal candidates were not both available."
    ))
  }

  asym_best <- asym[order(-asym$CompositeScore, asym$RMSE), , drop = FALSE][1, , drop = FALSE]
  sym_best <- sym[order(-sym$CompositeScore, sym$RMSE), , drop = FALSE][1, , drop = FALSE]
  tolerance <- m2_scalar_num(config$m2_interpretability_tolerance, default = 0.08)
  delta_score <- m2_scalar_num(asym_best$CompositeScore[1]) - m2_scalar_num(sym_best$CompositeScore[1])
  supported <- (is.finite(delta_score) && delta_score > tolerance) ||
    identical(regression$best_model$name %||% "", asym_best$Model[1])

  list(
    hypothesis = "The diffusion trajectory is asymmetric rather than symmetric logistic",
    null = "A symmetric logistic diffusion is sufficient",
    result = if (supported) "reject" else "fail_to_reject",
    test = "Asymmetric-versus-symmetric sigmoidal comparison",
    evidence_class = "heuristic",
    effect_size = delta_score,
    interpretation = sprintf(
      "Best asymmetric candidate %s scored %.3f versus best symmetric logistic candidate %s at %.3f.",
      asym_best$Model[1],
      m2_scalar_num(asym_best$CompositeScore[1]),
      sym_best$Model[1],
      m2_scalar_num(sym_best$CompositeScore[1])
    )
  )
}

m2_test_long_memory_persistence <- function(diagnostics, n_obs) {
  hurst <- m2_scalar_num(diagnostics$trend_statistics$hurst_exponent)
  if (!is.finite(hurst) || !is.finite(n_obs) || n_obs < 8) {
    return(m2_make_inconclusive_result(
      "The series shows long-memory persistence",
      "The series behaves like short-memory noise around the trend (H = 0.5)",
      "Hurst exponent deviation from 0.5",
      "statistical",
      "The Hurst exponent could not be estimated reliably."
    ))
  }

  se <- 1 / sqrt(n_obs)
  z <- safe_divide(hurst - 0.5, se, default = NA_real_)
  p_value <- if (is.finite(z)) 2 * (1 - stats::pnorm(abs(z))) else NA_real_
  ci <- hurst + c(-1, 1) * 1.96 * se

  list(
    hypothesis = "The series shows long-memory persistence",
    null = "The series behaves like short-memory noise around the trend (H = 0.5)",
    result = if (is.finite(p_value) && p_value < 0.05 && hurst > 0.55) "reject" else "fail_to_reject",
    test = "Hurst exponent deviation from 0.5",
    evidence_class = "statistical",
    p_value = p_value,
    effect_size = hurst - 0.5,
    ci_lower = ci[1],
    ci_upper = ci[2],
    interpretation = sprintf("Estimated Hurst exponent = %.3f.", hurst)
  )
}

m2_adf_test_simple <- function(series_payload) {
  y <- m2_num_vec(series_payload$articles)
  t <- m2_num_vec(series_payload$years)
  if (length(y) < 8) {
    return(list(gamma = NA_real_, p_value = NA_real_, ci = c(NA_real_, NA_real_)))
  }

  dy <- diff(y)
  ylag <- y[-length(y)]
  tt <- t[-1]
  fit <- tryCatch(stats::lm(dy ~ ylag + tt), error = function(e) NULL)
  if (is.null(fit)) {
    return(list(gamma = NA_real_, p_value = NA_real_, ci = c(NA_real_, NA_real_)))
  }

  coef_table <- summary(fit)$coefficients
  if (!"ylag" %in% rownames(coef_table)) {
    return(list(gamma = NA_real_, p_value = NA_real_, ci = c(NA_real_, NA_real_)))
  }

  ci <- tryCatch(stats::confint(fit, parm = "ylag"), error = function(e) c(NA_real_, NA_real_))
  list(
    gamma = m2_scalar_num(coef_table["ylag", "Estimate"]),
    p_value = m2_scalar_num(coef_table["ylag", "Pr(>|t|)"]),
    ci = c(m2_scalar_num(ci[1]), m2_scalar_num(ci[2]))
  )
}

m2_kpss_trend_test_simple <- function(series_payload) {
  y <- m2_num_vec(series_payload$articles)
  t <- seq_along(y)
  if (length(y) < 8) {
    return(list(statistic = NA_real_, p_value = NA_real_))
  }

  fit <- tryCatch(stats::lm(y ~ t), error = function(e) NULL)
  if (is.null(fit)) {
    return(list(statistic = NA_real_, p_value = NA_real_))
  }

  resid <- stats::residuals(fit)
  eta <- cumsum(resid)
  denom <- mean(resid^2, na.rm = TRUE)
  if (!is.finite(denom) || denom <= .Machine$double.eps) {
    return(list(statistic = NA_real_, p_value = NA_real_))
  }

  stat <- sum(eta^2, na.rm = TRUE) / (length(y)^2 * denom)
  p_value <- if (stat < 0.119) {
    0.10
  } else if (stat < 0.146) {
    0.075
  } else if (stat < 0.176) {
    0.05
  } else if (stat < 0.216) {
    0.025
  } else {
    0.01
  }
  list(statistic = stat, p_value = p_value)
}

m2_test_trend_stationarity <- function(series_payload) {
  adf <- m2_adf_test_simple(series_payload)
  kpss <- m2_kpss_trend_test_simple(series_payload)
  if (!is.finite(adf$p_value) || !is.finite(kpss$p_value)) {
    return(m2_make_inconclusive_result(
      "The series is better characterized as trend-stationary than unit-root dominated",
      "Trend-stationarity is not supported",
      "Approximate ADF + KPSS pair",
      "statistical",
      "ADF and KPSS evidence could not be computed jointly."
    ))
  }

  supported <- is.finite(adf$p_value) && is.finite(kpss$p_value) &&
    is.finite(adf$gamma) && adf$p_value < 0.05 && kpss$p_value > 0.05 && adf$gamma < 0
  list(
    hypothesis = "The series is better characterized as trend-stationary than unit-root dominated",
    null = "Trend-stationarity is not supported",
    result = if (supported) "reject" else "fail_to_reject",
    test = "Approximate ADF + KPSS pair",
    evidence_class = "statistical",
    p_value = adf$p_value,
    effect_size = adf$gamma,
    ci_lower = adf$ci[1],
    ci_upper = adf$ci[2],
    interpretation = sprintf(
      "ADF gamma = %.4f (p = %.4f) and KPSS trend-stationarity p ~ %.3f.",
      adf$gamma,
      adf$p_value,
      kpss$p_value
    )
  )
}

m2_test_variance_regime_shift <- function(series_payload, break_info) {
  data <- series_payload$data
  if (!is.finite(break_info$year)) {
    return(list(
      hypothesis = "Annual-production volatility changed after the dominant break",
      null = "Pre-break and post-break variance are equal",
      result = "fail_to_reject",
      test = "Pre/post-break variance ratio test",
      evidence_class = "statistical",
      interpretation = "No dominant breakpoint was available for a pre/post volatility comparison."
    ))
  }

  pre <- data$Articles[data$Year < break_info$year]
  post <- data$Articles[data$Year >= break_info$year]
  if (length(pre) < 4 || length(post) < 4) {
    return(m2_make_inconclusive_result(
      "Annual-production volatility changed after the dominant break",
      "Pre-break and post-break variance are equal",
      "Pre/post-break variance ratio test",
      "statistical",
      "At least four observations were required in each regime."
    ))
  }

  vt <- tryCatch(stats::var.test(pre, post), error = function(e) NULL)
  if (is.null(vt)) {
    return(m2_make_inconclusive_result(
      "Annual-production volatility changed after the dominant break",
      "Pre-break and post-break variance are equal",
      "Pre/post-break variance ratio test",
      "statistical",
      "Variance-ratio testing failed."
    ))
  }

  ratio <- safe_divide(stats::var(post, na.rm = TRUE), stats::var(pre, na.rm = TRUE), default = NA_real_)
  vt_p <- m2_scalar_num(vt$p.value)
  list(
    hypothesis = "Annual-production volatility changed after the dominant break",
    null = "Pre-break and post-break variance are equal",
    result = if (is.finite(vt_p) && vt_p < 0.05) "reject" else "fail_to_reject",
    test = "Pre/post-break variance ratio test",
    evidence_class = "statistical",
    p_value = vt_p,
    effect_size = ratio,
    ci_lower = m2_scalar_num(vt$conf.int[1]),
    ci_upper = m2_scalar_num(vt$conf.int[2]),
    interpretation = sprintf("Post-break variance / pre-break variance = %.3f around %.0f.", ratio, break_info$year)
  )
}

m2_kendall_stat <- function(x) {
  x <- m2_num_vec(x)
  x <- x[is.finite(x)]
  if (length(x) < 5) {
    return(list(tau = NA_real_, p_value = NA_real_))
  }
  kt <- tryCatch(stats::cor.test(seq_along(x), x, method = "kendall", exact = FALSE), error = function(e) NULL)
  if (is.null(kt)) {
    return(list(tau = NA_real_, p_value = NA_real_))
  }
  list(
    tau = m2_scalar_num(kt$estimate),
    p_value = m2_scalar_num(kt$p.value)
  )
}

m2_rolling_indicator <- function(x, window, fun) {
  x <- m2_num_vec(x)
  if (length(x) < window || window < 3) {
    return(numeric(0))
  }
  out <- numeric(length(x) - window + 1)
  for (i in seq_along(out)) {
    out[i] <- suppressWarnings(fun(x[i:(i + window - 1)]))
  }
  out
}

m2_test_early_warning_signals <- function(series_payload, break_info) {
  data <- series_payload$data
  if (!is.finite(break_info$year)) {
    return(list(
      hypothesis = "Early-warning signals are evident before the dominant structural break",
      null = "No early-warning signal is evident before the break",
      result = "fail_to_reject",
      test = "Rolling autocorrelation and variance trend diagnostics",
      evidence_class = "statistical",
      interpretation = "No dominant breakpoint was available for a pre-break early-warning analysis."
    ))
  }

  pre <- data$Articles[data$Year < break_info$year]
  if (length(pre) < 10) {
    return(m2_make_inconclusive_result(
      "Early-warning signals are evident before the dominant structural break",
      "No early-warning signal is evident before the break",
      "Rolling autocorrelation and variance trend diagnostics",
      "statistical",
      "At least ten pre-break observations were required to compute rolling early-warning signals."
    ))
  }

  window <- max(4L, min(6L, floor(length(pre) / 3)))
  lag1 <- m2_rolling_indicator(pre, window, function(v) {
    if (length(v) < 3) return(NA_real_)
    suppressWarnings(stats::cor(v[-length(v)], v[-1], use = "complete.obs"))
  })
  rolling_var <- m2_rolling_indicator(pre, window, function(v) stats::var(v, na.rm = TRUE))
  lag1_stat <- m2_kendall_stat(lag1)
  var_stat <- m2_kendall_stat(rolling_var)
  p_value <- min(c(lag1_stat$p_value, var_stat$p_value), na.rm = TRUE)
  if (!is.finite(p_value)) {
    p_value <- NA_real_
  }

  supported <- (is.finite(lag1_stat$tau) && lag1_stat$tau > 0 && is.finite(lag1_stat$p_value) && lag1_stat$p_value < 0.05) ||
    (is.finite(var_stat$tau) && var_stat$tau > 0 && is.finite(var_stat$p_value) && var_stat$p_value < 0.05)

  list(
    hypothesis = "Early-warning signals are evident before the dominant structural break",
    null = "No early-warning signal is evident before the break",
    result = if (supported) "reject" else "fail_to_reject",
    test = "Rolling autocorrelation and variance trend diagnostics",
    evidence_class = "statistical",
    p_value = p_value,
    effect_size = max(c(lag1_stat$tau, var_stat$tau), na.rm = TRUE),
    interpretation = sprintf(
      "Pre-break rolling lag-1 autocorrelation tau = %.3f and rolling variance tau = %.3f.",
      lag1_stat$tau,
      var_stat$tau
    )
  )
}

m2_test_model_selection_stability <- function(regression, bootstrap_profile) {
  best_name <- regression$best_model$name %||% NA_character_
  freqs <- bootstrap_profile$frequencies %||% data.frame()
  if (!is.character(best_name) || is.na(best_name) || !is.data.frame(freqs) || nrow(freqs) == 0) {
    return(m2_make_inconclusive_result(
      "The selected headline model is stable under bootstrap perturbations",
      "The selected headline model is not stable under bootstrap perturbations",
      "Bootstrap selection-frequency test",
      "statistical",
      "Bootstrap model-selection results were unavailable."
    ))
  }

  n_success <- max(bootstrap_profile$n_success, 0L)
  row <- freqs[freqs$model_names == best_name, , drop = FALSE]
  successes <- if (nrow(row) == 1) as.integer(row$Freq[1]) else 0L
  comparison <- m2_ensure_regression_comparison_columns(regression$comparison_table %||% data.frame())
  n_candidates <- length(unique(comparison$Model[comparison$HeadlineEligible %in% TRUE]))
  n_candidates <- max(n_candidates, 2L)
  bt <- tryCatch(stats::binom.test(successes, n_success, p = 1 / n_candidates, alternative = "greater"), error = function(e) NULL)
  p_value <- if (!is.null(bt)) m2_scalar_num(bt$p.value) else NA_real_
  ci <- if (!is.null(bt)) bt$conf.int else c(NA_real_, NA_real_)
  freq <- safe_divide(successes, n_success, default = NA_real_)

  list(
    hypothesis = "The selected headline model is stable under bootstrap perturbations",
    null = "The selected headline model is not stable under bootstrap perturbations",
    result = if (is.finite(p_value) && p_value < 0.05 && is.finite(freq) && freq >= 0.50) "reject" else "fail_to_reject",
    test = "Bootstrap selection-frequency test",
    evidence_class = "statistical",
    p_value = p_value,
    effect_size = freq,
    ci_lower = m2_scalar_num(ci[1]),
    ci_upper = m2_scalar_num(ci[2]),
    interpretation = sprintf("The headline model %s was re-selected in %.1f%% of bootstrap refits.", best_name, 100 * freq)
  )
}

m2_test_ensemble_superiority <- function(forecasting) {
  comparison <- forecasting$model_comparison$comparison %||% data.frame()
  if (!is.data.frame(comparison) || !("Ensemble" %in% comparison$model)) {
    return(m2_make_inconclusive_result(
      "The ensemble forecast outperforms the best individual forecast model",
      "The ensemble forecast does not outperform the best individual model",
      "Temporal CV comparison of ensemble versus best single model",
      "statistical",
      "Fold-level ensemble forecast errors were not available for a direct comparison."
    ))
  }

  ensemble_row <- comparison[comparison$model == "Ensemble", , drop = FALSE]
  non_ensemble <- comparison[comparison$model != "Ensemble", , drop = FALSE]
  non_ensemble <- non_ensemble[order(non_ensemble$CV_MAE, non_ensemble$CompositeRank), , drop = FALSE]
  best_single <- non_ensemble[1, , drop = FALSE]
  delta <- m2_scalar_num(best_single$CV_MAE[1]) - m2_scalar_num(ensemble_row$CV_MAE[1])
  cv_scores <- forecasting$cv_results$cv_scores %||% data.frame()
  p_value <- NA_real_
  ci <- c(NA_real_, NA_real_)
  if (is.data.frame(cv_scores) && nrow(cv_scores) > 0) {
    ensemble_scores <- cv_scores[cv_scores$model == "Ensemble", c("fold", "mae"), drop = FALSE]
    best_scores <- cv_scores[cv_scores$model == as.character(best_single$model[1]), c("fold", "mae"), drop = FALSE]
    paired <- merge(best_scores, ensemble_scores, by = "fold", suffixes = c("_single", "_ensemble"))
    if (nrow(paired) >= 3) {
      paired$delta_mae <- paired$mae_single - paired$mae_ensemble
      p_value <- tryCatch(stats::t.test(paired$delta_mae, alternative = "greater")$p.value, error = function(e) NA_real_)
      ci <- tryCatch({
        est <- mean(paired$delta_mae, na.rm = TRUE)
        se <- stats::sd(paired$delta_mae, na.rm = TRUE) / sqrt(nrow(paired))
        est + c(-1, 1) * stats::qt(0.975, df = nrow(paired) - 1) * se
      }, error = function(e) c(NA_real_, NA_real_))
    }
  }

  list(
    hypothesis = "The ensemble forecast outperforms the best individual forecast model",
    null = "The ensemble forecast does not outperform the best individual model",
    result = if (is.finite(p_value) && p_value < 0.05 && is.finite(delta) && delta > 0) "reject" else if (is.finite(delta) && delta > 0 && !is.finite(p_value)) "reject" else "fail_to_reject",
    test = "Temporal CV comparison of ensemble versus best single model",
    evidence_class = "statistical",
    p_value = p_value,
    effect_size = delta,
    ci_lower = m2_scalar_num(ci[1]),
    ci_upper = m2_scalar_num(ci[2]),
    interpretation = sprintf(
      "Ensemble CV MAE = %.3f versus best single-model CV MAE = %.3f.",
      m2_scalar_num(ensemble_row$CV_MAE[1]),
      m2_scalar_num(best_single$CV_MAE[1])
    )
  )
}

m2_test_forecast_interval_calibration <- function(forecasting) {
  coverage <- forecasting$cv_results$interval_coverage %||% data.frame()
  if (!is.data.frame(coverage) || nrow(coverage) == 0 || !all(c("coverage", "nominal") %in% names(coverage))) {
    return(m2_make_inconclusive_result(
      "Forecast intervals are calibrated and informative",
      "Forecast intervals are not demonstrably calibrated",
      "Interval coverage and sharpness backtest",
      "statistical",
      "Backtested forecast-interval coverage was unavailable."
    ))
  }

  best_model <- forecasting$model_comparison$best_model %||% NA_character_
  row <- coverage[coverage$model == best_model, , drop = FALSE]
  if (nrow(row) == 0) {
    row <- coverage[1, , drop = FALSE]
  }
  gap <- abs(m2_scalar_num(row$coverage[1]) - m2_scalar_num(row$nominal[1]))
  list(
    hypothesis = "Forecast intervals are calibrated and informative",
    null = "Forecast intervals are not demonstrably calibrated",
    result = if (is.finite(gap) && gap <= 0.10) "reject" else "fail_to_reject",
    test = "Interval coverage and sharpness backtest",
    evidence_class = "statistical",
    effect_size = -gap,
    p_value = NA_real_,
    interpretation = sprintf("Observed interval coverage = %.3f versus nominal = %.3f.", m2_scalar_num(row$coverage[1]), m2_scalar_num(row$nominal[1]))
  )
}

m2_extract_external_driver <- function(input, config) {
  if (!is.data.frame(input)) {
    return(list(status = "missing"))
  }

  driver_value <- config$m2_external_driver %||% NULL
  driver_name <- config$m2_external_driver_name %||% "external driver"
  if (is.character(driver_value) && length(driver_value) == 1 && driver_value %in% names(input)) {
    return(list(
      status = "column",
      name = driver_value,
      values = m2_num_vec(input[[driver_value]])
    ))
  }

  if (is.numeric(driver_value) && length(driver_value) == nrow(input)) {
    return(list(
      status = "vector",
      name = driver_name,
      values = m2_num_vec(driver_value)
    ))
  }

  candidate_cols <- c(config$m2_external_driver_column %||% character(0), "ExternalDriver", "Driver", "driver", "External_Signal")
  candidate_cols <- unique(candidate_cols[candidate_cols %in% names(input)])
  if (length(candidate_cols) > 0) {
    return(list(
      status = "column",
      name = candidate_cols[1],
      values = m2_num_vec(input[[candidate_cols[1]]])
    ))
  }

  list(status = "missing")
}

m2_test_cointegration_with_driver <- function(series_payload, driver_payload) {
  if (!(driver_payload$status %in% c("column", "vector"))) {
    return(m2_make_inconclusive_result(
      "Annual production is cointegrated with an external driver",
      "No long-run cointegrating relation with the external driver is supported",
      "Approximate Engle-Granger residual-stationarity test",
      "statistical",
      "No external driver was supplied for the cointegration test."
    ))
  }

  df <- data.frame(
    y = m2_num_vec(series_payload$articles),
    x = m2_num_vec(driver_payload$values)
  )
  keep <- stats::complete.cases(df)
  df <- df[keep, , drop = FALSE]
  if (nrow(df) < 8) {
    return(m2_make_inconclusive_result(
      "Annual production is cointegrated with an external driver",
      "No long-run cointegrating relation with the external driver is supported",
      "Approximate Engle-Granger residual-stationarity test",
      "statistical",
      "At least eight complete observations were required for the external-driver test."
    ))
  }

  fit <- tryCatch(stats::lm(y ~ x, data = df), error = function(e) NULL)
  if (is.null(fit)) {
    return(m2_make_inconclusive_result(
      "Annual production is cointegrated with an external driver",
      "No long-run cointegrating relation with the external driver is supported",
      "Approximate Engle-Granger residual-stationarity test",
      "statistical",
      "The long-run regression could not be estimated."
    ))
  }

  resid_payload <- list(
    years = seq_len(nrow(df)),
    articles = stats::residuals(fit)
  )
  adf <- m2_adf_test_simple(resid_payload)
  ci <- tryCatch(stats::confint(fit, parm = "x"), error = function(e) c(NA_real_, NA_real_))
  beta <- m2_scalar_num(stats::coef(fit)["x"])
  list(
    hypothesis = "Annual production is cointegrated with an external driver",
    null = "No long-run cointegrating relation with the external driver is supported",
    result = if (is.finite(adf$p_value) && adf$p_value < 0.05 && adf$gamma < 0) "reject" else "fail_to_reject",
    test = "Approximate Engle-Granger residual-stationarity test",
    evidence_class = "statistical",
    p_value = adf$p_value,
    effect_size = beta,
    ci_lower = m2_scalar_num(ci[1]),
    ci_upper = m2_scalar_num(ci[2]),
    interpretation = sprintf("The long-run association with %s had slope %.3f; residual ADF p-value = %.4f.", driver_payload$name, beta, adf$p_value)
  )
}

m2_test_granger_with_driver <- function(series_payload, driver_payload) {
  if (!(driver_payload$status %in% c("column", "vector"))) {
    return(m2_make_inconclusive_result(
      "The external driver Granger-causes annual production",
      "Lagged values of the external driver do not improve annual-production forecasts",
      "Lag-1 Granger-style nested regression",
      "statistical",
      "No external driver was supplied for the Granger-style test."
    ))
  }

  y <- m2_num_vec(series_payload$articles)
  x <- m2_num_vec(driver_payload$values)
  n <- min(length(y), length(x))
  y <- y[seq_len(n)]
  x <- x[seq_len(n)]
  if (n < 8) {
    return(m2_make_inconclusive_result(
      "The external driver Granger-causes annual production",
      "Lagged values of the external driver do not improve annual-production forecasts",
      "Lag-1 Granger-style nested regression",
      "statistical",
      "At least eight observations were required for the Granger-style test."
    ))
  }

  df <- data.frame(
    y = y[-1],
    y_lag = y[-n],
    x_lag = x[-n]
  )
  restricted <- tryCatch(stats::lm(y ~ y_lag, data = df), error = function(e) NULL)
  full <- tryCatch(stats::lm(y ~ y_lag + x_lag, data = df), error = function(e) NULL)
  if (is.null(restricted) || is.null(full)) {
    return(m2_make_inconclusive_result(
      "The external driver Granger-causes annual production",
      "Lagged values of the external driver do not improve annual-production forecasts",
      "Lag-1 Granger-style nested regression",
      "statistical",
      "The nested lag models could not be estimated."
    ))
  }

  cmp <- tryCatch(anova(restricted, full), error = function(e) NULL)
  p_value <- if (!is.null(cmp)) m2_scalar_num(cmp$`Pr(>F)`[2]) else NA_real_
  delta_r2 <- summary(full)$r.squared - summary(restricted)$r.squared

  list(
    hypothesis = "The external driver Granger-causes annual production",
    null = "Lagged values of the external driver do not improve annual-production forecasts",
    result = if (is.finite(p_value) && p_value < 0.05) "reject" else "fail_to_reject",
    test = "Lag-1 Granger-style nested regression",
    evidence_class = "statistical",
    p_value = p_value,
    effect_size = delta_r2,
    interpretation = sprintf("Adding the lagged external driver changed R-squared by %.3f.", delta_r2)
  )
}

m2_test_interrupted_time_series <- function(series_payload, config) {
  shock_year <- m2_scalar_num(config$m2_shock_year)
  data <- series_payload$data
  if (!is.finite(shock_year) || nrow(data) < 8) {
    return(m2_make_inconclusive_result(
      "A specified external shock altered the level or slope of the series",
      "The specified shock did not alter the level or slope of the series",
      "Interrupted time-series regression",
      "statistical",
      "No explicit shock year was supplied for interrupted time-series testing."
    ))
  }

  data$time <- seq_len(nrow(data))
  data$post <- as.integer(data$Year >= shock_year)
  data$time_after <- ifelse(data$post == 1, data$time - min(data$time[data$post == 1]) + 1, 0)
  if (sum(data$post == 0) < 4 || sum(data$post == 1) < 4) {
    return(m2_make_inconclusive_result(
      "A specified external shock altered the level or slope of the series",
      "The specified shock did not alter the level or slope of the series",
      "Interrupted time-series regression",
      "statistical",
      "At least four observations were required on each side of the shock year."
    ))
  }

  fit <- tryCatch(stats::lm(Articles ~ time + post + time_after, data = data), error = function(e) NULL)
  if (is.null(fit)) {
    return(m2_make_inconclusive_result(
      "A specified external shock altered the level or slope of the series",
      "The specified shock did not alter the level or slope of the series",
      "Interrupted time-series regression",
      "statistical",
      "The interrupted time-series regression could not be estimated."
    ))
  }

  coef_table <- summary(fit)$coefficients
  slope_term <- "time_after"
  p_value <- if (slope_term %in% rownames(coef_table)) m2_scalar_num(coef_table[slope_term, "Pr(>|t|)"]) else NA_real_
  effect <- if (slope_term %in% rownames(coef_table)) m2_scalar_num(coef_table[slope_term, "Estimate"]) else NA_real_
  ci <- tryCatch(stats::confint(fit, parm = slope_term), error = function(e) c(NA_real_, NA_real_))

  list(
    hypothesis = "A specified external shock altered the level or slope of the series",
    null = "The specified shock did not alter the level or slope of the series",
    result = if (is.finite(p_value) && p_value < 0.05) "reject" else "fail_to_reject",
    test = "Interrupted time-series regression",
    evidence_class = "statistical",
    p_value = p_value,
    effect_size = effect,
    ci_lower = m2_scalar_num(ci[1]),
    ci_upper = m2_scalar_num(ci[2]),
    interpretation = sprintf("The post-shock slope changed by %.3f articles/year after %.0f.", effect, shock_year)
  )
}

m2_extract_cluster_payload <- function(input, config) {
  if (!is.data.frame(input)) {
    return(list(status = "missing"))
  }

  candidate_cols <- c(config$m2_cluster_column %||% character(0), "Cluster", "cluster", "TopicCluster", "ThemeCluster")
  candidate_cols <- unique(candidate_cols[candidate_cols %in% names(input)])
  if (length(candidate_cols) == 0) {
    return(list(status = "missing"))
  }

  col <- candidate_cols[1]
  df <- data.frame(
    Year = m2_num_vec(input$Year),
    Articles = m2_num_vec(input$Articles),
    Cluster = as.character(input[[col]]),
    stringsAsFactors = FALSE
  )
  df <- df[stats::complete.cases(df), , drop = FALSE]
  if (nrow(df) == 0) {
    return(list(status = "missing"))
  }
  df <- stats::aggregate(Articles ~ Year + Cluster, data = df, FUN = sum)
  cluster_counts <- table(df$Cluster)

  list(
    status = "success",
    data = df,
    cluster_counts = cluster_counts,
    n_clusters = length(cluster_counts)
  )
}

m2_test_cluster_trajectory_heterogeneity <- function(cluster_payload) {
  if (!identical(cluster_payload$status, "success") || cluster_payload$n_clusters < 2) {
    return(m2_make_inconclusive_result(
      "Cluster-specific trajectories differ from one another",
      "Cluster trajectories do not differ materially over time",
      "Cluster-by-time interaction model",
      "statistical",
      "Cluster-resolved time-series data were not supplied."
    ))
  }

  df <- cluster_payload$data
  if (nrow(df) < 10) {
    return(m2_make_inconclusive_result(
      "Cluster-specific trajectories differ from one another",
      "Cluster trajectories do not differ materially over time",
      "Cluster-by-time interaction model",
      "statistical",
      "The cluster panel was too small for an interaction test."
    ))
  }

  reduced <- tryCatch(stats::lm(Articles ~ Year + Cluster, data = df), error = function(e) NULL)
  full <- tryCatch(stats::lm(Articles ~ Year * Cluster, data = df), error = function(e) NULL)
  if (is.null(reduced) || is.null(full)) {
    return(m2_make_inconclusive_result(
      "Cluster-specific trajectories differ from one another",
      "Cluster trajectories do not differ materially over time",
      "Cluster-by-time interaction model",
      "statistical",
      "The cluster interaction model could not be estimated."
    ))
  }

  cmp <- tryCatch(anova(reduced, full), error = function(e) NULL)
  p_value <- if (!is.null(cmp)) m2_scalar_num(cmp$`Pr(>F)`[2]) else NA_real_
  delta_r2 <- summary(full)$r.squared - summary(reduced)$r.squared

  list(
    hypothesis = "Cluster-specific trajectories differ from one another",
    null = "Cluster trajectories do not differ materially over time",
    result = if (is.finite(p_value) && p_value < 0.05) "reject" else "fail_to_reject",
    test = "Cluster-by-time interaction model",
    evidence_class = "statistical",
    p_value = p_value,
    effect_size = delta_r2,
    interpretation = sprintf("Allowing cluster-specific slopes changed R-squared by %.3f.", delta_r2)
  )
}

m2_test_cluster_divergence <- function(cluster_payload) {
  if (!identical(cluster_payload$status, "success") || cluster_payload$n_clusters < 2) {
    return(m2_make_inconclusive_result(
      "Clusters are diverging over time rather than converging",
      "Cross-cluster dispersion is not increasing over time",
      "Kendall trend on yearly cross-cluster dispersion",
      "statistical",
      "Cluster-resolved time-series data were not supplied."
    ))
  }

  yearly_dispersion <- stats::aggregate(Articles ~ Year, data = cluster_payload$data, FUN = function(x) stats::sd(x, na.rm = TRUE))
  if (nrow(yearly_dispersion) < 5) {
    return(m2_make_inconclusive_result(
      "Clusters are diverging over time rather than converging",
      "Cross-cluster dispersion is not increasing over time",
      "Kendall trend on yearly cross-cluster dispersion",
      "statistical",
      "Too few yearly dispersion estimates were available."
    ))
  }

  kt <- tryCatch(stats::cor.test(yearly_dispersion$Year, yearly_dispersion$Articles, method = "kendall", exact = FALSE), error = function(e) NULL)
  tau <- if (!is.null(kt)) m2_scalar_num(kt$estimate) else NA_real_
  p_value <- if (!is.null(kt)) m2_scalar_num(kt$p.value) else NA_real_

  list(
    hypothesis = "Clusters are diverging over time rather than converging",
    null = "Cross-cluster dispersion is not increasing over time",
    result = if (is.finite(p_value) && p_value < 0.05 && tau > 0) "reject" else "fail_to_reject",
    test = "Kendall trend on yearly cross-cluster dispersion",
    evidence_class = "statistical",
    p_value = p_value,
    effect_size = tau,
    interpretation = sprintf("Yearly cross-cluster dispersion had Kendall tau %.3f.", tau)
  )
}

m2_test_cluster_lifecycle_differential <- function(cluster_payload) {
  if (!identical(cluster_payload$status, "success") || cluster_payload$n_clusters < 2) {
    return(m2_make_inconclusive_result(
      "Clusters occupy statistically different lifecycle dynamics",
      "Clusters do not differ in lifecycle dynamics",
      "Cross-cluster growth-dynamics comparison",
      "statistical",
      "Cluster-resolved time-series data were not supplied."
    ))
  }

  df <- cluster_payload$data[order(cluster_payload$data$Cluster, cluster_payload$data$Year), , drop = FALSE]
  rows <- lapply(split(df, df$Cluster), function(chunk) {
    if (nrow(chunk) < 4) {
      return(NULL)
    }
    growth <- diff(chunk$Articles)
    data.frame(Cluster = chunk$Cluster[1], growth = growth, stringsAsFactors = FALSE)
  })
  growth_df <- dplyr::bind_rows(rows)
  if (!is.data.frame(growth_df) || nrow(growth_df) < 6 || length(unique(growth_df$Cluster)) < 2) {
    return(m2_make_inconclusive_result(
      "Clusters occupy statistically different lifecycle dynamics",
      "Clusters do not differ in lifecycle dynamics",
      "Cross-cluster growth-dynamics comparison",
      "statistical",
      "Insufficient within-cluster growth observations were available."
    ))
  }

  kw <- tryCatch(stats::kruskal.test(growth ~ Cluster, data = growth_df), error = function(e) NULL)
  if (is.null(kw)) {
    return(m2_make_inconclusive_result(
      "Clusters occupy statistically different lifecycle dynamics",
      "Clusters do not differ in lifecycle dynamics",
      "Cross-cluster growth-dynamics comparison",
      "statistical",
      "The cross-cluster growth comparison failed."
    ))
  }

  kw_p <- m2_scalar_num(kw$p.value)
  list(
    hypothesis = "Clusters occupy statistically different lifecycle dynamics",
    null = "Clusters do not differ in lifecycle dynamics",
    result = if (is.finite(kw_p) && kw_p < 0.05) "reject" else "fail_to_reject",
    test = "Cross-cluster growth-dynamics comparison",
    evidence_class = "statistical",
    p_value = kw_p,
    effect_size = m2_scalar_num(kw$statistic),
    interpretation = sprintf("Cross-cluster growth distributions yielded Kruskal-Wallis statistic %.3f.", m2_scalar_num(kw$statistic))
  )
}

m2_test_hierarchical_forecast_gain <- function(cluster_payload, config) {
  hierarchy <- config$m2_hierarchical_forecast %||% NULL
  if (is.list(hierarchy) && all(c("hierarchical_mae", "direct_mae") %in% names(hierarchy))) {
    delta <- m2_scalar_num(hierarchy$direct_mae) - m2_scalar_num(hierarchy$hierarchical_mae)
    return(list(
      hypothesis = "Hierarchical forecasting improves over direct aggregate forecasting",
      null = "Hierarchical forecasting does not improve over direct aggregate forecasting",
      result = if (is.finite(delta) && delta > 0) "reject" else "fail_to_reject",
      test = "Hierarchy-versus-direct forecast comparison",
      evidence_class = "statistical",
      effect_size = delta,
      p_value = m2_scalar_num(hierarchy$p_value),
      interpretation = sprintf(
        "Hierarchical MAE = %.3f versus direct aggregate MAE = %.3f.",
        m2_scalar_num(hierarchy$hierarchical_mae),
        m2_scalar_num(hierarchy$direct_mae)
      )
    ))
  }

  m2_make_inconclusive_result(
    "Hierarchical forecasting improves over direct aggregate forecasting",
    "Hierarchical forecasting does not improve over direct aggregate forecasting",
    "Hierarchy-versus-direct forecast comparison",
    "statistical",
    "Hierarchical forecast diagnostics were not supplied."
  )
}

m2_extract_scenario_payload <- function(input, config) {
  if (!is.data.frame(input)) {
    return(list(status = "missing"))
  }

  candidate_cols <- c(config$m2_scenario_column %||% character(0), "Scenario", "scenario", "SourceScenario")
  candidate_cols <- unique(candidate_cols[candidate_cols %in% names(input)])
  if (length(candidate_cols) == 0) {
    return(list(status = "missing"))
  }

  col <- candidate_cols[1]
  df <- data.frame(
    Year = m2_num_vec(input$Year),
    Articles = m2_num_vec(input$Articles),
    Scenario = as.character(input[[col]]),
    stringsAsFactors = FALSE
  )
  df <- df[stats::complete.cases(df), , drop = FALSE]
  df <- stats::aggregate(Articles ~ Year + Scenario, data = df, FUN = sum)
  list(status = "success", data = df)
}

m2_test_source_sensitivity_robustness <- function(scenario_payload) {
  if (!identical(scenario_payload$status, "success")) {
    return(m2_make_inconclusive_result(
      "Temporal conclusions are robust across source or deduplication scenarios",
      "Temporal conclusions are robust across scenarios",
      "Scenario-by-time interaction robustness test",
      "statistical",
      "No scenario-resolved sensitivity data were supplied."
    ))
  }

  df <- scenario_payload$data
  if (length(unique(df$Scenario)) < 2 || nrow(df) < 10) {
    return(m2_make_inconclusive_result(
      "Temporal conclusions are robust across source or deduplication scenarios",
      "Temporal conclusions are robust across scenarios",
      "Scenario-by-time interaction robustness test",
      "statistical",
      "At least two scenarios with sufficient overlap were required."
    ))
  }

  reduced <- tryCatch(stats::lm(Articles ~ Year + Scenario, data = df), error = function(e) NULL)
  full <- tryCatch(stats::lm(Articles ~ Year * Scenario, data = df), error = function(e) NULL)
  if (is.null(reduced) || is.null(full)) {
    return(m2_make_inconclusive_result(
      "Temporal conclusions are robust across source or deduplication scenarios",
      "Temporal conclusions are robust across scenarios",
      "Scenario-by-time interaction robustness test",
      "statistical",
      "The scenario robustness models could not be estimated."
    ))
  }

  cmp <- tryCatch(anova(reduced, full), error = function(e) NULL)
  p_value <- if (!is.null(cmp)) m2_scalar_num(cmp$`Pr(>F)`[2]) else NA_real_
  slopes <- vapply(split(df, df$Scenario), function(chunk) {
    fit <- tryCatch(stats::lm(Articles ~ Year, data = chunk), error = function(e) NULL)
    if (is.null(fit)) return(NA_real_)
    m2_scalar_num(stats::coef(fit)[2])
  }, numeric(1))
  slope_cv <- stats::sd(slopes, na.rm = TRUE) / pmax(abs(mean(slopes, na.rm = TRUE)), .Machine$double.eps)

  list(
    hypothesis = "Temporal conclusions are robust across source or deduplication scenarios",
    null = "Temporal conclusions are robust across scenarios",
    result = if (is.finite(p_value) && p_value >= 0.05 && is.finite(slope_cv) && slope_cv <= 0.25) "fail_to_reject" else "reject",
    test = "Scenario-by-time interaction robustness test",
    evidence_class = "statistical",
    p_value = p_value,
    effect_size = slope_cv,
    interpretation = sprintf("Scenario-specific slope coefficient of variation = %.3f.", slope_cv)
  )
}

m2_test_multiple_growth_regimes <- function(series_payload, changepoint, break_info) {
  data <- series_payload$data
  cp_years <- sort(unique(m2_num_vec(changepoint$summary$changepoint_years)))
  if (length(cp_years) < 2 || nrow(data) < 10) {
    return(list(
      hypothesis = "The series exhibits multiple growth regimes rather than a single-phase evolution",
      null = "A single growth regime is sufficient",
      result = "fail_to_reject",
      test = "Multi-segment versus single-trend comparison",
      evidence_class = "statistical",
      interpretation = "Fewer than two consensus breakpoints were available to support a multi-regime trajectory."
    ))
  }

  breaks_aug <- c(-Inf, cp_years, Inf)
  data$segment <- cut(data$Year, breaks = breaks_aug, labels = FALSE, right = TRUE)
  reduced <- tryCatch(stats::lm(Articles ~ Year, data = data), error = function(e) NULL)
  full <- tryCatch(stats::lm(Articles ~ Year * factor(segment), data = data), error = function(e) NULL)
  if (is.null(reduced) || is.null(full)) {
    return(m2_make_inconclusive_result(
      "The series exhibits multiple growth regimes rather than a single-phase evolution",
      "A single growth regime is sufficient",
      "Multi-segment versus single-trend comparison",
      "statistical",
      "The segmented growth-regime comparison could not be estimated."
    ))
  }

  cmp <- tryCatch(anova(reduced, full), error = function(e) NULL)
  p_value <- if (!is.null(cmp)) m2_scalar_num(cmp$`Pr(>F)`[2]) else NA_real_
  delta_r2 <- summary(full)$r.squared - summary(reduced)$r.squared
  list(
    hypothesis = "The series exhibits multiple growth regimes rather than a single-phase evolution",
    null = "A single growth regime is sufficient",
    result = if (is.finite(p_value) && p_value < 0.05) "reject" else "fail_to_reject",
    test = "Multi-segment versus single-trend comparison",
    evidence_class = "statistical",
    p_value = p_value,
    effect_size = delta_r2,
    interpretation = sprintf("Segmented regimes increased R-squared by %.3f across %d retained breaks.", delta_r2, length(cp_years))
  )
}

m2_test_forecast_probability_of_superiority <- function(forecasting) {
  cv_scores <- forecasting$cv_results$cv_scores %||% data.frame()
  best_model <- forecasting$model_comparison$best_model %||% NA_character_
  if (!is.data.frame(cv_scores) || nrow(cv_scores) == 0 || !all(c("fold", "model", "mae") %in% names(cv_scores)) || !is.character(best_model) || is.na(best_model)) {
    return(m2_make_inconclusive_result(
      "The selected forecast is more likely than not to outperform the naive baseline across folds",
      "The selected forecast is not more likely than not to outperform naive across folds",
      "Binomial probability-of-superiority test",
      "statistical",
      "Fold-level forecast errors were unavailable for the probability-of-superiority test."
    ))
  }

  best_rows <- cv_scores[cv_scores$model == best_model, c("fold", "mae"), drop = FALSE]
  naive_rows <- cv_scores[cv_scores$model == "Naive", c("fold", "mae"), drop = FALSE]
  paired <- merge(best_rows, naive_rows, by = "fold", suffixes = c("_best", "_naive"))
  if (nrow(paired) < 3) {
    return(m2_make_inconclusive_result(
      "The selected forecast is more likely than not to outperform the naive baseline across folds",
      "The selected forecast is not more likely than not to outperform naive across folds",
      "Binomial probability-of-superiority test",
      "statistical",
      "Fewer than three paired forecast folds were available."
    ))
  }

  wins <- sum(paired$mae_best < paired$mae_naive, na.rm = TRUE)
  bt <- tryCatch(stats::binom.test(wins, nrow(paired), p = 0.5, alternative = "greater"), error = function(e) NULL)
  p_value <- if (!is.null(bt)) m2_scalar_num(bt$p.value) else NA_real_
  ci <- if (!is.null(bt)) bt$conf.int else c(NA_real_, NA_real_)
  superiority <- safe_divide(wins, nrow(paired), default = NA_real_)

  list(
    hypothesis = "The selected forecast is more likely than not to outperform the naive baseline across folds",
    null = "The selected forecast is not more likely than not to outperform naive across folds",
    result = if (is.finite(p_value) && p_value < 0.05 && superiority > 0.5) "reject" else "fail_to_reject",
    test = "Binomial probability-of-superiority test",
    evidence_class = "statistical",
    p_value = p_value,
    effect_size = superiority,
    ci_lower = m2_scalar_num(ci[1]),
    ci_upper = m2_scalar_num(ci[2]),
    interpretation = sprintf("%s outperformed naive in %.1f%% of paired temporal CV folds.", best_model, 100 * superiority)
  )
}

m2_format_confidence_interval <- function(lower, upper) {
  if (!is.finite(lower) || !is.finite(upper)) {
    return(NA_character_)
  }
  sprintf("[%.3f, %.3f]", lower, upper)
}

m2_summarize_curated_hypotheses <- function(hypotheses) {
  decisions <- vapply(hypotheses, function(x) x$decision %||% "inconclusive", character(1))
  scopes <- vapply(hypotheses, function(x) x$reporting_scope %||% "appendix", character(1))
  n_supported <- sum(decisions == "supported", na.rm = TRUE)
  n_not_supported <- sum(decisions == "not_supported", na.rm = TRUE)
  n_inconclusive <- sum(decisions == "inconclusive", na.rm = TRUE)
  supported_ids <- names(hypotheses)[decisions == "supported"]
  main_text_idx <- scopes == "main_text"
  main_text_decisions <- decisions[main_text_idx]

  list(
    n_total = length(hypotheses),
    n_supported = n_supported,
    n_not_supported = n_not_supported,
    n_inconclusive = n_inconclusive,
    supported_ids = supported_ids,
    support_rate = safe_divide(n_supported, length(hypotheses), default = 0),
    n_main_text = sum(main_text_idx, na.rm = TRUE),
    n_appendix = sum(!main_text_idx, na.rm = TRUE),
    n_main_text_supported = sum(main_text_decisions == "supported", na.rm = TRUE),
    n_main_text_not_supported = sum(main_text_decisions == "not_supported", na.rm = TRUE),
    n_main_text_inconclusive = sum(main_text_decisions == "inconclusive", na.rm = TRUE)
  )
}

m2_hypothesis_reporting_scope <- function(id, decision = "inconclusive", evidence_class = "heuristic") {
  main_text_ids <- c(
    "H02_11", "H02_13", "H02_15", "H02_16", "H02_17", "H02_18",
    "H02_19", "H02_20", "H02_21", "H02_22", "H02_24", "H02_25",
    "H02_26", "H02_35", "H02_36"
  )

  if (id %in% main_text_ids) {
    return("main_text")
  }

  if (identical(evidence_class, "statistical") && identical(decision, "supported") && id %in% c("H02_10", "H02_12")) {
    return("main_text")
  }

  "appendix"
}

m2_order_hypotheses_for_reporting <- function(hypotheses) {
  if (length(hypotheses) == 0) {
    return(hypotheses)
  }

  priority_ids <- c(
    "H02_15", "H02_16", "H02_17", "H02_18", "H02_24", "H02_22",
    "H02_35", "H02_11", "H02_13", "H02_26", "H02_25", "H02_36",
    "H02_21", "H02_20", "H02_19", "H02_10", "H02_12"
  )
  ids <- names(hypotheses)
  priority_rank <- match(ids, priority_ids)
  priority_rank[is.na(priority_rank)] <- length(priority_ids) + seq_len(sum(is.na(priority_rank)))
  decision_rank <- vapply(
    hypotheses,
    function(x) switch(
      x$decision %||% "inconclusive",
      supported = 0,
      not_supported = 1,
      inconclusive = 2,
      3
    ),
    numeric(1)
  )
  evidence_rank <- vapply(hypotheses, function(x) if (identical(x$evidence_class, "statistical")) 0 else 1, numeric(1))
  p_rank <- vapply(hypotheses, function(x) m2_scalar_num(x$p_adjusted, default = m2_scalar_num(x$p_value, default = Inf)), numeric(1))
  hypotheses[order(priority_rank, decision_rank, evidence_rank, p_rank)]
}

m2_subset_hypotheses_by_scope <- function(hypotheses, scope = c("main_text", "appendix"), include_inconclusive = TRUE) {
  scope <- match.arg(scope)
  subset <- hypotheses[vapply(hypotheses, function(x) identical(x$reporting_scope %||% "appendix", scope), logical(1))]
  if (!include_inconclusive) {
    subset <- subset[vapply(subset, function(x) !identical(x$decision %||% "inconclusive", "inconclusive"), logical(1))]
  }
  m2_order_hypotheses_for_reporting(subset)
}

#' Build M2 regression tables
#' @export
build_m2_regression_table <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"comparison_table" %in% names(result)) {
    return(list(status = "stub", table = tibble::tibble()))
  }

  comparison <- tibble::as_tibble(m2_ensure_regression_comparison_columns(result$comparison_table))
  core_ranking <- comparison |>
    dplyr::filter(as.logical(.data$HeadlineEligible) %in% TRUE, is.finite(.data$CompositeScore)) |>
    dplyr::arrange(.data$NarrativeRank, .data$Rank, .data$RMSE)
  exploratory <- comparison |>
    dplyr::filter(!(as.logical(.data$HeadlineEligible) %in% TRUE) | as.logical(.data$BenchmarkOnly) %in% TRUE) |>
    dplyr::arrange(.data$Rank, .data$RMSE)

  selected <- result$best_model
  benchmark <- result$benchmark_best_model %||% list()
  params <- selected$parameter_summary %||% list()
  diagnostic_rows <- m2_regression_diagnostic_rows(result)
  limitations <- m2_regression_limitations(result)

  selected_model <- tibble::tibble(
    model = selected$name %||% NA_character_,
    family = selected$family %||% NA_character_,
    equation = selected$equation %||% NA_character_,
    benchmark_model = benchmark$name %||% NA_character_,
    benchmark_score = m2_scalar_num(benchmark$composite_score),
    selected_score = m2_scalar_num(selected$composite_score),
    adj_r2 = m2_scalar_num(selected$Adj_R2),
    rmse = m2_scalar_num(selected$RMSE),
    smape = m2_scalar_num(selected$SMAPE),
    aic = m2_scalar_num(selected$AIC),
    bic = m2_scalar_num(selected$BIC),
    carrying_capacity = m2_scalar_num(params$carrying_capacity),
    growth_rate = m2_scalar_num(params$growth_rate),
    inflection_year = m2_scalar_num(params$inflection_year),
    shape = m2_scalar_num(params$shape),
    headline_limitation = paste(limitations$issue[limitations$severity %in% c("moderate", "high")], collapse = " | ")
  )

  parameter_table <- tibble::tibble(
    parameter = c("carrying_capacity", "growth_rate", "inflection_year", "shape"),
    value = c(
      m2_scalar_num(params$carrying_capacity),
      m2_scalar_num(params$growth_rate),
      m2_scalar_num(params$inflection_year),
      m2_scalar_num(params$shape)
    )
  )

  list(
    status = "success",
    table = core_ranking,
    tables = list(
      core_ranking = core_ranking,
      exploratory_models = exploratory,
      selected_model = selected_model,
      parameter_summary = parameter_table,
      diagnostic_summary = diagnostic_rows,
      limitations = limitations
    )
  )
}

m2_regression_diagnostic_rows <- function(result) {
  performance <- result$performance %||% list()
  tibble::tibble(
    diagnostic = c("shapiro_p", "durbin_watson", "ljung_box_p", "breusch_pagan_p", "stability_score", "breakpoint_count"),
    value = c(
      m2_scalar_num(performance$shapiro_p),
      m2_scalar_num(performance$dw_statistic),
      m2_scalar_num(performance$ljung_box_p),
      m2_scalar_num(performance$breusch_pagan_p),
      m2_scalar_num(performance$stability_score),
      length(performance$breakpoint_years %||% numeric(0))
    ),
    pass = c(
      is.finite(m2_scalar_num(performance$shapiro_p)) && m2_scalar_num(performance$shapiro_p) >= 0.05,
      is.finite(m2_scalar_num(performance$dw_statistic)) && abs(m2_scalar_num(performance$dw_statistic) - 2) <= 0.5,
      is.finite(m2_scalar_num(performance$ljung_box_p)) && m2_scalar_num(performance$ljung_box_p) >= 0.05,
      is.finite(m2_scalar_num(performance$breusch_pagan_p)) && m2_scalar_num(performance$breusch_pagan_p) >= 0.05,
      is.finite(m2_scalar_num(performance$stability_score)) && m2_scalar_num(performance$stability_score) >= 0.60,
      length(performance$breakpoint_years %||% numeric(0)) <= 6
    ),
    interpretation = c(
      "Residual normality",
      "Low first-order autocorrelation",
      "No strong residual autocorrelation",
      "Approximate homoscedasticity",
      "Overall residual stability",
      "Parsimonious change-point narrative"
    )
  )
}

m2_regression_limitations <- function(result) {
  performance <- result$performance %||% list()
  rows <- list()

  if (is.finite(m2_scalar_num(performance$shapiro_p)) && m2_scalar_num(performance$shapiro_p) < 0.05) {
    rows[[length(rows) + 1L]] <- data.frame(
      severity = "moderate",
      issue = "Residual normality is not supported by Shapiro-Wilk.",
      evidence = sprintf("p = %.4f", m2_scalar_num(performance$shapiro_p)),
      stringsAsFactors = FALSE
    )
  }
  if (is.finite(m2_scalar_num(performance$breusch_pagan_p)) && m2_scalar_num(performance$breusch_pagan_p) < 0.05) {
    rows[[length(rows) + 1L]] <- data.frame(
      severity = "high",
      issue = "Residual variance is not constant.",
      evidence = sprintf("Breusch-Pagan p = %.4f", m2_scalar_num(performance$breusch_pagan_p)),
      stringsAsFactors = FALSE
    )
  }
  if (length(performance$breakpoint_years %||% numeric(0)) > 6) {
    rows[[length(rows) + 1L]] <- data.frame(
      severity = "moderate",
      issue = "Many candidate breakpoints were detected, suggesting a multi-phase evolution rather than a single discrete rupture.",
      evidence = paste(performance$breakpoint_years, collapse = ", "),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) {
    return(data.frame(
      severity = "low",
      issue = "No major regression limitation was flagged by the default diagnostics.",
      evidence = "All core checks passed or were inconclusive.",
      stringsAsFactors = FALSE
    ))
  }
  dplyr::bind_rows(rows)
}

#' Build M2 forecasting tables
#' @export
build_m2_forecasting_table <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || (result$status %||% "") != "success") {
    return(list(status = "stub", table = tibble::tibble()))
  }

  comparison <- tibble::as_tibble(result$model_comparison$comparison %||% data.frame())
  if (nrow(comparison) == 0) {
    return(list(status = "stub", table = tibble::tibble()))
  }

  selection_summary <- tibble::tibble(
    best_model = result$model_comparison$best_model %||% NA_character_,
    selection_basis = result$model_comparison$selection_basis %||% NA_character_,
    best_cv_mae = comparison$CV_MAE[match(result$model_comparison$best_model, comparison$model)] %||% NA_real_,
    naive_cv_mae = comparison$CV_MAE[match("Naive", comparison$model)] %||% NA_real_,
    relative_improvement_mae = comparison$RelativeImprovement_MAE[match(result$model_comparison$best_model, comparison$model)] %||% NA_real_,
    best_coverage_95 = comparison$Coverage95[match(result$model_comparison$best_model, comparison$model)] %||% NA_real_,
    best_interval_width_95 = comparison$IntervalWidth95[match(result$model_comparison$best_model, comparison$model)] %||% NA_real_,
    recommendation = result$model_comparison$recommendation %||% NA_character_
  )

  forecast_values <- create_forecast_values_table(result)
  cv_agg <- m2_extract_forecast_cv_aggregates(result$cv_results %||% list())
  cv_results <- if (is.data.frame(cv_agg) && nrow(cv_agg) > 0) {
    tibble::as_tibble(cv_agg)
  } else {
    tibble::tibble()
  }
  ensemble_weights <- if (!is.null(result$ensemble$weights)) {
    tibble::tibble(
      model = c("ARIMA", "ETS", "Naive"),
      weight = round(result$ensemble$weights, 4),
      aic = c(result$arima$AIC, result$ets$AIC, result$naive$AIC)
    )
  } else {
    tibble::tibble()
  }

  interval_calibration <- if (is.data.frame(result$cv_results$interval_coverage) && nrow(result$cv_results$interval_coverage) > 0) {
    tibble::as_tibble(result$cv_results$interval_coverage)
  } else {
    tibble::tibble()
  }

  list(
    status = "success",
    table = comparison,
    tables = list(
      model_comparison = comparison,
      selection_summary = selection_summary,
      forecasts = forecast_values,
      cv_results = cv_results,
      ensemble_weights = ensemble_weights,
      interval_calibration = interval_calibration
    )
  )
}

#' Build M2 hypotheses table
#' @export
build_m2_hypotheses_table <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || is.null(result$hypotheses) || length(result$hypotheses) == 0) {
    return(list(status = "stub", table = tibble::tibble()))
  }

  rows <- lapply(result$hypotheses, function(h) {
    tibble::tibble(
      hypothesis_id = h$hypothesis_id,
      question = h$question,
      test = h$test,
      effect_size = h$effect_size,
      confidence_interval = h$confidence_interval,
      p_value = h$p_value,
      p_adjusted = h$p_adjusted,
      decision = h$decision,
      evidence_class = h$evidence_class,
      reporting_scope = h$reporting_scope %||% "appendix",
      plain_language_interpretation = h$plain_language_interpretation
    )
  })
  table <- dplyr::bind_rows(rows) |>
    dplyr::arrange(
      dplyr::desc(.data$reporting_scope == "main_text"),
      dplyr::desc(.data$decision == "supported"),
      .data$p_adjusted,
      .data$p_value
    )
  main_text_table <- table[table$reporting_scope == "main_text", , drop = FALSE]
  appendix_table <- table[table$reporting_scope != "main_text", , drop = FALSE]

  summary_table <- tibble::tibble(
    metric = c(
      "n_total", "n_supported", "n_not_supported", "n_inconclusive", "support_rate",
      "n_main_text", "n_main_text_supported", "n_main_text_not_supported",
      "n_main_text_inconclusive", "n_appendix"
    ),
    value = c(
      result$summary$n_total,
      result$summary$n_supported,
      result$summary$n_not_supported,
      result$summary$n_inconclusive,
      result$summary$support_rate,
      result$summary$n_main_text,
      result$summary$n_main_text_supported,
      result$summary$n_main_text_not_supported,
      result$summary$n_main_text_inconclusive,
      result$summary$n_appendix
    )
  )

  list(
    status = "success",
    table = table,
    tables = list(
      hypotheses = table,
      main_text_hypotheses = main_text_table,
      appendix_hypotheses = appendix_table,
      summary = summary_table
    )
  )
}

m2_enhanced_time_series_validation <- function(years, articles, config) {
  years <- m2_num_vec(years)
  articles <- m2_num_vec(articles)
  keep <- is.finite(years) & is.finite(articles)
  years <- years[keep]
  articles <- articles[keep]
  n <- length(articles)
  min_train <- as.integer(m2_scalar_num(config$min_train_size, default = max(5, n %/% 3)))
  if (n < (min_train + 2L)) {
    return(list(status = "insufficient_data", cv_scores = data.frame(), cv_aggregated = data.frame(), interval_coverage = data.frame()))
  }

  n_folds <- min(5L, n - min_train)
  rows <- list()
  for (i in seq_len(n_folds)) {
    train_end <- min_train + i - 1L
    test_end <- train_end + 1L
    if (test_end > n) {
      next
    }

    train_years <- years[seq_len(train_end)]
    train_articles <- articles[seq_len(train_end)]
    actual <- articles[test_end]

    arima_fit <- fit_arima_model(train_years, train_articles)
    ets_fit <- fit_ets_model(train_years, train_articles)
    naive_fit <- fit_naive_forecast(train_years, train_articles)
    ensemble_fit <- create_ensemble_forecast(arima_fit, ets_fit, naive_fit, train_years, horizon = 1)
    pi <- compute_prediction_intervals(train_articles, arima_fit, ets_fit, horizon = 1)
    ensemble_pi <- m2_build_ensemble_interval(pi, naive_fit, ensemble_fit, train_articles)

    model_rows <- list(
      ARIMA = list(
        point = generate_forecast(arima_fit, 1)[1],
        lower_95 = m2_scalar_num(pi$arima$lower_95[1]),
        upper_95 = m2_scalar_num(pi$arima$upper_95[1])
      ),
      ETS = list(
        point = generate_forecast(ets_fit, 1)[1],
        lower_95 = m2_scalar_num(pi$ets$lower_95[1]),
        upper_95 = m2_scalar_num(pi$ets$upper_95[1])
      ),
      Naive = list(
        point = tail(train_articles, 1),
        lower_95 = m2_scalar_num(ensemble_pi$naive_lower_95[1]),
        upper_95 = m2_scalar_num(ensemble_pi$naive_upper_95[1])
      ),
      Ensemble = list(
        point = m2_scalar_num(ensemble_fit$forecast[1]),
        lower_95 = m2_scalar_num(ensemble_pi$lower_95[1]),
        upper_95 = m2_scalar_num(ensemble_pi$upper_95[1])
      )
    )

    for (model_name in names(model_rows)) {
      point <- m2_scalar_num(model_rows[[model_name]]$point)
      if (!is.finite(point)) {
        next
      }
      err <- actual - point
      lower_95 <- m2_scalar_num(model_rows[[model_name]]$lower_95)
      upper_95 <- m2_scalar_num(model_rows[[model_name]]$upper_95)
      rows[[length(rows) + 1L]] <- data.frame(
        fold = i,
        train_size = train_end,
        model = model_name,
        actual = actual,
        forecast = point,
        mae = abs(err),
        rmse = sqrt(err^2),
        mape = if (abs(actual) > .Machine$double.eps) abs(err / actual) * 100 else NA_real_,
        lower_95 = lower_95,
        upper_95 = upper_95,
        covered_95 = if (is.finite(lower_95) && is.finite(upper_95)) as.integer(actual >= lower_95 && actual <= upper_95) else NA_integer_,
        interval_width_95 = if (is.finite(lower_95) && is.finite(upper_95)) (upper_95 - lower_95) else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0L) {
    return(list(status = "no_cv_folds", cv_scores = data.frame(), cv_aggregated = data.frame(), interval_coverage = data.frame()))
  }

  cv_scores <- dplyr::bind_rows(rows)
  cv_aggregated <- stats::aggregate(
    cbind(mae, rmse, mape, covered_95, interval_width_95) ~ model,
    data = cv_scores,
    FUN = function(x) mean(as.numeric(x), na.rm = TRUE)
  )
  names(cv_aggregated)[names(cv_aggregated) == "covered_95"] <- "coverage_95"
  names(cv_aggregated)[names(cv_aggregated) == "interval_width_95"] <- "mean_interval_width_95"
  interval_coverage <- cv_aggregated[, c("model", "coverage_95", "mean_interval_width_95"), drop = FALSE]
  names(interval_coverage)[names(interval_coverage) == "coverage_95"] <- "coverage"
  names(interval_coverage)[names(interval_coverage) == "mean_interval_width_95"] <- "mean_interval_width"
  interval_coverage$nominal <- 0.95
  interval_coverage$n_folds <- vapply(split(cv_scores$fold, cv_scores$model), function(x) length(unique(x)), integer(1))

  list(
    status = "success",
    cv_scores = cv_scores,
    cv_aggregated = cv_aggregated,
    interval_coverage = interval_coverage,
    mean_mae = mean(cv_scores$mae, na.rm = TRUE),
    mean_rmse = mean(cv_scores$rmse, na.rm = TRUE),
    best_model = cv_aggregated$model[which.min(cv_aggregated$mae)][1]
  )
}

m2_build_ensemble_interval <- function(prediction_intervals, naive_fit, ensemble_fit, train_articles) {
  sigma_naive <- stats::sd(m2_num_vec(naive_fit$residuals), na.rm = TRUE)
  sigma_naive <- if (is.finite(sigma_naive) && sigma_naive > .Machine$double.eps) sigma_naive else max(stats::sd(m2_num_vec(train_articles), na.rm = TRUE), 1)
  naive_point <- m2_scalar_num(tail(train_articles, 1))
  horizon <- max(length(m2_num_vec(prediction_intervals$arima$lower_95)), length(m2_num_vec(prediction_intervals$ets$lower_95)), 1L)
  naive_lower_95 <- naive_point - 1.96 * sigma_naive * sqrt(seq_len(horizon))
  naive_upper_95 <- naive_point + 1.96 * sigma_naive * sqrt(seq_len(horizon))

  weights <- ensemble_fit$weights %||% c(1 / 3, 1 / 3, 1 / 3)
  if (length(weights) < 3) {
    weights <- c(weights, rep(0, 3 - length(weights)))
  }
  arima_lower <- m2_num_vec(prediction_intervals$arima$lower_95)
  arima_upper <- m2_num_vec(prediction_intervals$arima$upper_95)
  ets_lower <- m2_num_vec(prediction_intervals$ets$lower_95)
  ets_upper <- m2_num_vec(prediction_intervals$ets$upper_95)
  if (length(arima_lower) < horizon) arima_lower <- rep(tail(arima_lower, 1), horizon)
  if (length(arima_upper) < horizon) arima_upper <- rep(tail(arima_upper, 1), horizon)
  if (length(ets_lower) < horizon) ets_lower <- rep(tail(ets_lower, 1), horizon)
  if (length(ets_upper) < horizon) ets_upper <- rep(tail(ets_upper, 1), horizon)
  lower_95 <- weights[1] * arima_lower[seq_len(horizon)] +
    weights[2] * ets_lower[seq_len(horizon)] +
    weights[3] * naive_lower_95
  upper_95 <- weights[1] * arima_upper[seq_len(horizon)] +
    weights[2] * ets_upper[seq_len(horizon)] +
    weights[3] * naive_upper_95

  list(
    lower_95 = lower_95,
    upper_95 = upper_95,
    naive_lower_95 = naive_lower_95,
    naive_upper_95 = naive_upper_95
  )
}

#' Compare forecast models with aligned CV metrics
#' @keywords internal
compare_forecast_models <- function(arima_result, ets_result, naive_result, cv_results) {
  models <- data.frame(
    model = c("ARIMA", "ETS", "Naive"),
    AIC = c(arima_result$AIC, ets_result$AIC, naive_result$AIC),
    BIC = c(arima_result$BIC, ets_result$BIC, naive_result$BIC),
    stringsAsFactors = FALSE
  )

  cv_agg <- m2_extract_forecast_cv_aggregates(cv_results)
  if (is.data.frame(cv_agg) && nrow(cv_agg) > 0) {
    cv_agg$model <- trimws(as.character(cv_agg$model))
    models$model <- trimws(as.character(models$model))
    extra_models <- setdiff(unique(cv_agg$model), models$model)
    if (length(extra_models) > 0) {
      models <- dplyr::bind_rows(
        models,
        data.frame(model = extra_models, AIC = NA_real_, BIC = NA_real_, stringsAsFactors = FALSE)
      )
    }
    models <- dplyr::left_join(models, cv_agg, by = "model")
    if ("mae" %in% names(models)) names(models)[names(models) == "mae"] <- "CV_MAE"
    if ("rmse" %in% names(models)) names(models)[names(models) == "rmse"] <- "CV_RMSE"
    if ("mape" %in% names(models)) names(models)[names(models) == "mape"] <- "CV_MAPE"
    if ("coverage_95" %in% names(models)) names(models)[names(models) == "coverage_95"] <- "Coverage95"
    if ("mean_interval_width_95" %in% names(models)) names(models)[names(models) == "mean_interval_width_95"] <- "IntervalWidth95"
  } else {
    models$CV_MAE <- NA_real_
    models$CV_RMSE <- NA_real_
    models$CV_MAPE <- NA_real_
    models$Coverage95 <- NA_real_
    models$IntervalWidth95 <- NA_real_
  }

  models$AIC_score <- m2_normalize_metric(models$AIC, higher_is_better = FALSE)
  models$BIC_score <- m2_normalize_metric(models$BIC, higher_is_better = FALSE)
  models$CV_MAE_score <- m2_normalize_metric(models$CV_MAE, higher_is_better = FALSE)
  models$CV_RMSE_score <- m2_normalize_metric(models$CV_RMSE, higher_is_better = FALSE)
  models$CV_MAPE_score <- m2_normalize_metric(models$CV_MAPE, higher_is_better = FALSE)
  models$Coverage95_score <- if ("Coverage95" %in% names(models)) 1 - pmin(abs(models$Coverage95 - 0.95) / 0.95, 1) else NA_real_
  models$IntervalWidth95_score <- if ("IntervalWidth95" %in% names(models)) m2_normalize_metric(models$IntervalWidth95, higher_is_better = FALSE) else NA_real_
  score_cols <- c("AIC_score", "BIC_score", "CV_MAE_score", "CV_RMSE_score", "CV_MAPE_score", "Coverage95_score", "IntervalWidth95_score")
  models$EvidenceCoverage <- rowMeans(!is.na(models[, score_cols, drop = FALSE]))
  models$CompositeScore <- rowMeans(models[, score_cols, drop = FALSE], na.rm = TRUE)
  models$ComparableForCV <- is.finite(models$CV_MAE) | is.finite(models$CV_RMSE) | is.finite(models$CV_MAPE)

  naive_mae <- models$CV_MAE[models$model == "Naive"][1]
  naive_rmse <- models$CV_RMSE[models$model == "Naive"][1]
  models$DeltaVsNaive_MAE <- naive_mae - models$CV_MAE
  models$DeltaVsNaive_RMSE <- naive_rmse - models$CV_RMSE
  models$RelativeImprovement_MAE <- 100 * safe_divide(models$DeltaVsNaive_MAE, naive_mae, default = NA_real_)
  models$RelativeImprovement_RMSE <- 100 * safe_divide(models$DeltaVsNaive_RMSE, naive_rmse, default = NA_real_)

  comparable_rows <- models[models$ComparableForCV %in% TRUE, , drop = FALSE]
  if (nrow(comparable_rows) >= 2) {
    comparable_rows$SelectionScore <- comparable_rows$CompositeScore * comparable_rows$EvidenceCoverage
    best_model <- comparable_rows$model[which.max(comparable_rows$SelectionScore)][1]
    selection_basis <- "temporal_cv_and_information_criteria"
    recommendation <- sprintf(
      "Recommended forecast model: %s. Ranking combines information criteria, rolling temporal CV error, and interval calibration against the naive baseline.",
      best_model
    )
  } else {
    models$SelectionScore <- models$CompositeScore
    best_model <- models$model[which.max(models$SelectionScore)][1]
    selection_basis <- "provisional_information_criteria"
    recommendation <- sprintf(
      "Recommended forecast model: %s. Selection is provisional because comparable temporal CV metrics were unavailable for at least two candidate models.",
      best_model
    )
  }

  if (!"SelectionScore" %in% names(models)) {
    models$SelectionScore <- models$CompositeScore
  }
  models$CompositeRank <- rank(-models$SelectionScore, ties.method = "first", na.last = "keep")
  list(
    comparison = models[order(models$CompositeRank, models$CV_MAE), , drop = FALSE],
    best_model = best_model,
    selection_basis = selection_basis,
    recommendation = recommendation
  )
}

m2_extract_forecast_cv_aggregates <- function(cv_results) {
  if (is.data.frame(cv_results$cv_aggregated) && nrow(cv_results$cv_aggregated) > 0) {
    return(cv_results$cv_aggregated)
  }

  cv_scores <- cv_results$cv_scores %||% data.frame()
  if (!is.data.frame(cv_scores) || nrow(cv_scores) == 0 || !all(c("model", "mae", "rmse", "mape") %in% names(cv_scores))) {
    return(data.frame())
  }

  aggregate(cbind(mae, rmse, mape) ~ model, data = cv_scores, FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
}

#' Build M2 report payload
#' @keywords internal
build_m2_report <- function(data, config = biblio_config()) {
  best_reg <- data$regression$best_model %||% list()
  benchmark_reg <- data$regression$benchmark_best_model %||% list()
  trend_stats <- data$diagnostics$trend_statistics %||% list()
  forecast_cmp <- data$forecasting$model_comparison$comparison %||% data.frame()
  best_forecast <- data$forecasting$model_comparison$best_model %||% NA_character_
  forecast_basis <- data$forecasting$model_comparison$selection_basis %||% "unknown"
  limitations <- m2_regression_limitations(data$regression)
  all_hypotheses <- data$hypotheses$hypotheses %||% list()
  main_text_hypotheses <- m2_subset_hypotheses_by_scope(all_hypotheses, "main_text", include_inconclusive = TRUE)
  appendix_hypotheses <- m2_subset_hypotheses_by_scope(all_hypotheses, "appendix", include_inconclusive = TRUE)
  top_supported <- utils::head(
    main_text_hypotheses[vapply(main_text_hypotheses, function(x) identical(x$decision, "supported"), logical(1))],
    6
  )
  cautionary_findings <- utils::head(
    main_text_hypotheses[vapply(main_text_hypotheses, function(x) !identical(x$decision, "supported"), logical(1))],
    4
  )
  hypothesis_summary <- data$hypotheses$summary %||% list()
  params <- best_reg$parameter_summary %||% list()
  mk_tau <- m2_extract_kendall_tau(trend_stats, data$hypotheses)
  forecast_limitations <- m2_forecasting_limitations(forecast_cmp, best_forecast, forecast_basis)

  forecast_line <- "Forecast superiority over naive could not be quantified."
  coverage_line <- "Interval calibration could not be quantified."
  if (is.data.frame(forecast_cmp) && nrow(forecast_cmp) > 0) {
    best_row <- forecast_cmp[forecast_cmp$model == best_forecast, , drop = FALSE]
    if (nrow(best_row) == 1) {
      best_cv_mae <- m2_scalar_num(best_row$CV_MAE)
      naive_cv_mae <- m2_scalar_num(forecast_cmp$CV_MAE[forecast_cmp$model == "Naive"][1])
      relative_improvement <- m2_scalar_num(best_row$RelativeImprovement_MAE)
      coverage_95 <- m2_scalar_num(best_row$Coverage95[1])
      width_95 <- m2_scalar_num(best_row$IntervalWidth95[1])
      if (is.finite(best_cv_mae) && is.finite(naive_cv_mae) && is.finite(relative_improvement)) {
        forecast_line <- sprintf(
          "%s achieved CV MAE %.3f versus naive %.3f (relative improvement %.1f%%).",
          best_forecast,
          best_cv_mae,
          naive_cv_mae,
          relative_improvement
        )
      } else if (identical(forecast_basis, "provisional_information_criteria")) {
        forecast_line <- sprintf(
          "%s was selected provisionally from information criteria because rolling temporal CV metrics were unavailable for direct comparison. Naive baseline CV MAE = %.3f.",
          best_forecast,
          naive_cv_mae
        )
      }
      if (is.finite(coverage_95) && is.finite(width_95)) {
        coverage_line <- if (coverage_95 < 0.80) {
          sprintf(
            "Interval calibration is weak: observed 95%% coverage for %s = %.1f%% with mean width %.3f, so forecast uncertainty is likely understated.",
            best_forecast,
            100 * coverage_95,
            width_95
          )
        } else {
          sprintf(
            "Observed 95%% interval coverage for %s = %.1f%% with mean width %.3f.",
            best_forecast,
            100 * coverage_95,
            width_95
          )
        }
      }
    }
  }

  breakpoints <- trend_stats$breakpoint_years %||% numeric(0)
  breakpoint_story <- if (length(breakpoints) == 0) {
    "No stable breakpoint consensus was detected."
  } else if (length(breakpoints) <= 6) {
    paste("Consensus breakpoints were detected around", paste(breakpoints, collapse = ", "), ".")
  } else {
    paste(
      "Many candidate breakpoints were detected (",
      length(breakpoints),
      " total), indicating a multi-phase trajectory rather than a single rupture.",
      sep = ""
    )
  }

  parameter_story <- paste(
    c(
      if (is.finite(m2_scalar_num(params$carrying_capacity))) sprintf("Estimated carrying capacity: %.2f.", m2_scalar_num(params$carrying_capacity)) else NULL,
      if (is.finite(m2_scalar_num(params$growth_rate))) sprintf("Estimated growth-rate parameter: %.4f.", m2_scalar_num(params$growth_rate)) else NULL,
      if (is.finite(m2_scalar_num(params$inflection_year))) sprintf("Estimated inflection year: %.2f.", m2_scalar_num(params$inflection_year)) else NULL
    ),
    collapse = " "
  )

  lines <- c(
    "============================================================",
    "M2 Annual Production Report",
    paste("Generated:", Sys.time()),
    "============================================================",
    "",
    "Executive Summary",
    sprintf("  Headline model: %s", best_reg$name %||% "NA"),
    if (identical(best_reg$name %||% NA_character_, benchmark_reg$name %||% "__different__")) {
      "  Flexible benchmark: none beyond the selected headline model"
    } else {
      sprintf("  Flexible benchmark: %s", benchmark_reg$name %||% "NA")
    },
    sprintf("  Composite score: %.4f", m2_scalar_num(best_reg$composite_score)),
    sprintf("  Best forecasting model: %s", best_forecast),
    sprintf(
      "  Hypothesis balance: %d supported / %d not supported / %d inconclusive",
      m2_scalar_num(hypothesis_summary$n_supported, default = 0),
      m2_scalar_num(hypothesis_summary$n_not_supported, default = 0),
      m2_scalar_num(hypothesis_summary$n_inconclusive, default = 0)
    ),
    sprintf(
      "  Main-text hypothesis set: %d primary findings tracked (%d supported / %d cautionary / %d inconclusive).",
      m2_scalar_num(hypothesis_summary$n_main_text, default = 0),
      m2_scalar_num(hypothesis_summary$n_main_text_supported, default = 0),
      m2_scalar_num(hypothesis_summary$n_main_text_not_supported, default = 0),
      m2_scalar_num(hypothesis_summary$n_main_text_inconclusive, default = 0)
    ),
    "",
    "Narrative Evidence",
    paste0("  ", m2_report_narrative(data$narrative %||% list())),
    "",
    "Trend and Growth",
    sprintf("  CAGR: %.2f%%", 100 * m2_scalar_num(trend_stats$cagr)),
    sprintf("  Recent CAGR: %.2f%%", 100 * m2_scalar_num(trend_stats$recent_cagr)),
    sprintf("  Sen slope: %.3f articles/year", m2_scalar_num(trend_stats$sen_slope)),
    sprintf("  Mann-Kendall tau: %.3f (p = %.4f)", mk_tau, m2_scalar_num(trend_stats$mann_kendall$p_value)),
    sprintf("  Hurst exponent: %.3f", m2_scalar_num(trend_stats$hurst_exponent)),
    paste0("  ", breakpoint_story),
    if (nzchar(parameter_story)) paste0("  ", parameter_story) else NULL,
    "",
    "Forecasting",
    paste0("  ", forecast_line),
    paste0("  ", coverage_line),
    paste0("  Selection basis: ", forecast_basis),
    "",
    m2_advanced_report_lines(data$advanced_journal %||% list()),
    "",
    "Model Adequacy and Limitations",
    sprintf("  Residual normality p-value: %.4f", m2_scalar_num(data$regression$performance$shapiro_p)),
    sprintf("  Breusch-Pagan p-value: %.4f", m2_scalar_num(data$regression$performance$breusch_pagan_p)),
    sprintf("  Durbin-Watson statistic: %.3f", m2_scalar_num(data$regression$performance$dw_statistic)),
    paste0("  Selection rationale: ", best_reg$selection_reason %||% "NA"),
    paste0("  Limitation: ", limitations$issue),
    paste0("  Forecast limitation: ", forecast_limitations),
    "",
    "Primary Supported Findings"
  )

  if (length(top_supported) > 0) {
    for (item in top_supported) {
      lines <- c(lines, paste0("  - ", item$question, " ", item$plain_language_interpretation))
    }
  } else {
    lines <- c(lines, "  - No hypothesis reached the supported threshold under the current defaults.")
  }

  lines <- c(lines, "", "Primary Cautionary Findings")
  if (length(cautionary_findings) > 0) {
    for (item in cautionary_findings) {
      lines <- c(
        lines,
        paste0(
          "  - ",
          item$question,
          " ",
          item$plain_language_interpretation,
          if (identical(item$decision, "inconclusive")) " This remained inconclusive under the available evidence." else ""
        )
      )
    }
  } else {
    lines <- c(lines, "  - No main-text cautionary findings were triggered under the current defaults.")
  }

  lines <- c(
    lines,
    "",
    "Appendix Scope",
    sprintf(
      "  %d secondary or diagnostic hypotheses were retained for appendix interpretation rather than the main narrative.",
      length(appendix_hypotheses)
    )
  )

  tex <- c(
    "\\section*{M2 Annual Production Report}",
    paste0("\\textbf{Headline model}: ", best_reg$name %||% "NA", "\\\\"),
    paste0(
      "\\textbf{Flexible benchmark}: ",
      m0_escape_latex(if (identical(best_reg$name %||% NA_character_, benchmark_reg$name %||% "__different__")) {
        "none beyond the selected headline model"
      } else {
        benchmark_reg$name %||% "NA"
      }),
      "\\\\"
    ),
    paste0("\\textbf{Composite score}: ", sprintf("%.4f", m2_scalar_num(best_reg$composite_score)), "\\\\"),
    paste0("\\textbf{Best forecasting model}: ", best_forecast, "\\\\"),
    "\\subsection*{Narrative Evidence}",
    paste(m0_escape_latex(m2_report_narrative(data$narrative %||% list())), collapse = "\\\\"),
    paste0(
      "\\textbf{Main-text hypothesis set}: ",
      sprintf(
        "%d primary findings tracked (%d supported / %d cautionary / %d inconclusive).",
        m2_scalar_num(hypothesis_summary$n_main_text, default = 0),
        m2_scalar_num(hypothesis_summary$n_main_text_supported, default = 0),
        m2_scalar_num(hypothesis_summary$n_main_text_not_supported, default = 0),
        m2_scalar_num(hypothesis_summary$n_main_text_inconclusive, default = 0)
      ),
      "\\\\"
    ),
    paste0("\\textbf{CAGR}: ", sprintf("%.2f", 100 * m2_scalar_num(trend_stats$cagr)), "\\%\\\\"),
    paste0("\\textbf{Recent CAGR}: ", sprintf("%.2f", 100 * m2_scalar_num(trend_stats$recent_cagr)), "\\%\\\\"),
    paste0("\\textbf{Sen slope}: ", sprintf("%.3f", m2_scalar_num(trend_stats$sen_slope)), " articles/year\\\\"),
    paste0("\\textbf{Forecasting}: ", m0_escape_latex(forecast_line), "\\\\"),
    paste0("\\textbf{Intervals}: ", m0_escape_latex(coverage_line), "\\\\"),
    paste0("\\textbf{Breakpoints}: ", m0_escape_latex(breakpoint_story), "\\\\"),
    m2_advanced_report_tex(data$advanced_journal %||% list()),
    paste0("\\textbf{Limitations}: ", m0_escape_latex(paste(limitations$issue, collapse = " ")), "\\\\")
  )

  list(lines = lines, tex = tex)
}

m2_prioritize_supported_hypotheses <- function(hypotheses) {
  if (length(hypotheses) == 0) {
    return(hypotheses)
  }
  priority_ids <- c(
    "H02_15", "H02_16", "H02_17", "H02_18", "H02_19", "H02_20",
    "H02_21", "H02_22", "H02_23", "H02_24", "H02_25", "H02_26",
    "H02_27", "H02_28", "H02_29", "H02_30", "H02_31", "H02_32",
    "H02_33", "H02_34", "H02_35", "H02_36", "H02_13", "H02_14",
    "H02_10", "H02_11", "H02_12"
  )
  ids <- names(hypotheses)
  priority_rank <- match(ids, priority_ids)
  priority_rank[is.na(priority_rank)] <- length(priority_ids) + seq_len(sum(is.na(priority_rank)))
  evidence_rank <- vapply(hypotheses, function(x) if (identical(x$evidence_class, "statistical")) 0 else 1, numeric(1))
  p_rank <- vapply(hypotheses, function(x) m2_scalar_num(x$p_adjusted, default = m2_scalar_num(x$p_value, default = Inf)), numeric(1))
  hypotheses[order(priority_rank, evidence_rank, p_rank)]
}

m2_extract_kendall_tau <- function(trend_stats, hypotheses) {
  tau <- m2_scalar_num(trend_stats$mann_kendall$tau)
  if (is.finite(tau)) {
    return(tau)
  }
  h <- hypotheses$hypotheses$H02_11 %||% list()
  tau <- m2_scalar_num(h$effect_size)
  if (is.finite(tau)) {
    return(tau)
  }
  tau <- m2_extract_kendall_from_text(h$plain_language_interpretation %||% "")
  if (is.finite(tau)) {
    return(tau)
  }
  NA_real_
}

m2_forecasting_limitations <- function(forecast_cmp, best_forecast, forecast_basis) {
  issues <- character()

  if (identical(forecast_basis, "provisional_information_criteria")) {
    issues <- c(
      issues,
      "The winning forecast is provisional because rolling temporal cross-validation was incomplete."
    )
  }

  if (is.data.frame(forecast_cmp) && nrow(forecast_cmp) > 0) {
    best_row <- forecast_cmp[forecast_cmp$model == best_forecast, , drop = FALSE]
    if (nrow(best_row) == 1) {
      coverage_95 <- m2_scalar_num(best_row$Coverage95[1])
      width_95 <- m2_scalar_num(best_row$IntervalWidth95[1])
      if (is.finite(coverage_95) && coverage_95 < 0.80) {
        issues <- c(
          issues,
          sprintf(
            "The selected model's nominal 95%% intervals covered only %.1f%% of held-out folds, indicating under-calibrated uncertainty.",
            100 * coverage_95
          )
        )
      }
      if (is.finite(width_95) && width_95 <= 0) {
        issues <- c(
          issues,
          "Prediction intervals had non-positive average width, indicating a malformed uncertainty estimate."
        )
      }
    }
  }

  if (length(issues) == 0) {
    issues <- "No major forecasting limitation was flagged by the default backtesting diagnostics."
  }

  issues
}

m2_forecast_calibration_summary <- function(interval_calibration, best_model) {
  if (!is.data.frame(interval_calibration) || nrow(interval_calibration) == 0) {
    return(list(status = "unavailable"))
  }

  row <- interval_calibration[interval_calibration$model == best_model, , drop = FALSE]
  if (nrow(row) == 0) {
    row <- interval_calibration[1, , drop = FALSE]
  }

  coverage <- m2_scalar_num(row$coverage[1])
  nominal <- m2_scalar_num(row$nominal[1])
  mean_width <- m2_scalar_num(row$mean_interval_width[1])
  gap <- abs(coverage - nominal)

  list(
    status = "success",
    model = as.character(row$model[1]),
    coverage = coverage,
    nominal = nominal,
    coverage_gap = gap,
    mean_interval_width = mean_width,
    calibrated = is.finite(gap) && gap <= 0.10
  )
}

#' Export M2 artifacts with curated JSON payloads
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
        spec <- ieee_get_plot_export_spec(plot_obj, config = config, section_id = nm, plot_id = pnm)
        p <- build_artifact_path("m2", "plots", paste0("m2_", nm, "_", pnm), "png", config)
        tryCatch({
          exported_paths <- export_plot_artifact(
            plot_obj,
            tools::file_path_sans_ext(p),
            width = spec$width,
            height = spec$height,
            dpi = spec$dpi
          )
          exported_plots <- c(exported_plots, unname(exported_paths[!is.na(exported_paths)]))
        }, error = function(e) NULL)
      }
    }
  }

  if (config$export_json) {
    for (nm in names(result$data)) {
      payload <- m2_prepare_json_export_payload(nm, result$data[[nm]])
      j <- build_artifact_path("m2", "json", paste0("m2_", nm), "json", config)
      tryCatch({
        write_json_artifact(payload, j)
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

m2_prepare_json_export_payload <- function(name, payload) {
  if (identical(name, "model_registry")) {
    return(list(
      status = "success",
      annual_models = names(payload$annual %||% list()),
      cumulative_models = names(payload$cumulative %||% list()),
      n_annual_models = length(payload$annual %||% list()),
      n_cumulative_models = length(payload$cumulative %||% list())
    ))
  }

  if (identical(name, "regression")) {
    reg_tables <- build_m2_regression_table(payload)$tables
    return(list(
      status = payload$status,
      best_model = payload$best_model,
      benchmark_best_model = payload$benchmark_best_model,
      core_ranking = reg_tables$core_ranking,
      exploratory_models = reg_tables$exploratory_models,
      diagnostic_summary = reg_tables$diagnostic_summary,
      limitations = reg_tables$limitations,
      family_summary = payload$family_summary
    ))
  }

  if (identical(name, "forecasting")) {
    fc_tables <- build_m2_forecasting_table(payload)$tables
    return(list(
      status = payload$status,
      best_model = payload$model_comparison$best_model,
      selection_basis = payload$model_comparison$selection_basis,
      recommendation = payload$model_comparison$recommendation,
      model_comparison = fc_tables$model_comparison,
      selection_summary = fc_tables$selection_summary,
      cv_results = fc_tables$cv_results,
      ensemble_weights = fc_tables$ensemble_weights,
      interval_calibration = fc_tables$interval_calibration,
      calibration_summary = m2_forecast_calibration_summary(
        fc_tables$interval_calibration,
        payload$model_comparison$best_model
      ),
      forecast_summary = payload$forecast_summary,
      horizon = payload$horizon
    ))
  }

  if (identical(name, "hypotheses")) {
    hyp_tables <- build_m2_hypotheses_table(payload)$tables
    return(list(
      status = payload$status,
      summary = payload$summary,
      hypotheses = hyp_tables$hypotheses,
      main_text_hypotheses = hyp_tables$main_text_hypotheses,
      appendix_hypotheses = hyp_tables$appendix_hypotheses
    ))
  }

  payload
}

#' Create main forecast plot
#' @keywords internal
create_forecast_plot <- function(data, config) {
  years <- data$years
  articles <- data$articles
  horizon <- data$horizon
  last_year <- max(years)
  forecast_years <- (last_year + 1):(last_year + horizon)
  best_model <- data$model_comparison$best_model %||% "ETS"

  observed <- data.frame(Year = years, Articles = articles, stringsAsFactors = FALSE)
  fitted <- m2_get_forecast_fitted_series(data, best_model)
  interval_df <- m2_get_forecast_interval_df(data, best_model)
  future_df <- data.frame(
    Year = forecast_years,
    Forecast = m2_get_forecast_future_series(data, best_model),
    stringsAsFactors = FALSE
  )

  cmp <- data$model_comparison$comparison %||% data.frame()
  cmp_row <- cmp[cmp$model == best_model, , drop = FALSE]
  forecast_basis <- data$model_comparison$selection_basis %||% "unknown"
  best_cv_mae <- m2_scalar_num(cmp_row$CV_MAE[1])
  naive_improvement <- m2_scalar_num(cmp_row$RelativeImprovement_MAE[1])
  subtitle <- if (is.finite(best_cv_mae) && is.finite(naive_improvement)) {
    sprintf(
      "Best model: %s | Horizon: %d years | CV MAE: %.3f | Naive improvement: %.1f%%",
      best_model,
      horizon,
      best_cv_mae,
      naive_improvement
    )
  } else if (identical(forecast_basis, "provisional_information_criteria")) {
    sprintf(
      "Best model: %s | Horizon: %d years | provisional selection from information criteria because temporal CV was incomplete",
      best_model,
      horizon
    )
  } else {
    sprintf("Best model: %s | Horizon: %d years", best_model, horizon)
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = observed,
      ggplot2::aes(x = Year, y = Articles),
      color = "black",
      linewidth = 0.95
    ) +
    ggplot2::geom_point(
      data = observed,
      ggplot2::aes(x = Year, y = Articles),
      color = "black",
      size = 1.8
    ) +
    ggplot2::geom_line(
      data = fitted,
      ggplot2::aes(x = Year, y = Fitted),
      color = "#7A7A7A",
      linewidth = 0.7,
      linetype = "22"
    ) +
    ggplot2::geom_ribbon(
      data = interval_df,
      ggplot2::aes(x = Year, ymin = Lower, ymax = Upper),
      fill = "#C6DBEF",
      alpha = 0.45
    ) +
    ggplot2::geom_line(
      data = future_df,
      ggplot2::aes(x = Year, y = Forecast),
      color = "#C0362C",
      linewidth = 1.1
    ) +
    ggplot2::geom_point(
      data = future_df,
      ggplot2::aes(x = Year, y = Forecast),
      color = "#C0362C",
      size = 1.9
    ) +
    ggplot2::geom_vline(
      xintercept = last_year,
      color = "#C0362C",
      linetype = "22",
      linewidth = 0.7
    ) +
    ggplot2::labs(
      title = "Annual production forecast",
      subtitle = subtitle,
      x = "Year",
      y = "Number of publications",
      caption = "Black = observed series, gray dashed = fitted values, red = forward forecast, blue band = 95% interval."
    ) +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "none")

  ieee_mark_plot_layout(p, "full")
}

m2_get_forecast_fitted_series <- function(data, best_model) {
  values <- switch(
    best_model,
    ARIMA = data$arima$fitted,
    ETS = data$ets$fitted,
    Naive = data$naive$fitted,
    Ensemble = m2_build_ensemble_fitted(data),
    data$ets$fitted
  )
  data.frame(Year = data$years, Fitted = values, stringsAsFactors = FALSE)
}

m2_get_forecast_future_series <- function(data, best_model) {
  switch(
    best_model,
    ARIMA = generate_forecast_values(data$arima, data$horizon),
    ETS = generate_forecast_values(data$ets, data$horizon),
    Naive = rep(tail(data$articles, 1), data$horizon),
    Ensemble = data$ensemble$forecast,
    data$ensemble$forecast
  )
}

m2_get_forecast_interval_df <- function(data, best_model) {
  years <- (max(data$years) + 1):(max(data$years) + data$horizon)
  if (best_model == "ARIMA") {
    return(data.frame(
      Year = years,
      Lower = data$prediction_intervals$arima$lower_95,
      Upper = data$prediction_intervals$arima$upper_95,
      stringsAsFactors = FALSE
    ))
  }
  if (best_model == "ETS") {
    return(data.frame(
      Year = years,
      Lower = data$prediction_intervals$ets$lower_95,
      Upper = data$prediction_intervals$ets$upper_95,
      stringsAsFactors = FALSE
    ))
  }
  if (best_model == "Naive") {
    sigma_naive <- stats::sd(m2_num_vec(data$naive$residuals), na.rm = TRUE)
    sigma_naive <- if (is.finite(sigma_naive) && sigma_naive > .Machine$double.eps) sigma_naive else max(stats::sd(m2_num_vec(data$articles), na.rm = TRUE), 1)
    center <- rep(tail(data$articles, 1), data$horizon)
    return(data.frame(
      Year = years,
      Lower = center - 1.96 * sigma_naive * sqrt(seq_along(center)),
      Upper = center + 1.96 * sigma_naive * sqrt(seq_along(center)),
      stringsAsFactors = FALSE
    ))
  }
  if (best_model == "Ensemble") {
    naive_bounds <- m2_build_ensemble_interval(data$prediction_intervals, data$naive, data$ensemble, data$articles)
    return(data.frame(
      Year = years,
      Lower = m2_num_vec(naive_bounds$lower_95)[seq_along(years)],
      Upper = m2_num_vec(naive_bounds$upper_95)[seq_along(years)],
      stringsAsFactors = FALSE
    ))
  }

  lower <- pmin(data$prediction_intervals$arima$lower_95, data$prediction_intervals$ets$lower_95, na.rm = TRUE)
  upper <- pmax(data$prediction_intervals$arima$upper_95, data$prediction_intervals$ets$upper_95, na.rm = TRUE)
  data.frame(Year = years, Lower = lower, Upper = upper, stringsAsFactors = FALSE)
}

#' Create forecast model comparison plot
#' @keywords internal
create_forecast_model_comparison_plot <- function(data, config) {
  comparison <- tibble::as_tibble(data$model_comparison$comparison %||% data.frame())
  if (nrow(comparison) == 0) {
    return(NULL)
  }

  comparison <- comparison |>
    dplyr::mutate(
      model = factor(.data$model, levels = rev(.data$model[order(.data$CompositeRank)])),
      highlight = ifelse(.data$model == data$model_comparison$best_model, "Selected", "Other"),
      annotation = dplyr::case_when(
        is.finite(.data$CV_MAE) & is.finite(.data$Coverage95) ~ sprintf("MAE %.2f | Cov %.0f%%", .data$CV_MAE, 100 * .data$Coverage95),
        is.finite(.data$CV_MAE) ~ sprintf("MAE %.2f", .data$CV_MAE),
        TRUE ~ "CV unavailable"
      )
    )

  p <- ggplot2::ggplot(comparison, ggplot2::aes(x = model, y = CompositeScore, fill = highlight)) +
    ggplot2::geom_col(width = 0.68, color = "white", linewidth = 0.25) +
    ggplot2::coord_flip() +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$annotation),
      hjust = -0.05,
      size = 2.8
    ) +
    ggplot2::scale_fill_manual(values = c("Selected" = "#C0362C", "Other" = "#AAB4BE"), guide = "none") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::labs(
      title = "Forecast model leaderboard",
      subtitle = if (identical(data$model_comparison$selection_basis %||% "", "provisional_information_criteria")) {
        "Ranking is provisional because at least two candidate models did not yield comparable rolling temporal CV metrics."
      } else {
        "Composite score combines information criteria, temporal CV error, and interval calibration."
      },
      x = NULL,
      y = "Composite forecast score",
      caption = "Text labels show one-step CV MAE and empirical 95% interval coverage."
    ) +
    ieee_theme_wide(base_size = 8.4)

  ieee_mark_plot_layout(p, "full")
}

create_cv_error_plot <- function(data, config) {
  cv_scores <- data$cv_results$cv_scores %||% data.frame()
  if (!is.data.frame(cv_scores) || nrow(cv_scores) == 0) {
    return(NULL)
  }

  cv_scores$model <- factor(cv_scores$model, levels = c("ARIMA", "ETS", "Naive", "Ensemble"))
  colors <- c("ARIMA" = "#4E79A7", "ETS" = "#59A14F", "Naive" = "#9C755F", "Ensemble" = "#C0362C")
  p <- ggplot2::ggplot(cv_scores, ggplot2::aes(x = fold, y = mae, color = model)) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.9) +
    ggplot2::geom_point(size = 2.1) +
    ggplot2::scale_color_manual(values = colors, drop = FALSE) +
    ggplot2::scale_x_continuous(name = "Rolling-origin fold", breaks = sort(unique(cv_scores$fold))) +
    ggplot2::scale_y_continuous(name = "One-step MAE", expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
    ggplot2::labs(
      title = "Rolling-origin forecast errors",
      subtitle = "Lower trajectories indicate more stable out-of-sample forecasting performance.",
      caption = "Each point is a one-step-ahead forecast error from an expanding-window backtest."
    ) +
    ieee_theme(base_size = 8.2) +
    ggplot2::theme(legend.position = "bottom")

  ieee_mark_plot_layout(p, "single")
}

create_prediction_interval_plot <- function(data, config) {
  comparison <- data$model_comparison$comparison %||% data.frame()
  best_model <- data$model_comparison$best_model %||% "ARIMA"
  interval_df <- m2_get_forecast_interval_df(data, best_model)
  future_df <- data.frame(
    Year = (max(data$years) + 1):(max(data$years) + data$horizon),
    Forecast = m2_get_forecast_future_series(data, best_model),
    stringsAsFactors = FALSE
  )
  observed <- data.frame(Year = data$years, Articles = data$articles, stringsAsFactors = FALSE)
  best_row <- comparison[comparison$model == best_model, , drop = FALSE]
  coverage_95 <- m2_scalar_num(best_row$Coverage95[1])
  subtitle <- if (is.finite(coverage_95)) {
    sprintf("%s 95%% interval coverage in rolling-origin validation: %.1f%%.", best_model, 100 * coverage_95)
  } else {
    sprintf("Selected model: %s.", best_model)
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = observed, ggplot2::aes(x = Year, y = Articles), color = "black", linewidth = 0.9) +
    ggplot2::geom_point(data = observed, ggplot2::aes(x = Year, y = Articles), color = "black", size = 1.7) +
    ggplot2::geom_ribbon(
      data = interval_df,
      ggplot2::aes(x = Year, ymin = Lower, ymax = Upper),
      fill = "#B8D5F0",
      alpha = 0.45
    ) +
    ggplot2::geom_line(data = future_df, ggplot2::aes(x = Year, y = Forecast), color = "#C0362C", linewidth = 1.05) +
    ggplot2::geom_point(data = future_df, ggplot2::aes(x = Year, y = Forecast), color = "#C0362C", size = 1.9) +
    ggplot2::geom_vline(xintercept = max(data$years), color = "#C0362C", linetype = "22", linewidth = 0.7) +
    ggplot2::labs(
      title = "Forecast interval calibration",
      subtitle = subtitle,
      x = "Year",
      y = "Number of publications",
      caption = "Blue band = selected model 95% prediction interval; red = forward forecast path."
    ) +
    ieee_theme_wide(base_size = 8.4) +
    ggplot2::theme(legend.position = "none")

  ieee_mark_plot_layout(p, "full")
}

create_ensemble_weights_plot <- function(data, config) {
  weights <- data$ensemble$weights %||% numeric(0)
  if (length(weights) == 0) {
    return(NULL)
  }

  df <- data.frame(
    Model = factor(c("ARIMA", "ETS", "Naive"), levels = c("ARIMA", "ETS", "Naive")),
    Weight = as.numeric(weights[seq_len(min(3, length(weights)))]),
    stringsAsFactors = FALSE
  )
  colors <- c("ARIMA" = "#4E79A7", "ETS" = "#59A14F", "Naive" = "#9C755F")
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Model, y = Weight, fill = Model)) +
    ggplot2::geom_col(width = 0.62, color = "white", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(Weight, accuracy = 0.1)), vjust = -0.35, size = 3) +
    ggplot2::scale_fill_manual(values = colors, guide = "none") +
    ggplot2::scale_y_continuous(name = "Weight", labels = scales::percent, expand = ggplot2::expansion(mult = c(0, 0.14))) +
    ggplot2::labs(
      title = "Ensemble composition",
      subtitle = "Akaike-style normalized weights used to construct the ensemble forecast.",
      x = NULL
    ) +
    ieee_theme_bar(base_size = 8.2)

  ieee_mark_plot_layout(p, "single")
}

#' Render the main regression figure with safer benchmark messaging and label placement
#' @keywords internal
render_best_model_plot <- function(data, model_data, config) {
  if (is.null(data$best_model) || identical(data$best_model$name, "none") || nrow(model_data$raw) == 0) {
    return(NULL)
  }

  raw_df <- model_data$raw
  fitted_df <- model_data$fitted
  best_name <- data$best_model$name
  benchmark_name <- data$benchmark_best_model$name %||% data$best_model$benchmark_name %||% best_name
  params <- data$best_model$parameter_summary %||% list()

  capacity <- m2_scalar_num(params$carrying_capacity)
  inflection <- m2_scalar_num(params$inflection_year)
  growth_rate <- m2_scalar_num(params$growth_rate)
  transition_year <- m2_compute_transition_year(best_name, params)

  x_range <- range(c(raw_df$Year, fitted_df$Year, transition_year), na.rm = TRUE)
  y_range <- range(c(raw_df$Articles, fitted_df$Fitted, fitted_df$Upper, fitted_df$Lower), na.rm = TRUE)
  x_span <- diff(x_range)
  y_span <- diff(y_range)
  if (!is.finite(y_span) || y_span <= 0) y_span <- max(abs(y_range), 1)
  if (!is.finite(x_span) || x_span <= 0) x_span <- 1

  label_x <- x_range[1] + 0.04 * x_span
  label_y <- y_range[1] + 0.14 * y_span
  top_y <- y_range[2] - 0.04 * y_span

  label_lines <- c(
    best_name,
    sprintf("Adj. R2 = %.3f", m2_scalar_num(data$best_model$Adj_R2)),
    sprintf("RMSE = %.2f", m2_scalar_num(data$best_model$RMSE)),
    m2_compact_parameter_line(best_name, params)
  )
  label_lines <- label_lines[nzchar(label_lines)]

  benchmark_note <- if (identical(benchmark_name, best_name)) {
    "Flexible benchmark matched the headline model"
  } else {
    sprintf("Flexible benchmark: %s", benchmark_name)
  }
  subtitle_parts <- c(
    benchmark_note,
    if (is.finite(growth_rate)) sprintf("growth rate = %.4f", growth_rate) else NULL,
    if (is.finite(capacity)) sprintf("capacity = %.1f", capacity) else NULL
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = fitted_df,
      ggplot2::aes(x = Year, ymin = Lower, ymax = Upper),
      fill = "#BDBDBD",
      alpha = 0.18
    ) +
    ggplot2::geom_line(
      data = fitted_df,
      ggplot2::aes(x = Year, y = Fitted),
      color = "black",
      linewidth = 1.05,
      lineend = "round"
    ) +
    ggplot2::geom_point(
      data = raw_df,
      ggplot2::aes(x = Year, y = Articles),
      color = "black",
      fill = "black",
      shape = 16,
      size = 1.95,
      alpha = 0.95
    ) +
    ggplot2::scale_x_continuous(
      name = "Year",
      breaks = scales::breaks_pretty(n = 8),
      expand = ggplot2::expansion(mult = c(0.01, 0.06))
    ) +
    ggplot2::scale_y_continuous(
      name = "Number of publications",
      labels = scales::label_number(big.mark = ","),
      expand = ggplot2::expansion(mult = c(0.03, 0.08))
    ) +
    ggplot2::annotate(
      "label",
      x = label_x,
      y = label_y,
      label = paste(label_lines, collapse = "\n"),
      hjust = 0,
      vjust = 0,
      size = 3.2,
      family = "mono",
      fill = scales::alpha("white", 0.92),
      color = "black",
      linewidth = 0.2,
      label.padding = grid::unit(0.14, "lines")
    ) +
    ggplot2::labs(
      title = "Annual scientific production: selected growth model",
      subtitle = paste(subtitle_parts, collapse = " | "),
      caption = "Interpretable headline model with fitted uncertainty band."
    ) +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      legend.position = "none"
    )

  if (is.finite(capacity) && capacity > (y_range[1] + 0.6 * y_span)) {
    p <- p +
      ggplot2::geom_hline(yintercept = capacity, color = "#59A14F", linetype = "22", linewidth = 0.7) +
      ggplot2::annotate(
        "text",
        x = x_range[1] + 0.04 * x_span,
        y = capacity + 0.02 * y_span,
        label = sprintf("K = %.1f", capacity),
        hjust = 0,
        color = "#59A14F",
        size = 3.3,
        fontface = "bold"
      )
  }

  if (is.finite(inflection)) {
    inflection_hjust <- if (inflection > (x_range[1] + 0.88 * x_span)) 1.05 else -0.1
    p <- p +
      ggplot2::geom_vline(xintercept = inflection, color = "#4E79A7", linetype = "42", linewidth = 0.85) +
      ggplot2::annotate(
        "text",
        x = inflection,
        y = top_y,
        label = "t0",
        color = "#4E79A7",
        vjust = 1,
        hjust = inflection_hjust,
        size = 3.5,
        fontface = "bold"
      )
  }

  if (is.finite(transition_year)) {
    transition_hjust <- if (transition_year > (x_range[1] + 0.88 * x_span)) 1.05 else -0.1
    p <- p +
      ggplot2::geom_vline(xintercept = transition_year, color = "#E15759", linetype = "42", linewidth = 0.85) +
      ggplot2::annotate(
        "text",
        x = transition_year,
        y = top_y,
        label = "t95",
        color = "#E15759",
        vjust = 1,
        hjust = transition_hjust,
        size = 3.5,
        fontface = "bold"
      )
  }

  ieee_mark_plot_layout(p, "full")
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
