# ============================================================================
# module_m2/tables/m2_tables.R - Table builders for M2
# ============================================================================

#' @export
build_m2_eda_table <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"summary" %in% names(result)) {
    return(list(status = "stub", table = tibble::tibble()))
  }

  s <- result$summary
  tbl <- tibble::tibble(
    metric = c("Start Year", "End Year", "Peak Year", "Peak Articles",
               "Mean Articles", "Median Articles", "Std Dev", "Total Articles"),
    value = c(s$start_year, s$end_year, s$peak_year, s$peak_articles,
              s$mean_articles, s$median_articles, s$sd_articles, s$total_articles)
  )
  list(status = "success", table = tbl)
}

#' @export
build_m2_regression_table <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"comparison_table" %in% names(result)) {
    return(list(status = "stub", table = tibble::tibble()))
  }

  ranking <- tibble::as_tibble(result$comparison_table)
  selected <- tibble::tibble(
    model = result$best_model$name %||% NA_character_,
    benchmark = result$best_model$benchmark_name %||% NA_character_,
    adj_r2 = m2_scalar_num(result$best_model$Adj_R2),
    rmse = m2_scalar_num(result$best_model$RMSE),
    aic = m2_scalar_num(result$best_model$AIC),
    bic = m2_scalar_num(result$best_model$BIC),
    composite_score = m2_scalar_num(result$best_model$composite_score),
    selection_reason = result$best_model$selection_reason %||% NA_character_
  )

  param <- result$best_model$parameter_summary %||% list()
  parameter_table <- tibble::tibble(
    parameter = c("carrying_capacity", "growth_rate", "inflection_year", "shape"),
    value = c(
      m2_scalar_num(param$carrying_capacity),
      m2_scalar_num(param$growth_rate),
      m2_scalar_num(param$inflection_year),
      m2_scalar_num(param$shape)
    )
  )

  diagnostic_table <- tibble::tibble(
    metric = c("shapiro_p", "durbin_watson", "ljung_box_p", "breusch_pagan_p", "stability_score"),
    value = c(
      m2_scalar_num(result$performance$shapiro_p),
      m2_scalar_num(result$performance$dw_statistic),
      m2_scalar_num(result$performance$ljung_box_p),
      m2_scalar_num(result$performance$breusch_pagan_p),
      m2_scalar_num(result$performance$stability_score)
    )
  )

  list(
    status = "success",
    table = ranking,
    tables = list(
      ranking = ranking,
      selected_model = selected,
      parameter_summary = parameter_table,
      diagnostic_summary = diagnostic_table
    )
  )
}

#' @export
build_m2_forecasting_table <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || result$status != "success") {
    return(list(status = "stub", table = tibble::tibble()))
  }

  build_m2_forecasting_tables_impl(result, config)
}

#' @export
build_m2_hypotheses_table <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || is.null(result$hypotheses) || length(result$hypotheses) == 0) {
    return(list(status = "stub", table = tibble::tibble()))
  }

  rows <- lapply(names(result$hypotheses), function(id) {
    m2_hypothesis_to_row(id, result$hypotheses[[id]])
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(list(status = "stub", table = tibble::tibble()))
  }

  table <- dplyr::bind_rows(rows)
  table <- dplyr::arrange(
    table,
    dplyr::desc(evidence_class == "statistical"),
    dplyr::desc(decision == "reject"),
    p_value
  )

  list(status = "success", table = table)
}

#' @export
build_m2_diagnostics_table <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || is.null(result$trend_statistics)) {
    return(list(status = "stub", table = tibble::tibble()))
  }

  mk <- result$trend_statistics$mann_kendall %||% list()
  table <- tibble::tibble(
    metric = c(
      "cagr",
      "recent_cagr",
      "sen_slope",
      "mann_kendall_tau",
      "mann_kendall_p",
      "hurst_exponent",
      "acceleration",
      "volatility"
    ),
    value = c(
      m2_scalar_num(result$trend_statistics$cagr),
      m2_scalar_num(result$trend_statistics$recent_cagr),
      m2_scalar_num(result$trend_statistics$sen_slope),
      m2_scalar_num(mk$tau),
      m2_scalar_num(mk$p_value),
      m2_scalar_num(result$trend_statistics$hurst_exponent),
      m2_scalar_num(result$trend_statistics$acceleration),
      m2_scalar_num(result$trend_statistics$volatility)
    )
  )

  list(status = "success", table = table)
}

#' Flatten a hypothesis result into a row
#' @keywords internal
m2_hypothesis_to_row <- function(id, hypothesis) {
  if (!is.list(hypothesis)) {
    return(NULL)
  }

  label <- hypothesis$hypothesis %||% id
  decision <- hypothesis$result %||% "inconclusive"
  evidence_class <- hypothesis$evidence_class %||% "heuristic"
  test_name <- hypothesis$test %||% "heuristic diagnostic"
  interpretation <- hypothesis$interpretation %||% NA_character_
  p_value <- m2_extract_first_numeric(
    hypothesis$p_value,
    hypothesis$P_value
  )
  statistic <- m2_extract_first_numeric(
    hypothesis$statistic,
    hypothesis$F_statistic,
    hypothesis$W_statistic,
    hypothesis$BP_statistic,
    hypothesis$quadratic_coefficient,
    hypothesis$R_squared,
    hypothesis$kendall_tau
  )
  effect <- m2_extract_first_numeric(
    hypothesis$sen_slope,
    hypothesis$R_squared,
    hypothesis$kendall_tau,
    hypothesis$CV,
    hypothesis$quadratic_coefficient
  )

  tibble::tibble(
    id = id,
    hypothesis = label,
    decision = decision,
    evidence_class = evidence_class,
    test = test_name,
    statistic = statistic,
    p_value = p_value,
    effect = effect,
    interpretation = interpretation
  )
}

#' Extract the first finite numeric value from candidates
#' @keywords internal
m2_extract_first_numeric <- function(...) {
  candidates <- list(...)
  for (candidate in candidates) {
    value <- suppressWarnings(as.numeric(candidate))
    if (length(value) == 1L && is.finite(value)) {
      return(value)
    }
  }
  NA_real_
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
