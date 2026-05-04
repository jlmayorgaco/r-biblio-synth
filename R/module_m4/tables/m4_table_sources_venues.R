# ============================================================================
# m4_table_sources_venues.R - Source / journal / venue tables
# ============================================================================

build_m4_sources_table <- function(result, config = biblio_config()) {
  list(status = result$status %||% "stub", table = tibble::as_tibble(result$source_summary %||% tibble::tibble()))
}

build_m4_impact_table <- function(result, config = biblio_config()) {
  list(status = result$status %||% "stub", table = tibble::as_tibble(result$impact %||% tibble::tibble()))
}

build_m4_bradford_table <- function(result, config = biblio_config()) {
  list(
    status = result$status %||% "stub",
    table = tibble::as_tibble(result$zone_summary %||% tibble::tibble()),
    tables = list(zones = tibble::as_tibble(result$zones %||% tibble::tibble()))
  )
}

build_m4_growth_table <- function(result, config = biblio_config()) {
  list(
    status = result$status %||% "stub",
    table = tibble::as_tibble(result$growth %||% tibble::tibble()),
    tables = list(annual = tibble::as_tibble(result$annual %||% tibble::tibble()))
  )
}

build_m4_concentration_table <- function(result, config = biblio_config()) {
  list(status = result$status %||% "stub", table = tibble::as_tibble(result$metrics %||% tibble::tibble()))
}

build_m4_keywords_table <- function(result, config = biblio_config()) {
  list(status = result$status %||% "stub", table = tibble::as_tibble(result$source_keywords %||% tibble::tibble()))
}

build_m4_similarity_table <- function(result, config = biblio_config()) {
  list(
    status = result$status %||% "stub",
    table = tibble::as_tibble(result$pairwise %||% tibble::tibble()),
    tables = list(
      pairwise = tibble::as_tibble(result$pairwise %||% tibble::tibble()),
      network = tibble::as_tibble(result$network %||% tibble::tibble()),
      source_keyword_matrix = tibble::as_tibble(result$source_keyword_matrix %||% tibble::tibble())
    )
  )
}

build_m4_specialization_table <- function(result, config = biblio_config()) {
  list(status = result$status %||% "stub", table = tibble::as_tibble(result$specialization %||% tibble::tibble()))
}

build_m4_lifecycle_table <- function(result, config = biblio_config()) {
  list(status = result$status %||% "stub", table = tibble::as_tibble(result$lifecycle %||% tibble::tibble()))
}

build_m4_clusters_table <- function(result, config = biblio_config()) {
  list(status = result$status %||% "stub", table = tibble::as_tibble(result$clusters %||% tibble::tibble()))
}

build_m4_advanced_analytics_table <- function(result, config = biblio_config()) {
  list(
    status = result$status %||% "stub",
    table = tibble::as_tibble(result$features %||% tibble::tibble()),
    tables = list(
      features = tibble::as_tibble(result$features %||% tibble::tibble()),
      outliers = tibble::as_tibble(result$outliers %||% tibble::tibble()),
      regression_coefficients = tibble::as_tibble(result$regression$coefficients %||% tibble::tibble()),
      regression_fitted = tibble::as_tibble(result$regression$fitted %||% tibble::tibble()),
      ml_predictions = tibble::as_tibble(result$svm$predictions %||% tibble::tibble()),
      ml_cv_folds = tibble::as_tibble(result$ml_cv$folds %||% tibble::tibble()),
      ml_cv_summary = tibble::as_tibble(result$ml_cv$summary %||% tibble::tibble()),
      silhouette = tibble::as_tibble(result$silhouette$table %||% tibble::tibble())
    )
  )
}

build_m4_hypotheses_table <- function(result, config = biblio_config()) {
  hyp_list <- result$hypotheses %||% result$hyphypotheses %||% list()
  if (!is.list(hyp_list) || length(hyp_list) == 0) {
    return(list(status = "stub", table = tibble::tibble()))
  }
  rows <- lapply(names(hyp_list), function(id) {
    x <- hyp_list[[id]]
    if (!is.list(x)) return(NULL)
    tibble::tibble(
      id = id,
      hypothesis = x$hypothesis %||% x$hyphypothesis %||% id,
      null = x$null %||% NA_character_,
      decision = x$result %||% "unknown",
      evidence_class = x$evidence_class %||% if (!is.null(x$p_value)) "statistical" else "heuristic",
      test = x$test %||% if (!is.null(x$p_value)) "statistical test" else "heuristic diagnostic",
      statistic = m4_table_first_numeric(x$statistic),
      p_value = m4_table_first_numeric(x$p_value),
      effect_size = m4_table_first_numeric(x$effect_size),
      interpretation = x$interpretation %||% NA_character_
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(list(status = "stub", table = tibble::tibble()))
  }
  table <- dplyr::bind_rows(rows) |>
    dplyr::arrange(
      dplyr::desc(.data$evidence_class == "statistical"),
      dplyr::desc(.data$decision == "reject"),
      .data$p_value
    )
  list(status = result$status %||% "success", table = table)
}

build_m4_narrative_table <- function(result, config = biblio_config()) {
  list(status = result$status %||% "stub", table = tibble::as_tibble(result$metrics %||% tibble::tibble()))
}

m4_table_first_numeric <- function(...) {
  candidates <- list(...)
  for (candidate in candidates) {
    value <- suppressWarnings(as.numeric(candidate))
    if (length(value) == 1L && is.finite(value)) return(value)
  }
  NA_real_
}
