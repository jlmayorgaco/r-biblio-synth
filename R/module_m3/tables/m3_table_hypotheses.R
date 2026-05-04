# ============================================================================
# m3_table_hypotheses.R - Hypothesis table builder for M3
# ============================================================================

#' @export
m3_table_hypotheses <- function(result, config = biblio_config()) {
  hyp_list <- result$hypotheses %||% result$hyphypotheses %||% list()
  if (!is.list(hyp_list) || length(hyp_list) == 0) {
    return(list(status = "stub", table = tibble::tibble()))
  }

  rows <- lapply(names(hyp_list), function(id) {
    x <- hyp_list[[id]]
    if (!is.list(x)) return(NULL)
    tibble::tibble(
      id = id,
      hypothesis = x$hypothesis %||% x$hyphypothesis %||% x$hyphothesis %||% id,
      null = x$null %||% NA_character_,
      decision = x$result %||% "unknown",
      evidence_class = x$evidence_class %||% if (!is.null(x$p_value)) "statistical" else "heuristic",
      test = x$test %||% if (!is.null(x$p_value)) "statistical test" else "heuristic diagnostic",
      statistic = m3_table_first_numeric(x$statistic, x$correlation, x$slope, x$CV, x$gini),
      p_value = m3_table_first_numeric(x$p_value, x$pvalue, x$ks_pvalue),
      effect_size = m3_table_first_numeric(x$effect_size, x$correlation, x$slope, x$gini, x$CV, x$proportion_increasing),
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

  list(status = "success", table = table)
}

m3_table_first_numeric <- function(...) {
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
