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

  list(status = "success", table = tibble::as_tibble(result$comparison_table))
}

#' @export
build_m2_forecasting_table <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || result$status != "success") {
    return(list(status = "stub", table = tibble::tibble()))
  }

  build_m2_forecasting_tables_impl(result, config)
}
