# ============================================================================
# m1_table_countries.R - Table builder for countries
# ============================================================================

#' @export
build_m1_countries_table <- function(result, config = biblio_config()) {
  list(
    status = if (inherits(result, "list") && "top_countries_by_articles" %in% names(result)) "success" else "stub",
    table  = if (inherits(result, "list") && "top_countries_by_articles" %in% names(result)) result$top_countries_by_articles else tibble::tibble()
  )
}
