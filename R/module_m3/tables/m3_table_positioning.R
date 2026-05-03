# ============================================================================
# m3_table_positioning.R - Country positioning tables
# ============================================================================

m3_table_positioning <- function(result, config = biblio_config()) {
  table <- result$table %||% tibble::tibble()
  clusters <- result$clusters %||% tibble::tibble()
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(list(status = "stub", table = tibble::tibble(), tables = list()))
  }
  list(
    status = "success",
    table = tibble::as_tibble(table),
    tables = list(
      positioning = tibble::as_tibble(table),
      clusters = tibble::as_tibble(clusters)
    )
  )
}
