# ============================================================================
# m3_table_narrative.R - M3 narrative evidence table
# ============================================================================

m3_table_narrative <- function(result, config = biblio_config()) {
  metrics <- result$metrics %||% data.frame()
  if (!is.data.frame(metrics) || nrow(metrics) == 0) {
    return(list(status = "stub", table = tibble::tibble()))
  }
  list(
    status = "success",
    table = tibble::as_tibble(metrics[, c(
      "module", "dimension", "metric", "value", "score",
      "display", "signal", "interpretation"
    ), drop = FALSE])
  )
}
