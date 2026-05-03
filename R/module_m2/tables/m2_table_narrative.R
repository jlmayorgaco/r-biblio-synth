# ============================================================================
# m2_table_narrative.R - M2 narrative evidence table
# ============================================================================

build_m2_narrative_table <- function(result, config = biblio_config()) {
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
