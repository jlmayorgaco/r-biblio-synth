# ============================================================================
# m2_render_narrative.R - M2 narrative evidence plots
# ============================================================================

render_m2_narrative <- function(result, config = biblio_config()) {
  metrics <- result$metrics %||% data.frame()
  list(
    status = result$status %||% "stub",
    plots = list(
      evidence_dashboard = ieee_metric_dashboard(
        metrics,
        title = "M2 Temporal Narrative Evidence Dashboard",
        subtitle = "Growth, model fit, trend-test, dynamic, and structural-change signals.",
        caption = "Scores normalize heterogeneous temporal statistics for IEEE-ready visual synthesis.",
        layout = "full"
      ),
      signal_map = ieee_metric_signal_plot(
        metrics,
        title = "M2 Temporal Signal Map",
        subtitle = "Values annotate the normalized evidence map used to support report narratives.",
        layout = "full"
      )
    )
  )
}
