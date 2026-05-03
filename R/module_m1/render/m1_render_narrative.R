# ============================================================================
# m1_render_narrative.R - M1 narrative evidence plots
# ============================================================================

render_m1_narrative <- function(result, config = biblio_config()) {
  metrics <- result$metrics %||% data.frame()
  list(
    status = result$status %||% "stub",
    plots = list(
      evidence_dashboard = ieee_metric_dashboard(
        metrics,
        title = "M1 Narrative Evidence Dashboard",
        subtitle = "Normalized concentration, impact, collaboration, and conceptual-structure signals.",
        caption = "Scores normalize heterogeneous bibliometric indicators to support visual comparison.",
        layout = "full"
      ),
      signal_map = ieee_metric_signal_plot(
        metrics,
        title = "M1 Interpretive Signal Map",
        subtitle = "Metric values are printed beside each evidence point for manuscript-ready interpretation.",
        layout = "full"
      )
    )
  )
}
