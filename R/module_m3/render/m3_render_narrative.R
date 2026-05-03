# ============================================================================
# m3_render_narrative.R - M3 narrative evidence plots
# ============================================================================

render_m3_narrative <- function(result, config = biblio_config()) {
  metrics <- result$metrics %||% data.frame()
  list(
    status = result$status %||% "stub",
    plots = list(
      evidence_dashboard = ieee_metric_dashboard(
        metrics,
        title = "M3 Geographic Narrative Evidence Dashboard",
        subtitle = "Coverage, concentration, impact, collaboration, and growth signals.",
        caption = "Normalized scores make geographic narratives comparable across heterogeneous indicators.",
        layout = "full"
      ),
      signal_map = ieee_metric_signal_plot(
        metrics,
        title = "M3 Geographic Signal Map",
        subtitle = "Values annotate the country-level evidence used in the report narrative.",
        layout = "full"
      )
    )
  )
}
