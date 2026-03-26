# ============================================================================
# module_m1/render/m1_render_treemaps.R - Treemap visualizations
# ============================================================================

#' @export
render_m1_treemaps <- function(countries_result, sources_result, config = biblio_config()) {
  plots <- list()
  ec <- unlist(ieee_colors[1:8])

  # Countries treemap
  if (!is.null(countries_result) && "top_countries_by_articles" %in% names(countries_result)) {
    countries <- countries_result$top_countries_by_articles
    if (nrow(countries) > 0) {
      countries$label_text <- paste0(countries$label, "\n", countries$value)

      plots$countries <- ggplot2::ggplot(countries, ggplot2::aes(
        area = value, fill = as.factor(rank), label = label_text)) +
        treemapify::geom_treemap(color = "white", size = 2) +
        treemapify::geom_treemap_text(
          place = "centre", size = 12, color = "white", fontface = "bold",
          grow = TRUE, reflow = TRUE) +
        ggplot2::scale_fill_manual(values = ec) +
        ggplot2::labs(title = "Most Productive Countries") +
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 9, face = "bold", hjust = 0.5),
          legend.position = "none"
        )
    }
  }

  # Sources treemap
  if (!is.null(sources_result) && "top_sources" %in% names(sources_result)) {
    sources <- sources_result$top_sources
    if (nrow(sources) > 0) {
      # Truncate long source names
      sources$label_short <- substr(sources$label, 1, 30)
      sources$label_text <- paste0(sources$label_short, "\n", sources$value)

      plots$sources <- ggplot2::ggplot(sources, ggplot2::aes(
        area = value, fill = as.factor(rank), label = label_text)) +
        treemapify::geom_treemap(color = "white", size = 2) +
        treemapify::geom_treemap_text(
          place = "centre", size = 10, color = "white", fontface = "bold",
          grow = TRUE, reflow = TRUE) +
        ggplot2::scale_fill_manual(values = ec) +
        ggplot2::labs(title = "Most Productive Sources") +
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 9, face = "bold", hjust = 0.5),
          legend.position = "none"
        )
    }
  }

  list(status = "success", plots = plots, tables = list())
}
