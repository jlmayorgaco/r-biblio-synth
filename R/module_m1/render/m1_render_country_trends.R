# ============================================================================
# module_m1/render/m1_render_country_trends.R - Country trend visualizations
# ============================================================================

#' @export
render_m1_country_trends <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"trends" %in% names(result) || nrow(result$trends) == 0) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  plots <- list()
  trends <- result$trends

  # Productivity vs Impact bubble
  if ("citations" %in% names(trends)) {
    trends$citations[is.na(trends$citations)] <- 0

    plots$productivity_impact <- ggplot2::ggplot(trends, ggplot2::aes(x = articles, y = citations)) +
      ggplot2::geom_point(ggplot2::aes(size = articles, color = quadrant), alpha = 0.7) +
      ggrepel::geom_text_repel(ggplot2::aes(label = country), size = 2.5, max.overlaps = 15) +
      ggplot2::scale_color_manual(values = c(
        "High Productivity, High Impact" = ieee_colors$green,
        "High Productivity, Low Impact" = ieee_colors$orange,
        "Low Productivity, High Impact" = ieee_colors$blue,
        "Low Productivity, Low Impact" = ieee_colors$gray
      )) +
      ggplot2::scale_size_continuous(range = c(2, 10), guide = "none") +
      ggplot2::labs(
        title = "Country Productivity vs Impact",
        x = "Total Publications",
        y = "Total Citations"
      ) +
      ieee_theme(base_size = 8) +
      ggplot2::theme(legend.position = "right")
  }

  # Top countries bar
  plots$top_countries <- ggplot2::ggplot(trends, ggplot2::aes(x = reorder(country, articles), y = articles)) +
    ggplot2::geom_bar(stat = "identity", fill = ieee_colors$blue, color = "black", linewidth = 0.2) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Top Countries by Publications", x = "Country", y = "Articles") +
    ieee_theme(base_size = 8)

  list(status = "success", plots = plots, tables = list())
}
