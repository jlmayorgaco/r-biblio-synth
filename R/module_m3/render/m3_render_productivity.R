# ============================================================================
# m3_render_productivity.R - Production plots for M3
# ============================================================================

#' Render country productivity plots
#'
#' @param production_data Output from \code{m3_compute_production}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list with \code{status} and \code{plots}
#' @export
m3_render_productivity <- function(production_data, config = biblio_config()) {
  stub <- list(status = "stub", plots = list())

  if (!is.list(production_data) || nrow(production_data$top_countries_by_production) == 0) {
    return(stub)
  }

  top <- production_data$top_countries_by_production
  top$label_clean <- substr(trimws(top$label), 1, 30)

  plots <- list()

  # 1. Top countries bar chart
  plots$bar_production <- ggplot2::ggplot(
    top,
    ggplot2::aes(x = reorder(label_clean, value), y = value)
  ) +
    ggplot2::geom_col(fill = ieee_colors$blue, color = "black", linewidth = 0.2) +
    ggplot2::geom_text(ggplot2::aes(label = value), hjust = -0.15, size = 2.0) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(
      title = "Country Scientific Production",
      x     = NULL,
      y     = "Publications"
    ) +
    ieee_theme()

  # 2. Cumulative share plot (long-tail visualization)
  full <- production_data$country_production
  if (nrow(full) >= 3) {
    full_ranked <- full %>%
      dplyr::arrange(dplyr::desc(article_count)) %>%
      dplyr::mutate(rank = seq_len(dplyr::n()))

    plots$cumulative_share <- ggplot2::ggplot(
      full_ranked,
      ggplot2::aes(x = rank, y = cumulative_share)
    ) +
      ggplot2::geom_line(color = ieee_colors$blue, linewidth = 0.6) +
      ggplot2::geom_hline(yintercept = 0.8, linetype = "dashed",
                          color = ieee_colors$gray, linewidth = 0.4) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                  limits = c(0, 1)) +
      ggplot2::labs(
        title = "Cumulative Share of Production by Country Rank",
        x     = "Country Rank",
        y     = "Cumulative Share"
      ) +
      ieee_theme()
  }

  list(status = "success", plots = plots)
}
