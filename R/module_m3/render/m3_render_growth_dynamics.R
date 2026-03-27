# ============================================================================
# m3_render_growth_dynamics.R - Growth dynamics plots for M3
# ============================================================================

#' Render temporal growth dynamics plots
#'
#' @param growth_data Output from \code{m3_compute_growth_dynamics}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list with \code{status} and \code{plots}
#' @export
m3_render_growth_dynamics <- function(growth_data, config = biblio_config()) {
  stub <- list(status = "stub", plots = list())

  if (!is.list(growth_data) || nrow(growth_data$annual_productivity) == 0) {
    return(stub)
  }

  plots <- list()
  annual_prod <- growth_data$annual_productivity

  # Spaghetti plot of top countries productivity over time
  n_countries <- dplyr::n_distinct(annual_prod$country)
  palette_colors <- unlist(ieee_colors)[seq_len(min(n_countries, 7))]

  plots$spaghetti_production <- ggplot2::ggplot(
    annual_prod,
    ggplot2::aes(x = PY, y = article_count,
                 color = country, group = country)
  ) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point(size = 1.0) +
    ggplot2::scale_color_manual(values = palette_colors, name = NULL) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    ggplot2::labs(
      title = "Annual Scientific Production — Leading Countries",
      x     = "Year",
      y     = "Publications"
    ) +
    ieee_theme() +
    ggplot2::theme(legend.position = "right", legend.key.size = ggplot2::unit(0.3, "cm"))

  # CAGR bar chart if productivity_growth has enough data
  prod_growth <- growth_data$productivity_growth
  if (is.data.frame(prod_growth) && nrow(prod_growth) >= 2) {
    pg <- prod_growth %>%
      dplyr::filter(!is.na(cagr)) %>%
      dplyr::arrange(dplyr::desc(cagr)) %>%
      dplyr::mutate(label_clean = substr(trimws(country), 1, 25))

    if (nrow(pg) >= 2) {
      plots$cagr_bar <- ggplot2::ggplot(
        pg,
        ggplot2::aes(x = reorder(label_clean, cagr),
                     y = cagr * 100,
                     fill = cagr >= 0)
      ) +
        ggplot2::geom_col(color = "black", linewidth = 0.2, show.legend = FALSE) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = c(`TRUE` = ieee_colors$green,
                                              `FALSE` = ieee_colors$red)) +
        ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "black") +
        ggplot2::labs(
          title = "Compound Annual Growth Rate (CAGR) — Publications",
          x     = NULL,
          y     = "CAGR (%)"
        ) +
        ieee_theme()
    }
  }

  # Citations spaghetti if available
  annual_cit <- growth_data$annual_citations
  if (is.data.frame(annual_cit) && nrow(annual_cit) > 0) {
    n_c <- dplyr::n_distinct(annual_cit$country)
    pal_c <- unlist(ieee_colors)[seq_len(min(n_c, 7))]

    plots$spaghetti_citations <- ggplot2::ggplot(
      annual_cit,
      ggplot2::aes(x = PY, y = total_citations,
                   color = country, group = country)
    ) +
      ggplot2::geom_line(linewidth = 0.5) +
      ggplot2::geom_point(size = 1.0) +
      ggplot2::scale_color_manual(values = pal_c, name = NULL) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
      ggplot2::labs(
        title = "Annual Total Citations — Leading Countries",
        x     = "Year",
        y     = "Total Citations"
      ) +
      ieee_theme() +
      ggplot2::theme(legend.position = "right",
                     legend.key.size = ggplot2::unit(0.3, "cm"))
  }

  list(status = "success", plots = plots)
}
