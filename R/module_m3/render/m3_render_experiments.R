# ============================================================================
# m3_render_experiments.R - Exploratory experiment plots for M3
# ============================================================================

#' Render exploratory experiment plots
#'
#' @param experiments_data Output from \code{m3_compute_experiments}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list with \code{status} and \code{plots}
#' @export
m3_render_experiments <- function(experiments_data, config = biblio_config()) {
  stub <- list(status = "stub", plots = list())
  if (!is.list(experiments_data)) return(stub)

  plots <- list()

  # 1. Quadrant scatter plot
  quad <- experiments_data$quadrant
  if (is.data.frame(quad) && nrow(quad) >= 2) {
    quad_palette <- c(
      "High-Output / High-Impact" = ieee_colors$blue,
      "High-Output / Low-Impact"  = ieee_colors$orange,
      "Low-Output  / High-Impact" = ieee_colors$green,
      "Low-Output  / Low-Impact"  = ieee_colors$ltgray
    )

    p_quad <- ggplot2::ggplot(
      quad,
      ggplot2::aes(x = article_count, y = total_citations,
                   color = quadrant, label = country)
    ) +
      ggplot2::geom_point(size = 2.0) +
      ggplot2::scale_color_manual(values = quad_palette, name = NULL)

    # Try ggrepel for labels
    p_quad <- tryCatch(
      p_quad + ggrepel::geom_text_repel(size = 1.8, max.overlaps = 15),
      error = function(e) p_quad + ggplot2::geom_text(vjust = -0.8, size = 1.6)
    )

    p_quad <- p_quad +
      ggplot2::geom_vline(
        xintercept = stats::median(quad$article_count, na.rm = TRUE),
        linetype   = "dashed", color = ieee_colors$gray, linewidth = 0.4
      ) +
      ggplot2::geom_hline(
        yintercept = stats::median(quad$total_citations, na.rm = TRUE),
        linetype   = "dashed", color = ieee_colors$gray, linewidth = 0.4
      ) +
      ggplot2::labs(
        title    = "Productivity vs Impact Quadrant (Exploratory)",
        subtitle = "Dashed lines = medians",
        x        = "Publications",
        y        = "Total Citations"
      ) +
      ieee_theme() +
      ggplot2::theme(legend.position = "bottom",
                     legend.text     = ggplot2::element_text(size = 6))

    plots$quadrant <- p_quad
  }

  # 2. Concentration sensitivity plot
  conc <- experiments_data$concentration
  if (is.data.frame(conc) && nrow(conc) >= 2) {
    plots$concentration_sensitivity <- ggplot2::ggplot(
      conc,
      ggplot2::aes(x = top_k, y = gini)
    ) +
      ggplot2::geom_line(color = ieee_colors$purple, linewidth = 0.6) +
      ggplot2::geom_point(color = ieee_colors$purple, size = 1.5) +
      ggplot2::labs(
        title = "Gini Coefficient Sensitivity to Top-k Country Truncation",
        x     = "Number of Top Countries Included",
        y     = "Gini Coefficient"
      ) +
      ieee_theme()
  }

  # 3. Momentum score bar
  mom <- experiments_data$momentum
  if (is.data.frame(mom) && nrow(mom) >= 2) {
    mom_plot <- mom %>%
      dplyr::filter(!is.na(momentum_score)) %>%
      dplyr::mutate(label_clean = substr(trimws(country), 1, 25))

    if (nrow(mom_plot) >= 2) {
      plots$momentum_bar <- ggplot2::ggplot(
        mom_plot,
        ggplot2::aes(x = reorder(label_clean, momentum_score),
                     y = momentum_score,
                     fill = momentum_score >= 1)
      ) +
        ggplot2::geom_col(color = "black", linewidth = 0.2, show.legend = FALSE) +
        ggplot2::coord_flip() +
        ggplot2::geom_hline(yintercept = 1, linetype = "dashed",
                            color = ieee_colors$gray, linewidth = 0.4) +
        ggplot2::scale_fill_manual(values = c(`TRUE` = ieee_colors$green,
                                              `FALSE` = ieee_colors$red)) +
        ggplot2::labs(
          title    = "Temporal Momentum Score (Exploratory)",
          subtitle = "Score > 1 = accelerating; < 1 = decelerating",
          x        = NULL,
          y        = "Momentum Score (recent mean / prior mean)"
        ) +
        ieee_theme()
    }
  }

  if (length(plots) == 0) return(stub)
  list(status = "success", plots = plots)
}
