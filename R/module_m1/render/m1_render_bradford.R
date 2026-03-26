# ============================================================================
# m1_render_bradford.R - Bradford Plots (FIXED)
# ============================================================================

#' @export
render_m1_bradford <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"bradford_table" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  bt <- result$bradford_table
  if (nrow(bt) == 0) return(list(status = "stub", plots = list(), tables = list()))

  plots <- list()

  zone_colors <- c("Zone 1: Core" = ieee_colors$blue, "Zone 2: Moderate" = ieee_colors$orange, "Zone 3: Peripheral" = ieee_colors$green)

  n_show <- min(25, nrow(bt))
  bt_top <- bt[1:n_show, ]

  # Zone bar
  plots$bar <- ggplot2::ggplot(bt_top, ggplot2::aes(x = reorder(source, freq), y = freq, fill = zone)) +
    ggplot2::geom_bar(stat = "identity", color = "black", linewidth = 0.3) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = zone_colors, name = "Zone") +
    ggplot2::labs(title = "Bradford's Law Distribution", x = "Source", y = "Publications") +
    ieee_theme(base_size = 8) +
    ggplot2::theme(legend.position = c(0.75, 0.25), axis.text.y = ggplot2::element_text(size = 5))

  # Cumulative
  bt$cumulative <- cumsum(bt$freq) / sum(bt$freq) * 100
  bt$rank_pct <- bt$rank / max(bt$rank) * 100

  plots$cumulative <- ggplot2::ggplot(bt, ggplot2::aes(x = rank_pct, y = cumulative)) +
    ggplot2::geom_line(color = ieee_colors$blue, linewidth = 0.8) +
    ggplot2::geom_point(size = 0.8, color = ieee_colors$blue) +
    ggplot2::labs(title = "Bradford: Cumulative Distribution", x = "% Sources", y = "% Publications") +
    ggplot2::scale_x_continuous(breaks = seq(0, 100, 25)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 100, 25)) +
    ieee_theme(base_size = 8)

  list(status = "success", plots = plots, tables = list())
}
