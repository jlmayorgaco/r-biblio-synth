# ============================================================================
# m1_render_bradford.R - Bradford Plots (IEEE REFACTORED)
# ============================================================================

#' @export
render_m1_bradford <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"bradford_table" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  bt <- result$bradford_table
  if (nrow(bt) == 0) return(list(status = "stub", plots = list(), tables = list()))

  plots <- list()

  zone_colors <- c(
    "Zone 1: Core" = ieee_colors$blue,
    "Zone 2: Moderate" = ieee_colors$orange,
    "Zone 3: Peripheral" = ieee_colors$green
  )

  # Show only top sources to avoid clutter
  n_show <- min(15, nrow(bt))
  bt_top <- bt[1:n_show, ]
  bt_top$source_clean <- sapply(bt_top$source, function(x) {
    if (nchar(x) > 25) paste0(substr(x, 1, 22), "...") else x
  })

  # 1. Zone bar chart
  plots$bar <- ggplot2::ggplot(bt_top, ggplot2::aes(x = reorder(source_clean, freq), y = freq, fill = zone)) +
    ggplot2::geom_col(color = "black", linewidth = 0.2) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = zone_colors, name = NULL) +
    ggplot2::labs(x = NULL, y = "Publications") +
    ggplot2::ggtitle(NULL) +
    ieee_theme(base_size = 6) +
    ggplot2::theme(legend.position = "right")

  # 2. Cumulative distribution (smooth)
  bt$cumulative <- cumsum(bt$freq) / sum(bt$freq) * 100
  bt$rank_pct <- bt$rank / max(bt$rank) * 100

  # Interpolate for smooth curve
  if (nrow(bt) >= 4) {
    smooth_x <- seq(0, 100, length.out = 200)
    smooth_y <- stats::spline(bt$rank_pct, bt$cumulative, xout = smooth_x, method = "monoH.FC")$y
    smooth_df <- data.frame(rank_pct = smooth_x, cumulative = smooth_y)
  } else {
    smooth_df <- bt[, c("rank_pct", "cumulative")]
  }

  plots$cumulative <- ggplot2::ggplot(smooth_df, ggplot2::aes(x = rank_pct, y = cumulative)) +
    ggplot2::geom_line(color = ieee_colors$blue, linewidth = 0.5) +
    ggplot2::labs(x = "Cumulative % of Sources", y = "Cumulative % of Publications") +
    ggplot2::scale_x_continuous(breaks = seq(0, 100, 25)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 100, 25)) +
    ggplot2::ggtitle(NULL) +
    ieee_theme()

  list(status = "success", plots = plots, tables = list())
}
