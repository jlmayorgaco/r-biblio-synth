# ============================================================================
# m3_render_rankings.R - Rank structure plots for M3
# ============================================================================

#' Render rank structure plots
#'
#' @param rankings_data Output from \code{m3_compute_rankings}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list with \code{status} and \code{plots}
#' @export
m3_render_rankings <- function(rankings_data, config = biblio_config()) {
  stub <- list(status = "stub", plots = list())

  if (!is.list(rankings_data) || nrow(rankings_data$production_rankings) == 0) {
    return(stub)
  }

  plots <- list()

  # 1. Pareto-style cumulative production plot
  prod_rank <- rankings_data$production_rankings %>%
    dplyr::mutate(
      cumulative_share = cumsum(value) / sum(value)
    )

  plots$production_rank_cumshare <- ggplot2::ggplot(
    prod_rank,
    ggplot2::aes(x = rank, y = cumulative_share)
  ) +
    ggplot2::geom_area(fill = ieee_colors$blue, alpha = 0.15) +
    ggplot2::geom_line(color = ieee_colors$blue, linewidth = 0.6) +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed",
                        color = ieee_colors$gray, linewidth = 0.4) +
    ggplot2::geom_hline(yintercept = 0.8, linetype = "dotted",
                        color = ieee_colors$gray, linewidth = 0.4) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                limits = c(0, 1)) +
    ggplot2::labs(
      title = "Cumulative Production Share by Country Rank",
      x     = "Country Rank",
      y     = "Cumulative Share"
    ) +
    ieee_theme()

  # 2. Citations rank plot (if available)
  cit_rank <- rankings_data$citations_rankings
  if (nrow(cit_rank) >= 2 && sum(cit_rank$value) > 0) {
    cit_rank <- cit_rank %>%
      dplyr::mutate(cumulative_share = cumsum(value) / sum(value))

    plots$citations_rank_cumshare <- ggplot2::ggplot(
      cit_rank,
      ggplot2::aes(x = rank, y = cumulative_share)
    ) +
      ggplot2::geom_area(fill = ieee_colors$orange, alpha = 0.15) +
      ggplot2::geom_line(color = ieee_colors$orange, linewidth = 0.6) +
      ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed",
                          color = ieee_colors$gray, linewidth = 0.4) +
      ggplot2::geom_hline(yintercept = 0.8, linetype = "dotted",
                          color = ieee_colors$gray, linewidth = 0.4) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                  limits = c(0, 1)) +
      ggplot2::labs(
        title = "Cumulative Citation Share by Country Rank",
        x     = "Country Rank",
        y     = "Cumulative Share"
      ) +
      ieee_theme()
  }

  list(status = "success", plots = plots)
}
