# ============================================================================
# m3_render_change_points.R - Change-point visualization for M3
# ============================================================================

#' Render change-point analysis plots
#'
#' @param cp_data Output from \code{m3_compute_change_points}
#' @param growth_data Output from \code{m3_compute_growth_dynamics}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list with \code{status} and \code{plots}
#' @export
m3_render_change_points <- function(cp_data, growth_data, config = biblio_config()) {
  stub <- list(status = "stub", plots = list())

  if (!is.list(cp_data)) return(stub)

  prod_cp   <- cp_data$productivity_change_points
  annual_ts <- if (is.list(growth_data)) growth_data$annual_productivity else tibble::tibble()

  if (nrow(prod_cp) == 0 || nrow(annual_ts) == 0) {
    return(stub)
  }

  plots <- list()

  # For each country with a detected change point, annotate the time series.
  # We'll make one combined faceted plot.
  countries_with_cp <- unique(prod_cp$country)

  plot_data <- annual_ts %>%
    dplyr::filter(country %in% countries_with_cp)

  if (nrow(plot_data) == 0) return(stub)

  cp_lines <- prod_cp %>%
    dplyr::select(country, change_point_year) %>%
    dplyr::filter(!is.na(change_point_year))

  plots$change_point_facet <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = PY, y = article_count)
  ) +
    ggplot2::geom_line(color = ieee_colors$blue, linewidth = 0.6) +
    ggplot2::geom_point(size = 1.0, color = ieee_colors$blue) +
    ggplot2::geom_vline(
      data   = cp_lines,
      ggplot2::aes(xintercept = change_point_year),
      linetype = "dashed",
      color    = ieee_colors$red,
      linewidth = 0.5
    ) +
    ggplot2::facet_wrap(~ country, scales = "free_y") +
    ggplot2::labs(
      title    = "Detected Change Points in Annual Production (Exploratory)",
      subtitle = "Dashed line = estimated structural break (mean-shift heuristic)",
      x        = "Year",
      y        = "Publications"
    ) +
    ieee_theme() +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 6))

  list(status = "success", plots = plots)
}
