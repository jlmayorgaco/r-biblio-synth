# ============================================================================
# m3_render_lorenz.R - Lorenz curve plots for M3
# ============================================================================

#' Render Lorenz curves for production and citation inequality
#'
#' @param inequality_data Output from \code{m3_compute_inequality}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list with \code{status} and \code{plots}
#' @export
m3_render_lorenz <- function(inequality_data, config = biblio_config()) {
  stub <- list(status = "stub", plots = list())

  if (!is.list(inequality_data)) return(stub)

  plots <- list()

  # Helper: build a single Lorenz plot
  .lorenz_plot <- function(lorenz_df, gini, title, color) {
    if (nrow(lorenz_df) == 0) return(NULL)

    gini_val <- if (length(gini) > 0) suppressWarnings(as.numeric(gini[1])) else NA_real_
    gini_label <- if (length(gini_val) == 1L && is.finite(gini_val)) sprintf("Gini = %.3f", gini_val) else "Gini = N/A"

    # Smooth interpolation for display
    ent <- c(0, lorenz_df$cumulative_entities)
    val <- c(0, lorenz_df$cumulative_values)
    smooth_x <- seq(0, 1, length.out = 200)
    smooth_y <- tryCatch(
      stats::spline(ent, val, xout = smooth_x, method = "hyman")$y,
      error = function(e) stats::approx(ent, val, xout = smooth_x)$y
    )
    smooth_y <- pmax(0, pmin(1, smooth_y))
    lorenz_smooth <- data.frame(x = smooth_x, y = smooth_y)

    ggplot2::ggplot(lorenz_smooth, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = y, ymax = x),
                           fill = color, alpha = 0.10) +
      ggplot2::geom_line(color = color, linewidth = 0.6) +
      ggplot2::geom_abline(slope = 1, intercept = 0,
                           linetype = "dashed", color = ieee_colors$gray,
                           linewidth = 0.4) +
      ggplot2::annotate("text", x = 0.25, y = 0.72,
                        label = gini_label, size = 2.2, fontface = "italic") +
      ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 25)) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 25)) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1)) +
      ggplot2::labs(
        title = title,
        x     = "Cumulative % of Countries",
        y     = "Cumulative % of Output"
      ) +
      ieee_theme()
  }

  # Production Lorenz
  prod_lorenz <- inequality_data$production_inequality
  prod_gini   <- inequality_data$inequality_summary$production$gini
  p_prod <- .lorenz_plot(prod_lorenz, prod_gini,
                         "Lorenz Curve — Scientific Production",
                         ieee_colors$blue)
  if (!is.null(p_prod)) plots$lorenz_production <- p_prod

  # Citations Lorenz
  cit_lorenz <- inequality_data$citations_inequality
  cit_gini   <- inequality_data$inequality_summary$citations$gini
  p_cit <- .lorenz_plot(cit_lorenz, cit_gini,
                        "Lorenz Curve — Total Citations",
                        ieee_colors$orange)
  if (!is.null(p_cit)) plots$lorenz_citations <- p_cit

  # Combined overlay if both are available
  if (!is.null(p_prod) && !is.null(p_cit) &&
      nrow(prod_lorenz) > 0 && nrow(cit_lorenz) > 0) {

    combined <- dplyr::bind_rows(
      prod_lorenz %>% dplyr::mutate(metric = "Production"),
      cit_lorenz  %>% dplyr::mutate(metric = "Citations")
    )

    plots$lorenz_combined <- ggplot2::ggplot(
      combined,
      ggplot2::aes(x = cumulative_entities, y = cumulative_values,
                   color = metric, group = metric)
    ) +
      ggplot2::geom_line(linewidth = 0.6) +
      ggplot2::geom_abline(slope = 1, intercept = 0,
                           linetype = "dashed", color = ieee_colors$gray,
                           linewidth = 0.4) +
      ggplot2::scale_color_manual(
        values = c(Production = ieee_colors$blue, Citations = ieee_colors$orange)
      ) +
      ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 25)) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 25)) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1)) +
      ggplot2::labs(
        title = "Lorenz Curves: Production vs Citations",
        x     = "Cumulative % of Countries",
        y     = "Cumulative % of Output",
        color = NULL
      ) +
      ieee_theme()
  }

  list(status = "success", plots = plots)
}
