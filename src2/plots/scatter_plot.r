# ================================================================
# IEEE-style scatter with hollow points, optional connect/smooth,
# year-friendly x-axis, and percent y-axis when requested.
# ================================================================
plot_scatter <- function(
  df,
  x_col,
  y_col,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  xlabel = NULL,
  ylabel = NULL,
  x_breaks = NULL,          # pass sort(unique(df$year)) for discrete years
  y_limits = NULL,          # e.g. c(0, 1) for ratios
  y_percent = FALSE,        # show y as percentages (0..1 -> 0%..100%)
  y_percent_breaks = seq(0, 1, by = 0.2),
  connect = c("line","none","loess"),  # default "line" (no SE)
  loess_span = 0.75,
  hollow_points = TRUE,
  point_size = 1.8,
  point_stroke = 0.55,
  ieee_base_size = 8,
  base_family = "Times New Roman",
  drop_na = TRUE
) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  stopifnot(requireNamespace("rlang", quietly = TRUE))
  stopifnot(requireNamespace("scales", quietly = TRUE))

  connect <- match.arg(connect)

  x_col <- rlang::ensym(x_col)
  y_col <- rlang::ensym(y_col)

  df_plot <- df
  if (drop_na) {
    df_plot <- df_plot[stats::complete.cases(df_plot[, c(rlang::as_name(x_col), rlang::as_name(y_col))]), ]
  }

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = !!x_col, y = !!y_col))

  # Hollow points (outline + blank center)
  if (hollow_points) {
    p <- p + ggplot2::geom_point(shape = 21, fill = "white", color = "black",
                                 size = point_size, stroke = point_stroke)
  } else {
    p <- p + ggplot2::geom_point(size = point_size, alpha = 0.9)
  }

  # Connect points
  if (connect == "line") {
    p <- p + ggplot2::geom_line(linewidth = 0.35, alpha = 0.9)
  } else if (connect == "loess") {
    p <- p + ggplot2::geom_smooth(method = "loess", span = loess_span, se = FALSE,
                                  linewidth = 0.5, formula = y ~ x)
  }

  # Labels
  p <- p + ggplot2::labs(
    title = title, subtitle = subtitle, caption = caption,
    x = if (is.null(xlabel)) rlang::as_name(x_col) else xlabel,
    y = if (is.null(ylabel)) rlang::as_name(y_col) else ylabel
  )

  # X as years: plain integers (no commas)
  if (is.null(x_breaks)) {
    yrs <- sort(unique(df_plot[[rlang::as_name(x_col)]]))
    x_breaks <- if (length(yrs) <= 10) yrs else scales::breaks_pretty(n = 8)(range(yrs))
  }
  p <- p + ggplot2::scale_x_continuous(
    breaks = x_breaks,
    labels = scales::label_number(accuracy = 1, big.mark = "")
  )

  # Y scale (percent or numeric)
  if (y_percent) {
    p <- p + ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      breaks = y_percent_breaks,
      limits = y_limits
    )
  } else {
    p <- p + ggplot2::scale_y_continuous(
      labels = scales::label_number(big.mark = ","),
      limits = y_limits
    )
  }

  # IEEE-ish minimal theme
  p <- p + ggplot2::theme_minimal(base_size = ieee_base_size, base_family = base_family)
  p <- p + ggplot2::theme(
    plot.title.position = "plot",
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold",
                                       margin = ggplot2::margin(b = 3)),
    plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                          margin = ggplot2::margin(b = 4)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 3)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 3)),
    axis.text = ggplot2::element_text(color = "black"),
    panel.grid.major = ggplot2::element_line(linewidth = 0.25, color = "grey80"),
    panel.grid.minor = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(linewidth = 0.4, color = "black"),
    axis.ticks = ggplot2::element_line(linewidth = 0.3, color = "black"),
    legend.position = "none",
    plot.margin = ggplot2::margin(t = 3, r = 6, b = 3, l = 6)
  )

  return(p)
}
