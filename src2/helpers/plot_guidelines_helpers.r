
# ================================================================
# Add parameter guides (generalized) from model$graphs()
# ================================================================
add_model_guides <- function(p,
                             model,
                             df,
                             colors = list(x = "darkred", y = "darkgreen", none = "blue"),
                             line_width = 0.4,
                             text_size = 2.5,
                             font_family = "Times New Roman") {
  # -- Validate model$graphs() existence
  if (!"graphs" %in% names(model)) {
    return(p)
  }

  guides <- tryCatch(model$graphs(), error = function(e) NULL)
  if (is.null(guides) || length(guides) == 0) {
    return(p)
  }

  # -- Helpers (single responsibility)
    add_y_guide <- function(p, g) {
    p +
        ggplot2::geom_hline(
        yintercept = g$param_value,
        linetype   = "dashed",
        color      = colors$y,
        linewidth  = line_width
        ) +
        ggplot2::annotate(
        "text",
        x      = min(df$Year, na.rm = TRUE),
        y      = g$param_value,
        label  = latex2exp::TeX(g$param_latex),
        hjust  = -0.5,
        vjust  = -0.5,
        family = font_family,
        size   = text_size,
        color  = colors$y
        )
    }

    add_x_guide <- function(p, g) {
    p +
      ggplot2::geom_vline(
        xintercept = g$param_value,
        linetype   = "dashed",
        color      = colors$x,
        linewidth  = line_width
      ) +
      ggplot2::annotate(
        "text",
        x      = g$param_value,
        y      = max(df$Value, na.rm = TRUE),
        label  = latex2exp::TeX(g$param_latex),
        hjust  = -0.5,
        vjust  = 1.2,     # <- cerca de la parte superior, pero visible
        family = font_family,
        size   = text_size,
        color  = colors$x
      )
  }


  add_fallback_guide <- function(p, g) {
    p +
      ggplot2::annotate(
        "text",
        x      = min(df$Year, na.rm = TRUE),
        y      = max(df$Value, na.rm = TRUE),
        label  = paste0(g$param_latex, "=", signif(g$param_value, 3)),
        hjust  = 0,
        vjust  = -1,
        family = font_family,
        size   = text_size,
        color  = colors$none
      )
  }

  # -- Apply guides
  for (g in guides) {
    if (is.null(g$param_axis) || is.null(g$param_value)) next

    axis <- tolower(g$param_axis)
    if (axis == "y") {
      p <- add_y_guide(p, g)
    } else if (axis == "x") {
      p <- add_x_guide(p, g)
    } else {
      p <- add_fallback_guide(p, g)
    }
  }

  return(p)
}
