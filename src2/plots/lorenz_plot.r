# src/plots/lorenz_plot.r
# -------------------------------------------------------------------
# Lorenz Curve Plot (with Gini coefficient in title)
# -------------------------------------------------------------------
lorenz_plot <- function(values, title = NULL, show_ylabel = FALSE, base_size = 9) {
  lc <- ineq::Lc(values)
  df_lc <- data.frame(p = lc$p, L = lc$L)
  gini_val <- round(ineq::Gini(values), 3)

  if (is.null(title)) {
    title <- paste0("Lorenz Curve (Gini = ", gini_val, ")")
  } else {
    # append Gini to any provided title
    title <- paste0(title, " (Gini = ", gini_val, ")")
  }

  ggplot(df_lc, aes(x = p, y = L)) +
    geom_line(linewidth = 0.5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.4) +
    labs(x = "Cumulative share of countries", y = if (show_ylabel) "Cumulative share" else NULL, title = title) +
    theme_ieee(base_size = base_size)
}



