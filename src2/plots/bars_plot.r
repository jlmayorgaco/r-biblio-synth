# src/plots/bar_plot.r
# -------------------------------------------------------------------
# IEEE Transactions Style Bar Plot for Top-N Countries (with Pareto)
# -------------------------------------------------------------------
bar_plot_topn <- function(
  df,
  value_col,
  top_n          = 10,
  fill_color     = "grey20",      # grayscale -> better for print
  outline_col    = NA,
  y_label        = NULL,
  title          = NULL,
  show_pareto    = TRUE,
  pareto_target  = 0.80,          # dashed guide at 80% cumulative
  ticks_n        = 5,             # fewer ticks for single-column figures
  label_mode     = c("auto","inside","outside","none"),
  label_top_k    = 3,             # annotate only top-3 (cleaner for print)
  inside_cut     = 0.35,          # share of max required to keep label inside
  label_size     = 2.8,           # good for single-column @ 3.5in
  label_pad_frac = 0.015,         # outside label pad as share of max
  base_family    = "Times New Roman"
) {
  stopifnot(is.data.frame(df), value_col %in% names(df))
  if (nrow(df) == 0) stop("bar_plot_topn(): empty data frame.")
  label_mode <- match.arg(label_mode)

  # --- slice, order, rank (modern dplyr; deterministic ties) -----------------
  df_top <- df |>
    dplyr::mutate(value = .data[[value_col]]) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::slice_max(order_by = value, n = top_n, with_ties = FALSE) |>
    dplyr::arrange(value) |>
    dplyr::mutate(
      rank      = dplyr::row_number(),                         # ascending
      country_r = paste0(top_n - rank + 1, ". ", .data$country)
    ) |>
    dplyr::mutate(country_r = stats::reorder(country_r, value))

  total_all <- sum(df[[value_col]], na.rm = TRUE)
  if (total_all <= 0) stop("bar_plot_topn(): total sum is zero; cannot plot.")
  max_val   <- max(df_top$value, na.rm = TRUE)
  pad       <- label_pad_frac * max_val

  df_top <- df_top |>
    dplyr::mutate(
      cum_value = cumsum(value),
      cum_share = cum_value / total_all,
      label     = scales::comma(value),
      inside_ok = value >= inside_cut * max_val,
      label_x   = dplyr::case_when(
        label_mode == "none"    ~ NA_real_,
        label_mode == "inside"  ~ value - pad,
        label_mode == "outside" ~ value + pad,
        TRUE                    ~ ifelse(inside_ok, value - pad, value + pad)
      ),
      hjust     = dplyr::case_when(
        is.na(label_x)   ~ 0.5,
        label_x <= value ~ 1,      # inside → right-justified (white text)
        TRUE             ~ 0       # outside → left-justified (black text)
      ),
      col       = ifelse(hjust == 1, "white", "black")
    )

  # Keep labels only for the top_k values (by value, not the rank string)
  if (is.finite(label_top_k)) {
    keep_idx <- tail(order(df_top$value), label_top_k)
    df_top$label  [!seq_len(nrow(df_top)) %in% keep_idx] <- NA_character_
    df_top$label_x[!seq_len(nrow(df_top)) %in% keep_idx] <- NA_real_
  }

  # --- base chart ------------------------------------------------------------
  p <- ggplot2::ggplot(df_top, ggplot2::aes(x = country_r, y = value)) +
    ggplot2::geom_col(fill = fill_color, colour = outline_col, width = 0.65) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::labs(x = NULL, y = y_label %||% "Publications (TP)", title = title) +
    theme_ieee(base_family = base_family) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold", size = 10, hjust = 0.5),
      axis.title.y       = ggplot2::element_blank(),
      axis.title.x       = ggplot2::element_text(size = 9, margin = ggplot2::margin(t = 6)),
      axis.text.y        = ggplot2::element_text(size = 9),
      axis.text.x        = ggplot2::element_text(size = 9),
      panel.grid.major.y = ggplot2::element_line(color = "grey80", linewidth = 0.3),
      panel.grid.major.x = ggplot2::element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor   = ggplot2::element_blank(),
      # a bit more room on the left for long country names, and on the right for labels
      plot.margin        = grid::unit(c(6, 14, 6, 10), "pt")
    )

  # Primary scale + optional secondary axis for cumulative share
  p <- p + ggplot2::scale_y_continuous(
    breaks   = scales::pretty_breaks(n = ticks_n),
    labels   = scales::comma,
    expand   = ggplot2::expansion(mult = c(0, 0.12)),
    sec.axis = if (show_pareto) {
      ggplot2::sec_axis(
        ~ . / total_all,
        name   = "Cumulative share",
        labels = scales::percent_format(accuracy = 1)
      )
    } else ggplot2::waiver()
  )

  # Pareto layer (subtle; behind labels)
  if (show_pareto) {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(y = cum_value, group = 1),
        linewidth = 0.45, color = "black"
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = cum_value),
        size = 1.4, color = "black", stroke = 0
      ) +
      ggplot2::geom_hline(
        yintercept = pareto_target * total_all,
        linetype = "dashed", linewidth = 0.35, colour = "grey35"
      )
  }

  # Value labels (use colour identity to avoid vector-length checks)
  df_lab <- dplyr::filter(df_top, !is.na(label) & !is.na(label_x))
  if (nrow(df_lab) > 0) {
    p <- p +
      ggplot2::geom_text(
        data = df_lab,
        ggplot2::aes(y = label_x, label = label, colour = col, hjust = hjust),
        vjust = 0.5, size = label_size, show.legend = FALSE
      ) +
      ggplot2::scale_colour_identity()
  }

  p
}




bar_plot_topn_with_lorenz_inset <- function(
  df,
  value_col,
  top_n = 10,
  fill_color = "steelblue",
  y_label = NULL,
  title = NULL,
  lorenz_metric_label = "Value",
  inset_position = c(0.62, 0.05, 1.00, 0.43) # left, bottom, right, top in npc
) {
  stopifnot(value_col %in% names(df))

  # Main bar chart (your existing helper)
  p_bar <- bar_plot_topn(
    df = df,
    value_col = value_col,
    top_n = top_n,
    fill_color = fill_color,
    y_label = y_label,
    title = title
  )

  # Lorenz inset (use our lorenz_plot that accepts `title =`)
  vals <- df[[value_col]]
  p_lorenz <- lorenz_plot(
    values = vals,
    title  = paste0("Lorenz — ", lorenz_metric_label),
    show_ylabel = FALSE
  )

  # Combine with patchwork inset
  p_bar + inset_element(
    p_lorenz,
    left   = inset_position[1],
    bottom = inset_position[2],
    right  = inset_position[3],
    top    = inset_position[4],
    align_to = "panel"
  )
}
