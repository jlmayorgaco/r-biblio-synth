# src/plots/bar_plot.r

# -------------------------------------------------------------------
# IEEE-style Top-N bar chart with clearer spacing + annotated Pareto
# -------------------------------------------------------------------
bar_plot_topn <- function(
  df,
  value_col,
  top_n          = 10,
  fill_color     = "grey20",
  outline_col    = NA,
  y_label        = NULL,
  title          = NULL,
  show_pareto    = TRUE,
  pareto_target  = 0.80,
  ticks_n        = 4,              # fewer ticks => cleaner single-column
  label_mode     = c("auto","inside","outside","none"),
  label_top_k    = 3,
  inside_cut     = 0.35,
  label_size     = 2.8,
  label_pad_frac = 0.015,
  base_family    = "Times New Roman"
) {
  stopifnot(is.data.frame(df), value_col %in% names(df))
  if (nrow(df) == 0) stop("bar_plot_topn(): empty data frame.")
  label_mode <- match.arg(label_mode)

  # ---- pick & rank ----------------------------------------------------------
  df_top <- df |>
    dplyr::mutate(value = .data[[value_col]]) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::slice_max(order_by = value, n = top_n, with_ties = FALSE) |>
    dplyr::arrange(value) |>
    dplyr::mutate(
      rank      = dplyr::row_number(),
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
        label_x <= value ~ 1,
        TRUE             ~ 0
      ),
      col       = ifelse(hjust == 1, "white", "black")
    )

  if (is.finite(label_top_k)) {
    keep_idx <- tail(order(df_top$value), label_top_k)
    df_top$label  [!seq_len(nrow(df_top)) %in% keep_idx] <- NA_character_
    df_top$label_x[!seq_len(nrow(df_top)) %in% keep_idx] <- NA_real_
  }

  # headroom (more if labels are outside)
  headroom_mult <- if (label_mode == "outside") 1.15 else 1.05
  y_max_limit   <- max_val * headroom_mult

  # ---- plot -----------------------------------------------------------------
  p <- ggplot2::ggplot(df_top, ggplot2::aes(x = country_r, y = value)) +
    ggplot2::geom_col(fill = fill_color, colour = outline_col, width = 0.85) +  # thinner -> more space between bars
    ggplot2::coord_flip(clip = "off") +
    ggplot2::labs(x = NULL, y = y_label %||% "Publications (TP)", title = title) +
    # add padding between categories so labels aren’t cramped
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.1, 0.1))) +
    theme_ieee(base_family = base_family) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold", size = 10, hjust = 0.5),
      axis.title.y       = ggplot2::element_blank(),
      axis.title.x       = ggplot2::element_text(size = 9, margin = ggplot2::margin(t = 6)),
      axis.text.y        = ggplot2::element_text(size = 9, lineheight = 0.7, margin = ggplot2::margin(r = 3)), # clearer stacking
      axis.text.x        = ggplot2::element_text(size = 9),
      panel.grid.major.y = ggplot2::element_line(color = "grey85", linewidth = 0.3),
      panel.grid.major.x = ggplot2::element_line(color = "grey88", linewidth = 0.3),
      panel.grid.minor   = ggplot2::element_blank(),
      plot.margin        = grid::unit(c(6, 14, 6, 10), "pt")
    )

  # numeric axis: no padding, fewer ticks, tight limits so bars fill width
  p <- p + ggplot2::scale_y_continuous(
    limits   = c(0, y_max_limit),
    breaks   = scales::pretty_breaks(n = ticks_n),
    labels   = scales::comma,
    expand   = c(0, 0),
    sec.axis = if (show_pareto) {
      ggplot2::sec_axis(~ . / total_all, name = "Cumulative share",
                        labels = scales::percent_format(accuracy = 1))
    } else ggplot2::waiver()
  )

  # ---- Pareto layer + concise explanations ----------------------------------
  if (show_pareto) {
    # point where 80% is first reached
    star_idx <- which(df_top$cum_share >= pareto_target)[1]
    star_x   <- if (length(star_idx)) df_top$country_r[star_idx] else NA
    star_y   <- pareto_target * total_all

    # overall share of Top-N
    topn_share <- tail(df_top$cum_share, 1)

    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = cum_value, group = 1),
                         linewidth = 0.45, color = "black") +
      ggplot2::geom_point(ggplot2::aes(y = cum_value),
                          size = 1.4, color = "black", stroke = 0) +
      ggplot2::geom_hline(yintercept = star_y, linetype = "dashed",
                          linewidth = 0.35, colour = "grey35")

    # annotate “80% reached at k countries”
    if (!is.na(star_x)) {
      p <- p +
        ggplot2::annotate(
          "text",
          x = star_x, y = star_y,
          label = paste0(round(pareto_target*100), "% reached at Top-", (top_n - star_idx + 1)),
          vjust = -0.6, size = 2.5, family = base_family
        )
    }

    # annotate Top-N share on the last point
    p <- p +
      ggplot2::annotate(
        "text",
        x = tail(df_top$country_r, 1),
        y = tail(df_top$cum_value, 1),
        label = paste0("Top-", top_n, " share = ", scales::percent(topn_share, 1)),
        vjust = -0.6, size = 2.5, family = base_family
      )
  }

  # value labels
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
