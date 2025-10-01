
# helpers/helpers__m3_countries/95_continent_plots.R
# IEEE-ready continent aggregations & plots (robust to empty data & older ggplot2)

# ──────────────────────────────────────────────────────────────────────────────
# IEEE styling utilities
# ──────────────────────────────────────────────────────────────────────────────

# Single-column ~3.42in; 1.5-col ~5.0in; double ~7.16in
m3c_ieee_dims <- function(kind = c("single", "onehalf", "double"), aspect = 0.62) {
  kind <- match.arg(kind)
  w <- switch(kind,
              single  = 3.42,   # ~87 mm
              onehalf = 5.0,
              double  = 7.16)   # ~182 mm
  h <- max(1.6, w * aspect)
  list(width_in = w, height_in = h, dpi = 600)
}

# Minimal, high-contrast IEEE look (grayscale only).
# base_family defaults to "Times" for an IEEE-ish serif look (change if needed).
m3c_ieee_theme <- function(base_size = 8.5, base_family = "Times") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(size = 0.25, colour = "grey70"),
      axis.ticks         = ggplot2::element_line(size = 0.25, colour = "black"),
      axis.ticks.length  = grid::unit(2, "pt"),
      axis.title.x       = ggplot2::element_text(margin = ggplot2::margin(t = 4)),
      axis.title.y       = ggplot2::element_text(margin = ggplot2::margin(r = 4)),
      plot.title         = ggplot2::element_text(face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 4)),
      legend.key.size    = grid::unit(6, "pt"),
      legend.text        = ggplot2::element_text(size = base_size),
      legend.title       = ggplot2::element_blank()
    )
}

# Save helper: PNG (600dpi) + SVG, IEEE dimensions
m3c_ieee_save <- function(p, path_base,
                          kind = c("single","double","onehalf"),
                          aspect = 0.62,
                          formats = c("png","svg"),
                          dpi = 600) {
  kind <- match.arg(kind)
  dims <- m3c_ieee_dims(kind = kind, aspect = aspect)
  dir.create(dirname(path_base), recursive = TRUE, showWarnings = FALSE)
  if ("png" %in% formats) {
    ggplot2::ggsave(paste0(path_base, ".png"), plot = p,
                    width = dims$width_in, height = dims$height_in,
                    units = "in", dpi = dpi, device = "png", bg = "white")
  }
  if ("svg" %in% formats) {
    ggplot2::ggsave(paste0(path_base, ".svg"), plot = p,
                    width = dims$width_in, height = dims$height_in,
                    units = "in", dpi = dpi, device = "svg", bg = "white")
  }
  invisible(list(width_in = dims$width_in, height_in = dims$height_in, dpi = dpi))
}

# ──────────────────────────────────────────────────────────────────────────────
# Period helper: two recent windows (default 5 & 5 years)
# ──────────────────────────────────────────────────────────────────────────────

m3c_period_labels <- function(end_year = as.integer(format(Sys.Date(), "%Y")),
                              len1 = 5, len2 = 5) {
  stopifnot(len1 > 0, len2 > 0)
  start2 <- end_year - len2 + 1L
  end1   <- start2 - 1L
  start1 <- end1 - len1 + 1L
  list(
    list(start = start1, end = end1, label = sprintf("%d–%d", start1, end1)),
    list(start = start2, end = end_year, label = sprintf("%d–%d", start2, end_year))
  )
}

# ──────────────────────────────────────────────────────────────────────────────
# Guards & empty plot
# ──────────────────────────────────────────────────────────────────────────────

.m3c_continent_df_guard <- function(df_cy) {
  need <- c("continent","year","TP","TC")
  if (!all(need %in% names(df_cy))) {
    stop("df_cy must contain columns: continent, year, TP, TC.")
  }
  df_cy %>%
    dplyr::filter(!is.na(continent) & nzchar(continent)) %>%
    dplyr::mutate(year = suppressWarnings(as.integer(as.character(year)))) %>%
    dplyr::filter(!is.na(year))
}

.m3c_empty_plot <- function(msg = "No data") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg, size = 3) +
    ggplot2::theme_void()
}

# ──────────────────────────────────────────────────────────────────────────────
# 1) Totals by continent (TP or TC) — IEEE bars
# ──────────────────────────────────────────────────────────────────────────────

m3c_plot_continent_totals <- function(df_cy,
                                      value_col = c("TP","TC"),
                                      title = NULL,
                                      ylab  = NULL,
                                      out_base = NULL,   # e.g., ".../continent_tp_totals"
                                      kind = "single",
                                      aspect = 0.62,
                                      label_format = scales::label_number(big.mark = ",",
                                                                          accuracy = 1)) {
  df_cy <- .m3c_continent_df_guard(df_cy)
  value_col <- match.arg(value_col)

  agg <- df_cy %>%
    dplyr::group_by(continent) %>%
    dplyr::summarise(Value = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(is.finite(Value)) %>%
    dplyr::arrange(dplyr::desc(Value))

  if (!nrow(agg)) {
    p <- .m3c_empty_plot("No continent data")
    if (!is.null(out_base)) m3c_ieee_save(p, out_base, kind = kind, aspect = aspect)
    return(p)
  }

  if (is.null(title)) title <- sprintf("%s by Continent (All Years)", value_col)
  if (is.null(ylab))  ylab  <- value_col

  p <- ggplot2::ggplot(agg, ggplot2::aes(x = reorder(continent, Value), y = Value)) +
    ggplot2::geom_col(fill = "grey80", colour = "black", linewidth = 0.25, width = 0.75) +
    ggplot2::scale_x_discrete(expand = c(0.1, 0.1)) +
    ggplot2::scale_y_continuous(expand = c(0.02, 0.06), labels = label_format) +
    ggplot2::labs(x = NULL, y = ylab, title = title) +
    m3c_ieee_theme()

  if (!is.null(out_base)) m3c_ieee_save(p, out_base, kind = kind, aspect = aspect)
  p
}

# ──────────────────────────────────────────────────────────────────────────────
# 2) Two-period continent SHARE plots + ABSOLUTE plots (TP & TC) — IEEE style
# ──────────────────────────────────────────────────────────────────────────────
m3c_plot_continent_share_trend <- function(df_cy,
                                           value_col = c("TP","TC"), # kept for API compat; not used
                                           periods   = m3c_period_labels(),
                                           title     = NULL,          # optional title prefix
                                           out_base  = NULL,          # e.g., ".../continent_share_trend"
                                           kind      = "single",
                                           aspect    = 0.52,
                                           percent_format = scales::percent_format(accuracy = 1),
                                           show_labels = TRUE,        # add % labels on bars
                                           label_size  = 2.6,
                                           csv_digits  = 4) {

  # Guard & inputs
  df_cy <- .m3c_continent_df_guard(df_cy)
  stopifnot(length(periods) == 2)
  p1 <- periods[[1]]; p2 <- periods[[2]]

  # Filter data for each period (inclusive)
  df_p1 <- df_cy %>% dplyr::filter(year >= p1$start, year <= p1$end)
  df_p2 <- df_cy %>% dplyr::filter(year >= p2$start, year <= p2$end)

  # Aggregate counts by continent for each period (TP & TC)
  agg_p <- function(d) d %>%
    dplyr::group_by(continent) %>%
    dplyr::summarise(TP = sum(as.numeric(TP), na.rm = TRUE),
                     TC = sum(as.numeric(TC), na.rm = TRUE),
                     .groups = "drop")

  df1 <- agg_p(df_p1) %>% dplyr::mutate(Period = p1$label)
  df2 <- agg_p(df_p2) %>% dplyr::mutate(Period = p2$label)

  # Period totals (for metadata/return)
  totals <- list(
    TP_P1 = sum(df1$TP), TC_P1 = sum(df1$TC),
    TP_P2 = sum(df2$TP), TC_P2 = sum(df2$TC)
  )

  # Build SHARE tables (% per period)
  shares_of <- function(d, col) {
    tot <- sum(d[[col]], na.rm = TRUE)
    if (tot > 0) d[[col]] / tot else 0
  }
  df1 <- df1 %>% dplyr::mutate(Share_TP = shares_of(df1, "TP"), Share_TC = shares_of(df1, "TC"))
  df2 <- df2 %>% dplyr::mutate(Share_TP = shares_of(df2, "TP"), Share_TC = shares_of(df2, "TC"))

  # Long tables for plotting (share & absolute)
  all_cont <- sort(unique(c(df1$continent, df2$continent)))
  x_lvls   <- c(p1$label, p2$label)

  tp_share <- dplyr::bind_rows(
    df1 %>% dplyr::select(Period, continent, Value = Share_TP),
    df2 %>% dplyr::select(Period, continent, Value = Share_TP)
  ) %>% tidyr::complete(Period = x_lvls, continent = all_cont, fill = list(Value = 0)) %>%
       dplyr::mutate(Period = factor(Period, levels = x_lvls))

  tc_share <- dplyr::bind_rows(
    df1 %>% dplyr::select(Period, continent, Value = Share_TC),
    df2 %>% dplyr::select(Period, continent, Value = Share_TC)
  ) %>% tidyr::complete(Period = x_lvls, continent = all_cont, fill = list(Value = 0)) %>%
       dplyr::mutate(Period = factor(Period, levels = x_lvls))

  tp_abs <- dplyr::bind_rows(
    df1 %>% dplyr::select(Period, continent, Value = TP),
    df2 %>% dplyr::select(Period, continent, Value = TP)
  ) %>% tidyr::complete(Period = x_lvls, continent = all_cont, fill = list(Value = 0)) %>%
       dplyr::mutate(Period = factor(Period, levels = x_lvls))

  tc_abs <- dplyr::bind_rows(
    df1 %>% dplyr::select(Period, continent, Value = TC),
    df2 %>% dplyr::select(Period, continent, Value = TC)
  ) %>% tidyr::complete(Period = x_lvls, continent = all_cont, fill = list(Value = 0)) %>%
       dplyr::mutate(Period = factor(Period, levels = x_lvls))

  # Legend order = latest-period ranking (use TP share to keep all panels consistent)
  ord_global <- tp_share %>%
    dplyr::filter(Period == p2$label) %>%
    dplyr::arrange(dplyr::desc(Value)) %>%
    dplyr::pull(continent) %>% unique()

  # ── Plot builders ───────────────────────────────────────────────────────────

  # SHARE plots (labels only when slice ≥ 5%; small slices semi-transparent)
  build_share_plot <- function(dat, ttl_suffix,
                               label_threshold = 0.05,  # 5%
                               small_alpha     = 0.20,
                               label_size      = 2.6,
                               label_pad       = 0.015) {

    dat$continent <- factor(dat$continent, levels = ord_global)
    dat$label_txt <- ifelse(dat$Value >= label_threshold,
                            scales::percent(dat$Value, accuracy = 1), "")
    dat$.big <- dat$Value >= label_threshold

    ggplot2::ggplot(
      dat,
      ggplot2::aes(x = Period, y = Value,
                   fill  = continent,
                   alpha = .big)
    ) +
      ggplot2::geom_col(position = "stack",
                        colour   = "black",
                        linewidth= 0.25,
                        width    = 0.75) +
      ggplot2::geom_text(
        ggplot2::aes(label = label_txt),
        position = ggplot2::position_stack(vjust = 1),
        vjust = +1.6, size = label_size
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, 1 + label_pad),
        breaks = c(0, .25, .5, .75, 1),
        expand = c(0, 0.02)
      ) +
      ggplot2::scale_fill_grey(start = 0.9, end = 0.2, name = NULL) +
      ggplot2::scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = small_alpha), guide = "none") +
      ggplot2::labs(
        x = NULL, y = NULL,
        title = if (is.null(title))
          sprintf("Continent Share of %s\n(%s vs %s)", ttl_suffix, p1$label, p2$label)
        else sprintf("%s — %s", title, ttl_suffix)
      ) +
      m3c_ieee_theme() +
      ggplot2::theme(legend.position = "right",
                     plot.margin = ggplot2::margin(t = 3, r = 3, b = 3, l = 3)) +
      ggplot2::coord_cartesian(clip = "off")
  }

  # ABSOLUTE plots
  build_abs_plot <- function(dat, ttl_suffix) {
    dat$continent <- factor(dat$continent, levels = ord_global)
    labnum <- scales::label_number(big.mark = ",", accuracy = 1)
    ggplot2::ggplot(dat, ggplot2::aes(x = Period, y = Value, fill = continent)) +
      ggplot2::geom_col(position = "stack", width = 0.75, colour = "black", linewidth = 0.25) +
      ggplot2::scale_y_continuous(labels = labnum, expand = c(0.02, 0.02)) +
      ggplot2::scale_fill_grey(start = 0.9, end = 0.2, name = NULL) +
      ggplot2::labs(
        x = NULL, y = NULL,
        title = if (is.null(title))
          sprintf("Continent Totals of %s\n(%s vs %s)", ttl_suffix, p1$label, p2$label)
        else sprintf("%s — %s (Totals)", title, ttl_suffix)
      ) +
      m3c_ieee_theme() +
      ggplot2::theme(legend.position = "right")
  }

  # Build plots
  p_tp_share <- build_share_plot(tp_share, "Publications (TP)")
  p_tc_share <- build_share_plot(tc_share, "Citations (TC)")
  p_tp_abs   <- build_abs_plot  (tp_abs,   "Publications (TP)")
  p_tc_abs   <- build_abs_plot  (tc_abs,   "Citations (TC)")

  # Save figures + CSVs if requested ------------------------------------------
  if (!is.null(out_base)) {
    m3c_ieee_save(p_tp_share, paste0(out_base, "_tp_share"),     kind = kind, aspect = aspect)
    m3c_ieee_save(p_tc_share, paste0(out_base, "_tc_share"),     kind = kind, aspect = aspect)
    m3c_ieee_save(p_tp_abs,   paste0(out_base, "_tp_absolute"),  kind = kind, aspect = aspect)
    m3c_ieee_save(p_tc_abs,   paste0(out_base, "_tc_absolute"),  kind = kind, aspect = aspect)

    # CSV exports (counts & shares)
    dir.create(dirname(out_base), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(
      dplyr::bind_rows(df1, df2) %>%
        dplyr::select(Period, continent, TP, TC, Share_TP, Share_TC) %>%
        dplyr::mutate(Share_TP = round(Share_TP, csv_digits),
                      Share_TC = round(Share_TC, csv_digits)),
      paste0(out_base, "_tables.csv")
    )
    readr::write_csv(tp_share %>% dplyr::mutate(Value = round(Value, csv_digits)),
                     paste0(out_base, "_tp_share_table.csv"))
    readr::write_csv(tc_share %>% dplyr::mutate(Value = round(Value, csv_digits)),
                     paste0(out_base, "_tc_share_table.csv"))
    readr::write_csv(tp_abs, paste0(out_base, "_tp_absolute_table.csv"))
    readr::write_csv(tc_abs, paste0(out_base, "_tc_absolute_table.csv"))
  }

  # Return objects for further use
  invisible(list(
    plots  = list(tp_share = p_tp_share, tc_share = p_tc_share,
                  tp_absolute = p_tp_abs, tc_absolute = p_tc_abs),
    tables = list(period1 = df1, period2 = df2,
                  tp_share = tp_share, tc_share = tc_share,
                  tp_absolute = tp_abs, tc_absolute = tc_abs),
    totals = totals,
    order  = ord_global
  ))
}

# ──────────────────────────────────────────────────────────────────────────────
# 3) 2×2 grid (TP share, TC share, TP absolute, TC absolute) with ONE legend
# ──────────────────────────────────────────────────────────────────────────────
m3c_continent_trends_grid_2x2 <- function(df_cy,
                                          periods   = m3c_period_labels(),
                                          out_base  = NULL,          # path base (no extension)
                                          kind_save = c("single","double"),
                                          aspect    = 0.95) {

  kind_save <- match.arg(kind_save)

  res <- m3c_plot_continent_share_trend(
    df_cy, periods = periods, title = NULL, out_base = NULL
  )

  # Pull panels and make sure each hides its own legend (we’ll add one shared)
  p_tp_share <- res$plots$tp_share + ggplot2::theme(legend.position = "right")
  p_tc_share <- res$plots$tc_share + ggplot2::theme(legend.position = "right")
  p_tp_abs   <- res$plots$tp_absolute + ggplot2::theme(legend.position = "right")
  p_tc_abs   <- res$plots$tc_absolute + ggplot2::theme(legend.position = "right")

  have_cow <- requireNamespace("cowplot", quietly = TRUE)
  have_grd <- requireNamespace("gridExtra", quietly = TRUE)

  get_legend <- function(p) {
    if (have_cow) {
      cowplot::get_legend(p + ggplot2::theme(legend.position = "right"))
    } else {
      g <- ggplot2::ggplotGrob(p + ggplot2::theme(legend.position = "right"))
      g$grobs[[which(sapply(g$grobs, function(x) x$name) == "guide-box")]]
    }
  }
  shared_legend <- get_legend(p_tp_share)

  if (have_cow) {
    grid_2x2 <- cowplot::plot_grid(
      p_tp_share + ggplot2::theme(legend.position="none"),
      p_tc_share + ggplot2::theme(legend.position="none"),
      p_tp_abs   + ggplot2::theme(legend.position="none"),
      p_tc_abs   + ggplot2::theme(legend.position="none"),
      labels = c("A","B","C","D"),
      label_fontfamily = "Times",
      label_size = 7, label_fontface = "bold",
      ncol = 2, align = "hv"
    )
    final <- cowplot::plot_grid(grid_2x2, shared_legend, ncol = 2, rel_widths = c(1, 0.30))
  } else if (have_grd) {
    grid <- gridExtra::arrangeGrob(
      p_tp_share + ggplot2::theme(legend.position="none"),
      p_tc_share + ggplot2::theme(legend.position="none"),
      p_tp_abs   + ggplot2::theme(legend.position="none"),
      p_tc_abs   + ggplot2::theme(legend.position="none"),
      ncol = 2
    )
    # place legend to the right
    final <- gridExtra::arrangeGrob(grid, shared_legend,
                                    widths = grid::unit.c(grid::unit(1, "null"),
                                                          grid::unit(0.30, "null")),
                                    ncol = 2)
  } else {
    stop("Please install 'cowplot' (recommended) or 'gridExtra' to build the 2×2 grid.")
  }

  if (!is.null(out_base)) {
    # Save both single- and double-column versions for convenience
    m3c_ieee_save(final, paste0(out_base, "_2x2_single"),
                  kind = "single", aspect = aspect)
    m3c_ieee_save(final, paste0(out_base, "_2x2_double"),
                  kind = "double", aspect = aspect)
  }

  invisible(list(
    plot   = final,
    panels = list(tp_share = p_tp_share, tc_share = p_tc_share,
                  tp_absolute = p_tp_abs, tc_absolute = p_tc_abs),
    order  = res$order
  ))
}
