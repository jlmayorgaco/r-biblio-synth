# ============================================================================ #
# helpers__m1_data_ingestion/08_reporting.R
# EDA reporting: LaTeX table + compact multi-panel figure (IEEE-ready)
# Refactored: one function per subplot panel for easy debugging
# ============================================================================ #

suppressPackageStartupMessages({
  library(ggplot2)
  library(cowplot)
})

# ---- numeric/percent helpers (LaTeX-safe vs plot-safe) --------------------- #
fmt_num <- function(x, digits = 2) {
  if (is.null(x) || is.na(x)) "—" else formatC(as.numeric(x), format = "f", digits = digits)
}
fmt_int <- function(x) {
  if (is.null(x) || is.na(x)) "—" else sprintf("%s", x)
}
# For LaTeX content:
fmt_pct <- function(x, digits = 2) {
  if (is.null(x) || is.na(x)) "—" else paste0(fmt_num(x, digits), "\\%")
}
fmt_pct100 <- function(x, digits = 2) {
  if (is.null(x) || is.na(x)) "—" else paste0(fmt_num(100 * as.numeric(x), digits), "\\%")
}
# For plot titles/labels (plain %):
fmt_pct_plot <- function(x, digits = 2) {
  if (is.null(x) || is.na(x)) "—" else paste0(fmt_num(x, digits), "%")
}

# ASCII-safe text helper (avoids device errors with non-ASCII glyphs)
safe_txt <- function(s) {
  z <- try(iconv(s, to = "ASCII//TRANSLIT"), silent = TRUE)
  if (inherits(z, "try-error") || is.na(z)) s else z
}

m1i_ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

# ---- theme + small utilities ------------------------------------------------ #
m1i_theme_ieee <- function(base_sz = 8) {
  ggplot2::theme_minimal(base_size = base_sz) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "bold", margin = ggplot2::margin(b = 2)),
      axis.title = ggplot2::element_blank(),
      legend.position = "none",
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(2, 2, 2, 2),
      axis.text.y = ggplot2::element_text(lineheight = 1.05)
    )
}
m1i_wrap <- function(x, width) stringr::str_wrap(x, width = width)

# ---------------------------------------------------------------------------- #
# LaTeX EDA table
# ---------------------------------------------------------------------------- #
m1i_export_eda_tex <- function(eda, out_dir = "results2/M1", file = "EDA_Summary.tex") {
  m1i_ensure_dir(out_dir)
  stopifnot(is.list(eda), "overview" %in% names(eda),
            "completeness" %in% names(eda), "citations" %in% names(eda))

  ov   <- eda$overview
  cmp  <- eda$completeness
  cit  <- eda$citations
  ven  <- eda$venues
  coll <- eda$authorship_collab

  vals <- list(
    "Documents (n)"                  = fmt_int(ov$n_docs),
    "Years covered"                  = if (!is.null(ov$year_span)) sprintf("%s–%s", ov$year_span$min, ov$year_span$max) else "—",
    "Median / Mode year"             = sprintf("%s / %s", fmt_int(ov$median_year), fmt_int(ov$year_mode)),
    "Recent (last 5y) \\%"           = fmt_pct(ov$recent_5y_share_pct, 2),
    "CAGR (docs/y)"                  = if (is.null(ov$year_cagr) || is.na(ov$year_cagr)) "—" else paste0(fmt_num(100 * ov$year_cagr, 2), "\\%"),

    "DOI present \\%"                = fmt_pct(cmp$pct_with_doi, 2),
    "DOI valid \\%"                  = fmt_pct(cmp$pct_doi_valid, 2),
    "Abstract present \\%"           = fmt_pct(cmp$pct_with_abstract, 2),
    "Affiliations present \\%"       = fmt_pct(cmp$pct_with_affil, 2),
    "Has keywords (any) \\%"         = fmt_pct(cmp$pct_with_keywords, 2),

    "Citations: mean / sd"           = if (!is.null(cit$basic))
      sprintf("%s / %s", fmt_num(cit$basic$mean, 2), fmt_num(cit$basic$sd, 2)) else "—",
    "Citations: p50 / p90"           = if (!is.null(cit$basic))
      sprintf("%s / %s", fmt_int(cit$basic$quantiles["p50"]), fmt_int(cit$basic$quantiles["p90"])) else "—",
    "Dataset h-index"                = fmt_int(cit$h_index_dataset),
    "Citations Gini"                 = fmt_num(cit$gini, 3),
    "Top 10\\% citation share"       = fmt_pct100(cit$top10_share, 1),

    "Authors per doc (mean/median/p90)" =
      if (!is.null(coll$authors_per_doc))
        sprintf("%s / %s / %s",
                fmt_num(coll$authors_per_doc$mean, 2),
                fmt_int(coll$authors_per_doc$median),
                fmt_int(coll$authors_per_doc$p90)) else "—",
    "Multi-country papers \\%"       = if (!is.null(coll$collab)) fmt_pct(coll$collab$mcp_share_pct, 2) else "—",

    "Venue concentration (HHI)"      = if (!is.null(ven)) fmt_num(ven$hhi, 4) else "—",
    "Venue inequality (Gini)"        = if (!is.null(ven)) fmt_num(ven$gini, 3) else "—"
  )

  df <- data.frame(Metric = names(vals), Value = unname(unlist(vals)), row.names = NULL, check.names = FALSE)

  tex <- paste0(
    "\\begin{table}[!t]\n\\centering\n\\caption{Dataset EDA summary}\\label{tab:eda_summary}\n",
    "\\begin{tabular}{@{}ll@{}}\\toprule\n",
    paste0(apply(df, 1, function(r) sprintf("%s & %s \\\\", r[1], r[2])), collapse = "\n"), "\n",
    "\\bottomrule\\end{tabular}\n",
    "\\vspace{2mm}\\footnotesize{CAGR computed from annual document counts. HHI uses shares by source title; Gini measures inequality.}\n",
    "\\end{table}\n"
  )

  out_path <- file.path(out_dir, file)
  writeLines(tex, out_path, useBytes = TRUE)
  message("[M1] LaTeX EDA table written: ", out_path)
  invisible(out_path)
}

# =============================== PANEL HELPERS ============================== #
# Each returns a ggplot object. Inputs are minimal for easy debugging.

# Tighten plot outer margins
tighten_panel <- function(p, mt = 0, mr = 0, mb = 0, ml = 0) {
  p + ggplot2::theme(plot.margin = ggplot2::margin(mt, mr, mb, ml))
}

# Debug border around a panel
m1i_debug_border <- function(col = "black", size = 0.4) {
  ggplot2::theme(panel.border = ggplot2::element_rect(color = col, fill = NA, linewidth = size),
                 panel.background = ggplot2::element_blank())
}

# Align panel title and keep margins safe
m1i_title_align <- function(p, align = c("left","center"), right_pad = 8, top_pad = 4) {
  align <- match.arg(align)
  h <- if (align == "center") 0.5 else 0
  p + ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = h),
    plot.margin = ggplot2::margin(top_pad, right_pad, 4, 4)
  )
}

# Center/scale a subplot inside its grid cell (great for pies/donuts)
m1i_contain_plot <- function(p, x = 0.5, y = 0.5, width = 0.9, height = 0.9) {
  cowplot::ggdraw() + cowplot::draw_plot(p, x = x, y = y, width = width, height = height,
                                         hjust = 0.5, vjust = 0.5)
}

# --- A1 Docs per year with last-5y shade (fixed bounds; centered) ----------- #
panel_A1_docs <- function(df, overview, theme_ieee) {
  yrs_ok <- suppressWarnings(as.integer(df$Year))
  yrs_ok <- yrs_ok[!is.na(yrs_ok)]

  if (!length(yrs_ok)) {
    p <- ggplot2::ggplot() +
      ggplot2::labs(title = "Docs per Year (n/a)") +
      theme_ieee +
      ggplot2::theme(
        plot.margin = ggplot2::margin(6, 6, 6, 6),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )
    return(tighten_panel(p, 1, 1, 1, 1))
  }

  # span from overview if present
  yr_min <- if (!is.null(overview$year_span)) overview$year_span$min else min(yrs_ok, na.rm = TRUE)
  yr_max <- if (!is.null(overview$year_span)) overview$year_span$max else max(yrs_ok, na.rm = TRUE)

  seq_years <- seq.int(yr_min, yr_max)
  tab <- as.data.frame(table(factor(yrs_ok, levels = seq_years)), stringsAsFactors = FALSE)
  names(tab) <- c("Year", "Count")
  tab$Year  <- as.integer(as.character(tab$Year))
  tab$Count <- as.integer(tab$Count)

  # real y-limits (no -Inf/Inf anywhere)
  y0   <- 0
  ymax <- max(tab$Count, na.rm = TRUE)
  y1   <- if (is.finite(ymax) && ymax > 0) ymax * 1.04 else 1

  # last-5y band, clipped to the x/y window
  last5_start <- yr_max - 4L
  shade_df <- data.frame(
    xmin = max(last5_start - 0.5, yr_min - 0.5),
    xmax = yr_max + 0.5,
    ymin = y0,
    ymax = y1
  )
  last5_txt <- if (!is.null(overview$recent_5y_share_pct)) fmt_pct_plot(overview$recent_5y_share_pct, 2) else "—"

  p <- ggplot2::ggplot(tab, ggplot2::aes(Year, Count)) +
    ggplot2::geom_rect(
      data = shade_df,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = "grey90"
    ) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(size = 0.6, na.rm = TRUE) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 7)) +
    ggplot2::labs(title = sprintf("Docs per Year (last 5y %s)", last5_txt)) +
    theme_ieee +
    ggplot2::theme(
      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 2)),
      plot.margin = ggplot2::margin(6, 6, 6, 6),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    # lock & clip to the true panel window (prevents spill & misalign)
    ggplot2::coord_cartesian(
      xlim = c(yr_min - 0.5, yr_max + 0.5),
      ylim = c(y0, y1),
      expand = FALSE,
      clip = "on"
    )

  tighten_panel(p, 1, 1, 1, 1)
}



# --- A2 Lorenz curve + Gini ------------------------------------------------- #
panel_A2_lorenz <- function(df, citations, theme_ieee) {
  tc <- suppressWarnings(as.numeric(df$Times_Cited)); tc <- tc[is.finite(tc)]
  L  <- if (length(tc) > 1) ineq::Lc(tc) else list(p = c(0,1), L = c(0,1))
  lorenz <- data.frame(p = L$p, L = L$L)
  gini_txt <- fmt_num(if (!is.null(citations)) citations$gini else NA_real_, 3)

  ggplot2::ggplot(lorenz, ggplot2::aes(p, L)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(title = sprintf("Citation Inequality (Gini = %s)", gini_txt)) + theme_ieee
}

# --- A3 Citations per year (mean + 5y running median, n>=20) ---------------- #
panel_A3_citations <- function(df, theme_ieee) {
  tc <- suppressWarnings(as.numeric(df$Times_Cited))
  if (length(tc) == 0 || all(is.na(df$Year))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Citations per Year (n/a)") + theme_ieee)
  }
  cby <- df %>%
    dplyr::mutate(Year = suppressWarnings(as.integer(Year)),
                  Times_Cited = suppressWarnings(as.numeric(Times_Cited))) %>%
    dplyr::filter(!is.na(Year), !is.na(Times_Cited)) %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(n = dplyr::n(), mean_tc = mean(Times_Cited), med_tc = stats::median(Times_Cited), .groups="drop") %>%
    dplyr::filter(n >= 20) %>%
    dplyr::arrange(Year) %>%
    dplyr::mutate(roll_med = stats::runmed(med_tc, k = 5))

  ggplot2::ggplot(cby, ggplot2::aes(Year)) +
    ggplot2::geom_line(ggplot2::aes(y = mean_tc)) +
    ggplot2::geom_line(ggplot2::aes(y = roll_med), linetype = 3) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 7)) +
    ggplot2::labs(title = "Citations per Year (mean, 5y med)") + theme_ieee
}

# --- B1 Top sources (horizontal bars, wrapped labels) ----------------------- #
panel_B1_sources <- function(eda, theme_ieee) {
  lst <- if (!is.null(eda$venues) && !is.null(eda$venues$top10_sources)) eda$venues$top10_sources else list()
  df  <- data.frame(Source = names(lst), N = as.integer(unlist(lst)), check.names = FALSE)
  if (!nrow(df)) df <- data.frame(Source = character(0), N = integer(0))
  df <- df[order(-df$N), , drop = FALSE]
  df$Source <- m1i_wrap(df$Source, 24)

  ggplot2::ggplot(df, ggplot2::aes(reorder(Source, N), N)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()),
                                expand = ggplot2::expansion(mult = c(0, 0.02))) +
    theme_ieee + ggplot2::theme(plot.margin = ggplot2::margin(2, 30, 2, 2)) +
    ggplot2::labs(title = "Top Sources", x = NULL, y = NULL)
}

# --- B2 Top countries (horizontal bars) ------------------------------------ #
panel_B2_countries <- function(eda, theme_ieee) {
  lst <- if (!is.null(eda$toplists) && !is.null(eda$toplists$countries_top10)) eda$toplists$countries_top10 else list()
  df  <- data.frame(Country = names(lst), N = as.integer(unlist(lst)), check.names = FALSE)
  if (!nrow(df)) df <- data.frame(Country = character(0), N = integer(0))
  df <- df[order(-df$N), , drop = FALSE]
  df$Country <- m1i_wrap(df$Country, 18)

  ggplot2::ggplot(df, ggplot2::aes(reorder(Country, N), N)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()),
                                expand = ggplot2::expansion(mult = c(0, 0.02))) +
    theme_ieee + ggplot2::theme(plot.margin = ggplot2::margin(2, 20, 2, 2)) +
    ggplot2::labs(title = "Top Countries (by docs)", x = NULL, y = NULL)
}

# --- B3 Document types (top 8; horizontal bars) ----------------------------- #
panel_B3_doctypes <- function(eda, theme_ieee) {
  lst <- if (!is.null(eda$toplists) && !is.null(eda$toplists$doc_types_top10)) eda$toplists$doc_types_top10 else list()
  df  <- data.frame(Type = names(lst), N = as.integer(unlist(lst)), check.names = FALSE)
  if (!nrow(df)) df <- data.frame(Type = character(0), N = integer(0))
  df <- df[order(-df$N), , drop = FALSE]
  df <- utils::head(df, 8)
  df$Type <- m1i_wrap(df$Type, 24)

  ggplot2::ggplot(df, ggplot2::aes(reorder(Type, N), N)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()),
                                expand = ggplot2::expansion(mult = c(0, 0.02))) +
    theme_ieee + ggplot2::theme(plot.margin = ggplot2::margin(2, 30, 2, 2)) +
    ggplot2::labs(title = "Document Types", x = NULL, y = NULL)
}

# --- C1 Top author keywords (robust; force keywords if possible) ------------ #
panel_C1_keywords_or_languages <- function(eda, theme_ieee, df = NULL, top_n = 10) {
  m1i_wrap <- function(x, w) stringr::str_wrap(x, width = w)

  candidates <- list(
    eda$keywords$top_author_kw,
    eda$toplists$author_kw_top10,
    eda$toplists$author_keywords_top10,
    eda$keywords$author_kw_top10,
    eda$keywords$author_keywords_top10
  )

  find_kw_like <- function(x) {
    tryCatch({
      nm <- names(x)
      if (!is.null(nm) && any(grepl("author.*keyword", nm, ignore.case = TRUE))) return(x)
      NULL
    }, error = function(...) NULL)
  }
  if (all(vapply(candidates, function(z) is.null(z) || !length(z), logical(1)))) {
    more <- c(find_kw_like(eda$keywords), find_kw_like(eda$toplists), find_kw_like(eda))
    candidates <- c(candidates, more)
  }

  lst <- NULL
  for (cnd in candidates) if (!is.null(cnd) && length(cnd)) { lst <- cnd; break }

  if (is.null(lst) && !is.null(df) && "Author_Keywords" %in% names(df)) {
    ak <- df$Author_Keywords
    if (is.list(ak)) ak <- unlist(ak, use.names = FALSE)
    ak <- as.character(ak); ak <- ak[!is.na(ak) & nzchar(ak)]
    if (length(ak)) {
      toks <- unlist(strsplit(ak, ";|,|\\|", perl = TRUE), use.names = FALSE)
      toks <- trimws(toks); toks <- toks[nzchar(toks)]
      if (length(toks)) {
        tab <- sort(table(toks), decreasing = TRUE)
        lst <- as.list(tab)
      }
    }
  }

  if (!is.null(lst) && length(lst)) {
    dfk <- data.frame(KW = names(lst), N = as.integer(unlist(lst)), check.names = FALSE)
    dfk <- dfk[order(-dfk$N), , drop = FALSE]
    if (nrow(dfk) > top_n) dfk <- dfk[seq_len(top_n), , drop = FALSE]
    dfk$KW <- m1i_wrap(dfk$KW, 28)

    return(
      ggplot2::ggplot(dfk, ggplot2::aes(reorder(KW, N), N)) +
        ggplot2::geom_col(width = 0.75) +
        ggplot2::coord_flip(clip = "off") +
        ggplot2::scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()),
                                    expand = ggplot2::expansion(mult = c(0, 0.02))) +
        theme_ieee + ggplot2::theme(plot.margin = ggplot2::margin(2, 30, 2, 2)) +
        ggplot2::labs(title = "Top Author Keywords", x = NULL, y = NULL)
    )
  }

  langs_list <- if (!is.null(eda$toplists) && !is.null(eda$toplists$languages_top10)) eda$toplists$languages_top10 else list()
  dfl <- data.frame(Language = names(langs_list), N = as.integer(unlist(langs_list)), check.names = FALSE)
  dfl <- dfl[order(-dfl$N), , drop = FALSE]
  dfl <- utils::head(dfl, 6)
  dfl$Language <- m1i_wrap(dfl$Language, 18)

  ggplot2::ggplot(dfl, ggplot2::aes(reorder(Language, N), N)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.02))) +
    theme_ieee + ggplot2::theme(plot.margin = ggplot2::margin(2, 20, 2, 2)) +
    ggplot2::labs(title = "Languages", x = NULL, y = NULL)
}

# --- C2 Authors per document (All vs 5y, side-by-side, arrow callouts) ------ #
panel_C2_authors_overlay <- function(df, theme_ieee) {
  count_auth <- function(v) {
    if (is.list(v)) vapply(v, function(x) length(unlist(strsplit(as.character(x), ";", fixed = TRUE))), 1L)
    else vapply(as.character(v), function(s) if (is.na(s) || s == "") 0L else length(strsplit(s, ";", fixed = TRUE)[[1]]), 1L)
  }
  mk_hist <- function(x) {
    if (!length(x) || !any(x > 0)) return(NULL)
    k  <- pmin(x[x > 0], 10L)
    lv <- c(as.character(1:10), "10+")
    fac <- factor(ifelse(x[x > 0] > 10L, "10+", as.character(k)), levels = lv)
    dfh <- as.data.frame(table(fac))
    names(dfh) <- c("k","n")
    dfh$prop <- dfh$n / sum(dfh$n)
    dfh
  }
  med_bin <- function(x) {
    if (!length(x) || !any(x > 0)) return(list(ix = NA_real_, md = NA_real_))
    md <- stats::median(x[x > 0])
    ix <- match(as.character(round(md)), c(as.character(1:10), "10+"))
    list(ix = if (is.na(ix)) NA_real_ else ix, md = md)
  }

  if (!("Authors" %in% names(df))) return(ggplot2::ggplot() + ggplot2::labs(title = "Authors per Document (n/a)") + theme_ieee)

  year_num   <- suppressWarnings(as.integer(df$Year))
  y_max      <- suppressWarnings(max(year_num, na.rm = TRUE))
  last5_mask <- !is.na(year_num) & (year_num >= (y_max - 4L))

  apd_all <- count_auth(df$Authors)
  apd_5y  <- count_auth(df$Authors[last5_mask])

  h_all <- mk_hist(apd_all)
  h_5y  <- mk_hist(apd_5y)
  if (is.null(h_all)) return(ggplot2::ggplot() + ggplot2::labs(title = "Authors per Document (n/a)") + theme_ieee)

  lv        <- levels(h_all$k)
  n_lv      <- length(lv)
  idx_map   <- setNames(seq_along(lv), lv)

  if (!is.null(h_5y)) {
    h_5y <- merge(data.frame(k = lv), h_5y, by = "k", all.x = TRUE)
    h_5y$n[is.na(h_5y$n)] <- 0
    h_5y$prop[is.na(h_5y$prop)] <- 0
  }

  off_all <- -0.18
  off_5y  <-  0.18
  bar_w   <-  0.34

  d_all <- if (!is.null(h_all)) transform(h_all, Period = "All years", xi = idx_map[as.character(k)] + off_all)
  d_5y  <- if (!is.null(h_5y))  transform(h_5y,  Period = "Last 5y",  xi = idx_map[as.character(k)] + off_5y)
  d_plot <- rbind(if (!is.null(d_all)) d_all[, c("k","prop","Period","xi")] else NULL,
                  if (!is.null(d_5y))  d_5y[,  c("k","prop","Period","xi")]  else NULL)

  m_all <- med_bin(apd_all)
  m_5y  <- med_bin(apd_5y)

  y_all <- if (is.finite(m_all$ix)) d_all$prop[d_all$k == lv[m_all$ix]] else NA_real_
  y_5y  <- if (!is.null(d_5y) && is.finite(m_5y$ix)) d_5y$prop[d_5y$k == lv[m_5y$ix]] else NA_real_

  ymax_prop <- max(d_plot$prop, na.rm = TRUE)

  lbl <- data.frame(
    x = c(n_lv - 0.35, n_lv - 0.35),
    y = c(ymax_prop * 0.95, ymax_prop * 0.80),
    txt = c(if (is.finite(m_all$md)) sprintf("All years (median %.0f)", m_all$md) else "All years (n/a)",
            if (is.finite(m_5y$md))  sprintf("Last 5y (median %.0f)",  m_5y$md)  else "Last 5y (n/a)")
  )

  seg <- rbind(
    if (is.finite(m_all$ix)) data.frame(x = n_lv - 0.55, y = ymax_prop * 0.92,
                                        xend = m_all$ix + off_all, yend = if (is.finite(y_all)) y_all else 0) else NULL,
    if (is.finite(m_5y$ix))  data.frame(x = n_lv - 0.55, y = ymax_prop * 0.77,
                                        xend = m_5y$ix + off_5y,  yend = if (is.finite(y_5y))  y_5y  else 0) else NULL
  )

  ggplot2::ggplot(d_plot, ggplot2::aes(x = xi, y = prop)) +
    ggplot2::geom_col(data = subset(d_plot, Period == "All years"), width = bar_w, fill = "grey30") +
    ggplot2::geom_col(data = subset(d_plot, Period == "Last 5y"),  width = bar_w, fill = "grey70") +
    ggplot2::geom_segment(data = seg, ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                          inherit.aes = FALSE, linewidth = 0.4,
                          arrow = grid::arrow(length = grid::unit(2, "mm"))) +
    ggplot2::geom_label(data = lbl, ggplot2::aes(x = x, y = y, label = txt),
                        inherit.aes = FALSE, size = 3, label.size = 0) +
    ggplot2::scale_x_continuous(breaks = seq_len(n_lv), labels = lv,
                                expand = ggplot2::expansion(mult = c(0.01, 0.02))) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                limits = c(0, ymax_prop * 1.05)) +
    ggplot2::labs(title = "Authors per Document",
                  subtitle = "All vs 5y (side-by-side); arrows mark medians") +
    theme_ieee +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(2, 8, 2, 2))
}

# --- C3 Collaboration nested donut (All vs 5y) ------------------------------ #
panel_C3_collab <- function(df, eda, theme_ieee) {
  parse_country_vec <- function(x) {
    if (is.null(x)) return(character(0))
    if (is.list(x)) x <- unlist(x, use.names = FALSE)
    if (!length(x)) return(character(0))
    x <- as.character(x)
    x <- unlist(strsplit(x, "[;|,]", perl = TRUE), use.names = FALSE)
    x <- trimws(x); x[nzchar(x)]
  }
  mcp_from_mask <- function(mask = NULL) {
    if (!("Country_List" %in% names(df))) return(NA_real_)
    idx <- if (is.null(mask)) rep(TRUE, nrow(df)) else as.logical(mask)
    if (!any(idx, na.rm = TRUE)) return(NA_real_)
    cl <- df$Country_List[idx]
    multi <- vapply(seq_along(cl), function(i) length(parse_country_vec(cl[[i]])) > 1, logical(1))
    if (!length(multi)) return(NA_real_)
    round(mean(multi, na.rm = TRUE) * 100, 1)
  }
  pct_txt <- function(x, d = 1) if (is.finite(x)) sprintf(paste0("%.", d, "f%%"), x) else "n/a"

  year_num <- suppressWarnings(as.integer(df$Year))
  y_max <- suppressWarnings(max(year_num, na.rm = TRUE))
  last5_mask <- !is.na(year_num) & (year_num >= (y_max - 4L))

  mcp_all <- mcp_from_mask(NULL)
  if (!is.finite(mcp_all)) {
    if (!is.null(eda$authorship_collab$collab$mcp_share_pct))
      mcp_all <- as.numeric(eda$authorship_collab$collab$mcp_share_pct)
  }
  mcp_5y  <- mcp_from_mask(last5_mask)

  if (!is.finite(mcp_all)) return(ggplot2::ggplot() + ggplot2::labs(title = "Collaboration (n/a)") + theme_ieee)

  outer <- data.frame(Ring = "All", Class = c("Single-country","Multi-country"),
                      Share = c(100 - mcp_all, mcp_all))
  inner <- if (is.finite(mcp_5y)) data.frame(Ring = "5y", Class = c("Single-country","Multi-country"),
                                             Share = c(100 - mcp_5y, mcp_5y)) else NULL

  label_pos <- function(d) {
    d <- d[order(d$Class), ]
    d$ymax <- cumsum(d$Share); d$ymin <- d$ymax - d$Share; d$ymid <- (d$ymin + d$ymax) / 2
    d
  }
  oL <- subset(label_pos(outer), Class == "Multi-country")
  iL <- if (!is.null(inner)) subset(label_pos(inner), Class == "Multi-country") else NULL

  oL$x0 <- 2.00; oL$x1 <- 2.35; oL$xl <- 2.55; oL$lab <- paste0("All: ", pct_txt(mcp_all, 1))
  if (!is.null(iL)) { iL$x0 <- 1.40; iL$x1 <- 1.58; iL$xl <- 1.78; iL$lab <- paste0("5y: ", pct_txt(mcp_5y, 1)) }

  ggplot2::ggplot() +
    ggplot2::geom_col(data = outer, ggplot2::aes(x = 2.0, y = Share, fill = Class),
                      width = 0.80, colour = "grey30") +
    { if (!is.null(inner)) ggplot2::geom_col(data = inner, ggplot2::aes(x = 1.4, y = Share, fill = Class),
                                             width = 0.80, colour = "grey20") } +
    ggplot2::geom_segment(data = oL, ggplot2::aes(x = x0, xend = x1, y = ymid, yend = ymid),
                          inherit.aes = FALSE, linewidth = 0.4) +
    ggplot2::geom_label(data = oL, ggplot2::aes(x = xl, y = ymid, label = lab),
                        inherit.aes = FALSE, size = 3, label.size = 0) +
    { if (!is.null(iL)) ggplot2::geom_segment(data = iL, ggplot2::aes(x = x0, xend = x1, y = ymid, yend = ymid),
                                              inherit.aes = FALSE, linewidth = 0.4) } +
    { if (!is.null(iL)) ggplot2::geom_label(data = iL, ggplot2::aes(x = xl, y = ymid, label = lab),
                                            inherit.aes = FALSE, size = 3, label.size = 0) } +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::scale_x_continuous(limits = c(0.5, 2.6), breaks = NULL, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
    ggplot2::guides(fill = "none") +
    theme_ieee +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(title = "Collaboration")
}

# =============================== ASSEMBLER ================================== #
m1i_plot_eda_3x3 <- function(df, eda,
                             out_dir  = "results2/M1",
                             file_png = "EDA_3x3.png",
                             file_pdf = "EDA_3x3.pdf",
                             debug_frames = FALSE,   # panel borders
                             draw_grid    = TRUE,    # global 3×3 grid lines
                             title_align  = c("left","center")) {
  m1i_ensure_dir(out_dir)
  suppressPackageStartupMessages({
    library(cowplot); library(ineq); library(stringr); library(dplyr); library(scales)
  })

  title_align <- match.arg(title_align)
  stopifnot(nrow(df) > 0, !is.null(eda$overview))

  theme_ieee <- m1i_theme_ieee(8)

  # Build panels
  pA1 <- panel_A1_docs(df, eda$overview, theme_ieee) + ggplot2::theme(plot.margin = ggplot2::margin(0,0,0,0))
  pA2 <- panel_A2_lorenz(df, eda$citations, theme_ieee)
  pA3 <- panel_A3_citations(df, theme_ieee)

  pB1 <- panel_B1_sources(eda, theme_ieee)
  pB2 <- panel_B2_countries(eda, theme_ieee)
  pB3 <- panel_B3_doctypes(eda, theme_ieee)

  pC1 <- panel_C1_keywords_or_languages(eda, theme_ieee, df = df)
  pC2 <- panel_C2_authors_overlay(df, theme_ieee)
  pC3 <- panel_C3_collab(df, eda, theme_ieee)

  # Center small-format plots (donut) within their grid cell
  pC3 <- m1i_contain_plot(pC3, width = 0.86, height = 0.86)

  # Uniform title alignment + safe margins; optional panel borders
  dbg <- if (debug_frames) theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.4)) else theme()
  norm_title <- function(p) m1i_title_align(p, align = title_align, right_pad = 8, top_pad = 4)
  norm <- function(p) norm_title(p) + theme(plot.margin = margin(0,0,0,0)) + dbg

  plots <- lapply(list(pA1,pA2,pA3,pB1,pB2,pB3,pC1,pC2,pC3), norm)

  # Align and compose fixed 3×3 grid (equal cells)
  aligned <- cowplot::align_plots(plotlist = plots, align = "hv", axis = "tblr")
  grid_3x3 <- cowplot::plot_grid(plotlist = aligned, ncol = 3, nrow = 3,
                                 rel_widths = c(1,1,1), rel_heights = c(1,1,1))

  # Optional: draw global grid lines for visual debugging
  if (draw_grid) {
    grid_3x3 <- cowplot::ggdraw() +
      cowplot::draw_plot(grid_3x3) +
      cowplot::draw_line(x = c(1/3, 1/3), y = c(0, 1), colour = "black", linewidth = 0.4) +
      cowplot::draw_line(x = c(2/3, 2/3), y = c(0, 1), colour = "black", linewidth = 0.4) +
      cowplot::draw_line(x = c(0, 1), y = c(1/3, 1/3), colour = "black", linewidth = 0.4) +
      cowplot::draw_line(x = c(0, 1), y = c(2/3, 2/3), colour = "black", linewidth = 0.4)
  }

  # Export (IEEE two-column width)
  png_w <- 7.16; png_h <- 5.6
  ggsave(file.path(out_dir, file_png), grid_3x3, width = png_w, height = png_h, dpi = 300)
  pdf_dev <- function(filename, width, height, ...) {
    grDevices::pdf(file = filename, width = width, height = height, useDingbats = FALSE, ...)
  }
  ggsave(file.path(out_dir, file_pdf), plot = grid_3x3, width = 7.16, height = 5.6, device = pdf_dev)
}
