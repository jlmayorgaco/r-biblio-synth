# ============================================================================ #
# src/plots/quad_bubble_plot.r
# quad_bubble_plot — TP–TC and SCP–MCP Quadrant Bubble Plots (function-based)
# Compatible with:
#   - Raw document-level data (bibliometrix or tidy)
#   - Country-year aggregates produced by M3_Countries$prepare_data()
# Returns: list( plots = list(tp_tc, scp_mcp), metrics = list(...), data = list(...) )
# IEEE Transactions–ready: grayscale-safe trends, lighter fills, readable metrics
# ============================================================================ #
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(grid)
  library(ggrepel)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---------- Public API ----------------------------------------------------- #

# df can be:
#  A) Raw docs with columns like "Publication Year"/"Times Cited"/Countries/String or Array
#  B) Aggregated by country-year with columns: Country, Year, TP, TC, MCP, SCP
quad_bubble_plot <- function(
  df,
  # Detection hints (optional; auto-coalesced if absent)
  year_col      = NULL,
  country_col   = NULL,
  citations_col = NULL,
  countries_arr_col = NULL,

  # Sliding window + selection
  N_years      = 5,
  top_n        = 10,

  # Visuals
  layout             = c("onecol","twocol"),
  show_arrows        = TRUE,
  zero_offset        = 0.01,
  quadrant_alpha     = 0.08,
  point_size         = 2,
  arrow_vs_dot_mult  = 2,
  add_metrics        = TRUE
) {
  layout <- match.arg(layout)

  parts <- .extract_country_halves(
    df, year_col, country_col, citations_col, countries_arr_col, N_years
  )

  # *_first_half / *_second_half + deltas for citations/papers
  country_data <- .build_country_delta_table(parts$first, parts$second)

  # ---- TP–TC variant (totals over halves) ---------------------------------
  tp_tc_in <- country_data %>%
    mutate(
      Total_Citations   = (Total_Citations_first_half   %||% 0) + (Total_Citations_second_half   %||% 0),
      Total_Paper_Count = (Total_Paper_Count_first_half %||% 0) + (Total_Paper_Count_second_half %||% 0)
    )

  tp_tc_top <- .filter_top_by_two(tp_tc_in, "Total_Citations", "Total_Paper_Count", top_n)
  tp_tc_res <- .plot_quadrants_generic(
    df_total      = tp_tc_top,
    x_col_total   = "Total_Citations",
    y_col_total   = "Total_Paper_Count",
    dx_col        = "delta_x",
    dy_col        = "delta_y",
    label_col     = "Countries_Array_first_half",
    layout        = layout,
    show_arrows   = show_arrows,
    zero_offset   = zero_offset,
    quadrant_alpha= quadrant_alpha,
    point_size    = point_size,
    arrow_vs_dot_mult = arrow_vs_dot_mult,
    add_metrics   = add_metrics,
    fill_labels   = c(
      "I: High Citations, High Papers",
      "II: High Citations, Low Papers",
      "III: Low Citations, High Papers",
      "IV: Low Citations, Low Papers"
    ),
    axis_lab_x = sprintf("Total Citations (log\u2081\u2080, \u03B5=%.2f)", zero_offset),
    axis_lab_y = sprintf("Total Papers (log\u2081\u2080, \u03B5=%.2f)",    zero_offset),
    # TP–TC: solid, thicker arrows
    linetype_by_trend = FALSE,
    arrow_linewidth   = 0.70
  )

  # ---- SCP–MCP variant -----------------------------------------------------
  scp_mcp_in <- country_data %>%
    mutate(
      Total_SCP = (SCP_Count_first_half %||% 0) + (SCP_Count_second_half %||% 0),
      Total_MCP = (MCP_Count_first_half %||% 0) + (MCP_Count_second_half %||% 0),
      delta_x   = (SCP_Count_second_half %||% 0) - (SCP_Count_first_half %||% 0),
      delta_y   = (MCP_Count_second_half %||% 0) - (MCP_Count_first_half %||% 0)
    )

  scp_mcp_top <- .filter_top_by_two(scp_mcp_in, "Total_SCP", "Total_MCP", top_n)
  scp_mcp_res <- .plot_quadrants_generic(
    df_total      = scp_mcp_top,
    x_col_total   = "Total_SCP",
    y_col_total   = "Total_MCP",
    dx_col        = "delta_x",
    dy_col        = "delta_y",
    label_col     = "Countries_Array_first_half",
    layout        = layout,
    show_arrows   = show_arrows,
    zero_offset   = zero_offset,
    quadrant_alpha= quadrant_alpha,
    point_size    = point_size,
    arrow_vs_dot_mult = arrow_vs_dot_mult,
    add_metrics   = add_metrics,
    fill_labels   = c(
      "I: High SCP, High MCP",
      "II: High SCP, Low MCP",
      "III: Low SCP, High MCP",
      "IV: Low SCP, Low MCP"
    ),
    axis_lab_x = sprintf("SCP (log\u2081\u2080, \u03B5=%.2f)", zero_offset),
    axis_lab_y = sprintf("MCP (log\u2081\u2080, \u03B5=%.2f)", zero_offset),
    # SCP–MCP: keep linetype mapping (grayscale-safe), default thickness
    linetype_by_trend = TRUE,
    arrow_linewidth   = 0.45
  )

  list(
    plots = list(
      tp_tc   = tp_tc_res$plot,
      scp_mcp = scp_mcp_res$plot
    ),
    metrics = list(
      tp_tc   = tp_tc_res$metrics,
      scp_mcp = scp_mcp_res$metrics
    ),
    data = list(
      halves       = parts$half_ranges,
      country_data = country_data,
      tp_tc_top    = tp_tc_top,
      scp_mcp_top  = scp_mcp_top
    )
  )
}

# ---------- Helpers: data extraction / normalization ----------------------- #

.extract_country_halves <- function(df, year_col, country_col, citations_col, countries_arr_col, N) {
  df <- as.data.frame(df)

  # A) Aggregated by country-year?
  agg_is_present <- all(c("TP","TC","MCP","SCP") %in% names(df)) &&
                    any(tolower(names(df)) %in% tolower(c("Country","Countries","Countries_Array","country"))) &&
                    any(tolower(names(df)) %in% tolower(c("Year","Publication Year","PY","year")))

  if (agg_is_present) {
    # Normalize names
    yr  <- year_col %||% (intersect(names(df), c("Year","Publication Year","PY","year"))[1])
    cty <- country_col %||% (intersect(names(df), c("Country","Countries","Countries_Array","country"))[1])
    stopifnot(!is.null(yr), !is.null(cty))

    # If Countries_Array missing, derive from Country string
    if (!("Countries_Array" %in% names(df))) {
      if (identical(cty, "Countries_Array")) {
        df$Countries_Array <- df[[cty]]
      } else {
        df$Countries_Array <- lapply(as.character(df[[cty]]), function(x) trimws(unlist(strsplit(x %||% "", ";"))))
      }
    }

    df$`Publication Year` <- suppressWarnings(as.integer(df[[yr]]))
    df$`Times Cited`      <- suppressWarnings(as.numeric(df$TC %||% 0))

    # Build a synthetic doc-like frame to reuse the same split/summarize logic
    synth <- df %>%
      transmute(
        `Publication Year` = `Publication Year`,
        Countries_Array    = Countries_Array,
        `Times Cited`      = as.numeric(TC %||% 0),
        TP                 = as.numeric(TP %||% 0),   # carry TP (fixes flat y-axis)
        SCP = as.integer(SCP %||% 0),
        MCP = as.integer(MCP %||% 0)
      )

    split <- .split_halves(synth, N)
    list(
      first       = .summarize_countries(split$first),
      second      = .summarize_countries(split$second),
      half_ranges = split$ranges
    )

  } else {
    # B) Raw docs (bibliometrix or tidy)
    yr  <- year_col %||% (intersect(names(df), c("Publication Year","PY","Year","year","PublicationYear","pub_year"))[1])
    tc  <- citations_col %||% (intersect(names(df), c("Times Cited","TC","Citations","times_cited","TimesCited"))[1])

    if (is.null(yr)) stop("[quad_bubble_plot] Could not detect year column.")
    if (is.null(tc)) tc <- NA_character_

    # Countries handling: try array first, then string columns (several common names)
    if (is.null(countries_arr_col) || !(countries_arr_col %in% names(df))) {
      if (!("Countries_Array" %in% names(df))) {
        cand_str <- intersect(
          names(df),
          c("Countries","Country","Countries_String","Country_String","AU_CO","AU_UN","C1","country")
        )
        if (length(cand_str) > 0) {
          df$Countries <- as.character(df[[cand_str[1]]])
          df$Countries_Array <- strsplit(df$Countries %||% "", ";")
        }
      }
    } else {
      df$Countries_Array <- df[[countries_arr_col]]
    }

    if (!("Countries_Array" %in% names(df))) {
      stop("[quad_bubble_plot] No countries column found (need Countries or Countries_Array).")
    }

    df$Countries_Array <- lapply(df$Countries_Array, function(x) {
      x <- x %||% character(0)
      x <- x[!is.na(x) & nzchar(trimws(x))]
      trimws(as.character(x))
    })

    df$`Publication Year` <- suppressWarnings(as.integer(df[[yr]]))
    if (is.na(tc)) {
      df$`Times Cited` <- 0
    } else {
      df$`Times Cited` <- suppressWarnings(as.numeric(df[[tc]]))
    }

    # Enrich with SCP/MCP flags at doc level
    df <- df %>%
      mutate(
        SCP = if_else(lengths(Countries_Array) == 1, 1L, 0L),
        MCP = if_else(lengths(Countries_Array) > 1,  1L, 0L)
      )

    split <- .split_halves(df %>% select(`Publication Year`, Countries_Array, `Times Cited`, SCP, MCP), N)
    list(
      first       = .summarize_countries(split$first),
      second      = .summarize_countries(split$second),
      half_ranges = split$ranges
    )
  }
}

.split_halves <- function(df, N) {
  last_year <- suppressWarnings(max(df$`Publication Year`, na.rm = TRUE))
  if (!is.finite(last_year)) stop("[quad_bubble_plot] Invalid years in data.")

  start_first  <- last_year - (2 * N)
  end_first    <- last_year - N
  start_second <- end_first + 1
  end_second   <- last_year

  list(
    first  = dplyr::filter(df, `Publication Year` >= start_first  & `Publication Year` <= end_first),
    second = dplyr::filter(df, `Publication Year` >= start_second & `Publication Year` <= end_second),
    ranges = data.frame(
      First_Half_Range  = paste(start_first,  end_first,  sep = "-"),
      Second_Half_Range = paste(start_second, end_second, sep = "-"),
      stringsAsFactors = FALSE
    )
  )
}

.summarize_countries <- function(df_half) {
  has_tp <- "TP" %in% names(df_half)

  df_half %>%
    tidyr::unnest(cols = c(Countries_Array)) %>%
    group_by(Countries_Array) %>%
    summarise(
      Total_Paper_Count = if (has_tp) sum(TP, na.rm = TRUE) else n(),
      Total_Citations   = sum(`Times Cited`, na.rm = TRUE),
      SCP_Count         = sum(SCP %||% 0, na.rm = TRUE),
      MCP_Count         = sum(MCP %||% 0, na.rm = TRUE),
      .groups = "drop"
    )
}

.build_country_delta_table <- function(first, second) {
  full_join(
    first  %>% rename_with(~ paste0(., "_first_half")),
    second %>% rename_with(~ paste0(., "_second_half")),
    by = c("Countries_Array_first_half" = "Countries_Array_second_half")
  ) %>%
    mutate(
      across(where(is.numeric), ~ tidyr::replace_na(., 0)),
      across(where(is.character), ~ tidyr::replace_na(., "")),
      delta_x = Total_Citations_second_half   - Total_Citations_first_half,
      delta_y = Total_Paper_Count_second_half - Total_Paper_Count_first_half
    )
}

.filter_top_by_two <- function(df, a, b, n) {
  top_a <- df %>% arrange(desc(.data[[a]])) %>% slice_head(n = n)
  top_b <- df %>% arrange(desc(.data[[b]])) %>% slice_head(n = n)
  bind_rows(top_a, top_b) %>% distinct(Countries_Array_first_half, .keep_all = TRUE)
}

# ---------- Helpers: plotting core (generic) ------------------------------- #

.plot_quadrants_generic <- function(
  df_total,
  x_col_total, y_col_total,
  dx_col, dy_col,
  label_col,
  layout, show_arrows, zero_offset, quadrant_alpha,
  point_size, arrow_vs_dot_mult, add_metrics,
  fill_labels,
  axis_lab_x, axis_lab_y,
  # NEW: arrow styling controls
  linetype_by_trend = TRUE,
  arrow_linewidth   = 0.55
) {

  # Clamp non-negatives (avoid tidy-eval headaches)
  df_total[[x_col_total]] <- pmax(df_total[[x_col_total]], 0)
  df_total[[y_col_total]] <- pmax(df_total[[y_col_total]], 0)

  # Trend categories
  df_total <- df_total %>%
    mutate(
      trend_category = case_when(
        .data[[dx_col]] > 0 & .data[[dy_col]] > 0 ~ "Positive",
        .data[[dx_col]] < 0 & .data[[dy_col]] < 0 ~ "Negative",
        TRUE ~ "Mixed"
      )
    )

  # Subset medians (linear)
  med_x_lin <- median(df_total[[x_col_total]], na.rm = TRUE)
  med_y_lin <- median(df_total[[y_col_total]], na.rm = TRUE)

  # Region rects (linear → log) – align with Roman numerals (TR=I, BR=II, TL=III, BL=IV)
  region_log <- .make_region_rects(df_total[[x_col_total]], df_total[[y_col_total]], fill_labels) %>%
    mutate(
      xmin = log10(pmax(xmin + zero_offset, 1e-9)),
      xmax = log10(pmax(xmax + zero_offset, 1e-9)),
      ymin = log10(pmax(ymin + zero_offset, 1e-9)),
      ymax = log10(pmax(ymax + zero_offset, 1e-9))
    )

  # Arrow scaling baseline
  max_dx <- max(abs(df_total[[dx_col]]), na.rm = TRUE); if (!is.finite(max_dx) || max_dx == 0) max_dx <- 1e-9
  max_dy <- max(abs(df_total[[dy_col]]), na.rm = TRUE); if (!is.finite(max_dy) || max_dy == 0) max_dy <- 1e-9

  ref_x <- 0.10 * max(df_total[[x_col_total]] + zero_offset, na.rm = TRUE)
  ref_y <- 0.10 * max(df_total[[y_col_total]] + zero_offset, na.rm = TRUE)
  ref_di <- sqrt(ref_x^2 + ref_y^2)
  Lmin <- 0.01 * ref_di
  Lmax <- 0.10 * ref_di

  filt0 <- df_total %>%
    mutate(
      dx0  = (.data[[dx_col]] / max_dx) * ref_x,
      dy0  = (.data[[dy_col]] / max_dy) * ref_y,
      len0 = sqrt(dx0^2 + dy0^2),
      target_len = case_when(
        len0 == 0 ~ 0,
        len0 <  Lmin ~ Lmin,
        len0 >  Lmax ~ Lmax,
        TRUE ~ len0
      ),
      ux = if_else(len0 > 0, dx0 / len0, 0),
      uy = if_else(len0 > 0, dy0 / len0, 0),
      dx = ux * target_len,
      dy = uy * target_len,

      x_lin    = .data[[x_col_total]] + zero_offset,
      y_lin    = .data[[y_col_total]] + zero_offset,
      xend_lin = pmax(.data[[x_col_total]] + dx + zero_offset, 1e-9),
      yend_lin = pmax(.data[[y_col_total]] + dy + zero_offset, 1e-9),

      x    = log10(x_lin),
      y    = log10(y_lin),
      xend = log10(xend_lin),
      yend = log10(yend_lin)
    )

  # Ensure min visual arrow length in log space
  log_len_min <- (arrow_vs_dot_mult * (point_size / 2)) * 0.04
  filt <- filt0 %>%
    mutate(
      log_len = sqrt((xend - x)^2 + (yend - y)^2),
      scale_up = if_else(log_len > 0 & log_len < log_len_min, log_len_min / log_len, 1),
      xend = x + (xend - x) * scale_up,
      yend = y + (yend - y) * scale_up
    )

  # Crosshair medians (subset medians; linear → log)
  log_med_x <- log10(pmax(median(10^filt$x, na.rm = TRUE), 1))
  log_med_y <- log10(pmax(median(10^filt$y, na.rm = TRUE), 1))

  # Axis breaks
  min_x <- min(region_log$xmin, filt$x, na.rm = TRUE)
  max_x <- max(region_log$xmax, filt$x, na.rm = TRUE)
  min_y <- min(region_log$ymin, filt$y, na.rm = TRUE)
  max_y <- max(region_log$ymax, filt$y, na.rm = TRUE)
  br_x <- .log_breaks(min_x, max_x)
  br_y <- .log_breaks(min_y, max_y)
  p10_lab <- function(v) scales::math_format(10^.x)(v)

  base_size <- if (layout == "onecol") 9 else 10
  lab_size  <- if (layout == "onecol") 3.0 else 3.3

  # ---- Metrics (Coherence, LFI, Net→QI, movers) ---------------------------
  metrics <- list()
  metrics_layers <- list()
  if (isTRUE(add_metrics)) {
    mag <- sqrt((filt0$dx)^2 + (filt0$dy)^2)
    ux  <- ifelse(mag > 0, filt0$dx / mag, 0)
    uy  <- ifelse(mag > 0, filt0$dy / mag, 0)
    mean_u    <- c(mean(ux), mean(uy))
    coherence <- sqrt(sum(mean_u^2))

    # Leader = max(x+y) with nonzero delta
    ranking <- df_total %>% mutate(score_lead = .data[[x_col_total]] + .data[[y_col_total]]) %>% arrange(desc(score_lead))
    LFI <- NA_real_; leader <- NA_character_
    if (nrow(ranking) > 0) {
      for (k in seq_len(nrow(ranking))) {
        cand <- ranking[k, , drop = FALSE]
        vLx <- cand[[dx_col]]; vLy <- cand[[dy_col]]
        Llen <- sqrt(vLx^2 + vLy^2)
        if (is.finite(Llen) && Llen > 0) {
          uL <- c(vLx, vLy) / Llen
          leader <- cand[[label_col]]
          cosines <- c(); weights <- c()
          for (i in seq_len(nrow(df_total))) {
            if (identical(df_total[[label_col]][i], leader)) next
            vi <- c(df_total[[dx_col]][i], df_total[[dy_col]][i])
            li <- sqrt(sum(vi^2))
            if (is.finite(li) && li > 0) {
              ui <- vi / li
              cosines <- c(cosines, sum(ui * uL))
              weights <- c(weights, li)
            }
          }
          if (length(cosines) > 0) LFI <- sum(weights * cosines) / sum(weights)
          break
        }
      }
    }

    # Net flow into Quadrant I (subset medians in linear)
    qof <- function(x, y, mx, my) {
      if (x >= mx && y >= my) return("I")
      if (x >= mx && y <  my) return("II")
      if (x <  mx && y >= my) return("III")
      "IV"
    }
    start_q <- mapply(qof, df_total[[x_col_total]], df_total[[y_col_total]],
                      MoreArgs = list(mx = med_x_lin, my = med_y_lin))
    end_q   <- mapply(qof,
                      df_total[[x_col_total]] + df_total[[dx_col]],
                      df_total[[y_col_total]] + df_total[[dy_col]],
                      MoreArgs = list(mx = med_x_lin, my = med_y_lin))
    entrants_I <- sum(start_q != "I" & end_q == "I")
    leavers_I  <- sum(start_q == "I" & end_q != "I")
    net_I      <- entrants_I - leavers_I
    movers_tot <- sum(start_q != end_q)

    # Global mean arrow at (subset) medians (linear → log)
    cx <- median(filt0$x_lin, na.rm = TRUE)
    cy <- median(filt0$y_lin, na.rm = TRUE)
    mean_mag  <- median(mag, na.rm = TRUE)
    g_len     <- 0.12 * mean_mag
    gx1 <- log10(pmax(cx, 1e-9)); gy1 <- log10(pmax(cy, 1e-9))
    gx2 <- log10(pmax(cx + mean_u[1]*g_len, 1e-9))
    gy2 <- log10(pmax(cy + mean_u[2]*g_len, 1e-9))

    padx <- (max_x - min_x) * 0.05
    pady <- (max_y - min_y) * 0.05
    lab_x <- min_x + padx
    lab_y <- max_y - pady

    metrics_layers <- list(
      geom_segment(
        aes(x = gx1, y = gy1, xend = gx2, yend = gy2),
        data = data.frame(gx1 = gx1, gy1 = gy1, gx2 = gx2, gy2 = gy2),
        linewidth = 0.7, color = "black", alpha = 0.9,
        arrow = grid::arrow(length = grid::unit(0.22, "cm"))
      ),
      geom_label(
        data = data.frame(
          lab_x = lab_x,
          lab_y = lab_y,
          lbl   = sprintf(
            "Coherence = %.2f\nLFI (%s) = %s\nNet\u2192Q\u2160 = %+d (movers: %d)",
            coherence,
            ifelse(is.na(leader), "—", leader),
            ifelse(is.na(LFI), "—", sprintf("%.2f", LFI)),
            net_I, movers_tot
          )
        ),
        aes(x = lab_x, y = lab_y, label = lbl),
        hjust = 0, vjust = 1,
        size = 3.2,
        label.size = 0,
        fill = "white",
        alpha = 0.85,
        inherit.aes = FALSE
      )
    )

    metrics <- list(
      coherence = coherence,
      LFI = LFI,
      leader = leader,
      net_to_Q1 = net_I,
      entrants_to_Q1 = entrants_I,
      leavers_from_Q1 = leavers_I,
      movers_total = movers_tot
    )
  }

  # Zero-axis hint arrows (lower-left)
  axis_arrows <- {
    padx <- (max_x - min_x) * 0.03
    pady <- (max_y - min_y) * 0.03
    list(
      geom_segment(
        aes(x = min_x + padx, y = min_y + pady, xend = min_x + 6*padx, yend = min_y + pady),
        data = data.frame(), inherit.aes = FALSE,
        arrow = grid::arrow(length = grid::unit(0.18, "cm")),
        linewidth = 0.4, color = "grey35"
      ),
      geom_segment(
        aes(x = min_x + padx, y = min_y + pady, xend = min_x + padx, yend = min_y + 6*pady),
        data = data.frame(), inherit.aes = FALSE,
        arrow = grid::arrow(length = grid::unit(0.18, "cm")),
        linewidth = 0.4, color = "grey35"
      )
    )
  }

  # Roman numerals (TR=I, BR=II, TL=III, BL=IV)
  roman_layers <- {
    padx <- (max_x - min_x) * 0.05
    pady <- (max_y - min_y) * 0.05
    labs_df <- data.frame(
      label = c("III","I","IV","II"),
      x = c(min_x + padx, max_x - padx, min_x + padx, max_x - padx),
      y = c(max_y - pady, max_y - pady, min_y + pady, min_y + pady)
    )
    geom_text(
      data = labs_df,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      color = "grey30", fontface = "bold",
      size = if (layout == "onecol") 3.8 else 4.2
    )
  }

  # Arrow layer: conditional linetype mapping
  arrow_layer <-
    if (linetype_by_trend) {
      list(
        geom_segment(
          data = filt %>% filter(is.finite(x) & is.finite(y) & is.finite(xend) & is.finite(yend)),
          aes(x = x, y = y, xend = xend, yend = yend,
              color = trend_category, linetype = trend_category),
          arrow = grid::arrow(length = grid::unit(0.18, "cm")),
          linewidth = arrow_linewidth, show.legend = TRUE
        ),
        scale_linetype_manual(
          name = "Trend",
          values = c(Positive = "solid", Mixed = "solid", Negative = "solid"),
          breaks = c("Positive","Mixed","Negative")
        )
      )
    } else {
      list(
        geom_segment(
          data = filt %>% filter(is.finite(x) & is.finite(y) & is.finite(xend) & is.finite(yend)),
          aes(x = x, y = y, xend = xend, yend = yend, color = trend_category),
          linetype = "solid",
          arrow = grid::arrow(length = grid::unit(0.22, "cm")),
          linewidth = arrow_linewidth, show.legend = TRUE
        )
      )
    }

  # ---- Build plot ----------------------------------------------------------
  p <- ggplot() +
    geom_rect(
      data = region_log,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = region),
      alpha = quadrant_alpha, inherit.aes = FALSE, show.legend = TRUE
    ) +
    geom_point(
      data = filt,
      aes(x = x, y = y),
      size = point_size, alpha = 0.9, color = "grey30", show.legend = FALSE
    ) +
    arrow_layer +
    ggrepel::geom_text_repel(
      data = filt,
      aes(x = x, y = y, label = .data[[label_col]]),
      size = lab_size, fontface = "bold",
      color = "grey20", box.padding = 0.22, point.padding = 0.22,
      max.overlaps = 25, segment.size = 0.2, show.legend = FALSE
    ) +
    geom_vline(xintercept = log_med_x, linetype = "22", linewidth = 0.4, color = "grey40") +
    geom_hline(yintercept = log_med_y, linetype = "22", linewidth = 0.4, color = "grey40") +
    axis_arrows +
    roman_layers +
    metrics_layers +
    scale_x_continuous(
      expand = expansion(mult = c(0.05, 0.05)),
      breaks = br_x$major, minor_breaks = br_x$minor,
      labels = p10_lab
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.05, 0.05)),
      breaks = br_y$major, minor_breaks = br_y$minor,
      labels = p10_lab
    ) +
    # Lighter quadrant fills (distinct luminance for print)
    scale_fill_manual(
      name = "Quadrants",
      values = setNames(
        c("#9ddfd3", "#ffd6b5", "#c9d1f0", "#f3c2e6"),
        fill_labels
      )
    ) +
    # Trend encoded by color (linetype optionally added above)
    scale_color_manual(
      name = "Trend",
      values = c(Positive = "#009E73", Mixed = "#6B6B6B", Negative = "#D55E00"),
      breaks = c("Positive","Mixed","Negative")
    ) +
    labs(x = axis_lab_x, y = axis_lab_y) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_blank(),
      legend.position  = "right",
      legend.title     = element_text(size = base_size),
      legend.text      = element_text(size = base_size - 1),
      legend.key       = element_rect(fill = "white", color = NA),
      legend.background= element_rect(fill = "white", color = NA),
      axis.title       = element_text(size = base_size),
      axis.text        = element_text(size = base_size - 1),
      panel.grid.major = element_line(color="grey80", linewidth = 0.3),
      panel.grid.minor = element_line(color="grey92", linewidth = 0.25),
      panel.border     = element_blank(),
      plot.margin      = margin(10, 24, 10, 10) # extra right margin; avoids clipping with legend
    )

  attr(p, "metrics") <- metrics
  list(plot = p, metrics = metrics)
}

.make_region_rects <- function(x_lin, y_lin, labels4) {
  mx <- median(x_lin, na.rm = TRUE)
  my <- median(y_lin, na.rm = TRUE)
  pad <- 0.25
  min_x <- max(1, floor(min(x_lin, na.rm = TRUE) * (1 - pad)))
  max_x <- max(1, ceiling(max(x_lin, na.rm = TRUE) * (1 + pad)))
  min_y <- max(1, floor(min(y_lin, na.rm = TRUE) * (1 - pad)))
  max_y <- max(1, ceiling(max(y_lin, na.rm = TRUE) * (1 + pad)))

  # TR = I, BR = II, TL = III, BL = IV  (match Roman numerals you draw)
  data.frame(
    xmin = c(mx,   mx,   min_x, min_x),
    xmax = c(max_x, max_x, mx,    mx),
    ymin = c(my,   min_y, my,    min_y),
    ymax = c(max_y, my,   max_y, my),
    region = factor(c(labels4[1], labels4[2], labels4[3], labels4[4]), levels = labels4)
  )
}

.log_breaks <- function(min_log, max_log) {
  min_lin <- 10^min_log; max_lin <- 10^max_log
  Lmin <- floor(log10(min_lin)); Lmax <- ceiling(log10(max_lin))
  major <- 10^(seq(Lmin, Lmax))
  minor <- sort(unique(c(major, 2*major, 4*major, 6*major, 8*major)))
  list(
    major = major[major >= min_lin & major <= max_lin] %>% log10(),
    minor = minor[minor >= min_lin & minor <= max_lin] %>% log10()
  )
}
