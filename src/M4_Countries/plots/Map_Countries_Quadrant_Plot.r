# -------------------------------------------------------------------
# Map_Countries_Quadrant_Plot (robusto frente a nombres de columnas)
# -------------------------------------------------------------------
library(R6)
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(ggrepel)
library(bibliometrix)

# helper: operador "x %||% y"
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

Map_Countries_Quadrant_Plot <- R6Class("Map_Countries_Quadrant_Plot",
  public = list(
    data = NULL,
    df = NULL,
    N_years = 5,
    num_countries = 10,
    show_arrows = TRUE,
    show_scale_arrows = TRUE,
    processed_data = NULL,
    country_data = NULL,

    # ---------------------------------------------------------------
    # Constructor
    # ---------------------------------------------------------------
    initialize = function(data, N_years = 5, num_countries = 10, show_arrows = TRUE, show_scale_arrows = TRUE) {
      self$data <- data
      self$N_years <- N_years
      self$num_countries <- num_countries
      self$show_arrows <- show_arrows
      self$show_scale_arrows <- show_scale_arrows
      self$process_data()
    },

    # ---------------------------------------------------------------
    # Paso 1: orquesta el preproceso
    # ---------------------------------------------------------------
    process_data = function() {
      parts <- self$extract_country_data(self$data, self$N_years)
      self$processed_data <- parts
      self$country_data <- self$validate_and_prepare_data(
        parts$df_first_half_summary,
        parts$df_second_half_summary
      )
    },

    # ---------------------------------------------------------------
    # Paso 4: resumen por países
    # ---------------------------------------------------------------
    summarize_data = function(df) {
      df %>%
        mutate(
          SCP = if_else(lengths(Countries_Array) == 1, 1, 0),
          MCP = if_else(lengths(Countries_Array) > 1, 1, 0)
        ) %>%
        tidyr::unnest(cols = c(Countries_Array)) %>%
        group_by(Countries_Array) %>%
        summarise(
          Total_Paper_Count = n(),
          Total_Citations   = sum(`Times Cited`, na.rm = TRUE),
          SCP_Count         = sum(SCP, na.rm = TRUE),
          MCP_Count         = sum(MCP, na.rm = TRUE),
          .groups = "drop"
        )
    },

    # ---------------------------------------------------------------
    # Paso 2: extracción/normalización (SÚPER ROBUSTA)
    # ---------------------------------------------------------------
    extract_country_data = function(converted_data, N) {

      # utilidades internas
      rename_if_exists <- function(df, from, to) {
        hit <- intersect(names(df), from)
        if (length(hit)) names(df)[match(hit, names(df))] <- to[match(hit, from)]
        df
      }
      coalesce_to <- function(.tbl, candidates, new_name, default = NULL) {
        if (!is.data.frame(.tbl)) stop("coalesce_to: .tbl debe ser data.frame")
        src <- intersect(candidates, names(.tbl))
        if (length(src)) {
          if (!(new_name %in% names(.tbl))) {
            .tbl[[new_name]] <- .tbl[[src[1]]]
          } else {
            # ya existe; no tocamos salvo que esté vacía
            na_idx <- is.na(.tbl[[new_name]]) | .tbl[[new_name]] == ""
            .tbl[[new_name]][na_idx] <- .tbl[[src[1]]][na_idx]
          }
        } else if (!is.null(default)) {
          .tbl[[new_name]] <- default
        }
        .tbl
      }

      data <- as.data.frame(converted_data)
      original_names <- names(data)

      # --- ¿viene de bibliometrix? (tiene columna DB) ---
      if ("DB" %in% names(data)) {
        data <- bibliometrix::metaTagExtraction(data, Field = "AU_CO", sep = ";")
        data <- rename_if_exists(data,
          from = c("PY","TC","AU_CO"),
          to   = c("Publication Year","Times Cited","Countries")
        )
      } else {
        # --- modo "tidy": coalesce a los nombres estándar ---
        data <- coalesce_to(
          .tbl = data,
          candidates = c("Publication Year","PY","Year","year","PublicationYear","pub_year"),
          new_name   = "Publication Year"
        )
        data <- coalesce_to(
          .tbl = data,
          candidates = c("Times Cited","TC","Citations","times_cited","TimesCited"),
          new_name   = "Times Cited",
          default    = 0
        )
        # países en string o lista
        if (!("Countries" %in% names(data)) && !("Countries_Array" %in% names(data))) {
          data <- coalesce_to(
            .tbl = data,
            candidates = c("Country","Countries_String","Country_String"),
            new_name   = "Countries"
          )
          data <- coalesce_to(
            .tbl = data,
            candidates = c("Countries_Array","Country_Array"),
            new_name   = "Countries_Array"
          )
        }
      }

      # --- POST: asegurar columnas requeridas ---
      req <- c("Publication Year","Times Cited")
      faltan <- setdiff(req, names(data))
      if (length(faltan)) {
        stop(
          "[Map_Countries_Quadrant_Plot] Faltan columnas requeridas: ",
          paste(faltan, collapse = ", "),
          ". Debes tener 'Publication Year' y 'Times Cited' y una columna de países.\n",
          "Nombres originales: ", paste(original_names, collapse = ", "), "\n",
          "Nombres tras normalizar: ", paste(names(data), collapse = ", ")
        )
      }

      # --- Countries_Array garantizada ---
      if (!("Countries_Array" %in% names(data))) {
        if ("Countries" %in% names(data)) {
          data$Countries <- as.character(data$Countries %||% "")
          data$Countries_Array <- strsplit(data$Countries, ";")
        } else {
          stop("[Map_Countries_Quadrant_Plot] No se encontraron columnas de países ('Countries' o 'Countries_Array').")
        }
      }
      # limpia la lista
      data$Countries_Array <- lapply(data$Countries_Array, function(x) {
        x <- x %||% character(0)
        x <- x[!is.na(x) & nzchar(trimws(x))]
        trimws(as.character(x))
      })
      # genera Countries (string) si no existe
      if (!("Countries" %in% names(data))) {
        data$Countries <- vapply(data$Countries_Array, function(x) paste(x, collapse = ";"), FUN.VALUE = character(1))
      }

      # --- tipos numéricos para año/citas ---
      if (!is.numeric(data[["Publication Year"]])) {
        data[["Publication Year"]] <- suppressWarnings(as.integer(data[["Publication Year"]]))
      }
      if (!is.numeric(data[["Times Cited"]])) {
        data[["Times Cited"]] <- suppressWarnings(as.numeric(data[["Times Cited"]]))
      }

      # guarda df normalizado
      self$df <- data

      # --- dataset mínimo para split ---
      new_df <- data %>% dplyr::select(`Publication Year`, Countries_Array, `Times Cited`)

      # --- split y resúmenes ---
      split_result <- self$split_data_by_years(new_df, N)
      df_first_half_summary  <- self$summarize_data(split_result$df_first_half)
      df_second_half_summary <- self$summarize_data(split_result$df_second_half)

      list(
        df_total               = split_result$df_total,
        df_first_half_summary  = df_first_half_summary,
        df_second_half_summary = df_second_half_summary,
        half_ranges            = split_result$half_ranges
      )
    },

    # ---------------------------------------------------------------
    # Paso 3: partir por años (usar SIEMPRE dplyr::filter)
    # ---------------------------------------------------------------
    split_data_by_years = function(data, N) {
      stopifnot(is.data.frame(data))
      if (!("Publication Year" %in% names(data))) {
        stop("[split_data_by_years] Falta columna 'Publication Year'")
      }

      LastYear <- suppressWarnings(max(data[["Publication Year"]], na.rm = TRUE))
      if (!is.finite(LastYear)) stop("[split_data_by_years] 'Publication Year' no tiene valores válidos.")

      start_first_half  <- LastYear - (2 * N)
      end_first_half    <- LastYear - N
      start_second_half <- end_first_half + 1
      end_second_half   <- LastYear

      df_total       <- data
      df_first_half  <- dplyr::filter(data, `Publication Year` >= start_first_half  & `Publication Year` <= end_first_half)
      df_second_half <- dplyr::filter(data, `Publication Year` >= start_second_half & `Publication Year` <= end_second_half)

      list(
        df_total      = df_total,
        df_first_half = df_first_half,
        df_second_half= df_second_half,
        half_ranges   = data.frame(
          First_Half_Range  = paste(start_first_half,  end_first_half,  sep = "-"),
          Second_Half_Range = paste(start_second_half, end_second_half, sep = "-"),
          stringsAsFactors = FALSE
        )
      )
    },

    # ---------------------------------------------------------------
    # Paso 5: join y deltas
    # ---------------------------------------------------------------
    validate_and_prepare_data = function(df_first_half_summary, df_second_half_summary) {
      dplyr::full_join(
        df_first_half_summary  %>% rename_with(~ paste0(., "_first_half")),
        df_second_half_summary %>% rename_with(~ paste0(., "_second_half")),
        by = c("Countries_Array_first_half" = "Countries_Array_second_half")
      ) %>%
        mutate(
          across(where(is.numeric), ~ tidyr::replace_na(., 0)),
          across(where(is.character), ~ tidyr::replace_na(., "")),
          delta_x = Total_Citations_second_half   - Total_Citations_first_half,
          delta_y = Total_Paper_Count_second_half - Total_Paper_Count_first_half
        )
    },

    # ---------------------------------------------------------------
    # Top países
    # ---------------------------------------------------------------
    filter_top_countries = function(data) {
      top_citations <- data %>% arrange(desc(Total_Citations))   %>% slice_head(n = self$num_countries)
      top_papers    <- data %>% arrange(desc(Total_Paper_Count)) %>% slice_head(n = self$num_countries)
      bind_rows(top_citations, top_papers) %>%
        distinct(Countries_Array_first_half, .keep_all = TRUE)
    },

    filter_top_countries_scp_mcp = function(data) {
      top_scp <- data %>% arrange(desc(Total_SCP)) %>% slice_head(n = self$num_countries)
      top_mcp <- data %>% arrange(desc(Total_MCP)) %>% slice_head(n = self$num_countries)
      bind_rows(top_scp, top_mcp) %>%
        distinct(Countries_Array_first_half, .keep_all = TRUE)
    },

    # ---------------------------------------------------------------
    # Regiones (TP/TC)
    # ---------------------------------------------------------------
    create_region_data = function(df_total) {
      df_total <- df_total %>%
        mutate(
          Total_Citations   = ifelse(is.infinite(Total_Citations), NA, Total_Citations),
          Total_Paper_Count = ifelse(is.infinite(Total_Paper_Count), NA, Total_Paper_Count)
        )
      median_x <- median(df_total$Total_Citations,   na.rm = TRUE)
      median_y <- median(df_total$Total_Paper_Count, na.rm = TRUE)

      delta_padding <- 0.25
      min_x <- max(1, floor(min(df_total$Total_Citations,   na.rm = TRUE) * (1 - delta_padding)))
      max_x <- max(1, ceiling(max(df_total$Total_Citations, na.rm = TRUE) * (1 + delta_padding)))
      min_y <- max(1, floor(min(df_total$Total_Paper_Count, na.rm = TRUE) * (1 - delta_padding)))
      max_y <- max(1, ceiling(max(df_total$Total_Paper_Count, na.rm = TRUE) * (1 + delta_padding)))

      data.frame(
        median_x = median_x, median_y = median_y,
        xmin = c(median_x, min_x,   median_x, min_x),
        xmax = c(max_x,   median_x, max_x,   median_x),
        ymin = c(median_y, median_y, min_y,  min_y),
        ymax = c(max_y,   max_y,   median_y, median_y),
        region = factor(c(
          "I: High Citations, High Papers",
          "II: High Citations, Low Papers",
          "III: Low Citations, High Papers",
          "IV: Low Citations, Low Papers"
        ))
      )
    },

    # ---------------------------------------------------------------
    # Regiones (SCP/MCP)
    # ---------------------------------------------------------------
    create_region_data_scp_mcp = function(df_total) {
      df_total <- df_total %>%
        mutate(
          Total_SCP = ifelse(is.infinite(Total_SCP), NA, Total_SCP),
          Total_MCP = ifelse(is.infinite(Total_MCP), NA, Total_MCP)
        )
      median_x <- median(df_total$Total_SCP, na.rm = TRUE)
      median_y <- median(df_total$Total_MCP, na.rm = TRUE)

      delta_padding <- 0.25
      min_x <- max(1, floor(min(df_total$Total_SCP, na.rm = TRUE) * (1 - delta_padding)))
      max_x <- max(1, ceiling(max(df_total$Total_SCP, na.rm = TRUE) * (1 + delta_padding)))
      min_y <- max(1, floor(min(df_total$Total_MCP, na.rm = TRUE) * (1 - delta_padding)))
      max_y <- max(1, ceiling(max(df_total$Total_MCP, na.rm = TRUE) * (1 + delta_padding)))

      data.frame(
        median_x = median_x, median_y = median_y,
        xmin = c(median_x, min_x,   median_x, min_x),
        xmax = c(max_x,   median_x, max_x,   median_x),
        ymin = c(median_y, median_y, min_y,  min_y),
        ymax = c(max_y,   max_y,   median_y, median_y),
        region = factor(c(
          "I: High SCP, High MCP",
          "II: High SCP, Low MCP",
          "III: Low SCP, High MCP",
          "IV: Low SCP, Low MCP"
        ))
      )
    },

    get_top_countries_dataset = function () self$country_data,

    # ---------------------------------------------------------------
    # Breaks en log10 (recibe límites ya en log10)
    # ---------------------------------------------------------------
    generate_log_breaks = function(min_val, max_val) {
      min_lin <- 10^min_val; max_lin <- 10^max_val
      log_min <- floor(log10(min_lin)); log_max <- ceiling(log10(max_lin))
      major_breaks <- 10^(seq(log_min, log_max))
      minor_breaks <- sort(unique(c(major_breaks, 2*major_breaks, 4*major_breaks, 6*major_breaks, 8*major_breaks)))
      major_breaks <- major_breaks[major_breaks >= min_lin & major_breaks <= max_lin]
      minor_breaks <- minor_breaks[minor_breaks >= min_lin & minor_breaks <= max_lin]
      list(major = log10(major_breaks), minor = log10(minor_breaks))
    },

    # ---------------------------------------------------------------
    # Serie temporal por país (opcional)
    # ---------------------------------------------------------------
    do_run_by_countries = function(){
      df <- self$df
      country_year_summary <- df %>%
        mutate(
          SCP_Count = ifelse(lengths(Countries_Array) == 1, 1, 0),
          MCP_Count = ifelse(lengths(Countries_Array) > 1, 1, 0)
        ) %>%
        tidyr::unnest(cols = c(Countries_Array)) %>%
        group_by(Countries_Array, `Publication Year`) %>%
        summarise(
          Total_Paper_Count = n(),
          Total_Citations   = sum(`Times Cited`, na.rm = TRUE),
          SCP_Count         = sum(SCP_Count),
          MCP_Count         = sum(MCP_Count),
          Density           = ifelse(Total_Paper_Count > 0, Total_Citations/Total_Paper_Count, 0),
          .groups = "drop"
        )

      top_mcp_countries <- country_year_summary %>%
        arrange(desc(MCP_Count)) %>%
        slice_head(n = 10)

      message("\n[do_run_by_countries] top MCP:")
      print(head(top_mcp_countries, 10))

      plot_country_trends(country_year_summary, "results/M4_Countries/figures/byCountry/")
      invisible(country_year_summary)
    },





















# ---------------------------------------------------------------
# Plot TP vs TC (IEEE style + quadrants, arrows, clamps + metrics)
# ---------------------------------------------------------------
generate_bubble_tp_vs_tc_plot = function(
  layout             = c("onecol", "twocol"),
  min_arrow_frac     = 0.01,   # linear-space min as fraction of ref diagonal
  max_arrow_frac     = 0.10,   # linear-space max as fraction of ref diagonal
  quadrant_alpha     = 0.08,
  show_crosshair     = TRUE,
  zero_offset        = 0.01,   # ε to place zeros on log10 axes
  point_size         = 2,
  arrow_vs_dot_mult  = 2,      # min visual arrow length ≈ this × dot radius
  add_metrics        = TRUE,   # coherence + LFI + net flow
  metrics_alpha      = 0.9
) {
  .guard <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
  layout <- match.arg(layout)

  need <- c("Total_Citations_first_half","Total_Citations_second_half",
            "Total_Paper_Count_first_half","Total_Paper_Count_second_half",
            "Countries_Array_first_half","delta_x","delta_y")
  .guard(is.data.frame(self$country_data), "[TPvsTC] self$country_data must be a data.frame.")
  .guard(all(need %in% names(self$country_data)),
         sprintf("[TPvsTC] Missing columns: %s", paste(setdiff(need, names(self$country_data)), collapse = ", ")))

  # ---- totals (linear)
  df_total <- self$country_data %>%
    dplyr::mutate(
      Total_Citations   = (Total_Citations_first_half   %||% 0) + (Total_Citations_second_half   %||% 0),
      Total_Paper_Count = (Total_Paper_Count_first_half %||% 0) + (Total_Paper_Count_second_half %||% 0)
    )

  # ---- Top‑N selection (what is shown)
  filtered_data <- self$filter_top_countries(df_total) %>%
    dplyr::filter(!is.na(Countries_Array_first_half),
                  Countries_Array_first_half != "",
                  Countries_Array_first_half != "NA")
  .guard(nrow(filtered_data) > 0, "[TPvsTC] No valid data after filtering.")

  # ---- trend categories from RAW deltas
  filtered_data <- filtered_data %>%
    dplyr::mutate(
      Total_Citations   = pmax(Total_Citations,   0),
      Total_Paper_Count = pmax(Total_Paper_Count, 0),
      trend_category = dplyr::case_when(
        delta_x > 0 & delta_y > 0 ~ "Positive",
        delta_x < 0 & delta_y < 0 ~ "Negative",
        TRUE ~ "Mixed"
      )
    )

  # ---- subset medians in linear space (for quadrants & flow)
  med_x_lin <- stats::median(filtered_data$Total_Citations,   na.rm = TRUE)
  med_y_lin <- stats::median(filtered_data$Total_Paper_Count, na.rm = TRUE)

  # ---- region rectangles from presented set, then to log
  region_log <- self$create_region_data(filtered_data) %>%
    dplyr::mutate(
      xmin = log10(pmax(xmin + zero_offset, 1e-9)),
      xmax = log10(pmax(xmax + zero_offset, 1e-9)),
      ymin = log10(pmax(ymin + zero_offset, 1e-9)),
      ymax = log10(pmax(ymax + zero_offset, 1e-9))
    )

  # ---- arrow scaling (linear), then convert to log; ensure visual min in log
  max_dx <- max(abs(filtered_data$delta_x), na.rm = TRUE); if (!is.finite(max_dx) || max_dx == 0) max_dx <- 1e-9
  max_dy <- max(abs(filtered_data$delta_y), na.rm = TRUE); if (!is.finite(max_dy) || max_dy == 0) max_dy <- 1e-9

  ref_x <- 0.10 * max(filtered_data$Total_Citations   + zero_offset, na.rm = TRUE)
  ref_y <- 0.10 * max(filtered_data$Total_Paper_Count + zero_offset, na.rm = TRUE)
  ref_di <- sqrt(ref_x^2 + ref_y^2)
  Lmin   <- min_arrow_frac * ref_di
  Lmax   <- max_arrow_frac * ref_di
  .guard(Lmin <= Lmax, "[TPvsTC] min_arrow_frac must be <= max_arrow_frac.")

  filt0 <- filtered_data %>%
    dplyr::mutate(
      dx0  = (delta_x / max_dx) * ref_x,
      dy0  = (delta_y / max_dy) * ref_y,
      len0 = sqrt(dx0^2 + dy0^2),
      target_len = dplyr::case_when(
        len0 == 0 ~ 0,       # true no-change → no arrow (show point)
        len0 <  Lmin ~ Lmin,
        len0 >  Lmax ~ Lmax,
        TRUE ~ len0
      ),
      ux = dplyr::if_else(len0 > 0, dx0 / len0, 0),
      uy = dplyr::if_else(len0 > 0, dy0 / len0, 0),
      dx = ux * target_len,
      dy = uy * target_len,

      x_lin    = Total_Citations   + zero_offset,
      y_lin    = Total_Paper_Count + zero_offset,
      xend_lin = pmax(Total_Citations   + dx + zero_offset, 1e-9),
      yend_lin = pmax(Total_Paper_Count + dy + zero_offset, 1e-9),

      x    = log10(x_lin),
      y    = log10(y_lin),
      xend = log10(xend_lin),
      yend = log10(yend_lin)
    )

  # ---- enforce arrow >= ~2× dot radius visually (in log space)
  log_len_min <- (arrow_vs_dot_mult * (point_size / 2)) * 0.04
  filt <- filt0 %>%
    dplyr::mutate(
      log_len = sqrt((xend - x)^2 + (yend - y)^2),
      scale_up = dplyr::if_else(log_len > 0 & log_len < log_len_min, log_len_min / log_len, 1),
      xend = x + (xend - x) * scale_up,
      yend = y + (yend - y) * scale_up
    )

  # ---- medians for crosshair (subset medians; linear → log)
  log_med_x <- log10(pmax(stats::median(10^filt$x, na.rm = TRUE), 1))
  log_med_y <- log10(pmax(stats::median(10^filt$y, na.rm = TRUE), 1))

  # ---- axis breaks/labels
  min_x <- min(region_log$xmin, filt$x, na.rm = TRUE)
  max_x <- max(region_log$xmax, filt$x, na.rm = TRUE)
  min_y <- min(region_log$ymin, filt$y, na.rm = TRUE)
  max_y <- max(region_log$ymax, filt$y, na.rm = TRUE)
  x_breaks <- self$generate_log_breaks(min_x, max_x)
  y_breaks <- self$generate_log_breaks(min_y, max_y)
  p10_lab  <- function(v) scales::math_format(10^.x)(v)

  # ---- typography
  base_size <- if (layout == "onecol") 9 else 10
  lab_size  <- if (layout == "onecol") 3.0 else 3.3

  # ===================== METRICS =====================
  metrics <- list()
  if (isTRUE(add_metrics)) {
    # --- Coherence (mean unit direction length)
    mag <- sqrt((filt0$dx)^2 + (filt0$dy)^2)
    ux  <- ifelse(mag > 0, filt0$dx / mag, 0)
    uy  <- ifelse(mag > 0, filt0$dy / mag, 0)
    mean_u     <- c(mean(ux), mean(uy))
    coherence  <- sqrt(sum(mean_u^2))  # [0,1]

    # --- Leader–Follower Index (weighted mean cosine vs leader)
    # leader = max(Total_Citations + Total_Paper_Count) with non-zero delta
    ranking <- filtered_data %>%
      dplyr::mutate(score_lead = Total_Citations + Total_Paper_Count) %>%
      dplyr::arrange(dplyr::desc(score_lead))
    LFI <- NA_real_; leader <- NA_character_
    if (nrow(ranking) > 0) {
      for (k in seq_len(nrow(ranking))) {
        cand <- ranking[k, , drop = FALSE]
        vLx <- cand$delta_x; vLy <- cand$delta_y
        Llen <- sqrt(vLx^2 + vLy^2)
        if (is.finite(Llen) && Llen > 0) {
          uL <- c(vLx, vLy) / Llen
          leader <- cand$Countries_Array_first_half
          # Weighted by follower magnitude
          cosines <- c()
          weights <- c()
          for (i in seq_len(nrow(filtered_data))) {
            if (identical(filtered_data$Countries_Array_first_half[i], leader)) next
            vi <- c(filtered_data$delta_x[i], filtered_data$delta_y[i])
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

    # --- Net Quadrant Flow (to Quadrant I) using subset medians
    qof <- function(x, y, mx, my) {
      if (x >= mx && y >= my) return("I")
      if (x >= mx && y <  my) return("II")
      if (x <  mx && y >= my) return("III")
      return("IV")
    }
    start_q <- mapply(qof,
                      filtered_data$Total_Citations,
                      filtered_data$Total_Paper_Count,
                      MoreArgs = list(mx = med_x_lin, my = med_y_lin))
    end_q   <- mapply(qof,
                      filtered_data$Total_Citations + filtered_data$delta_x,
                      filtered_data$Total_Paper_Count + filtered_data$delta_y,
                      MoreArgs = list(mx = med_x_lin, my = med_y_lin))
    entrants_I <- sum(start_q != "I" & end_q == "I")
    leavers_I  <- sum(start_q == "I" & end_q != "I")
    net_I      <- entrants_I - leavers_I
    movers_tot <- sum(start_q != end_q)

    # --- Global mean arrow (drawn at subset medians in linear → log)
    cx <- stats::median(filt0$x_lin, na.rm = TRUE)
    cy <- stats::median(filt0$y_lin, na.rm = TRUE)
    mean_mag  <- stats::median(mag, na.rm = TRUE)
    g_len     <- 0.12 * mean_mag
    gx1 <- log10(pmax(cx, 1e-9)); gy1 <- log10(pmax(cy, 1e-9))
    gx2 <- log10(pmax(cx + mean_u[1]*g_len, 1e-9))
    gy2 <- log10(pmax(cy + mean_u[2]*g_len, 1e-9))

    # ---- top-left label position (with extra padding)
    padx <- (max_x - min_x) * 0.05
    pady <- (max_y - min_y) * 0.05
    lab_x <- min_x + padx
    lab_y <- max_y - pady

    metrics_layers <- list(
      ggplot2::geom_segment(
        aes(x = gx1, y = gy1, xend = gx2, yend = gy2),
        data = data.frame(gx1 = gx1, gy1 = gy1, gx2 = gx2, gy2 = gy2),
        linewidth = 0.7, color = "black", alpha = metrics_alpha,
        arrow = grid::arrow(length = grid::unit(0.22, "cm"))
      ),
      ggplot2::annotate(
        "label",
        x = lab_x, y = lab_y, hjust = 0, vjust = 1,
        label = sprintf("Coherence = %.2f\nLFI (%s) = %s\nNet→QⅠ = %+d (movers: %d)",
                        coherence,
                        ifelse(is.na(leader), "—", leader),
                        ifelse(is.na(LFI), "—", sprintf("%.2f", LFI)),
                        net_I, movers_tot),
        size = 3.2, label.size = 0, alpha = 0.85
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
  } else {
    metrics_layers <- list()
  }

  # ---- “zero-axis” direction cues (tiny arrows near lower-left)
  axis_arrows <- {
    padx <- (max_x - min_x) * 0.03
    pady <- (max_y - min_y) * 0.03
    list(
      ggplot2::geom_segment(
        aes(x = min_x + padx, y = min_y + pady, xend = min_x + 6*padx, yend = min_y + pady),
        data = data.frame(), inherit.aes = FALSE,
        arrow = grid::arrow(length = grid::unit(0.18, "cm")),
        linewidth = 0.4, color = "grey35"
      ),
      ggplot2::geom_segment(
        aes(x = min_x + padx, y = min_y + pady, xend = min_x + padx, yend = min_y + 6*pady),
        data = data.frame(), inherit.aes = FALSE,
        arrow = grid::arrow(length = grid::unit(0.18, "cm")),
        linewidth = 0.4, color = "grey35"
      )
    )
  }

  # ---- Roman numerals in corners (TR=I, BR=II, TL=III, BL=IV) with safer padding
  roman_layers <- {
    padx <- (max_x - min_x) * 0.05
    pady <- (max_y - min_y) * 0.05
    labs_df <- data.frame(
      label = c("III","I","IV","II"),
      x = c(min_x + padx, max_x - padx, min_x + padx, max_x - padx),
      y = c(max_y - pady, max_y - pady, min_y + pady, min_y + pady)
    )
    ggplot2::geom_text(
      data = labs_df,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      color = "grey30", fontface = "bold",
      size = if (layout == "onecol") 3.8 else 4.2
    )
  }

  # ---- plot
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
    { if (isTRUE(self$show_arrows)) geom_segment(
        data = filt %>% dplyr::filter(is.finite(x) & is.finite(y) & is.finite(xend) & is.finite(yend)),
        aes(x = x, y = y, xend = xend, yend = yend, color = trend_category),
        arrow = grid::arrow(length = grid::unit(0.18, "cm")),
        linewidth = 0.45, show.legend = TRUE
      ) } +
    ggrepel::geom_text_repel(
      data = filt,
      aes(x = x, y = y, label = Countries_Array_first_half),
      size = lab_size, fontface = "bold",
      color = "grey20", box.padding = 0.22, point.padding = 0.22,
      max.overlaps = 25, segment.size = 0.2, show.legend = FALSE
    ) +
    { if (show_crosshair) list(
        geom_vline(xintercept = log_med_x, linetype = "22", linewidth = 0.4, color = "grey40"),
        geom_hline(yintercept = log_med_y, linetype = "22", linewidth = 0.4, color = "grey40")
      ) } +
    axis_arrows +
    roman_layers +
    metrics_layers +
    scale_x_continuous(
      expand = expansion(mult = c(0.05, 0.05)),
      breaks = x_breaks$major, minor_breaks = x_breaks$minor,
      labels = p10_lab
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.05, 0.05)),
      breaks = y_breaks$major, minor_breaks = y_breaks$minor,
      labels = p10_lab
    ) +
    scale_fill_manual(
      name = "Quadrants",
      values = c(
        "I: High Citations, High Papers" = "#65C3B3",
        "II: High Citations, Low Papers"  = "#FDAE85",
        "III: Low Citations, High Papers" = "#98A5CC",
        "IV: Low Citations, Low Papers"   = "#E79CD3"
      )
    ) +
    scale_color_manual(
      name = "Trend",
      values = c(Positive = "#009E73", Mixed = "#6B6B6B", Negative = "#D55E00"),
      breaks = c("Positive","Mixed","Negative")
    ) +
    labs(
      x = sprintf("Total Citations (log\u2081\u2080, \u03B5=%.2f)", zero_offset),
      y = sprintf("Total Papers (log\u2081\u2080, \u03B5=%.2f)",    zero_offset)
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      plot.title      = element_blank(),
      legend.position = "right",
      axis.title      = element_text(size = base_size),
      axis.text       = element_text(size = base_size - 1),
      panel.grid.major= element_line(color="grey80", linewidth = 0.3),
      panel.grid.minor= element_line(color="grey92", linewidth = 0.25),
      panel.border    = element_blank()
    )

  ggplot_build(p)
  attr(p, "metrics") <- metrics
  p
}
,




# ---------------------------------------------------------------
# Plot SCP vs MCP (IEEE style + quadrants, arrows, clamps + metrics)
# ---------------------------------------------------------------
generate_bubble_scp_vs_mcp_plot = function(
  layout             = c("onecol", "twocol"),
  min_arrow_frac     = 0.01,
  max_arrow_frac     = 0.10,
  quadrant_alpha     = 0.08,
  show_crosshair     = TRUE,
  zero_offset        = 0.01,   # ε
  point_size         = 2,
  arrow_vs_dot_mult  = 2,
  add_metrics        = TRUE,
  metrics_alpha      = 0.9
) {
  .guard <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
  layout <- match.arg(layout)

  need <- c("SCP_Count_first_half","SCP_Count_second_half",
            "MCP_Count_first_half","MCP_Count_second_half",
            "Countries_Array_first_half","delta_x","delta_y")
  .guard(is.data.frame(self$country_data), "[SCPvsMCP] self$country_data must be a data.frame.")
  .guard(all(need %in% names(self$country_data)),
         sprintf("[SCPvsMCP] Missing columns: %s", paste(setdiff(need, names(self$country_data)), collapse = ", ")))

  # ---- totals (linear)
  df_total <- self$country_data %>%
    dplyr::mutate(
      Total_SCP = (SCP_Count_first_half %||% 0) + (SCP_Count_second_half %||% 0),
      Total_MCP = (MCP_Count_first_half %||% 0) + (MCP_Count_second_half %||% 0)
    )

  # ---- Top‑N selection; compute raw deltas here
  filtered_data <- self$filter_top_countries_scp_mcp(
    df_total %>% dplyr::mutate(
      delta_x = (SCP_Count_second_half %||% 0) - (SCP_Count_first_half %||% 0),
      delta_y = (MCP_Count_second_half %||% 0) - (MCP_Count_first_half %||% 0)
    )
  ) %>%
    dplyr::filter(!is.na(Countries_Array_first_half),
                  Countries_Array_first_half != "",
                  Countries_Array_first_half != "NA")
  .guard(nrow(filtered_data) > 0, "[SCPvsMCP] No valid data after filtering.")

  # ---- trend categories
  filtered_data <- filtered_data %>%
    dplyr::mutate(
      Total_SCP = pmax(Total_SCP, 0),
      Total_MCP = pmax(Total_MCP, 0),
      trend_category = dplyr::case_when(
        delta_x > 0 & delta_y > 0 ~ "Positive",
        delta_x < 0 & delta_y < 0 ~ "Negative",
        TRUE ~ "Mixed"
      )
    )

  # ---- subset medians (for quadrants & flow)
  med_x_lin <- stats::median(filtered_data$Total_SCP, na.rm = TRUE)
  med_y_lin <- stats::median(filtered_data$Total_MCP, na.rm = TRUE)

  # ---- regions (subset medians) → log
  region_log <- self$create_region_data_scp_mcp(filtered_data) %>%
    dplyr::mutate(
      xmin = log10(pmax(xmin + zero_offset, 1e-9)),
      xmax = log10(pmax(xmax + zero_offset, 1e-9)),
      ymin = log10(pmax(ymin + zero_offset, 1e-9)),
      ymax = log10(pmax(ymax + zero_offset, 1e-9))
    )

  # ---- arrow scaling (linear) then ensure min visual in log
  max_dx <- max(abs(filtered_data$delta_x), na.rm = TRUE); if (!is.finite(max_dx) || max_dx == 0) max_dx <- 1e-9
  max_dy <- max(abs(filtered_data$delta_y), na.rm = TRUE); if (!is.finite(max_dy) || max_dy == 0) max_dy <- 1e-9

  ref_x <- 0.10 * max(filtered_data$Total_SCP + zero_offset, na.rm = TRUE)
  ref_y <- 0.10 * max(filtered_data$Total_MCP + zero_offset, na.rm = TRUE)
  ref_di <- sqrt(ref_x^2 + ref_y^2)
  Lmin   <- min_arrow_frac * ref_di
  Lmax   <- max_arrow_frac * ref_di
  .guard(Lmin <= Lmax, "[SCPvsMCP] min_arrow_frac must be <= max_arrow_frac.")

  filt0 <- filtered_data %>%
    dplyr::mutate(
      dx0  = (delta_x / max_dx) * ref_x,
      dy0  = (delta_y / max_dy) * ref_y,
      len0 = sqrt(dx0^2 + dy0^2),
      target_len = dplyr::case_when(
        len0 == 0 ~ 0,
        len0 <  Lmin ~ Lmin,
        len0 >  Lmax ~ Lmax,
        TRUE ~ len0
      ),
      ux = dplyr::if_else(len0 > 0, dx0 / len0, 0),
      uy = dplyr::if_else(len0 > 0, dy0 / len0, 0),
      dx = ux * target_len,
      dy = uy * target_len,

      x_lin    = Total_SCP + zero_offset,
      y_lin    = Total_MCP + zero_offset,
      xend_lin = pmax(Total_SCP + dx + zero_offset, 1e-9),
      yend_lin = pmax(Total_MCP + dy + zero_offset, 1e-9),

      x    = log10(x_lin),
      y    = log10(y_lin),
      xend = log10(xend_lin),
      yend = log10(yend_lin)
    )

  # min visual arrow in log space
  log_len_min <- (arrow_vs_dot_mult * (point_size / 2)) * 0.04
  filt <- filt0 %>%
    dplyr::mutate(
      log_len = sqrt((xend - x)^2 + (yend - y)^2),
      scale_up = dplyr::if_else(log_len > 0 & log_len < log_len_min, log_len_min / log_len, 1),
      xend = x + (xend - x) * scale_up,
      yend = y + (yend - y) * scale_up
    )

  # ---- medians (subset) for crosshair
  log_med_x <- log10(pmax(stats::median(10^filt$x, na.rm = TRUE), 1))
  log_med_y <- log10(pmax(stats::median(10^filt$y, na.rm = TRUE), 1))

  # ---- axis breaks/labels
  min_x <- min(region_log$xmin, filt$x, na.rm = TRUE)
  max_x <- max(region_log$xmax, filt$x, na.rm = TRUE)
  min_y <- min(region_log$ymin, filt$y, na.rm = TRUE)
  max_y <- max(region_log$ymax, filt$y, na.rm = TRUE)
  x_breaks <- self$generate_log_breaks(min_x, max_x)
  y_breaks <- self$generate_log_breaks(min_y, max_y)
  p10_lab  <- function(v) scales::math_format(10^.x)(v)

  base_size <- if (layout == "onecol") 9 else 10
  lab_size  <- if (layout == "onecol") 3.0 else 3.3

  # ===================== METRICS =====================
  metrics <- list()
  if (isTRUE(add_metrics)) {
    # Coherence
    mag <- sqrt((filt0$dx)^2 + (filt0$dy)^2)
    ux  <- ifelse(mag > 0, filt0$dx / mag, 0)
    uy  <- ifelse(mag > 0, filt0$dy / mag, 0)
    mean_u     <- c(mean(ux), mean(uy))
    coherence  <- sqrt(sum(mean_u^2))

    # Leader–Follower Index (leader = max Total_SCP+Total_MCP with non-zero delta)
    ranking <- filtered_data %>%
      dplyr::mutate(score_lead = Total_SCP + Total_MCP) %>%
      dplyr::arrange(dplyr::desc(score_lead))
    LFI <- NA_real_; leader <- NA_character_
    if (nrow(ranking) > 0) {
      for (k in seq_len(nrow(ranking))) {
        cand <- ranking[k, , drop = FALSE]
        vLx <- cand$delta_x; vLy <- cand$delta_y
        Llen <- sqrt(vLx^2 + vLy^2)
        if (is.finite(Llen) && Llen > 0) {
          uL <- c(vLx, vLy) / Llen
          leader <- cand$Countries_Array_first_half
          cosines <- c(); weights <- c()
          for (i in seq_len(nrow(filtered_data))) {
            if (identical(filtered_data$Countries_Array_first_half[i], leader)) next
            vi <- c(filtered_data$delta_x[i], filtered_data$delta_y[i])
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

    # Net Quadrant Flow (to QI) using subset medians
    qof <- function(x, y, mx, my) {
      if (x >= mx && y >= my) return("I")
      if (x >= mx && y <  my) return("II")
      if (x <  mx && y >= my) return("III")
      return("IV")
    }
    start_q <- mapply(qof, filtered_data$Total_SCP, filtered_data$Total_MCP,
                      MoreArgs = list(mx = med_x_lin, my = med_y_lin))
    end_q   <- mapply(qof,
                      filtered_data$Total_SCP + filtered_data$delta_x,
                      filtered_data$Total_MCP + filtered_data$delta_y,
                      MoreArgs = list(mx = med_x_lin, my = med_y_lin))
    entrants_I <- sum(start_q != "I" & end_q == "I")
    leavers_I  <- sum(start_q == "I" & end_q != "I")
    net_I      <- entrants_I - leavers_I
    movers_tot <- sum(start_q != end_q)

    # global mean arrow at subset medians (linear → log)
    cx <- stats::median(filt0$x_lin, na.rm = TRUE)
    cy <- stats::median(filt0$y_lin, na.rm = TRUE)
    mean_mag  <- stats::median(mag, na.rm = TRUE)
    g_len     <- 0.12 * mean_mag
    gx1 <- log10(pmax(cx, 1e-9)); gy1 <- log10(pmax(cy, 1e-9))
    gx2 <- log10(pmax(cx + mean_u[1]*g_len, 1e-9))
    gy2 <- log10(pmax(cy + mean_u[2]*g_len, 1e-9))

    # top-left label with extra padding
    padx <- (max_x - min_x) * 0.05
    pady <- (max_y - min_y) * 0.05
    lab_x <- min_x + padx
    lab_y <- max_y - pady

    metrics_layers <- list(
      ggplot2::geom_segment(
        aes(x = gx1, y = gy1, xend = gx2, yend = gy2),
        data = data.frame(gx1 = gx1, gy1 = gy1, gx2 = gx2, gy2 = gy2),
        linewidth = 0.7, color = "black", alpha = metrics_alpha,
        arrow = grid::arrow(length = grid::unit(0.22, "cm"))
      ),
      ggplot2::annotate(
        "label",
        x = lab_x, y = lab_y, hjust = 0, vjust = 1,
        label = sprintf("Coherence = %.2f\nLFI (%s) = %s\nNet→QⅠ = %+d (movers: %d)",
                        coherence,
                        ifelse(is.na(leader), "—", leader),
                        ifelse(is.na(LFI), "—", sprintf("%.2f", LFI)),
                        net_I, movers_tot),
        size = 3.2, label.size = 0, alpha = 0.85
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
  } else {
    metrics_layers <- list()
  }

  # ---- “zero-axis” direction cues
  axis_arrows <- {
    padx <- (max_x - min_x) * 0.03
    pady <- (max_y - min_y) * 0.03
    list(
      ggplot2::geom_segment(
        aes(x = min_x + padx, y = min_y + pady, xend = min_x + 6*padx, yend = min_y + pady),
        data = data.frame(), inherit.aes = FALSE,
        arrow = grid::arrow(length = grid::unit(0.18, "cm")),
        linewidth = 0.4, color = "grey35"
      ),
      ggplot2::geom_segment(
        aes(x = min_x + padx, y = min_y + pady, xend = min_x + padx, yend = min_y + 6*pady),
        data = data.frame(), inherit.aes = FALSE,
        arrow = grid::arrow(length = grid::unit(0.18, "cm")),
        linewidth = 0.4, color = "grey35"
      )
    )
  }

  # ---- Roman numerals in corners with safer padding
  roman_layers <- {
    padx <- (max_x - min_x) * 0.05
    pady <- (max_y - min_y) * 0.05
    labs_df <- data.frame(
      label = c("III","I","IV","II"),
      x = c(min_x + padx, max_x - padx, min_x + padx, max_x - padx),
      y = c(max_y - pady, max_y - pady, min_y + pady, min_y + pady)
    )
    ggplot2::geom_text(
      data = labs_df,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      color = "grey30", fontface = "bold",
      size = if (layout == "onecol") 3.8 else 4.2
    )
  }

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
    { if (isTRUE(self$show_arrows)) geom_segment(
        data = filt %>% dplyr::filter(is.finite(x) & is.finite(y) & is.finite(xend) & is.finite(yend)),
        aes(x = x, y = y, xend = xend, yend = yend, color = trend_category),
        arrow = grid::arrow(length = grid::unit(0.18, "cm")),
        linewidth = 0.45, show.legend = TRUE
      ) } +
    ggrepel::geom_text_repel(
      data = filt,
      aes(x = x, y = y, label = Countries_Array_first_half),
      size = lab_size, fontface = "bold",
      color = "grey20", box.padding = 0.22, point.padding = 0.22,
      max.overlaps = 25, segment.size = 0.2, show.legend = FALSE
    ) +
    { if (show_crosshair) list(
        geom_vline(xintercept = log_med_x, linetype = "22", linewidth = 0.4, color = "grey40"),
        geom_hline(yintercept = log_med_y, linetype = "22", linewidth = 0.4, color = "grey40")
      ) } +
    axis_arrows +
    roman_layers +
    metrics_layers +
    scale_x_continuous(
      expand = expansion(mult = c(0.05, 0.05)),
      breaks = x_breaks$major, minor_breaks = x_breaks$minor,
      labels = p10_lab
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.05, 0.05)),
      breaks = y_breaks$major, minor_breaks = y_breaks$minor,
      labels = p10_lab
    ) +
    scale_fill_manual(
      name = "Quadrants",
      values = c(
        "I: High SCP, High MCP" = "#65C3B3",
        "II: High SCP, Low MCP"  = "#FDAE85",
        "III: Low SCP, High MCP" = "#98A5CC",
        "IV: Low SCP, Low MCP"   = "#E79CD3"
      )
    ) +
    scale_color_manual(
      name = "Trend",
      values = c(Positive = "#009E73", Mixed = "#6B6B6B", Negative = "#D55E00"),
      breaks = c("Positive","Mixed","Negative")
    ) +
    labs(
      x = sprintf("SCP (log\u2081\u2080, \u03B5=%.2f)", zero_offset),
      y = sprintf("MCP (log\u2081\u2080, \u03B5=%.2f)", zero_offset)
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      plot.title      = element_blank(),
      legend.position = "right",
      axis.title      = element_text(size = base_size),
      axis.text       = element_text(size = base_size - 1),
      panel.grid.major= element_line(color="grey80", linewidth = 0.3),
      panel.grid.minor= element_line(color="grey92", linewidth = 0.25),
      panel.border    = element_blank()
    )

  ggplot_build(p)
  attr(p, "metrics") <- metrics
  p
}


















  )
)

# -------------------------------------------------------------------
# Format labels para ejes log10
# -------------------------------------------------------------------
custom_log_formatter <- function(x) {
  formatted_values <- round(10^x)
  format(formatted_values, big.mark = ",")
}
