# ============================================================================ #
# M4_M3_MCP_SHARE_TREND — Hypothesis 1: Is collaboration increasing?
# - Yearly MCP share = MCP / (SCP + MCP)
# - Adds SCP share = SCP / (SCP + MCP)
# - Outputs 3 plots:
#     1. MCP share + regression + annotation
#     2. SCP share + regression + annotation
#     3. Combined MCP + SCP shares (no regression)
# - Auto-detects country column name (Country_Array, Countries_Array, etc.)
# - IEEE Transactions–ready formatting
# ============================================================================ #

suppressPackageStartupMessages({
  library(ggplot2)
  library(jsonlite)
  library(scales)
  library(grid)
  library(tidyr)
})

M4_M3_MCP_SHARE_TREND <- setRefClass(
  "M4_M3_MCP_SHARE_TREND",
  fields = list(
    df               = "data.frame",
    country_col      = "character",
    year_col         = "character",
    scp_col          = "character",
    mcp_col          = "character",
    countries_is_raw = "logical",
    sep_candidates   = "character",
    results          = "list",
    plots            = "list",
    params           = "list"
  ),

  methods = list(

    initialize = function(df,
                          country_col = "Countries_Array",
                          year_col    = "Publication Year",
                          scp_col     = "SCP",
                          mcp_col     = "MCP",
                          countries_is_raw = TRUE,
                          sep_candidates   = c(";", "\\|", ","),
                          debug            = FALSE) {

      .self$df               <- as.data.frame(df)
      .self$country_col      <- country_col
      .self$year_col         <- year_col
      .self$scp_col          <- scp_col
      .self$mcp_col          <- mcp_col
      .self$countries_is_raw <- countries_is_raw
      .self$sep_candidates   <- sep_candidates
      .self$results          <- list()
      .self$plots            <- list()
      .self$params           <- list(debug = debug)

      if (!.self$year_col %in% names(.self$df)) {
        stop(sprintf("[M4_M3] Column '%s' not found in df.", .self$year_col))
      }
      .self$df[[.self$year_col]] <- suppressWarnings(as.numeric(.self$df[[.self$year_col]]))

      # --- Auto-resolve usable country column if missing ------------------- #
      if (isTRUE(.self$countries_is_raw) && !.self$country_col %in% names(.self$df)) {
        candidates <- c(
          .self$country_col,
          "Country", "Countries",
          "Country_Array", "Countries_Array",
          "Country/Region", "C1",
          "Affiliations", "Author Countries", "AU_CO"
        )
        found <- candidates[candidates %in% names(.self$df)]
        if (length(found) > 0) {
          .self$country_col <- found[1]
          if (debug) message(sprintf("[M4_M3] country_col not found. Using '%s'.", .self$country_col))
        } else {
          stop(sprintf("[M4_M3] countries_is_raw=TRUE but no expected country column found. DF cols: %s",
                       paste(names(.self$df), collapse = ", ")))
        }
      }

      invisible(.self)
    },

    # ----------------------------- Helpers -------------------------------- #
    logmsg = function(...) {
      if (isTRUE(.self$params$debug)) message(sprintf("[M4_M3] %s", sprintf(...)))
    },

    .safe_dir = function(path) {
      if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
    },

    .detect_separator = function(x) {
      counts <- sapply(.self$sep_candidates, function(sep) {
        mean(vapply(x, function(s) {
          if (is.na(s) || is.null(s) || s == "") return(1L)
          length(unlist(strsplit(as.character(s), sep)))
        }, integer(1L)), na.rm = TRUE)
      })
      names(counts)[which.max(counts)]
    },

    .derive_yearly_scp_mcp = function() {
      df_local <- .self$df
      ycol <- .self$year_col
      ccol <- .self$country_col
      vals <- df_local[[ccol]]

      # detect separator from non-empty values
      sep <- ";"
      non_empty <- vals[!is.na(vals) & !is.list(vals) & vals != ""]
      if (length(non_empty) > 0) sep <- .self$.detect_separator(non_empty)

      tokens_len <- vapply(seq_along(vals), function(i) {
        s <- vals[[i]]
        if (is.null(s) || (length(s) == 1 && is.na(s))) return(0L)
        if (is.list(s) || length(s) > 1) return(length(unique(as.character(unlist(s)))))
        if (is.character(s) && s != "") {
          return(length(unique(trimws(unlist(strsplit(s, sep))))))
        }
        return(0L)
      }, integer(1L))

      df_tmp <- data.frame(
        Year = suppressWarnings(as.numeric(df_local[[ycol]])),
        k    = tokens_len
      )

      df_tmp <- df_tmp[is.finite(df_tmp$Year) & df_tmp$k > 0, , drop = FALSE]
      df_tmp$SCP_flag <- as.integer(df_tmp$k == 1L)
      df_tmp$MCP_flag <- as.integer(df_tmp$k >= 2L)

      agg <- aggregate(cbind(SCP_flag, MCP_flag) ~ Year, data = df_tmp, sum, na.rm = TRUE)
      names(agg) <- c("Year", "SCP", "MCP")
      agg[order(agg$Year), , drop = FALSE]
    },

    .use_aggregated_scp_mcp = function() {
      agg <- .self$df[, c(.self$year_col, .self$scp_col, .self$mcp_col)]
      names(agg) <- c("Year", "SCP", "MCP")
      agg <- agg[is.finite(agg$Year), , drop = FALSE]
      agg[order(agg$Year), , drop = FALSE]
    },

    .build_year_series = function() {
      agg <- if (isTRUE(.self$countries_is_raw)) .self$.derive_yearly_scp_mcp() else .self$.use_aggregated_scp_mcp()
      agg$Total     <- agg$SCP + agg$MCP
      agg$MCP_share <- ifelse(agg$Total > 0, agg$MCP / agg$Total, NA_real_)
      agg$SCP_share <- ifelse(agg$Total > 0, agg$SCP / agg$Total, NA_real_)
      agg <- agg[is.finite(agg$MCP_share), , drop = FALSE]
      agg
    },

    .fit_trend = function(y, x) {
      fit  <- lm(y ~ x)
      summ <- summary(fit)
      slope <- unname(coef(fit)["x"])
      intercept <- unname(coef(fit)["(Intercept)"])
      r2 <- summ$r.squared
      p  <- tryCatch(coef(summ)["x", "Pr(>|t|)"], error = function(e) NA_real_)

      trend <- if (is.na(p)) {
        "undetermined"
      } else if (p < 0.05 && slope > 0) "increasing"
      else if (p < 0.05 && slope < 0) "decreasing"
      else "flat"

      list(intercept = intercept, slope = slope, r2 = r2, p_value = p, trend = trend)
    },

    .theme_ieee = function() {
      theme_minimal(base_family = "Times New Roman", base_size = 9) +
        theme(
          plot.title   = element_text(hjust = 0.5, face = "bold", margin = margin(b = 6)),
          axis.title.x = element_text(margin = margin(t = 4)),
          axis.title.y = element_text(margin = margin(r = 4)),
          axis.text    = element_text(color = "black"),
          panel.grid.minor = element_blank(),
          legend.position  = "none",
          plot.margin      = margin(6, 6, 6, 6)
        )
    },

    # ----------------------------- Plotters -------------------------------- #
    .build_share_plot = function(agg, ycol, title, fit_stats) {
  yhat <- fit_stats$intercept + fit_stats$slope * agg$Year
  df_fit <- data.frame(Year = agg$Year, yhat = yhat)

  label_lines <- sprintf("Trend: %s\nβ = %.4f\nR² = %.3f\np = %.3g",
                         fit_stats$trend, fit_stats$slope, fit_stats$r2, fit_stats$p_value)

  x_min <- min(agg$Year, na.rm = TRUE); x_max <- max(agg$Year, na.rm = TRUE)
  y_min <- max(0, min(agg[[ycol]], na.rm = TRUE))
  y_max <- min(1, max(agg[[ycol]], na.rm = TRUE))
  x_box <- x_min + 0.05 * (x_max - x_min)
  y_box <- y_max - 0.05 * (y_max - y_min)

  ggplot(agg, aes(x = Year, y = .data[[ycol]])) +
    geom_point(size = 1.5, shape = 1, color = "black") +
    geom_line(data = df_fit, aes(y = yhat), linetype = "dashed", color = "black") +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(y_min, y_max)) +
    scale_x_continuous(breaks = seq(x_min, x_max, by = 5)) +
    labs(title = title, x = "Year", y = paste0(title, " (Share)")) +
    geom_text(data = data.frame(Year = x_box, Share = y_box, lab = label_lines),
              aes(x = Year, y = Share, label = lab),
              hjust = 0, vjust = 1, family = "Times New Roman", size = 3) +
    .self$.theme_ieee()
}
,

    .plot_mcp = function(agg, fit_stats) .self$.build_share_plot(agg, "MCP_share", "MCP Share Over Time", fit_stats),
    .plot_scp = function(agg, fit_stats) .self$.build_share_plot(agg, "SCP_share", "SCP Share Over Time", fit_stats),

    .plot_combined = function(agg) {
  df_long <- tidyr::pivot_longer(
    agg, cols = c("MCP_share", "SCP_share"),
    names_to = "Type", values_to = "Share"
  )

  y_min <- max(0, min(df_long$Share, na.rm = TRUE))
  y_max <- min(1, max(df_long$Share, na.rm = TRUE))

  ggplot(df_long, aes(x = Year, y = Share, color = Type, linetype = Type)) +
    geom_point(size = 1.5, shape = 1) +
    geom_line() +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(y_min, y_max)) +
    scale_x_continuous(breaks = seq(min(df_long$Year), max(df_long$Year), by = 5)) +
    scale_color_manual(values = c("MCP_share" = "black", "SCP_share" = "gray40"),
                       labels = c("MCP Share", "SCP Share")) +
    scale_linetype_manual(values = c("MCP_share" = "dashed", "SCP_share" = "solid")) +
    labs(title = "MCP vs SCP Shares Over Time", x = "Year", y = "Share of Publications") +
    .self$.theme_ieee()
}
,

    # ----------------------------- Public API ------------------------------ #
    run = function() {
      agg <- .self$.build_year_series()
      if (nrow(agg) < 2) stop("[M4_M3] Not enough data")

      mcp_fit <- .self$.fit_trend(agg$MCP_share, agg$Year)
      scp_fit <- .self$.fit_trend(agg$SCP_share, agg$Year)

      .self$results <- list(data = agg, MCP = mcp_fit, SCP = scp_fit)

      .self$plots$MCP      <- .self$.plot_mcp(agg, mcp_fit)
      .self$plots$SCP      <- .self$.plot_scp(agg, scp_fit)
      .self$plots$Combined <- .self$.plot_combined(agg)

      invisible(.self)
    },

    save_json = function(out_dir,
                         filename = "M4_M3_MCP_Share_Over_Time.json",
                         pretty = TRUE) {
      if (length(.self$results) == 0) stop("[M4_M3] run() must be called before save_json().")
      .self$.safe_dir(out_dir)

      res <- .self$results
      data_tbl <- res$data
      res$data <- NULL

      json_path <- file.path(out_dir, filename)
      writeLines(jsonlite::toJSON(res, pretty = pretty, auto_unbox = TRUE), con = json_path)

      json_path_data <- file.path(out_dir, gsub("\\.json$", "_series.json", filename))
      writeLines(jsonlite::toJSON(data_tbl, pretty = pretty, dataframe = "rows", na = "null"), con = json_path_data)

      invisible(.self)
    },

    save_plots = function(out_dir, filename_base = "M4_M3_Share", width_cm = 8.9, height_cm = 6, dpi = 600) {
      .self$.safe_dir(out_dir)
      w_in <- width_cm / 2.54; h_in <- height_cm / 2.54
      for (name in names(.self$plots)) {
        for (ext in c("png", "svg")) {
          ggsave(
            filename = file.path(out_dir, paste0(filename_base, "_", name, ".", ext)),
            plot = .self$plots[[name]],
            width = w_in, height = h_in, dpi = dpi, units = "in", device = ext
          )
        }
      }
    }
  )
)
