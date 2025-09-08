# ============================================================================ #
# M3_Countries: Advanced Country-Level Scientometric Analysis (R6 version)
# ============================================================================ #
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(jsonlite)
  library(igraph); library(ineq); library(DescTools); library(text2vec)
  library(vars); library(lmtest); library(WaveletComp); library(broom)
  library(scales); library(purrr)
})

M3_Countries <- R6::R6Class(
  "M3_Countries",
  inherit = M0_Abstract_Module,

  public = list(

    # ---------------------------------------------------------------------- #
    # Fields
    # ---------------------------------------------------------------------- #
    df                 = NULL,
    country_col        = NULL,
    year_col           = NULL,
    citation_col       = NULL,
    results            = list(),
    plots              = list(),
    path_results_jsons = NULL,
    path_results_plots = NULL,

    # ---------------------------------------------------------------------- #
    # Constructor
    # ---------------------------------------------------------------------- #
    initialize = function(df,
                          country_col = "Country_Array",
                          year_col    = "Year",
                          citation_col= "Times Cited",
                          report_path = "results/M3_Countries",
                          ...) {
      super$initialize(input = df, module_index = "M3")
      self$df <- as.data.frame(df)
      self$country_col <- country_col
      self$year_col <- year_col
      self$citation_col <- citation_col
      self$results <- list(); self$plots <- list()
      self$path_results_jsons <- file.path(report_path, "jsons")
      self$path_results_plots <- file.path(report_path, "figures")
      if (!dir.exists(self$path_results_jsons)) dir.create(self$path_results_jsons, recursive=TRUE)
      if (!dir.exists(self$path_results_plots)) dir.create(self$path_results_plots, recursive=TRUE)
    },

    # ---------------------------------------------------------------------- #
    # Run full analysis
    # ---------------------------------------------------------------------- #
    run = function() {
      message("=============== RUNNING M3 COUNTRIES ==================")

      df <- self$prepare_data()
      self$results$indicators <- self$compute_indicators(df)
      self$results$inequality <- self$compute_inequality(self$results$indicators)

      ind <- self$results$indicators

      # Bars
      self$plots$p1_tp_bars <- bar_plot_topn(ind, "TP", top_n = 10,
        fill_color="steelblue", y_label="Total Publications", title="Top 10 Countries by TP")
      self$plots$p1_tc_bars <- bar_plot_topn(ind, "TC", top_n = 10,
        fill_color="darkred", y_label="Total Citations", title="Top 10 Countries by TC")

      # Lorenz
      self$plots$p1_tp_gini <- lorenz_plot(ind$TP, "Publications")
      self$plots$p1_tc_gini <- lorenz_plot(ind$TC, "Citations")

      # Global Gini vs Year
      gt <- build_gini_timeseries(df)
      df_tp_gini_vs_years <- gt$tp %>% dplyr::rename(Year = year, Value = Gini_TP)
      fit_tp <- fit_models(df_tp_gini_vs_years)
      self$results$gini_tp_fit <- list(best_name = fit_tp$best_name, r2 = fit_tp$best_r2)
      self$plots$p1_tp_gini_vs_year <- plot_regression(
        df_tp_gini_vs_years, fit_tp$best_model, fit_tp$best_name,
        title = "Global Gini (TP) vs. Year"
      )

      df_tc_gini_vs_years <- gt$tc %>% dplyr::rename(Year = year, Value = Gini_TC)
      fit_tc <- fit_models(df_tc_gini_vs_years)
      self$results$gini_tc_fit <- list(best_name = fit_tc$best_name, r2 = fit_tc$best_r2)
      self$plots$p1_tc_gini_vs_year <- plot_regression(
        df_tc_gini_vs_years, fit_tc$best_model, fit_tc$best_name,
        title = "Global Gini (TC) vs. Year"
      )

      # Global MCP share vs Year
      mcp_ts <- build_mcp_timeseries(df)  # uses country-year aggregated df
      df_mcp_share <- mcp_ts %>% dplyr::transmute(Year, Value = MCP_share_pct)
      fit_mcp <- fit_models(df_mcp_share)
      self$results$mcp_share_fit <- list(best_name = fit_mcp$best_name, r2 = fit_mcp$best_r2)
      self$plots$p1_mcp_share_vs_year <- plot_regression(
        df_mcp_share, fit_mcp$best_model, fit_mcp$best_name,
        xlab = "Year", ylab = "% Share", title = "Global MCP Share vs. Year"
      )

      # Per-country series with fits + PNGs
      pc <- private$make_country_timeseries_plots(df)
      #self$results$per_country_fits <- pc$fits
      #self$plots$per_country_plots  <- pc$plots
      

      #Network Relationship / Clusters

      #Quadtrans TP,TC
      #Quadrants SCP,MCP

      

      #Keywords/Thematic by Countries


      invisible(self)
    },

    # ---------------------------------------------------------------------- #
    # Data preparation
    # ---------------------------------------------------------------------- #
    prepare_data = function() {
      message("[M3][prepare] Starting data preparation...")
      df <- self$df

      keep_cols <- c("Year","Times_Cited","Country_List","Author_Keywords",
                     "Indexed_Keywords","Abstract","Title")
      df <- df %>% dplyr::select(dplyr::any_of(keep_cols))
      message("[M3][prepare] Kept columns: ", paste(names(df), collapse=", "))

      df <- df %>% dplyr::mutate(
        citations = suppressWarnings(as.numeric(.data[["Times_Cited"]])),
        year      = suppressWarnings(as.integer(.data[["Year"]]))
      )

      # Per-document country list + flags
      df_docs <- df %>%
        dplyr::mutate(
          Country_List = gsub('c\\(|\\)|"', "", .data[["Country_List"]]),
          Country_List = gsub(",", ";", Country_List),
          .countries   = purrr::map(
            .data[["Country_List"]],
            ~ {
              if (is.null(.x)) return(character(0))
              vec <- as.character(.x)
              if (length(vec) == 1 && !is.na(vec)) vec <- strsplit(vec, "\\s*[,;|]\\s*")[[1]]
              vec <- vec[!is.na(vec) & nzchar(vec)]
              vec <- trimws(vec)
              unique(vec)
            }
          ),
          .n_countries = purrr::map_int(.countries, length),
          mcp_flag     = .n_countries >= 2,
          scp_flag     = .n_countries == 1
        )

      self$results$docs_raw <- df_docs

      # Expand to country-level rows
      df_expanded <- df_docs %>%
        tidyr::unnest_longer(.countries, values_to = "country", keep_empty = FALSE) %>%
        dplyr::mutate(country = trimws(country)) %>%
        dplyr::filter(!is.na(country) & country != "")

      # Aggregate by country-year
      df_grouped <- df_expanded %>%
        dplyr::group_by(country, year) %>%
        dplyr::summarise(
          TP = dplyr::n(),
          TC = sum(citations, na.rm = TRUE),
          MCP = sum(mcp_flag, na.rm = TRUE),
          SCP = sum(scp_flag, na.rm = TRUE),
          Keywords = {
            kw <- c(.data[["Author_Keywords"]], .data[["Indexed_Keywords"]])
            kw <- unlist(strsplit(kw, "\\s*;\\s*"))
            kw <- unique(trimws(kw[!is.na(kw) & nzchar(kw)]))
            paste(kw, collapse = "; ")
          },
          Titles    = paste(na.omit(.data[["Title"]]),    collapse = " || "),
          Abstracts = paste(na.omit(.data[["Abstract"]]), collapse = " || "),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          MCP_ratio = dplyr::if_else(TP > 0, MCP / TP, NA_real_),
          SCP_ratio = dplyr::if_else(TP > 0, SCP / TP, NA_real_)
        ) %>%
        dplyr::arrange(country, year)

      message("[M3][prepare] Aggregated rows (country-year): ", nrow(df_grouped))
      df_grouped
    },

    # ---------------------------------------------------------------------- #
    # Indicators
    # ---------------------------------------------------------------------- #
    compute_indicators = function(df) {
      agg <- df %>%
        dplyr::group_by(country) %>%
        dplyr::summarise(
          TP = sum(TP, na.rm=TRUE),
          TC = sum(TC, na.rm=TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(CPP = ifelse(TP > 0, TC/TP, 0))

      total_tp <- sum(agg$TP); total_tc <- sum(agg$TC)

      agg %>%
        dplyr::mutate(
          RCA = (TP / total_tp) / mean(TP / total_tp),
          RCR = (TC / total_tc) / mean(TC / total_tc)
        )
    },

    # ---------------------------------------------------------------------- #
    # Inequality metrics
    # ---------------------------------------------------------------------- #
    compute_inequality = function(indicators) {
      list(
        gini_tp  = Gini(indicators$TP),
        gini_tc  = Gini(indicators$TC),
        theil_tp = Theil(indicators$TP),
        theil_tc = Theil(indicators$TC),
        hhi_tp   = sum((indicators$TP / sum(indicators$TP))^2),
        hhi_tc   = sum((indicators$TC / sum(indicators$TC))^2)
      )
    },

    # ---------------------------------------------------------------------- #
    # Network / Dynamics / Coop (unchanged)
    # ---------------------------------------------------------------------- #
    analyze_network = function(df) {  },
    analyze_dynamics = function(df) { },
    analyze_cooperation_factors = function(df) { list(message="...") },

    # ---------------------------------------------------------------------- #
    # Plot exporters
    # ---------------------------------------------------------------------- #
    generate_plots = function(df) {

    },

    save_json = function(out_dir) {
      out_path <- file.path(out_dir, "M3_Countries.json")
      jsonlite::write_json(self$results, out_path, auto_unbox=TRUE, pretty=TRUE)
      message("[M3] Saved JSON at: ", out_path)
    },

    save_plots = function(out_dir) {
      for (nm in names(self$plots)) {
        ggsave(file.path(out_dir, paste0("M3_", nm, ".png")),
               plot=self$plots[[nm]], width=7, height=5, dpi=600)
        ggsave(file.path(out_dir, paste0("M3_", nm, ".svg")),
               plot=self$plots[[nm]], width=7, height=5, dpi=600)
      }
      message("[M3] Saved plots at: ", out_dir)
    }
  ),

  # =======================================================================
  # PRIVATE
  # =======================================================================
  private = list(

    # Per-country time series plots (TP, TC, MCP%)
    make_country_timeseries_plots = function(
      df,
      countries    = NULL,           # optional subset of country names
      top_n        = NULL,           # optional: keep top-N countries by total TP
      metric_for_top = c("TP","TC"), # which metric to rank for top_n
      save_png     = TRUE,
      width_px     = 1050,
      height_px    = 590,
      dpi          = 300
    ) {
      out_dir <- self$path_results_plots
      metric_for_top <- match.arg(metric_for_top)

      # Prep data
      df_cy <- df %>%
        dplyr::mutate(
          Year    = as.integer(year),
          MCP_pct = dplyr::if_else(TP > 0, 100 * MCP / TP, NA_real_)
        ) %>%
        dplyr::arrange(country, Year)

      # Filter by explicit list if provided
      if (!is.null(countries)) {
        df_cy <- df_cy %>% dplyr::filter(country %in% countries)
      }

      # Optionally keep top-N countries by total TP/TC across years
      if (!is.null(top_n) && is.numeric(top_n) && top_n > 0) {
        rank_tbl <- df_cy %>%
          dplyr::group_by(country) %>%
          dplyr::summarise(score = sum(.data[[metric_for_top]], na.rm = TRUE), .groups = "drop") %>%
          dplyr::arrange(dplyr::desc(score)) %>%
          dplyr::slice_head(n = top_n)
        df_cy <- df_cy %>% dplyr::filter(country %in% rank_tbl$country)
      }

      # containers
      plots_list <- list()
      fits_list  <- list()

      # util: safe file name
      safe_name <- function(x) {
        x <- gsub("[^A-Za-z0-9_\\-]+", "_", x)
        x <- gsub("_+", "_", x)
        trimws(x)
      }

      # iterate per country
      for (cc in unique(df_cy$country)) {
        dcc <- df_cy %>% dplyr::filter(country == cc)
        # save PNGs
        cc_file <- safe_name(cc)
        plots_list.append(plot_scatter(dcc))
      }

      list(plots = plots_list, fits = fits_list)
    }

  )
)

# --------------------------------------------------------------------------- #
# Helpers (outside the class)
# --------------------------------------------------------------------------- #

# Build time series of global Gini per year from country-year data
build_gini_timeseries <- function(df) {
  # ensure columns
  if (!all(c("country","year","TP") %in% names(df))) {
    stop("build_gini_timeseries: df must contain country, year, TP (and TC if available).")
  }
  if (!("TC" %in% names(df))) {
    # derive TC from citations if needed
    if ("citations" %in% names(df)) {
      df <- df %>%
        dplyr::group_by(country, year) %>%
        dplyr::summarise(TP = dplyr::n(),
                         TC = sum(ifelse(is.na(citations), 0, citations), na.rm=TRUE),
                         .groups="drop")
    } else {
      df <- df %>% dplyr::mutate(TC = 0)
    }
  } else {
    df <- df %>% dplyr::select(country, year, TP, TC) %>% dplyr::distinct()
  }

  ts_tp <- df %>% dplyr::group_by(year) %>%
    dplyr::summarise(Gini_TP = ineq::Gini(TP), .groups="drop")
  ts_tc <- df %>% dplyr::group_by(year) %>%
    dplyr::summarise(Gini_TC = ineq::Gini(TC), .groups="drop")

  list(tp = ts_tp, tc = ts_tc)
}

# Global MCP/SCP time series from country–year aggregated df
build_mcp_timeseries <- function(df, year_col = "year") {
  stopifnot(is.data.frame(df))
  if (!year_col %in% names(df)) stop(sprintf("build_mcp_timeseries: year column '%s' not found.", year_col))
  if (!all(c("MCP","SCP","TP") %in% names(df))) stop("build_mcp_timeseries: df must contain MCP, SCP, TP.")

  df %>%
    dplyr::group_by(.data[[year_col]]) %>%
    dplyr::summarise(
      MCP_docs   = sum(.data$MCP, na.rm=TRUE),
      SCP_docs   = sum(.data$SCP, na.rm=TRUE),
      Total_docs = sum(.data$TP,  na.rm=TRUE),
      .groups="drop"
    ) %>%
    dplyr::mutate(
      Year          = suppressWarnings(as.integer(as.character(.data[[year_col]]))),
      MCP_share     = ifelse(Total_docs > 0, MCP_docs / Total_docs, NA_real_),
      MCP_share_pct = 100 * MCP_share
    ) %>%
    dplyr::select(Year, MCP_docs, SCP_docs, Total_docs, MCP_share, MCP_share_pct) %>%
    dplyr::arrange(Year)
}
