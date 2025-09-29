# ============================================================================ #
# M3_Countries: Slim Orchestrator (all heavy logic in helpers__m3_countries)
# ============================================================================ #
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(jsonlite)
  library(igraph); library(ineq); library(DescTools); library(text2vec)
  library(vars); library(lmtest); library(WaveletComp); library(broom)
  library(scales); library(purrr)
})

source(file.path(root, "helpers/helpers__m3_countries/", "00_loader.r"))
m3c_source_helpers(file.path(root, "helpers/helpers__m3_countries/"))

M3_Countries <- R6::R6Class(
  "M3_Countries",
  inherit = M0_Abstract_Module,

  public = list(
    df = NULL,
    country_col = NULL,
    year_col = NULL,
    citation_col = NULL,
    results = list(),
    plots = list(),
    path_results_jsons = NULL,
    path_results_plots = NULL,
    cfg = NULL,
    meta_countries = NULL,

    initialize = function(df,M_biblio = NULL,
                          country_col  = "Country_Array",
                          year_col     = "Year",
                          citation_col = "Times Cited",
                          report_path  = "results2/M3",
                          ...) {
      # load helpers
      
      super$initialize(input = df, module_index = "M3")

      self$df                 <- as.data.frame(df)
      self$country_col        <- country_col
      self$year_col           <- year_col
      self$citation_col       <- citation_col
      self$results            <- list()
      self$plots              <- list()
      self$cfg                <- m3c_default_cfg()
      self$cfg$report_path    <- report_path
      self$path_results_jsons <- file.path(report_path, "jsons")
      self$path_results_plots <- file.path(report_path, "figures")
      if (!dir.exists(self$path_results_jsons)) dir.create(self$path_results_jsons, recursive = TRUE)
      if (!dir.exists(self$path_results_plots)) dir.create(self$path_results_plots, recursive = TRUE)

      # load NAME→CONTINENT mapping
      countries_csv = here::here("data", "countries.csv")
      self$meta_countries <- m3c_country_meta_read(countries_csv)

      if (is.null(self$meta_countries)) {
        stop(sprintf(
          "[M3] countries.csv could not be loaded. Expected at: %s\nWorking dir: %s\nFix the path or file (needs NAME, CONTINENT).",
          self$cfg$countries_csv, getwd()
        ))
      }
    },

    run = function() {
      message("=============== RUNNING M3 COUNTRIES ==================")

      # 1) Documents → enrich (adds Continent_List) and persist in self$df
      df_docs <- m3c_prepare_documents(self$df, self$meta_countries, country_col_guess = self$country_col)
      self$df <- df_docs

      # 2) Documents → country-year aggregate (+ continent)
      df_cy <- m3c_expand_country_year(df_docs)
      df_cy <- m3c_add_continent_to_grouped(df_cy, self$meta_countries)

      # 3) Indicators + inequality
      self$results$indicators <- m3c_compute_indicators(df_cy)
      self$results$inequality <- m3c_compute_inequality(self$results$indicators)

      # 4) Inequality trends + fits (Gini vs Year)
      gt <- m3c_build_gini_timeseries(df_cy)
      df_tp_gini_vs_years <- gt$tp %>% dplyr::rename(Year = year, Value = Gini_TP)
      fit_tp <- fit_models(df_tp_gini_vs_years)
      self$results$gini_tp_fit <- list(best_name = fit_tp$best_name, r2 = fit_tp$best_r2)
      self$plots$p1_tp_gini_vs_year <- plot_regression(df_tp_gini_vs_years, fit_tp$best_model, fit_tp$best_name,
                                                       title = "Global Gini (TP) vs. Year")

      df_tc_gini_vs_years <- gt$tc %>% dplyr::rename(Year = year, Value = Gini_TC)
      fit_tc <- fit_models(df_tc_gini_vs_years)
      self$results$gini_tc_fit <- list(best_name = fit_tc$best_name, r2 = fit_tc$best_r2)
      self$plots$p1_tc_gini_vs_year <- plot_regression(df_tc_gini_vs_years, fit_tc$best_model, fit_tc$best_name,
                                                       title = "Global Gini (TC) vs. Year")

      # 5) Global MCP share vs Year
      mcp_ts <- m3c_build_mcp_timeseries(df_cy)
      df_mcp_share <- mcp_ts %>% dplyr::transmute(Year, Value = MCP_share_pct)
      fit_mcp <- fit_models(df_mcp_share)
      self$results$mcp_share_fit <- list(best_name = fit_mcp$best_name, r2 = fit_mcp$best_r2)
      self$plots$p1_mcp_share_vs_year <- plot_regression(
        df_mcp_share, fit_mcp$best_model, fit_mcp$best_name,
        xlab = "Year", ylab = "% Share", title = "Global MCP Share vs. Year"
      )

      # 6) Top bars + Lorenz
      ind <- self$results$indicators
      self$plots$p1_tp_bars <- bar_plot_topn(ind, "TP", top_n = 10, fill_color = "steelblue",
                                             y_label = "Total Publications", title = "Top 10 Countries by TP")
      self$plots$p1_tc_bars <- bar_plot_topn(ind, "TC", top_n = 10, fill_color = "darkred",
                                             y_label = "Total Citations", title = "Top 10 Countries by TC")
      self$plots$p1_tp_gini <- lorenz_plot(ind$TP, "Publications")
      self$plots$p1_tc_gini <- lorenz_plot(ind$TC, "Citations")

      # 7) Age-adjustment + per-country plots
      df_cy_norm <- m3c_add_age_adjustments(df_cy)
      entries    <- m3c_build_country_plot_entries(df_cy_norm)
      m3c_save_country_plots_ieee(entries, self$path_results_plots,
                                  width_in = self$cfg$ieee$width_in,
                                  height_in = self$cfg$ieee$height_in,
                                  dpi = self$cfg$ieee$dpi)

      # 8) Quadrants
      qb <- m3c_build_quadrants(df_cy, self$cfg$quadrants)
      m3c_save_quadrants(qb, self$cfg$quadrants$out_dir,
                         w_in = self$cfg$quadrants$w_in,
                         h_in = self$cfg$quadrants$h_in,
                         dpi  = self$cfg$quadrants$dpi)
      self$results$quad_metrics <- qb$metrics
      self$results$quad_data    <- qb$data

      # 9) Collab network
      cc <- m3c_build_collab_network(df_docs = self$df, self_country_col = self$country_col, cfg = self$cfg$collab)
      cfg_used <- m3c_reconstruct_cc_cfg(self$country_col, self$cfg$collab)
      comm_payload <- cc_summarize_communities(cc = cc, cfg = cfg_used,
                                               country_col = cfg_used$country_col, sep_regex = cfg_used$sep_regex)
      m3c_save_collab_plot(cc, self$cfg$quadrants$out_dir, self$cfg$collab)

      self$results$country_clusters <- list(
        communities = comm_payload,
        metrics = cc$metrics,
        nodes   = cc$nodes,
        edges   = cc$edges
      )

      # 10) Themes
      self$results$themes_9_1 <- m3c_build_themes(self$df)

      # --- 11) By Continents ---------------------------------------------------
      cont_fig_dir <- file.path(self$path_results_plots, "continents")
      if (!dir.exists(cont_fig_dir)) dir.create(cont_fig_dir, recursive = TRUE)

      self$plots$continent_tp_totals <- m3c_plot_continent_totals(
        df_cy, value_col = "TP",
        title = "Publications by Continent (All Years)",
        out_base = file.path(cont_fig_dir, "continent_tp_totals")  # no extension
      )

      self$plots$continent_tc_totals <- m3c_plot_continent_totals(
        df_cy, value_col = "TC",
        title = "Citations by Continent (All Years)",
        out_base = file.path(cont_fig_dir, "continent_tc_totals")
      )

      cont_fig_dir <- file.path(self$path_results_plots, "continents")
      if (!dir.exists(cont_fig_dir)) dir.create(cont_fig_dir, recursive = TRUE)

      res <- m3c_plot_continent_share_trend(
        df_cy,
        periods  = m3c_period_labels(len1 = 5, len2 = 5),
        out_base = file.path(cont_fig_dir, "continent_share_trend") # saves _tp_share.* and _tc_share.*
      )
      grid_res <- m3c_continent_trends_grid_2x2(
        df_cy,
        periods  = m3c_period_labels(len1 = 5, len2 = 5),
        out_base = file.path(cont_fig_dir, "continent_trends_2x2"), # saves PNG+SVG
        kind     = "double",   # IEEE double-column width (7.16 in)
        aspect   = 0.5
      )

    },

    save_json = function(out_dir = self$path_results_jsons) {
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      out_path <- file.path(out_dir, "M3_Countries.json")
      jsonlite::write_json(self$results, out_path, auto_unbox = TRUE, pretty = TRUE)
      message("[M3] Saved JSON at: ", out_path)
    },

    save_plots = function(out_dir = self$path_results_plots) {
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      for (nm in names(self$plots)) {
        if (inherits(self$plots[[nm]], "gg")) {
          ggplot2::ggsave(file.path(out_dir, paste0("M3_", nm, ".png")),
                          plot = self$plots[[nm]], width = 7, height = 5, dpi = 600)
          ggplot2::ggsave(file.path(out_dir, paste0("M3_", nm, ".svg")),
                          plot = self$plots[[nm]], width = 7, height = 5, dpi = 600)
        }
      }
      message("[M3] Saved plots at: ", out_dir)
    }
  )
)
