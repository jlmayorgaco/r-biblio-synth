
# ============================================================================ #
# M5_Institutions: Slim Orchestrator (all heavy logic in helpers__m5_institutions)
# ============================================================================ #
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(jsonlite)
  library(igraph); library(ineq); library(DescTools); library(text2vec)
  library(vars); library(lmtest); library(WaveletComp); library(broom)
  library(scales); library(purrr)
})

# --- Load helpers (mirror of M3_Countries but for institutions) ---------------
source(file.path(root, "helpers/helpers__m5_institutions/", "00_loader.r"))
m5i_source_helpers(file.path(root, "helpers/helpers__m5_institutions/"))

M5_Institutions <- R6::R6Class(
  "M5_Institutions",
  inherit = M0_Abstract_Module,

  public = list(
    df = NULL,
    M_biblio = NULL,
    institution_col = NULL,
    year_col = NULL,
    citation_col = NULL,
    results = list(),
    plots = list(),
    path_results_jsons = NULL,
    path_results_plots = NULL,
    cfg = NULL,
    meta_institutions = NULL,   # optional dictionary for aliasing/normalization

    initialize = function(df,
                          M_biblio,
                          institution_col = "Affiliations",  # column with institution strings / arrays
                          year_col        = "Year",
                          citation_col    = "Times Cited",
                          report_path     = "results2/M5",
                          ...) {
      super$initialize(input = df, module_index = "M5")

      self$df                  <- as.data.frame(df)
      self$M_biblio            <- M_biblio
      self$institution_col     <- institution_col
      self$year_col            <- year_col
      self$citation_col        <- citation_col
      self$results             <- list()
      self$plots               <- list()
      self$cfg                 <- m5i_default_cfg()
      self$cfg$report_path     <- report_path
      self$path_results_jsons  <- file.path(report_path, "jsons")
      self$path_results_plots  <- file.path(report_path, "figures")
      if (!dir.exists(self$path_results_jsons)) dir.create(self$path_results_jsons, recursive = TRUE)
      if (!dir.exists(self$path_results_plots)) dir.create(self$path_results_plots, recursive = TRUE)

   
    },

    run = function() {
      message("=============== RUNNING M5 INSTITUTIONS ==================")

      df_docs <- m5i_prepare_documents(
        df             = self$df,
        M_biblio       = self$M_biblio,      # <-- pass it through
        inst_alias     = self$meta_institutions,  # supports ALIAS/NORMALIZED or alias/canonical
        inst_col_guess = self$institution_col,
        year_col       = self$year_col,
        citation_col   = self$citation_col
      )
      self$df <- df_docs

      # 2) Documents → institution-year aggregate (TP/TC, optional h-index etc.)
      inst_year <- m5i_aggregate_documents_to_inst_year(
        df             = df_docs,
        inst_list_col  = "Institution_List",
        year_col       = self$year_col,        # o NULL para autodetectar
        citation_col   = self$citation_col     # o NULL si no hay
      )

      self$results$inst_year <- inst_year


      print(' ')
      print(' ')
      print(' ')
      print(' inst_year <- m5i_expand_institution_year ')
      print(' ')
      print(head(inst_year))
      print(' ')
      print(' ')
      stop(' ===========')


      # 3) Indicators by institution + inequality metrics
      self$results$indicators <- m5i_compute_indicators(df_iy)
      self$results$inequality <- m5i_compute_inequality(self$results$indicators)

      # 4) Inequality trends (Gini vs Year) + fits
      gt <- m5i_build_gini_timeseries(df_iy)
      df_tp_gini_vs_years <- gt$tp %>% dplyr::rename(Year = year, Value = Gini_TP)
      fit_tp <- fit_models(df_tp_gini_vs_years)
      self$results$gini_tp_fit <- list(best_name = fit_tp$best_name, r2 = fit_tp$best_r2)
      self$plots$p5_tp_gini_vs_year <- plot_regression(
        df_tp_gini_vs_years, fit_tp$best_model, fit_tp$best_name,
        title = "Institutional Gini (TP) vs. Year"
      )

      df_tc_gini_vs_years <- gt$tc %>% dplyr::rename(Year = year, Value = Gini_TC)
      fit_tc <- fit_models(df_tc_gini_vs_years)
      self$results$gini_tc_fit <- list(best_name = fit_tc$best_name, r2 = fit_tc$best_r2)
      self$plots$p5_tc_gini_vs_year <- plot_regression(
        df_tc_gini_vs_years, fit_tc$best_model, fit_tc$best_name,
        title = "Institutional Gini (TC) vs. Year"
      )

      # 5) MCP share over time at institution level (optional; uses helpers if defined)
      if (exists("m5i_build_mcp_timeseries")) {
        mcp_ts <- m5i_build_mcp_timeseries(df_iy)
        df_mcp_share <- mcp_ts %>% dplyr::transmute(Year, Value = MCP_share_pct)
        fit_mcp <- fit_models(df_mcp_share)
        self$results$mcp_share_fit <- list(best_name = fit_mcp$best_name, r2 = fit_mcp$best_r2)
        self$plots$p5_mcp_share_vs_year <- plot_regression(
          df_mcp_share, fit_mcp$best_model, fit_mcp$best_name,
          xlab = "Year", ylab = "% Share", title = "Global Institutional MCP Share vs. Year"
        )
      }

      # 6) Top institutions (bars) + Lorenz
      ind <- self$results$indicators
      self$plots$p5_tp_bars <- bar_plot_topn(
        ind, "TP", top_n = 15, fill_color = "grey60",
        y_label = "Total Publications", title = "Top 15 Institutions by TP"
      )
      self$plots$p5_tc_bars <- bar_plot_topn(
        ind, "TC", top_n = 15, fill_color = "grey40",
        y_label = "Total Citations", title = "Top 15 Institutions by TC"
      )
      self$plots$p5_tp_lorenz <- lorenz_plot(ind$TP, "Publications (Institutions)")
      self$plots$p5_tc_lorenz <- lorenz_plot(ind$TC, "Citations (Institutions)")

      # 7) Institution profiles (optional small multiples per institution)
      if (exists("m5i_build_institution_plot_entries")) {
        entries <- m5i_build_institution_plot_entries(df_iy)
        m5i_save_institution_plots_ieee(
          entries, out_dir = file.path(self$path_results_plots, "institutions"),
          width_in = self$cfg$ieee$width_in,
          height_in = self$cfg$ieee$height_in,
          dpi = self$cfg$ieee$dpi
        )
      }

      # 8) Quadrants (productivity vs impact, etc.)
      if (exists("m5i_build_quadrants")) {
        qb <- m5i_build_quadrants(df_iy, self$cfg$quadrants)
        m5i_save_quadrants(qb, self$cfg$quadrants$out_dir,
                           w_in = self$cfg$quadrants$w_in,
                           h_in = self$cfg$quadrants$h_in,
                           dpi  = self$cfg$quadrants$dpi)
        self$results$quad_metrics <- qb$metrics
        self$results$quad_data    <- qb$data
      }

      # 9) Collaboration network (institution–institution)
      if (exists("m5i_build_collab_network")) {
        cc <- m5i_build_collab_network(df_docs = self$df,
                                       inst_col = self$institution_col,
                                       cfg = self$cfg$collab)
        cfg_used <- m5i_reconstruct_cc_cfg(self$institution_col, self$cfg$collab)
        comm_payload <- cc_summarize_communities(
          cc = cc, cfg = cfg_used,
          country_col = NULL,          # not used here
          sep_regex   = cfg_used$sep_regex,
          label_field = "institution"
        )
        m5i_save_collab_plot(cc, self$cfg$quadrants$out_dir, self$cfg$collab)

        self$results$institution_clusters <- list(
          communities = comm_payload,
          metrics     = cc$metrics,
          nodes       = cc$nodes,
          edges       = cc$edges
        )
      }

      # 10) Themes (optional reuse of M3 theme builder on institution tag space)
      if (exists("m5i_build_themes")) {
        self$results$themes <- m5i_build_themes(self$df)
      }

      # 11) (Optional) Institution size/sector breakdowns if helpers exist
      if (exists("m5i_plot_sector_breakdowns")) {
        self$plots$p5_sector <- m5i_plot_sector_breakdowns(df_iy, out_base = file.path(self$path_results_plots, "institutions", "sector_breakdown"))
      }

      invisible(TRUE)
    },

    save_json = function(out_dir = self$path_results_jsons) {
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      out_path <- file.path(out_dir, "M5_Institutions.json")
      jsonlite::write_json(self$results, out_path, auto_unbox = TRUE, pretty = TRUE)
      message("[M5] Saved JSON at: ", out_path)
    },

    save_plots = function(out_dir = self$path_results_plots) {
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      for (nm in names(self$plots)) {
        if (inherits(self$plots[[nm]], "gg")) {
          ggplot2::ggsave(file.path(out_dir, paste0("M5_", nm, ".png")),
                          plot = self$plots[[nm]], width = 7, height = 5, dpi = 600, bg = "white")
          ggplot2::ggsave(file.path(out_dir, paste0("M5_", nm, ".svg")),
                          plot = self$plots[[nm]], width = 7, height = 5, dpi = 600, bg = "white")
        }
      }
      message("[M5] Saved plots at: ", out_dir)
    }
  )
)
