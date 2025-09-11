# ============================================================================ #
# M3_Countries: Advanced Country-Level Scientometric Analysis (R6 version)
# Clean, DRY, KISS, SOLID refactor — same functionality
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
                          country_col  = "Country_Array",
                          year_col     = "Year",
                          citation_col = "Times Cited",
                          report_path  = "results/M3_Countries",
                          ...) {
      super$initialize(input = df, module_index = "M3")
      self$df                 <- as.data.frame(df)
      self$country_col        <- country_col
      self$year_col           <- year_col
      self$citation_col       <- citation_col
      self$results            <- list()
      self$plots              <- list()
      self$path_results_jsons <- file.path(report_path, "jsons")
      self$path_results_plots <- file.path(report_path, "figures")
      if (!dir.exists(self$path_results_jsons)) dir.create(self$path_results_jsons, recursive = TRUE)
      if (!dir.exists(self$path_results_plots)) dir.create(self$path_results_plots, recursive = TRUE)
    },

    # ---------------------------------------------------------------------- #
    # Orchestrate full analysis
    # ---------------------------------------------------------------------- #
    run = function() {
      message("=============== RUNNING M3 COUNTRIES ==================")

      # 1) Prep data (country-year aggregate with TP, TC, MCP, SCP, ratios)
      df_cy <- self$prepare_data()

      # 2) Indicators + inequality snapshots
      self$results$indicators <- self$compute_indicators(df_cy)
      self$results$inequality <- self$compute_inequality(self$results$indicators)

      # 3) Global inequality trends and fits (Gini vs Year)
      gt <- build_gini_timeseries(df_cy)

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

      # 4) Global MCP share vs Year (trend + fit)
      mcp_ts <- build_mcp_timeseries(df_cy)
      df_mcp_share <- mcp_ts %>% dplyr::transmute(Year, Value = MCP_share_pct)
      fit_mcp <- fit_models(df_mcp_share)
      self$results$mcp_share_fit <- list(best_name = fit_mcp$best_name, r2 = fit_mcp$best_r2)
      self$plots$p1_mcp_share_vs_year <- plot_regression(
        df_mcp_share, fit_mcp$best_model, fit_mcp$best_name,
        xlab = "Year", ylab = "% Share", title = "Global MCP Share vs. Year"
      )

      # 5) Top bars + Lorenz (keep existing functionality)
      ind <- self$results$indicators
      self$plots$p1_tp_bars <- bar_plot_topn(
        ind, "TP", top_n = 10, fill_color = "steelblue",
        y_label = "Total Publications", title = "Top 10 Countries by TP"
      )
      self$plots$p1_tc_bars <- bar_plot_topn(
        ind, "TC", top_n = 10, fill_color = "darkred",
        y_label = "Total Citations", title = "Top 10 Countries by TC"
      )
      self$plots$p1_tp_gini <- lorenz_plot(ind$TP, "Publications")
      self$plots$p1_tc_gini <- lorenz_plot(ind$TC, "Citations")

      # 6) Age-adjustment + per-country series plots (TP, TC, TC_adj, MCP_ratio)
      message(" ")
      message(" ====================== M3 COUNTRIES, PLOT BY COUNTRY =================== ")

      df_cy_norm <- private$add_age_adjustments(df_cy)       # adds age + TC_adj
      entries     <- private$build_country_plot_entries(df_cy_norm)

      # Save to IEEE single column size (PNG+SVG) using user's saver
      save_countries_plots_ieee(
        plots_by_countries = entries,
        root_out  = self$path_results_plots,
        width_in  = private$FIG_WIDTH_IN,
        height_in = private$FIG_HEIGHT_IN,
        dpi       = 300
      )

      # 7) Quadrants (TP–TC and SCP–MCP)
      qb <- quad_bubble_plot(
        df = df_cy,
        year_col = "year",
        country_col = "country",
        N_years = 5,
        top_n = 10,
        layout = "twocol",
        show_arrows = TRUE,
        zero_offset = 0.01,
        quadrant_alpha = 0.08,
        point_size = 2,
        arrow_vs_dot_mult = 2,
        add_metrics = TRUE
      )

      # Create output folder
      out_dir <- file.path("results2/M3", "quadrants")
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

      # IEEE-ish single-column width (182mm ≈ 7.16in). Height ~2:1 for readability.
      w_in <- 7.16
      h_in <- 3.6
      dpi  <- 600

      ggsave(file.path(out_dir, "quad_tp_tc.png"),   qb$plots$tp_tc,   width = w_in, height = h_in, units = "in", dpi = dpi)
      ggsave(file.path(out_dir, "quad_scp_mcp.png"), qb$plots$scp_mcp, width = w_in, height = h_in, units = "in", dpi = dpi)
      ggsave(file.path(out_dir, "quad_tp_tc.svg"),   qb$plots$tp_tc,   width = w_in, height = h_in, units = "in", device = "svg")
      ggsave(file.path(out_dir, "quad_scp_mcp.svg"), qb$plots$scp_mcp, width = w_in, height = h_in, units = "in", device = "svg")

      self$results$quad_metrics <- qb$metrics
      self$results$quad_data    <- qb$data

      # 8) International co-authorship (country collaboration) network — clusters
      message(" ")
      message(" ====================== M3 COUNTRIES, COLLAB NETWORK =================== ")

      # Params
      cc_country_col <- private$detect_country_col(self$df, preferred = self$country_col)
      cc_sep_regex   <- ";|,|\\|"
      cc_min_w       <- 2
      cc_min_deg     <- 2
      cc_label_top   <- 15
      cc_method      <- "louvain"   # or "leiden" if leidenbase installed
      cc_title       <- "International Co-authorship Network — Communities"

      # Build + plot via helper (returns plot, metrics, nodes, edges)
      cc <- country_cluster_plot(
        df = self$df,
        country_col = self$country_col,
        min_edge_weight = 2,
        min_degree = 2,
        label_top = 15,
        community_method = "louvain",
        title = "International Co-authorship Network — Communities",
        arrange = "auto",
        spread = 5,
        intra_scale = 4,
        top_k_legend = 5,
        base_size = 9
      )

      # Reconstruct the config you actually used (so the payload records it)
      cfg_used <- cc_config(
        country_col      = self$country_col,
        sep_regex        = ";|,|\\|",
        min_edge_weight  = 2,
        min_degree       = 2,
        label_top        = 15,
        community_method = "louvain",
        title            = "International Co-authorship Network — Communities",
        base_size        = 9,
        top_k_legend     = 5,
        spread           = 5,
        intra_scale      = 4,
        arrange          = "auto",
        legend_upper     = TRUE,
        seed             = 42
      )

      # Build the communities payload
      comm_payload <- cc_summarize_communities(
        cc          = cc,
        cfg         = cfg_used,
        country_col = cfg_used$country_col,
        sep_regex   = cfg_used$sep_regex
      )
   
      # Save (PNG + SVG) 
      out_png <- file.path(out_dir, "country_collab_clusters_w.png") 
      out_svg <- file.path(out_dir, "country_collab_clusters_w.svg") 
      ggplot2::ggsave(out_png, plot = cc$plot, width =  7.5, height = 7, units = "in", dpi = 600) 
      ggplot2::ggsave(out_svg, plot = cc$plot, width = 7.5, height = 7, units = "in", device = "svg")
      #ggplot2::ggsave(out_png, plot = cc$plot, width = 7.16, height = 5, units = "in", dpi = 600) 
      #ggplot2::ggsave(out_svg, plot = cc$plot, width = 7.16, height = 5, units = "in", device = "svg")
      
      self$results$country_clusters <- list(
        communities  = comm_payload,
        metrics = cc$metrics,
        nodes   = cc$nodes,
        edges   = cc$edges
      )


      # 9) (Reserved) Keywords/Thematic by Countries
        # 9.1 What are the main themes for each country in each years
      t9 <- themes_country_year(
        df            = self$df,
        country_col   = NULL,              # let it auto-detect (or set "Country_List"/"Country_Array")
        sep_regex     = ";|,|\\|",
        min_docs_cell = 5,                 # require at least 5 docs for (Country, Year)
        n_top         = 12,                # keep top 12 terms per cell
        use_tfidf     = TRUE               # rank themes by TF–IDF
      )

      # 2) Store in your results object
      self$results$themes_9_1 <- list(
        groups   = t9$groups,              # (Country, Year, n_docs)
        themes   = t9$themes,              # (Country, Year, term, n_docs_term, n_docs, share, tfidf)
        top      = t9$top,                 # top-K per (Country, Year)
        top_wide = t9$top_wide             # one row per cell with a concatenated list of top themes
      )

      # 3) Example: make a heatmap for one country
      p_us <- themes_country_heatmap(t9$themes, country = "United States", top_k_terms = 15)
      print(p_us)

        # 9.2 Based on the cooperation clusters, take the main theames that are similar in the cluster and differs with the other clusters.
        # 9.3 Based on the Clusters/Communities, make a stats tests about TP,TC,MCP,SCP in all years - Is there a grupal diffrerences? or not?

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
      message("[M3][prepare] Kept columns: ", paste(names(df), collapse = ", "))

      df <- df %>% dplyr::mutate(
        citations = suppressWarnings(as.numeric(.data[["Times_Cited"]])),
        year      = suppressWarnings(as.integer(.data[["Year"]]))
      )

      # Per-document countries & flags
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

      # Expand documents → country-level rows
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
    # Country-level indicators (TP/TC sums + CPP, RCA/RCR proxies)
    # ---------------------------------------------------------------------- #
    compute_indicators = function(df) {
      agg <- df %>%
        dplyr::group_by(country) %>%
        dplyr::summarise(
          TP = sum(TP, na.rm = TRUE),
          TC = sum(TC, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(CPP = ifelse(TP > 0, TC / TP, 0))

      total_tp <- sum(agg$TP)
      total_tc <- sum(agg$TC)

      agg %>%
        dplyr::mutate(
          RCA = (TP / total_tp) / mean(TP / total_tp),
          RCR = (TC / total_tc) / mean(TC / total_tc)
        )
    },

    # ---------------------------------------------------------------------- #
    # Inequality metrics (country totals)
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
    # Stubs (reserved for extensions)
    # ---------------------------------------------------------------------- #
    analyze_network = function(df) { },
    analyze_dynamics = function(df) { },
    analyze_cooperation_factors = function(df) { list(message = "...") },

    # ---------------------------------------------------------------------- #
    # Exporters
    # ---------------------------------------------------------------------- #
    save_json = function(out_dir = self$path_results_jsons) {
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      out_path <- file.path(out_dir, "M3_Countries.json")
      jsonlite::write_json(self$results, out_path, auto_unbox = TRUE, pretty = TRUE)
      message("[M3] Saved JSON at: ", out_path)
    },

    save_plots = function(out_dir = self$path_results_plots) {
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      for (nm in names(self$plots)) {
        # Only save ggplot objects here; base-graphics plots were already saved in run()
        if (inherits(self$plots[[nm]], "gg")) {
          ggplot2::ggsave(file.path(out_dir, paste0("M3_", nm, ".png")),
                          plot = self$plots[[nm]], width = 7, height = 5, dpi = 600)
          ggplot2::ggsave(file.path(out_dir, paste0("M3_", nm, ".svg")),
                          plot = self$plots[[nm]], width = 7, height = 5, dpi = 600)
        }
      }
      message("[M3] Saved plots at: ", out_dir)
    }
  ),

  # ======================================================================= #
  # PRIVATE
  # ======================================================================= #
  private = list(

    # ---------------------------- Constants -------------------------------- #
    FIG_WIDTH_IN  = 3.2,   # IEEE single-column size
    FIG_HEIGHT_IN = 1.8,

    # ------------------------- Utility helpers ----------------------------- #
    .or = function(a, b) if (is.null(a) || length(a) == 0) b else a,

    slugify = function(x) {
      x <- trimws(x)
      x <- gsub("[^A-Za-z0-9]+", "_", x)
      tolower(gsub("^_+|_+$", "", x))
    },

    as_year = function(x) as.integer(as.character(x)),

    year_breaks = function(y) {
      y <- sort(unique(private$as_year(y)))
      r <- range(y); span <- diff(r)
      if (span <= 10) {
        seq(r[1], r[2], 1L)
      } else if (span <= 35) {
        seq(floor(r[1] / 5) * 5,  ceiling(r[2] / 5) * 5,  5L)
      } else {
        seq(floor(r[1] / 10) * 10, ceiling(r[2] / 10) * 10, 10L)
      }
    },

    # -------------------- Age adjustments (TC_adj) -------------------------- #
    add_age_adjustments = function(df_cy) {
      current_year <- as.integer(format(Sys.Date(), "%Y"))
      df_cy %>%
        dplyr::mutate(
          year = private$as_year(year),
          age  = pmax(1L, current_year - year + 1L),
          TC_adj = ifelse(is.finite(TC), TC / age, NA_real_)
        )
    },

    # --------------- Build per-country plot entries (no save) --------------- #
    build_country_plot_entries = function(df_cy_norm) {
      stopifnot(is.data.frame(df_cy_norm))
      plots_by_countries <- list()
      idx <- 1L

      for (cname in unique(df_cy_norm$country)) {
        df_c <- df_cy_norm %>%
          dplyr::filter(country == cname) %>%
          dplyr::arrange(year)

        if (!nrow(df_c)) next

        df_c$year <- private$as_year(df_c$year)
        brks      <- private$year_breaks(df_c$year)
        slug      <- private$slugify(cname)
        out_dir   <- file.path("by_countries", slug)

        # --- TP ---
        p_tp <- plot_scatter(
          df = df_c, x_col = year, y_col = TP,
          title = paste0(cname, " — Total Publications (TP)"),
          xlabel = "Year", ylabel = "TP",
          x_breaks = brks, connect = "line"
        ) + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

        # --- TC ---
        p_tc <- plot_scatter(
          df = df_c, x_col = year, y_col = TC,
          title = paste0(cname, " — Total Citations (TC)"),
          xlabel = "Year", ylabel = "TC",
          x_breaks = brks, connect = "line"
        ) + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

        # --- TC_adj (citations per year) ---
        p_tc_adj <- plot_scatter(
          df = df_c, x_col = year, y_col = TC_adj,
          title = paste0(cname, " — Citations per Year (TC_adj)"),
          xlabel = "Year", ylabel = "TC per year",
          x_breaks = brks, connect = "line"
        ) + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

        # --- MCP ratio (0..1 → percentage axis) ---
        p_mcp <- plot_scatter(
          df = df_c, x_col = year, y_col = MCP_ratio,
          title = paste0(cname, " — MCP Ratio"),
          xlabel = "Year", ylabel = "MCP ratio",
          x_breaks = brks,
          y_percent = TRUE, y_percent_breaks = seq(0, 1, 0.2),
          y_limits = c(0, 1),
          connect = "line"
        ) + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

        # Queue records (country, metric, plot, relative filename)
        plots_by_countries[[idx]] <- list(
          country  = cname, metric = "TP", plot = p_tp,
          filename = file.path(out_dir, paste0(slug, "_tp.png"))
        ); idx <- idx + 1L

        plots_by_countries[[idx]] <- list(
          country  = cname, metric = "TC", plot = p_tc,
          filename = file.path(out_dir, paste0(slug, "_tc.png"))
        ); idx <- idx + 1L

        plots_by_countries[[idx]] <- list(
          country  = cname, metric = "TC_adj", plot = p_tc_adj,
          filename = file.path(out_dir, paste0(slug, "_tc_adj.png"))
        ); idx <- idx + 1L

        plots_by_countries[[idx]] <- list(
          country  = cname, metric = "MCP_ratio", plot = p_mcp,
          filename = file.path(out_dir, paste0(slug, "_mcp.png"))
        ); idx <- idx + 1L
      }

      plots_by_countries
    },

    # ---------------- Country column auto-detect ---------------------------- #
    detect_country_col = function(df, preferred = NULL) {
      if (!is.null(preferred) && preferred %in% names(df)) return(preferred)

      common <- c("Country_List", "Country_Array", "country_list", "country_array",
                  "Countries", "Country", "country", "countries")

      name_hits <- unique(c(common, names(df)[grepl("country", names(df), ignore.case = TRUE)]))
      candidates <- name_hits[name_hits %in% names(df)]
      if (!length(candidates)) {
        stop("[M3] No country list column found. Tried common names like Country_List / Country_Array. ",
             "Pass country_col='Country_List' (or correct name) to M3_Countries$new(...).")
      }

      score_col <- function(col) {
        x <- df[[col]]
        if (is.null(x)) return(-Inf)
        if (is.list(x)) {
          return(sum(vapply(x, function(v) length(na.omit(v)) > 0, logical(1))))
        } else {
          x <- as.character(x); x[is.na(x)] <- ""
          parts <- strsplit(x, "\\s*[,;|]\\s*")
          return(sum(vapply(parts, function(v) any(nzchar(trimws(v))), logical(1))))
        }
      }

      scores <- vapply(candidates, score_col, numeric(1))
      sel <- candidates[which.max(scores)]
      if (length(sel) == 0 || is.infinite(scores[which.max(scores)])) {
        stop("[M3] Could not find a usable country list column among: ",
             paste(candidates, collapse = ", "), ".")
      }
      sel
    },

    # ---------------- Country collaboration network builders ---------------- #
    .build_country_edges = function(df, country_col = "Country_Array", sep_regex = ";|,|\\|") {
      # Robust: if not found, auto-detect
      if (!country_col %in% names(df)) {
        country_col <- private$detect_country_col(df, preferred = country_col)
      }

      # Ensure per-row id for grouping
      if (!("PaperID" %in% names(df))) {
        df$PaperID <- seq_len(nrow(df))
      }

      norm <- df %>%
        mutate(.raw = .data[[country_col]]) %>%
        mutate(.raw = ifelse(is.na(.raw), "", .raw)) %>%
        mutate(.list = if (is.list(.raw)) .raw else strsplit(as.character(.raw), sep_regex)) %>%
        select(PaperID, .list) %>%
        tidyr::unnest_longer(.list, values_to = "Country") %>%
        mutate(Country = trimws(Country)) %>%
        filter(nchar(Country) > 0) %>%
        group_by(PaperID) %>%
        summarise(Countries = list(sort(unique(Country))), .groups = "drop") %>%
        filter(lengths(Countries) >= 2)

      comb2 <- function(x) {
        if (length(x) < 2) return(NULL)
        m <- t(combn(x, 2))
        data.frame(from = m[,1], to = m[,2], stringsAsFactors = FALSE)
      }

      edges <- norm %>%
        mutate(pairs = purrr::map(Countries, comb2)) %>%
        select(pairs) %>%
        tidyr::unnest(pairs) %>%
        group_by(from, to) %>%
        summarise(weight = dplyr::n(), .groups = "drop")

      edges
    },

    .build_country_graph = function(edges,
                                    min_edge_weight = 2,
                                    min_degree = 2,
                                    community_method = c("louvain","leiden")) {
      community_method <- match.arg(community_method)

      e <- edges %>% filter(weight >= min_edge_weight)
      if (nrow(e) == 0) {
        g <- igraph::make_empty_graph(directed = FALSE)
        return(list(graph = g,
                    metrics = list(n_nodes = 0, n_edges = 0, n_communities = 0,
                                   modularity = NA_real_, density = 0,
                                   avg_degree = 0, avg_wdegree = 0)))
      }

      g <- igraph::graph_from_data_frame(e, directed = FALSE)
      g <- igraph::simplify(g, remove.loops = TRUE, edge.attr.comb = list(weight = "sum"))

      deg <- igraph::degree(g)
      keep <- V(g)[deg >= min_degree]
      g <- igraph::induced_subgraph(g, vids = keep)

      # communities
      if (community_method == "leiden" && requireNamespace("leidenbase", quietly = TRUE)) {
        part <- leidenbase::leiden_find_partition(g, resolution_parameter = 1.0, seed = 42)
        comm <- part$membership
      } else {
        comm <- igraph::cluster_louvain(g, weights = E(g)$weight)$membership
      }
      V(g)$community <- as.integer(comm)

      V(g)$degree  <- igraph::degree(g)
      V(g)$wdegree <- igraph::strength(g, vids = V(g), mode = "all", weights = E(g)$weight)

      mods <- tryCatch(igraph::modularity(g, membership = V(g)$community, weights = E(g)$weight),
                       error = function(...) NA_real_)
      dens <- igraph::edge_density(g, loops = FALSE)

      metrics <- list(
        n_nodes       = igraph::vcount(g),
        n_edges       = igraph::ecount(g),
        n_communities = length(unique(V(g)$community)),
        modularity    = round(mods, 3),
        density       = round(dens, 5),
        avg_degree    = round(mean(V(g)$degree), 2),
        avg_wdegree   = round(mean(V(g)$wdegree), 2)
      )

      list(graph = g, metrics = metrics)
    }
  )
)

# --------------------------------------------------------------------------- #
# Helpers (outside the class) — unchanged interface                          #
# --------------------------------------------------------------------------- #

build_gini_timeseries <- function(df) {
  if (!all(c("country","year","TP") %in% names(df))) {
    stop("build_gini_timeseries: df must contain country, year, TP (and TC if available).")
  }
  if (!("TC" %in% names(df))) {
    if ("citations" %in% names(df)) {
      df <- df %>%
        dplyr::group_by(country, year) %>%
        dplyr::summarise(TP = dplyr::n(),
                         TC = sum(ifelse(is.na(citations), 0, citations), na.rm = TRUE),
                         .groups = "drop")
    } else {
      df <- df %>% dplyr::mutate(TC = 0)
    }
  } else {
    df <- df %>% dplyr::select(country, year, TP, TC) %>% dplyr::distinct()
  }

  ts_tp <- df %>% dplyr::group_by(year) %>%
    dplyr::summarise(Gini_TP = ineq::Gini(TP), .groups = "drop")
  ts_tc <- df %>% dplyr::group_by(year) %>%
    dplyr::summarise(Gini_TC = ineq::Gini(TC), .groups = "drop")

  list(tp = ts_tp, tc = ts_tc)
}

build_mcp_timeseries <- function(df, year_col = "year") {
  stopifnot(is.data.frame(df))
  if (!year_col %in% names(df)) stop(sprintf("build_mcp_timeseries: year column '%s' not found.", year_col))
  if (!all(c("MCP","SCP","TP") %in% names(df))) stop("build_mcp_timeseries: df must contain MCP, SCP, TP.")

  df %>%
    dplyr::group_by(.data[[year_col]]) %>%
    dplyr::summarise(
      MCP_docs   = sum(.data$MCP, na.rm = TRUE),
      SCP_docs   = sum(.data$SCP, na.rm = TRUE),
      Total_docs = sum(.data$TP,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Year          = suppressWarnings(as.integer(as.character(.data[[year_col]]))),
      MCP_share     = ifelse(Total_docs > 0, MCP_docs / Total_docs, NA_real_),
      MCP_share_pct = 100 * MCP_share
    ) %>%
    dplyr::select(Year, MCP_docs, SCP_docs, Total_docs, MCP_share, MCP_share_pct) %>%
    dplyr::arrange(Year)
}
