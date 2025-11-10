# ============================================================================ #
# M4_M4_COLLAB_NETWORK_CLUSTERS — Hypothesis 2
# IEEE Transactions–ready: International collaboration network with communities
# ============================================================================ #

suppressPackageStartupMessages({
  library(ggplot2)
  library(jsonlite)
  library(igraph)
  library(ggraph)
  library(tidygraph)
})

M4_M4_COLLAB_NETWORK_CLUSTERS <- setRefClass(
  "M4_M4_COLLAB_NETWORK_CLUSTERS",
  fields = list(
    df                = "data.frame",
    country_col       = "character",
    year_col          = "character",
    countries_is_raw  = "logical",
    sep_candidates    = "character",
    min_year          = "numeric",
    max_year          = "numeric",
    edge_min_weight   = "numeric",
    top_n_labels      = "numeric",
    language_map      = "ANY",
    results           = "list",
    plot_obj          = "ANY",
    params            = "list",
    edges_built       = "data.frame",
    nodes_built       = "data.frame",
    graph_obj         = "ANY",
    font_ieee         = "character"
  ),

  methods = list(

    initialize = function(df,
                          country_col      = "Countries_Array",
                          year_col         = "Publication Year",
                          countries_is_raw = TRUE,
                          sep_candidates   = c(";", "\\|", ","),
                          min_year         = NA,
                          max_year         = NA,
                          edge_min_weight  = 1,
                          top_n_labels     = 10,
                          language_map     = NULL,
                          debug            = FALSE) {

      .self$df               <- as.data.frame(df)
      .self$country_col      <- country_col
      .self$year_col         <- year_col
      .self$countries_is_raw <- countries_is_raw
      .self$sep_candidates   <- sep_candidates
      .self$min_year <- if (is.na(min_year)) NA_real_ else as.numeric(min_year)
      .self$max_year <- if (is.na(max_year)) NA_real_ else as.numeric(max_year)
      .self$edge_min_weight  <- edge_min_weight
      .self$top_n_labels     <- top_n_labels
      .self$language_map     <- language_map
      .self$results          <- list()
      .self$plot_obj         <- NULL
      .self$graph_obj        <- NULL
      .self$params           <- list(debug = debug)

      # font detection fallback
      available_fonts <- tryCatch(extrafont::fonts(), error = function(e) c())
      if ("Times New Roman" %in% available_fonts) {
        .self$font_ieee <- "Times New Roman"
      } else {
        .self$font_ieee <- "serif"
      }

      # coerce year if present
      if (.self$year_col %in% names(.self$df)) {
        .self$df[[.self$year_col]] <- suppressWarnings(as.numeric(.self$df[[.self$year_col]]))
      }

      # auto-detect country column if missing
      if (isTRUE(.self$countries_is_raw) && !.self$country_col %in% names(.self$df)) {
        candidates <- c(.self$country_col, "Country", "Countries", "Countries_Array",
                        "Country/Region", "C1", "Affiliations", "Author Countries", "AU_CO")
        found <- candidates[candidates %in% names(.self$df)]
        if (length(found) > 0) {
          .self$country_col <- found[1]
          if (debug) message(sprintf("[M4_M4] country_col not found. Using '%s'.", .self$country_col))
        } else {
          stop("[M4_M4] No country column found.")
        }
      }

      invisible(.self)
    },

    logmsg = function(...) { if (isTRUE(.self$params$debug)) message(sprintf("[M4_M4] %s", sprintf(...))) },

    # -------------------------- Build edges -------------------------------- #
    .detect_separator = function(x) {
      counts <- sapply(.self$sep_candidates, function(sep) {
        mean(vapply(x, function(s) {
          if (is.na(s) || is.null(s)) return(1L)
          length(unlist(strsplit(as.character(s), sep)))
        }, integer(1L)), na.rm = TRUE)
      })
      names(counts)[which.max(counts)]
    },

    .build_edges_from_raw = function() {
      ycol <- .self$year_col
      ccol <- .self$country_col
      df   <- .self$df

      if (!is.na(.self$min_year)) df <- df[df[[ycol]] >= .self$min_year, , drop=FALSE]
      if (!is.na(.self$max_year)) df <- df[df[[ycol]] <= .self$max_year, , drop=FALSE]

      get_tokens <- function(s, sep) {
        if (is.na(s) || is.null(s)) return(character(0))
        toks <- unique(trimws(unlist(strsplit(as.character(s), sep))))
        toks[nchar(toks) > 0]
      }

      if (is.list(df[[ccol]])) {
        countries_list <- lapply(df[[ccol]], as.character)
      } else {
        sep <- .self$.detect_separator(df[[ccol]])
        countries_list <- lapply(df[[ccol]], get_tokens, sep = sep)
      }

      make_pairs <- function(vec) {
        vec <- unique(vec)
        if (length(vec) < 2) return(NULL)
        combn(vec, 2, simplify = FALSE)
      }

      pairs <- unlist(lapply(countries_list, make_pairs), recursive = FALSE)
      if (length(pairs) == 0) stop("[M4_M4] No multi-country papers found.")

      edge_df <- do.call(rbind, lapply(pairs, function(p) data.frame(from = p[1], to = p[2], weight = 1L)))
      agg <- aggregate(weight ~ from + to, data = edge_df, FUN = sum)
      agg[agg$weight >= .self$edge_min_weight, ]
    },

    .use_edges_prebuilt = function() {
      required <- c("from", "to")
      if (!all(required %in% names(.self$df))) {
        stop("[M4_M4] Aggregated edge mode requires columns: from, to [, weight].")
      }
      edges <- .self$df[, intersect(names(.self$df), c("from","to","weight")), drop=FALSE]
      if (!"weight" %in% names(edges)) edges$weight <- 1L
      edges[edges$weight >= .self$edge_min_weight, ]
    },

    .build_graph = function() {
      edges <- if (isTRUE(.self$countries_is_raw)) .self$.build_edges_from_raw() else .self$.use_edges_prebuilt()
      g <- graph_from_data_frame(edges, directed = FALSE)
      E(g)$weight <- edges$weight
      list(edges=edges, graph=g)
    },

    # ------------------------ Community & metadata ------------------------- #
    .louvain_communities = function(g) cluster_louvain(g, weights = E(g)$weight),

    .country_meta = function(nodes) {
      df_nodes <- data.frame(country = nodes, stringsAsFactors = FALSE)
      df_nodes$TP <- vapply(nodes, function(cn) sum(grepl(cn, .self$df[[.self$country_col]])), numeric(1))
      if (requireNamespace("countrycode", quietly = TRUE)) {
        df_nodes$iso3      <- countrycode::countrycode(df_nodes$country, "country.name", "iso3c", warn = FALSE)
        df_nodes$continent <- countrycode::countrycode(df_nodes$country, "country.name", "continent", warn = FALSE)
        df_nodes$region    <- countrycode::countrycode(df_nodes$country, "country.name", "region", warn = FALSE)
      } else {
        df_nodes$iso3 <- df_nodes$continent <- df_nodes$region <- NA_character_
      }
      df_nodes
    },

    .weighted_purity = function(cluster_vec, category_vec) {
      ok <- !is.na(cluster_vec) & !is.na(category_vec)
      if (!any(ok)) return(NA_real_)
      cl <- cluster_vec[ok]; cat <- category_vec[ok]
      tabs <- tapply(seq_along(cl), cl, function(ix) {
        tb <- sort(table(cat[ix]), decreasing = TRUE)
        c(size = length(ix), purity = as.numeric(tb[1]) / length(ix))
      })
      sizes <- sapply(tabs, function(x) x["size"])
      pur   <- sapply(tabs, function(x) x["purity"])
      sum(pur * sizes) / sum(sizes)
    },

    # ------------------------------- Plot ---------------------------------- #
    .theme_ieee = function() {
      theme_void(base_family = .self$font_ieee) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
          plot.subtitle = element_text(hjust = 0.5, size = 8),
          plot.margin = margin(10, 10, 10, 10),
          legend.title = element_text(size = 8),
          legend.text  = element_text(size = 7)
        )
    },

    .plot_network = function(g, comm, nodes_df, purity_geo, purity_lang) {
      V(g)$cluster <- comm$membership
      V(g)$deg     <- degree(g)
      V(g)$name    <- nodes_df$country
      V(g)$TP      <- nodes_df$TP

      comm_ids <- sort(unique(V(g)$cluster))
      # Okabe-Ito palette (colorblind safe)
      pal <- c("#E69F00","#56B4E9","#009E73","#F0E442",
               "#0072B2","#D55E00","#CC79A7","#999999")
      col_map <- setNames(rep(pal, length.out = length(comm_ids)), comm_ids)

      lab_names <- names(sort(V(g)$deg, decreasing = TRUE))[seq_len(min(.self$top_n_labels, vcount(g)))]
      mod <- round(modularity(comm), 3)

      tg <- tidygraph::as_tbl_graph(g)

      # Cluster summaries: top 3 countries by TP
      cluster_summary <- aggregate(V(g)$TP, by=list(cluster=V(g)$cluster, country=V(g)$name), FUN=sum)
      cluster_summary <- cluster_summary[order(cluster_summary$cluster, -cluster_summary$x),]
      top_countries <- tapply(seq_len(nrow(cluster_summary)), cluster_summary$cluster, function(ix) {
        paste(head(cluster_summary$country[ix], 3), collapse=", ")
      })
      labels_vec <- paste0("Cluster ", names(top_countries), ": ", top_countries)

      set.seed(123) # reproducible layout
      layout_fr <- igraph::layout_with_fr(g, weights = E(g)$weight, niter = 5000, grid = "nogrid")

      comm <- cluster_louvain(g)
      layout_fr <- igraph::layout_with_fr(g)
      coords <- layout_fr
      # Shift clusters apart
      for (cl in unique(comm$membership)) {
        idx <- which(comm$membership == cl)
        coords[idx, ] <- scale(coords[idx, ]) + c(runif(1, -5, 5), runif(1, -5, 5))
      }


      p <- ggraph::ggraph(tg, layout = "fr") +
        ggraph::geom_edge_link(aes(width = weight), colour="gray70", alpha=0.6, show.legend=TRUE) +
        ggraph::geom_node_point(aes(size = deg, fill = factor(cluster)),
                                shape = 21, color = "black", stroke = 0.2, show.legend = TRUE) +
        ggraph::geom_node_text(aes(label = ifelse(name %in% lab_names, name, "")),
                               repel = TRUE, size = 2.8, family = .self$font_ieee,
                               box.padding = 0.5, point.padding = 0.3) +
        scale_size_continuous(name = "Collaborating partners", range = c(2, 8)) +
        guides(size = guide_legend(order = 1)) +
        scale_edge_width_continuous(name = "Co-authored papers", range = c(0.2, 2)) +
        guides(edge_width = guide_legend(order = 2)) +
        scale_fill_manual(
          name   = NULL,
          values = col_map,
          labels = labels_vec
        ) +
        guides(fill = guide_legend(override.aes = list(size=5), order=3, ncol=1)) +
        labs(
          title    = "International Collaboration Network",
          subtitle = paste("Louvain communities (Q =", mod, ")")
        ) +
        .self$.theme_ieee() +
        theme(
          legend.position   = "left",
          legend.box        = "vertical",
          legend.key.height = unit(0.15, "lines"),
          legend.spacing.y  = unit(0.02, "lines"),
          legend.margin     = margin(t=6, b=6, unit="pt")
        )

      return(p)
    },

    # ----------------------------- Public API ------------------------------ #
    run = function() {
      built <- .self$.build_graph()
      .self$edges_built <- built$edges
      .self$graph_obj   <- built$graph
      g <- .self$graph_obj
      nodes_df <- .self$.country_meta(V(g)$name)
      comm <- .self$.louvain_communities(g)

      mod  <- modularity(comm)
      sizes_df <- data.frame(cluster = names(sizes(comm)), size = as.integer(sizes(comm)))
      geo_vec <- if (!all(is.na(nodes_df$continent))) nodes_df$continent else nodes_df$region
      purity_geo <- .self$.weighted_purity(comm$membership, geo_vec)
      purity_lang <- if (!all(is.na(nodes_df$language))) .self$.weighted_purity(comm$membership, nodes_df$language) else NA_real_

      .self$results <- list(
        metric="collaboration_network_clusters",
        nodes=vcount(g), edges=ecount(g),
        modularity=mod,
        n_communities=length(sizes(comm)),
        community_sizes=sizes_df,
        geography_purity=purity_geo,
        language_purity=purity_lang,
        membership=data.frame(country=nodes_df$country, cluster=as.integer(comm$membership))
      )

      .self$plot_obj <- .self$.plot_network(g, comm, nodes_df, purity_geo, purity_lang)
      invisible(.self)
    },

    save_plot = function(out_dir,
                         filename_base = "M4_M3_Collab_Communities",
                         width_cm = 18.5, height_cm = 11.0, dpi = 600) {
      if (is.null(.self$plot_obj)) stop("[M4_M4] run() before save_plot().")
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

      w_in <- width_cm / 2.54; h_in <- height_cm / 2.54
      png_path <- file.path(out_dir, paste0(filename_base, ".png"))
      svg_path <- file.path(out_dir, paste0(filename_base, ".svg"))
      eps_path <- file.path(out_dir, paste0(filename_base, ".eps"))

      if (inherits(.self$plot_obj, "ggplot")) {
        ggsave(png_path, plot = .self$plot_obj, width = w_in, height = h_in, dpi = dpi, limitsize = FALSE)
        ggsave(svg_path, plot = .self$plot_obj, width = w_in, height = h_in, device = "svg", limitsize = FALSE)
        ggsave(eps_path, plot = .self$plot_obj, width = w_in, height = h_in, device = cairo_ps)
      } else {
        grDevices::png(png_path, width=w_in, height=h_in, units="in", res=dpi)
        .self$plot_obj(); grDevices::dev.off()
        grDevices::svg(svg_path, width=w_in, height=h_in)
        .self$plot_obj(); grDevices::dev.off()
        grDevices::cairo_ps(eps_path, width=w_in, height=h_in)
        .self$plot_obj(); grDevices::dev.off()
      }
    },

    save_json = function(out_dir, filename="M4_M4_Collab_Communities.json", pretty=TRUE) {
      if (length(.self$results) == 0) stop("[M4_M4] run() before save_json().")
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      writeLines(toJSON(.self$results, auto_unbox=TRUE, pretty=pretty), con=file.path(out_dir, filename))
    }
  )
)
