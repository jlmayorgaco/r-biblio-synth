# ============================================================================ #
# country_collab_clusters.r
# International Collaboration Network with Community Detection (Louvain/Leiden)
#
# Public functional API (unchanged):
#   - country_cluster_plot(df, ...) -> list(plot, graph, metrics, nodes, edges, legend_labels)
#   - plot_country_collab_clusters(df, ...)   # backward-compatible wrapper
#
# Also provided: Optional OOP facade (R6) "CountryCollab" for extensibility.
#
# Design: SOLID & DRY helpers, reproducible, optional dependencies used defensively.
# Plot construction uses incremental "p <- p + ..." steps (readable & diff-friendly).
# ============================================================================ #

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(igraph)
  library(ggplot2)
})

# ------------------------------- Logging ----------------------------------- #

# Global verbosity switch: options(cc.verbose = TRUE/FALSE)
.cc_log <- function(tag, ...) {
  if (isTRUE(getOption("cc.verbose", TRUE))) {
    cat(sprintf("[%s] %s\n", tag, paste0(..., collapse = " ")))
  }
}

# --------------------------- Small Utilities -------------------------------- #

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

cc_seed <- function(seed) { set.seed(seed); invisible(seed) }

# Human + colorblind-safe palette (Okabe–Ito). Loop to n.
cc_palette <- function(n, name = "okabe_ito") {
  stopifnot(n >= 1)
  base <- c("#E69F00","#56B4E9","#009E73","#F0E442",
            "#0072B2","#D55E00","#CC79A7","#999999")
  rep(base, length.out = n)
}

# Normalize country names for readability/consistency
cc_country_alias <- function(x) {
  map <- c(
    "United States"             = "USA",
    "United States of America"  = "USA",
    "United Kingdom"            = "UK",
    "United Arab Emirates"      = "UAE",
    "Republic of Korea"         = "South Korea",
    "Korea, South"              = "South Korea",
    "Russian Federation"        = "Russia",
    "Czech Republic"            = "Czechia"
  )
  out <- ifelse(x %in% names(map), unname(map[x]), x)
  tools::toTitleCase(tolower(out))
}

# ------------------------------ Configuration ------------------------------ #

# A tiny, validated config object (simple list + validator).
cc_config <- function(
  country_col      = NULL,
  sep_regex        = ";|,|\\|",
  min_edge_weight  = 2,
  min_degree       = 2,
  label_top        = 15,
  community_method = c("louvain","leiden"),
  title            = "International Co-authorship Network — Communities",
  base_size        = 9,
  top_k_legend     = 5,
  spread           = 6,
  intra_scale      = 0.65,
  arrange          = c("auto","grid","circle","line"),
  legend_upper     = TRUE,
  seed             = 42
) {
  cfg <- list(
    country_col      = country_col,
    sep_regex        = sep_regex,
    min_edge_weight  = as.integer(min_edge_weight),
    min_degree       = as.integer(min_degree),
    label_top        = as.integer(label_top),
    community_method = match.arg(community_method),
    title            = title,
    base_size        = as.numeric(base_size),
    top_k_legend     = as.integer(top_k_legend),
    spread           = as.numeric(spread),
    intra_scale      = as.numeric(intra_scale),
    arrange          = match.arg(arrange),
    legend_upper     = isTRUE(legend_upper),
    seed             = as.integer(seed)
  )
  stopifnot(cfg$min_edge_weight >= 1, cfg$min_degree >= 0, cfg$label_top >= 0)
  .cc_log("cc_config", "OK | arrange =", cfg$arrange, "| spread =", cfg$spread, "| intra_scale =", cfg$intra_scale)
  cfg
}

# -------------------------- Data Column Detection -------------------------- #

# Try to infer the best country list column (supports list or delimited string)
cc_detect_country_col <- function(df, preferred = NULL) {
  .cc_log("cc_detect_country_col", "START | preferred =", preferred)
  if (!is.null(preferred) && preferred %in% names(df)) {
    .cc_log("cc_detect_country_col", "Using preferred:", preferred); return(preferred)
  }
  candidates <- unique(c(
    "Country_List","Country_Array","Countries","Country",
    "country_list","country_array","countries","country","C1","AU_CO",
    names(df)[grepl("country|countries|C1|AU_CO", names(df), ignore.case = TRUE)]
  ))
  candidates <- candidates[candidates %in% names(df)]
  .cc_log("cc_detect_country_col", "Candidates =", paste(candidates, collapse = ","))
  if (!length(candidates)) stop("[country_collab_clusters] No country list column found. Provide country_col.")

  score_col <- function(col) {
    x <- df[[col]]
    if (is.list(x)) {
      sum(vapply(x, function(v) length(na.omit(v)) > 0, logical(1)))
    } else {
      x <- as.character(x); x[is.na(x)] <- ""
      parts <- strsplit(x, "\\s*[,;|]\\s*")
      sum(vapply(parts, function(v) any(nzchar(trimws(v))), logical(1)))
    }
  }
  scores <- vapply(candidates, score_col, numeric(1))
  best <- candidates[which.max(scores)]
  .cc_log("cc_detect_country_col", "Chosen =", best)
  best
}

# ------------------------------ Edge Building ------------------------------ #

# Build undirected co-occurrence edges between countries within each paper.
cc_build_edges <- function(df, country_col, sep_regex) {
  .cc_log("cc_build_edges", "START | col =", country_col, "| sep =", sep_regex)
  if (!("PaperID" %in% names(df))) df$PaperID <- seq_len(nrow(df))

  per_paper <- df %>%
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

  pairs_of_2 <- function(x) {
    if (length(x) < 2) return(NULL)
    m <- t(combn(x, 2))
    tibble::tibble(from = m[, 1], to = m[, 2])
  }

  edges <- per_paper %>%
    mutate(pairs = purrr::map(Countries, pairs_of_2)) %>%
    select(pairs) %>%
    tidyr::unnest(cols = c(pairs)) %>%
    group_by(from, to) %>%
    summarise(weight = dplyr::n(), .groups = "drop")

  .cc_log("cc_build_edges", "END | edges =", nrow(edges))
  edges
}

# ------------------------------ Graph Building ----------------------------- #

# Build igraph, compute communities, metrics, and node metrics.
cc_build_graph <- function(edges, min_edge_weight, min_degree, community_method) {
  .cc_log("cc_build_graph", "START | edges =", nrow(edges),
          "| min_edge_weight =", min_edge_weight, "| min_degree =", min_degree,
          "| method =", community_method)

  e <- edges %>% filter(.data$weight >= min_edge_weight)
  if (nrow(e) == 0) {
    .cc_log("cc_build_graph", "No edges after threshold -> empty graph")
    g <- igraph::make_empty_graph(directed = FALSE)
    return(list(graph = g, metrics = list(), nodes = tibble::tibble(), edges = tibble::tibble()))
  }

  g <- igraph::graph_from_data_frame(e, directed = FALSE)
  g <- igraph::simplify(g, remove.loops = TRUE, edge.attr.comb = list(weight = "sum"))

  # Degree prune
  keep <- igraph::V(g)[igraph::degree(g) >= min_degree]
  g <- igraph::induced_subgraph(g, vids = keep)
  .cc_log("cc_build_graph", "Graph v/e =", igraph::vcount(g), "/", igraph::ecount(g))

  # Community detection
  if (community_method == "leiden" && requireNamespace("leidenbase", quietly = TRUE)) {
    part <- leidenbase::leiden_find_partition(g, resolution_parameter = 1.0, seed = 42)
    comm <- part$membership
  } else {
    comm <- igraph::cluster_louvain(g, weights = igraph::E(g)$weight)$membership
  }
  igraph::V(g)$community <- as.integer(factor(as.integer(comm), levels = sort(unique(as.integer(comm)))))

  # Node metrics
  igraph::V(g)$degree  <- igraph::degree(g)
  igraph::V(g)$wdegree <- igraph::strength(g, vids = igraph::V(g), mode = "all", weights = igraph::E(g)$weight)

  # Graph metrics
  mods <- tryCatch(igraph::modularity(g, membership = igraph::V(g)$community, weights = igraph::E(g)$weight),
                   error = function(...) NA_real_)
  dens <- igraph::edge_density(g, loops = FALSE)
  metrics <- list(
    n_nodes       = igraph::vcount(g),
    n_edges       = igraph::ecount(g),
    n_communities = length(unique(igraph::V(g)$community)),
    modularity    = round(mods, 3),
    density       = round(dens, 5),
    avg_degree    = round(mean(igraph::V(g)$degree), 2),
    avg_wdegree   = round(mean(igraph::V(g)$wdegree), 2)
  )
  .cc_log("cc_build_graph", sprintf("Metrics | nodes=%d edges=%d K=%d Q=%.3f dens=%.5f",
                                    metrics$n_nodes, metrics$n_edges, metrics$n_communities,
                                    metrics$modularity, metrics$density))

  list(
    graph = g, metrics = metrics,
    nodes  = tibble::tibble(
      name      = igraph::V(g)$name,
      community = igraph::V(g)$community,
      degree    = igraph::V(g)$degree,
      wdegree   = igraph::V(g)$wdegree
    ),
    edges  = tibble::as_tibble(igraph::as_data_frame(g, what = "edges"))
  )
}

# ----------------------------- Layout (robust) ----------------------------- #

# Place communities using meta-layout anchors + within-cluster FR, with hard fallbacks.
cc_clustered_layout <- function(
  g, membership, rank_map,
  method = c("auto","grid","circle","line"),
  spread = 6, intra_scale = 0.65, seed = 42,
  verbose = TRUE
) {
  logf <- function(...) if (isTRUE(verbose)) .cc_log("cc_clustered_layout", paste(..., collapse = " "))
  method <- match.arg(method)
  set.seed(seed)

  stopifnot(igraph::is.igraph(g))
  Vn <- igraph::vcount(g)

  # --- Membership checks
  mem_raw <- membership
  if (length(mem_raw) != Vn) mem_raw <- rep_len(mem_raw, Vn)
  if (any(is.na(mem_raw))) {
    idx_na <- which(is.na(mem_raw))
    mem_raw[idx_na] <- max(mem_raw, na.rm = TRUE) + seq_along(idx_na)
  }
  mem <- as.integer(factor(as.integer(mem_raw), levels = sort(unique(as.integer(mem_raw)))))
  K   <- length(unique(mem))
  logf("K communities =", K)

  # --- Within-cluster FR (preserves local structure)
  base <- tryCatch(igraph::layout_with_fr(g, weights = igraph::E(g)$weight),
                   error = function(e) tryCatch(igraph::layout_nicely(g),
                                                error = function(e2) matrix(0, nrow = Vn, ncol = 2)))
  base <- as.matrix(base)[, 1:2, drop = FALSE]
  if (any(!is.finite(base))) base[!is.finite(base)] <- 0

  # --- Community anchors
  make_circle <- function(K) { th <- seq(0, 2*pi, length.out = K + 1L)[-(K + 1L)]; cbind(cos(th), sin(th)) }
  make_line   <- function(K) cbind(seq(-(K-1)/2, (K-1)/2, length.out = K), rep(0, K))
  make_grid   <- function(K) { nc <- ceiling(sqrt(K)); nr <- ceiling(K / nc)
    grd <- expand.grid(row = seq_len(nr), col = seq_len(nc))[1:K, , drop = FALSE]
    cbind(grd$col - mean(unique(grd$col)), -(grd$row - mean(unique(grd$row)))) }

  anc <- NULL
  if (K == 1L) {
    anc <- matrix(c(0,0), nrow = 1, ncol = 2)
  } else if (method %in% c("grid","circle","line")) {
    anc <- switch(method, circle = make_circle(K), line = make_line(K), grid = make_grid(K))
  } else {
    # AUTO: meta-graph layout
    g_con <- tryCatch(igraph::contract(g, mapping = mem, vertex.attr.comb = list(weight = "sum")),
                      error = function(e) NULL)
    if (!is.null(g_con)) {
      g_con <- tryCatch(igraph::simplify(g_con, edge.attr.comb = list(weight = "sum")),
                        error = function(e) g_con)
      anc <- tryCatch(igraph::layout_with_fr(g_con, weights = igraph::E(g_con)$weight),
                      error = function(e) tryCatch(igraph::layout_nicely(g_con),
                                                   error = function(e2) NULL))
    }
    if (is.null(anc)) anc <- make_circle(K)
  }

  # Normalize + scale anchors
  anc <- as.matrix(anc)[, 1:2, drop = FALSE]
  if (any(!is.finite(anc))) anc[!is.finite(anc)] <- 0
  anc <- tryCatch({
    sc <- suppressWarnings(scale(anc)); if (!is.matrix(sc)) sc <- as.matrix(sc)
    if (any(!is.finite(sc))) sc[,] <- 0
    sc * spread
  }, error = function(e) anc * spread)

  # --- Compose final coordinates per community
  get_anchor <- function(c) anc[rank_map[as.character(c)], , drop = FALSE]
  coords <- matrix(NA_real_, nrow = Vn, ncol = 2)
  for (c in sort(unique(mem))) {
    idx <- which(mem == c)
    sub <- base[idx, , drop = FALSE]
    if (nrow(sub) > 1) {
      sub <- tryCatch({
        sc <- suppressWarnings(scale(sub))
        sc / max(1, max(abs(sc), na.rm = TRUE))
      }, error = function(e) matrix(0, nrow = nrow(sub), ncol = 2))
    } else sub <- matrix(0, nrow = 1, ncol = 2)
    ac <- get_anchor(c)
    coords[idx, 1] <- ac[1] + intra_scale * sub[, 1]
    coords[idx, 2] <- ac[2] + intra_scale * sub[, 2]
  }
  if (any(!is.finite(coords))) coords[!is.finite(coords)] <- 0
  colnames(coords) <- c("x","y")
  coords
}

# ---------------------- Legend Label Preparation --------------------------- #

# Build community labels (ranked by total TP or wdegree)
cc_build_legend_labels <- function(g, top_k, legend_upper = TRUE) {
  .cc_log("cc_build_legend_labels", "START | top_k =", top_k)
  score <- ifelse(is.na(igraph::V(g)$TP), igraph::V(g)$wdegree, igraph::V(g)$TP)
  comm  <- igraph::V(g)$community

  comm_score  <- tapply(score, comm, sum, na.rm = TRUE)
  comm_levels <- as.integer(names(sort(comm_score, decreasing = TRUE)))
  igraph::V(g)$community_f <- factor(comm, levels = comm_levels)

  node_df <- tibble::tibble(
    country   = igraph::V(g)$name,
    alias     = cc_country_alias(igraph::V(g)$name),
    community = comm,
    score     = score
  )
  node_df$label <- if (isTRUE(legend_upper)) toupper(node_df$alias) else node_df$alias

  top_by_comm <- node_df %>%
    group_by(community) %>%
    arrange(desc(score)) %>%
    summarise(label = paste0(head(label, top_k), collapse = ", "), .groups = "drop")

  rank_map <- setNames(seq_along(comm_levels), comm_levels)
  labels <- vapply(comm_levels, function(c) {
    paste0(rank_map[as.character(c)], ": ",
           top_by_comm$label[top_by_comm$community == c])
  }, character(1))

  .cc_log("cc_build_legend_labels", "END | K =", length(labels))
  list(labels = labels, comm_levels = comm_levels)
}

# ------------------------------ Theme & Saving ----------------------------- #

cc_detect_ieee_font <- function() {
  fam <- "serif"
  try({
    avail <- names(grDevices::pdfFonts())
    if (any(grepl("Times", avail, ignore.case = TRUE))) fam <- "Times"
  }, silent = TRUE)
  fam
}

cc_theme_ieee <- function(
  base_size = 9,
  family = cc_detect_ieee_font(),
  legend_position = "bottom",
  legend_box = "horizontal"    # <- default to horizontal so legends are side-by-side
) {
  ggplot2::theme_minimal(base_size = base_size, base_family = family) +
    ggplot2::theme(
      legend.position   = legend_position,
      legend.direction  = if (legend_position %in% c("top","bottom")) "horizontal" else "vertical",
      legend.box        = legend_box,   # <- important
      legend.title      = ggplot2::element_text(size = base_size - 1),
      legend.text       = ggplot2::element_text(size = base_size - 1),
      legend.key.height = grid::unit(0.5, "lines"),
      legend.key.width  = grid::unit(1.0, "lines"),
      plot.title        = ggplot2::element_text(face = "bold"),
      panel.grid        = ggplot2::element_blank(),
      axis.text         = ggplot2::element_blank(),
      axis.ticks        = ggplot2::element_blank(),
      plot.margin       = grid::unit(c(4, 6, 4, 6), "pt")
    )
}


cc_save_ieee <- function(plot, out_base, columns = c(1,2), height_in = NULL,
                         dpi = 600, formats = c("png","svg","eps")) {
  .cc_log("cc_save_ieee", "START | out =", out_base)
  columns <- match.arg(as.character(columns), choices = c("1","2"))
  width_in <- if (columns == "1") 3.5 else 7.16
  if (is.null(height_in)) height_in <- if (columns == "1") 2.7 else 3.6
  if (inherits(plot, "gg")) {
    for (f in formats) {
      dev <- if (f == "eps") cairo_ps else f
      ggplot2::ggsave(paste0(out_base, ".", f), plot = plot,
                      width = width_in, height = height_in, units = "in",
                      dpi = dpi, device = dev, limitsize = FALSE)
      .cc_log("cc_save_ieee", "Saved", paste0(out_base, ".", f))
    }
  } else if (is.function(plot)) {
    pngf <- paste0(out_base, ".png"); svgf <- paste0(out_base, ".svg")
    grDevices::png(pngf, width = width_in, height = height_in, units = "in", res = dpi)
    plot(); grDevices::dev.off(); .cc_log("cc_save_ieee", "Saved", pngf)
    grDevices::svg(svgf, width = width_in, height = height_in)
    plot(); grDevices::dev.off(); .cc_log("cc_save_ieee", "Saved", svgf)
  }
  .cc_log("cc_save_ieee", "END")
}

# -------------------------- Plot Builders (DRY) ---------------------------- #

# Create manual layout for ggraph
cc_create_ggraph_layout <- function(g, coords) {
  ggraph::create_layout(g, layout = "manual", x = coords[,1], y = coords[,2])
}

# Optional community hulls layer (concave if pkgs available)
cc_layer_hulls <- function() {
  if (requireNamespace("ggforce", quietly = TRUE) &&
      requireNamespace("concaveman", quietly = TRUE)) {
    ggforce::geom_mark_hull(
      aes(x = x, y = y, group = community_f, fill = community_f),
      concavity = 4, expand = grid::unit(3, "pt"),
      alpha = 0.06, show.legend = FALSE
    )
  } else {
    NULL
  }
}


# Apply scales for size and fill
cc_apply_scales <- function(p, g, legend_labels, one_row = TRUE) {
  # Size legend
  p <- p + ggplot2::scale_size_continuous(name = "Weighted degree", range = c(2.2, 8.5))

  # Community fill legend
  p <- p + ggplot2::scale_fill_manual(
    values = cc_palette(nlevels(igraph::V(g)$community_f)),
    name   = "Communities",
    labels = legend_labels
  )

  if (isTRUE(one_row)) {
    p <- p + ggplot2::guides(
      size = ggplot2::guide_legend(
        order = 1, title.position = "top",
        nrow = 1, byrow = TRUE                 # <- single row
      ),
      fill = ggplot2::guide_legend(
        order = 2, title.position = "top",
        nrow = 1, byrow = TRUE,                # <- single row
        override.aes = list(size = 4)
      )
    )
  }
  p
}


cc_apply_titles_theme <- function(p, cfg, metrics,
                                  legend_position = "bottom",
                                  legend_box = "horizontal") {   # <— NEW ARG

  p <- p + ggplot2::labs(
    title    = cfg$title,
    subtitle = paste0(
      "Communities: ", metrics$n_communities,
      " | Modularity Q: ", metrics$modularity,
      " | Nodes: ", metrics$n_nodes,
      " | Edges: ", metrics$n_edges
    ),
    x = NULL, y = NULL
  )

  p <- p + cc_theme_ieee(
    base_size       = cfg$base_size,
    legend_position = legend_position,
    legend_box      = legend_box            # <— PASS THROUGH
  )
  p
}

# Weighted degree: single horizontal row
# Communities: single vertical column underneath
cc_apply_legends_bottom_stacked <- function(p, g, legend_labels) {
  p +
    ggplot2::scale_size_continuous(name = "Weighted degree", range = c(2.2, 8.5)) +
    ggplot2::scale_fill_manual(
      values = cc_palette(nlevels(igraph::V(g)$community_f)),
      name   = "Communities",
      labels = legend_labels
    ) +
    ggplot2::guides(
      size = ggplot2::guide_legend(
        order = 1, title.position = "top",
        nrow = 1, byrow = TRUE
      ),
      fill = ggplot2::guide_legend(
        order = 2, title.position = "top",
        ncol = 1, byrow = TRUE,            # <— one item per row (vertical list)
        override.aes = list(size = 4)
      )
    ) +
    ggplot2::theme(
      legend.position   = "bottom",
      legend.direction  = "horizontal",
      legend.box        = "vertical",      # <— STACK legends (row then column)
      legend.spacing.y  = grid::unit(2, "pt"),
      legend.text       = ggplot2::element_text(hjust = 0)
    )
}


cc_build_base_plot <- function(lay, g, cfg, metrics, legend_labels) {
  p <- ggraph::ggraph(lay)

  # Edges
  p <- p + ggraph::geom_edge_link(
    aes(edge_width = weight),
    edge_alpha = 0.15, show.legend = FALSE
  )
  p <- p + ggraph::scale_edge_width(range = c(0.25, 1.6))

  # Optional hulls
  hull_layer <- cc_layer_hulls()
  if (!is.null(hull_layer)) p <- p + hull_layer

  # Nodes
  p <- p + ggraph::geom_node_point(
    aes(size = wdegree, fill = community_f),
    shape = 21, color = "black", stroke = 0.25, alpha = 0.95
  )

  # Node labels
  p <- p + ggraph::geom_node_text(aes(label = label), repel = TRUE, size = 2.8)

  # Scales + titles + bottom legends (single row for BOTH legends)
  p <- cc_apply_scales(p, g, legend_labels, one_row = TRUE)
  p <- cc_apply_titles_theme(p, cfg, metrics, legend_position = "bottom")

  p
}

# Get legends as grobs (for split layout)
cc_get_legends <- function(p, vertical_comm = TRUE) {
  if (!requireNamespace("cowplot", quietly = TRUE)) return(NULL)
  size_leg <- cowplot::get_legend(
    p + ggplot2::guides(
      size = ggplot2::guide_legend(title.position = "top", order = 1, nrow = 1, byrow = TRUE),
      fill = "none"
    ) + ggplot2::theme(legend.position = "bottom", legend.direction = "horizontal")
  )
  comm_leg <- cowplot::get_legend(
    p + ggplot2::guides(
      size = "none",
      fill = ggplot2::guide_legend(title = "Communities", title.position = "top",
                                   order = 1, ncol = if (vertical_comm) 1 else NULL,
                                   byrow = TRUE, override.aes = list(size = 4))
    ) + ggplot2::theme(legend.position = "right", legend.direction = "vertical")
  )
  list(size_leg = size_leg, comm_leg = comm_leg)
}

# Combine base panel + legends into split layout (right + bottom)
cc_combine_split_layout <- function(base_plot, legends,
                                    legend_right_width = 0.40,
                                    legend_bottom_height = 0.16) {
  stopifnot(requireNamespace("cowplot", quietly = TRUE))
  main <- base_plot + ggplot2::guides(size = "none", fill = "none")
  top_row <- cowplot::plot_grid(
    main, cowplot::ggdraw(legends$comm_leg),
    ncol = 2, rel_widths = c(1 - legend_right_width, legend_right_width), align = "h"
  )
  cowplot::plot_grid(
    top_row, cowplot::ggdraw(legends$size_leg),
    nrow = 2, rel_heights = c(1 - legend_bottom_height, legend_bottom_height)
  )
}

# Single-figure legend placement (bottom/right). Ensures communities in one column.
cc_apply_single_legend_layout <- function(p, position = c("bottom", "right")) {
  position <- match.arg(position)
  direction <- if (position == "right") "vertical" else "horizontal"
  p <- p + ggplot2::guides(
    size = ggplot2::guide_legend(title.position = "top", order = 1,
                                 nrow = if (position == "bottom") 1 else NULL,
                                 byrow = TRUE),
    fill = ggplot2::guide_legend(title.position = "top", order = 2,
                                 ncol = 1, byrow = TRUE, override.aes = list(size = 4))
  )
  p <- p + ggplot2::theme(
    legend.position   = position,
    legend.direction  = direction,
    legend.text       = ggplot2::element_text(hjust = 0),
    legend.key.width  = grid::unit(10, "pt"),
    legend.key.height = grid::unit(10, "pt")
  )
  p
}

# ----------------------------- Plot Assembly ------------------------------- #
# ---- Construct figure with split / single / stacked-bottom legends ----
cc_plot_graph <- function(
  g, metrics, cfg, legend_labels,
  legend_layout        = c("split","bottom","right","bottom_stacked"),
  legend_right_width   = 0.40,
  legend_bottom_height = 0.16,
  legend_wrap_width    = NULL,
  verbose              = TRUE
) {
  legend_layout <- match.arg(legend_layout)
  .cc_log("cc_plot_graph", "START | legend_layout =", legend_layout)

  # Wrap long legend labels if requested
  if (!is.null(legend_wrap_width) && requireNamespace("stringr", quietly = TRUE)) {
    legend_labels <- stringr::str_wrap(legend_labels, width = legend_wrap_width)
  }

  # ---- Coordinates
  comm_levels <- sort(unique(igraph::V(g)$community))
  igraph::V(g)$community_f <- factor(igraph::V(g)$community, levels = comm_levels)
  rank_map <- setNames(seq_along(comm_levels), comm_levels)

  coords <- cc_clustered_layout(
    g,
    membership = igraph::V(g)$community, rank_map = rank_map,
    method = cfg$arrange, spread = cfg$spread, intra_scale = cfg$intra_scale,
    seed = cfg$seed, verbose = verbose
  )

  # ---- Base-igraph fallback
  if (!requireNamespace("ggraph", quietly = TRUE)) {
    .cc_log("cc_plot_graph", "ggraph not available -> base igraph fallback")
    cols <- cc_palette(length(comm_levels))[as.integer(factor(igraph::V(g)$community, levels = comm_levels))]
    return(function() {
      igraph::plot.igraph(
        g, layout = coords,
        vertex.size  = scales::rescale(igraph::V(g)$wdegree, to = c(3, 18)),
        vertex.label = igraph::V(g)$label, vertex.label.cex = 0.7,
        vertex.color = cols,
        edge.width   = scales::rescale(igraph::E(g)$weight, to = c(0.2, 3)),
        main = cfg$title
      )
    })
  }

  # ---- Build plot incrementally
  lay <- cc_create_ggraph_layout(g, coords)
  p   <- cc_build_base_plot(lay, g, cfg, metrics, legend_labels)

  # ---- Legend layout branches
  if (legend_layout == "split" && requireNamespace("cowplot", quietly = TRUE)) {
    legends <- cc_get_legends(p, vertical_comm = TRUE)  # communities: one per row
    out <- cc_combine_split_layout(
      base_plot            = p,
      legends              = legends,
      legend_right_width   = legend_right_width,
      legend_bottom_height = legend_bottom_height
    )
    return(out)
  }

  if (legend_layout == "bottom_stacked") {
    # Weighted degree in one horizontal row, communities below in one column
    p <- cc_apply_legends_bottom_stacked(p, g, legend_labels)
    p <- cc_apply_titles_theme(
      p, cfg, metrics,
      legend_position = "bottom",
      legend_box      = "vertical"  # keep the stacked shape
    )
    return(p)
  }

  # Single-figure legends (either bottom side-by-side or right)
  position <- ifelse(legend_layout == "right", "right", "bottom")
  p <- cc_apply_single_legend_layout(p, position = position)
  p
}


# --------------------------- TP Counting (optional) ------------------------ #

# Term production (TP) per country to sort legend entries; robust to list/string columns.
cc_count_country_tp <- function(df, country_col, sep_regex = ";|,|\\|") {
  .cc_log("cc_count_country_tp", "START | col =", country_col)
  x <- df[[country_col]]
  if (is.list(x)) {
    out <- tibble::tibble(country = unlist(x, use.names = FALSE)) %>%
      mutate(country = trimws(as.character(country))) %>%
      filter(nzchar(country)) %>%
      count(country, name = "TP")
  } else {
    out <- tibble::tibble(country = as.character(x)) %>%
      mutate(country = ifelse(is.na(country), "", country)) %>%
      mutate(country = strsplit(country, sep_regex)) %>%
      tidyr::unnest(cols = c(country)) %>%
      mutate(country = trimws(country)) %>%
      filter(nzchar(country)) %>%
      count(country, name = "TP")
  }
  .cc_log("cc_count_country_tp", "END | rows =", nrow(out))
  out
}

# ------------------------------- Public API -------------------------------- #

#' Build and plot the collaboration network from a data frame.
#' @return list(plot, graph, metrics, nodes, edges, legend_labels)
country_cluster_plot <- function(df,
                                 country_col = NULL,
                                 sep_regex   = ";|,|\\|",
                                 min_edge_weight = 2,
                                 min_degree      = 2,
                                 label_top       = 15,
                                 community_method = c("louvain","leiden"),
                                 title = "International Co-authorship Network — Communities",
                                 base_size = 9,
                                 top_k_legend = 5,
                                 spread = 6,
                                 intra_scale = 0.65,
                                 arrange = c("auto","grid","circle","line"),
                                 legend_upper = TRUE,
                                 seed = 42) {

  .cc_log("country_cluster_plot", "START")
  cfg <- cc_config(
    country_col, sep_regex, min_edge_weight, min_degree, label_top,
    community_method, title, base_size, top_k_legend, spread,
    intra_scale, match.arg(arrange), legend_upper, seed
  )

  # Detect column + edges + graph
  cc_col <- cc_detect_country_col(df, preferred = cfg$country_col)
  edges  <- cc_build_edges(df, country_col = cc_col, sep_regex = cfg$sep_regex)
  built  <- cc_build_graph(edges, cfg$min_edge_weight, cfg$min_degree, cfg$community_method)
  g      <- built$graph

  if (igraph::vcount(g) == 0) {
    warning("[country_cluster_plot] graph empty after filtering. Lower thresholds.")
    return(list(plot = NULL, graph = g, metrics = built$metrics,
                nodes = tibble::tibble(), edges = tibble::tibble(),
                legend_labels = character(0)))
  }

  # Label hubs
  hub_ix <- order(igraph::V(g)$wdegree, decreasing = TRUE)[seq_len(min(cfg$label_top, igraph::vcount(g)))]
  igraph::V(g)$label <- ifelse(seq_along(igraph::V(g)) %in% hub_ix, igraph::V(g)$name, "")

  # Optional TP for ordering
  tp_tbl <- tryCatch(cc_count_country_tp(df, cc_col, cfg$sep_regex), error = function(e) NULL)
  if (!is.null(tp_tbl)) {
    tp_map <- setNames(tp_tbl$TP, tp_tbl$country)
    igraph::V(g)$TP <- tp_map[igraph::V(g)$name]
  } else {
    igraph::V(g)$TP <- NA_real_
  }

  # Legend labels + ranked community levels
  lab <- cc_build_legend_labels(g, top_k = cfg$top_k_legend, legend_upper = cfg$legend_upper)
  legend_labels <- lab$labels
  igraph::V(g)$community_f <- factor(igraph::V(g)$community, levels = lab$comm_levels)

  # Optional: geography purity metric (does not affect plot)
  if (requireNamespace("countrycode", quietly = TRUE)) {
    cont <- countrycode::countrycode(igraph::V(g)$name, "country.name", "continent", warn = FALSE)
    reg  <- countrycode::countrycode(igraph::V(g)$name, "country.name", "region", warn = FALSE)
    cat_vec <- ifelse(is.na(cont), reg, cont)
    built$metrics$geography_purity <- cc_weighted_purity(igraph::V(g)$community, cat_vec)
  }

  # Plot with split layout by default (bottom size legend; right communities vertical)
plot_obj <- cc_plot_graph(
  g, built$metrics, cfg, legend_labels,
  legend_layout = "bottom", legend_right_width = 0.40, legend_bottom_height = 0.16
)

  .cc_log("country_cluster_plot", "END")
  nodes_df <- tibble::tibble(
    Country   = igraph::V(g)$name,
    Community = igraph::V(g)$community,
    Degree    = igraph::V(g)$degree,
    WDegree   = igraph::V(g)$wdegree,
    TP        = igraph::V(g)$TP
  )
  edges_df <- tibble::as_tibble(igraph::as_data_frame(g, what = "edges"))

  list(
    plot = plot_obj,
    graph = g,
    metrics = built$metrics,
    nodes = nodes_df,
    edges = edges_df,
    legend_labels = legend_labels
  )
}

# Backward-compatible wrapper
plot_country_collab_clusters <- function(df,
                                         country_col = "Country_List",
                                         sep_regex = ";|,|\\|",
                                         min_edge_weight = 2,
                                         min_degree = 2,
                                         label_top = 15,
                                         community_method = c("louvain","leiden"),
                                         title = "International Collaboration Network — Communities",
                                         base_size = 9) {
  .cc_log("plot_country_collab_clusters", "START")
  res <- country_cluster_plot(
    df = df,
    country_col = country_col,
    sep_regex = sep_regex,
    min_edge_weight = min_edge_weight,
    min_degree = min_degree,
    label_top = label_top,
    community_method = community_method,
    title = title,
    base_size = base_size,
    arrange = "auto"
  )
  .cc_log("plot_country_collab_clusters", "END")
  list(plot = res$plot, graph = res$graph, metrics = res$metrics,
       data = list(nodes = res$nodes, edges = res$edges))
}

# ---------------------------- Optional OOP API ----------------------------- #
# A light R6 facade that uses the same helpers; handy for pipelines or unit tests.
if (requireNamespace("R6", quietly = TRUE)) {
  CountryCollab <- R6::R6Class(
    "CountryCollab",
    public = list(
      df      = NULL,
      cfg     = NULL,
      edges   = NULL,
      graph   = NULL,
      metrics = NULL,
      nodes   = NULL,
      edges_df= NULL,
      labels  = NULL,

      initialize = function(df, cfg = cc_config()) {
        self$df  <- df
        self$cfg <- cfg
      },

      build = function() {
        col      <- cc_detect_country_col(self$df, preferred = self$cfg$country_col)
        self$edges <- cc_build_edges(self$df, country_col = col, sep_regex = self$cfg$sep_regex)
        built       <- cc_build_graph(self$edges, self$cfg$min_edge_weight, self$cfg$min_degree, self$cfg$community_method)
        self$graph   <- built$graph; self$metrics <- built$metrics
        self$nodes   <- built$nodes; self$edges_df <- built$edges

        # label hubs
        if (igraph::vcount(self$graph) > 0) {
          hub_ix <- order(igraph::V(self$graph)$wdegree, decreasing = TRUE)[
            seq_len(min(self$cfg$label_top, igraph::vcount(self$graph)))
          ]
          igraph::V(self$graph)$label <- ifelse(seq_along(igraph::V(self$graph)) %in% hub_ix,
                                                igraph::V(self$graph)$name, "")
          # TP map
          tp_tbl <- tryCatch(cc_count_country_tp(self$df, col, self$cfg$sep_regex), error = function(e) NULL)
          if (!is.null(tp_tbl)) {
            tp_map <- setNames(tp_tbl$TP, tp_tbl$country)
            igraph::V(self$graph)$TP <- tp_map[igraph::V(self$graph)$name]
          } else igraph::V(self$graph)$TP <- NA_real_

          lab <- cc_build_legend_labels(self$graph, top_k = self$cfg$top_k_legend, legend_upper = self$cfg$legend_upper)
          self$labels <- lab$labels
          igraph::V(self$graph)$community_f <- factor(igraph::V(self$graph)$community, levels = lab$comm_levels)
        }
        invisible(self)
      },

      plot = function(legend_layout = "split", right_width = 0.40, bottom_height = 0.16) {
        stopifnot(!is.null(self$graph))
        cc_plot_graph(self$graph, self$metrics, self$cfg, self$labels,
                      legend_layout = legend_layout,
                      legend_right_width = right_width,
                      legend_bottom_height = bottom_height)
      },

      save = function(p, out_base, columns = 2, height_in = NULL, dpi = 600, formats = c("png","svg","eps")) {
        cc_save_ieee(p, out_base, columns = columns, height_in = height_in, dpi = dpi, formats = formats)
      }
    )
  )
}
# --------------------------- Cluster purity metric --------------------------- #
# Weighted purity of a partition, i.e., for each community take the fraction of
# the modal category, then weight by community size and average.
# - membership: integer/atomic vector of community ids (length ~ number of nodes)
# - category_vec: factor/character with the node’s category (e.g., continent/region)
# Returns a scalar in [0, 1] or NA_real_ when not computable.
cc_weighted_purity <- function(membership, category_vec, verbose = TRUE) {
  logf <- function(...) if (isTRUE(verbose) && isTRUE(getOption("cc.verbose", TRUE))) {
    cat("[cc_weighted_purity]", paste(..., collapse = " "), "\n")
  }

  # Coerce and align lengths defensively
  membership  <- as.vector(membership)
  category_vec <- as.vector(category_vec)

  n_mem <- length(membership)
  n_cat <- length(category_vec)
  if (n_mem == 0L || n_cat == 0L) {
    logf("Empty inputs -> NA")
    return(NA_real_)
  }
  if (n_mem != n_cat) {
    logf("WARN length mismatch: membership =", n_mem, "category_vec =", n_cat, "-> recycling")
    L <- max(n_mem, n_cat)
    membership  <- rep_len(membership,  L)
    category_vec <- rep_len(category_vec, L)
  }

  ok <- !is.na(membership) & !is.na(category_vec)
  if (!any(ok)) {
    logf("All values NA -> NA")
    return(NA_real_)
  }

  mem <- membership[ok]
  cat <- category_vec[ok]

  # For each community, compute modal-category share and size
  tabs <- tapply(seq_along(mem), mem, function(ix) {
    tb <- sort(table(cat[ix]), decreasing = TRUE)
    size_i   <- length(ix)
    purity_i <- as.numeric(tb[1]) / size_i
    c(size = size_i, purity = purity_i)
  })

  if (is.null(tabs) || length(tabs) == 0L) {
    logf("No groups after tapply -> NA")
    return(NA_real_)
  }

  sizes <- sapply(tabs, `[[`, "size")
  purs  <- sapply(tabs, `[[`, "purity")

  if (!is.numeric(sizes) || !is.numeric(purs) || sum(sizes) <= 0) {
    logf("Degenerate sizes/purities -> NA")
    return(NA_real_)
  }

  res <- as.numeric(sum(purs * sizes) / sum(sizes))
  # Clamp to [0,1] for numerical safety
  res <- max(0, min(1, res))
  logf("OK | purity =", sprintf("%.4f", res), "| groups =", length(sizes))
  res
}
# Summarize communities into a clean, auditable payload
# - cc: result from country_cluster_plot()
# - cfg: the config you used (or reconstruct a minimal list with the same fields)
# - country_col: the detected/used column in your df
# - sep_regex: separators used to split country lists
cc_summarize_communities <- function(cc, cfg, country_col, sep_regex = cfg$sep_regex) {
  stopifnot(!is.null(cc$graph))
  g <- cc$graph

  # Ranked community order: prefer factor levels (already ranked in country_cluster_plot),
  # otherwise rank by total TP (fallback: wdegree).
  comm_levels <- levels(igraph::V(g)$community_f)
  if (is.null(comm_levels)) {
    score <- ifelse(is.na(igraph::V(g)$TP), igraph::V(g)$wdegree, igraph::V(g)$TP)
    comm   <- igraph::V(g)$community
    comm_score <- tapply(score, comm, sum, na.rm = TRUE)
    comm_levels <- names(sort(comm_score, decreasing = TRUE))
  }
  comm_levels <- as.integer(comm_levels)

  # Build groups: countries per community (sorted by weighted degree)
  groups_list <- lapply(seq_along(comm_levels), function(i) {
    c_id <- comm_levels[i]
    ix   <- which(igraph::V(g)$community == c_id)
    ord  <- order(igraph::V(g)$wdegree[ix], decreasing = TRUE)
    as.character(igraph::V(g)$name[ix][ord])
  })
  names(groups_list) <- paste0("group", seq_along(groups_list))

  # Tidy membership (long) with useful columns
  membership_df <- tibble::tibble(
    Country        = igraph::V(g)$name,
    CommunityId    = igraph::V(g)$community,
    CommunityRank  = match(igraph::V(g)$community, comm_levels),  # 1..K (ranked)
    Degree         = igraph::V(g)$degree,
    WeightedDegree = igraph::V(g)$wdegree
  ) %>% dplyr::arrange(CommunityRank, dplyr::desc(WeightedDegree))

  # Per-community short label (same order as groups)
  comm_labels <- cc$legend_labels %||% character(0)

  list(
    algorithm = list(
      name   = cfg$community_method,
      params = list(
        min_edge_weight = cfg$min_edge_weight,
        min_degree      = cfg$min_degree,
        arrange         = cfg$arrange,
        spread          = cfg$spread,
        intra_scale     = cfg$intra_scale,
        seed            = cfg$seed,
        label_top       = cfg$label_top,
        top_k_legend    = cfg$top_k_legend
      )
    ),
    input = list(
      country_col = country_col,
      sep_regex   = sep_regex,
      n_rows      = igraph::vcount(g),
      n_edges     = igraph::ecount(g)
    ),
    summary = list(
      n_communities = length(groups_list),
      community_rank_order = paste0("group", seq_along(groups_list)),
      community_labels_topk = comm_labels  # e.g. "1: USA, UK, ..."
    ),
    groups   = groups_list,      # list(group1 = c("USA","UK",...), group2=..., ...)
    membership = membership_df,  # tidy mapping Country -> community (ranked)
    created_at = Sys.time()
  )
}

# ============================================================================ #
# End of file
# ============================================================================ #
