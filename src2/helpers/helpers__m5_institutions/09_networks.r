# ---- Institution collaboration network -------------------------------------

m5i_build_collab_network <- function(df_docs, inst_col = "Affiliations", cfg = list(min_edge_weight=1, sep_regex="[;|,]")) {
  # Build edges from co-occurrence of institutions within each paper
  edges_list <- lapply(seq_len(nrow(df_docs)), function(i) {
    insts <- df_docs$Institution_List[[i]]
    insts <- unique(unlist(insts))
    insts <- insts[nzchar(insts)]
    if (length(insts) < 2) return(NULL)
    t(combn(insts, 2))
  })
  edges_mat <- do.call(rbind, edges_list)
  if (is.null(edges_mat)) {
    g <- igraph::make_empty_graph(directed = FALSE)
    return(list(g = g, metrics = list(), nodes = data.frame(), edges = data.frame()))
  }
  edges_df <- as.data.frame(edges_mat, stringsAsFactors = FALSE)
  names(edges_df) <- c("from","to")
  edges_df <- edges_df %>%
    dplyr::count(from, to, name = "weight") %>%
    dplyr::filter(weight >= cfg$min_edge_weight)

  g <- igraph::graph_from_data_frame(edges_df, directed = FALSE)
  comm <- tryCatch(igraph::cluster_louvain(g), error = function(e) NULL)
  metrics <- list(
    n_nodes = igraph::gorder(g),
    n_edges = igraph::gsize(g),
    modularity = if (!is.null(comm)) igraph::modularity(comm) else NA_real_
  )
  nodes <- data.frame(institution = igraph::V(g)$name, stringsAsFactors = FALSE)
  edges <- edges_df
  list(g = g, metrics = metrics, nodes = nodes, edges = edges, community = comm)
}

m5i_reconstruct_cc_cfg <- function(inst_col, base_cfg) {
  base_cfg$institution_col <- inst_col
  base_cfg
}

cc_summarize_communities <- function(cc, cfg, country_col = NULL, sep_regex = cfg$sep_regex, label_field = "institution") {
  if (is.null(cc$community)) return(list(communities = list(), sizes = data.frame()))
  memb <- data.frame(node = igraph::V(cc$g)$name, comm = cc$community$membership, stringsAsFactors = FALSE)
  sizes <- memb %>% dplyr::count(comm, name = "size") %>% dplyr::arrange(dplyr::desc(size))
  list(membership = memb, sizes = sizes)
}

m5i_save_collab_plot <- function(cc, out_dir, cfg) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (igraph::gorder(cc$g) == 0) return(invisible(NULL))
  set.seed(42)
  lay <- igraph::layout_with_fr(cc$g)
  png(file.path(out_dir, "M5_collaboration_network.png"), width = 1600, height = 1200, res = 200, bg = "white")
  par(mar = c(0,0,0,0))
  plot(cc$g, layout = lay, vertex.size = 3, vertex.label = NA, edge.width = pmin(3, 1 + log1p(E(cc$g)$weight)))
  dev.off()
}
