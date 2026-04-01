# ============================================================================
# m5_run.R - M5 Citation Network Analysis Orchestrator
# ============================================================================

#' Run M5 module (Citation Network Analysis)
#'
#' @param input Bibliographic data frame with CR (cited references) field
#' @param config Configuration list
#' @param export Logical. If TRUE, exports artifacts to disk.
#' @return Module result object
#' @export
run_m5 <- function(input, config = biblio_config(), export = TRUE) {
  config <- merge_biblio_config(config)
  
  log_message("INFO", "Starting M5: Citation Network Analysis")
  
  # Validate input
  validation <- validate_m5_input(input, config)
  if (!validation$ok) {
    log_message("ERROR", "M5 validation failed: {msg}", msg = validation$error)
    return(create_error_result("m5", "Citation Network", validation$error))
  }
  
  # Build citation network
  log_message("INFO", "Building citation network...")
  citation_network <- m5_build_citation_network(input, config)
  
  if (citation_network$status != "success") {
    return(create_error_result("m5", "Citation Network", citation_network$status))
  }
  
  # Compute metrics
  log_message("INFO", "Computing citation metrics...")
  data <- list(
    network = citation_network,
    metrics = m5_compute_citation_metrics(citation_network, config),
    cocitation = m5_build_cocitation_network(input, config),
    coupling = m5_build_coupling_network(input, config),
    bursts = m5_detect_citation_bursts(input, config)
  )
  
  # Build result
  result <- new_module_result(
    module_id = "m5",
    module_name = "Citation Network Analysis",
    status = "success",
    inputs = list(n_records = nrow(input)),
    data = data,
    diagnostics = list()
  )
  
  # Render
  result <- m5_render_all(result, data, config)
  
  # Export
  if (export) {
    exported <- export_m5(result, config)
    manifest <- build_m5_manifest(result, exported, config)
    result <- attach_manifest_to_result(result, manifest)
  }
  
  log_message("INFO", "M5: Citation Network Analysis completed")
  result
}

#' Build citation network from references
#' @export
m5_build_citation_network <- function(input, config = biblio_config()) {
  # Parse cited references
  references <- parse_references(input$CR)
  
  # Create edge list: citing_paper -> cited_paper
  edges <- data.frame(
    from = character(),
    to = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(nrow(input))) {
    citing <- input$DI[i] %||% paste0("doc_", i)
    cited <- references[[i]]
    
    if (length(cited) > 0) {
      edges <- rbind(edges, data.frame(
        from = rep(citing, length(cited)),
        to = cited,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Build graph
  if (requireNamespace("igraph", quietly = TRUE) && nrow(edges) > 0) {
    g <- igraph::graph_from_data_frame(edges, directed = TRUE)
    
    list(
      status = "success",
      graph = g,
      edges = edges,
      n_nodes = igraph::vcount(g),
      n_edges = igraph::ecount(g)
    )
  } else {
    list(
      status = "success",
      edges = edges,
      n_nodes = length(unique(c(edges$from, edges$to))),
      n_edges = nrow(edges)
    )
  }
}

#' Parse reference strings
#' @keywords internal
parse_references <- function(cr_field) {
  lapply(cr_field, function(cr) {
    if (is.na(cr) || cr == "") return(character())
    
    # Split by semicolon
    refs <- strsplit(cr, ";")[[1]]
    refs <- trimws(refs)
    
    # Extract DOI if present
    dois <- regmatches(refs, gregexpr("10\\.\\S+", refs))
    unlist(dois)
  })
}

#' Build co-citation network
#' @export
m5_build_cocitation_network <- function(input, config = biblio_config()) {
  references <- parse_references(input$CR)
  
  # Find pairs of papers cited together
  cocitation_pairs <- list()
  
  for (refs in references) {
    if (length(refs) >= 2) {
      pairs <- combn(refs, 2, simplify = FALSE)
      for (pair in pairs) {
        key <- paste(sort(pair), collapse = "::")
        cocitation_pairs[[key]] <- (cocitation_pairs[[key]] %||% 0) + 1
      }
    }
  }
  
  # Convert to data frame
  if (length(cocitation_pairs) > 0) {
    pairs_df <- data.frame(
      paper1 = sapply(strsplit(names(cocitation_pairs), "::"), `[`, 1),
      paper2 = sapply(strsplit(names(cocitation_pairs), "::"), `[`, 2),
      cocitation_count = unlist(cocitation_pairs),
      stringsAsFactors = FALSE
    )
    
    list(status = "success", pairs = pairs_df, n_pairs = nrow(pairs_df))
  } else {
    list(status = "error: no co-citations found")
  }
}

#' Build bibliographic coupling network
#' @export
m5_build_coupling_network <- function(input, config = biblio_config()) {
  references <- parse_references(input$CR)
  
  # Calculate coupling strength between papers
  n <- length(references)
  coupling <- data.frame(
    paper1 = character(),
    paper2 = character(),
    shared_references = integer(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      shared <- length(intersect(references[[i]], references[[j]]))
      if (shared > 0) {
        coupling <- rbind(coupling, data.frame(
          paper1 = input$DI[i] %||% paste0("doc_", i),
          paper2 = input$DI[j] %||% paste0("doc_", j),
          shared_references = shared,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  list(status = "success", coupling = coupling, n_pairs = nrow(coupling))
}

#' Compute citation metrics
#' @export
m5_compute_citation_metrics <- function(network, config = biblio_config()) {
  if (!requireNamespace("igraph", quietly = TRUE) || is.null(network$graph)) {
    return(list(status = "error: igraph not available"))
  }
  
  g <- network$graph
  
  list(
    status = "success",
    degree = igraph::degree(g, mode = "all"),
    indegree = igraph::degree(g, mode = "in"),
    outdegree = igraph::degree(g, mode = "out"),
    betweenness = igraph::betweenness(g),
    closeness = igraph::closeness(g),
    pagerank = igraph::page_rank(g)$vector,
    eigenvector = igraph::eigen_centrality(g)$vector
  )
}

#' Detect citation bursts
#' @export
m5_detect_citation_bursts <- function(input, config = biblio_config()) {
  # Group citations by year
  yearly_citations <- input %>%
    dplyr::group_by(PY) %>%
    dplyr::summarise(n_citations = sum(TC, na.rm = TRUE), .groups = "drop")
  
  # Simple burst detection: years with >2x average
  avg_citations <- mean(yearly_citations$n_citations, na.rm = TRUE)
  
  bursts <- yearly_citations %>%
    dplyr::filter(n_citations > avg_citations * 2) %>%
    dplyr::mutate(burst_strength = n_citations / avg_citations)
  
  list(
    status = "success",
    bursts = bursts,
    avg_citations = avg_citations,
    n_bursts = nrow(bursts)
  )
}