# ============================================================================
# m4_compute_production.R - Institutional Productivity Metrics
# ============================================================================

#' Compute institutional productivity metrics
#'
#' @param parsed_data Output from m4_extract_institutions
#' @param config Configuration list
#' @return List with production metrics
#' @export
m4_compute_institutional_production <- function(parsed_data, config = biblio_config()) {
  if (parsed_data$status != "success") {
    return(list(status = "error: invalid input data"))
  }
  
  institutions <- parsed_data$institutions
  
  log_message("INFO", "Computing production metrics for {n} institutions", 
              n = length(unique(institutions$institution_canonical)))
  
  # Aggregate by institution
  production_summary <- institutions %>%
    dplyr::group_by(institution_canonical, sector) %>%
    dplyr::summarise(
      n_papers = dplyr::n_distinct(doc_id),
      n_authors = dplyr::n(),
      n_countries = dplyr::n_distinct(country, na.rm = TRUE),
      top_country = names(sort(table(country), decreasing = TRUE))[1],
      n_cities = dplyr::n_distinct(city, na.rm = TRUE),
      n_departments = dplyr::n_distinct(department, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(n_papers)) %>%
    dplyr::mutate(
      rank = dplyr::row_number(),
      pct_of_total = n_papers / sum(n_papers) * 100
    )
  
  # Compute sector distribution
  sector_summary <- production_summary %>%
    dplyr::group_by(sector) %>%
    dplyr::summarise(
      n_institutions = dplyr::n(),
      total_papers = sum(n_papers),
      avg_papers_per_inst = mean(n_papers),
      pct_of_total = sum(n_papers) / sum(production_summary$n_papers) * 100,
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(total_papers))
  
  # Top institutions
  top_n <- config$top_n_sources %||% 20
  top_institutions <- production_summary %>%
    dplyr::slice(1:min(top_n, nrow(.)))
  
  # Gini coefficient for inequality
  gini_production <- compute_gini(production_summary$n_papers)
  
  # Concentration metrics
  total_papers <- sum(production_summary$n_papers)
  cumulative_pct <- cumsum(production_summary$n_papers) / total_papers * 100
  
  top_5_pct <- production_summary %>%
    dplyr::slice(1:5) %>%
    dplyr::summarise(pct = sum(n_papers) / total_papers * 100) %>%
    dplyr::pull(pct)
  
  top_10_pct <- production_summary %>%
    dplyr::slice(1:10) %>%
    dplyr::summarise(pct = sum(n_papers) / total_papers * 100) %>%
    dplyr::pull(pct)
  
  list(
    status = "success",
    institutions = production_summary,
    sector_summary = sector_summary,
    top_institutions = top_institutions,
    summary = list(
      total_institutions = nrow(production_summary),
      total_papers = total_papers,
      gini_coefficient = gini_production,
      top_5_share = top_5_pct,
      top_10_share = top_10_pct,
      dominant_sector = sector_summary$sector[1]
    )
  )
}

#' Compute institutional collaboration
#' @export
m4_compute_institutional_collaboration <- function(parsed_data, config = biblio_config()) {
  institutions <- parsed_data$institutions
  
  log_message("INFO", "Computing institutional collaboration networks")
  
  # Find papers with multiple institutions
  multi_inst <- institutions %>%
    dplyr::group_by(doc_id) %>%
    dplyr::filter(dplyr::n_distinct(institution_canonical) > 1) %>%
    dplyr::ungroup()
  
  if (nrow(multi_inst) == 0) {
    return(list(
      status = "success",
      collaboration_index = 0,
      n_collaborative_papers = 0,
      collaboration_matrix = matrix()
    ))
  }
  
  # Build collaboration pairs
  collab_pairs <- multi_inst %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(
      institutions = list(unique(institution_canonical)),
      .groups = "drop"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::do({
      insts <- .$institutions[[1]]
      if (length(insts) < 2) return(data.frame(inst1 = character(), inst2 = character()))
      
      pairs <- combn(insts, 2, simplify = FALSE)
      data.frame(
        inst1 = sapply(pairs, `[`, 1),
        inst2 = sapply(pairs, `[`, 2),
        stringsAsFactors = FALSE
      )
    }) %>%
    dplyr::ungroup()
  
  # Count collaborations
  collab_counts <- collab_pairs %>%
    dplyr::group_by(inst1, inst2) %>%
    dplyr::summarise(
      n_collaborations = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(n_collaborations))
  
  # Build collaboration matrix
  all_insts <- unique(c(collab_counts$inst1, collab_counts$inst2))
  n_insts <- length(all_insts)
  
  collab_matrix <- matrix(0, nrow = n_insts, ncol = n_insts,
                         dimnames = list(all_insts, all_insts))
  
  for (i in seq_len(nrow(collab_counts))) {
    row <- collab_counts[i, ]
    collab_matrix[row$inst1, row$inst2] <- row$n_collaborations
    collab_matrix[row$inst2, row$inst1] <- row$n_collaborations
  }
  
  # Collaboration metrics
  total_papers <- length(unique(institutions$doc_id))
  collaborative_papers <- length(unique(multi_inst$doc_id))
  collaboration_index <- collaborative_papers / total_papers
  
  # Institution-level collaboration stats
  inst_collab_stats <- data.frame(
    institution = all_insts,
    n_collaborators = rowSums(collab_matrix > 0),
    total_collaborations = rowSums(collab_matrix),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::arrange(dplyr::desc(total_collaborations))
  
  list(
    status = "success",
    collaboration_index = collaboration_index,
    n_collaborative_papers = collaborative_papers,
    n_total_papers = total_papers,
    collaboration_pairs = collab_counts,
    collaboration_matrix = collab_matrix,
    institution_stats = inst_collab_stats
  )
}

#' Compute institutional ranking
#' @export
m4_compute_institutional_ranking <- function(parsed_data, config = biblio_config()) {
  institutions <- parsed_data$institutions
  
  # Get paper-level data
  paper_inst <- institutions %>%
    dplyr::group_by(doc_id, institution_canonical) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  # Compute rankings by different metrics
  rankings <- list()
  
  # By paper count
  rankings$by_papers <- paper_inst %>%
    dplyr::count(institution_canonical, sort = TRUE, name = "n_papers") %>%
    dplyr::mutate(rank = dplyr::row_number())
  
  # By author count (if AU field available)
  rankings$by_authors <- institutions %>%
    dplyr::count(institution_canonical, sort = TRUE, name = "n_authors") %>%
    dplyr::mutate(rank = dplyr::row_number())
  
  # By geographic diversity
  rankings$by_geography <- institutions %>%
    dplyr::group_by(institution_canonical) %>%
    dplyr::summarise(
      n_countries = dplyr::n_distinct(country, na.rm = TRUE),
      n_cities = dplyr::n_distinct(city, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(n_countries), dplyr::desc(n_cities)) %>%
    dplyr::mutate(rank = dplyr::row_number())
  
  # Composite ranking (average of ranks)
  all_insts <- unique(institutions$institution_canonical)
  composite <- data.frame(
    institution = all_insts,
    paper_rank = match(all_insts, rankings$by_papers$institution_canonical),
    author_rank = match(all_insts, rankings$by_authors$institution_canonical),
    geography_rank = match(all_insts, rankings$by_geography$institution_canonical),
    stringsAsFactors = FALSE
  )
  
  composite$avg_rank <- rowMeans(composite[, c("paper_rank", "author_rank", "geography_rank")], na.rm = TRUE)
  composite <- composite %>%
    dplyr::arrange(avg_rank) %>%
    dplyr::mutate(composite_rank = dplyr::row_number())
  
  rankings$composite <- composite
  
  list(
    status = "success",
    rankings = rankings,
    top_institution = rankings$by_papers$institution_canonical[1]
  )
}

#' Compute sector analysis
#' @export
m4_compute_sector_analysis <- function(parsed_data, config = biblio_config()) {
  institutions <- parsed_data$institutions
  
  sector_analysis <- institutions %>%
    dplyr::group_by(sector) %>%
    dplyr::summarise(
      n_institutions = dplyr::n_distinct(institution_canonical),
      n_papers = dplyr::n_distinct(doc_id),
      avg_papers_per_inst = n_papers / n_institutions,
      n_countries = dplyr::n_distinct(country, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(n_papers))
  
  # Sector collaboration
  sector_collab <- institutions %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(
      sectors = list(unique(sector)),
      .groups = "drop"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::do({
      secs <- .$sectors[[1]]
      if (length(secs) < 2) return(data.frame(sector1 = character(), sector2 = character()))
      
      pairs <- combn(secs, 2, simplify = FALSE)
      data.frame(
        sector1 = sapply(pairs, `[`, 1),
        sector2 = sapply(pairs, `[`, 2),
        stringsAsFactors = FALSE
      )
    }) %>%
    dplyr::ungroup() %>%
    dplyr::count(sector1, sector2, name = "n_collaborations") %>%
    dplyr::arrange(dplyr::desc(n_collaborations))
  
  list(
    status = "success",
    sector_distribution = sector_analysis,
    sector_collaboration = sector_collab
  )
}

#' Compute institutional geography
#' @export
m4_compute_institutional_geography <- function(parsed_data, config = biblio_config()) {
  institutions <- parsed_data$institutions
  
  # Country-level analysis
  country_institutions <- institutions %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      n_institutions = dplyr::n_distinct(institution_canonical),
      n_papers = dplyr::n_distinct(doc_id),
      top_institution = names(sort(table(institution_canonical), decreasing = TRUE))[1],
      sectors = paste(unique(sector), collapse = "; "),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(country)) %>%
    dplyr::arrange(dplyr::desc(n_papers))
  
  # City-level analysis
  city_institutions <- institutions %>%
    dplyr::group_by(country, city) %>%
    dplyr::summarise(
      n_institutions = dplyr::n_distinct(institution_canonical),
      n_papers = dplyr::n_distinct(doc_id),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(city)) %>%
    dplyr::arrange(dplyr::desc(n_papers))
  
  list(
    status = "success",
    by_country = country_institutions,
    by_city = city_institutions,
    n_countries = nrow(country_institutions),
    n_cities = nrow(city_institutions)
  )
}

#' Compute institutional networks
#' @export
m4_compute_institutional_networks <- function(parsed_data, config = biblio_config()) {
  # This builds on collaboration data but adds network metrics
  collab_data <- m4_compute_institutional_collaboration(parsed_data, config)
  
  if (collab_data$n_collaborative_papers == 0) {
    return(list(status = "error: no collaboration data"))
  }
  
  matrix <- collab_data$collaboration_matrix
  
  # Convert to igraph if available
  if (requireNamespace("igraph", quietly = TRUE)) {
    g <- igraph::graph_from_adjacency_matrix(matrix, mode = "undirected", weighted = TRUE)
    
    network_metrics <- list(
      n_nodes = igraph::vcount(g),
      n_edges = igraph::ecount(g),
      density = igraph::edge_density(g),
      avg_degree = mean(igraph::degree(g)),
      clustering_coefficient = igraph::transitivity(g, type = "global"),
      diameter = igraph::diameter(g, weights = NA),
      betweenness = igraph::betweenness(g),
      closeness = igraph::closeness(g),
      eigenvector = igraph::eigen_centrality(g)$vector
    )
  } else {
    # Compute basic metrics without igraph
    n <- nrow(matrix)
    network_metrics <- list(
      n_nodes = n,
      n_edges = sum(matrix > 0) / 2,
      density = sum(matrix > 0) / (n * (n - 1)),
      avg_degree = mean(rowSums(matrix > 0))
    )
  }
  
  list(
    status = "success",
    collaboration_matrix = matrix,
    network_metrics = network_metrics
  )
}

#' Helper: Compute Gini coefficient
#' @keywords internal
compute_gini <- function(x) {
  x <- sort(x)
  n <- length(x)
  if (n < 2 || sum(x) == 0) return(0)
  
  cumsum_x <- cumsum(x)
  (n + 1 - 2 * sum(cumsum_x) / sum(x)) / n
}