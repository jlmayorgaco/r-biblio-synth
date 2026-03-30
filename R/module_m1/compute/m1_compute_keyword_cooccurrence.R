# ============================================================================
# m1_compute_keyword_cooccurrence.R - Keyword Co-occurrence Network Analysis
# ============================================================================
# IEEE Q1 Enhancement: Network analysis of keyword co-occurrence patterns
# Computes adjacency matrix, network metrics, and community detection

#' Compute keyword co-occurrence network
#'
#' Creates a co-occurrence network from author keywords (KW, DE, ID fields).
#' Includes network metrics, centrality measures, and community detection.
#'
#' @param input Bibliographic data frame
#' @param config Configuration list
#' @return List with network data, metrics, and visualization data
#' @export
compute_m1_keyword_cooccurrence <- function(input, config = biblio_config()) {
  cat("      [M1-kw-cooc] Starting...\n")
  validate_is_data_frame(input)
  
  kw_col <- NULL
  for (col in c("KW_Merged", "KW", "DE", "ID", "Keywords")) {
    if (col %in% names(input)) {
      kw_col <- col
      break
    }
  }
  cat("      [M1-kw-cooc] Using column:", kw_col, "\n")
  
  if (is.null(kw_col)) {
    return(list(
      adjacency = matrix(0, 0, 0),
      metrics = list(),
      communities = list(),
      status = "error: no keyword column found"
    ))
  }
  
  cat("      [M1-kw-cooc] Extracting keywords...\n")
  keywords <- extract_keywords(input[[kw_col]])
  cat("      [M1-kw-cooc] Extracted", length(keywords), "documents with keywords\n")
  
  cat("      [M1-kw-cooc] Building co-occurrence matrix...\n")
  flush.console()
  
  if (length(keywords) == 0) {
    return(list(
      adjacency = matrix(0, 0, 0),
      metrics = list(),
      communities = list(),
      status = "error: no keywords found"
    ))
  }
  
  cooc_matrix <- build_cooccurrence_matrix(keywords)
  cat("      [M1-kw-cooc] Co-occurrence matrix built:", nrow(cooc_matrix), "x", ncol(cooc_matrix), "\n")
  flush.console()
  
  if (nrow(cooc_matrix) == 0) {
    return(list(
      adjacency = matrix(0, 0, 0),
      metrics = list(),
      communities = list(),
      status = "error: empty co-occurrence matrix"
    ))
  }
  
  top_n <- config$top_n_keywords %||% 50
  top_keywords <- get_top_keywords(cooc_matrix, top_n)
  
  cooc_top <- cooc_matrix[top_keywords, top_keywords]
  
  network_metrics <- compute_network_metrics(cooc_top)
  
  communities <- detect_communities(cooc_top, method = "louvain")
  
  keyword_freq <- sort(colSums(cooc_matrix), decreasing = TRUE)
  
  summary_stats <- list(
    total_keywords = ncol(cooc_matrix),
    total_pairs = sum(cooc_matrix) / 2,
    unique_pairs = sum(cooc_matrix > 0) / 2,
    avg_cooccurrence = mean(cooc_matrix[cooc_matrix > 0]),
    max_cooccurrence = max(cooc_matrix),
    density = sum(cooc_matrix > 0) / (ncol(cooc_matrix)^2 - ncol(cooc_matrix)),
    top_keywords = names(keyword_freq)[1:min(10, length(keyword_freq))],
    top_pairs = get_top_pairs(cooc_matrix, 20)
  )
  
  adjacency_list <- matrix_to_edgelist(cooc_top, min_weight = 1)
  
  list(
    adjacency = cooc_matrix,
    adjacency_top = cooc_top,
    edgelist = adjacency_list,
    keyword_freq = keyword_freq,
    metrics = network_metrics,
    communities = communities,
    summary = summary_stats,
    status = "success"
  )
}

#' Extract keywords from keyword field
#' @keywords internal
extract_keywords <- function(keyword_col) {
  if (is.null(keyword_col) || length(keyword_col) == 0) {
    return(list())
  }
  
  keywords_list <- lapply(as.character(keyword_col), function(x) {
    if (is.na(x) || x == "") return(character(0))
    kw <- strsplit(x, ";")[[1]]
    kw <- trimws(kw)
    kw <- kw[kw != ""]
    tolower(kw)
  })
  
  names(keywords_list) <- seq_along(keywords_list)
  keywords_list
}

#' Build co-occurrence matrix
#' @keywords internal
build_cooccurrence_matrix <- function(keywords_list) {
  # Deduplicate keywords within each document
  keywords_list <- lapply(keywords_list, unique)
  
  docs <- rep(seq_along(keywords_list), lengths(keywords_list))
  kws <- unlist(keywords_list)
  
  if (length(kws) == 0) return(matrix(0, 0, 0))
  
  kw_factor <- factor(kws)
  u_kws <- levels(kw_factor)
  n_kws <- length(u_kws)
  
  if (requireNamespace("Matrix", quietly = TRUE)) {
    dtm <- Matrix::sparseMatrix(
      i = docs,
      j = as.integer(kw_factor),
      x = 1,
      dims = c(length(keywords_list), n_kws)
    )
    cooc <- Matrix::crossprod(dtm)
    cooc <- as.matrix(cooc)
  } else {
    dtm <- table(docs, kw_factor)
    cooc <- as.matrix(crossprod(dtm))
  }
  
  dimnames(cooc) <- list(u_kws, u_kws)
  diag(cooc) <- 0
  
  cooc
}

#' Get top keywords by frequency
#' @keywords internal
get_top_keywords <- function(cooc_matrix, n) {
  freq <- colSums(cooc_matrix)
  n <- min(n, length(freq))
  names(sort(freq, decreasing = TRUE))[1:n]
}

#' Compute network metrics
#' @keywords internal
compute_network_metrics <- function(adj_matrix) {
  n <- nrow(adj_matrix)
  
  degree <- rowSums(adj_matrix > 0)
  strength <- rowSums(adj_matrix)
  
  betweenness <- calculate_betweenness(adj_matrix)
  closeness <- calculate_closeness(adj_matrix, strength)
  
  eigen_centrality <- calculate_eigenvector(adj_matrix)
  
  clustering_coef <- calculate_clustering(adj_matrix)
  
  density <- sum(adj_matrix > 0) / (n * (n - 1))
  
  modularity <- calculate_modularity(adj_matrix)
  
  small_world <- calculate_small_world(adj_matrix, clustering_coef, closeness)
  
  list(
    degree = sort(degree, decreasing = TRUE),
    strength = sort(strength, decreasing = TRUE),
    betweenness = sort(betweenness, decreasing = TRUE),
    closeness = sort(closeness, decreasing = TRUE),
    eigen_centrality = sort(eigen_centrality, decreasing = TRUE),
    clustering_coefficient = clustering_coef,
    density = density,
    modularity = modularity,
    is_small_world = small_world$sw_index > 1,
    small_world_index = small_world$sw_index,
    avg_path_length = small_world$avg_path_length,
    summary = data.frame(
      keyword = names(degree),
      degree = degree,
      strength = strength,
      betweenness = betweenness,
      closeness = closeness,
      eigenvector = eigen_centrality,
      stringsAsFactors = FALSE
    )
  )
}

#' Calculate betweenness centrality
#' @keywords internal
calculate_betweenness <- function(adj) {
  n <- nrow(adj)
  if (n <= 1) return(rep(0, n))
  
  betweenness <- rep(0, n)
  names(betweenness) <- rownames(adj)
  
  for (s in 1:n) {
    for (t in 1:n) {
      if (s != t) {
        paths <- find_all_shortest_paths(adj, s, t)
        if (length(paths) > 0) {
          for (v in setdiff(1:n, c(s, t))) {
            paths_through_v <- sum(sapply(paths, function(p) v %in% p))
            betweenness[v] <- betweenness[v] + paths_through_v / length(paths)
          }
        }
      }
    }
  }
  
  betweenness / 2
}

#' Find all shortest paths (BFS-based)
#' @keywords internal
find_all_shortest_paths <- function(adj, from, to) {
  n <- nrow(adj)
  if (from == to) return(list(c(from)))
  
  visited <- rep(FALSE, n)
  dist <- rep(Inf, n)
  dist[from] <- 0
  prev <- vector("list", n)
  
  queue <- from
  visited[from] <- TRUE
  
  while (length(queue) > 0) {
    u <- queue[1]
    queue <- queue[-1]
    
    neighbors <- which(adj[u, ] > 0 & !visited)
    for (v in neighbors) {
      if (dist[v] > dist[u] + 1) {
        dist[v] <- dist[u] + 1
        prev[[v]] <- list(u)
        if (!visited[v]) {
          visited[v] <- TRUE
          queue <- c(queue, v)
        }
      } else if (dist[v] == dist[u] + 1) {
        prev[[v]] <- c(prev[[v]], list(u))
      }
    }
  }
  
  if (is.infinite(dist[to])) return(list())
  
  reconstruct_paths(prev, from, to)
}

#' Reconstruct paths from predecessor list
#' @keywords internal
reconstruct_paths <- function(prev, from, to) {
  if (from == to) return(list(c(from)))
  
  paths <- list()
  stack <- list(c(to))
  
  while (length(stack) > 0) {
    current <- stack[[1]]
    stack <- stack[-1]
    node <- current[1]
    
    if (node == from) {
      paths <- c(paths, list(rev(current)))
    } else {
      for (p in prev[[node]]) {
        stack <- c(stack, list(c(p, current)))
      }
    }
  }
  
  paths
}

#' Calculate closeness centrality
#' @keywords internal
calculate_closeness <- function(adj, strength) {
  n <- nrow(adj)
  if (n <= 1) return(rep(1, n))
  
  closeness <- rep(0, n)
  names(closeness) <- rownames(adj)
  
  for (i in 1:n) {
    distances <- bfs_distances(adj, i)
    reachable <- sum(distances < Inf) - 1
    if (reachable > 0) {
      closeness[i] <- reachable / sum(distances[distances < Inf])
    }
  }
  
  closeness
}

#' BFS distances
#' @keywords internal
bfs_distances <- function(adj, from) {
  n <- nrow(adj)
  dist <- rep(Inf, n)
  dist[from] <- 0
  
  queue <- from
  while (length(queue) > 0) {
    u <- queue[1]
    queue <- queue[-1]
    
    neighbors <- which(adj[u, ] > 0)
    for (v in neighbors) {
      if (is.infinite(dist[v])) {
        dist[v] <- dist[u] + 1
        queue <- c(queue, v)
      }
    }
  }
  
  dist
}

#' Calculate eigenvector centrality (power iteration)
#' @keywords internal
calculate_eigenvector <- function(adj, max_iter = 100, tol = 1e-6) {
  n <- nrow(adj)
  if (n <= 1) return(rep(1, n))
  
  eigen <- rep(1, n)
  
  for (iter in 1:max_iter) {
    new_eigen <- adj %*% eigen
    new_eigen <- as.vector(new_eigen)
    new_eigen <- new_eigen / sqrt(sum(new_eigen^2))
    
    if (max(abs(new_eigen - eigen)) < tol) break
    eigen <- new_eigen
  }
  
  names(eigen) <- rownames(adj)
  eigen
}

#' Calculate clustering coefficient
#' @keywords internal
calculate_clustering <- function(adj) {
  n <- nrow(adj)
  if (n <= 1) return(0)
  
  clustering <- rep(0, n)
  names(clustering) <- rownames(adj)
  
  for (i in 1:n) {
    neighbors <- which(adj[i, ] > 0)
    k <- length(neighbors)
    
    if (k >= 2) {
      possible_triads <- k * (k - 1) / 2
      actual_triads <- 0
      
      for (j in neighbors) {
        for (l in neighbors) {
          if (j < l && adj[j, l] > 0) {
            actual_triads <- actual_triads + 1
          }
        }
      }
      
      clustering[i] <- actual_triads / possible_triads
    }
  }
  
  mean(clustering, na.rm = TRUE)
}

#' Calculate modularity (simplified)
#' @keywords internal
calculate_modularity <- function(adj) {
  n <- nrow(adj)
  if (n <= 1) return(0)
  
  m <- sum(adj) / 2
  if (m == 0) return(0)
  
  degree <- colSums(adj)
  
  modularity <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      modularity <- modularity + (adj[i, j] - degree[i] * degree[j] / (2 * m)) / (2 * m)
    }
  }
  
  modularity
}

#' Calculate small-world index
#' @keywords internal
calculate_small_world <- function(adj, clustering_coef, closeness) {
  n <- nrow(adj)
  if (n <= 2) return(list(sw_index = 0, avg_path_length = Inf))
  
  avg_pl <- mean(closeness[closeness > 0], na.rm = TRUE)
  if (is.nan(avg_pl)) avg_pl <- Inf
  
  random_clustering <- mean(colSums(adj > 0)) / n
  random_pl <- log(n) / log(mean(colSums(adj > 0)))
  
  if (random_clustering == 0 || random_pl == 0) {
    sw_index <- 0
  } else {
    sw_index <- (clustering_coef / random_clustering) / (avg_pl / random_pl)
  }
  
  list(
    sw_index = sw_index,
    avg_path_length = avg_pl
  )
}

#' Detect communities (Louvain-like simplification)
#' @keywords internal
detect_communities <- function(adj, method = "louvain") {
  n <- nrow(adj)
  if (n <= 1) return(list(membership = rep(1, n), modularity = 0))
  
  membership <- seq_len(n)
  modularity <- calculate_modularity(adj)
  
  improved <- TRUE
  iterations <- 0
  max_iterations <- 50
  
  while (improved && iterations < max_iterations) {
    improved <- FALSE
    iterations <- iterations + 1
    
    for (i in sample(n)) {
      current_community <- membership[i]
      neighbors <- which(adj[i, ] > 0)
      
      if (length(neighbors) == 0) next
      
      neighbor_communities <- unique(membership[neighbors])
      
      best_modularity <- modularity
      best_community <- current_community
      
      for (community in neighbor_communities) {
        if (community == current_community) next
        
        test_membership <- membership
        test_membership[i] <- community
        
        test_mod <- calculate_modularity_for_membership(adj, test_membership)
        
        if (test_mod > best_modularity) {
          best_modularity <- test_mod
          best_community <- community
          improved <- TRUE
        }
      }
      
      if (best_community != current_community) {
        membership[i] <- best_community
        modularity <- best_modularity
      }
    }
  }
  
  unique_communities <- unique(membership)
  membership <- match(membership, unique_communities)
  names(membership) <- rownames(adj)
  
  list(
    membership = membership,
    n_communities = length(unique(membership)),
    modularity = modularity,
    community_sizes = table(membership)
  )
}

#' Calculate modularity for membership vector
#' @keywords internal
calculate_modularity_for_membership <- function(adj, membership) {
  n <- nrow(adj)
  m <- sum(adj) / 2
  if (m == 0) return(0)
  
  degree <- colSums(adj)
  
  modularity <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      if (membership[i] == membership[j]) {
        modularity <- modularity + (adj[i, j] - degree[i] * degree[j] / (2 * m))
      }
    }
  }
  
  modularity / (2 * m)
}

#' Get top pairs
#' @keywords internal
get_top_pairs <- function(adj, n) {
  pairs <- which(adj > 0, arr.ind = TRUE)
  if (nrow(pairs) == 0) return(data.frame())
  
  pairs <- pairs[pairs[, 1] < pairs[, 2], , drop = FALSE]
  
  weights <- apply(pairs, 1, function(p) adj[p[1], p[2]])
  
  top_idx <- order(weights, decreasing = TRUE)[1:min(n, length(weights))]
  
  data.frame(
    keyword1 = rownames(adj)[pairs[top_idx, 1]],
    keyword2 = rownames(adj)[pairs[top_idx, 2]],
    weight = weights[top_idx],
    stringsAsFactors = FALSE
  )
}

#' Convert matrix to edge list
#' @keywords internal
matrix_to_edgelist <- function(adj, min_weight = 1) {
  pairs <- which(adj >= min_weight, arr.ind = TRUE)
  if (nrow(pairs) == 0) return(data.frame())
  
  pairs <- pairs[pairs[, 1] < pairs[, 2], , drop = FALSE]
  
  weights <- apply(pairs, 1, function(p) adj[p[1], p[2]])
  
  data.frame(
    from = rownames(adj)[pairs[, 1]],
    to = rownames(adj)[pairs[, 2]],
    weight = weights,
    stringsAsFactors = FALSE
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b