# ============================================================================
# m1_compute_author_advanced.R - Advanced Author Analysis
# ============================================================================
# Author career trajectory, collaboration networks, centrality metrics

#' Compute author career trajectory
#'
#' @param input Bibliographic data frame
#' @param config Configuration list
#' @return List with career metrics per author
#' @export
compute_m1_author_career <- function(input, config = biblio_config()) {
  au_col <- "AU"
  py_col <- "PY"
  tc_col <- "TC"
  
  if (!au_col %in% names(input) || !py_col %in% names(input)) {
    return(list(status = "error: missing author or year column"))
  }
  
  # Extract authors and their papers
  author_data <- extract_author_career_papers(input, au_col, py_col, tc_col)
  
  if (length(author_data) == 0) {
    return(list(status = "error: no authors found"))
  }
  
  # Compute career metrics
  safe_compute_career <- function(author) {
    papers <- author_data[[author]]
    if (is.null(papers) || !is.list(papers)) {
      return(list(
        author = author,
        total_papers = 0L,
        first_publication_year = NA_integer_,
        last_publication_year = NA_integer_,
        career_length = NA_integer_,
        total_citations = 0,
        mean_citations = NA_real_,
        h_index = 0L,
        g_index = 0L,
        m_index = NA_real_,
        citations_per_year = NA_real_,
        productivity_trend = NA_real_,
        citation_trend = NA_real_,
        active_years = 0L,
        publication_gaps = list(n_gaps = 0L, max_gap = 0L, gap_years = integer(0)),
        career_stage = NA_character_
      ))
    }
    
    years <- papers$year
    citations <- papers$citations
    
    # Handle NULL or non-numeric years
    if (is.null(years) || length(years) == 0 || all(is.na(years))) {
      years_val <- NA_integer_
      first_year <- NA_integer_
      last_year <- NA_integer_
    } else {
      years_val <- years[!is.na(years)]
      first_year <- as.integer(min(years_val, na.rm = TRUE))
      last_year <- as.integer(max(years_val, na.rm = TRUE))
    }
    
    career_len <- if (is.na(first_year) || is.na(last_year)) NA_integer_ else last_year - first_year + 1L
    
    # Handle citations
    if (is.null(citations) || length(citations) == 0) {
      citations_val <- numeric(0)
    } else {
      citations_val <- citations[!is.na(citations)]
    }
    
    list(
      author = author,
      total_papers = length(years),
      first_publication_year = first_year,
      last_publication_year = last_year,
      career_length = career_len,
      total_citations = sum(citations_val, na.rm = TRUE),
      mean_citations = if (length(citations_val) > 0) mean(citations_val, na.rm = TRUE) else NA_real_,
      h_index = compute_h_index(citations_val),
      g_index = compute_g_index(citations_val),
      m_index = compute_m_index(citations_val, years),
      citations_per_year = if (!is.na(career_len) && career_len > 0) sum(citations_val, na.rm = TRUE) / career_len else NA_real_,
      productivity_trend = compute_productivity_trend(years),
      citation_trend = compute_citation_trend(years, citations),
      active_years = length(unique(years_val)),
      publication_gaps = compute_publication_gaps(years),
      career_stage = determine_career_stage(first_year, last_year)
    )
  }
  
  career_metrics <- lapply(names(author_data), safe_compute_career)
  names(career_metrics) <- names(author_data)
  
  # Aggregate statistics - flatten nested lists before converting to data frame
  safe_flatten <- function(x) {
    x_flat <- as.list(x)
    pg <- x_flat$publication_gaps
    if (!is.null(pg) && is.list(pg)) {
      x_flat$publication_gaps_n <- if (!is.null(pg$n_gaps)) pg$n_gaps[1] else 0
      x_flat$publication_gaps_max <- if (!is.null(pg$max_gap)) pg$max_gap[1] else 0
    } else {
      x_flat$publication_gaps_n <- 0
      x_flat$publication_gaps_max <- 0
    }
    x_flat$publication_gaps <- NULL
    as.data.frame(x_flat, stringsAsFactors = FALSE)
  }
  
  career_df <- tryCatch({
    do.call(rbind, lapply(career_metrics, safe_flatten))
  }, error = function(e) {
    data.frame()
  })
  
  # Handle empty data frame
  if (is.null(career_df) || nrow(career_df) == 0) {
    return(list(
      career_metrics = career_metrics,
      career_df = data.frame(),
      top_by_papers = data.frame(),
      top_by_citations = data.frame(),
      top_by_h_index = data.frame(),
      rising_stars = data.frame(),
      established_stars = data.frame(),
      n_authors = length(author_data),
      status = "success"
    ))
  }

  career_df$display_author <- vapply(career_df$author, m1_author_display_label, character(1))
  
  # Top authors by various metrics
  top_by_papers <- head(career_df[order(-as.numeric(career_df$total_papers)), ], 20)
  top_by_citations <- head(career_df[order(-as.numeric(career_df$total_citations)), ], 20)
  top_by_h_index <- head(career_df[order(-as.numeric(career_df$h_index)), ], 20)
  trajectories <- build_author_trajectories(author_data, top_by_citations$author)
  
  # Rising stars (high M-index, early career)
  rising <- career_df[career_df$career_stage == "early", , drop = FALSE]
  rising_stars <- if (nrow(rising) > 0) head(rising[order(-as.numeric(rising$m_index)), , drop = FALSE], 10) else data.frame()
  
  # Established researchers (high h-index, mid-late career)
  established <- career_df[career_df$career_stage %in% c("mid", "late"), , drop = FALSE]
  established_stars <- if (nrow(established) > 0) head(established[order(-as.numeric(established$h_index)), , drop = FALSE], 10) else data.frame()
  
  list(
    career_metrics = career_metrics,
    career_df = career_df,
    top_by_papers = top_by_papers,
    top_by_citations = top_by_citations,
    top_by_h_index = top_by_h_index,
    rising_stars = rising_stars,
    established_stars = established_stars,
    trajectories = trajectories,
    n_authors = length(author_data),
    status = "success"
  )
}

#' Build cumulative author trajectories
#' @keywords internal
build_author_trajectories <- function(author_data, authors = character()) {
  if (length(author_data) == 0) {
    return(data.frame(
      author = character(),
      year = numeric(),
      papers = integer(),
      citations = numeric(),
      cumulative_papers = numeric(),
      cumulative_citations = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  if (length(authors) == 0) {
    authors <- names(author_data)
  }

  authors <- unique(authors[authors %in% names(author_data)])
  authors <- authors[seq_len(min(length(authors), 10))]

  trajectory_rows <- lapply(authors, function(author) {
    papers <- author_data[[author]]
    if (is.null(papers) || !is.list(papers) || is.null(papers$year) || length(papers$year) == 0) {
      return(NULL)
    }

    years <- as.numeric(papers$year)
    citations <- as.numeric(papers$citations)
    valid <- !is.na(years)
    if (!any(valid)) {
      return(NULL)
    }

    yearly <- stats::aggregate(
      cbind(papers = rep(1L, sum(valid)), citations = citations[valid]) ~ year,
      data = data.frame(
        year = years[valid],
        citations = citations[valid]
      ),
      FUN = sum
    )
    yearly <- yearly[order(yearly$year), , drop = FALSE]
    yearly$cumulative_papers <- cumsum(yearly$papers)
    yearly$cumulative_citations <- cumsum(yearly$citations)
    yearly$author <- author
    yearly
  })

  out <- do.call(rbind, trajectory_rows[!vapply(trajectory_rows, is.null, logical(1))])
  if (is.null(out)) {
    return(data.frame(
      author = character(),
      year = numeric(),
      papers = integer(),
      citations = numeric(),
      cumulative_papers = numeric(),
      cumulative_citations = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  rownames(out) <- NULL
  out
}

#' Extract author papers
#' @keywords internal
extract_author_career_papers <- function(input, au_col, py_col, tc_col) {
  authors_list <- strsplit(as.character(input[[au_col]]), ";")
  years <- as.numeric(input[[py_col]])
  citations <- if (tc_col %in% names(input)) as.numeric(input[[tc_col]]) else rep(0, nrow(input))
  
  # Handle NAs
  years[is.na(years)] <- NA_integer_
  citations[is.na(citations)] <- 0
  
  # Create author-paper mapping
  author_papers <- list()
  
  for (i in seq_along(authors_list)) {
    authors <- trimws(authors_list[[i]])
    authors <- authors[authors != ""]
    
    for (author in authors) {
      author <- normalize_author_name(author)
      
      if (!author %in% names(author_papers)) {
        author_papers[[author]] <- list(
          year = numeric(0),
          citations = numeric(0)
        )
      }
      
      # Only add valid year values
      if (!is.na(years[i])) {
        author_papers[[author]]$year <- c(author_papers[[author]]$year, years[i])
      }
      author_papers[[author]]$citations <- c(author_papers[[author]]$citations, citations[i])
    }
  }
  
  author_papers
}

#' Convert canonical author key into a journal-facing display label
#' @keywords internal
m1_author_display_label <- function(name) {
  raw_name <- trimws(as.character(name))
  if (!nzchar(raw_name) || is.na(raw_name)) {
    return(raw_name)
  }

  if (grepl(",", raw_name, fixed = TRUE)) {
    split_name <- strsplit(raw_name, ",", fixed = TRUE)[[1]]
    last_name <- trimws(split_name[1])
    initials <- gsub("\\s+", " ", trimws(paste(split_name[-1], collapse = " ")))
    initials <- gsub("[^A-Za-z ]", "", initials)
    initials <- trimws(initials)
    return(trimws(paste(last_name, initials)))
  }

  raw_name
}

#' Normalize author name
#' @keywords internal
normalize_author_name <- function(name) {
  raw_name <- trimws(as.character(name))
  if (!nzchar(raw_name)) return(raw_name)

  to_title <- function(x) {
    parts <- unlist(strsplit(tolower(x), "\\s+"))
    parts <- parts[nzchar(parts)]
    paste0(toupper(substr(parts, 1, 1)), substring(parts, 2), collapse = " ")
  }

  if (grepl(",", raw_name, fixed = TRUE)) {
    split_name <- strsplit(raw_name, ",", fixed = TRUE)[[1]]
    last_name <- trimws(split_name[1])
    given_name <- paste(trimws(split_name[-1]), collapse = " ")
    given_parts <- unlist(strsplit(gsub("[[:punct:]]", " ", given_name), "\\s+"))
    given_parts <- given_parts[nzchar(given_parts)]
    initials <- paste(toupper(substr(given_parts, 1, 1)), collapse = "")
    return(paste(to_title(last_name), initials, sep = ", "))
  }

  clean_name <- gsub("[[:punct:]]", " ", raw_name)
  clean_name <- gsub("\\s+", " ", clean_name)
  parts <- unlist(strsplit(trimws(clean_name), "\\s+"))
  if (length(parts) == 0) return(raw_name)

  # Heuristic: "Lastname Initial" or "Lastname Initial Initial" is common in
  # bibliographic exports and should not be inverted to "Initial, L".
  if (length(parts) >= 2 && nchar(parts[1]) > 1 && all(nchar(parts[-1]) <= 2)) {
    last_name <- parts[1]
    initials <- paste(toupper(substr(parts[-1], 1, 1)), collapse = "")
    return(paste(to_title(last_name), initials, sep = ", "))
  }

  last_name <- parts[length(parts)]
  initials <- if (length(parts) > 1) paste(toupper(substr(parts[-length(parts)], 1, 1)), collapse = "") else ""
  paste(to_title(last_name), initials, sep = ", ")
}

#' Compute M-index (h-index / career length)
#' @keywords internal
compute_m_index <- function(citations, years) {
  h <- compute_h_index(citations)
  years_valid <- years[!is.na(years)]
  if (length(years_valid) == 0) return(NA_real_)
  career_length <- as.numeric(max(years_valid)) - as.numeric(min(years_valid)) + 1
  if (career_length <= 0 || is.na(career_length)) return(NA_real_)
  h / career_length
}

#' Compute productivity trend
#' @keywords internal
compute_productivity_trend <- function(years) {
  years_valid <- years[!is.na(years)]
  if (length(years_valid) < 3) return(NA_real_)
  
  year_counts <- table(years_valid)
  years_unique <- as.numeric(names(year_counts))
  counts <- as.numeric(year_counts)
  
  if (length(years_unique) < 3) return(NA_real_)
  
  fit <- lm(counts ~ years_unique)
  slope <- coef(fit)[2]
  if (length(slope) > 1) slope <- slope[1]
  slope
}

#' Compute citation trend
#' @keywords internal
compute_citation_trend <- function(years, citations) {
  years_valid <- years[!is.na(years) & !is.na(citations)]
  citations_valid <- citations[!is.na(years) & !is.na(citations)]
  if (length(years_valid) < 3) return(NA_real_)
  
  # Mean citations per year
  year_citations <- aggregate(citations_valid ~ years_valid, FUN = sum)
  
  if (nrow(year_citations) < 3) return(NA_real_)
  
  fit <- lm(citations_valid ~ years_valid, data = year_citations)
  slope <- coef(fit)[2]
  if (length(slope) > 1) slope <- slope[1]
  slope
}

#' Compute publication gaps
#' @keywords internal
compute_publication_gaps <- function(years) {
  years_valid <- years[!is.na(years)]
  if (length(years_valid) < 2) return(list(n_gaps = 0L, max_gap = 0L, gap_years = integer(0)))
  
  years_sorted <- sort(unique(years_valid))
  gaps <- diff(years_sorted)
  gaps <- gaps[gaps > 1]  # Gap of 2+ years
  
  list(
    n_gaps = length(gaps),
    max_gap = if (length(gaps) > 0) as.integer(max(gaps)) else 0L,
    gap_years = as.integer(gaps)
  )
}

#' Determine career stage
#' @keywords internal
determine_career_stage <- function(first_year, last_year) {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  career_start_age <- current_year - first_year
  
  if (career_start_age <= 5) {
    "early"
  } else if (career_start_age <= 15) {
    "mid"
  } else {
    "late"
  }
}

#' Compute author collaboration network
#'
#' @param input Bibliographic data frame
#' @param config Configuration list
#' @return List with network metrics
#' @export
compute_m1_author_network <- function(input, config = biblio_config()) {
  au_col <- "AU"
  
  if (!au_col %in% names(input)) {
    return(list(status = "error: no author column"))
  }
  
  # Create co-authorship matrix
  coauthor_matrix <- create_coauthor_matrix(input, au_col)
  
  if (is.null(coauthor_matrix) || nrow(coauthor_matrix) == 0) {
    return(list(status = "error: could not create co-author matrix"))
  }
  
  # Network metrics
  n_authors <- nrow(coauthor_matrix)
  n_edges <- sum(coauthor_matrix > 0) / 2
  
  # Degree centrality
  degree_centrality <- rowSums(coauthor_matrix > 0)
  
  # Weighted degree (strength)
  strength <- rowSums(coauthor_matrix)
  
  # Betweenness centrality (approximate)
  betweenness <- compute_network_betweenness(coauthor_matrix)
  
  # Closeness centrality
  closeness <- compute_network_closeness(coauthor_matrix)
  
  # Eigenvector centrality
  eigenvector <- compute_network_eigenvector(coauthor_matrix)
  
  # Clustering coefficient
  clustering <- compute_network_clustering(coauthor_matrix)
  
  # Create author metrics dataframe
  author_metrics <- data.frame(
    author = rownames(coauthor_matrix),
    degree = degree_centrality,
    strength = strength,
    betweenness = betweenness,
    closeness = closeness,
    eigenvector = eigenvector,
    clustering = clustering,
    stringsAsFactors = FALSE
  )
  
  # Sort by strength
  author_metrics <- author_metrics[order(-author_metrics$strength), ]
  
  # Network-level metrics
  density <- n_edges / (n_authors * (n_authors - 1) / 2)
  
  # Components
  components <- find_network_components(coauthor_matrix)
  n_components <- length(unique(components))
  
  # Modularity (approximate)
  modularity <- compute_network_modularity(coauthor_matrix, components)
  
  list(
    coauthor_matrix = coauthor_matrix,
    author_metrics = author_metrics,
    network_metrics = list(
      n_authors = n_authors,
      n_edges = n_edges,
      density = density,
      n_components = n_components,
      modularity = modularity,
      average_degree = mean(degree_centrality),
      average_clustering = mean(clustering, na.rm = TRUE)
    ),
    top_by_degree = head(author_metrics[order(-author_metrics$degree), ], 20),
    top_by_betweenness = head(author_metrics[order(-author_metrics$betweenness), ], 20),
    top_by_eigenvector = head(author_metrics[order(-author_metrics$eigenvector), ], 20),
    components = components,
    status = "success"
  )
}

#' Create co-authorship matrix
#' @keywords internal
create_coauthor_matrix <- function(input, au_col) {
  authors_list <- strsplit(as.character(input[[au_col]]), ";")
  
  # Get unique authors
  all_authors <- unique(unlist(lapply(authors_list, function(x) {
    trimws(x[x != ""])
  })))
  
  if (length(all_authors) == 0) return(NULL)
  
  all_authors <- all_authors[!is.na(all_authors)]
  
  # Limit to top authors if too many
  if (length(all_authors) > 1000) {
    author_freq <- table(unlist(authors_list))
    all_authors <- names(sort(author_freq, decreasing = TRUE))[1:1000]
  }
  
  n_authors <- length(all_authors)
  
  # Initialize matrix
  coauthor_matrix <- matrix(0, n_authors, n_authors)
  rownames(coauthor_matrix) <- colnames(coauthor_matrix) <- all_authors
  
  # Fill matrix
  for (authors in authors_list) {
    authors <- trimws(authors)
    authors <- authors[authors != "" & authors %in% all_authors]
    
    if (length(authors) > 1) {
      for (i in 1:(length(authors) - 1)) {
        for (j in (i + 1):length(authors)) {
          idx_i <- match(authors[i], all_authors)
          idx_j <- match(authors[j], all_authors)
          
          if (!is.na(idx_i) && !is.na(idx_j)) {
            coauthor_matrix[idx_i, idx_j] <- coauthor_matrix[idx_i, idx_j] + 1
            coauthor_matrix[idx_j, idx_i] <- coauthor_matrix[idx_j, idx_i] + 1
          }
        }
      }
    }
  }
  
  coauthor_matrix
}

#' Compute network betweenness (approximate)
#' @keywords internal
compute_network_betweenness <- function(adj_matrix) {
  n <- nrow(adj_matrix)
  betweenness <- numeric(n)
  
  # Simplified: count shortest paths through each node
  for (s in 1:n) {
    # BFS from s
    dist <- rep(Inf, n)
    dist[s] <- 0
    prev <- rep(NA, n)
    
    queue <- s
    while (length(queue) > 0) {
      v <- queue[1]
      queue <- queue[-1]
      
      neighbors <- which(adj_matrix[v, ] > 0)
      for (w in neighbors) {
        if (dist[w] == Inf) {
          dist[w] <- dist[v] + 1
          prev[w] <- v
          queue <- c(queue, w)
        }
      }
    }
    
    # Count nodes that pass through s
    for (t in 1:n) {
      if (t != s) {
        path <- t
        while (!is.na(prev[path[1]])) {
          path <- c(prev[path[1]], path)
        }
        if (length(path) > 2) {
          betweenness[path[-c(1, length(path))]] <- betweenness[path[-c(1, length(path))]] + 1
        }
      }
    }
  }
  
  # Normalize
  betweenness / (n * (n - 1) / 2)
}

#' Compute network closeness
#' @keywords internal
compute_network_closeness <- function(adj_matrix) {
  n <- nrow(adj_matrix)
  closeness <- numeric(n)
  
  for (i in 1:n) {
    # BFS for distances
    dist <- rep(Inf, n)
    dist[i] <- 0
    
    queue <- i
    while (length(queue) > 0) {
      v <- queue[1]
      queue <- queue[-1]
      
      neighbors <- which(adj_matrix[v, ] > 0)
      for (w in neighbors) {
        if (dist[w] == Inf) {
          dist[w] <- dist[v] + 1
          queue <- c(queue, w)
        }
      }
    }
    
    reachable <- sum(is.finite(dist)) - 1
    if (reachable > 0) {
      closeness[i] <- reachable / sum(dist[is.finite(dist)])
    }
  }
  
  closeness
}

#' Compute network eigenvector centrality
#' @keywords internal
compute_network_eigenvector <- function(adj_matrix) {
  n <- nrow(adj_matrix)
  
  # Power iteration
  eigenvec <- rep(1, n)
  
  for (iter in 1:100) {
    eigenvec_new <- adj_matrix %*% eigenvec
    eigenvec_new <- as.numeric(eigenvec_new)
    eigenvec_new <- eigenvec_new / max(eigenvec_new)
    
    if (max(abs(eigenvec_new - eigenvec)) < 1e-6) {
      break
    }
    eigenvec <- eigenvec_new
  }
  
  as.numeric(eigenvec)
}

#' Compute network clustering coefficient
#' @keywords internal
compute_network_clustering <- function(adj_matrix) {
  n <- nrow(adj_matrix)
  clustering <- numeric(n)
  
  binary <- adj_matrix > 0
  
  for (i in 1:n) {
    neighbors <- which(binary[i, ])
    k <- length(neighbors)
    
    if (k < 2) {
      clustering[i] <- 0
      next
    }
    
    # Count triangles
    n_triangles <- 0
    for (j in 1:(k - 1)) {
      for (l in (j + 1):k) {
        if (binary[neighbors[j], neighbors[l]]) {
          n_triangles <- n_triangles + 1
        }
      }
    }
    
    # Maximum possible triangles
    n_possible <- k * (k - 1) / 2
    
    clustering[i] <- if (n_possible > 0) n_triangles / n_possible else 0
  }
  
  clustering
}

#' Find network components
#' @keywords internal
find_network_components <- function(adj_matrix) {
  n <- nrow(adj_matrix)
  binary <- adj_matrix > 0
  
  visited <- rep(FALSE, n)
  components <- rep(NA, n)
  component_id <- 0
  
  for (i in 1:n) {
    if (!visited[i]) {
      component_id <- component_id + 1
      
      # BFS
      queue <- i
      visited[i] <- TRUE
      
      while (length(queue) > 0) {
        v <- queue[1]
        queue <- queue[-1]
        components[v] <- component_id
        
        neighbors <- which(binary[v, ])
        for (w in neighbors) {
          if (!visited[w]) {
            visited[w] <- TRUE
            queue <- c(queue, w)
          }
        }
      }
    }
  }
  
  components
}

#' Compute network modularity
#' @keywords internal
compute_network_modularity <- function(adj_matrix, components) {
  n <- nrow(adj_matrix)
  m <- sum(adj_matrix) / 2
  
  if (m == 0) return(0)
  
  Q <- 0
  unique_components <- unique(components)
  
  for (c in unique_components) {
    nodes_in_c <- which(components == c)
    
    # Edges within component
    edges_within <- sum(adj_matrix[nodes_in_c, nodes_in_c]) / 2
    
    # Total degree of nodes in component
    degree_within <- sum(rowSums(adj_matrix[nodes_in_c, , drop = FALSE]))
    
    Q <- Q + (edges_within / m - (degree_within / (2 * m))^2)
  }
  
  Q
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
