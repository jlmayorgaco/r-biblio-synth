# ============================================================================
# bslr_mapping.R - Internal bibliometric mapping for B-SLR workflows
# ============================================================================

#' Build bibliometric mapping outputs for B-SLR
#'
#' @param input Bibliographic data frame.
#' @param protocol B-SLR protocol list or path.
#' @param config Configuration list.
#' @return A list with clusters, rankings, metrics, and plot-ready data.
#' @keywords internal
bslr_build_bibliometric_map <- function(input,
                                        protocol,
                                        config = biblio_config()) {
  protocol <- bslr_read_protocol(protocol)
  approach <- tolower(trimws(as.character(protocol$bibliometric$approach %||% "bibliographic_coupling")))

  doc_tbl <- bslr_prepare_document_index(input)
  if (nrow(doc_tbl) == 0) {
    return(list(
      status = "error: no documents available",
      approach = approach,
      document_clusters = data.frame(),
      cluster_summary = data.frame(),
      sample_ranking = data.frame(),
      sample_selected = data.frame(),
      metrics = list(),
      plots = list()
    ))
  }

  if (identical(approach, "co_citation")) {
    network <- bslr_build_cocitation_document_map(doc_tbl, protocol, config)
  } else {
    network <- bslr_build_coupling_document_map(doc_tbl, protocol, config)
  }

  if ((network$metrics$n_clusters %||% 0) == 0) {
    fallback <- bslr_build_keyword_similarity_map(doc_tbl, protocol, config)
    fallback$metrics$primary_approach <- approach
    fallback$metrics$fallback_used <- "keyword_similarity"
    return(fallback)
  }

  network
}

bslr_prepare_document_index <- function(input) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(data.frame())
  }

  input <- data.frame(input, stringsAsFactors = FALSE)
  if (!"M0_DOC_ID" %in% names(input)) {
    input$M0_DOC_ID <- seq_len(nrow(input))
  }

  input$doc_key <- as.character(input$M0_DOC_ID)
  if ("TI" %in% names(input)) {
    input$title <- as.character(input$TI)
  } else {
    input$title <- paste("Document", seq_len(nrow(input)))
  }
  input$year <- suppressWarnings(as.integer(input$PY %||% NA_integer_))
  input$citations <- suppressWarnings(as.numeric(input$TC %||% 0))
  input$doi <- as.character(input$DI %||% NA_character_)
  input$keywords_joined <- bslr_collect_keyword_strings(input)
  input
}

bslr_build_coupling_document_map <- function(doc_tbl, protocol, config) {
  ref_lists <- bslr_extract_reference_lists(doc_tbl$CR %||% rep(NA_character_, nrow(doc_tbl)))
  edges <- bslr_build_coupling_edges(doc_tbl$doc_key, ref_lists)
  bslr_finalize_document_clusters(
    doc_tbl = doc_tbl,
    edges = edges,
    protocol = protocol,
    config = config,
    approach = "bibliographic_coupling"
  )
}

bslr_build_keyword_similarity_map <- function(doc_tbl, protocol, config) {
  keyword_lists <- lapply(doc_tbl$keywords_joined, function(x) {
    parts <- trimws(unlist(strsplit(as.character(x), ";", fixed = TRUE)))
    parts <- tolower(parts)
    parts <- gsub("[^a-z0-9 ]", "", parts)
    unique(parts[nzchar(parts)])
  })
  edges <- bslr_build_coupling_edges(doc_tbl$doc_key, keyword_lists)
  bslr_finalize_document_clusters(
    doc_tbl = doc_tbl,
    edges = edges,
    protocol = protocol,
    config = config,
    approach = "keyword_similarity"
  )
}

bslr_build_cocitation_document_map <- function(doc_tbl, protocol, config) {
  ref_lists <- bslr_extract_reference_lists(doc_tbl$CR %||% rep(NA_character_, nrow(doc_tbl)))
  ref_edges <- bslr_build_cocitation_edges(ref_lists)
  ref_clusters <- bslr_cluster_reference_network(ref_edges, protocol)
  document_assignments <- bslr_assign_documents_from_reference_clusters(doc_tbl$doc_key, ref_lists, ref_clusters)

  edges <- bslr_document_edges_from_assignments(document_assignments)
  bslr_finalize_document_clusters(
    doc_tbl = doc_tbl,
    edges = edges,
    protocol = protocol,
    config = config,
    approach = "co_citation",
    forced_membership = document_assignments
  )
}

bslr_build_coupling_edges <- function(doc_keys, ref_lists) {
  n <- length(ref_lists)
  out <- list()
  idx <- 1L

  for (i in seq_len(max(0, n - 1L))) {
    refs_i <- unique(ref_lists[[i]])
    if (length(refs_i) == 0) next
    for (j in seq.int(i + 1L, n)) {
      refs_j <- unique(ref_lists[[j]])
      if (length(refs_j) == 0) next
      shared <- intersect(refs_i, refs_j)
      if (length(shared) > 0) {
        out[[idx]] <- data.frame(
          from = doc_keys[i],
          to = doc_keys[j],
          weight = length(shared),
          stringsAsFactors = FALSE
        )
        idx <- idx + 1L
      }
    }
  }

  if (length(out) == 0) {
    return(data.frame(from = character(), to = character(), weight = numeric(), stringsAsFactors = FALSE))
  }

  dplyr::bind_rows(out)
}

bslr_build_cocitation_edges <- function(ref_lists) {
  counts <- list()
  for (refs in ref_lists) {
    refs <- unique(refs)
    if (length(refs) < 2) next
    pairs <- utils::combn(sort(refs), 2, simplify = FALSE)
    for (pair in pairs) {
      key <- paste(pair, collapse = "::")
      counts[[key]] <- (counts[[key]] %||% 0L) + 1L
    }
  }

  if (length(counts) == 0) {
    return(data.frame(from = character(), to = character(), weight = numeric(), stringsAsFactors = FALSE))
  }

  data.frame(
    from = vapply(strsplit(names(counts), "::", fixed = TRUE), `[`, character(1), 1),
    to = vapply(strsplit(names(counts), "::", fixed = TRUE), `[`, character(1), 2),
    weight = as.numeric(unlist(counts)),
    stringsAsFactors = FALSE
  )
}

bslr_cluster_reference_network <- function(ref_edges, protocol) {
  if (!is.data.frame(ref_edges) || nrow(ref_edges) == 0) {
    return(data.frame(reference = character(), cluster_id = integer(), stringsAsFactors = FALSE))
  }
  bslr_cluster_graph(
    edges = ref_edges,
    nodes = unique(c(ref_edges$from, ref_edges$to)),
    min_cluster_size = protocol$bibliometric$min_cluster_size %||% 5L
  )
}

bslr_assign_documents_from_reference_clusters <- function(doc_keys, ref_lists, ref_clusters) {
  if (!is.data.frame(ref_clusters) || nrow(ref_clusters) == 0) {
    return(data.frame(doc_key = doc_keys, cluster_id = 0L, cluster_support = 0, stringsAsFactors = FALSE))
  }

  ref_map <- stats::setNames(ref_clusters$cluster_id, ref_clusters$reference)
  rows <- lapply(seq_along(doc_keys), function(i) {
    refs <- unique(ref_lists[[i]])
    if (length(refs) == 0) {
      return(data.frame(doc_key = doc_keys[i], cluster_id = 0L, cluster_support = 0, stringsAsFactors = FALSE))
    }
    matched <- ref_map[refs]
    matched <- matched[!is.na(matched) & matched > 0]
    if (length(matched) == 0) {
      return(data.frame(doc_key = doc_keys[i], cluster_id = 0L, cluster_support = 0, stringsAsFactors = FALSE))
    }
    cluster_counts <- sort(table(matched), decreasing = TRUE)
    data.frame(
      doc_key = doc_keys[i],
      cluster_id = as.integer(names(cluster_counts)[1]),
      cluster_support = as.integer(cluster_counts[1]),
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(rows)
}

bslr_document_edges_from_assignments <- function(assignments) {
  assignments <- assignments[assignments$cluster_id > 0, , drop = FALSE]
  if (nrow(assignments) < 2) {
    return(data.frame(from = character(), to = character(), weight = numeric(), stringsAsFactors = FALSE))
  }

  rows <- list()
  idx <- 1L
  split_docs <- split(assignments, assignments$cluster_id)
  for (cluster_docs in split_docs) {
    if (nrow(cluster_docs) < 2) next
    combos <- utils::combn(cluster_docs$doc_key, 2, simplify = FALSE)
    for (pair in combos) {
      rows[[idx]] <- data.frame(
        from = pair[1],
        to = pair[2],
        weight = 1,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }

  if (length(rows) == 0) {
    return(data.frame(from = character(), to = character(), weight = numeric(), stringsAsFactors = FALSE))
  }

  dplyr::bind_rows(rows)
}

bslr_finalize_document_clusters <- function(doc_tbl,
                                            edges,
                                            protocol,
                                            config,
                                            approach,
                                            forced_membership = NULL) {
  min_cluster_size <- as.integer(protocol$bibliometric$min_cluster_size %||% 5L)
  cluster_membership <- if (is.data.frame(forced_membership) && nrow(forced_membership) > 0) {
    forced_membership[, c("doc_key", "cluster_id"), drop = FALSE]
  } else {
    bslr_cluster_graph(edges, doc_tbl$doc_key, min_cluster_size)
  }

  doc_clusters <- dplyr::left_join(
    doc_tbl,
    cluster_membership,
    by = "doc_key"
  )
  doc_clusters$cluster_id[is.na(doc_clusters$cluster_id)] <- 0L
  doc_clusters$cluster_id <- as.integer(doc_clusters$cluster_id)
  doc_clusters$cluster_label <- bslr_label_clusters(doc_clusters)
  doc_clusters$weighted_degree <- bslr_weighted_degree(doc_clusters$doc_key, edges)
  doc_clusters$age_normalized_citations <- bslr_age_normalized_citations(doc_clusters$citations, doc_clusters$year)
  doc_clusters$ranking_score <- bslr_rank_documents(doc_clusters, protocol)

  cluster_summary <- bslr_build_cluster_summary(doc_clusters)
  sample_ranking <- doc_clusters[doc_clusters$cluster_id > 0, c(
    "doc_key", "cluster_id", "cluster_label", "title", "year", "doi",
    "citations", "age_normalized_citations", "weighted_degree", "ranking_score"
  ), drop = FALSE]
  sample_ranking <- sample_ranking[order(sample_ranking$cluster_id, -sample_ranking$ranking_score, -sample_ranking$citations), , drop = FALSE]

  sample_top_n <- as.integer(protocol$bibliometric$sample_top_n %||% 5L)
  sample_selected <- do.call(
    rbind,
    lapply(split(sample_ranking, sample_ranking$cluster_id), function(df) utils::head(df, sample_top_n))
  )
  if (is.null(sample_selected)) {
    sample_selected <- sample_ranking[0, , drop = FALSE]
  }

  metrics <- list(
    n_documents = nrow(doc_tbl),
    n_edges = if (is.data.frame(edges)) nrow(edges) else 0L,
    n_clustered_documents = sum(doc_clusters$cluster_id > 0, na.rm = TRUE),
    n_clusters = dplyr::n_distinct(doc_clusters$cluster_id[doc_clusters$cluster_id > 0]),
    min_cluster_size = min_cluster_size,
    approach = approach
  )

  list(
    status = if (nrow(sample_ranking) > 0) "success" else "warning",
    approach = approach,
    edges = edges,
    document_clusters = doc_clusters,
    cluster_summary = cluster_summary,
    sample_ranking = sample_ranking,
    sample_selected = sample_selected,
    metrics = metrics
  )
}

bslr_cluster_graph <- function(edges, nodes, min_cluster_size = 5L) {
  nodes <- unique(as.character(nodes))
  empty <- data.frame(doc_key = nodes, cluster_id = 0L, stringsAsFactors = FALSE)

  if (!requireNamespace("igraph", quietly = TRUE) || !is.data.frame(edges) || nrow(edges) == 0) {
    return(empty)
  }

  g <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = data.frame(name = nodes, stringsAsFactors = FALSE))
  if (igraph::ecount(g) == 0) {
    return(empty)
  }

  clustering <- tryCatch(
    igraph::cluster_louvain(g, weights = igraph::E(g)$weight),
    error = function(e) NULL
  )
  if (is.null(clustering)) {
    return(empty)
  }

  membership <- igraph::membership(clustering)
  out <- data.frame(doc_key = names(membership), cluster_id = as.integer(membership), stringsAsFactors = FALSE)

  if (!is.null(min_cluster_size) && min_cluster_size > 1) {
    cluster_sizes <- table(out$cluster_id)
    small_clusters <- as.integer(names(cluster_sizes[cluster_sizes < min_cluster_size]))
    out$cluster_id[out$cluster_id %in% small_clusters] <- 0L
  }

  dplyr::right_join(out, empty, by = "doc_key", suffix = c("", ".default")) |>
    dplyr::transmute(
      doc_key = .data$doc_key,
      cluster_id = ifelse(is.na(.data$cluster_id), .data$cluster_id.default, .data$cluster_id)
    )
}

bslr_weighted_degree <- function(doc_keys, edges) {
  degree <- stats::setNames(rep(0, length(doc_keys)), doc_keys)
  if (!is.data.frame(edges) || nrow(edges) == 0) {
    return(as.numeric(degree))
  }
  for (i in seq_len(nrow(edges))) {
    row <- edges[i, , drop = FALSE]
    degree[[row$from]] <- degree[[row$from]] + as.numeric(row$weight)
    degree[[row$to]] <- degree[[row$to]] + as.numeric(row$weight)
  }
  as.numeric(degree[doc_keys])
}

bslr_age_normalized_citations <- function(citations, year) {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  age <- pmax(current_year - suppressWarnings(as.integer(year)) + 1L, 1L)
  safe_divide(citations, age, default = 0)
}

bslr_rank_documents <- function(doc_clusters, protocol) {
  rule <- tolower(trimws(as.character(protocol$bibliometric$ranking_rule %||% "hybrid")))
  impact <- bslr_rescale(doc_clusters$age_normalized_citations)
  centrality <- bslr_rescale(doc_clusters$weighted_degree)

  score <- switch(
    rule,
    normalized_citations = impact,
    within_cluster_centrality = centrality,
    hybrid = (impact + centrality) / 2,
    (impact + centrality) / 2
  )
  as.numeric(score)
}

bslr_rescale <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[is.na(x)] <- 0
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) {
    return(rep(0, length(x)))
  }
  (x - rng[1]) / diff(rng)
}

bslr_build_cluster_summary <- function(doc_clusters) {
  clustered <- doc_clusters[doc_clusters$cluster_id > 0, , drop = FALSE]
  if (nrow(clustered) == 0) {
    return(data.frame())
  }

  out <- clustered |>
    dplyr::group_by(cluster_id, cluster_label) |>
    dplyr::summarise(
      article_count = dplyr::n(),
      mean_citations = mean(citations, na.rm = TRUE),
      total_citations = sum(citations, na.rm = TRUE),
      mean_year = mean(year, na.rm = TRUE),
      representative_titles = paste(utils::head(title[order(-ranking_score)], 3), collapse = " | "),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(article_count), dplyr::desc(total_citations))

  as.data.frame(out, stringsAsFactors = FALSE)
}

bslr_label_clusters <- function(doc_clusters) {
  labels <- stats::setNames(rep("Unclustered", dplyr::n_distinct(doc_clusters$cluster_id)), sort(unique(doc_clusters$cluster_id)))
  for (cluster_id in unique(doc_clusters$cluster_id)) {
    if (cluster_id <= 0) next
    rows <- doc_clusters[doc_clusters$cluster_id == cluster_id, , drop = FALSE]
    tokens <- unlist(strsplit(paste(rows$keywords_joined, collapse = ";"), ";", fixed = TRUE))
    tokens <- trimws(tolower(tokens))
    tokens <- tokens[nzchar(tokens)]
    tokens <- tokens[!tokens %in% c("article", "review", "analysis", "study", "system", "systems")]
    if (length(tokens) == 0) {
      labels[[as.character(cluster_id)]] <- paste("Cluster", cluster_id)
    } else {
      top_terms <- names(sort(table(tokens), decreasing = TRUE))[1:min(3, length(unique(tokens)))]
      labels[[as.character(cluster_id)]] <- paste(top_terms, collapse = " / ")
    }
  }
  unname(labels[as.character(doc_clusters$cluster_id)])
}

bslr_collect_keyword_strings <- function(input) {
  cols <- intersect(c("DE", "ID", "KW", "Keywords"), names(input))
  if (length(cols) == 0) {
    return(if ("TI" %in% names(input)) as.character(input$TI) else rep("", nrow(input)))
  }
  out <- apply(input[, cols, drop = FALSE], 1, function(row) {
    parts <- row[!is.na(row) & nzchar(trimws(row))]
    paste(parts, collapse = ";")
  })
  as.character(out)
}

bslr_extract_reference_lists <- function(cr_field) {
  lapply(as.character(cr_field), function(cr) {
    if (is.na(cr) || !nzchar(trimws(cr))) return(character())
    refs <- trimws(strsplit(cr, ";")[[1]])
    refs <- refs[nzchar(refs)]
    normalized <- vapply(refs, function(ref) {
      doi <- regmatches(ref, regexpr("10\\.[^ ;,]+", ref))
      if (length(doi) > 0 && !is.na(doi[1]) && nzchar(doi[1])) {
        return(tolower(doi[1]))
      }
      ref <- tolower(ref)
      ref <- gsub("[^a-z0-9 ]", " ", ref)
      ref <- gsub("\\s+", " ", ref)
      trimws(substr(ref, 1, 120))
    }, character(1))
    unique(normalized[nzchar(normalized)])
  })
}
