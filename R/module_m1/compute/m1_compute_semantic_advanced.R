# ============================================================================
# m1_compute_semantic_advanced.R - Journal semantic layer for M1
# ============================================================================

compute_m1_semantic_advanced <- function(input, data = list(), config = biblio_config()) {
  config <- merge_biblio_config(config)
  if (!isTRUE(config$advanced_analytics)) {
    return(list(status = "disabled", reason = "advanced_analytics is FALSE."))
  }

  tryCatch({
    keyword_docs <- m1_semantic_keyword_docs(input)
    synonym_dictionary <- m1_build_keyword_synonym_dictionary(keyword_docs)
    co_word_network <- m1_build_coword_network(data$keyword_cooccurrence %||% list())
    thematic_map <- m1_build_thematic_map(data$keyword_cooccurrence %||% list(), synonym_dictionary)
    thematic_evolution <- m1_build_thematic_evolution(keyword_docs, input)
    topic_stability <- m1_build_topic_stability(data$topic_modeling %||% list(), thematic_evolution)

    list(
      status = "success",
      reason = NA_character_,
      synonym_dictionary = synonym_dictionary,
      co_word_network = co_word_network,
      thematic_map = thematic_map,
      thematic_evolution = thematic_evolution,
      topic_stability = topic_stability
    )
  }, error = function(e) {
    if (identical(config$advanced_fail_policy %||% "soft", "hard")) stop(e)
    list(
      status = "error",
      reason = e$message,
      synonym_dictionary = tibble::tibble(),
      co_word_network = list(nodes = tibble::tibble(), edges = tibble::tibble()),
      thematic_map = tibble::tibble(),
      thematic_evolution = tibble::tibble(),
      topic_stability = tibble::tibble()
    )
  })
}

m1_semantic_keyword_docs <- function(input) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(tibble::tibble(doc_id = integer(), year = numeric(), keyword = character(), keyword_norm = character()))
  }
  kw_col <- intersect(c("KW_Merged", "KW", "DE", "ID", "Keywords"), names(input))[1]
  if (is.na(kw_col)) {
    return(tibble::tibble(doc_id = integer(), year = numeric(), keyword = character(), keyword_norm = character()))
  }
  year_col <- intersect(c("PY", "Year", "year"), names(input))[1]
  years <- if (!is.na(year_col)) suppressWarnings(as.numeric(input[[year_col]])) else rep(NA_real_, nrow(input))
  rows <- lapply(seq_len(nrow(input)), function(i) {
    kws <- unlist(strsplit(as.character(input[[kw_col]][i] %||% ""), "[;|,]", perl = TRUE), use.names = FALSE)
    kws <- trimws(kws)
    kws <- kws[nzchar(kws)]
    if (length(kws) == 0) return(NULL)
    tibble::tibble(
      doc_id = i,
      year = years[i],
      keyword = kws,
      keyword_norm = m1_normalize_keyword_phrase(kws)
    )
  })
  out <- dplyr::bind_rows(rows)
  if (!is.data.frame(out) || nrow(out) == 0) {
    return(tibble::tibble(doc_id = integer(), year = numeric(), keyword = character(), keyword_norm = character()))
  }
  out |>
    dplyr::filter(!is.na(.data$keyword_norm), nzchar(.data$keyword_norm)) |>
    dplyr::distinct(.data$doc_id, .data$keyword_norm, .keep_all = TRUE)
}

m1_build_keyword_synonym_dictionary <- function(keyword_docs) {
  if (!is.data.frame(keyword_docs) || nrow(keyword_docs) == 0) {
    return(tibble::tibble(canonical_keyword = character(), variants = character(), n_variants = integer(), documents = integer()))
  }

  keyword_docs |>
    dplyr::mutate(
      canonical_keyword = m1_keyword_canonical_key(.data$keyword_norm),
      variant = .data$keyword_norm
    ) |>
    dplyr::group_by(.data$canonical_keyword) |>
    dplyr::summarise(
      variants = paste(sort(unique(.data$variant)), collapse = "; "),
      n_variants = dplyr::n_distinct(.data$variant),
      documents = dplyr::n_distinct(.data$doc_id),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$documents), .data$canonical_keyword)
}

m1_keyword_canonical_key <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[_-]+", " ", x)
  x <- gsub("\\b(modelling|modeling)\\b", "model", x)
  x <- gsub("\\b(optimization|optimisation)\\b", "optimization", x)
  x <- gsub("\\b(behaviour|behavior)\\b", "behavior", x)
  x <- gsub("\\b(analyses)\\b", "analysis", x)
  x <- gsub("\\b(systems)\\b", "system", x)
  x <- gsub("\\b(models)\\b", "model", x)
  x <- gsub("\\b(methods)\\b", "method", x)
  x <- gsub("\\b(algorithms)\\b", "algorithm", x)
  x <- gsub("\\s+", " ", trimws(x))
  m1_title_case_phrase(x)
}

m1_build_coword_network <- function(keyword_cooccurrence) {
  edges <- keyword_cooccurrence$edgelist %||% tibble::tibble()
  metrics <- keyword_cooccurrence$metrics$summary %||% tibble::tibble()
  if (!is.data.frame(edges) || nrow(edges) == 0) {
    return(list(status = "insufficient_data", nodes = tibble::tibble(), edges = tibble::tibble()))
  }
  names(edges) <- tolower(names(edges))
  from_col <- intersect(names(edges), c("from", "source", "keyword1", "term1"))[1]
  to_col <- intersect(names(edges), c("to", "target", "keyword2", "term2"))[1]
  weight_col <- intersect(names(edges), c("weight", "value", "n"))[1]
  if (is.na(from_col) || is.na(to_col)) {
    return(list(status = "insufficient_data", nodes = tibble::tibble(), edges = tibble::tibble()))
  }
  edges <- tibble::tibble(
    from = as.character(edges[[from_col]]),
    to = as.character(edges[[to_col]]),
    weight = if (!is.na(weight_col)) suppressWarnings(as.numeric(edges[[weight_col]])) else 1
  ) |>
    dplyr::filter(nzchar(.data$from), nzchar(.data$to), is.finite(.data$weight)) |>
    dplyr::arrange(dplyr::desc(.data$weight))

  if (is.data.frame(metrics) && nrow(metrics) > 0 && "keyword" %in% names(metrics)) {
    nodes <- metrics
  } else {
    nodes <- tibble::tibble(keyword = unique(c(edges$from, edges$to)))
  }
  list(status = "success", nodes = nodes, edges = edges)
}

m1_build_thematic_map <- function(keyword_cooccurrence, synonym_dictionary) {
  metrics <- keyword_cooccurrence$metrics$summary %||% tibble::tibble()
  communities <- keyword_cooccurrence$communities %||% list()
  if (!is.data.frame(metrics) || nrow(metrics) == 0 || !"keyword" %in% names(metrics)) {
    return(tibble::tibble(theme_id = character(), label = character(), centrality = numeric(), density = numeric(), quadrant = character(), top_terms = character()))
  }

  community_vec <- m1_communities_to_vector(communities, metrics$keyword)
  centrality_raw <- if ("strength" %in% names(metrics)) metrics$strength else if ("degree" %in% names(metrics)) metrics$degree else rep(0, nrow(metrics))
  density_raw <- if ("closeness" %in% names(metrics)) metrics$closeness else if ("eigenvector" %in% names(metrics)) metrics$eigenvector else rep(0, nrow(metrics))
  community_raw <- unname(community_vec[as.character(metrics$keyword)])
  community_raw[is.na(community_raw) | !nzchar(community_raw)] <- "theme_1"
  df <- metrics |>
    dplyr::mutate(
      community = community_raw,
      centrality_value = suppressWarnings(as.numeric(centrality_raw)),
      density_value = suppressWarnings(as.numeric(density_raw))
    )

  theme_rows <- df |>
    dplyr::group_by(.data$community) |>
    dplyr::summarise(
      centrality = mean(.data$centrality_value, na.rm = TRUE),
      density = mean(.data$density_value, na.rm = TRUE),
      top_terms = paste(utils::head(.data$keyword[order(-.data$centrality_value)], 6), collapse = "; "),
      documents_proxy = sum(.data$centrality_value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      theme_id = paste0("T", dplyr::row_number()),
      label = m1_theme_label(.data$top_terms),
      quadrant = m1_thematic_quadrant(.data$centrality, .data$density)
    ) |>
    dplyr::select("theme_id", "label", "community", "centrality", "density", "quadrant", "documents_proxy", "top_terms") |>
    dplyr::arrange(dplyr::desc(.data$centrality))

  theme_rows
}

m1_communities_to_vector <- function(communities, keywords) {
  out <- stats::setNames(rep("theme_1", length(keywords)), as.character(keywords))
  if (is.null(communities) || length(communities) == 0) return(out)
  if (is.atomic(communities) && length(communities) == length(keywords)) {
    vals <- as.character(communities)
    names(vals) <- names(communities) %||% keywords
    return(vals)
  }
  if (is.list(communities)) {
    nms <- names(communities)
    if (is.null(nms)) nms <- paste0("theme_", seq_along(communities))
    for (i in seq_along(communities)) {
      members <- as.character(communities[[i]])
      out[names(out) %in% members] <- nms[i]
    }
  }
  out
}

m1_theme_label <- function(top_terms) {
  terms <- trimws(unlist(strsplit(as.character(top_terms), ";", fixed = TRUE), use.names = FALSE))
  terms <- terms[nzchar(terms)]
  if (length(terms) == 0) return("Unlabeled Theme")
  paste(utils::head(terms, 3), collapse = " / ")
}

m1_thematic_quadrant <- function(centrality, density) {
  c_cut <- stats::median(centrality, na.rm = TRUE)
  d_cut <- stats::median(density, na.rm = TRUE)
  dplyr::case_when(
    centrality >= c_cut & density >= d_cut ~ "motor_theme",
    centrality >= c_cut & density < d_cut ~ "basic_transversal_theme",
    centrality < c_cut & density >= d_cut ~ "niche_theme",
    TRUE ~ "emerging_or_declining_theme"
  )
}

m1_build_thematic_evolution <- function(keyword_docs, input) {
  if (!is.data.frame(keyword_docs) || nrow(keyword_docs) == 0 || !any(is.finite(keyword_docs$year))) {
    return(tibble::tibble(period = character(), keyword = character(), documents = integer(), share = numeric(), rank = integer(), period_role = character()))
  }
  years <- sort(unique(keyword_docs$year[is.finite(keyword_docs$year)]))
  cuts <- m1_semantic_periods(years)
  keyword_docs |>
    dplyr::mutate(period = dplyr::case_when(
      .data$year %in% cuts$early ~ "early",
      .data$year %in% cuts$middle ~ "middle",
      .data$year %in% cuts$late ~ "late",
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(.data$period)) |>
    dplyr::count(.data$period, .data$keyword_norm, name = "documents") |>
    dplyr::group_by(.data$period) |>
    dplyr::mutate(
      share = .data$documents / sum(.data$documents, na.rm = TRUE),
      rank = dplyr::min_rank(dplyr::desc(.data$documents))
    ) |>
    dplyr::ungroup() |>
    dplyr::rename(keyword = "keyword_norm") |>
    dplyr::mutate(period_role = dplyr::case_when(
      .data$period == "late" & .data$rank <= 10 ~ "current_core",
      .data$period == "early" & .data$rank <= 10 ~ "legacy_core",
      TRUE ~ "peripheral"
    )) |>
    dplyr::arrange(.data$period, .data$rank)
}

m1_semantic_periods <- function(years) {
  years <- sort(unique(suppressWarnings(as.numeric(years))))
  years <- years[is.finite(years)]
  if (length(years) < 3) {
    return(list(early = years, middle = years, late = years))
  }
  k <- max(1, floor(length(years) / 3))
  list(
    early = utils::head(years, k),
    middle = years[(k + 1):max(k + 1, length(years) - k)],
    late = utils::tail(years, k)
  )
}

m1_build_topic_stability <- function(topic_modeling, thematic_evolution) {
  topic_evolution <- topic_modeling$topic_evolution %||% tibble::tibble()
  topic_metrics <- topic_modeling$topic_metrics %||% tibble::tibble()
  if (is.data.frame(topic_evolution) && nrow(topic_evolution) > 0) {
    names(topic_evolution) <- tolower(names(topic_evolution))
    topic_col <- intersect(names(topic_evolution), c("topic", "topic_id"))[1]
    value_col <- intersect(names(topic_evolution), c("proportion", "share", "n", "documents", "count"))[1]
    if (!is.na(topic_col) && !is.na(value_col)) {
      return(topic_evolution |>
        dplyr::group_by(.data[[topic_col]]) |>
        dplyr::summarise(
          mean_share = mean(suppressWarnings(as.numeric(.data[[value_col]])), na.rm = TRUE),
          stability = 1 / (1 + stats::sd(suppressWarnings(as.numeric(.data[[value_col]])), na.rm = TRUE)),
          periods_observed = dplyr::n(),
          .groups = "drop"
        ) |>
        stats::setNames(c("topic", "mean_share", "stability", "periods_observed")) |>
        dplyr::arrange(dplyr::desc(.data$stability)))
    }
  }

  if (is.data.frame(thematic_evolution) && nrow(thematic_evolution) > 0) {
    return(thematic_evolution |>
      dplyr::group_by(.data$keyword) |>
      dplyr::summarise(
        mean_share = mean(.data$share, na.rm = TRUE),
        stability = 1 / (1 + stats::sd(.data$share, na.rm = TRUE)),
        periods_observed = dplyr::n_distinct(.data$period),
        .groups = "drop"
      ) |>
      dplyr::rename(topic = "keyword") |>
      dplyr::arrange(dplyr::desc(.data$mean_share)) |>
      utils::head(25))
  }

  tibble::tibble(topic = character(), mean_share = numeric(), stability = numeric(), periods_observed = integer())
}
