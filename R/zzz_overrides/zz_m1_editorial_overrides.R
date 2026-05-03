# ============================================================================
# zz_m1_editorial_overrides.R - Late-binding editorial/semantic cleanup for M1
# ============================================================================

m1_title_case_phrase <- function(x) {
  x <- as.character(x)
  vapply(x, function(value) {
    if (is.na(value) || !nzchar(trimws(value))) {
      return(NA_character_)
    }
    words <- unlist(strsplit(tolower(trimws(value)), "\\s+"))
    stop_words <- c("and", "or", "of", "the", "for", "in", "on", "to", "with", "via")
    words <- ifelse(
      words %in% stop_words,
      words,
      paste0(toupper(substr(words, 1, 1)), substring(words, 2))
    )
    paste(words, collapse = " ")
  }, character(1))
}

m1_normalize_keyword_phrase <- function(x) {
  x <- as.character(x)
  normalized <- vapply(x, function(value) {
    if (is.na(value) || !nzchar(trimws(value))) {
      return(NA_character_)
    }
    value <- gsub("[_/]+", " ", value)
    value <- gsub("-", " ", value)
    value <- gsub("[^[:alnum:]\\s]", " ", value)
    value <- gsub("\\s+", " ", trimws(value))
    if (!nzchar(value)) {
      return(NA_character_)
    }
    tolower(value)
  }, character(1))

  m1_title_case_phrase(normalized)
}

m1_build_citation_display_label <- function(title, source = NA_character_, year = NA, doi = NA_character_, max_chars = 80L) {
  title <- as.character(title)
  source <- as.character(source)
  doi <- as.character(doi)
  year <- as.character(year)

  vapply(seq_along(title), function(i) {
    ti <- if (!is.na(title[i])) trimws(title[i]) else ""
    so <- if (!is.na(source[i])) trimws(source[i]) else ""
    yr <- if (!is.na(year[i])) trimws(year[i]) else ""
    di <- if (!is.na(doi[i])) trimws(doi[i]) else ""

    if (!nzchar(ti)) {
      ti <- if (nzchar(di)) di else paste("Document", i)
    }
    if (nchar(ti) > max_chars) {
      ti <- paste0(substr(ti, 1, max_chars - 3L), "...")
    }

    meta <- c(if (nzchar(yr)) yr else NULL, if (nzchar(so)) so else NULL)
    meta_text <- if (length(meta) > 0) paste(meta, collapse = ", ") else NULL
    paste(c(ti, meta_text), collapse = " | ")
  }, character(1))
}

m1_make_topic_label <- function(top_words) {
  top_words <- as.character(top_words %||% character())
  top_words <- top_words[!is.na(top_words) & nzchar(trimws(top_words))]
  if (length(top_words) == 0) {
    return("Topic")
  }

  generic_terms <- c(
    "study", "studies", "analysis", "review", "reviews", "method", "methods",
    "approach", "approaches", "paper", "papers", "article", "articles",
    "model", "models", "based", "using", "application", "applications"
  )

  clean_terms <- vapply(top_words, function(term) {
    term <- m1_normalize_keyword_phrase(term)
    words <- unlist(strsplit(tolower(term), "\\s+"))
    words <- words[!words %in% generic_terms]
    if (length(words) == 0) {
      return(NA_character_)
    }
    m1_title_case_phrase(paste(words, collapse = " "))
  }, character(1))

  clean_terms <- unique(clean_terms[!is.na(clean_terms) & nzchar(clean_terms)])
  if (length(clean_terms) == 0) {
    clean_terms <- unique(m1_normalize_keyword_phrase(top_words))
    clean_terms <- clean_terms[!is.na(clean_terms) & nzchar(clean_terms)]
  }
  if (length(clean_terms) == 0) {
    return("Topic")
  }

  paste(utils::head(clean_terms, 3), collapse = " / ")
}

#' @export
compute_m1_keywords <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(keywords_summary = list(), top_keywords = m1_empty_rank_table(), status = "error"))
  }

  top_n <- config$top_n_keywords

  kw_data <- tryCatch({
    cached <- get_cached_biblio_analysis(input)
    res <- cached$res
    s <- summary(res, pause = FALSE, verbose = FALSE)
    mr <- s$MostRelKeywords
    colnames(mr) <- make.unique(colnames(mr))
    freq_col <- if ("Occurrences" %in% colnames(mr)) "Occurrences" else if ("Freq" %in% colnames(mr)) "Freq" else colnames(mr)[2]
    mr[[freq_col]] <- suppressWarnings(as.numeric(mr[[freq_col]]))
    mr <- mr[!is.na(mr[[freq_col]]), ]
    mr <- mr[order(-mr[[freq_col]]), ]
    kw_col <- if ("Keywords" %in% colnames(mr)) "Keywords" else colnames(mr)[1]
    list(keywords = mr[[kw_col]], freq = mr[[freq_col]])
  }, error = function(e) {
    if ("DE" %in% names(input)) {
      kw_list <- lapply(input$DE[!is.na(input$DE)], function(x) trimws(unlist(strsplit(x, ";"))))
      kw_counts <- sort(table(unlist(kw_list)), decreasing = TRUE)
      list(keywords = names(kw_counts), freq = as.integer(kw_counts))
    } else {
      list(keywords = character(0), freq = integer(0))
    }
  })

  if (length(kw_data$keywords) > 0) {
    kw_df <- data.frame(
      keyword = as.character(kw_data$keywords),
      freq = as.numeric(kw_data$freq),
      stringsAsFactors = FALSE
    )
    kw_df$keyword_norm <- m1_normalize_keyword_phrase(kw_df$keyword)
    kw_df <- kw_df[!is.na(kw_df$keyword_norm) & nzchar(kw_df$keyword_norm), , drop = FALSE]
    kw_df <- kw_df |>
      dplyr::group_by(keyword_norm) |>
      dplyr::summarise(value = sum(freq, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(value), keyword_norm)

    top_keywords <- tibble::tibble(
      rank = seq_len(min(top_n, nrow(kw_df))),
      label = kw_df$keyword_norm[1:min(top_n, nrow(kw_df))],
      value = kw_df$value[1:min(top_n, nrow(kw_df))]
    )
  } else {
    top_keywords <- m1_empty_rank_table()
  }

  list(
    keywords_summary = list(
      total_keywords_de = sum(!is.na(input$DE) & input$DE != ""),
      total_keywords_id = sum(!is.na(input$ID) & input$ID != "")
    ),
    top_keywords = top_keywords,
    status = "success"
  )
}

#' @export
compute_m1_citations <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(
      top_cited_documents = m1_empty_rank_table(),
      citations_per_year = m1_empty_rank_table(),
      citation_summary = list(),
      status = "error"
    ))
  }

  top_n <- if (!is.null(config$top_n_documents)) as.integer(config$top_n_documents) else 10L

  citation_df <- data.frame(
    title = as.character(input$TI %||% NA_character_),
    total_citations = suppressWarnings(as.numeric(input$TC %||% NA_real_)),
    doi = as.character(input$DI %||% NA_character_),
    source = as.character(input$SO %||% NA_character_),
    year = suppressWarnings(as.integer(input$PY %||% NA_integer_)),
    stringsAsFactors = FALSE
  )
  citation_df <- citation_df[!is.na(citation_df$total_citations), , drop = FALSE]
  if (nrow(citation_df) == 0) {
    return(list(
      top_cited_documents = m1_empty_rank_table(),
      citations_per_year = m1_empty_rank_table(),
      citation_summary = list(total_citations = 0, mean_citations = 0),
      status = "success"
    ))
  }

  citation_df$label <- m1_build_citation_display_label(
    title = citation_df$title,
    source = citation_df$source,
    year = citation_df$year,
    doi = citation_df$doi
  )
  citation_df$label_short <- ifelse(
    nchar(citation_df$label) > 48,
    paste0(substr(citation_df$label, 1, 45), "..."),
    citation_df$label
  )

  by_total <- citation_df[order(-citation_df$total_citations, citation_df$year, citation_df$title), , drop = FALSE]
  top_cited <- tibble::tibble(
    rank = seq_len(min(top_n, nrow(by_total))),
    label = by_total$label[1:min(top_n, nrow(by_total))],
    label_short = by_total$label_short[1:min(top_n, nrow(by_total))],
    value = by_total$total_citations[1:min(top_n, nrow(by_total))],
    title = by_total$title[1:min(top_n, nrow(by_total))],
    source = by_total$source[1:min(top_n, nrow(by_total))],
    year = by_total$year[1:min(top_n, nrow(by_total))],
    doi = by_total$doi[1:min(top_n, nrow(by_total))]
  )

  current_year <- suppressWarnings(max(as.integer(input$PY), na.rm = TRUE))
  if (!is.finite(current_year)) current_year <- as.integer(format(Sys.Date(), "%Y"))
  citation_df$age_years <- pmax(current_year - citation_df$year + 1L, 1L)
  citation_df$citations_per_year_value <- citation_df$total_citations / citation_df$age_years

  by_rate <- citation_df[order(-citation_df$citations_per_year_value, -citation_df$total_citations, citation_df$title), , drop = FALSE]
  citations_per_year <- tibble::tibble(
    rank = seq_len(min(top_n, nrow(by_rate))),
    label = by_rate$label[1:min(top_n, nrow(by_rate))],
    label_short = by_rate$label_short[1:min(top_n, nrow(by_rate))],
    value = round(by_rate$citations_per_year_value[1:min(top_n, nrow(by_rate))], 3),
    title = by_rate$title[1:min(top_n, nrow(by_rate))],
    source = by_rate$source[1:min(top_n, nrow(by_rate))],
    year = by_rate$year[1:min(top_n, nrow(by_rate))],
    doi = by_rate$doi[1:min(top_n, nrow(by_rate))]
  )

  list(
    top_cited_documents = top_cited,
    citations_per_year = citations_per_year,
    citation_summary = list(
      total_citations = sum(citation_df$total_citations, na.rm = TRUE),
      mean_citations = round(mean(citation_df$total_citations, na.rm = TRUE), 2)
    ),
    status = "success"
  )
}

extract_keywords <- function(keyword_column) {
  abstract_stopwords <- c(
    "the", "and", "for", "with", "from", "into", "that", "this", "these", "those",
    "study", "studies", "analysis", "review", "reviews", "paper", "article",
    "using", "based", "approach", "approaches", "method", "methods", "results",
    "through", "across", "among", "between", "within", "their", "there", "which"
  )

  keywords_list <- lapply(as.character(keyword_column), function(x) {
    if (is.na(x) || x == "") return(character(0))

    if (grepl("[;,]", x)) {
      kw <- strsplit(x, "[;,]")[[1]]
      kw <- trimws(kw)
      kw <- m1_normalize_keyword_phrase(kw)
      kw <- kw[!is.na(kw) & nzchar(kw)]
      return(unique(kw))
    }

    kw <- tolower(x)
    kw <- gsub("[^a-z0-9 ]", " ", kw)
    kw <- gsub("\\s+", " ", trimws(kw))
    tokens <- unlist(strsplit(kw, "\\s+"))
    tokens <- tokens[nchar(tokens) > 2 & !tokens %in% abstract_stopwords]
    tokens <- unique(m1_title_case_phrase(tokens))
    tokens[!is.na(tokens) & nzchar(tokens)]
  })

  names(keywords_list) <- seq_along(keywords_list)
  keywords_list
}

extract_topics <- function(lda_result, dtm, n_top_words = 10) {
  n_topics <- lda_result$n_topics
  beta <- lda_result$beta
  terms <- colnames(beta)
  if (is.null(terms)) terms <- colnames(dtm)
  if (is.null(terms)) terms <- paste0("term", seq_len(ncol(beta)))

  topics_list <- list()
  topic_terms_long <- list()

  for (k in seq_len(n_topics)) {
    top_idx <- order(beta[k, ], decreasing = TRUE)[1:n_top_words]
    top_words <- terms[top_idx]
    top_probs <- beta[k, top_idx]
    label <- m1_make_topic_label(top_words)

    topics_list[[k]] <- list(
      topic_id = k,
      label = label,
      top_words = top_words,
      probabilities = top_probs,
      weight = mean(lda_result$gamma[, k])
    )

    topic_terms_long[[k]] <- data.frame(
      topic_id = k,
      topic_label = label,
      term = m1_normalize_keyword_phrase(top_words),
      probability = as.numeric(top_probs),
      rank = seq_along(top_words),
      stringsAsFactors = FALSE
    )
  }

  if (length(topics_list) == 0) {
    return(list(topics = data.frame(), topics_list = list(), topic_terms_long = data.frame()))
  }

  topics_df <- do.call(rbind, lapply(topics_list, function(t) {
    data.frame(
      topic_id = t$topic_id,
      label = t$label,
      top_words = paste(m1_normalize_keyword_phrase(t$top_words), collapse = ", "),
      top_probabilities = paste(round(t$probabilities, 4), collapse = ", "),
      weight = t$weight,
      stringsAsFactors = FALSE
    )
  }))

  if (nrow(topics_df) > 0) {
    topics_df <- topics_df[order(-topics_df$weight), ]
  }

  list(
    topics = topics_df,
    topics_list = topics_list,
    topic_terms_long = do.call(rbind, topic_terms_long)
  )
}

#' @export
render_m1_citations <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"top_cited_documents" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  tc <- result$top_cited_documents
  if (nrow(tc) == 0) return(list(status = "stub", plots = list(), tables = list()))

  plots <- list()

  tc$label_clean <- if ("label_short" %in% names(tc)) tc$label_short else tc$label
  tc$label_clean <- ifelse(is.na(tc$label_clean) | tc$label_clean == "NA", paste0("Document ", tc$rank), tc$label_clean)
  tc$label_clean <- substr(tc$label_clean, 1, 48)

  plots$bar <- ggplot2::ggplot(tc[1:min(10, nrow(tc)), ], ggplot2::aes(x = reorder(label_clean, value), y = value)) +
    ggplot2::geom_col(fill = ieee_colors$orange, color = "black", linewidth = 0.2) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(x = NULL, y = "Citations") +
    ggplot2::ggtitle(NULL) +
    ieee_theme() +
    ggplot2::theme(plot.title = ggplot2::element_blank())

  if ("citations_per_year" %in% names(result) && nrow(result$citations_per_year) > 0) {
    cpy <- result$citations_per_year
    cpy$label_clean <- if ("label_short" %in% names(cpy)) cpy$label_short else cpy$label
    cpy$label_clean <- ifelse(is.na(cpy$label_clean) | cpy$label_clean == "NA", paste0("Document ", cpy$rank), cpy$label_clean)
    cpy$label_clean <- substr(cpy$label_clean, 1, 48)

    plots$citations_per_year <- ggplot2::ggplot(cpy[1:min(10, nrow(cpy)), ], ggplot2::aes(x = reorder(label_clean, value), y = value)) +
      ggplot2::geom_col(fill = ieee_colors$green, color = "black", linewidth = 0.2) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
      ggplot2::labs(x = NULL, y = "Citations/Year") +
      ggplot2::ggtitle(NULL) +
      ieee_theme()

    merged <- merge(tc, cpy, by = "rank", suffixes = c("_tc", "_cpy"))
    if (nrow(merged) > 0) {
      merged$label_clean <- ifelse(is.na(merged$label_tc) | merged$label_tc == "NA", paste0("Document ", merged$rank), merged$label_tc)
      merged$label_clean <- substr(merged$label_clean, 1, 48)

      plots$dual_bar_line <- ggplot2::ggplot(merged[1:min(8, nrow(merged)), ]) +
        ggplot2::geom_col(ggplot2::aes(x = reorder(label_clean, value_tc), y = value_tc),
                          fill = ieee_colors$blue, color = "black", linewidth = 0.2) +
        ggplot2::geom_point(ggplot2::aes(x = reorder(label_clean, value_tc), y = value_cpy * max(value_tc) / max(value_cpy)),
                            color = ieee_colors$orange, size = 2) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(
          name = "Total Citations",
          sec.axis = ggplot2::sec_axis(~ . * max(merged$value_cpy) / max(merged$value_tc), name = "Citations/Year")
        ) +
        ggplot2::labs(x = NULL) +
        ggplot2::ggtitle(NULL) +
        ieee_theme(base_size = 6) +
        ggplot2::theme(axis.title.y.right = ggplot2::element_text(color = ieee_colors$orange))
    }
  }

  list(status = "success", plots = plots, tables = list())
}
