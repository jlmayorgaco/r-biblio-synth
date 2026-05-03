# ============================================================================
# m1_report.R - Complete report builder for M1
# ============================================================================

#' @export
build_m1_report <- function(result, config = biblio_config()) {
  if (!inherits(result, "biblio_module_result")) {
    return(list(status = "stub", title = "M1 Report", sections = list(), lines = character(), tex = character()))
  }

  data <- result$data
  sections <- Filter(
    function(x) length(x$lines) > 0,
    list(
      narrative = m1_report_section_payload("Narrative Evidence", m1_report_narrative(data)),
      overview = m1_report_section_payload("Overview", m1_report_overview(data)),
      doc_types = m1_report_section_payload("Document Types", m1_report_doc_types(data)),
      authors = m1_report_section_payload("Authors", m1_report_authors(data)),
      author_indices = m1_report_section_payload("Author Indices", m1_report_author_indices(data)),
      citations = m1_report_section_payload("Citations", m1_report_citations(data)),
      citation_analysis = m1_report_section_payload("Citation Analysis", m1_report_citation_analysis(data)),
      countries = m1_report_section_payload("Countries", m1_report_countries(data)),
      sources = m1_report_section_payload("Sources", m1_report_sources(data)),
      keywords = m1_report_section_payload("Keywords", m1_report_keywords(data)),
      keyword_structure = m1_report_section_payload("Keyword Structure", m1_report_keyword_structure(data)),
      collaboration = m1_report_section_payload("Collaboration", m1_report_collaboration(data)),
      lotka = m1_report_section_payload("Lotka", m1_report_lotka(data)),
      bradford = m1_report_section_payload("Bradford", m1_report_bradford(data)),
      price_law = m1_report_section_payload("Price Law", m1_report_price_law(data)),
      topic_modeling = m1_report_section_payload("Topic Modeling", m1_report_topic_modeling(data)),
      author_career = m1_report_section_payload("Author Career", m1_report_author_career(data)),
      hypotheses = m1_report_section_payload("Hypotheses", m1_report_hypotheses(data))
    )
  )

  lines <- c(
    "==========================================",
    "M1 Main Information Report",
    "==========================================",
    "",
    paste("Generated:", Sys.time()),
    paste("Status:", result$status),
    ""
  )

  for (section_name in names(sections)) {
    section <- sections[[section_name]]
    lines <- c(lines, paste0("--- ", section$title, " ---"), section$lines, "")
  }

  list(
    status = "success",
    title = "M1 Main Information Report",
    sections = names(sections),
    lines = lines,
    tex = build_m1_report_tex("M1 Main Information Report", sections)
  )
}

m1_report_section_payload <- function(title, lines) {
  list(title = title, lines = lines[!is.na(lines) & nzchar(lines)])
}

m1_report_overview <- function(data) {
  tbl <- data$overview$summary_table %||% NULL
  if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) return(character())
  paste(tbl$metric, ":", tbl$value)
}

m1_report_narrative <- function(data) {
  narrative <- data$narrative %||% list()
  metrics <- narrative$metrics %||% data.frame()
  if (!is.data.frame(metrics) || nrow(metrics) == 0) return(character())
  c(
    "Narrative evidence integrates concentration, impact, collaboration, and conceptual-structure indicators.",
    ieee_narrative_lines(metrics, max_lines = 8)
  )
}

m1_report_doc_types <- function(data) {
  dt <- m1_prepare_doc_type_report_table(data$doc_types$doc_type_table %||% NULL)
  if (nrow(dt) == 0) return(character())
  paste(dt$label, ":", dt$value, sprintf("(%.1f%%)", dt$percentage))
}

m1_report_authors <- function(data) {
  out <- character()
  gini <- data$authors$author_gini %||% NA_real_
  if (is.finite(gini)) {
    out <- c(out, sprintf("Author productivity Gini: %.3f", gini))
  }
  top_authors <- data$authors$top_authors %||% NULL
  if (is.data.frame(top_authors) && nrow(top_authors) > 0) {
    out <- c(out, paste(seq_len(min(5, nrow(top_authors))), ".", top_authors$label[1:min(5, nrow(top_authors))], "-", top_authors$value[1:min(5, nrow(top_authors))], "publications"))
  }
  out
}

m1_report_author_indices <- function(data) {
  indices <- data$author_indices$top_h_index %||% data$author_indices$indices %||% NULL
  if (is.null(indices) || !is.data.frame(indices) || nrow(indices) == 0) return(character())

  summary <- data$author_indices$summary %||% list()
  out <- character()
  if (!is.null(summary$h_index_mean)) {
    out <- c(out, sprintf("Mean h-index: %.2f | Max h-index: %.2f", summary$h_index_mean %||% NA_real_, summary$h_index_max %||% NA_real_))
  }

  top_n <- min(5, nrow(indices))
  out <- c(
    out,
    paste(
      seq_len(top_n), ".",
      indices$author[1:top_n],
      sprintf("(h=%s, g=%s)", indices$h_index[1:top_n], indices$g_index[1:top_n])
    )
  )
  out
}

m1_report_citations <- function(data) {
  cit <- data$citations$top_cited_documents %||% NULL
  if (is.null(cit) || !is.data.frame(cit) || nrow(cit) == 0) return(character())
  labels <- if ("label" %in% names(cit)) cit$label else if ("label_short" %in% names(cit)) cit$label_short else cit$title
  paste(seq_len(min(5, nrow(cit))), ".", labels[1:min(5, nrow(cit))], "-", cit$value[1:min(5, nrow(cit))], "citations")
}

m1_report_citation_analysis <- function(data) {
  ca <- data$citation_analysis %||% list()
  if ((ca$status %||% "") != "success") return(character())

  summary <- ca$summary %||% list()
  age <- ca$age_analysis %||% list()
  out <- c(
    sprintf("Total citations: %s | Mean: %.2f | Median: %.2f | Gini: %.3f",
            summary$total_citations %||% NA_real_,
            summary$mean_citations %||% NA_real_,
            summary$median_citations %||% NA_real_,
            summary$gini_coefficient %||% NA_real_),
    sprintf("Best fit distribution: %s", format_distribution_name(ca$distribution_fit$best_fit %||% "unknown")),
    sprintf("Zero-citation share: %.2f%% | Half-life: %s years | Price Index: %.3f",
            summary$pct_zero_citations %||% NA_real_,
            age$half_life %||% NA_real_,
            age$price_index %||% NA_real_)
  )
  out
}

m1_report_countries <- function(data) {
  co <- data$countries$top_countries_by_articles %||% NULL
  if (is.null(co) || !is.data.frame(co) || nrow(co) == 0) return(character())
  out <- character()
  gini <- data$countries$country_gini_articles %||% NA_real_
  if (is.finite(gini)) {
    out <- c(out, sprintf("Country production Gini: %.3f", gini))
  }
  out <- c(out, paste(seq_len(min(5, nrow(co))), ".", trimws(co$label[1:min(5, nrow(co))]), "-", co$value[1:min(5, nrow(co))], "articles"))
  out
}

m1_report_sources <- function(data) {
  so <- data$sources$top_sources %||% NULL
  if (is.null(so) || !is.data.frame(so) || nrow(so) == 0) return(character())
  out <- character()
  gini <- data$sources$source_gini %||% NA_real_
  if (is.finite(gini)) {
    out <- c(out, sprintf("Source concentration Gini: %.3f", gini))
  }
  out <- c(out, paste(seq_len(min(5, nrow(so))), ".", trimws(so$label[1:min(5, nrow(so))]), "-", so$value[1:min(5, nrow(so))], "documents"))
  out
}

m1_report_keywords <- function(data) {
  kw <- data$keywords$top_keywords %||% NULL
  if (is.null(kw) || !is.data.frame(kw) || nrow(kw) == 0) return(character())
  paste(seq_len(min(8, nrow(kw))), ".", kw$label[1:min(8, nrow(kw))], "-", kw$value[1:min(8, nrow(kw))])
}

m1_report_keyword_structure <- function(data) {
  out <- character()

  kw_network <- data$keyword_cooccurrence %||% list()
  summary <- kw_network$summary %||% NULL
  metrics <- kw_network$metrics %||% NULL
  communities <- kw_network$communities %||% NULL
  if (is.list(summary) && length(summary) > 0) {
    out <- c(
      out,
      sprintf("Keyword network: %s keywords | %s unique pairs | density %.3f",
              summary$total_keywords %||% NA_real_,
              summary$unique_pairs %||% NA_real_,
              summary$density %||% NA_real_)
    )
  }
  if (is.list(metrics) && length(metrics) > 0) {
    out <- c(out, sprintf("Network modularity: %.3f | Small-world index: %.3f",
                          metrics$modularity %||% NA_real_,
                          metrics$small_world_index %||% NA_real_))
  }
  if (is.list(communities) && length(communities) > 0) {
    out <- c(out, sprintf("Detected communities: %s", communities$n_communities %||% NA_real_))
  }

  burst_summary <- data$keyword_burst$summary %||% NULL
  if (is.list(burst_summary) && length(burst_summary) > 0) {
    out <- c(
      out,
      sprintf("Burst detection: %s bursty keywords | %s recent bursts",
              burst_summary$n_bursty_keywords %||% 0,
              burst_summary$n_recent_bursts %||% 0)
    )
  }

  out
}

m1_report_collaboration <- function(data) {
  summary <- data$collaboration$summary %||% NULL
  if (is.null(summary) || !is.list(summary) || length(summary) == 0) return(character())
  c(
    sprintf("Collaboration index: %.3f", summary$collaboration_index %||% NA_real_),
    sprintf("Collaboration coefficient: %.3f | Degree of collaboration: %.3f",
            summary$collaboration_coefficient %||% NA_real_,
            summary$degree_of_collaboration %||% NA_real_),
    sprintf("Mean authors per paper: %.2f", summary$mean_authors_per_paper %||% NA_real_)
  )
}

m1_report_lotka <- function(data) {
  lotka <- data$lotka$lotka %||% NULL
  if (is.null(lotka) || !is.list(lotka) || length(lotka) == 0) return(character())
  c(
    sprintf("Lotka alpha: %.3f", lotka$alpha %||% NA_real_),
    sprintf("K-S statistic: %.3f | p-value: %.4f", lotka$gof_ks %||% NA_real_, lotka$gof_pvalue %||% NA_real_)
  )
}

m1_report_bradford <- function(data) {
  zs <- data$bradford$zone_summary %||% NULL
  if (is.null(zs) || !is.list(zs) || length(zs) == 0) return(character())
  c(
    paste("Core:", zs$zone1$n_sources, "sources,", zs$zone1$n_articles, "articles"),
    paste("Moderate:", zs$zone2$n_sources, "sources,", zs$zone2$n_articles, "articles"),
    paste("Peripheral:", zs$zone3$n_sources, "sources,", zs$zone3$n_articles, "articles")
  )
}

m1_report_price_law <- function(data) {
  price_data <- data$price_law %||% list()
  summary <- price_data$summary %||% NULL
  price_law <- price_data$price_law %||% NULL
  if (is.null(summary) || !is.list(summary) || length(summary) == 0) return(character())
  c(
    sprintf("Price index: %.3f", summary$price_index %||% NA_real_),
    sprintf("Top sqrt(N) authors contribution: %.3f", price_law$actual_proportion %||% NA_real_),
    as.character(summary$interpretation %||% "")
  )
}

m1_report_topic_modeling <- function(data) {
  tm <- data$topic_modeling %||% list()
  if ((tm$status %||% "") != "success") return(character())

  topics <- tm$topics %||% data.frame()
  out <- c(
    sprintf("Topics extracted: %s | Keyword source: %s", tm$n_topics %||% 0, tm$keywords_used %||% "NA"),
    sprintf("Coherence: %.3f | Perplexity: %.3f", tm$coherence %||% NA_real_, tm$perplexity %||% NA_real_)
  )

  if (is.data.frame(topics) && nrow(topics) > 0) {
    top_n <- min(5, nrow(topics))
    out <- c(out, paste(seq_len(top_n), ".", topics$label[1:top_n], "->", topics$top_words[1:top_n]))
  }

  out
}

m1_report_author_career <- function(data) {
  ac <- data$author_career %||% list()
  if ((ac$status %||% "") != "success") return(character())

  out <- c(sprintf("Authors analyzed for career metrics: %s", ac$n_authors %||% 0))
  top_h <- ac$top_by_h_index %||% data.frame()
  if (is.data.frame(top_h) && nrow(top_h) > 0) {
    top_n <- min(5, nrow(top_h))
    labels <- if ("display_author" %in% names(top_h)) top_h$display_author else top_h$author
    out <- c(out, paste(seq_len(top_n), ".", labels[1:top_n], sprintf("(h=%s, career=%s years)", top_h$h_index[1:top_n], top_h$career_length[1:top_n])))
  }
  if (is.data.frame(ac$rising_stars) && nrow(ac$rising_stars) > 0) {
    stars <- if ("display_author" %in% names(ac$rising_stars)) ac$rising_stars$display_author[1:min(3, nrow(ac$rising_stars))] else ac$rising_stars$author[1:min(3, nrow(ac$rising_stars))]
    stars <- stars[!is.na(stars) & nzchar(stars)]
    if (length(stars) > 0) {
      out <- c(out, paste("Rising stars:", paste(stars, collapse = ", ")))
    }
  }
  out
}

m1_rank_reportable_hypotheses <- function(hypotheses) {
  if (length(hypotheses) == 0) {
    return(character())
  }

  ids <- names(hypotheses)
  decisive_rank <- vapply(hypotheses, function(item) {
    result <- item$result %||% "unknown"
    if (identical(result, "inconclusive")) return(3L)
    if (identical(result, "reject")) return(1L)
    if (identical(result, "fail_to_reject")) return(2L)
    4L
  }, integer(1))

  preferred <- c("H01_1", "H01_4", "H01_5", "H01_7", "H01_9", "H01_3", "H01_2")
  preferred_rank <- match(ids, preferred)
  preferred_rank[is.na(preferred_rank)] <- length(preferred) + seq_len(sum(is.na(match(ids, preferred))))

  ids[order(decisive_rank, preferred_rank, ids)]
}

m1_report_hypotheses <- function(data) {
  hyp <- data$hypotheses %||% list()
  if ((hyp$status %||% "") != "success") return(character())

  summary <- hyp$summary %||% list()
  out <- c(
    sprintf("Hypotheses tested: %s", hyp$n_hypotheses %||% 0),
    sprintf("Rejected: %s | Failed to reject: %s | Inconclusive: %s",
            summary$n_rejected %||% 0,
            summary$n_failed_to_reject %||% 0,
            summary$n_inconclusive %||% 0)
  )

  hyp_rows <- hyp$hypotheses %||% list()
  if (length(hyp_rows) > 0) {
    key_ids <- utils::head(m1_rank_reportable_hypotheses(hyp_rows), 5)
    out <- c(out, vapply(key_ids, function(id) {
      item <- hyp_rows[[id]]
      label <- item$hypothesis %||% item$hyphypothesis %||% id
      interpretation <- item$interpretation %||% ""
      paste0(id, " [", item$result %||% "unknown", "]: ", label, ". ", interpretation)
    }, character(1)))
  }

  out
}

#' @export
build_m1_doc_types_tex <- function(result) {
  report <- build_m1_report(result)
  report$tex %||% character()
}

build_m1_report_tex <- function(title, sections) {
  if (length(sections) == 0) return(character())

  tex <- c(
    "\\documentclass[12pt]{article}",
    "\\usepackage{geometry}",
    "\\geometry{a4paper, margin=1in}",
    "\\begin{document}",
    paste0("\\title{", m1_escape_latex(title), "}"),
    "\\author{RBiblioSynth}",
    "\\date{\\today}",
    "\\maketitle"
  )

  for (section_name in names(sections)) {
    section <- sections[[section_name]]
    tex <- c(
      tex,
      paste0("\\section*{", m1_escape_latex(section$title), "}"),
      "\\begin{itemize}",
      paste0("\\item ", m1_escape_latex(section$lines)),
      "\\end{itemize}"
    )
  }

  c(tex, "\\end{document}")
}

m1_escape_latex <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("\\^", "\\\\textasciicircum{}", x, perl = TRUE)
  x
}

m1_prepare_doc_type_report_table <- function(tbl) {
  if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) {
    return(tibble::tibble(
      label = character(),
      value = numeric(),
      percentage = numeric()
    ))
  }

  out <- tibble::as_tibble(tbl)
  if (!"label" %in% names(out)) {
    out$label <- if ("type" %in% names(out)) out$type else as.character(seq_len(nrow(out)))
  }
  if (!"value" %in% names(out)) {
    out$value <- if ("count" %in% names(out)) out$count else numeric(nrow(out))
  }
  if (!"percentage" %in% names(out)) {
    if ("proportion" %in% names(out)) {
      out$percentage <- as.numeric(out$proportion) * 100
    } else {
      total <- sum(out$value, na.rm = TRUE)
      out$percentage <- if (total > 0) round((out$value / total) * 100, 2) else 0
    }
  }

  out$label <- as.character(out$label)
  out$value <- suppressWarnings(as.numeric(out$value))
  out$percentage <- suppressWarnings(as.numeric(out$percentage))
  out$value[is.na(out$value)] <- 0
  out$percentage[is.na(out$percentage)] <- 0
  out[, c("label", "value", "percentage")]
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
