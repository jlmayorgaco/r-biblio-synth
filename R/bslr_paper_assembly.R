# ============================================================================
# bslr_paper_assembly.R - Journal-oriented Quarto/LaTeX assembly for B-SLR
# ============================================================================

#' Assemble a journal-oriented B-SLR paper bundle
#'
#' Builds source artifacts for a journal-style manuscript on top of a completed
#' `run_bslr()` result. The bundle always includes Quarto and LaTeX sources and
#' optionally renders Quarto outputs when the `quarto` executable is available.
#'
#' @param bslr_result Result produced by [run_bslr()].
#' @param config Configuration list.
#' @param render Logical. If `TRUE`, attempt to render Quarto outputs when
#'   Quarto is installed.
#' @return A list describing the assembled bundle and generated files.
#' @export
bslr_assemble_paper <- function(bslr_result,
                                config = biblio_config(),
                                render = TRUE) {
  config <- merge_biblio_config(config)

  is_bslr_like <- inherits(bslr_result, "biblio_module_result") &&
    identical(bslr_result$module_id %||% "", "bslr") &&
    is.list(bslr_result$data) &&
    is.list(bslr_result$data$modules %||% list()) &&
    is.list(bslr_result$data$protocol %||% list())

  if (!inherits(bslr_result, "bslr_workflow_result") && !is_bslr_like) {
    cli::cli_abort("bslr_assemble_paper() requires a result returned by run_bslr().")
  }

  protocol <- bslr_result$data$protocol %||% list()
  protocol <- bslr_read_protocol(protocol)
  module_results <- bslr_result$data$modules %||% list()
  checkpoints <- bslr_result$data$checkpoints %||% list()
  journal_assessment <- bslr_result$data$journal_assessment %||% list()
  bibliometric_map <- bslr_result$data$bibliometric_map %||% list()
  scaffolds <- bslr_result$data$slr_scaffolds %||% list()

  paper_dir <- build_output_path("bslr", "paper", config = config, create = TRUE)
  manuscript_qmd <- file.path(paper_dir, "bslr_manuscript.qmd")
  appendix_qmd <- file.path(paper_dir, "bslr_appendix.qmd")
  ieee_tex <- file.path(paper_dir, "bslr_ieee_manuscript.tex")
  manifest_json <- file.path(paper_dir, "bslr_paper_manifest.json")
  template_resources <- bslr_stage_template_resources(paper_dir)
  exhibits <- bslr_collect_paper_exhibits(
    module_results = module_results,
    bibliometric_map = bibliometric_map,
    journal_assessment = journal_assessment,
    protocol = protocol,
    paper_dir = paper_dir
  )

  manuscript_lines <- bslr_build_manuscript_qmd(
    protocol = protocol,
    module_results = module_results,
    checkpoints = checkpoints,
    bibliometric_map = bibliometric_map,
    journal_assessment = journal_assessment,
    scaffolds = scaffolds,
    exhibits = exhibits,
    template_resources = template_resources,
    config = config
  )
  appendix_lines <- bslr_build_appendix_qmd(
    protocol = protocol,
    module_results = module_results,
    checkpoints = checkpoints,
    bibliometric_map = bibliometric_map,
    journal_assessment = journal_assessment,
    scaffolds = scaffolds,
    exhibits = exhibits,
    template_resources = template_resources,
    config = config
  )
  tex_lines <- bslr_build_ieee_tex(
    protocol = protocol,
    module_results = module_results,
    checkpoints = checkpoints,
    bibliometric_map = bibliometric_map,
    journal_assessment = journal_assessment,
    scaffolds = scaffolds,
    exhibits = exhibits,
    template_resources = template_resources
  )

  write_text_report(manuscript_lines, manuscript_qmd)
  write_text_report(appendix_lines, appendix_qmd)
  write_text_report(tex_lines, ieee_tex)

  quarto_rendered <- bslr_render_quarto_bundle(
    manuscript_qmd = manuscript_qmd,
    appendix_qmd = appendix_qmd,
    config = config,
    render = render
  )

  bundle <- list(
    status = if (length(quarto_rendered$errors) > 0) "warning" else "success",
    paper_dir = paper_dir,
    sources = list(
      manuscript_qmd = manuscript_qmd,
      appendix_qmd = appendix_qmd,
      ieee_tex = ieee_tex
    ),
    rendered = quarto_rendered$rendered,
    exhibits = exhibits,
    template_resources = template_resources,
    notes = quarto_rendered$notes,
    errors = quarto_rendered$errors
  )

  write_json_artifact(bundle, manifest_json)
  bundle$manifest_json <- manifest_json
  bundle$files <- c(
    manuscript_qmd,
    appendix_qmd,
    ieee_tex,
    manifest_json,
    unname(template_resources$files %||% character()),
    exhibits$inventory_csv %||% character(),
    exhibits$inventory_json %||% character(),
    unname(exhibits$files %||% character()),
    unname(quarto_rendered$rendered)
  )
  bundle
}

bslr_find_template_resource_dir <- function() {
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  candidates <- character()

  path <- wd
  repeat {
    candidates <- c(
      candidates,
      file.path(path, "resources", "template-latex"),
      file.path(path, "inst", "resources", "template-latex")
    )
    parent <- dirname(path)
    if (identical(parent, path)) {
      break
    }
    path <- parent
  }

  installed <- system.file("resources", "template-latex", package = "RBiblioSynth")
  if (nzchar(installed)) {
    candidates <- c(candidates, installed)
  }

  candidates <- unique(candidates[file.exists(candidates)])
  if (length(candidates) == 0) {
    return("")
  }
  candidates[1]
}

bslr_stage_template_resources <- function(paper_dir) {
  source_dir <- bslr_find_template_resource_dir()
  if (!nzchar(source_dir)) {
    return(list(
      source_dir = "",
      paper_dir = "",
      relative_dir = "",
      header_path = "",
      latex_template_path = "",
      files = character()
    ))
  }

  dest_dir <- file.path(paper_dir, "template-latex")
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  source_files <- list.files(source_dir, full.names = TRUE, recursive = FALSE)
  copied <- character()
  for (src in source_files) {
    dest <- file.path(dest_dir, basename(src))
    if (file.copy(src, dest, overwrite = TRUE)) {
      copied <- c(copied, dest)
    }
  }

  list(
    source_dir = source_dir,
    paper_dir = dest_dir,
    relative_dir = "template-latex",
    header_path = file.path(dest_dir, "ieee_header.tex"),
    latex_template_path = file.path(dest_dir, "ieee_manuscript_template.tex"),
    files = copied
  )
}

bslr_markdown_paragraphs <- function(paragraphs) {
  paragraphs <- as.character(paragraphs %||% character())
  paragraphs <- trimws(paragraphs)
  paragraphs <- paragraphs[nzchar(paragraphs)]
  if (length(paragraphs) == 0) {
    return(character())
  }
  unlist(lapply(paragraphs, function(p) c(p, "")), use.names = FALSE)
}

bslr_tex_paragraphs <- function(paragraphs) {
  paragraphs <- as.character(paragraphs %||% character())
  paragraphs <- trimws(paragraphs)
  paragraphs <- paragraphs[nzchar(paragraphs)]
  if (length(paragraphs) == 0) {
    return("")
  }
  paste(m0_escape_latex(paragraphs), collapse = "\n\n")
}

bslr_ensure_sentence <- function(text) {
  text <- trimws(as.character(text %||% ""))
  if (!nzchar(text)) {
    return("")
  }
  if (grepl("[.!?]$", text)) {
    return(text)
  }
  paste0(text, ".")
}

bslr_text_value <- function(value, fallback = "") {
  value <- as.character(value %||% "")
  value <- trimws(value)
  if (length(value) == 0 || is.na(value[1]) || !nzchar(value[1]) || identical(toupper(value[1]), "NA")) {
    return(fallback)
  }
  value[1]
}

bslr_report_value <- function(value, fallback = "not reported") {
  out <- bslr_text_value(value, fallback)
  if (!nzchar(out) || identical(toupper(out), "NA")) {
    return(fallback)
  }
  out
}

bslr_percent_text <- function(value, digits = 1L, fallback = "not estimable") {
  value <- suppressWarnings(as.numeric(value %||% NA_real_))
  if (!is.finite(value)) {
    return(fallback)
  }
  paste0(round(100 * value, digits), "%")
}

bslr_number_text <- function(value, digits = 2L, fallback = "not estimable") {
  value <- suppressWarnings(as.numeric(value %||% NA_real_))
  if (!is.finite(value)) {
    return(fallback)
  }
  format(round(value, digits), nsmall = digits, trim = TRUE, scientific = FALSE)
}

bslr_take_paragraphs <- function(paragraphs, from = 1L, to = length(paragraphs)) {
  paragraphs <- as.character(paragraphs %||% character())
  paragraphs <- trimws(paragraphs)
  paragraphs <- paragraphs[nzchar(paragraphs)]
  if (length(paragraphs) == 0) {
    return(character())
  }
  from <- max(1L, as.integer(from))
  to <- min(length(paragraphs), as.integer(to))
  if (to < from) {
    return(character())
  }
  paragraphs[from:to]
}

bslr_join_with_and <- function(values) {
  values <- trimws(as.character(values %||% character()))
  values <- values[nzchar(values)]
  if (length(values) == 0) return("")
  if (length(values) == 1) return(values)
  if (length(values) == 2) return(paste(values, collapse = " and "))
  paste0(paste(values[-length(values)], collapse = ", "), ", and ", values[length(values)])
}

bslr_human_label <- function(values) {
  values <- as.character(values %||% character())
  values <- gsub("_", " ", values, fixed = TRUE)
  trimws(values)
}

bslr_join_human_terms <- function(values) {
  bslr_join_with_and(bslr_human_label(values))
}

bslr_join_clauses <- function(values) {
  values <- as.character(values %||% character())
  values <- trimws(values)
  values <- values[nzchar(values)]
  if (length(values) == 0) {
    return("")
  }
  values <- gsub("[[:space:]]*[.;:]+$", "", values)
  paste(values, collapse = "; ")
}

bslr_extract_report_value <- function(lines, pattern, group = 1L) {
  lines <- as.character(lines %||% character())
  hit <- grep(pattern, lines, perl = TRUE, ignore.case = FALSE, value = TRUE)
  if (length(hit) == 0) {
    return(NA_character_)
  }
  match <- regexec(pattern, hit[1], perl = TRUE, ignore.case = FALSE)
  groups <- regmatches(hit[1], match)[[1]]
  if (identical(group, 0L)) {
    return(trimws(groups[1]))
  }
  if (length(groups) <= group) {
    return(NA_character_)
  }
  trimws(groups[group + 1L])
}

bslr_extract_copy_ready_paragraphs <- function(lines) {
  lines <- as.character(lines %||% character())
  lines <- lines[!grepl("^=+$", lines)]
  lines <- lines[!grepl("^Generated:", lines)]
  start <- match("Copy-ready paragraphs", lines)
  if (!is.na(start)) {
    lines <- lines[(start + 1L):length(lines)]
  }
  lines <- lines[!grepl("^Readiness band:", lines)]
  lines <- lines[!grepl("^Readiness score:", lines)]
  groups <- split(lines, cumsum(lines == ""))
  paragraphs <- vapply(groups, function(chunk) paste(trimws(chunk[nzchar(trimws(chunk))]), collapse = " "), character(1))
  paragraphs <- trimws(paragraphs)
  paragraphs[nzchar(paragraphs)]
}

bslr_build_intro_paragraphs <- function(protocol, module_results, bibliometric_map) {
  review_topic <- bslr_report_value(protocol$review_topic, "the focal research stream")
  question <- bslr_report_value(protocol$research_question, "the guiding research question has not yet been inserted")
  gap <- bslr_text_value(protocol$research_gap, "")
  objectives <- protocol$objectives %||% character()
  cluster_summary <- bibliometric_map$cluster_summary %||% data.frame()
  n_clusters <- if (is.data.frame(cluster_summary)) nrow(cluster_summary) else 0L
  objective_sentence <- if (length(objectives) > 0) {
    paste0("The review is organized around three operational objectives: ", bslr_join_clauses(objectives), ".")
  } else {
    "The review links corpus description, temporal modeling, geographical comparison, and cluster-guided synthesis."
  }

  p1 <- paste0(
    "This paper studies ",
    review_topic,
    " through the 10-step Bibliometric-Systematic Literature Review (B-SLR) logic proposed by Marzi et al. (2025). The research question is: ",
    question,
    ". The review therefore treats bibliometric analysis as a structured entry point into systematic synthesis and theory development, not as an end in itself"
  )
  p2 <- paste0(
    if (nzchar(gap)) paste0(gap, " ") else "",
    objective_sentence,
    " RBiblioSynth is used as the reproducible analysis engine. It replaces external mapping software with a documented pipeline for data consolidation, PRISMA accounting, descriptive bibliometrics, temporal modeling, country-level comparison, cluster-guided sampling, and audit-ready manuscript assembly."
  )
  p3 <- paste0(
    "The bibliometric map identified ",
    n_clusters,
    if (n_clusters == 1) " thematic cluster. " else " thematic clusters. ",
    "These clusters are not treated as final theory by themselves. They define the reading frame for coding, within-cluster synthesis, cross-cluster comparison, and the contribution assessment checkpoint."
  )
  c(bslr_ensure_sentence(p1), bslr_ensure_sentence(p2), bslr_ensure_sentence(p3))
}

bslr_build_methods_paragraphs <- function(protocol, module_results, bibliometric_map, journal_assessment) {
  prisma_lines <- bslr_clean_report_lines(module_results$m0$artifacts$reports$prisma_methodology$lines %||% character())
  screening_summary <- module_results$m0$data$screening_summary %||% list()
  topic <- bslr_report_value(protocol$review_topic, "the focal research stream")
  question <- bslr_report_value(protocol$research_question, "the guiding research question has not yet been specified")
  inclusion <- bslr_report_value(bslr_join_clauses(protocol$inclusion_criteria %||% character()), "not yet reported")
  exclusion <- bslr_report_value(bslr_join_clauses(protocol$exclusion_criteria %||% character()), "not yet reported")
  databases <- bslr_join_with_and(c(protocol$search$primary_database %||% character(), protocol$search$databases %||% character()))
  databases <- bslr_report_value(databases, "the configured bibliographic databases")
  crosscheck <- bslr_join_with_and(c(protocol$search$secondary_databases %||% character(), protocol$search$crosscheck_databases %||% character()))
  search_string <- bslr_report_value(protocol$search$queries$primary %||% protocol$search$search_string, "the validated search string should be inserted here")
  year_start <- bslr_report_value(protocol$search$time_span$start, "not reported")
  year_end <- bslr_report_value(protocol$search$time_span$end, "not reported")
  language <- bslr_report_value(protocol$search$language, "the protocol-specified language limits")
  doc_types <- bslr_join_with_and(protocol$search$document_types %||% character())
  doc_types <- bslr_report_value(doc_types, "the protocol-specified document types")
  raw_records <- bslr_report_value(bslr_extract_report_value(prisma_lines, "^Raw records:\\s*([0-9,]+);"))
  unique_records <- bslr_report_value(bslr_extract_report_value(prisma_lines, "^Raw records:\\s*[0-9,]+; unique records after deduplication:\\s*([0-9,]+);"))
  records_screened <- bslr_report_value(bslr_extract_report_value(prisma_lines, "^Screening considered\\s+([0-9,]+)\\s+records,"))
  excluded_screening <- bslr_report_value(bslr_extract_report_value(prisma_lines, "^Screening considered\\s+[0-9,]+\\s+records, with\\s+([0-9,]+)\\s+exclusions"))
  fulltext <- bslr_report_value(bslr_extract_report_value(prisma_lines, "^Eligibility review assessed\\s+([0-9,]+)\\s+full-text"))
  excluded_fulltext <- bslr_report_value(bslr_extract_report_value(prisma_lines, "^Eligibility review assessed\\s+[0-9,]+\\s+full-text records and excluded\\s+([0-9,]+)\\."))
  included <- bslr_report_value(bslr_extract_report_value(prisma_lines, "^The final synthesis included\\s+([0-9,]+)\\s+studies\\."))
  agreement <- bslr_text_value(bslr_extract_report_value(prisma_lines, "^Krippendorff's alpha:\\s*([0-9.]+)\\."), "")
  ranking_rule <- bslr_report_value(protocol$bibliometric$ranking_rule, "configured")
  approach <- bslr_report_value(bibliometric_map$approach %||% bibliometric_map$selected_approach %||% protocol$bibliometric$approach, "configured bibliometric mapping")
  n_clusters <- if (is.data.frame(bibliometric_map$cluster_summary)) nrow(bibliometric_map$cluster_summary) else 0L
  sample_selected <- if (is.data.frame(bibliometric_map$sample_selected)) nrow(bibliometric_map$sample_selected) else 0L
  readiness <- journal_assessment$summary %||% list()
  mode <- bslr_report_value(readiness$methodological_mode %||% screening_summary$methodological_mode, "counts-only")
  has_full_ledger <- identical(mode, "full-review") || isTRUE(screening_summary$ok %||% FALSE)
  agreement_sentence <- if (nzchar(agreement)) {
    paste0("Inter-reviewer agreement was quantified with Krippendorff's alpha = ", agreement, ".")
  } else if (has_full_ledger) {
    "The screening ledger is available, but inter-reviewer agreement was not estimable from the supplied decisions."
  } else {
    "No reviewer-level screening ledger was supplied, so the PRISMA section is reported as counts-only and does not claim inter-reviewer reliability."
  }

  p1 <- paste0(
    "The review followed a B-SLR design for ",
    topic,
    ". The protocol fixed the research question before data consolidation: ",
    question,
    ". The perimeter was controlled through explicit inclusion criteria (",
    inclusion,
    ") and exclusion criteria (",
    exclusion,
    "). These choices correspond to B-SLR Steps 1 to 3: defining the boundary, validating the search string, and selecting the databases before any analytical model is fitted."
  )
  p2 <- paste0(
    "The search was executed in ",
    databases,
    if (nzchar(crosscheck)) paste0(", with cross-checking against ", crosscheck) else "",
    ". The exact query was: \"",
    search_string,
    "\". The search covered ",
    year_start,
    " to ",
    year_end,
    ", was limited to ",
    language,
    ", and retained document types ",
    doc_types,
    "."
  )
  p3 <- paste0(
    "M0 harmonized source exports before merging them, then removed duplicate records by DOI and normalized title-year evidence. This implements the B-SLR data-consolidation checkpoint and avoids mixing raw database schemas downstream. The resulting PRISMA audit reports ",
    raw_records,
    " raw records and ",
    unique_records,
    " unique records after harmonization and deduplication."
  )
  p4 <- paste0(
    if (has_full_ledger) "The full-review ledger records " else "The available counts indicate ",
    records_screened,
    " title-and-abstract screening decisions with ",
    excluded_screening,
    " exclusions, followed by ",
    fulltext,
    " eligibility assessments and ",
    excluded_fulltext,
    " full-text exclusions. The synthesis corpus contains ",
    included,
    " studies. ",
    agreement_sentence
  )
  p5 <- paste0(
    "The bibliometric stage used RBiblioSynth's internal ",
    gsub("_", " ", approach, fixed = TRUE),
    " engine rather than VOSviewer. The paper therefore reports the aggregation rule, cluster count, sampling rule, and reproducibility metadata instead of a VOSviewer version number. It produced ",
    n_clusters,
    " thematic clusters and a ranked sample of ",
    sample_selected,
    " representative documents using the ",
    ranking_rule,
    " rule. The export readiness band was ",
    gsub("_", " ", bslr_text_value(readiness$readiness_band, "unknown"), fixed = TRUE),
    " (",
    round(100 * (readiness$readiness_score %||% 0), 1),
    "%), which defines what can be claimed automatically and what still requires human thematic synthesis."
  )
  p6 <- paste0(
    "The final B-SLR stages remain deliberately human-led. RBiblioSynth generates the coding assets, evidence matrix, representative sample, and synthesis scaffold; the review team must still read the selected papers, reconcile cluster meanings, and convert the evidence into the chosen theorising outputs. This separation prevents the manuscript from presenting automated bibliometric patterns as completed theory."
  )

  c(
    bslr_ensure_sentence(p1),
    bslr_ensure_sentence(p2),
    bslr_ensure_sentence(p3),
    bslr_ensure_sentence(p4),
    bslr_ensure_sentence(p5),
    bslr_ensure_sentence(p6)
  )
}

bslr_build_m1_paragraphs <- function(module_result) {
  lines <- bslr_compact_module_report(module_result)
  docs <- bslr_text_value(bslr_extract_report_value(lines, "^Documents\\s*:\\s*(.+)$"), "NA")
  sources <- bslr_text_value(bslr_extract_report_value(lines, "^Sources\\s*:\\s*(.+)$"), "NA")
  authors <- bslr_text_value(bslr_extract_report_value(lines, "^Authors\\s*:\\s*(.+)$"), "NA")
  timespan <- bslr_text_value(bslr_extract_report_value(lines, "^Timespan\\s*:\\s*(.+)$"), "NA")
  article_share <- bslr_text_value(bslr_extract_report_value(lines, "^Article\\s*:\\s*(.+)$"), "NA")
  conf_share <- bslr_text_value(bslr_extract_report_value(lines, "^Conference Paper\\s*:\\s*(.+)$"), "NA")
  review_share <- bslr_extract_report_value(lines, "^Review\\s*:\\s*(.+)$")
  top_country_name <- bslr_text_value(bslr_extract_report_value(lines, "^1 \\.\\s*(.+?)\\s*-\\s*([0-9]+) articles$", group = 1L), "the leading country")
  top_country_count <- bslr_text_value(bslr_extract_report_value(lines, "^1 \\.\\s*(.+?)\\s*-\\s*([0-9]+) articles$", group = 2L), "NA")
  country_gini <- bslr_text_value(bslr_extract_report_value(lines, "^Country production Gini:\\s*(.+)$"), "NA")
  top_source_name <- bslr_text_value(bslr_extract_report_value(lines, "^1 \\.\\s*(.+?)\\s*-\\s*([0-9]+) documents$", group = 1L), "the leading source")
  top_source_count <- bslr_text_value(bslr_extract_report_value(lines, "^1 \\.\\s*(.+?)\\s*-\\s*([0-9]+) documents$", group = 2L), "NA")
  keyword_1 <- bslr_text_value(bslr_extract_report_value(lines, "^1 \\.\\s*(.+?)\\s*-\\s*([0-9]+)$", group = 1L), "the dominant keyword")
  keyword_2 <- bslr_text_value(bslr_extract_report_value(lines, "^2 \\.\\s*(.+?)\\s*-\\s*([0-9]+)$", group = 1L), "a second major keyword")
  keyword_3 <- bslr_text_value(bslr_extract_report_value(lines, "^3 \\.\\s*(.+?)\\s*-\\s*([0-9]+)$", group = 1L), "a third major keyword")
  collaboration_index <- bslr_text_value(bslr_extract_report_value(lines, "^Collaboration index:\\s*(.+)$"), "NA")
  best_distribution <- bslr_text_value(bslr_extract_report_value(lines, "^Best fit distribution:\\s*(.+)$"), "an over-dispersed distribution")

  p1 <- paste0(
    "The descriptive layer covers ", docs, " documents from ", sources,
    " publication sources and ", authors, " author occurrences, spanning ",
    timespan, ". Journal articles account for ", article_share,
    ", while conference papers account for ", conf_share,
    if (!is.na(review_share)) paste0(" and reviews account for ", review_share) else "",
    "."
  )
  p2 <- paste0(
    "The corpus is concentrated but not dominated by a single outlet or country. ",
    top_country_name, " leads production with ", top_country_count,
    " documents, and the corresponding country-level Gini coefficient reaches ",
    country_gini, ". On the source side, ", top_source_name,
    " is the leading venue with ", top_source_count,
    " documents. The keyword profile is led by ", keyword_1, ", ",
    keyword_2, ", and ", keyword_3,
    ", which points to a field shaped by estimation, synchronization, and signal-processing problems. ",
    "The citation distribution is best described by ", best_distribution,
    ", and the collaboration index of ", collaboration_index,
    " indicates that multi-authored work is the default mode of production."
  )
  c(bslr_ensure_sentence(p1), bslr_ensure_sentence(p2))
}

bslr_build_m2_paragraphs <- function(module_result) {
  lines <- bslr_compact_module_report(module_result)
  advanced <- module_result$data$advanced_journal %||% list()
  gr <- advanced$growth_regimes$summary %||% data.frame()
  fv <- advanced$forecast_validation$leaderboard %||% data.frame()
  cc <- advanced$changepoint_consensus$table %||% data.frame()
  cal <- advanced$forecast_validation$interval_calibration %||% data.frame()
  headline_model <- bslr_report_value(
    if (is.data.frame(gr) && nrow(gr) > 0) gr$headline_model[1] else bslr_extract_report_value(lines, "^Headline model:\\s*(.+)$"),
    "the selected interpretable model"
  )
  benchmark_model <- bslr_report_value(bslr_extract_report_value(lines, "^Flexible benchmark:\\s*(.+)$"), "the flexible benchmark")
  cagr <- bslr_report_value(bslr_extract_report_value(lines, "^CAGR:\\s*(.+)$"), "not estimable")
  recent_cagr <- bslr_report_value(bslr_extract_report_value(lines, "^Recent CAGR:\\s*(.+)$"), "not estimable")
  tau <- bslr_report_value(bslr_extract_report_value(lines, "^Mann-Kendall tau:\\s*(.+)$"), "not estimable")
  capacity <- if (is.data.frame(gr) && nrow(gr) > 0) suppressWarnings(as.numeric(gr$capacity[1])) else NA_real_
  inflection <- if (is.data.frame(gr) && nrow(gr) > 0) suppressWarnings(as.numeric(gr$inflection_year[1])) else NA_real_
  peak_growth <- if (is.data.frame(gr) && nrow(gr) > 0) suppressWarnings(as.numeric(gr$peak_growth_year[1])) else NA_real_
  distance <- if (is.data.frame(gr) && nrow(gr) > 0) suppressWarnings(as.numeric(gr$distance_to_saturation[1])) else NA_real_
  saturation_sentence <- if (is.finite(capacity)) {
    paste0(
      "The fitted carrying capacity is ",
      bslr_number_text(capacity, digits = 1L),
      " cumulative articles; the inflection year is ",
      bslr_number_text(inflection, digits = 1L),
      ", the peak-growth year is ",
      bslr_number_text(peak_growth, digits = 1L),
      ", and the remaining distance to saturation is ",
      bslr_percent_text(distance),
      "."
    )
  } else {
    "The advanced growth layer did not estimate a finite saturation envelope, so saturation claims should be treated as inconclusive."
  }
  forecast_sentence <- "Rolling-origin forecast validation was not available for the manuscript bundle."
  if (is.data.frame(fv) && nrow(fv) > 0 && all(c("model", "horizon", "mase") %in% names(fv))) {
    fv$mase <- suppressWarnings(as.numeric(fv$mase))
    fv <- fv[is.finite(fv$mase), , drop = FALSE]
    if (nrow(fv) > 0) {
      best <- fv[order(fv$mase, fv$horizon), , drop = FALSE][1, , drop = FALSE]
      forecast_sentence <- paste0(
        "Rolling-origin validation identifies ",
        best$model[1],
        " as the strongest reported comparator at horizon ",
        best$horizon[1],
        " with MASE = ",
        bslr_number_text(best$mase[1], digits = 2L),
        if (best$mase[1] < 1) ", outperforming the naive benchmark." else ", not clearly outperforming the naive benchmark."
      )
    }
  }
  break_sentence <- "No consensus structural break was strong enough to headline the temporal interpretation."
  if (is.data.frame(cc) && nrow(cc) > 0 && all(c("breakpoint_year", "support_count", "decision") %in% names(cc))) {
    top_break <- cc[order(-suppressWarnings(as.numeric(cc$support_count)), cc$breakpoint_year), , drop = FALSE][1, , drop = FALSE]
    break_sentence <- paste0(
      "The changepoint consensus table highlights ",
      top_break$breakpoint_year[1],
      " as the leading candidate break year, supported by ",
      top_break$support_count[1],
      " method(s) and classified as ",
      top_break$decision[1],
      "."
    )
  }
  calibration_sentence <- "Interval calibration should be audited in the appendix before using forecasts as claims."
  if (is.data.frame(cal) && nrow(cal) > 0 && all(c("nominal", "coverage") %in% names(cal))) {
    cal$gap <- abs(suppressWarnings(as.numeric(cal$coverage)) - suppressWarnings(as.numeric(cal$nominal)))
    cal <- cal[is.finite(cal$gap), , drop = FALSE]
    if (nrow(cal) > 0) {
      calibration_sentence <- paste0(
        "Prediction-interval calibration has a median absolute coverage gap of ",
        bslr_percent_text(stats::median(cal$gap, na.rm = TRUE)),
        ", so the paper reports forecast intervals as uncertainty diagnostics rather than deterministic projections."
      )
    }
  }

  p1 <- paste0(
    "M2 treats annual production as a temporal system rather than as a descriptive line chart. ",
    headline_model,
    " is the headline interpretable model and ",
    benchmark_model,
    " is retained as the flexible benchmark. Long-run CAGR is ",
    cagr,
    ", recent CAGR is ",
    recent_cagr,
    ", and Mann-Kendall tau is ",
    tau,
    ". ",
    saturation_sentence
  )
  p2 <- paste0(
    break_sentence,
    " ",
    forecast_sentence,
    " ",
    calibration_sentence,
    " These diagnostics support a maturation narrative only when the parametric model, changepoint consensus, residual behavior, and forecast validation point in the same direction."
  )
  c(bslr_ensure_sentence(p1), bslr_ensure_sentence(p2))
}

bslr_build_m3_paragraphs <- function(module_result) {
  lines <- bslr_compact_module_report(module_result)
  advanced <- module_result$data$advanced_journal %||% list()
  countries <- bslr_report_value(bslr_extract_report_value(lines, "^Countries:\\s*(.+)$"), "not reported")
  leading_producer <- bslr_report_value(bslr_extract_report_value(lines, "^Leading producer\\s*:\\s*(.+)$"), "the leading producer")
  leading_citer <- bslr_report_value(bslr_extract_report_value(lines, "^Leading citer\\s*:\\s*(.+)$"), "the leading citer")
  mcp_ratio <- bslr_report_value(bslr_extract_report_value(lines, "^Overall MCP ratio:\\s*(.+)$"), "not estimable")
  prod_gini <- bslr_report_value(bslr_extract_report_value(lines, "^Production Gini\\s*:\\s*(.+)$"), "not estimable")
  cit_gini <- bslr_report_value(bslr_extract_report_value(lines, "^Citations\\s+Gini\\s*:\\s*(.+)$"), "not estimable")
  concentration <- advanced$geo_concentration$table %||% data.frame()
  premium <- advanced$collaboration_premium$table %||% data.frame()
  trajectories <- advanced$trajectories$table %||% data.frame()
  mobility <- advanced$mobility$rank_stability %||% data.frame()
  prod_conc <- if (is.data.frame(concentration) && nrow(concentration) > 0 && "metric" %in% names(concentration)) {
    concentration[concentration$metric == "production", , drop = FALSE]
  } else {
    data.frame()
  }
  conc_sentence <- if (nrow(prod_conc) > 0) {
    paste0(
      "The advanced concentration dashboard reports production Gini = ",
      bslr_number_text(prod_conc$gini[1], digits = 3L),
      ", HHI = ",
      bslr_number_text(prod_conc$hhi[1], digits = 3L),
      ", Theil = ",
      bslr_number_text(prod_conc$theil[1], digits = 3L),
      ", and top-5 share = ",
      bslr_percent_text(prod_conc$top5_share[1]),
      "."
    )
  } else {
    paste0("The core report estimates production Gini = ", prod_gini, " and citation Gini = ", cit_gini, ".")
  }
  premium_sentence <- if (is.data.frame(premium) && nrow(premium) > 0 && "mean_difference" %in% names(premium)) {
    paste0(
      "The MCP/SCP comparison estimates a mean citation premium of ",
      bslr_number_text(premium$mean_difference[1], digits = 2L),
      " citations per article with a bootstrap interval [",
      bslr_number_text(premium$ci_low[1], digits = 2L),
      ", ",
      bslr_number_text(premium$ci_high[1], digits = 2L),
      "]."
    )
  } else {
    paste0("International collaboration is summarized by an MCP ratio of ", mcp_ratio, ".")
  }
  trajectory_sentence <- "Country trajectories were not rich enough to support a stable emerging-declining classification."
  if (is.data.frame(trajectories) && nrow(trajectories) > 0 && "trajectory_class" %in% names(trajectories)) {
    counts <- sort(table(trajectories$trajectory_class), decreasing = TRUE)
    trajectory_sentence <- paste0(
      "Trajectory classes are distributed as ",
      paste(sprintf("%s=%d", names(counts), as.integer(counts)), collapse = ", "),
      "."
    )
  }
  stability_sentence <- ""
  if (is.data.frame(mobility) && nrow(mobility) > 0 && all(c("method", "estimate", "p_value") %in% names(mobility))) {
    spearman <- mobility[mobility$method == "spearman", , drop = FALSE]
    if (nrow(spearman) > 0) {
      stability_sentence <- paste0(
        " Rank stability is summarized by Spearman rho = ",
        bslr_number_text(spearman$estimate[1], digits = 3L),
        " (p = ",
        bslr_number_text(spearman$p_value[1], digits = 4L),
        ")."
      )
    }
  }

  p1 <- paste0(
    "M3 compares geographical production, impact, collaboration, and mobility across ",
    countries,
    " countries. ",
    leading_producer, " leads publication volume, whereas ", leading_citer,
    " leads citation impact. ",
    conc_sentence
  )
  p2 <- paste0(
    premium_sentence,
    " ",
    trajectory_sentence,
    stability_sentence,
    " These country-level outputs are descriptive and comparative; any economic or regional interpretation remains correlational unless an external causal design is added."
  )
  c(bslr_ensure_sentence(p1), bslr_ensure_sentence(p2))
}

bslr_build_cluster_paragraphs <- function(bibliometric_map, protocol) {
  cluster_summary <- bibliometric_map$cluster_summary %||% data.frame()
  sample_selected <- bibliometric_map$sample_selected %||% data.frame()
  if (!is.data.frame(cluster_summary) || nrow(cluster_summary) == 0) {
    return(c("The bibliometric mapping stage did not produce a stable cluster structure, so the cluster-guided synthesis still needs a human redesign."))
  }

  top_clusters <- utils::head(cluster_summary, 3)
  descriptors <- vapply(seq_len(nrow(top_clusters)), function(i) {
    row <- top_clusters[i, , drop = FALSE]
    sprintf(
      "Cluster %s centers on %s and contains %s documents with a mean citation count of %.2f",
      row$cluster_id,
      row$cluster_label,
      row$article_count,
      as.numeric(row$mean_citations)
    )
  }, character(1))

  p1 <- paste0(
    "The bibliometric map is used as a sampling and sense-making device for the systematic review. ",
    paste(descriptors, collapse = ". "),
    "."
  )
  p2 <- paste0(
    "The current ranked sample contains ",
    if (is.data.frame(sample_selected)) nrow(sample_selected) else 0,
    " representative documents selected with the ",
    protocol$bibliometric$ranking_rule %||% "configured",
    " rule. The paper should convert those ranked records into coded evidence before making final claims about a research agenda, taxonomy, conceptual framework, or metatheory."
  )
  c(bslr_ensure_sentence(p1), bslr_ensure_sentence(p2))
}

bslr_build_discussion_paragraphs <- function(protocol, journal_assessment) {
  modes <- bslr_join_human_terms(protocol$theorising$modes %||% character())
  pending <- journal_assessment$pending_actions %||% character()
  p1 <- paste0(
    "The contribution separates automated evidence from human interpretation. The automated layer supports claims about corpus structure, temporal maturation, geographical concentration, and cluster composition; the systematic-review layer explains why those patterns matter theoretically. The final argument should convert coded cluster evidence into ",
    if (nzchar(modes)) modes else "the selected theorising outputs",
    " and should state the review position on the miner-prospector continuum."
  )
  p2 <- if (length(pending) > 0) {
    paste0("Before submission, the review team should close these methodological actions: ", paste(pending, collapse = "; "), ".")
  } else {
    "The protocol gates are closed in the current bundle; the remaining work is article-level coding, synthesis writing, and theory development."
  }
  c(bslr_ensure_sentence(p1), bslr_ensure_sentence(p2))
}

bslr_build_abstract_paragraph <- function(protocol, module_results, bibliometric_map) {
  m1_lines <- bslr_compact_module_report(module_results$m1 %||% list())
  m2_lines <- bslr_compact_module_report(module_results$m2 %||% list())
  m3_lines <- bslr_compact_module_report(module_results$m3 %||% list())
  prisma_lines <- bslr_clean_report_lines(module_results$m0$artifacts$reports$prisma_methodology$lines %||% character())

  docs <- bslr_report_value(bslr_extract_report_value(m1_lines, "^Documents\\s*:\\s*(.+)$"))
  timespan <- bslr_report_value(bslr_extract_report_value(m1_lines, "^Timespan\\s*:\\s*(.+)$"))
  included <- bslr_report_value(bslr_extract_report_value(prisma_lines, "^The final synthesis included\\s+([0-9,]+)\\s+studies\\."))
  clusters <- if (is.data.frame(bibliometric_map$cluster_summary)) nrow(bibliometric_map$cluster_summary) else 0L
  model <- bslr_report_value(bslr_extract_report_value(m2_lines, "^Headline model:\\s*(.+)$"), "an interpretable growth model")
  forecast <- bslr_report_value(bslr_extract_report_value(m2_lines, "^Best forecasting model:\\s*(.+)$"), "the selected forecasting model")
  leading_producer <- bslr_report_value(bslr_extract_report_value(m3_lines, "^Leading producer\\s*:\\s*(.+)$"), "the leading producer")
  leading_citer <- bslr_report_value(bslr_extract_report_value(m3_lines, "^Leading citer\\s*:\\s*(.+)$"), "the leading citer")

  abstract <- paste0(
    "This paper applies a 10-step Bibliometric-Systematic Literature Review to ",
    protocol$review_topic %||% "the focal topic",
    " using RBiblioSynth as the reproducible analysis engine. The review consolidated ", docs,
    " records spanning ", timespan,
    " and retained ", included,
    " studies after PRISMA-based reconciliation and source harmonization. ",
    "The bibliometric map identified ", clusters,
    if (clusters == 1) " thematic cluster. " else " thematic clusters. ",
    "M2 evaluates temporal growth with ", tolower(model),
    " as the interpretable headline model and ", forecast,
    " as the short-horizon forecasting comparator. ",
    leading_producer, " leads publication volume and ", leading_citer,
    " leads citation impact. The contribution is methodological and substantive: the paper separates auditable automated evidence from the human-coded synthesis required to move from trends and clusters to a defensible research agenda, taxonomy, conceptual framework, or metatheory."
  )
  bslr_ensure_sentence(abstract)
}

bslr_build_core_references <- function(format = c("markdown", "latex")) {
  format <- match.arg(format)
  refs <- c(
    "Marzi, G., Balzano, M., Caputo, A., and Pellegrini, M. M. (2025). Guidelines for Bibliometric-Systematic Literature Reviews: 10 steps to combine analysis, synthesis and theory development. International Journal of Management Reviews, 27(1), 81-103. https://doi.org/10.1111/ijmr.12381",
    "Page, M. J., McKenzie, J. E., Bossuyt, P. M., Boutron, I., Hoffmann, T. C., Mulrow, C. D., et al. (2021). The PRISMA 2020 statement: an updated guideline for reporting systematic reviews. BMJ, 372, n71. https://doi.org/10.1136/bmj.n71",
    "Aria, M., and Cuccurullo, C. (2017). bibliometrix: An R-tool for comprehensive science mapping analysis. Journal of Informetrics, 11(4), 959-975. https://doi.org/10.1016/j.joi.2017.08.007",
    "Donthu, N., Kumar, S., Mukherjee, D., Pandey, N., and Lim, W. M. (2021). How to conduct a bibliometric analysis: An overview and guidelines. Journal of Business Research, 133, 285-296. https://doi.org/10.1016/j.jbusres.2021.04.070",
    "Zupic, I., and Cater, T. (2015). Bibliometric methods in management and organization. Organizational Research Methods, 18(3), 429-472. https://doi.org/10.1177/1094428114562629"
  )
  if (identical(format, "markdown")) {
    return(paste0("- ", refs))
  }
  items <- paste0("\\bibitem{ref", seq_along(refs), "} ", m0_escape_latex(refs))
  c("\\begin{thebibliography}{5}", items, "\\end{thebibliography}")
}

bslr_build_manuscript_qmd <- function(protocol,
                                      module_results,
                                      checkpoints,
                                      bibliometric_map,
                                      journal_assessment,
                                      scaffolds,
                                      exhibits,
                                      template_resources,
                                      config) {
  title <- protocol$title %||% "B-SLR Study"
  readiness <- journal_assessment$summary %||% list()
  methods_paragraphs <- bslr_build_methods_paragraphs(protocol, module_results, bibliometric_map, journal_assessment)
  intro_paragraphs <- bslr_build_intro_paragraphs(protocol, module_results, bibliometric_map)
  abstract_paragraph <- bslr_build_abstract_paragraph(protocol, module_results, bibliometric_map)
  m1_paragraphs <- bslr_build_m1_paragraphs(module_results$m1 %||% list())
  m2_paragraphs <- bslr_build_m2_paragraphs(module_results$m2 %||% list())
  m3_paragraphs <- bslr_build_m3_paragraphs(module_results$m3 %||% list())
  cluster_paragraphs <- bslr_build_cluster_paragraphs(bibliometric_map, protocol)
  discussion_paragraphs <- bslr_build_discussion_paragraphs(protocol, journal_assessment)
  manuscript_outline <- scaffolds$manuscript_outline %||% data.frame()
  sample_selected <- bibliometric_map$sample_selected %||% data.frame()
  figure_blocks <- bslr_build_figure_blocks(exhibits$figures %||% data.frame())
  table_blocks <- bslr_build_table_blocks(exhibits$tables %||% data.frame())
  methods_design <- bslr_take_paragraphs(methods_paragraphs, 1L, 2L)
  methods_protocol <- bslr_take_paragraphs(methods_paragraphs, 3L, 4L)
  methods_mapping <- bslr_take_paragraphs(methods_paragraphs, 5L, 5L)
  methods_synthesis <- bslr_take_paragraphs(methods_paragraphs, 6L, 6L)
  include_header <- if (nzchar(template_resources$relative_dir %||% "") && file.exists(template_resources$header_path %||% "")) {
    c(
      "    include-in-header:",
      paste0("      - ", template_resources$relative_dir, "/ieee_header.tex")
    )
  } else {
    character()
  }

  yaml <- c(
    "---",
    sprintf("title: \"%s\"", gsub("\"", "'", title)),
    "format:",
    "  html:",
    "    toc: true",
    "    number-sections: true",
    "    embed-resources: false",
    "  pdf:",
    "    documentclass: IEEEtran",
    "    classoption: conference",
    "    keep-tex: true",
    "    fig-pos: H",
    "    number-sections: true",
    include_header,
    "execute:",
    "  echo: false",
    "  warning: false",
    "  message: false",
    "---",
    ""
  )

  lines <- c(
    yaml,
    "# Abstract",
    "",
    abstract_paragraph,
    "",
    "# Introduction",
    "",
    bslr_markdown_paragraphs(intro_paragraphs),
    "# Protocol-Driven B-SLR Method",
    "",
    "## Steps 1-3: Question, Query, and Sources",
    "",
    if (length(methods_design) > 0) bslr_markdown_paragraphs(methods_design) else bslr_markdown_paragraphs("The review design paragraph is unavailable and should be completed from the validated B-SLR protocol."),
    "## Steps 4-5: Screening and Data Consolidation",
    "",
    if (length(methods_protocol) > 0) bslr_markdown_paragraphs(methods_protocol) else bslr_markdown_paragraphs("The protocol-and-screening paragraph is unavailable and should be completed from the validated PRISMA and screening records."),
    "## Steps 6-8: Bibliometric Mapping and Sample Ordering",
    "",
    if (length(methods_mapping) > 0) bslr_markdown_paragraphs(methods_mapping) else bslr_markdown_paragraphs("The bibliometric-to-systematic bridge is unavailable and should be completed from the validated B-SLR map."),
    "## Steps 9-10: Systematic Synthesis and Theorising",
    "",
    if (length(methods_synthesis) > 0) bslr_markdown_paragraphs(methods_synthesis) else bslr_markdown_paragraphs("The systematic synthesis and theorising paragraph is unavailable and should be completed from the human-coded B-SLR evidence matrix."),
    "# Bibliometric Evidence",
    "",
    "## Descriptive Structure",
    "",
    bslr_markdown_paragraphs(m1_paragraphs),
    "## Temporal Dynamics and Forecasting",
    "",
    bslr_markdown_paragraphs(m2_paragraphs),
    "## Geographical and Comparative Patterns",
    "",
    bslr_markdown_paragraphs(m3_paragraphs),
    "# Journal Figures",
    "",
    "The figures below are selected because they support the argument of the paper. Diagnostic exports and file-format inventories are kept in the appendix.",
    "",
    if (length(figure_blocks) > 0) figure_blocks else "- No journal-ready figure exhibits were assembled for the current run.",
    "# Methodological Audit Tables",
    "",
    "The following tables document the audit trail, cluster logic, and readiness checks that support the review protocol.",
    "",
    if (length(table_blocks) > 0) table_blocks else "- No journal-ready table exhibits were assembled for the current run.",
    "# Systematic Synthesis",
    "",
    paste0("Representative documents currently selected for coding: **", if (is.data.frame(sample_selected)) nrow(sample_selected) else 0, "**."),
    "",
    bslr_markdown_paragraphs(cluster_paragraphs),
    "",
    "The coding templates and detailed evidence matrices are retained in the appendix. The main manuscript keeps only the interpretive synthesis so that the paper reads as a connected article rather than as an assembly of workflow fragments.",
    "",
    "# Theory Development and Discussion",
    "",
    bslr_markdown_paragraphs(discussion_paragraphs[1]),
    "# Theoretical Contribution",
    "",
    paste0(
      "The theorising stage should translate the coded evidence into ",
      bslr_join_human_terms(protocol$theorising$modes %||% character()),
      ". Each theoretical move should remain traceable to the selected cluster evidence rather than to high-level bibliometric trends alone."
    ),
    "",
    if (!is.null(protocol$theorising$contribution_goal) && nzchar(protocol$theorising$contribution_goal)) {
      bslr_ensure_sentence(paste0("The current contribution goal is: ", protocol$theorising$contribution_goal))
    } else {
      NULL
    },
    "# Limitations and Remaining Human Actions",
    "",
    bslr_markdown_paragraphs(discussion_paragraphs[2]),
    "# References",
    "",
    bslr_build_core_references("markdown")
  )

  lines
}

bslr_build_appendix_qmd <- function(protocol,
                                    module_results,
                                    checkpoints,
                                    bibliometric_map,
                                    journal_assessment,
                                    scaffolds,
                                    exhibits,
                                    template_resources,
                                    config) {
  title <- protocol$title %||% "B-SLR Study"
  checkpoint_summary <- checkpoints$summary %||% data.frame()
  gates <- journal_assessment$gates %||% data.frame()
  cluster_summary <- bibliometric_map$cluster_summary %||% data.frame()
  extraction_template <- scaffolds$extraction_template %||% data.frame()
  evidence_matrix <- scaffolds$evidence_matrix %||% data.frame()
  exhibit_inventory <- exhibits$inventory %||% data.frame()
  quality_gate <- journal_assessment$quality_gate %||% list()
  claim_ledger <- journal_assessment$claim_ledger %||% data.frame()
  reproducibility <- journal_assessment$reproducibility %||% list()
  include_header <- if (nzchar(template_resources$relative_dir %||% "") && file.exists(template_resources$header_path %||% "")) {
    c(
      "    include-in-header:",
      paste0("      - ", template_resources$relative_dir, "/ieee_header.tex")
    )
  } else {
    character()
  }

  yaml <- c(
    "---",
    sprintf("title: \"%s - Appendix\"", gsub("\"", "'", title)),
    "format:",
    "  html:",
    "    toc: true",
    "    number-sections: true",
    "  pdf:",
    "    documentclass: IEEEtran",
    "    classoption: conference",
    "    keep-tex: true",
    "    number-sections: true",
    include_header,
    "execute:",
    "  echo: false",
    "  warning: false",
    "  message: false",
    "---",
    ""
  )

  lines <- c(
    yaml,
    "This appendix consolidates the methodological checkpoints, readiness gates, coding assets, and exhibit inventory that support the main manuscript. It is intended to document the audit trail behind the automated bundle rather than to replace the narrative synthesis written in the article body.",
    "",
    "# Appendix A. Checkpoints",
    "",
    if (is.data.frame(checkpoint_summary) && nrow(checkpoint_summary) > 0) {
      unlist(lapply(seq_len(nrow(checkpoint_summary)), function(i) {
        row <- checkpoint_summary[i, , drop = FALSE]
        paste0("- `", row$checkpoint, "`: ", row$completed, "/", row$total, " complete (", row$state, ").")
      }), use.names = FALSE)
    } else {
      "- Checkpoint summary unavailable."
    },
    "",
    "# Appendix B. Journal-Grade Gates",
    "",
    if (is.data.frame(gates) && nrow(gates) > 0) {
      unlist(lapply(seq_len(nrow(gates)), function(i) {
        row <- gates[i, , drop = FALSE]
        c(
          paste0("## ", row$gate),
          paste0("Status: ", if (isTRUE(row$status)) "PASS" else "PENDING"),
          paste0("Evidence: ", row$evidence),
          paste0("Action: ", row$action),
          ""
        )
      }), use.names = FALSE)
    } else {
      "- Gate assessment unavailable."
    },
    "# Appendix C. Bibliometric Map",
    "",
    if (is.data.frame(cluster_summary) && nrow(cluster_summary) > 0) {
      unlist(lapply(seq_len(nrow(cluster_summary)), function(i) {
        row <- cluster_summary[i, , drop = FALSE]
        paste0("- Cluster ", row$cluster_id, ": ", row$cluster_label, " (", row$article_count, " docs, mean citations ", round(row$mean_citations %||% NA_real_, 2), ").")
      }), use.names = FALSE)
    } else {
      "- Cluster summary unavailable."
    },
    "",
    "# Appendix D. Coding Assets",
    "",
    paste0("- Extraction-template rows: ", if (is.data.frame(extraction_template)) nrow(extraction_template) else 0),
    paste0("- Evidence-matrix rows: ", if (is.data.frame(evidence_matrix)) nrow(evidence_matrix) else 0),
    "",
    "# Appendix E. Exhibit Inventory",
    "",
    if (is.data.frame(exhibit_inventory) && nrow(exhibit_inventory) > 0) {
      unlist(lapply(seq_len(nrow(exhibit_inventory)), function(i) {
        row <- exhibit_inventory[i, , drop = FALSE]
        paste0("- `", row$exhibit_id, "` (", row$type, ", ", row$module_id, "): ", row$caption, " -> `", row$relative_path, "`")
      }), use.names = FALSE)
    } else {
      "- Exhibit inventory unavailable."
    },
    "",
    "# Appendix F. Claim Ledger",
    "",
    if (is.data.frame(claim_ledger) && nrow(claim_ledger) > 0) {
      unlist(lapply(seq_len(nrow(claim_ledger)), function(i) {
        row <- claim_ledger[i, , drop = FALSE]
        paste0("- `", row$claim_id, "` [", row$strength, "]: ", row$claim, " Evidence: ", row$evidence_summary, " Limitation: ", row$limitation)
      }), use.names = FALSE)
    } else {
      "- Claim ledger unavailable."
    },
    "",
    "# Appendix G. Quality Gate and Reproducibility Capsule",
    "",
    paste0("- Quality-gate recommendation: ", quality_gate$recommendation %||% "not available"),
    if (length(quality_gate$blocking_issues %||% character()) > 0) {
      paste0("- Blocking issue: ", quality_gate$blocking_issues)
    } else {
      "- No blocking issue recorded by the automated quality gate."
    },
    paste0("- Reproducibility status: ", reproducibility$status %||% "not available"),
    paste0("- Session-info lines captured: ", length(reproducibility$session_info %||% character()))
  )

  lines
}

bslr_build_ieee_author_block <- function(protocol) {
  manuscript_authors <- protocol$manuscript$authors %||% character()
  manuscript_authors <- trimws(as.character(manuscript_authors))
  manuscript_authors <- manuscript_authors[nzchar(manuscript_authors)]
  if (length(manuscript_authors) == 0) {
    return("\\IEEEauthorblockN{Author Name(s) Here}\n\\IEEEauthorblockA{Affiliation and contact details}")
  }

  paste0(
    "\\IEEEauthorblockN{",
    m0_escape_latex(paste(manuscript_authors, collapse = " \\and ")),
    "}\n\\IEEEauthorblockA{Affiliation and contact details}"
  )
}

bslr_build_ieee_keywords <- function(protocol, module_results) {
  configured <- protocol$search$keywords %||% protocol$keywords %||% character()
  configured <- trimws(as.character(configured))
  configured <- configured[nzchar(configured)]
  if (length(configured) >= 3) {
    return(m0_escape_latex(paste(utils::head(unique(configured), 5), collapse = ", ")))
  }

  lines <- bslr_compact_module_report(module_results$m1 %||% list())
  keywords <- c(
    bslr_extract_report_value(lines, "^1 \\.\\s*(.+?)\\s*-\\s*([0-9]+)$", group = 1L),
    bslr_extract_report_value(lines, "^2 \\.\\s*(.+?)\\s*-\\s*([0-9]+)$", group = 1L),
    bslr_extract_report_value(lines, "^3 \\.\\s*(.+?)\\s*-\\s*([0-9]+)$", group = 1L)
  )
  keywords <- unique(vapply(keywords, bslr_text_value, character(1), fallback = ""))
  keywords <- keywords[nzchar(keywords)]
  if (length(keywords) == 0) {
    keywords <- c("bibliometric analysis", "systematic literature review", "research synthesis")
  }
  m0_escape_latex(paste(keywords, collapse = ", "))
}

bslr_latex_label_id <- function(x) {
  x <- gsub("[^A-Za-z0-9:_-]+", "_", as.character(x %||% "artifact"))
  x <- gsub("_+", "_", x)
  trimws(x, whitespace = "_")
}

bslr_latex_graphic_path <- function(path) {
  path <- gsub("\\\\", "/", as.character(path %||% ""))
  paste0("\\detokenize{", path, "}")
}

bslr_build_ieee_exhibit_blocks <- function(exhibits) {
  inventory <- exhibits$inventory %||% data.frame()
  if (!is.data.frame(inventory) || nrow(inventory) == 0) {
    return("\\section{Core Exhibits}\nNo figure or table exhibits were assembled for the current bundle.")
  }

  blocks <- unlist(lapply(seq_len(nrow(inventory)), function(i) {
    row <- inventory[i, , drop = FALSE]
    if (identical(row$type, "figure")) {
      layout <- bslr_text_value(row$layout %||% "single", "single")
      env <- if (identical(layout, "full")) "figure*" else "figure"
      width <- if (identical(layout, "full")) "\\textwidth" else "\\columnwidth"
      fig_path <- bslr_text_value(row$latex_relative_path %||% row$relative_path, row$relative_path)
      caption <- bslr_ensure_sentence(row$caption)
      interpretation <- bslr_text_value(row$interpretation %||% "", "")
      if (nzchar(interpretation)) {
        caption <- paste(caption, bslr_ensure_sentence(interpretation))
      }
      c(
        paste0("\\begin{", env, "}[!t]"),
        "\\centering",
        paste0("\\includegraphics[width=", width, "]{", bslr_latex_graphic_path(fig_path), "}"),
        paste0("\\caption{", m0_escape_latex(caption), "}"),
        paste0("\\label{fig:", bslr_latex_label_id(row$exhibit_id), "}"),
        paste0("\\end{", env, "}")
      )
    } else {
      c(
        "\\begin{table}[!t]",
        "\\centering",
        paste0("\\caption{", m0_escape_latex(row$caption), "}"),
        paste0("\\label{tbl:", bslr_latex_label_id(row$exhibit_id), "}"),
        paste0("\\small Artifact attached as ", m0_escape_latex(row$relative_path), "."),
        "\\end{table}"
      )
    }
  }), use.names = FALSE)

  paste(blocks, collapse = "\n")
}

bslr_render_latex_template <- function(template_lines, replacements) {
  text <- paste(template_lines, collapse = "\n")
  for (name in names(replacements)) {
    placeholder <- paste0("<<", name, ">>")
    parts <- strsplit(text, placeholder, fixed = TRUE)[[1]]
    text <- paste(parts, collapse = replacements[[name]])
  }
  unlist(strsplit(text, "\n", fixed = TRUE), use.names = FALSE)
}

bslr_build_ieee_tex <- function(protocol,
                                module_results,
                                checkpoints,
                                bibliometric_map,
                                journal_assessment,
                                scaffolds,
                                exhibits,
                                template_resources) {
  template_path <- template_resources$latex_template_path %||% ""
  if (nzchar(template_path) && file.exists(template_path)) {
    template_lines <- readLines(template_path, warn = FALSE, encoding = "UTF-8")
  } else {
    template_lines <- c(
      "\\documentclass[conference]{IEEEtran}",
      "\\begin{document}",
      "\\title{<<TITLE>>}",
      "\\author{<<AUTHOR_BLOCK>>}",
      "\\maketitle",
      "\\begin{abstract}",
      "<<ABSTRACT>>",
      "\\end{abstract}",
      "\\section{Introduction}",
      "<<INTRODUCTION>>",
      "\\section{Protocol-Driven B-SLR Method}",
      "<<METHODS>>",
      "\\section{Bibliometric Evidence}",
      "<<RESULTS>>",
      "<<EXHIBITS>>",
      "\\section{Systematic Synthesis}",
      "<<CLUSTER_SYNTHESIS>>",
      "\\section{Theory Development and Discussion}",
      "<<DISCUSSION>>",
      "\\section{Limitations and Audit Trail}",
      "<<LIMITATIONS>>",
      "<<REFERENCES>>",
      "\\end{document}"
    )
  }

  abstract_paragraph <- bslr_build_abstract_paragraph(protocol, module_results, bibliometric_map)
  intro_paragraphs <- bslr_build_intro_paragraphs(protocol, module_results, bibliometric_map)
  methods_paragraphs <- bslr_build_methods_paragraphs(protocol, module_results, bibliometric_map, journal_assessment)
  results_paragraphs <- c(
    bslr_build_m1_paragraphs(module_results$m1 %||% list()),
    bslr_build_m2_paragraphs(module_results$m2 %||% list()),
    bslr_build_m3_paragraphs(module_results$m3 %||% list())
  )
  cluster_paragraphs <- bslr_build_cluster_paragraphs(bibliometric_map, protocol)
  discussion_paragraphs <- bslr_build_discussion_paragraphs(protocol, journal_assessment)
  pending <- journal_assessment$pending_actions %||% character()
  limitations_text <- if (length(pending) > 0) {
    paste0("The remaining human actions are as follows: ", paste(m0_escape_latex(pending), collapse = "; "), ".")
  } else {
    "No blocking methodological actions remain in the current bundle, so the remaining effort should focus on article-level coding, synthesis writing, and theory development."
  }

  replacements <- c(
    TITLE = m0_escape_latex(protocol$title %||% "B-SLR Study"),
    AUTHOR_BLOCK = bslr_build_ieee_author_block(protocol),
    ABSTRACT = bslr_tex_paragraphs(abstract_paragraph),
    KEYWORDS = bslr_build_ieee_keywords(protocol, module_results),
    INTRODUCTION = bslr_tex_paragraphs(intro_paragraphs),
    METHODS = bslr_tex_paragraphs(methods_paragraphs),
    RESULTS = bslr_tex_paragraphs(results_paragraphs),
    CLUSTER_SYNTHESIS = bslr_tex_paragraphs(cluster_paragraphs),
    DISCUSSION = bslr_tex_paragraphs(discussion_paragraphs[1]),
    LIMITATIONS = bslr_tex_paragraphs(c(discussion_paragraphs[2], limitations_text)),
    REFERENCES = paste(bslr_build_core_references("latex"), collapse = "\n"),
    EXHIBITS = bslr_build_ieee_exhibit_blocks(exhibits)
  )

  bslr_render_latex_template(template_lines, replacements)
}

bslr_paper_dimensions <- function(layout = "full", height = NULL) {
  layout <- if (identical(layout, "single")) "single" else "full"
  width <- if (identical(layout, "single")) 3.5 else 7.16
  if (is.null(height)) {
    height <- if (identical(layout, "single")) 2.55 else 3.75
  }
  list(width = width, height = height)
}

bslr_export_paper_plot <- function(plot,
                                   figures_dir,
                                   exhibit_id,
                                   module_id,
                                   caption,
                                   interpretation = "",
                                   layout = "full",
                                   height = NULL,
                                   dpi = 450) {
  if (is.null(plot)) {
    return(NULL)
  }
  dims <- bslr_paper_dimensions(layout = layout, height = height)
  base_path <- file.path(figures_dir, exhibit_id)
  paths <- tryCatch(
    export_plot_artifact(
      plot = plot,
      path = base_path,
      width = dims$width,
      height = dims$height,
      dpi = dpi
    ),
    error = function(e) character()
  )
  bslr_figure_row_from_paths(
    paths = paths,
    exhibit_id = exhibit_id,
    module_id = module_id,
    caption = caption,
    interpretation = interpretation,
    layout = layout,
    source = "paper_generated"
  )
}

bslr_figure_row_from_paths <- function(paths,
                                       exhibit_id,
                                       module_id,
                                       caption,
                                       interpretation = "",
                                       layout = "full",
                                       source = "copied") {
  paths <- as.character(paths %||% character())
  paths <- paths[!is.na(paths) & nzchar(paths) & file.exists(paths)]
  if (length(paths) == 0) {
    return(NULL)
  }

  names(paths) <- tolower(names(paths) %||% tools::file_ext(paths))
  get_path <- function(ext) {
    hit <- paths[names(paths) == ext]
    if (length(hit) == 0) {
      hit <- paths[tolower(tools::file_ext(paths)) == ext]
    }
    if (length(hit) == 0) {
      return(NA_character_)
    }
    hit[1]
  }

  png_path <- get_path("png")
  svg_path <- get_path("svg")
  pdf_path <- get_path("pdf")
  eps_path <- get_path("eps")
  primary <- dplyr::coalesce(png_path, svg_path, pdf_path, eps_path)
  latex_primary <- dplyr::coalesce(pdf_path, eps_path, png_path, svg_path)

  rel <- function(path) {
    if (is.na(path) || !nzchar(path)) {
      return(NA_character_)
    }
    file.path("figures", basename(path))
  }

  data.frame(
    exhibit_id = exhibit_id,
    type = "figure",
    module_id = module_id,
    caption = caption,
    interpretation = interpretation,
    relative_path = rel(primary),
    latex_relative_path = rel(latex_primary),
    absolute_path = primary,
    png_path = png_path,
    svg_path = svg_path,
    pdf_path = pdf_path,
    eps_path = eps_path,
    all_paths = paste(unique(paths), collapse = ";"),
    formats = paste(sort(unique(tolower(tools::file_ext(paths)))), collapse = ","),
    layout = layout,
    source = source,
    stringsAsFactors = FALSE
  )
}

bslr_copy_figure_family <- function(src, figures_dir, exhibit_id) {
  if (is.na(src) || !file.exists(src)) {
    return(character())
  }
  src_base <- tools::file_path_sans_ext(src)
  out <- character()
  for (ext in c("png", "svg", "pdf", "eps")) {
    candidate <- paste0(src_base, ".", ext)
    if (!file.exists(candidate)) {
      next
    }
    dest <- file.path(figures_dir, paste0(exhibit_id, ".", ext))
    if (isTRUE(file.copy(candidate, dest, overwrite = TRUE))) {
      out[ext] <- dest
    }
  }
  out
}

bslr_num <- function(x, default = NA_real_) {
  x <- as.character(x %||% "")
  x <- gsub(",", "", x, fixed = TRUE)
  x <- gsub("%", "", x, fixed = TRUE)
  x <- suppressWarnings(as.numeric(x[1]))
  if (length(x) == 0 || !is.finite(x)) default else x
}

bslr_get_module_plot <- function(module_result, section, plot_name) {
  plots <- module_result$artifacts$plots[[section]] %||% list()
  if (is.list(plots$plots) && !is.null(plots$plots[[plot_name]])) {
    return(plots$plots[[plot_name]])
  }
  if (!is.null(plots[[plot_name]])) {
    return(plots[[plot_name]])
  }
  NULL
}

bslr_wrap <- function(x, width = 70) {
  paste(strwrap(as.character(x %||% ""), width = width), collapse = "\n")
}

bslr_pick_col <- function(df, candidates) {
  candidates <- as.character(candidates)
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) > 0) {
    return(hit[1])
  }
  normalized <- function(x) gsub("[^a-z0-9]+", "", tolower(x))
  idx <- match(normalized(candidates), normalized(names(df)))
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0) {
    return(NA_character_)
  }
  names(df)[idx[1]]
}

bslr_stage_count <- function(stage_counts, stage, decision = NULL) {
  if (!is.data.frame(stage_counts) || nrow(stage_counts) == 0) {
    return(NA_real_)
  }
  stage_col <- bslr_pick_col(stage_counts, c("stage", "Stage"))
  n_col <- bslr_pick_col(stage_counts, c("n_documents", "n", "count", "Count"))
  decision_col <- bslr_pick_col(stage_counts, c("consensus_decision", "decision", "Decision"))
  if (is.na(stage_col) || is.na(n_col)) {
    return(NA_real_)
  }
  rows <- tolower(as.character(stage_counts[[stage_col]])) == tolower(stage)
  if (!is.null(decision) && !is.na(decision_col)) {
    rows <- rows & tolower(as.character(stage_counts[[decision_col]])) == tolower(decision)
  }
  val <- sum(suppressWarnings(as.numeric(stage_counts[[n_col]][rows])), na.rm = TRUE)
  if (!is.finite(val) || val == 0) NA_real_ else val
}

bslr_prisma_shorten_reason <- function(reason) {
  reason <- as.character(reason %||% "")
  reason <- gsub("Non-power-system context", "Non-power-system context", reason, fixed = TRUE)
  reason <- gsub("No explicit frequency-estimation focus", "No frequency-estimation focus", reason, fixed = TRUE)
  reason <- gsub("Insufficient methodological detail for synthesis", "Insufficient method detail", reason, fixed = TRUE)
  reason <- gsub("Peripheral frequency/control focus outside review perimeter", "Peripheral focus outside perimeter", reason, fixed = TRUE)
  reason <- gsub("Unsupported document type", "Unsupported document type", reason, fixed = TRUE)
  reason
}

bslr_prisma_reason_text <- function(exclusion_reasons, stage, top_n = 3L) {
  if (!is.data.frame(exclusion_reasons) || nrow(exclusion_reasons) == 0 ||
      !all(c("stage", "reason", "n_documents") %in% names(exclusion_reasons))) {
    return("Reasons not recorded")
  }
  rows <- exclusion_reasons[tolower(exclusion_reasons$stage) == tolower(stage), , drop = FALSE]
  if (nrow(rows) == 0) {
    return("Reasons not recorded")
  }
  rows$n_documents <- suppressWarnings(as.numeric(rows$n_documents))
  rows <- rows[order(rows$n_documents, decreasing = TRUE), , drop = FALSE]
  rows <- rows[seq_len(min(top_n, nrow(rows))), , drop = FALSE]
  paste(
    vapply(seq_len(nrow(rows)), function(i) {
      sprintf("%s: %s", bslr_prisma_shorten_reason(rows$reason[i]), scales::comma(rows$n_documents[i]))
    }, character(1)),
    collapse = "\n"
  )
}

bslr_prisma_count <- function(x) {
  if (!is.finite(x)) {
    return("NA")
  }
  scales::comma(round(x))
}

bslr_make_prisma_plot <- function(module_result) {
  stage_counts <- module_result$data$screening_summary$stage_counts %||% data.frame()
  exclusion_reasons <- module_result$data$screening_summary$exclusion_reasons %||% data.frame()
  prisma_lines <- bslr_clean_report_lines(module_result$artifacts$reports$prisma_methodology$lines %||% character())

  screened_in <- bslr_stage_count(stage_counts, "screening", "include")
  screened_out <- bslr_stage_count(stage_counts, "screening", "exclude")
  eligible_in <- bslr_stage_count(stage_counts, "eligibility", "include")
  eligible_out <- bslr_stage_count(stage_counts, "eligibility", "exclude")
  included <- bslr_stage_count(stage_counts, "included", "include")

  raw_records <- bslr_num(bslr_extract_report_value(prisma_lines, "^Raw records:\\s*([0-9,]+);"), default = NA_real_)
  unique_records <- bslr_num(
    bslr_extract_report_value(prisma_lines, "^Raw records:\\s*[0-9,]+; unique records after deduplication:\\s*([0-9,]+);"),
    default = NA_real_
  )
  if (!is.finite(unique_records)) {
    unique_records <- sum(c(screened_in, screened_out), na.rm = TRUE)
  }
  if (!is.finite(raw_records)) {
    raw_records <- unique_records
  }
  duplicates <- max(0, raw_records - unique_records)
  full_text <- if (is.finite(screened_in)) screened_in else sum(c(eligible_in, eligible_out), na.rm = TRUE)
  final_included <- dplyr::coalesce(included, eligible_in, NA_real_)

  boxes <- data.frame(
    id = c("identified", "deduplicated", "screened", "eligible", "included"),
    x = 0.38,
    y = c(0.90, 0.73, 0.56, 0.39, 0.22),
    label = c(
      sprintf("1. Identification\nRecords retrieved from selected databases\nn = %s", bslr_prisma_count(raw_records)),
      sprintf("2. Data consolidation\nCanonical merge after DOI/title-year deduplication\nn = %s", bslr_prisma_count(unique_records)),
      sprintf("3. Screening\nTitle and abstract review\nn = %s", bslr_prisma_count(unique_records)),
      sprintf("4. Eligibility\nFull-text review\nn = %s", bslr_prisma_count(full_text)),
      sprintf("5. Inclusion\nFinal corpus for bibliometric and systematic synthesis\nn = %s", bslr_prisma_count(final_included))
    ),
    stringsAsFactors = FALSE
  )
  exclusions <- data.frame(
    id = c("duplicates", "screening_exclusions", "eligibility_exclusions"),
    x = 0.76,
    y = c(0.73, 0.56, 0.39),
    label = c(
      sprintf("Removed before screening\nDuplicates\nn = %s", bslr_prisma_count(duplicates)),
      sprintf(
        "Excluded at screening\nn = %s\n%s",
        bslr_prisma_count(screened_out),
        bslr_prisma_reason_text(exclusion_reasons, "screening", top_n = 3L)
      ),
      sprintf(
        "Excluded at eligibility\nn = %s\n%s",
        bslr_prisma_count(eligible_out),
        bslr_prisma_reason_text(exclusion_reasons, "eligibility", top_n = 3L)
      )
    ),
    stringsAsFactors = FALSE
  )
  controls <- data.frame(
    x = 0.08,
    y = c(0.90, 0.56, 0.22),
    label = c(
      "B-SLR checkpoint 1\nquestion, boundaries,\ncriteria, search string",
      "B-SLR checkpoint 2\nscreening, clusters,\nrepresentative sample",
      "B-SLR checkpoint 3\nsystematic synthesis,\ntheory contribution"
    ),
    stringsAsFactors = FALSE
  )
  connectors <- data.frame(
    x = boxes$x[-nrow(boxes)],
    xend = boxes$x[-1],
    y = boxes$y[-nrow(boxes)] - 0.065,
    yend = boxes$y[-1] + 0.065
  )
  side_connectors <- data.frame(
    x = c(0.52, 0.52, 0.52),
    xend = c(0.63, 0.63, 0.63),
    y = exclusions$y,
    yend = exclusions$y
  )

  p <- ggplot2::ggplot() +
    ggplot2::annotate(
      "rect",
      xmin = 0.00,
      xmax = 0.18,
      ymin = 0.08,
      ymax = 0.98,
      fill = "#F4F7FB",
      color = "#C9D2DD",
      linewidth = 0.25
    ) +
    ggplot2::geom_label(
      data = controls,
      ggplot2::aes(x = x, y = y, label = label),
      size = 2.0,
      lineheight = 0.90,
      linewidth = 0.18,
      label.padding = grid::unit(0.18, "lines"),
      fill = "white",
      color = "#25364A"
    ) +
    ggplot2::geom_segment(
      data = connectors,
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      arrow = grid::arrow(length = grid::unit(0.10, "inches"), type = "closed"),
      linewidth = 0.42,
      color = "#2F2F2F"
    ) +
    ggplot2::geom_segment(
      data = side_connectors,
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      arrow = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed"),
      linewidth = 0.32,
      color = "#595959"
    ) +
    ggplot2::geom_label(
      data = boxes,
      ggplot2::aes(x = x, y = y, label = label),
      size = 2.35,
      lineheight = 0.95,
      linewidth = 0.30,
      label.r = grid::unit(0.10, "lines"),
      label.padding = grid::unit(0.26, "lines"),
      fill = "white",
      color = "#111111"
    ) +
    ggplot2::geom_label(
      data = exclusions,
      ggplot2::aes(x = x, y = y, label = label),
      size = 1.95,
      lineheight = 0.90,
      linewidth = 0.18,
      label.padding = grid::unit(0.20, "lines"),
      fill = "#F7F7F7",
      color = "#333333"
    ) +
    ggplot2::labs(
      title = "B-SLR and PRISMA Corpus Flow",
      subtitle = "Search, consolidation, screening, eligibility, and final inclusion are kept auditable from M0",
      caption = "If no reviewer-level ledger is supplied, the diagram reports counts-only evidence and does not infer inter-reviewer reliability."
    ) +
    ggplot2::coord_cartesian(xlim = c(-0.02, 1.02), ylim = c(0.08, 0.99), clip = "off") +
    ggplot2::theme_void(base_size = 9.0) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 10.2, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 7.4, color = "#333333", hjust = 0),
      plot.caption = ggplot2::element_text(size = 6.2, color = "#555555", hjust = 0),
      plot.margin = ggplot2::margin(8, 12, 8, 10)
    )

  ieee_mark_plot_layout(p, "full")
}

bslr_make_bslr_workflow_plot <- function(protocol, module_results, bibliometric_map, journal_assessment) {
  m1_lines <- bslr_compact_module_report(module_results$m1 %||% list())
  prisma_lines <- bslr_clean_report_lines(module_results$m0$artifacts$reports$prisma_methodology$lines %||% character())
  docs <- bslr_text_value(bslr_extract_report_value(m1_lines, "^Documents\\s*:\\s*(.+)$"), "NA")
  included <- bslr_text_value(bslr_extract_report_value(prisma_lines, "^The final synthesis included\\s+([0-9,]+)\\s+studies\\."), docs)
  clusters <- if (is.data.frame(bibliometric_map$cluster_summary)) nrow(bibliometric_map$cluster_summary) else 0L
  sample_selected <- if (is.data.frame(bibliometric_map$sample_selected)) nrow(bibliometric_map$sample_selected) else 0L
  readiness <- journal_assessment$summary %||% list()
  readiness_band <- gsub("_", " ", bslr_text_value(readiness$readiness_band, "not assessed"), fixed = TRUE)
  theorising_modes <- bslr_join_human_terms(protocol$theorising$modes %||% character())
  if (!nzchar(theorising_modes)) {
    theorising_modes <- "research agenda / taxonomy / framework"
  }

  steps <- data.frame(
    step = 1:10,
    x = c(1, 2, 3, 4, 5, 1.45, 2.95, 4.45, 2.2, 3.8),
    y = c(rep(3, 5), rep(2, 3), rep(1, 2)),
    phase = c(rep("Data consolidation", 5), rep("Bibliometric assessment", 3), rep("Synthesis and theory", 2)),
    label = c(
      "1\nQuestion\nboundaries",
      "2\nSearch\nquery",
      "3\nDatabase\nselection",
      "4\nScreening\ncross-checks",
      paste0("5\nClean export\nn = ", included),
      "6\nBibliometric\napproach",
      paste0("7\nClusters\nk = ", clusters),
      paste0("8\nSample order\nn = ", sample_selected),
      "9\nSystematic\nreview",
      paste0("10\nTheory\n", bslr_wrap(theorising_modes, 14))
    ),
    stringsAsFactors = FALSE
  )
  connector_order <- steps[order(steps$step), , drop = FALSE]
  connectors <- data.frame(
    x = connector_order$x[-nrow(connector_order)] + 0.42,
    y = connector_order$y[-nrow(connector_order)],
    xend = connector_order$x[-1] - 0.42,
    yend = connector_order$y[-1],
    stringsAsFactors = FALSE
  )
  connectors <- connectors[abs(connectors$y - connectors$yend) < 1e-6, , drop = FALSE]
  checkpoints <- data.frame(
    x = c(5.75, 5.75, 5.75),
    y = c(3, 2, 1),
    label = c(
      paste0("Checkpoint 1\nData consolidated\n", docs, " records"),
      paste0("Checkpoint 2\nPreliminary results\n", readiness_band),
      "Checkpoint 3\nContribution assessed\nhuman synthesis required"
    ),
    stringsAsFactors = FALSE
  )
  palette <- c(
    "Data consolidation" = "#F4F7FB",
    "Bibliometric assessment" = "#F7F4ED",
    "Synthesis and theory" = "#F3F6F1"
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = connectors,
      ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
      arrow = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed"),
      color = "#3B3B3B",
      linewidth = 0.32
    ) +
    ggplot2::geom_tile(
      data = steps,
      ggplot2::aes(x = .data$x, y = .data$y, fill = .data$phase),
      width = 0.84,
      height = 0.50,
      color = "#343434",
      linewidth = 0.22
    ) +
    ggplot2::geom_text(
      data = steps,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = 2.15,
      lineheight = 0.88,
      color = "#111111"
    ) +
    ggplot2::geom_label(
      data = checkpoints,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = 2.05,
      lineheight = 0.88,
      linewidth = 0.22,
      label.padding = grid::unit(0.18, "lines"),
      fill = "white",
      color = "#26384D"
    ) +
    ggplot2::scale_fill_manual(values = palette, name = NULL) +
    ggplot2::labs(
      title = "B-SLR 10-Step Execution Logic",
      subtitle = "The manuscript separates data consolidation, bibliometric assessment, and contribution assessment rather than treating bibliometric maps as final theory",
      caption = "Adapted to RBiblioSynth from the B-SLR workflow: PRISMA accounts for record flow, while B-SLR checkpoints govern interpretation and theorising."
    ) +
    ggplot2::coord_cartesian(xlim = c(0.45, 6.35), ylim = c(0.58, 3.38), clip = "off") +
    ggplot2::theme_void(base_size = 8.0) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 6.4),
      plot.title = ggplot2::element_text(face = "bold", size = 10.0),
      plot.subtitle = ggplot2::element_text(size = 7.0, color = "#333333"),
      plot.caption = ggplot2::element_text(size = 5.8, color = "#555555", hjust = 0),
      plot.margin = ggplot2::margin(8, 18, 8, 10)
    )
  ieee_mark_plot_layout(p, "full")
}

bslr_make_evidence_bridge_plot <- function(module_results, bibliometric_map, journal_assessment, protocol) {
  m1_lines <- bslr_compact_module_report(module_results$m1 %||% list())
  m2_lines <- bslr_compact_module_report(module_results$m2 %||% list())
  m3_lines <- bslr_compact_module_report(module_results$m3 %||% list())
  docs <- bslr_text_value(bslr_extract_report_value(m1_lines, "^Documents\\s*:\\s*(.+)$"), "NA")
  timespan <- bslr_text_value(bslr_extract_report_value(m1_lines, "^Timespan\\s*:\\s*(.+)$"), "NA")
  model <- bslr_text_value(bslr_extract_report_value(m2_lines, "^Headline model:\\s*(.+)$"), "model pending")
  countries <- bslr_text_value(bslr_extract_report_value(m3_lines, "^Countries:\\s*(.+)$"), "countries pending")
  clusters <- if (is.data.frame(bibliometric_map$cluster_summary)) nrow(bibliometric_map$cluster_summary) else 0L
  sample_selected <- if (is.data.frame(bibliometric_map$sample_selected)) nrow(bibliometric_map$sample_selected) else 0L
  readiness <- round(100 * (journal_assessment$summary$readiness_score %||% 0), 1)
  modes <- bslr_join_human_terms(protocol$theorising$modes %||% character())
  if (!nzchar(modes)) modes <- "theory outputs"
  nodes <- data.frame(
    x = seq_len(6),
    y = 1,
    label = c(
      paste0("M0\nCorpus\n", docs, " docs"),
      paste0("M1\nStructure\n", timespan),
      paste0("M2\nTemporal\n", model),
      paste0("M3\nGeography\n", countries, " countries"),
      paste0("B-SLR\nClusters + sample\nk = ", clusters, "; n = ", sample_selected),
      paste0("Human synthesis\n", bslr_wrap(modes, 18))
    ),
    type = c(rep("Automated evidence", 4), "Bridge", "Human interpretation"),
    stringsAsFactors = FALSE
  )
  connectors <- data.frame(x = nodes$x[-nrow(nodes)] + 0.42, xend = nodes$x[-1] - 0.42, y = 1, yend = 1)
  palette <- c("Automated evidence" = "#EAF1F7", "Bridge" = "#FFF4D8", "Human interpretation" = "#EAF4EA")

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = connectors,
      ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
      arrow = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed"),
      linewidth = 0.35,
      color = "#3A3A3A"
    ) +
    ggplot2::geom_tile(
      data = nodes,
      ggplot2::aes(x = .data$x, y = .data$y, fill = .data$type),
      width = 0.82,
      height = 0.62,
      color = "#2F2F2F",
      linewidth = 0.22
    ) +
    ggplot2::geom_text(
      data = nodes,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = 2.1,
      lineheight = 0.88
    ) +
    ggplot2::annotate(
      "label",
      x = 3.5,
      y = 1.55,
      label = paste0("Readiness score: ", readiness, "%\nClaims remain bounded by protocol completeness and human coding"),
      size = 2.15,
      linewidth = 0.18,
      label.padding = grid::unit(0.18, "lines"),
      fill = "white",
      color = "#333333"
    ) +
    ggplot2::scale_fill_manual(values = palette, name = NULL) +
    ggplot2::labs(
      title = "Evidence Bridge from Bibliometrics to Systematic Review",
      subtitle = "Automated modules produce auditable evidence; the B-SLR layer turns it into a reading sample and theory-development scaffold",
      caption = "The last step is intentionally not automated: cluster interpretation and theoretical contribution require human reading and coding."
    ) +
    ggplot2::coord_cartesian(xlim = c(0.45, 6.55), ylim = c(0.55, 1.85), clip = "off") +
    ggplot2::theme_void(base_size = 8.0) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 6.4),
      plot.title = ggplot2::element_text(face = "bold", size = 10.0),
      plot.subtitle = ggplot2::element_text(size = 7.0, color = "#333333"),
      plot.caption = ggplot2::element_text(size = 5.8, color = "#555555", hjust = 0),
      plot.margin = ggplot2::margin(8, 18, 8, 10)
    )
  ieee_mark_plot_layout(p, "full")
}

bslr_make_cluster_strategy_plot <- function(bibliometric_map) {
  df <- bibliometric_map$cluster_summary %||% data.frame()
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(NULL)
  }
  label_col <- bslr_pick_col(df, c("cluster_label", "label", "Cluster"))
  count_col <- bslr_pick_col(df, c("article_count", "documents", "n", "count"))
  cite_col <- bslr_pick_col(df, c("mean_citations", "avg_citations", "citations"))
  if (any(is.na(c(label_col, count_col)))) {
    return(NULL)
  }
  if (is.na(cite_col)) {
    df$mean_citations_plot <- NA_real_
    cite_col <- "mean_citations_plot"
  }
  plot_df <- data.frame(
    cluster = bslr_wrap(df[[label_col]], 34),
    article_count = suppressWarnings(as.numeric(df[[count_col]])),
    mean_citations = suppressWarnings(as.numeric(df[[cite_col]])),
    stringsAsFactors = FALSE
  )
  plot_df <- plot_df[is.finite(plot_df$article_count), , drop = FALSE]
  if (nrow(plot_df) == 0) {
    return(NULL)
  }
  plot_df <- plot_df[order(plot_df$article_count), , drop = FALSE]
  plot_df$cluster <- factor(plot_df$cluster, levels = plot_df$cluster)
  plot_df$cite_label <- ifelse(is.finite(plot_df$mean_citations), sprintf("mean citations %.1f", plot_df$mean_citations), "mean citations not reported")

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$cluster, y = .data$article_count)) +
    ggplot2::geom_col(width = 0.64, fill = "#1F5A85", color = "#111111", linewidth = 0.18) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(scales::comma(.data$article_count), " docs | ", .data$cite_label)),
      hjust = -0.04,
      size = 2.1,
      color = "#222222"
    ) +
    ggplot2::scale_y_continuous(
      name = "Documents assigned to cluster",
      labels = scales::label_number(big.mark = ","),
      expand = ggplot2::expansion(mult = c(0, 0.30))
    ) +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::labs(
      title = "Cluster-Guided Systematic-Review Sampling Strategy",
      subtitle = "Cluster size and citation profile define the reading frame for Step 8 sample ordering",
      caption = "The cluster map organizes the human review; it does not replace article-level coding."
    ) +
    ieee_theme_wide(base_size = 7.6) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(size = 5.8, color = "#555555", hjust = 0),
      plot.margin = ggplot2::margin(8, 34, 8, 10)
    )
  ieee_mark_plot_layout(p, "full")
}

bslr_make_m2_model_selection_plot <- function(module_result) {
  regression <- module_result$data$regression %||% list()
  growth_models <- module_result$data$growth_models %||% list()
  candidates <- list(
    regression$tables$core_ranking,
    regression$core_ranking,
    regression$ranking,
    regression$model_comparison,
    regression$comparison,
    growth_models$table
  )
  df <- NULL
  for (candidate in candidates) {
    if (is.data.frame(candidate) && nrow(candidate) > 0) {
      df <- candidate
      break
    }
  }
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(NULL)
  }
  model_col <- bslr_pick_col(df, c("Model", "model", "name"))
  family_col <- bslr_pick_col(df, c("Family", "family"))
  score_col <- bslr_pick_col(df, c("NarrativeScore", "CompositeScore", "score", "R_squared", "R2", "Adj_R2"))
  rank_col <- bslr_pick_col(df, c("NarrativeRank", "Rank", "rank"))
  aic_col <- bslr_pick_col(df, c("AIC", "aic"))
  if (is.na(model_col)) {
    return(NULL)
  }
  plot_df <- data.frame(
    model = as.character(df[[model_col]]),
    family = if (!is.na(family_col)) as.character(df[[family_col]]) else "",
    rank = if (!is.na(rank_col)) suppressWarnings(as.numeric(df[[rank_col]])) else NA_real_,
    score = if (!is.na(score_col)) suppressWarnings(as.numeric(df[[score_col]])) else NA_real_,
    aic = if (!is.na(aic_col)) suppressWarnings(as.numeric(df[[aic_col]])) else NA_real_,
    stringsAsFactors = FALSE
  )
  if (!any(is.finite(plot_df$score)) && any(is.finite(plot_df$aic))) {
    plot_df$score <- max(plot_df$aic, na.rm = TRUE) - plot_df$aic
  }
  plot_df <- plot_df[is.finite(plot_df$score) | is.finite(plot_df$rank), , drop = FALSE]
  if (nrow(plot_df) == 0) {
    return(NULL)
  }
  if (!any(is.finite(plot_df$rank))) {
    plot_df$rank <- rank(-plot_df$score, ties.method = "first")
  }
  plot_df$type <- ifelse(
    grepl("spline|loess|gam|benchmark", plot_df$model, ignore.case = TRUE) |
      grepl("flex", plot_df$family, ignore.case = TRUE),
    "Flexible benchmark",
    "Interpretable candidate"
  )
  plot_df <- plot_df[order(plot_df$rank, -plot_df$score), , drop = FALSE]
  plot_df <- utils::head(plot_df, 12)
  if (diff(range(plot_df$score, na.rm = TRUE)) > 0) {
    plot_df$score_scaled <- (plot_df$score - min(plot_df$score, na.rm = TRUE)) / diff(range(plot_df$score, na.rm = TRUE))
  } else {
    plot_df$score_scaled <- plot_df$score
  }
  plot_df$model <- factor(plot_df$model, levels = rev(plot_df$model))
  headline <- as.character(plot_df$model[which.min(plot_df$rank)])
  x_max <- max(plot_df$score_scaled, na.rm = TRUE)
  if (!is.finite(x_max) || x_max <= 0) {
    x_max <- 1
  }
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(y = .data$model, x = .data$score_scaled, color = .data$type)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$score_scaled, yend = .data$model), linewidth = 0.50) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0("#", .data$rank)),
      hjust = -0.55,
      size = 2.1,
      color = "#111111",
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = c("Interpretable candidate" = "#1F5A85", "Flexible benchmark" = "#B55A4F"), name = NULL) +
    ggplot2::scale_x_continuous(name = "Normalized model-selection score", limits = c(0, x_max * 1.14), labels = scales::label_number(accuracy = 0.01)) +
    ggplot2::scale_y_discrete(name = NULL) +
    ggplot2::labs(
      title = "M2 Model-Selection Evidence",
      subtitle = paste0("Headline model: ", headline, " | interpretable models are preferred when they remain close to the flexible benchmark"),
      caption = "Scores combine fit, parsimony, residual adequacy, and interpretability when available; raw tables are exported for audit."
    ) +
    ieee_theme_wide(base_size = 7.6) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(size = 5.8, color = "#555555", hjust = 0),
      plot.margin = ggplot2::margin(8, 20, 8, 10)
    )
  ieee_mark_plot_layout(p, "full")
}

bslr_make_m3_collaboration_premium_plot <- function(module_result) {
  premium <- module_result$data$advanced_journal$collaboration_premium$table %||% data.frame()
  if (!is.data.frame(premium) || nrow(premium) == 0 ||
      !all(c("mean_difference", "ci_low", "ci_high") %in% names(premium))) {
    return(bslr_get_module_plot(module_result, "advanced_journal", "collaboration_premium_forest"))
  }
  for (col in c("comparison", "ratio", "cohen_d", "p_value")) {
    if (!col %in% names(premium)) {
      premium[[col]] <- if (identical(col, "comparison")) "MCP vs SCP" else NA_real_
    }
  }
  df <- premium |>
    dplyr::mutate(
      comparison = gsub("_", " ", as.character(.data$comparison), fixed = TRUE),
      mean_difference = suppressWarnings(as.numeric(.data$mean_difference)),
      ci_low = suppressWarnings(as.numeric(.data$ci_low)),
      ci_high = suppressWarnings(as.numeric(.data$ci_high)),
      ratio = suppressWarnings(as.numeric(.data$ratio)),
      cohen_d = suppressWarnings(as.numeric(.data$cohen_d)),
      p_value = suppressWarnings(as.numeric(.data$p_value))
    ) |>
    dplyr::filter(is.finite(.data$mean_difference), is.finite(.data$ci_low), is.finite(.data$ci_high))
  if (nrow(df) == 0) {
    return(NULL)
  }
  p_text <- if (is.finite(df$p_value[1]) && df$p_value[1] < 0.001) {
    "<0.001"
  } else {
    paste0("= ", bslr_number_text(df$p_value[1], 4L))
  }
  subtitle <- sprintf(
    "Ratio %s | d %s | p %s",
    bslr_number_text(df$ratio[1], 2L),
    bslr_number_text(df$cohen_d[1], 2L),
    p_text
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(y = .data$comparison, x = .data$mean_difference)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "22", color = "#666666", linewidth = 0.42) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$ci_low, xmax = .data$ci_high), height = 0.18, linewidth = 0.74, color = "#1F5A85") +
    ggplot2::geom_point(size = 2.6, color = "#1F5A85") +
    ggplot2::scale_x_continuous(name = "Citation difference", labels = scales::label_number(accuracy = 0.1)) +
    ggplot2::scale_y_discrete(name = NULL) +
    ggplot2::labs(
      title = "M3 Collaboration Premium",
      subtitle = subtitle,
      caption = "Positive values favor MCP papers; interval is bootstrap-based when available."
    ) +
    ieee_theme_wide(base_size = 7.0) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(size = 5.2, color = "#555555", hjust = 0),
      plot.margin = ggplot2::margin(6, 8, 6, 8)
    )
  ieee_mark_plot_layout(p, "single")
}

bslr_make_m3_rank_mobility_plot <- function(module_result) {
  mobility <- module_result$data$advanced_journal$mobility$rank_windows %||%
    module_result$data$advanced_journal$mobility$rank_mobility %||%
    module_result$data$advanced_journal$mobility$table %||%
    module_result$data$advanced_journal$tables$rank_mobility %||%
    data.frame()
  if (!is.data.frame(mobility) || nrow(mobility) == 0 ||
      !all(c("country", "rank_first", "rank_last") %in% names(mobility))) {
    return(bslr_get_module_plot(module_result, "advanced_journal", "rank_mobility_bump"))
  }
  if (!"rank_change" %in% names(mobility)) {
    mobility$rank_change <- suppressWarnings(as.numeric(mobility$rank_first)) - suppressWarnings(as.numeric(mobility$rank_last))
  }
  df <- mobility |>
    dplyr::mutate(
      country = m3_title_case_country(.data$country),
      rank_first = suppressWarnings(as.numeric(.data$rank_first)),
      rank_last = suppressWarnings(as.numeric(.data$rank_last)),
      rank_change = suppressWarnings(as.numeric(.data$rank_change))
    ) |>
    dplyr::filter(is.finite(.data$rank_first), is.finite(.data$rank_last)) |>
    dplyr::arrange(.data$rank_last, dplyr::desc(abs(.data$rank_change))) |>
    utils::head(14)
  if (nrow(df) == 0) {
    return(NULL)
  }
  df$direction_label <- ifelse(df$rank_change >= 0, "Rank improved/stable", "Rank declined")
  df$country <- factor(df$country, levels = df$country[order(df$rank_change)])
  df$rank_label <- paste0("#", df$rank_first, " -> #", df$rank_last)
  x_span <- max(abs(df$rank_change), na.rm = TRUE)
  if (!is.finite(x_span) || x_span <= 0) {
    x_span <- 1
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$rank_change, y = .data$country, fill = .data$direction_label)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "22", color = "#666666", linewidth = 0.38) +
    ggplot2::geom_col(width = 0.64, color = "#111111", linewidth = 0.16) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$rank_label, hjust = ifelse(.data$rank_change >= 0, -0.08, 1.08)),
      size = 2.1,
      color = "#111111"
    ) +
    ggplot2::scale_fill_manual(values = c("Rank declined" = "#B55A4F", "Rank improved/stable" = "#4D7E49"), name = NULL) +
    ggplot2::scale_x_continuous(
      name = "Rank shift (early rank - late rank)",
      limits = c(min(df$rank_change, na.rm = TRUE) - 0.16 * x_span, max(df$rank_change, na.rm = TRUE) + 0.22 * x_span),
      labels = scales::label_number(accuracy = 1)
    ) +
    ggplot2::scale_y_discrete(name = NULL) +
    ggplot2::labs(
      title = "M3 Country Leadership Mobility",
      subtitle = "Positive values indicate an improved relative production rank in the late window",
      caption = "Labels report early-rank to late-rank movement; complete rank-mobility tables are exported."
    ) +
    ieee_theme_wide(base_size = 7.6) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(size = 5.8, color = "#555555", hjust = 0),
      plot.margin = ggplot2::margin(8, 30, 8, 10)
    )
  ieee_mark_plot_layout(p, "full")
}

bslr_make_m1_overview_plot <- function(module_results, bibliometric_map) {
  m1_lines <- bslr_compact_module_report(module_results$m1 %||% list())
  m3_lines <- bslr_compact_module_report(module_results$m3 %||% list())
  prisma_lines <- bslr_clean_report_lines(module_results$m0$artifacts$reports$prisma_methodology$lines %||% character())

  docs <- bslr_text_value(bslr_extract_report_value(m1_lines, "^Documents\\s*:\\s*(.+)$"), "NA")
  sources <- bslr_text_value(bslr_extract_report_value(m1_lines, "^Sources\\s*:\\s*(.+)$"), "NA")
  authors <- bslr_text_value(bslr_extract_report_value(m1_lines, "^Authors\\s*:\\s*(.+)$"), "NA")
  timespan <- bslr_text_value(bslr_extract_report_value(m1_lines, "^Timespan\\s*:\\s*(.+)$"), "NA")
  countries <- bslr_text_value(bslr_extract_report_value(m3_lines, "^Countries:\\s*(.+)$"), "NA")
  included <- bslr_text_value(bslr_extract_report_value(prisma_lines, "^The final synthesis included\\s+([0-9,]+)\\s+studies\\."), docs)
  clusters <- if (is.data.frame(bibliometric_map$cluster_summary)) nrow(bibliometric_map$cluster_summary) else 0L
  top_country <- bslr_text_value(bslr_extract_report_value(m3_lines, "^Leading producer\\s*:\\s*(.+)$"), "leading country unavailable")
  top_model <- bslr_text_value(bslr_extract_report_value(bslr_compact_module_report(module_results$m2 %||% list()), "^Headline model:\\s*(.+)$"), "growth model unavailable")

  cards <- data.frame(
    metric = c("Final corpus", "Sources", "Authors", "Countries", "Timespan", "Thematic clusters"),
    value = c(included, sources, authors, countries, timespan, as.character(clusters)),
    col = rep(1:3, 2),
    row = rep(2:1, each = 3),
    stringsAsFactors = FALSE
  )

  p <- ggplot2::ggplot(cards, ggplot2::aes(x = col, y = row)) +
    ggplot2::geom_tile(width = 0.92, height = 0.72, fill = "#F7F8FA", color = "#5E6470", linewidth = 0.28) +
    ggplot2::geom_text(ggplot2::aes(label = value), vjust = 0.25, size = 4.0, fontface = "bold", color = "#111111") +
    ggplot2::geom_text(ggplot2::aes(label = metric), vjust = 2.4, size = 2.55, color = "#444444") +
    ggplot2::labs(
      title = "Corpus and review audit overview",
      subtitle = paste0("Leading producer: ", top_country, " | Headline temporal model: ", top_model),
      caption = "Values are extracted from M0-M3 module reports and B-SLR mapping outputs."
    ) +
    ggplot2::coord_cartesian(xlim = c(0.45, 3.55), ylim = c(0.52, 2.48), clip = "off") +
    ggplot2::theme_void(base_size = 8.2) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 9.6),
      plot.subtitle = ggplot2::element_text(size = 7.1, color = "#333333"),
      plot.caption = ggplot2::element_text(size = 6.4, color = "#555555"),
      plot.margin = ggplot2::margin(8, 10, 8, 10)
    )

  ieee_mark_plot_layout(p, "full")
}

bslr_make_m2_growth_plot_advanced <- function(module_result) {
  advanced <- module_result$data$advanced_journal %||% list()
  regimes <- advanced$growth_regimes %||% list()
  curve <- regimes$curve %||% data.frame()
  summary <- regimes$summary %||% data.frame()
  if (!is.data.frame(curve) || nrow(curve) < 2 ||
      !all(c("year", "cumulative", "fitted_cumulative") %in% names(curve))) {
    return(NULL)
  }
  curve <- curve |>
    dplyr::mutate(
      year = suppressWarnings(as.numeric(.data$year)),
      cumulative = suppressWarnings(as.numeric(.data$cumulative)),
      fitted_cumulative = suppressWarnings(as.numeric(.data$fitted_cumulative))
    ) |>
    dplyr::filter(is.finite(.data$year), is.finite(.data$cumulative), is.finite(.data$fitted_cumulative))
  if (nrow(curve) < 2) {
    return(NULL)
  }

  capacity <- if (is.data.frame(summary) && nrow(summary) > 0) suppressWarnings(as.numeric(summary$capacity[1])) else NA_real_
  inflection <- if (is.data.frame(summary) && nrow(summary) > 0) suppressWarnings(as.numeric(summary$inflection_year[1])) else NA_real_
  peak_growth <- if (is.data.frame(summary) && nrow(summary) > 0) suppressWarnings(as.numeric(summary$peak_growth_year[1])) else NA_real_
  distance <- if (is.data.frame(summary) && nrow(summary) > 0) suppressWarnings(as.numeric(summary$distance_to_saturation[1])) else NA_real_
  model <- if (is.data.frame(summary) && nrow(summary) > 0) bslr_report_value(summary$headline_model[1], "headline model") else "headline model"
  y_range <- range(c(curve$cumulative, curve$fitted_cumulative, capacity), na.rm = TRUE)
  x_range <- range(curve$year, na.rm = TRUE)
  label <- paste(
    c(
      paste0("Model: ", model),
      if (is.finite(capacity)) paste0("K = ", bslr_number_text(capacity, 1L)) else "K not finite",
      if (is.finite(inflection)) paste0("Inflection = ", bslr_number_text(inflection, 1L)) else NULL,
      if (is.finite(peak_growth)) paste0("Peak growth = ", bslr_number_text(peak_growth, 1L)) else NULL,
      if (is.finite(distance)) paste0("Distance to K = ", bslr_percent_text(distance)) else NULL
    ),
    collapse = "\n"
  )

  p <- ggplot2::ggplot(curve, ggplot2::aes(x = .data$year)) +
    ggplot2::geom_point(ggplot2::aes(y = .data$cumulative), size = 1.5, color = "#111111", alpha = 0.88) +
    ggplot2::geom_line(ggplot2::aes(y = .data$fitted_cumulative), linewidth = 0.90, color = "#1F5A85", lineend = "round") +
    ggplot2::annotate(
      "label",
      x = x_range[1] + 0.03 * diff(x_range),
      y = y_range[2] - 0.08 * diff(y_range),
      label = label,
      hjust = 0,
      vjust = 1,
      size = 2.35,
      lineheight = 0.90,
      linewidth = 0.18,
      label.padding = grid::unit(0.16, "lines"),
      fill = grDevices::adjustcolor("white", alpha.f = 0.94)
    ) +
    ggplot2::scale_x_continuous(name = "Year", breaks = scales::breaks_pretty(n = 8)) +
    ggplot2::scale_y_continuous(name = "Cumulative publications", labels = scales::label_number(big.mark = ",")) +
    ggplot2::labs(
      title = "M2 Growth-Regime Evidence",
      subtitle = "Observed cumulative production against the fitted interpretable saturation curve",
      caption = "Vertical guides mark the estimated inflection and peak-growth years when available."
    ) +
    ieee_theme_wide(base_size = 7.8) +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 5.9, color = "#555555", hjust = 0))

  if (is.finite(capacity)) {
    p <- p + ggplot2::geom_hline(yintercept = capacity, color = "#4D7E49", linetype = "22", linewidth = 0.45)
  }
  if (is.finite(inflection)) {
    p <- p + ggplot2::geom_vline(xintercept = inflection, color = "#1F5A85", linetype = "42", linewidth = 0.48)
  }
  if (is.finite(peak_growth) && (!is.finite(inflection) || abs(peak_growth - inflection) > 0.2)) {
    p <- p + ggplot2::geom_vline(xintercept = peak_growth, color = "#B55A4F", linetype = "33", linewidth = 0.48)
  }

  ieee_mark_plot_layout(p, "full")
}

bslr_make_m2_growth_plot <- function(module_result) {
  advanced_plot <- bslr_make_m2_growth_plot_advanced(module_result)
  if (!is.null(advanced_plot)) {
    return(advanced_plot)
  }
  regression <- module_result$data$regression %||% list()
  eda <- module_result$data$eda %||% list()
  raw_df <- regression$data %||% eda$annual_production %||% data.frame()
  if (!is.data.frame(raw_df) || nrow(raw_df) < 2 || !all(c("Year", "Articles") %in% names(raw_df))) {
    return(NULL)
  }
  best <- regression$best_model %||% list()
  curve <- best$curve$dense %||% best$curve$observed %||% data.frame()
  if (!is.data.frame(curve) || nrow(curve) == 0 || !"Fitted" %in% names(curve)) {
    curve <- data.frame(
      Year = best$years %||% raw_df$Year,
      Fitted = best$predictions %||% rep(NA_real_, nrow(raw_df)),
      Lower = best$predictions %||% rep(NA_real_, nrow(raw_df)),
      Upper = best$predictions %||% rep(NA_real_, nrow(raw_df))
    )
  }
  if (!"Lower" %in% names(curve)) curve$Lower <- curve$Fitted
  if (!"Upper" %in% names(curve)) curve$Upper <- curve$Fitted
  curve <- curve[is.finite(curve$Year) & is.finite(curve$Fitted), , drop = FALSE]
  if (nrow(curve) == 0) {
    return(NULL)
  }

  params <- best$parameter_summary %||% list()
  capacity <- bslr_num(params$carrying_capacity)
  inflection <- bslr_num(params$inflection_year)
  growth_rate <- bslr_num(params$growth_rate)
  transition_year <- tryCatch(m2_compute_transition_year(best$name %||% "", params), error = function(e) NA_real_)
  y_values <- c(raw_df$Articles, curve$Fitted, curve$Lower, curve$Upper, capacity)
  y_range <- range(y_values[is.finite(y_values)], na.rm = TRUE)
  y_span <- diff(y_range)
  if (!is.finite(y_span) || y_span <= 0) y_span <- max(y_range, 1)
  x_range <- range(raw_df$Year, na.rm = TRUE)

  label <- paste(
    c(
      best$name %||% "Selected model",
      sprintf("Adj. R2 = %.3f", bslr_num(best$Adj_R2)),
      sprintf("RMSE = %.2f", bslr_num(best$RMSE)),
      if (is.finite(capacity)) sprintf("K = %.1f", capacity) else NULL,
      if (is.finite(inflection)) sprintf("t0 = %.1f", inflection) else NULL
    ),
    collapse = "\n"
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = curve,
      ggplot2::aes(x = Year, ymin = Lower, ymax = Upper),
      fill = "#BFC5CF",
      alpha = 0.22
    ) +
    ggplot2::geom_line(
      data = curve,
      ggplot2::aes(x = Year, y = Fitted),
      color = "black",
      linewidth = 0.78,
      lineend = "round"
    ) +
    ggplot2::geom_point(
      data = raw_df,
      ggplot2::aes(x = Year, y = Articles),
      color = "black",
      size = 1.42,
      alpha = 0.9
    ) +
    ggplot2::annotate(
      "label",
      x = x_range[1] + 0.03 * diff(x_range),
      y = y_range[1] + 0.17 * y_span,
      label = label,
      hjust = 0,
      vjust = 0,
      size = 2.45,
      linewidth = 0.18,
      label.padding = grid::unit(0.13, "lines"),
      fill = grDevices::adjustcolor("white", alpha.f = 0.9)
    ) +
    ggplot2::scale_x_continuous(name = "Year", breaks = scales::breaks_pretty(n = 7)) +
    ggplot2::scale_y_continuous(name = "Annual publications", labels = scales::label_number(big.mark = ",")) +
    ggplot2::labs(
      title = "Annual production fitted by the headline interpretable model",
      subtitle = sprintf(
        "Benchmark: %s | growth parameter: %s",
        best$benchmark_name %||% "not reported",
        if (is.finite(growth_rate)) sprintf("%.4f", growth_rate) else "not estimated"
      ),
      caption = bslr_wrap(best$selection_reason %||% "Model selected by the M2 composite ranking.", width = 120)
    ) +
    ieee_theme_wide(base_size = 7.5) +
    ggplot2::theme(
      legend.position = "none",
      plot.caption = ggplot2::element_text(size = 5.8, color = "#555555", hjust = 0)
    )

  if (is.finite(capacity) && capacity > y_range[1] && capacity < y_range[2] + 0.4 * y_span) {
    p <- p +
      ggplot2::geom_hline(yintercept = capacity, color = "#4F8A46", linetype = "22", linewidth = 0.45) +
      ggplot2::annotate("text", x = x_range[2], y = capacity, label = "K", hjust = 1.1, vjust = -0.35, size = 2.5, color = "#4F8A46")
  }
  if (is.finite(inflection)) {
    p <- p + ggplot2::geom_vline(xintercept = inflection, color = "#3E6EA7", linetype = "42", linewidth = 0.5)
  }
  if (is.finite(transition_year)) {
    p <- p + ggplot2::geom_vline(xintercept = transition_year, color = "#C94C4C", linetype = "42", linewidth = 0.5)
  }

  ieee_mark_plot_layout(p, "full")
}

bslr_make_m2_validation_plot <- function(module_result) {
  plot <- bslr_get_module_plot(module_result, "advanced_journal", "forecast_backtesting_heatmap")
  if (!is.null(plot)) {
    return(plot)
  }
  leaderboard <- module_result$data$advanced_journal$forecast_validation$leaderboard %||% data.frame()
  if (!is.data.frame(leaderboard) || nrow(leaderboard) == 0 || !"mase" %in% names(leaderboard)) {
    return(NULL)
  }
  df <- leaderboard |>
    dplyr::mutate(
      mase = suppressWarnings(as.numeric(.data$mase)),
      horizon = as.factor(.data$horizon)
    ) |>
    dplyr::filter(is.finite(.data$mase))
  if (nrow(df) == 0) {
    return(NULL)
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$horizon, y = .data$model, fill = .data$mase)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$mase)), size = 2.45, color = "#111111") +
    ggplot2::scale_fill_gradient(low = "#F7FBFF", high = "#084594", name = "MASE") +
    ggplot2::labs(
      title = "M2 Forecast Backtesting Leaderboard",
      subtitle = "Rolling-origin validation by model and horizon; MASE below 1 beats naive forecasting",
      x = "Forecast horizon",
      y = NULL,
      caption = "The manuscript uses this plot to justify forecast claims, not to overstate long-range prediction."
    ) +
    ieee_theme_wide(base_size = 7.8)
  ieee_mark_plot_layout(p, "full")
}

bslr_make_m2_forecast_plot <- function(module_result) {
  data <- module_result$data$forecasting %||% list()
  if (!identical(data$status %||% "success", "success") || length(data$years %||% numeric()) == 0) {
    return(NULL)
  }
  years <- as.numeric(data$years)
  articles <- as.numeric(data$articles)
  horizon <- as.integer(data$horizon %||% length(data$ensemble$forecast %||% numeric()))
  if (length(years) < 2 || length(articles) != length(years) || horizon <= 0) {
    return(NULL)
  }
  forecast_years <- as.numeric(data$ensemble$years %||% seq(max(years, na.rm = TRUE) + 1, length.out = horizon))
  forecast_values <- as.numeric(data$ensemble$forecast %||% rep(NA_real_, horizon))
  forecast_df <- data.frame(Year = forecast_years, Forecast = forecast_values)
  forecast_df <- forecast_df[is.finite(forecast_df$Year) & is.finite(forecast_df$Forecast), , drop = FALSE]
  if (nrow(forecast_df) == 0) {
    return(NULL)
  }
  obs_df <- data.frame(Year = years, Articles = articles)
  pi <- data$prediction_intervals$arima %||% list()
  ribbon <- data.frame(
    Year = forecast_years,
    lower = as.numeric(pi$lower_95 %||% rep(NA_real_, horizon)),
    upper = as.numeric(pi$upper_95 %||% rep(NA_real_, horizon))
  )
  ribbon <- ribbon[is.finite(ribbon$Year) & is.finite(ribbon$lower) & is.finite(ribbon$upper), , drop = FALSE]
  comparison <- data$model_comparison$comparison %||% data.frame()
  best_model <- data$model_comparison$best_model %||% "ensemble"
  cv_line <- ""
  if (is.data.frame(comparison) && nrow(comparison) > 0 && "CV_MAE" %in% names(comparison)) {
    best_row <- comparison[comparison$model == best_model, , drop = FALSE]
    if (nrow(best_row) > 0) {
      cv_line <- sprintf(" | CV MAE = %.2f", bslr_num(best_row$CV_MAE[1]))
    }
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = obs_df, ggplot2::aes(x = Year, y = Articles), color = "black", linewidth = 0.68) +
    ggplot2::geom_point(data = obs_df, ggplot2::aes(x = Year, y = Articles), color = "black", size = 1.25) +
    {if (nrow(ribbon) > 0) ggplot2::geom_ribbon(data = ribbon, ggplot2::aes(x = Year, ymin = lower, ymax = upper), fill = "#D0D4DA", alpha = 0.35) else NULL} +
    ggplot2::geom_vline(xintercept = max(years, na.rm = TRUE), color = "#C94C4C", linetype = "22", linewidth = 0.48) +
    ggplot2::geom_line(data = forecast_df, ggplot2::aes(x = Year, y = Forecast), color = "#C94C4C", linewidth = 0.82) +
    ggplot2::geom_point(data = forecast_df, ggplot2::aes(x = Year, y = Forecast), color = "#C94C4C", size = 1.45) +
    ggplot2::scale_x_continuous(name = "Year", breaks = scales::breaks_pretty(n = 8)) +
    ggplot2::scale_y_continuous(name = "Annual publications", labels = scales::label_number(big.mark = ",")) +
    ggplot2::labs(
      title = "Short-horizon forecast of annual production",
      subtitle = paste0("Forecast origin marked by the dashed line | best comparator: ", best_model, cv_line),
      caption = "The ribbon reports the available 95% prediction interval from the forecasting layer."
    ) +
    ieee_theme_wide(base_size = 7.5) +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 5.8, color = "#555555", hjust = 0))

  ieee_mark_plot_layout(p, "full")
}

bslr_make_m3_quadrant_plot <- function(module_result) {
  quad <- module_result$data$experiments$quadrant %||% data.frame()
  if (!is.data.frame(quad) || nrow(quad) < 2) {
    return(NULL)
  }
  country_col <- bslr_pick_col(quad, c("country", "Country"))
  articles_col <- bslr_pick_col(quad, c("article_count", "Articles"))
  cites_col <- bslr_pick_col(quad, c("total_citations", "Total Cit.", "Total Citations"))
  avg_col <- bslr_pick_col(quad, c("avg_citations", "Avg Cit.", "Average Citations"))
  quadrant_col <- bslr_pick_col(quad, c("quadrant", "Quadrant"))
  output_median_col <- bslr_pick_col(quad, c("output_median"))
  impact_median_col <- bslr_pick_col(quad, c("impact_median"))
  if (any(is.na(c(country_col, articles_col, cites_col, avg_col, quadrant_col)))) {
    return(NULL)
  }
  df <- data.frame(
    country = m3_title_case_country(quad[[country_col]]),
    article_count = suppressWarnings(as.numeric(quad[[articles_col]])),
    total_citations = suppressWarnings(as.numeric(quad[[cites_col]])),
    avg_citations = suppressWarnings(as.numeric(quad[[avg_col]])),
    quadrant = as.character(quad[[quadrant_col]]),
    stringsAsFactors = FALSE
  )
  df <- df[is.finite(df$article_count) & is.finite(df$avg_citations), , drop = FALSE]
  if (nrow(df) < 2) {
    return(NULL)
  }
  df <- df %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      article_count = sum(article_count, na.rm = TRUE),
      total_citations = sum(total_citations, na.rm = TRUE),
      avg_citations = dplyr::if_else(
        article_count > 0,
        total_citations / article_count,
        mean(avg_citations, na.rm = TRUE)
      ),
      .groups = "drop"
    )
  if (nrow(df) < 2) {
    return(NULL)
  }
  x_med <- stats::median(df$article_count, na.rm = TRUE)
  y_med <- stats::median(df$avg_citations, na.rm = TRUE)
  df$quadrant <- dplyr::case_when(
    df$article_count >= x_med & df$avg_citations >= y_med ~ "High-Output / High-Impact",
    df$article_count >= x_med & df$avg_citations < y_med ~ "High-Output / Low-Impact",
    df$article_count < x_med & df$avg_citations >= y_med ~ "Low-Output / High-Impact",
    TRUE ~ "Low-Output / Low-Impact"
  )
  labels <- head(df$country[order(-df$article_count, -df$total_citations)], 8)
  label_df <- df[df$country %in% labels, , drop = FALSE]

  palette <- c(
    "High-Output / High-Impact" = "#1F5A85",
    "High-Output / Low-Impact" = "#B55A4F",
    "Low-Output / High-Impact" = "#4D7E49",
    "Low-Output / Low-Impact" = "#8A8A8A"
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(x = article_count, y = avg_citations)) +
    ggplot2::geom_vline(xintercept = x_med, linetype = "22", color = "#5E6470", linewidth = 0.42) +
    ggplot2::geom_hline(yintercept = y_med, linetype = "22", color = "#5E6470", linewidth = 0.42) +
    ggplot2::geom_point(
      ggplot2::aes(color = quadrant, size = total_citations),
      alpha = 0.88,
      stroke = 0.15
    ) +
    ggplot2::scale_color_manual(values = palette, name = NULL) +
    ggplot2::scale_size_area(max_size = 5.4, guide = "none") +
    ggplot2::scale_x_continuous(
      name = "Publications",
      trans = scales::pseudo_log_trans(base = 10),
      labels = scales::label_number(big.mark = ","),
      expand = ggplot2::expansion(mult = c(0.08, 0.13))
    ) +
    ggplot2::scale_y_continuous(
      name = "Average citations per article",
      labels = scales::label_number(accuracy = 0.1),
      expand = ggplot2::expansion(mult = c(0.06, 0.12))
    ) +
    ggplot2::labs(
      title = "Country productivity-impact positioning",
      subtitle = "Dashed guides mark median output and median citation impact",
      caption = "Labels are limited to the highest-volume and highest-impact countries to avoid overlap."
    ) +
    ieee_theme_wide(base_size = 7.4) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 6.3),
      plot.caption = ggplot2::element_text(size = 5.8, color = "#555555", hjust = 0)
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 2, byrow = TRUE))

  p <- p + ggplot2::geom_text(
    data = label_df,
    ggplot2::aes(label = country),
    size = 2.05,
    vjust = -0.7,
    check_overlap = TRUE,
    show.legend = FALSE
  )

  ieee_mark_plot_layout(p, "full")
}

bslr_make_m3_share_plot <- function(module_result) {
  share <- module_result$data$temporal_dynamics$share_evolution %||% list()
  df <- share$share_trends %||% data.frame()
  if (!is.data.frame(df) || nrow(df) == 0 || !"change" %in% names(df)) {
    return(NULL)
  }
  country_col <- bslr_pick_col(df, c("country", "Country"))
  if (is.na(country_col)) {
    return(NULL)
  }
  df$country <- m3_title_case_country(df[[country_col]])
  df$change <- suppressWarnings(as.numeric(df$change))
  df <- df[is.finite(df$change), , drop = FALSE]
  if (nrow(df) == 0) {
    return(NULL)
  }
  pos_df <- head(df[order(-df$change), , drop = FALSE], 6)
  neg_df <- head(df[order(df$change), , drop = FALSE], 6)
  plot_df <- dplyr::bind_rows(pos_df, neg_df)
  plot_df <- plot_df[!duplicated(plot_df$country), , drop = FALSE]
  plot_df$direction <- ifelse(plot_df$change > 0, "Gaining share", "Losing share")
  plot_df <- plot_df[order(plot_df$change), , drop = FALSE]
  plot_df$country <- factor(plot_df$country, levels = plot_df$country)

  first_label <- if (length(share$first_window %||% numeric()) > 0) {
    paste0(min(share$first_window), "-", max(share$first_window))
  } else {
    "initial window"
  }
  last_label <- if (length(share$last_window %||% numeric()) > 0) {
    paste0(min(share$last_window), "-", max(share$last_window))
  } else {
    "final window"
  }

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = country, y = change, fill = direction)) +
    ggplot2::geom_col(width = 0.68, color = "black", linewidth = 0.18) +
    ggplot2::coord_flip() +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.38) +
    ggplot2::scale_fill_manual(values = c("Gaining share" = "#4D7E49", "Losing share" = "#B55A4F"), name = NULL) +
    ggplot2::scale_y_continuous(name = "Share change (percentage points)", labels = scales::label_number(accuracy = 0.1)) +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::labs(
      title = "Largest country share shifts across observation windows",
      subtitle = sprintf("Window comparison: %s vs %s", first_label, last_label),
      caption = "Positive values indicate an increased share of global corpus output."
    ) +
    ieee_theme_wide(base_size = 7.4) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(size = 5.8, color = "#555555", hjust = 0)
    )

  ieee_mark_plot_layout(p, "full")
}

bslr_make_m3_concentration_plot <- function(module_result) {
  module_plot <- bslr_get_module_plot(module_result, "advanced_journal", "concentration_dashboard")
  concentration <- module_result$data$advanced_journal$geo_concentration %||% list()
  lorenz <- concentration$lorenz %||% data.frame()
  metrics <- concentration$table %||% data.frame()
  if (!is.data.frame(lorenz) || nrow(lorenz) == 0 ||
      !all(c("cumulative_countries", "cumulative_production") %in% names(lorenz))) {
    return(module_plot)
  }
  prod <- if (is.data.frame(metrics) && nrow(metrics) > 0 && "metric" %in% names(metrics)) {
    metrics[metrics$metric == "production", , drop = FALSE]
  } else {
    data.frame()
  }
  subtitle <- if (nrow(prod) > 0) {
    sprintf(
      "Gini %s | HHI %s | Theil %s | Top-5 %s",
      bslr_number_text(prod$gini[1], 3L),
      bslr_number_text(prod$hhi[1], 3L),
      bslr_number_text(prod$theil[1], 3L),
      bslr_percent_text(prod$top5_share[1])
    )
  } else {
    "Lorenz curve for country production"
  }
  p <- ggplot2::ggplot(lorenz, ggplot2::aes(x = .data$cumulative_countries, y = .data$cumulative_production)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "22", color = "#777777", linewidth = 0.4) +
    ggplot2::geom_area(fill = "#1F5A85", alpha = 0.12) +
    ggplot2::geom_line(color = "#1F5A85", linewidth = 0.90) +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title = "M3 Geographic Concentration",
      subtitle = subtitle,
      x = "Countries (cumulative share)",
      y = "Production (cumulative share)",
      caption = "Distance from the diagonal summarizes concentration."
    ) +
    ieee_theme_wide(base_size = 6.7) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(size = 5.1, color = "#555555", hjust = 0),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 3)),
      plot.margin = ggplot2::margin(6, 8, 6, 8)
    )
  ieee_mark_plot_layout(p, "single")
}

bslr_make_m3_trajectory_plot <- function(module_result) {
  trajectories <- module_result$data$advanced_journal$trajectories$table %||% data.frame()
  if (!is.data.frame(trajectories) || nrow(trajectories) == 0 ||
      !all(c("country", "share_change", "share_last", "trajectory_class") %in% names(trajectories))) {
    return(NULL)
  }
  if (!"rank_change" %in% names(trajectories)) {
    trajectories$rank_change <- 0
  }
  df <- trajectories |>
    dplyr::mutate(
      country = m3_title_case_country(.data$country),
      share_change = suppressWarnings(as.numeric(.data$share_change)),
      share_last = suppressWarnings(as.numeric(.data$share_last)),
      rank_change = suppressWarnings(as.numeric(.data$rank_change))
    ) |>
    dplyr::filter(is.finite(.data$share_change), is.finite(.data$share_last)) |>
    dplyr::arrange(dplyr::desc(abs(.data$share_change))) |>
    utils::head(25)
  if (nrow(df) == 0) {
    return(NULL)
  }
  df$trajectory_class <- factor(df$trajectory_class, levels = c("emerging", "late_entry", "volatile", "stable", "declining"))
  label_df <- df |>
    dplyr::arrange(dplyr::desc(abs(.data$share_change)), dplyr::desc(.data$share_last)) |>
    utils::head(10)
  label_df$label_hjust <- ifelse(label_df$share_change <= stats::median(df$share_change, na.rm = TRUE), -0.05, 1.05)
  label_df$label_vjust <- ifelse(label_df$share_last >= stats::median(df$share_last, na.rm = TRUE), -0.50, 1.25)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$share_change, y = .data$share_last)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "22", color = "#666666", linewidth = 0.35) +
    ggplot2::geom_point(
      ggplot2::aes(size = abs(.data$rank_change), fill = .data$trajectory_class),
      shape = 21,
      color = "#111111",
      alpha = 0.84
    ) +
    ggplot2::geom_text(
      data = label_df,
      ggplot2::aes(label = .data$country, hjust = .data$label_hjust, vjust = .data$label_vjust),
      size = 2.1,
      check_overlap = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = c(emerging = "#4D7E49", declining = "#B55A4F", stable = "#BDBDBD", volatile = "#EDB120", late_entry = "#1F5A85"),
      drop = FALSE,
      name = "Trajectory"
    ) +
    ggplot2::scale_size_area(max_size = 7.0, name = "Rank shift") +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 0.1), name = "Production-share change") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), name = "Late-window production share") +
    ggplot2::labs(
      title = "M3 Emerging and Declining Country Trajectories",
      subtitle = "Share movement and late-window position classify country roles",
      caption = "Labels are pruned by overlap rules; full country trajectory tables are exported with the bundle."
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ieee_theme_wide(base_size = 7.8) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(size = 5.9, color = "#555555", hjust = 0),
      plot.margin = ggplot2::margin(8, 28, 8, 24)
    )
  ieee_mark_plot_layout(p, "full")
}

bslr_build_paper_figure_rows <- function(module_results,
                                         bibliometric_map,
                                         journal_assessment,
                                         protocol,
                                         figures_dir) {
  specs <- list(
    list(
      exhibit_id = "fig_bslr_workflow",
      module_id = "bslr",
      caption = "Ten-step B-SLR execution logic adapted to the RBiblioSynth workflow.",
      interpretation = "This figure fixes the methodological story: PRISMA is the record-flow audit, while B-SLR checkpoints control consolidation, preliminary bibliometric assessment, and contribution assessment.",
      layout = "full",
      height = 4.35,
      builder = function() bslr_make_bslr_workflow_plot(protocol, module_results, bibliometric_map, journal_assessment)
    ),
    list(
      exhibit_id = "fig_bslr_evidence_bridge",
      module_id = "bslr",
      caption = "Evidence bridge showing how automated module outputs become human-coded systematic-review inputs.",
      interpretation = "The bridge prevents a common B-SLR failure: treating maps, trends, and rankings as conclusions before the representative papers have been read and coded.",
      layout = "full",
      height = 2.85,
      builder = function() bslr_make_evidence_bridge_plot(module_results, bibliometric_map, journal_assessment, protocol)
    ),
    list(
      exhibit_id = "fig_prisma",
      module_id = "m0",
      caption = "B-SLR and PRISMA flow linking corpus consolidation to the later synthesis checkpoints.",
      interpretation = "The figure makes the review boundary auditable: records move from search to canonical merge, screening, eligibility, and final inclusion, while exclusions remain visible rather than hidden in prose.",
      layout = "full",
      height = 5.0,
      builder = function() bslr_make_prisma_plot(module_results$m0 %||% list())
    ),
    list(
      exhibit_id = "fig_m1_overview",
      module_id = "m1",
      caption = "Compact audit overview of the refined corpus, bibliometric scope, and thematic mapping structure.",
      interpretation = "This panel is the manuscript-level checksum for the corpus: size, time span, country coverage, and cluster count must match the methods section.",
      layout = "single",
      height = 2.45,
      builder = function() bslr_make_m1_overview_plot(module_results, bibliometric_map)
    ),
    list(
      exhibit_id = "fig_bslr_cluster_strategy",
      module_id = "bslr",
      caption = "Cluster-guided sample-ordering strategy for the systematic literature review.",
      interpretation = "This exhibit links Step 7 cluster identification to Step 8 representative sampling, making clear which research streams will anchor human coding.",
      layout = "full",
      height = 3.30,
      builder = function() bslr_make_cluster_strategy_plot(bibliometric_map)
    ),
    list(
      exhibit_id = "fig_m2_model_selection",
      module_id = "m2",
      caption = "M2 model-selection evidence comparing interpretable candidates against flexible benchmarks.",
      interpretation = "The figure supports the headline temporal model choice and makes the interpretability tradeoff visible instead of hiding it in a table.",
      layout = "full",
      height = 3.45,
      builder = function() bslr_make_m2_model_selection_plot(module_results$m2 %||% list())
    ),
    list(
      exhibit_id = "fig_m2_growth",
      module_id = "m2",
      caption = "Growth-regime evidence for the selected interpretable temporal model.",
      interpretation = "The plot moves M2 from curve fitting to interpretation by showing cumulative production, fitted saturation, inflection, peak-growth timing, and remaining distance to capacity.",
      layout = "full",
      height = 3.95,
      builder = function() bslr_make_m2_growth_plot(module_results$m2 %||% list())
    ),
    list(
      exhibit_id = "fig_m2_validation",
      module_id = "m2",
      caption = "Rolling-origin forecast validation leaderboard for annual production.",
      interpretation = "The heatmap supports or rejects forecasting claims by comparing horizons and baselines; MASE below one is the key threshold for beating naive forecasting.",
      layout = "full",
      height = 3.25,
      builder = function() bslr_make_m2_validation_plot(module_results$m2 %||% list())
    ),
    list(
      exhibit_id = "fig_m2_forecast",
      module_id = "m2",
      caption = "Short-horizon annual-production forecast with origin marker and available prediction interval.",
      interpretation = "This figure is retained as the reader-facing forecast, while the validation leaderboard determines whether the forecast should be stated as supported or only exploratory.",
      layout = "full",
      height = 3.35,
      builder = function() bslr_make_m2_forecast_plot(module_results$m2 %||% list())
    ),
    list(
      exhibit_id = "fig_m3_concentration",
      module_id = "m3",
      caption = "Geographic concentration evidence using the country-production Lorenz curve and concentration metrics.",
      interpretation = "The Lorenz curve explains whether production is broadly distributed or concentrated in a small group of countries; the captioned metrics provide the statistical support.",
      layout = "single",
      height = 2.70,
      builder = function() bslr_make_m3_concentration_plot(module_results$m3 %||% list())
    ),
    list(
      exhibit_id = "fig_m3_collaboration_premium",
      module_id = "m3",
      caption = "International collaboration citation premium comparing MCP and SCP documents.",
      interpretation = "The forest-style interval gives the collaboration claim statistical weight: direction, magnitude, uncertainty, and test evidence are visible in one exhibit.",
      layout = "single",
      height = 2.45,
      builder = function() bslr_make_m3_collaboration_premium_plot(module_results$m3 %||% list())
    ),
    list(
      exhibit_id = "fig_m3_rank_mobility",
      module_id = "m3",
      caption = "Country leadership mobility between early and late observation windows.",
      interpretation = "The rank-shift chart adds a temporal comparative story to the geographic evidence by showing whether leadership is persistent, emerging, or declining.",
      layout = "full",
      height = 3.55,
      builder = function() bslr_make_m3_rank_mobility_plot(module_results$m3 %||% list())
    ),
    list(
      exhibit_id = "fig_m3_trajectories",
      module_id = "m3",
      caption = "Emerging and declining country trajectories across early and late windows.",
      interpretation = "This view tells the country-dynamics story directly: who gained share, who declined, and which countries changed rank enough to matter analytically.",
      layout = "full",
      height = 3.75,
      builder = function() bslr_make_m3_trajectory_plot(module_results$m3 %||% list())
    ),
    list(
      exhibit_id = "fig_m3_quadrant",
      module_id = "m3",
      caption = "Country productivity-impact quadrant using publication volume and average citation impact.",
      interpretation = "Quadrants separate volume leadership from citation impact, preventing the manuscript from equating productivity with influence.",
      layout = "full",
      height = 3.85,
      builder = function() bslr_make_m3_quadrant_plot(module_results$m3 %||% list())
    ),
    list(
      exhibit_id = "fig_m3_share_trends",
      module_id = "m3",
      caption = "Largest positive and negative country share shifts between the initial and final observation windows.",
      interpretation = "The share-shift chart complements the quadrant by showing temporal movement rather than only end-state position.",
      layout = "full",
      height = 3.35,
      builder = function() bslr_make_m3_share_plot(module_results$m3 %||% list())
    )
  )

  rows <- lapply(specs, function(spec) {
    plot <- tryCatch(spec$builder(), error = function(e) NULL)
    bslr_export_paper_plot(
      plot = plot,
      figures_dir = figures_dir,
      exhibit_id = spec$exhibit_id,
      module_id = spec$module_id,
      caption = spec$caption,
      interpretation = spec$interpretation %||% "",
      layout = spec$layout,
      height = spec$height
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame())
  }
  dplyr::bind_rows(rows)
}

bslr_collect_paper_exhibits <- function(module_results,
                                        bibliometric_map,
                                        journal_assessment,
                                        protocol,
                                        paper_dir) {
  figures_dir <- file.path(paper_dir, "figures")
  tables_dir <- file.path(paper_dir, "tables")
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

  generated_figures <- bslr_build_paper_figure_rows(
    module_results = module_results,
    bibliometric_map = bibliometric_map,
    journal_assessment = journal_assessment,
    protocol = protocol,
    figures_dir = figures_dir
  )

  fallback_specs <- list(
    list(exhibit_id = "fig_prisma", module_id = "m0", pattern = "m0_prisma_diagram\\.(png|svg|pdf|eps)$", caption = "PRISMA-style flow diagram summarizing record identification, screening, and inclusion decisions.", interpretation = "Fallback M0 diagram copied from the module manifest.", layout = "full"),
    list(exhibit_id = "fig_m1_overview", module_id = "m1", pattern = "m1_overview_summary_bar\\.(png|svg|pdf|eps)$", caption = "Descriptive overview of the refined bibliometric corpus.", interpretation = "Fallback M1 overview copied from the module manifest.", layout = "single"),
    list(exhibit_id = "fig_m2_model_selection", module_id = "m2", pattern = "m2_(regression_model_comparison_score|growth_models_model_comparison|advanced_ts_model_comparison)\\.(png|svg|pdf|eps)$", caption = "M2 model-selection evidence comparing interpretable and flexible candidates.", interpretation = "Fallback M2 model-selection plot copied from the module manifest.", layout = "full"),
    list(exhibit_id = "fig_m2_growth", module_id = "m2", pattern = "m2_regression_best_model\\.(png|svg|pdf|eps)$", caption = "Headline interpretable growth model fitted to annual production.", interpretation = "Fallback M2 growth plot copied from the module manifest.", layout = "full"),
    list(exhibit_id = "fig_m2_validation", module_id = "m2", pattern = "m2_advanced_journal_forecast_backtesting_heatmap\\.(png|svg|pdf|eps)$", caption = "Rolling-origin forecast validation leaderboard.", interpretation = "Fallback M2 validation heatmap copied from the module manifest.", layout = "full"),
    list(exhibit_id = "fig_m2_forecast", module_id = "m2", pattern = "m2_forecasting_forecast\\.(png|svg|pdf|eps)$", caption = "Forecast trajectory and held-out comparison for annual production.", interpretation = "Fallback M2 forecast plot copied from the module manifest.", layout = "full"),
    list(exhibit_id = "fig_m3_concentration", module_id = "m3", pattern = "m3_advanced_journal_concentration_dashboard\\.(png|svg|pdf|eps)$", caption = "Geographic concentration dashboard for country production.", interpretation = "Fallback M3 concentration dashboard copied from the module manifest.", layout = "single"),
    list(exhibit_id = "fig_m3_collaboration_premium", module_id = "m3", pattern = "m3_advanced_journal_collaboration_premium_forest\\.(png|svg|pdf|eps)$", caption = "International collaboration citation premium comparing MCP and SCP documents.", interpretation = "Fallback M3 collaboration-premium plot copied from the module manifest.", layout = "single"),
    list(exhibit_id = "fig_m3_rank_mobility", module_id = "m3", pattern = "m3_advanced_journal_rank_mobility_bump\\.(png|svg|pdf|eps)$", caption = "Country leadership mobility between observation windows.", interpretation = "Fallback M3 rank-mobility plot copied from the module manifest.", layout = "full"),
    list(exhibit_id = "fig_m3_trajectories", module_id = "m3", pattern = "m3_advanced_journal_emerging_declining_bubbles\\.(png|svg|pdf|eps)$", caption = "Emerging and declining country trajectory plot.", interpretation = "Fallback M3 trajectory plot copied from the module manifest.", layout = "full"),
    list(exhibit_id = "fig_m3_quadrant", module_id = "m3", pattern = "m3_experiments_quadrant\\.(png|svg|pdf|eps)$", caption = "Country-level productivity-impact quadrants highlighting comparative positioning.", interpretation = "Fallback M3 quadrant plot copied from the module manifest.", layout = "full"),
    list(exhibit_id = "fig_m3_share_trends", module_id = "m3", pattern = "m3_temporal_dynamics_share_trends\\.(png|svg|pdf|eps)$", caption = "Country share trajectories across the temporal observation windows.", interpretation = "Fallback M3 share-trend plot copied from the module manifest.", layout = "full")
  )

  generated_ids <- if (is.data.frame(generated_figures) && nrow(generated_figures) > 0) {
    generated_figures$exhibit_id
  } else {
    character()
  }
  fallback_rows <- lapply(fallback_specs, function(spec) {
    if (spec$exhibit_id %in% generated_ids) {
      return(NULL)
    }
    manifest <- module_results[[spec$module_id]]$artifacts$manifest$plots %||% character()
    src <- bslr_select_manifest_file(manifest, spec$pattern)
    copied <- bslr_copy_figure_family(src, figures_dir, spec$exhibit_id)
    bslr_figure_row_from_paths(
      paths = copied,
      exhibit_id = spec$exhibit_id,
      module_id = spec$module_id,
      caption = spec$caption,
      interpretation = spec$interpretation %||% "",
      layout = spec$layout,
      source = "module_fallback"
    )
  })
  fallback_rows <- Filter(Negate(is.null), fallback_rows)
  fallback_figures <- if (length(fallback_rows) > 0) dplyr::bind_rows(fallback_rows) else data.frame()
  figures <- dplyr::bind_rows(generated_figures, fallback_figures)
  figures <- bslr_curate_figures_for_paper(figures, journal_assessment)
  main_figures <- if (is.data.frame(figures) && nrow(figures) > 0 && "curation_role" %in% names(figures)) {
    figures[figures$curation_role == "main", , drop = FALSE]
  } else {
    figures
  }

  table_specs <- list(
    list(exhibit_id = "tbl_screening_stage_counts", module_id = "m0", caption = "Screening decision counts by review stage.", data = module_results$m0$data$screening_summary$stage_counts %||% data.frame()),
    list(exhibit_id = "tbl_cluster_summary", module_id = "bslr", caption = "Cluster sizes and citation profiles from the bibliometric map.", data = bibliometric_map$cluster_summary %||% data.frame()),
    list(exhibit_id = "tbl_sample_selected", module_id = "bslr", caption = "Representative documents selected for downstream coding and thematic synthesis.", data = bibliometric_map$sample_selected %||% data.frame()),
    list(exhibit_id = "tbl_journal_gates", module_id = "bslr", caption = "Journal-grade readiness gates and current evidence.", data = journal_assessment$gates %||% data.frame()),
    list(exhibit_id = "tbl_quality_gate", module_id = "bslr", caption = "PRISMA/B-SLR quality gate checklist before journal-ready claims.", data = journal_assessment$quality_gate$checklist %||% data.frame()),
    list(exhibit_id = "tbl_claim_ledger", module_id = "bslr", caption = "Auditable claim ledger linking claims to evidence, tests, limitations, and strength.", data = journal_assessment$claim_ledger %||% data.frame()),
    list(exhibit_id = "tbl_reproducibility", module_id = "bslr", caption = "Reproducibility capsule summary for the generated manuscript bundle.", data = journal_assessment$reproducibility$summary %||% data.frame())
  )

  table_rows <- lapply(table_specs, function(spec) {
    tab <- spec$data
    if (!is.data.frame(tab) || nrow(tab) == 0) {
      return(NULL)
    }
    dest <- file.path(tables_dir, paste0(spec$exhibit_id, ".csv"))
    utils::write.csv(tab, dest, row.names = FALSE, na = "")
    data.frame(
      exhibit_id = spec$exhibit_id,
      type = "table",
      module_id = spec$module_id,
      caption = spec$caption,
      relative_path = file.path("tables", basename(dest)),
      absolute_path = dest,
      stringsAsFactors = FALSE
    )
  })
  table_rows <- Filter(Negate(is.null), table_rows)
  tables <- if (length(table_rows) > 0) dplyr::bind_rows(table_rows) else data.frame()

  inventory <- dplyr::bind_rows(figures, tables)
  inventory_csv <- file.path(paper_dir, "bslr_exhibit_inventory.csv")
  inventory_json <- file.path(paper_dir, "bslr_exhibit_inventory.json")
  if (is.data.frame(inventory) && nrow(inventory) > 0) {
    utils::write.csv(inventory, inventory_csv, row.names = FALSE, na = "")
    write_json_artifact(inventory, inventory_json)
  } else {
    inventory_csv <- NA_character_
    inventory_json <- NA_character_
  }

  figure_files <- if (is.data.frame(figures) && "all_paths" %in% names(figures)) {
    unique(unlist(strsplit(paste(figures$all_paths, collapse = ";"), ";", fixed = TRUE), use.names = FALSE))
  } else if (is.data.frame(figures) && "absolute_path" %in% names(figures)) {
    figures$absolute_path
  } else {
    character()
  }
  figure_files <- figure_files[nzchar(figure_files) & file.exists(figure_files)]
  table_files <- if (is.data.frame(tables) && "absolute_path" %in% names(tables)) tables$absolute_path else character()

  list(
    figures = main_figures,
    figures_all = figures,
    tables = tables,
    inventory = inventory,
    inventory_csv = inventory_csv,
    inventory_json = inventory_json,
    files = c(figure_files, table_files)
  )
}

bslr_curate_figures_for_paper <- function(figures, journal_assessment = list()) {
  if (!is.data.frame(figures) || nrow(figures) == 0 || !"exhibit_id" %in% names(figures)) {
    return(figures)
  }

  claim_ledger <- journal_assessment$claim_ledger %||% data.frame()
  quality_gate <- journal_assessment$quality_gate %||% list()
  gate_ok <- identical(quality_gate$recommendation %||% "", "journal_ready_with_human_synthesis") ||
    identical(quality_gate$recommendation %||% "", "near_ready_close_methodological_gaps")

  core_ids <- c(
    "fig_bslr_workflow", "fig_prisma", "fig_m1_overview",
    "fig_m2_model_selection", "fig_m2_growth", "fig_m2_validation",
    "fig_m3_concentration", "fig_m3_collaboration_premium",
    "fig_m3_rank_mobility", "fig_m3_trajectories", "fig_m3_quadrant"
  )

  support_score <- rep(0, nrow(figures))
  if (is.data.frame(claim_ledger) && nrow(claim_ledger) > 0 && "plot_source" %in% names(claim_ledger)) {
    for (i in seq_len(nrow(figures))) {
      id <- figures$exhibit_id[i]
      score <- sum(grepl(gsub("^fig_", "", id), claim_ledger$plot_source, ignore.case = TRUE), na.rm = TRUE)
      support_score[i] <- score
    }
  }

  figures$claim_support_score <- support_score
  figures$curation_role <- ifelse(
    figures$exhibit_id %in% core_ids | support_score > 0,
    "main",
    "appendix"
  )
  figures$curation_reason <- ifelse(
    figures$curation_role == "main",
    "Selected for the main paper because it supports the B-SLR storyline or an auditable claim.",
    "Retained in appendix/inventory as a diagnostic or robustness exhibit."
  )

  if (!gate_ok && "fig_bslr_workflow" %in% figures$exhibit_id) {
    figures$curation_reason[figures$exhibit_id == "fig_bslr_workflow"] <- "Main-paper method anchor; quality gate still records remaining submission blockers."
  }

  figures
}

bslr_select_manifest_file <- function(paths, pattern) {
  paths <- unique(as.character(paths %||% character()))
  paths <- paths[file.exists(paths)]
  if (length(paths) == 0) {
    return(NA_character_)
  }
  match <- paths[grepl(pattern, basename(paths), ignore.case = TRUE)]
  if (length(match) == 0) {
    return(NA_character_)
  }
  match[1]
}

bslr_build_figure_blocks <- function(figures) {
  if (!is.data.frame(figures) || nrow(figures) == 0) {
    return(character())
  }

  unlist(lapply(seq_len(nrow(figures)), function(i) {
    row <- figures[i, , drop = FALSE]
    interpretation <- bslr_text_value(row$interpretation %||% "", "")
    layout <- bslr_text_value(row$layout %||% "single", "single")
    env <- if (identical(layout, "full")) "figure*" else "figure"
    width <- if (identical(layout, "full")) "\\textwidth" else "\\columnwidth"
    latex_path <- bslr_text_value(row$latex_relative_path %||% row$relative_path, row$relative_path)
    latex_caption <- bslr_ensure_sentence(row$caption)
    if (nzchar(interpretation)) {
      latex_caption <- paste(latex_caption, bslr_ensure_sentence(interpretation))
    }
    c(
      paste0("## ", row$exhibit_id),
      "",
      bslr_ensure_sentence(row$caption),
      "",
      "::: {.content-visible when-format=\"pdf\"}",
      paste0("\\begin{", env, "}[!t]"),
      "\\centering",
      paste0("\\includegraphics[width=", width, "]{", bslr_latex_graphic_path(latex_path), "}"),
      paste0("\\caption{", m0_escape_latex(latex_caption), "}"),
      paste0("\\label{fig:", bslr_latex_label_id(row$exhibit_id), "}"),
      paste0("\\end{", env, "}"),
      ":::",
      "",
      "::: {.content-visible unless-format=\"pdf\"}",
      paste0(
        "![",
        row$caption,
        "](",
        row$relative_path,
        "){#",
        row$exhibit_id,
        " fig-pos='H' width='",
        if (identical(layout, "full")) "100%" else "56%",
        "'}"
      ),
      ":::",
      "",
      if (nzchar(interpretation)) c(paste0("*Interpretation.* ", bslr_ensure_sentence(interpretation)), "") else character()
    )
  }), use.names = FALSE)
}

bslr_build_table_blocks <- function(tables) {
  if (!is.data.frame(tables) || nrow(tables) == 0) {
    return(character())
  }

  unlist(lapply(seq_len(nrow(tables)), function(i) {
    row <- tables[i, , drop = FALSE]
    tab <- bslr_read_table_artifact(row$absolute_path)
    n_rows <- if (is.data.frame(tab)) nrow(tab) else 0L
    c(
      paste0("## ", row$exhibit_id),
      "",
      bslr_ensure_sentence(row$caption),
      "",
      paste0("Artifact format: CSV. Reported rows: ", n_rows, "."),
      "",
      paste0("Full table attached at `", row$relative_path, "`."),
      ""
    )
  }), use.names = FALSE)
}

bslr_read_table_artifact <- function(path) {
  withCallingHandlers(
    tryCatch(
      utils::read.csv(
        path,
        stringsAsFactors = FALSE,
        encoding = "UTF-8",
        check.names = FALSE,
        fill = TRUE,
        comment.char = ""
      ),
      error = function(e) data.frame()
    ),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("EOF within quoted string|number of items read is not a multiple|items le.dos no es m.ltiplo", msg, ignore.case = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

bslr_markdown_table <- function(tab, caption, label, max_rows = 8L) {
  if (!is.data.frame(tab) || nrow(tab) == 0) {
    return(c(paste0("Table: ", caption, " {#", label, "}"), "", "| Status |", "| --- |", "| No rows available |"))
  }

  tab <- utils::head(tab, max_rows)
  tab[] <- lapply(tab, function(col) {
    out <- as.character(col)
    out[is.na(out)] <- ""
    out
  })

  header <- paste0("| ", paste(names(tab), collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", ncol(tab)), collapse = " | "), " |")
  body <- apply(tab, 1, function(row) paste0("| ", paste(row, collapse = " | "), " |"))

  c(
    paste0("Table: ", caption, " {#", label, "}"),
    "",
    header,
    sep,
    body
  )
}

bslr_build_ieee_exhibit_lines <- function(inventory) {
  if (!is.data.frame(inventory) || nrow(inventory) == 0) {
    return(character())
  }
  paste0("\\begin{itemize}", paste0("\\item ", m0_escape_latex(inventory$caption), " [", inventory$type, ", ", inventory$module_id, "]", collapse = ""), "\\end{itemize}")
}

bslr_find_quarto_bin <- function() {
  quarto_bin <- Sys.which("quarto")
  if (nzchar(quarto_bin) && file.exists(quarto_bin)) {
    return(quarto_bin)
  }

  candidates <- c(
    "C:/Program Files/Quarto/bin/quarto.exe",
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe"
  )
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) > 0) {
    return(candidates[1])
  }

  ""
}

bslr_render_quarto_bundle <- function(manuscript_qmd, appendix_qmd, config, render = TRUE) {
  quarto_bin <- bslr_find_quarto_bin()
  if (!isTRUE(render)) {
    return(list(rendered = character(), notes = "Rendering skipped by request.", errors = character()))
  }
  if (!nzchar(quarto_bin)) {
    return(list(rendered = character(), notes = "Quarto executable not found. Source files were generated only.", errors = character()))
  }

  requested_targets <- unique(as.character(config$paper_render_targets %||% character()))
  requested_targets <- requested_targets[nzchar(requested_targets)]
  requested_targets <- tolower(requested_targets)
  requested_targets <- requested_targets[requested_targets %in% c("html", "pdf")]
  if (length(requested_targets) == 0) {
    requested_targets <- if (identical(config$report_format, "quarto_pdf")) {
      "pdf"
    } else if (identical(config$report_format, "quarto_html")) {
      "html"
    } else {
      character()
    }
  }
  if (length(requested_targets) == 0) {
    return(list(rendered = character(), notes = "No Quarto render targets requested; source bundle only.", errors = character()))
  }

  rendered <- character()
  errors <- character()
  notes <- character()

  for (format in requested_targets) {
    render_paths <- c(manuscript_qmd, appendix_qmd)
    if (identical(format, "pdf") && !isTRUE(config$paper_render_appendix_pdf %||% TRUE)) {
      render_paths <- manuscript_qmd
      notes <- c(notes, "Appendix PDF rendering skipped by paper_render_appendix_pdf = FALSE.")
    }
    for (path in render_paths) {
      cmd <- c("render", basename(path), "--to", format)
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(dirname(path))
      status <- tryCatch(
        system2(quarto_bin, cmd, stdout = FALSE, stderr = FALSE, wait = TRUE),
        error = function(e) {
          errors <<- c(errors, sprintf("Quarto render failed for %s (%s): %s", basename(path), format, e$message))
          1L
        }
      )
      setwd(old_wd)
      if (identical(status, 0L)) {
        out_ext <- if (identical(format, "pdf")) "pdf" else "html"
        rendered <- c(rendered, file.path(dirname(path), paste0(tools::file_path_sans_ext(basename(path)), ".", out_ext)))
        notes <- c(notes, sprintf("Rendered %s as %s.", basename(path), format))
      } else if (status != 0L) {
        errors <- c(errors, sprintf("Quarto render returned exit code %s for %s (%s).", status, basename(path), format))
      }
    }
  }

  list(
    rendered = rendered,
    notes = if (length(notes) > 0) paste(notes, collapse = " ") else "Quarto render attempted but did not produce outputs.",
    errors = errors
  )
}

bslr_clean_report_lines <- function(lines) {
  lines <- lines %||% character()
  lines <- as.character(lines)
  lines <- trimws(lines)
  lines <- lines[nzchar(trimws(lines))]
  lines
}

bslr_markdown_bullets <- function(lines, max_lines = 20L) {
  lines <- bslr_clean_report_lines(lines)
  if (length(lines) == 0) {
    return(character())
  }
  lines <- utils::head(lines, max_lines)
  paste0("- ", gsub("^[-*]\\s*", "", lines))
}

bslr_compact_module_report <- function(module_result) {
  if (!inherits(module_result, "biblio_module_result")) {
    return(character())
  }
  reports <- module_result$artifacts$reports %||% list()
  fallback_reports <- Filter(function(x) is.list(x) && length(x$lines %||% character()) > 0, reports)
  primary_report <- reports$report %||% reports$main %||%
    if (length(fallback_reports) > 0) fallback_reports[[1]] else list(lines = character())
  report_lines <- primary_report$lines %||% character()
  report_lines <- bslr_clean_report_lines(report_lines)
  report_lines <- report_lines[!grepl("^=+$", report_lines)]
  report_lines <- report_lines[!grepl("^Generated:", report_lines)]
  report_lines
}

bslr_result_null_lines <- function() {
  character()
}

bslr_result_placeholder_protocol_validation <- function() {
  list(
    ok = TRUE,
    completeness = 1,
    missing_required = character(),
    missing_human_review = character(),
    missing_journal_grade = character(),
    required_actions = character(),
    human_gate_ready = TRUE,
    journal_ready = TRUE,
    maturity_band = "journal_protocol_ready"
  )
}
