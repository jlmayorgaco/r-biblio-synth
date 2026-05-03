# ============================================================================
# main_journal_gate.R - End-to-end acceptance gate for the Scopus example
# ============================================================================

project_root <- local({
  path <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(path, "DESCRIPTION")) && dir.exists(file.path(path, "R"))) {
      break
    }
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Could not locate the RBiblioSynth project root from: ", getwd())
    }
    path <- parent
  }
  path
})

suppressPackageStartupMessages(library(pkgload))
suppressWarnings(
  suppressPackageStartupMessages(
    pkgload::load_all(project_root, helpers = FALSE, quiet = TRUE)
  )
)

example_dir <- file.path(project_root, "examples", "SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR")
output_dir <- file.path(example_dir, "results_acceptance_gate")
unlink(output_dir, recursive = TRUE, force = TRUE)

`%||%` <- function(a, b) if (!is.null(a)) a else b

muffle_benign_gate_warnings <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("EOF within quoted string|number of items read is not a multiple|n[úu]mero de items le[ií]dos no es m[úu]ltiplo", msg, ignore.case = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

build_demo_screening_ledger <- function(bib_data) {
  text_part <- function(x) {
    x <- as.character(x %||% "")
    x[is.na(x)] <- ""
    tolower(x)
  }

  title <- text_part(bib_data$TI)
  abstract <- text_part(bib_data$AB)
  author_kw <- text_part(bib_data$DE)
  index_kw <- text_part(bib_data$ID)
  doc_type <- text_part(bib_data$DT)
  language <- text_part(bib_data$LA)
  doi <- as.character(bib_data$DI %||% NA_character_)
  source <- as.character(bib_data$SO %||% NA_character_)
  year <- suppressWarnings(as.integer(bib_data$PY))
  text_blob <- paste(title, abstract, author_kw, index_kw)

  has_power_context <- grepl("\\b(power system|power systems|electric power|grid|microgrid|load frequency|frequency deviation|phasor|pmu|inverter|generator|distribution system|transmission system)\\b", text_blob)
  has_estimation_focus <- grepl("\\b(frequency estimat|frequency estimator|frequency measurement|frequency detector|frequency tracking|frequency monitor|rocof|dft|windowed dft|kalman|observer|synchrophasor|phase-locked loop|pll)\\b", text_blob)
  off_topic_context <- grepl("\\b(underwater|acoustic communication|orthogonal chirp|mass?\\b|robot|vehicle charger|satellite|wireless|speech|image|video|biomedical|consensus control|dos attacks?|markovian)\\b", text_blob)
  control_only <- grepl("\\b(load frequency control|consensus control|sliding mode control|predictive control|observer-based control)\\b", text_blob) &
    !grepl("\\b(frequency estimat|frequency measurement|rocof|pmu|phasor|dft|pll)\\b", text_blob)

  allowed_doc_type <- grepl("article|review|conference", doc_type)
  allowed_language <- !nzchar(language) | grepl("english", language)
  metadata_ok <- nzchar(title) & !is.na(year)

  screen_include <- metadata_ok & allowed_doc_type & allowed_language & has_power_context & has_estimation_focus & !off_topic_context
  screen_reason <- ifelse(
    !metadata_ok, "Missing bibliographic metadata",
    ifelse(!allowed_doc_type, "Unsupported document type",
      ifelse(!allowed_language, "Non-English document",
        ifelse(off_topic_context | !has_power_context, "Non-power-system context",
          ifelse(!has_estimation_focus, "No explicit frequency-estimation focus", "")
        )
      )
    )
  )

  strong_method_signal <- grepl("\\b(dft|windowed dft|iterative|kalman|observer|synchrophasor|pmu|pll|goertzel|wavelet|fourier|prony|least squares|state estimation)\\b", text_blob)
  peripheral_focus <- grepl("\\b(stability control|consensus|communications|synchronization of|timing synchronization|event-triggered|load frequency control)\\b", text_blob) &
    !grepl("\\b(frequency estimat|frequency measurement|rocof|phasor|pmu|dft|pll)\\b", text_blob)
  eligibility_include <- screen_include & strong_method_signal & !peripheral_focus
  eligibility_reason <- ifelse(
    !screen_include, "Excluded at screening",
    ifelse(!strong_method_signal, "Insufficient methodological detail for synthesis",
      ifelse(peripheral_focus, "Peripheral frequency/control focus outside review perimeter", "")
    )
  )

  screen_marginal <- screen_include & (grepl("\\bcontrol\\b", text_blob) | grepl("\\bobserver\\b", text_blob))
  elig_marginal <- eligibility_include & grepl("\\b(control|observer|adaptive)\\b", text_blob)
  reviewer_b_screen <- ifelse(screen_marginal & (bib_data$M0_DOC_ID %% 11L == 0L), "exclude", ifelse(screen_include, "include", "exclude"))
  reviewer_b_elig <- ifelse(elig_marginal & (bib_data$M0_DOC_ID %% 13L == 0L), "exclude", ifelse(eligibility_include, "include", "exclude"))

  make_stage_rows <- function(stage, reviewer, decision, reason, final = FALSE, status = "submitted") {
    data.frame(
      M0_DOC_ID = bib_data$M0_DOC_ID,
      title = bib_data$TI,
      doi = doi,
      year = year,
      source = source,
      stage = stage,
      reviewer = reviewer,
      decision = decision,
      reason = ifelse(decision == "exclude", reason, NA_character_),
      notes = NA_character_,
      decision_date = as.character(Sys.Date()),
      is_final = final,
      status = status,
      stringsAsFactors = FALSE
    )
  }

  screening_a <- make_stage_rows("screening", "Reviewer A", ifelse(screen_include, "include", "exclude"), screen_reason)
  screening_b <- make_stage_rows("screening", "Reviewer B", reviewer_b_screen, screen_reason)
  screening_final_decision <- ifelse(screen_include, "include", "exclude")
  screening_conflict <- screening_a$decision != screening_b$decision
  screening_adj <- make_stage_rows("screening", "Adjudicator", screening_final_decision, screen_reason, final = TRUE, status = "adjudicated")
  screening_adj <- screening_adj[screening_conflict, , drop = FALSE]

  included_after_screen <- screen_include
  eligibility_a <- make_stage_rows("eligibility", "Reviewer A", ifelse(eligibility_include, "include", "exclude"), eligibility_reason)
  eligibility_b <- make_stage_rows("eligibility", "Reviewer B", reviewer_b_elig, eligibility_reason)
  eligibility_a <- eligibility_a[included_after_screen, , drop = FALSE]
  eligibility_b <- eligibility_b[included_after_screen, , drop = FALSE]
  elig_conflict <- eligibility_a$decision != eligibility_b$decision
  eligibility_adj <- make_stage_rows("eligibility", "Adjudicator", ifelse(eligibility_include, "include", "exclude"), eligibility_reason, final = TRUE, status = "adjudicated")
  eligibility_adj <- eligibility_adj[included_after_screen, , drop = FALSE]
  eligibility_adj <- eligibility_adj[elig_conflict, , drop = FALSE]

  final_included <- eligibility_include[included_after_screen]
  included_rows <- make_stage_rows("included", "Adjudicator", ifelse(eligibility_include, "include", "exclude"), eligibility_reason, final = TRUE, status = "final")
  included_rows <- included_rows[eligibility_include, , drop = FALSE]

  ledger <- dplyr::bind_rows(
    screening_a,
    screening_b,
    screening_adj,
    eligibility_a,
    eligibility_b,
    eligibility_adj,
    included_rows
  )

  ledger$decision_date <- as.character(as.Date("2026-04-29") + ((seq_len(nrow(ledger)) - 1L) %% 7L))
  ledger
}

protocol <- bslr_protocol_template("Power Systems Frequency Estimation B-SLR")
protocol$review_topic <- "power systems frequency estimation"
protocol$overview_scan_date <- as.character(Sys.Date())
protocol$research_gap <- "An updated, integrated bibliometric-systematic synthesis of power-systems frequency estimation remains fragmented across methods, applications, and geographies."
protocol$research_question <- "How has research on power-systems frequency estimation evolved in terms of themes, temporal dynamics, and geographical structure?"
protocol$objectives <- c(
  "Map the descriptive structure of the corpus.",
  "Quantify the temporal evolution and forecasting profile of the field.",
  "Identify geographical leadership, collaboration, and comparative positioning."
)
protocol$inclusion_criteria <- c(
  "English-language scholarly documents focused on power-systems frequency estimation.",
  "Documents indexed in Scopus or Web of Science and returned by the validated search string.",
  "Records with sufficient bibliographic metadata for automated analysis."
)
protocol$exclusion_criteria <- c(
  "Records outside electric power-system frequency estimation.",
  "Non-scholarly items or records without usable bibliographic metadata.",
  "Duplicates retained after export harmonization."
)
protocol$search$primary_database <- "scopus"
protocol$search$secondary_databases <- c("web of science")
protocol$search$queries$primary <- 'TITLE-ABS-KEY(("power system*" OR grid OR microgrid) AND ("frequency estimation" OR "frequency estimator"))'
protocol$search$search_string_validation <- "Validated through iterative informal scanning of representative frequency-estimation papers and term variants."
protocol$search$database_rationale <- "Scopus and Web of Science were combined to improve engineering and energy coverage while retaining cross-database deduplication."
protocol$search$first_run_date <- as.character(Sys.Date())
protocol$search$crosscheck_date <- as.character(Sys.Date())
protocol$search$time_span$start <- 1970
protocol$search$time_span$end <- 2026
protocol$search$language <- "English"
protocol$search$document_types <- c("article", "review", "conference paper")
protocol$screening$reviewers <- c("Reviewer A", "Reviewer B")
protocol$screening$screening_tool <- "RBiblioSynth screening ledger workflow"
protocol$screening$quality_tool <- "Protocol-defined eligibility assessment"
protocol$screening$agreement_target <- 0.80
protocol$data_extraction$fields <- c("aim", "method", "application_context", "signal_model", "validation_strategy", "key_findings", "limitations", "contribution")
protocol$bibliometric$approach <- "bibliographic_coupling"
protocol$bibliometric$secondary_approach <- "co_citation"
protocol$bibliometric$min_cluster_size <- 5L
protocol$bibliometric$resolution <- 1
protocol$bibliometric$ranking_rule <- "hybrid"
protocol$bibliometric$cluster_labeling_process <- "Representative papers are read independently and cluster labels are assigned through consensus."
protocol$bibliometric$sample_selection_criteria <- "Representative papers are prioritised through hybrid ranking using normalized citations and within-cluster centrality."
protocol$slr$holistic_questions <- c(
  "What are the dominant methodological streams in power-systems frequency estimation?",
  "How has the field shifted across time, applications, and validation strategies?"
)
protocol$slr$cluster_questions <- c(
  "What problem does each cluster address?",
  "Which signal models, algorithms, and evaluation settings dominate each cluster?"
)
protocol$theorising$perimeter <- "The theorising perimeter focuses on methodological categories, application domains, and future research opportunities in frequency estimation."
protocol$theorising$rationale <- "The synthesis aims to turn descriptive and structural evidence into a research agenda and conceptual map."
protocol$theorising$contribution_goal <- "Produce a journal-oriented bibliometric-systematic review scaffold with a defensible research agenda."

data_dir <- file.path(example_dir, "data")
wos_files <- sort(Sys.glob(file.path(data_dir, "wos_*.txt")))
if (length(wos_files) == 0L) {
  stop("No Web of Science files found at: ", file.path(data_dir, "wos_*.txt"))
}

sources <- list(
  scopus = list(
    file = file.path(data_dir, "scopus.bib"),
    db = "scopus",
    format = "bibtex"
  ),
  wos = list(
    files = wos_files,
    db = "wos",
    format = "plaintext"
  )
)

config <- biblio_config(
  output_dir = output_dir,
  export_plots = TRUE,
  export_json = TRUE,
  export_reports = TRUE,
  report_format = "latex_bundle",
  paper_render_targets = c("html", "pdf"),
  paper_render_appendix_pdf = FALSE,
  verbose = FALSE
)

preflight_m0 <- muffle_benign_gate_warnings(
  run_m0(
    sources = sources,
    config = config,
    search_metadata = bslr_protocol_to_search_metadata(protocol),
    export = FALSE
  )
)
screening_ledger <- build_demo_screening_ledger(m0_get_bib_data(preflight_m0))
screening_ledger_path <- file.path(output_dir, "bslr_screening_ledger_example.csv")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(screening_ledger, screening_ledger_path, row.names = FALSE, na = "")

result <- muffle_benign_gate_warnings(
  run_bslr(
    sources = sources,
    protocol = protocol,
    screening_ledger = screening_ledger_path,
    prisma_spec = "auto",
    modules = c("m1", "m2", "m3"),
    config = config,
    export = TRUE
  )
)

paper_dir <- file.path(output_dir, "bslr", "paper")
snapshot <- list(
  generated_at = as.character(Sys.time()),
  result_status = result$status,
  methodological_mode = result$data$modules$m0$data$screening_summary$methodological_mode %||% "unknown",
  module_statuses = vapply(result$data$modules, function(x) x$status %||% "unknown", character(1)),
  paper_bundle = list(
    manuscript_qmd = file.path(paper_dir, "bslr_manuscript.qmd"),
    appendix_qmd = file.path(paper_dir, "bslr_appendix.qmd"),
    ieee_tex = file.path(paper_dir, "bslr_ieee_manuscript.tex"),
    manuscript_html = file.path(paper_dir, "bslr_manuscript.html"),
    appendix_html = file.path(paper_dir, "bslr_appendix.html"),
    manuscript_pdf = file.path(paper_dir, "bslr_manuscript.pdf"),
    appendix_pdf = file.path(paper_dir, "bslr_appendix.pdf"),
    exhibit_inventory_csv = file.path(paper_dir, "bslr_exhibit_inventory.csv"),
    manifest_json = file.path(paper_dir, "bslr_paper_manifest.json")
  ),
  key_reports = list(
    m0_prisma = file.path(output_dir, "m0", "reports", "m0_prisma_report.txt"),
    m1 = file.path(output_dir, "m1", "reports", "m1_report.txt"),
    m2 = file.path(output_dir, "m2", "reports", "m2_report.txt"),
    m3 = file.path(output_dir, "m3", "reports", "m3_report.txt"),
    bslr_methods = file.path(output_dir, "bslr", "reports", "bslr_methods.txt"),
    bslr_journal_grade = file.path(output_dir, "bslr", "reports", "bslr_journal_grade.txt")
  ),
  key_figures = list(
    prisma = file.path(paper_dir, "figures", "fig_prisma.png"),
    m1_overview = file.path(paper_dir, "figures", "fig_m1_overview.png"),
    m2_growth = file.path(paper_dir, "figures", "fig_m2_growth.png"),
    m2_validation = file.path(paper_dir, "figures", "fig_m2_validation.png"),
    m2_forecast = file.path(paper_dir, "figures", "fig_m2_forecast.png"),
    m3_concentration = file.path(paper_dir, "figures", "fig_m3_concentration.png"),
    m3_trajectories = file.path(paper_dir, "figures", "fig_m3_trajectories.png"),
    m3_quadrant = file.path(paper_dir, "figures", "fig_m3_quadrant.png"),
    m3_share_trends = file.path(paper_dir, "figures", "fig_m3_share_trends.png")
  )
)

snapshot$checks <- list(
  paper_bundle_complete = all(vapply(snapshot$paper_bundle[c(
    "manuscript_qmd",
    "appendix_qmd",
    "ieee_tex",
    "manuscript_html",
    "appendix_html",
    "manuscript_pdf",
    "exhibit_inventory_csv",
    "manifest_json"
  )], file.exists, logical(1))),
  appendix_pdf_present = file.exists(snapshot$paper_bundle$appendix_pdf),
  key_reports_present = all(vapply(snapshot$key_reports, file.exists, logical(1))),
  key_figures_present = all(vapply(snapshot$key_figures, file.exists, logical(1))),
  screening_ledger_present = file.exists(screening_ledger_path)
)

snapshot_json <- file.path(output_dir, "acceptance_snapshot.json")
snapshot_txt <- file.path(output_dir, "acceptance_snapshot.txt")
jsonlite::write_json(snapshot, snapshot_json, auto_unbox = TRUE, pretty = TRUE)
writeLines(c(
  "RBiblioSynth Acceptance Snapshot",
  paste0("Generated: ", snapshot$generated_at),
  paste0("Result status: ", snapshot$result_status),
  paste0("Methodological mode: ", snapshot$methodological_mode),
  paste0("Module statuses: ", paste(names(snapshot$module_statuses), snapshot$module_statuses, sep = "=", collapse = "; ")),
  paste0("Paper bundle complete: ", snapshot$checks$paper_bundle_complete),
  paste0("Appendix PDF present: ", snapshot$checks$appendix_pdf_present),
  paste0("Key reports present: ", snapshot$checks$key_reports_present),
  paste0("Key figures present: ", snapshot$checks$key_figures_present),
  paste0("Screening ledger present: ", snapshot$checks$screening_ledger_present)
), snapshot_txt)

cat(paste0("ACCEPTANCE_STATUS=", snapshot$result_status, "\n"))
cat(paste0("ACCEPTANCE_MODE=", snapshot$methodological_mode, "\n"))
cat(paste0("ACCEPTANCE_PAPER_BUNDLE=", snapshot$checks$paper_bundle_complete, "\n"))
cat(paste0("ACCEPTANCE_REPORTS=", snapshot$checks$key_reports_present, "\n"))
cat(paste0("ACCEPTANCE_FIGURES=", snapshot$checks$key_figures_present, "\n"))
cat(paste0("ACCEPTANCE_SNAPSHOT=", snapshot_json, "\n"))
