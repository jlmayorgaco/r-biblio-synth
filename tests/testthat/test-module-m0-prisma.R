# ============================================================================
# test-module-m0-prisma.R - Tests for M0 PRISMA and export workflow
# ============================================================================

test_that("export_plot_artifact exports recorded base-R plots", {
  export_plot <- get("export_plot_artifact", envir = asNamespace("RBiblioSynth"))
  tmp_base <- tempfile("prisma_plot_")

  grDevices::pdf(NULL)
  plot(1:5, 1:5, type = "b")
  recorded <- grDevices::recordPlot()
  grDevices::dev.off()

  expect_no_error(export_plot(recorded, tmp_base, width = 4, height = 4, dpi = 96))
  expect_true(file.exists(paste0(tmp_base, ".png")))
  expect_true(file.exists(paste0(tmp_base, ".svg")))
  expect_true(file.exists(paste0(tmp_base, ".pdf")))
})

test_that("run_m0 auto-builds PRISMA counts and methodology from partial spec", {
  run_m0_ns <- get("run_m0", envir = asNamespace("RBiblioSynth"))
  df_a <- data.frame(
    AU = c("Smith J", "Jones M", "Brown A"),
    TI = c("Paper A", "Paper B", "Paper C"),
    PY = c(2020, 2021, 2022),
    SO = c("Journal A", "Journal B", "Journal C"),
    DI = c("10.1000/a", "10.1000/b", "10.1000/c"),
    DT = c("Article", "Article", "Review"),
    TC = c(5, 10, 3),
    stringsAsFactors = FALSE
  )
  df_b <- data.frame(
    AU = c("Smith J", "Taylor R", "Clark P"),
    TI = c("Paper A Duplicate", "Paper D", "Paper E"),
    PY = c(2020, 2023, 2024),
    SO = c("Journal A", "Journal D", "Journal E"),
    DI = c("10.1000/a", "10.1000/d", "10.1000/e"),
    DT = c("Article", "Conference Paper", "Article"),
    TC = c(5, 2, 1),
    stringsAsFactors = FALSE
  )

  file_a <- tempfile(fileext = ".csv")
  file_b <- tempfile(fileext = ".csv")
  on.exit(unlink(c(file_a, file_b)), add = TRUE)
  utils::write.csv(df_a, file_a, row.names = FALSE)
  utils::write.csv(df_b, file_b, row.names = FALSE)

  sources <- list(
    db_source = list(file = file_a, db = "generic", format = "csv", prisma_role = "database", query = "power systems"),
    other_source = list(file = file_b, db = "generic", format = "csv", prisma_role = "other")
  )

  prisma_partial <- list(
    title = "Auto PRISMA Test",
    screening = list(excluded_screening = 1),
    eligibility = list(
      excluded_fulltext = 1,
      excluded_reasons = list("Not relevant" = 1)
    )
  )

  result <- run_m0_ns(
    sources = sources,
    prisma_spec = prisma_partial,
    config = biblio_config(verbose = FALSE, cache_enabled = FALSE),
    export = FALSE
  )

  expect_s3_class(result, "biblio_module_result")
  expect_equal(result$data$prisma$identification$records_database, 3)
  expect_equal(result$data$prisma$identification$records_other, 3)
  expect_equal(result$data$prisma$identification$duplicates_removed, 1)
  expect_equal(result$data$prisma$screening$records_screened, 5)
  expect_equal(result$data$prisma$eligibility$fulltext_assessed, 4)
  expect_equal(result$data$prisma$included$studies_included, 3)
  expect_true(result$data$prisma_validation$ok)
  expect_true("provenance" %in% names(result$data$organized))
  expect_true(is.data.frame(result$data$source_summary))
  expect_true(is.data.frame(result$data$dedup_summary))
  expect_true("prisma_methodology" %in% names(result$artifacts$reports))
  expect_true(any(grepl("Deduplication removed 1 records", result$artifacts$reports$prisma_methodology$lines, fixed = TRUE)))
})

test_that("run_m0 exports PRISMA diagram and methodology with auto mode", {
  run_m0_ns <- get("run_m0", envir = asNamespace("RBiblioSynth"))
  df <- data.frame(
    AU = c("Smith J", "Jones M"),
    TI = c("Paper A", "Paper B"),
    PY = c(2020, 2021),
    SO = c("Journal A", "Journal B"),
    DI = c("10.1000/a", "10.1000/b"),
    DT = c("Article", "Review"),
    TC = c(5, 10),
    stringsAsFactors = FALSE
  )

  file_a <- tempfile(fileext = ".csv")
  out_dir <- tempfile("m0_prisma_results_")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(c(file_a, out_dir), recursive = TRUE, force = TRUE), add = TRUE)
  utils::write.csv(df, file_a, row.names = FALSE)

  result <- run_m0_ns(
    sources = list(src = list(file = file_a, db = "generic", format = "csv", prisma_role = "database")),
    prisma_spec = "auto",
    config = biblio_config(output_dir = out_dir, verbose = FALSE, cache_enabled = FALSE),
    export = TRUE
  )

  expect_true(file.exists(file.path(out_dir, "m0", "plots", "m0_prisma_diagram.png")))
  expect_true(file.exists(file.path(out_dir, "m0", "plots", "m0_prisma_diagram.svg")))
  expect_true(file.exists(file.path(out_dir, "m0", "plots", "m0_prisma_diagram.pdf")))
  expect_true(file.exists(file.path(out_dir, "m0", "reports", "m0_prisma_report.txt")))
  expect_true(file.exists(file.path(out_dir, "m0", "reports", "m0_prisma_methodology.txt")))
  expect_true(file.exists(file.path(out_dir, "m0", "reports", "m0_prisma_methodology.tex")))
  expect_true(file.exists(file.path(out_dir, "m0", "json", "m0_prisma.json")))
  expect_s3_class(result$artifacts$manifest, "biblio_artifact_manifest")
})

test_that("M0 builds screening templates and full-review artifacts from explicit decisions", {
  run_m0_ns <- get("run_m0", envir = asNamespace("RBiblioSynth"))
  template_ns <- get("m0_screening_ledger_template", envir = asNamespace("RBiblioSynth"))

  df <- data.frame(
    AU = c("Smith J", "Jones M"),
    TI = c("Paper A", "Paper B"),
    PY = c(2020, 2021),
    SO = c("Journal A", "Journal B"),
    DI = c("10.1000/a", "10.1000/b"),
    DT = c("Article", "Review"),
    TC = c(5, 10),
    stringsAsFactors = FALSE
  )

  screening_ledger <- data.frame(
    title = c("Paper A", "Paper A", "Paper B", "Paper B"),
    stage = c("screening", "screening", "screening", "screening"),
    reviewer = c("r1", "r2", "r1", "r2"),
    decision = c("include", "include", "exclude", "exclude"),
    reason = c(NA, NA, "Out of scope", "Out of scope"),
    is_final = c(TRUE, TRUE, TRUE, TRUE),
    status = c("final", "final", "final", "final"),
    stringsAsFactors = FALSE
  )

  file_a <- tempfile(fileext = ".csv")
  out_dir <- tempfile("m0_screening_results_")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(c(file_a, out_dir), recursive = TRUE, force = TRUE), add = TRUE)
  utils::write.csv(df, file_a, row.names = FALSE)

  result <- run_m0_ns(
    sources = list(src = list(file = file_a, db = "generic", format = "csv", prisma_role = "database")),
    prisma_spec = "auto",
    screening_ledger = screening_ledger,
    search_metadata = list(
      review_topic = "Test topic",
      research_question = "What survives screening?",
      reviewers = c("r1", "r2"),
      conflict_rule = "consensus",
      screening_tool = "Rayyan"
    ),
    config = biblio_config(output_dir = out_dir, verbose = FALSE, cache_enabled = FALSE),
    export = TRUE
  )

  template <- template_ns(result$data$bib_merged, reviewers = c("r1", "r2"))
  expect_true(all(c("M0_DOC_ID", "title", "doi", "stage", "reviewer", "decision", "reason", "decision_date") %in% names(template)))
  expect_equal(nrow(template), nrow(result$data$bib_merged) * 2 * 2)

  expect_true(is.data.frame(result$artifacts$tables$screening_template))
  expect_true(is.data.frame(result$artifacts$tables$screening_consensus))
  expect_true(any(grepl("Methodological mode: full-review", result$artifacts$reports$screening$lines, fixed = TRUE)))
  expect_true(any(grepl("Krippendorff", result$artifacts$reports$prisma_methodology$lines, fixed = TRUE)))
  expect_true(file.exists(file.path(out_dir, "m0", "tables", "m0_screening_template.csv")))
  expect_true(file.exists(file.path(out_dir, "m0", "tables", "m0_screening_consensus.csv")))
  expect_true(file.exists(file.path(out_dir, "m0", "tables", "m0_screening_stage_counts.csv")))
})

test_that("run_m0 builds rich organized M0 tables for downstream modules", {
  run_m0_ns <- get("run_m0", envir = asNamespace("RBiblioSynth"))
  df <- data.frame(
    AU = c("Smith J; Brown A", "Garcia M"),
    TI = c("Paper A", "Paper B"),
    PY = c(2020, 2021),
    SO = c("Journal A", "Journal B"),
    DI = c("10.1000/a", "10.1000/b"),
    DT = c("Article", "Review"),
    TC = c(5, 10),
    AU_CO = c("USA; UK", "Colombia"),
    C1 = c(
      "Department of Energy, University of Texas, Austin, USA; Imperial College London, London, UK",
      "Universidad Nacional de Colombia, Bogota, Colombia"
    ),
    AB = c(
      "This study evaluates forecasting methods for power systems.",
      "A systematic review of bibliometric workflows."
    ),
    DE = c("forecasting; power systems", "bibliometrics; prisma"),
    ID = c("time series; regression", "systematic review"),
    CR = c(
      "Smith J, 2018, IEEE Transactions on Power Systems, V33, P100-110, DOI 10.1000/ref1; Brown A, 2019, Energy, V44, P20-30",
      "Garcia M, 2017, Journal of Informetrics, V11, P40-50, DOI 10.1000/ref2"
    ),
    FX = c("Supported by National Science Foundation grant CCF1234567", ""),
    stringsAsFactors = FALSE
  )

  file_a <- tempfile(fileext = ".csv")
  on.exit(unlink(file_a), add = TRUE)
  utils::write.csv(df, file_a, row.names = FALSE)

  result <- run_m0_ns(
    sources = list(src = list(file = file_a, db = "generic", format = "csv", prisma_role = "database")),
    config = biblio_config(verbose = FALSE, cache_enabled = FALSE),
    export = FALSE
  )

  organized <- result$data$organized
  expect_true(all(c(
    "documents", "author_documents", "country_documents", "affiliations", "locations",
    "keywords_long", "abstracts", "references", "reference_stats", "funding"
  ) %in% names(organized)))

  expect_true(all(c("title", "year", "country", "n_keywords", "n_references", "status") %in% names(organized$documents)))
  expect_true(all(c("author", "author_position", "weight") %in% names(organized$author_documents)))
  expect_true(all(c("institution", "city", "country") %in% names(organized$affiliations)))
  expect_true(all(c("location", "article_count") %in% names(organized$locations)))
  expect_true(all(c("abstract_clean", "abstract_language", "abstract_word_count") %in% names(organized$abstracts)))
  expect_true(all(c("keyword", "keyword_type") %in% names(organized$keywords_long)))
  expect_true(all(c("reference_journal", "reference_doi", "citing_title") %in% names(organized$references)))
  expect_equal(nrow(organized$reference_stats), 1)
  expect_true(any(organized$funding$funding_type == "grant"))
})
