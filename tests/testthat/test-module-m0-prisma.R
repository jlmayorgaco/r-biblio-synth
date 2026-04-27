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
  expect_true(file.exists(file.path(out_dir, "m0", "reports", "m0_prisma_report.txt")))
  expect_true(file.exists(file.path(out_dir, "m0", "reports", "m0_prisma_methodology.txt")))
  expect_true(file.exists(file.path(out_dir, "m0", "reports", "m0_prisma_methodology.tex")))
  expect_true(file.exists(file.path(out_dir, "m0", "json", "m0_prisma.json")))
  expect_s3_class(result$artifacts$manifest, "biblio_artifact_manifest")
})
