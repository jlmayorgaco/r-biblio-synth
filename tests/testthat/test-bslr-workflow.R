# ============================================================================
# test-bslr-workflow.R - Tests for the B-SLR methodology layer
# ============================================================================

test_that("bslr protocol template exposes the expected structure", {
  protocol <- bslr_protocol_template("My Study")

  expect_type(protocol, "list")
  expect_equal(protocol$title, "My Study")
  expect_true("search" %in% names(protocol))
  expect_true("screening" %in% names(protocol))
  expect_true("bibliometric" %in% names(protocol))
  expect_true("theorising" %in% names(protocol))
})

test_that("validate_bslr_protocol reports missing human-defined fields", {
  validation <- validate_bslr_protocol(bslr_protocol_template("Incomplete"))

  expect_false(validation$ok)
  expect_true(length(validation$missing_required) > 0)
  expect_true("research_question" %in% validation$missing_required)
  expect_true("overview_scan_date" %in% validation$missing_journal_grade)
  expect_true(validation$maturity_band %in% c("protocol_incomplete", "core_protocol_ready_human_review_incomplete", "full_review_ready_journal_enrichment_incomplete", "journal_protocol_ready"))
})

test_that("run_bslr orchestrates M0-M3 and builds methodology artifacts", {
  skip_if_not_installed("pkgload")

  fixture <- make_m1_rich_fixture()
  source_file <- tempfile(fileext = ".csv")
  utils::write.csv(fixture, source_file, row.names = FALSE)

  protocol <- bslr_protocol_template("Workflow test")
  protocol$review_topic <- "Power systems frequency estimation"
  protocol$research_question <- "How has the field evolved?"
  protocol$inclusion_criteria <- c("Peer-reviewed", "Relevant to topic")
  protocol$exclusion_criteria <- c("Out of scope")
  protocol$search$primary_database <- "scopus"
  protocol$search$secondary_databases <- c("wos")
  protocol$search$queries$primary <- "TITLE-ABS-KEY(freq estim*)"
  protocol$search$search_string_validation <- "Validated against pilot records."
  protocol$search$database_rationale <- "Scopus as primary source; WoS for cross-checking."
  protocol$search$first_run_date <- "2026-04-28"
  protocol$search$crosscheck_date <- "2026-04-29"
  protocol$search$time_span$start <- "2000"
  protocol$search$time_span$end <- "2025"
  protocol$overview_scan_date <- "2026-04-20"
  protocol$objectives <- c("Map the field", "Identify thematic clusters")
  protocol$screening$reviewers <- c("Reviewer A", "Reviewer B")
  protocol$screening$screening_tool <- "Rayyan"
  protocol$screening$quality_tool <- "MMAT"
  protocol$bibliometric$cluster_labeling_process <- "Independent reading of top-ranked documents followed by consensus labels."
  protocol$bibliometric$sample_selection_criteria <- "Hybrid ranking using citations and within-cluster centrality."
  protocol$theorising$perimeter <- "Research trends and thematic structure."
  protocol$theorising$rationale <- "Combine bibliometric mapping with focused thematic synthesis."
  protocol$theorising$contribution_goal <- "Research agenda and conceptual framework."

  result <- run_bslr(
    sources = list(sample = list(file = source_file, db = "generic", format = "csv")),
    protocol = protocol,
    modules = c("m1", "m2", "m3"),
    config = biblio_config(validate_strict = TRUE, export = FALSE, verbose = FALSE),
    export = FALSE
  )

  expect_s3_class(result, "biblio_module_result")
  expect_true(result$status %in% c("success", "warning"))
  expect_equal(result$module_id, "bslr")
  expect_true(inherits(result$data$modules$m0, "biblio_module_result"))
  expect_equal(result$data$search_metadata$primary_database, "scopus")
  expect_gt(result$data$bibliometric_map$metrics$n_clusters %||% 0, 0)
  expect_gt(nrow(result$data$bibliometric_map$sample_selected %||% data.frame()), 0)
  expect_true(is.data.frame(result$artifacts$tables$checkpoint_summary))
  expect_true(is.data.frame(result$artifacts$tables$extraction_template))
  expect_true(is.data.frame(result$artifacts$tables$evidence_matrix))
  expect_true(is.data.frame(result$artifacts$tables$journal_grade_gates))
  expect_true(is.data.frame(result$artifacts$tables$human_action_plan))
  expect_true("manuscript" %in% names(result$artifacts$reports))
  expect_true("methods_template" %in% names(result$artifacts$reports))
  expect_true("journal_grade" %in% names(result$artifacts$reports))
  expect_true("journal_assessment" %in% names(result$data))

  m0_lines <- result$data$modules$m0$artifacts$reports$prisma_methodology$lines
  expect_true(any(grepl("Primary database", m0_lines, fixed = TRUE)))
  expect_true(any(grepl("Screening tool", result$artifacts$reports$methods$lines, fixed = TRUE)))
  expect_true(any(grepl("Methodological maturity", result$artifacts$reports$methods$lines, fixed = TRUE)))
  expect_true(any(grepl("Journal-grade readiness", result$artifacts$reports$methods$lines, fixed = TRUE)))
  expect_true(any(grepl("Copy-ready paragraphs", result$artifacts$reports$methods_template$lines, fixed = TRUE)))
  expect_true(any(grepl("Gate assessment", result$artifacts$reports$journal_grade$lines, fixed = TRUE)))
})
