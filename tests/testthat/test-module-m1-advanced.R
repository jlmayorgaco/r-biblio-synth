# ============================================================================
# test-module-m1-advanced.R - Advanced integration tests for M1
# ============================================================================

test_that("run_m1 exposes advanced M1 sections coherently on rich fixture", {
  fixture <- make_m1_rich_fixture()
  result <- run_m1(fixture, export = FALSE)

  expect_equal(result$data$topic_modeling$status, "success")
  expect_equal(result$data$citation_analysis$status, "success")
  expect_equal(result$data$author_career$status, "success")
  expect_equal(result$data$hypotheses$status, "success")

  expect_equal(result$artifacts$tables$topic_modeling$status, "success")
  expect_equal(result$artifacts$tables$citation_analysis$status, "success")
  expect_equal(result$artifacts$tables$author_career$status, "success")
  expect_equal(result$artifacts$tables$hypotheses$status, "success")

  expect_true(nrow(result$artifacts$tables$topic_modeling$table) > 0)
  expect_true(nrow(result$artifacts$tables$citation_analysis$table) > 0)
  expect_true(nrow(result$artifacts$tables$author_career$table) > 0)
  expect_true(nrow(result$artifacts$tables$hypotheses$table) > 0)

  expect_true("summary" %in% names(result$artifacts$plots$hypotheses$plots))
  expect_true("distribution_fit" %in% names(result$artifacts$plots$citation_analysis$plots))
  expect_true("topic_words" %in% names(result$artifacts$plots$topic_modeling$plots))
})

test_that("M1 report includes advanced sections on rich fixture", {
  fixture <- make_m1_rich_fixture()
  result <- run_m1(fixture, export = FALSE)

  report <- result$artifacts$reports[[1]]
  expected_sections <- c(
    "author_indices", "citation_analysis", "keyword_structure",
    "collaboration", "lotka", "price_law", "topic_modeling",
    "author_career", "hypotheses"
  )

  expect_true(all(expected_sections %in% report$sections))
  expect_true(any(grepl("Topic Model", report$lines, fixed = TRUE)))
  expect_true(any(grepl("Hypotheses tested", report$lines, fixed = TRUE)))
})

test_that("M1 produces journal-grade labels for citations, keywords, countries, and topics", {
  fixture <- make_m1_rich_fixture()
  result <- run_m1(fixture, export = FALSE)

  citations <- result$data$citations$top_cited_documents
  keywords <- result$data$keywords$top_keywords
  countries <- result$data$countries$top_countries_by_articles
  topics <- result$data$topic_modeling$topics

  expect_true(all(!grepl("^\\[[0-9]+\\]$", citations$label)))
  expect_true(any(grepl("Advanced study", citations$label, fixed = TRUE)))
  expect_true(all(!grepl("_", topics$label, fixed = TRUE)))
  expect_true(all(!grepl("^term[0-9]+$", tolower(topics$label))))
  expect_true(all(!grepl("^[A-Z]{2}$", countries$label)))
  expect_true(all(!grepl("_", keywords$label, fixed = TRUE)))
})

test_that("M1 author-career outputs use publication-grade author labels", {
  fixture <- make_m1_rich_fixture()
  result <- run_m1(fixture, export = FALSE)

  top_h <- result$data$author_career$top_by_h_index
  report <- result$artifacts$reports[[1]]

  expect_true("display_author" %in% names(top_h))
  expect_true(any(grepl("^Patel N$", top_h$display_author)))
  expect_false(any(grepl("^[A-Z],\\s*[A-Z]$", top_h$display_author)))
  expect_true(any(grepl("Patel N", report$lines, fixed = TRUE)))
})

test_that("M1 export manifest tracks multi-format plots", {
  fixture <- make_m1_rich_fixture()
  tmp <- tempdir()
  cfg <- list(output_dir = file.path(tmp, "m1_advanced_output"))
  result <- run_m1(fixture, config = cfg, export = TRUE)

  expect_true(any(grepl("\\.png$", result$artifacts$manifest$plots)))
  expect_true(any(grepl("\\.pdf$", result$artifacts$manifest$plots)))
  expect_true(any(grepl("\\.csv$", result$artifacts$manifest$tables)))
})
