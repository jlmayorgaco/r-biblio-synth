# ============================================================================
# test-module-m1-run.R - Tests for M1 orchestrator
# ============================================================================

context("module m1 - run")

test_that("run_m1 returns biblio_module_result", {
  fixture <- make_minimal_biblio_fixture()
  result <- run_m1(fixture, export = FALSE)
  expect_true(inherits(result, "biblio_module_result"))
})

test_that("result$data contains all expected slots", {
  fixture <- make_minimal_biblio_fixture()
  result <- run_m1(fixture, export = FALSE)
  expected <- c("overview", "doc_types", "authors", "citations",
                "countries", "sources", "keywords", "bradford")
  expect_true(all(expected %in% names(result$data)))
})

test_that("result$artifacts exists", {
  fixture <- make_minimal_biblio_fixture()
  result <- run_m1(fixture, export = FALSE)
  expect_true("artifacts" %in% names(result))
})

test_that("result$status is set", {
  fixture <- make_minimal_biblio_fixture()
  result <- run_m1(fixture, export = FALSE)
  expect_true(!is.null(result$status))
})
