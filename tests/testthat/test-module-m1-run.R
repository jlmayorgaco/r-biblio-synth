# ============================================================================
# test-module-m1-run.R - Tests for M1 orchestrator
# ============================================================================

context("module m1 - run")

test_that("run_m1 returns biblio_module_result on extended fixture", {
  fixture <- make_extended_biblio_fixture()
  result <- run_m1(fixture, export = FALSE)
  expect_true(inherits(result, "biblio_module_result"))
})

test_that("result$data contains all expected slots", {
  fixture <- make_extended_biblio_fixture()
  result <- run_m1(fixture, export = FALSE)
  expected <- c("overview", "doc_types", "authors", "citations",
                "countries", "sources", "keywords", "bradford")
  expect_true(all(expected %in% names(result$data)))
})

test_that("result$artifacts exists and has plots and tables", {
  fixture <- make_extended_biblio_fixture()
  result <- run_m1(fixture, export = FALSE)
  expect_true("artifacts" %in% names(result))
  expect_true("plots" %in% names(result$artifacts))
  expect_true("tables" %in% names(result$artifacts))
})

test_that("result$status is set", {
  fixture <- make_extended_biblio_fixture()
  result <- run_m1(fixture, export = FALSE)
  expect_true(!is.null(result$status))
  expect_equal(result$status, "success")
})

test_that("run_m1 works with export=TRUE", {
  fixture <- make_extended_biblio_fixture()
  tmp <- tempdir()
  cfg <- list(output_dir = file.path(tmp, "test_output"))
  result <- run_m1(fixture, config = cfg, export = TRUE)
  expect_true(inherits(result, "biblio_module_result"))
  expect_true("manifest" %in% names(result$artifacts))
})
