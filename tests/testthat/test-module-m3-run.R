# ============================================================================
# test-module-m3-run.R
# ============================================================================
test_that("run_m3 returns a biblio_module_result on minimal fixture", {
  result <- run_m3(make_m3_minimal_fixture(), export = FALSE)
  expect_s3_class(result, "biblio_module_result")
})

test_that("run_m3 module_id is m3", {
  result <- run_m3(make_m3_minimal_fixture(), export = FALSE)
  expect_equal(result$module_id, "m3")
})

test_that("run_m3 data slot contains all expected components", {
  result <- run_m3(make_m3_minimal_fixture(), export = FALSE)
  expect_true(all(c("production", "citations", "scp_mcp", "inequality",
                    "rankings", "distribution_tests", "growth_dynamics",
                    "profiles", "experiments") %in% names(result$data)))
})

test_that("run_m3 artifacts contain plots and tables", {
  result <- run_m3(make_m3_minimal_fixture(), export = FALSE)
  expect_true(is.list(result$artifacts$plots))
  expect_true(is.list(result$artifacts$tables))
})

test_that("run_m3 attaches a report", {
  result <- run_m3(make_m3_minimal_fixture(), export = FALSE)
  expect_true(length(result$artifacts$reports) > 0)
})

test_that("run_m3 works on extended fixture without error", {
  result <- run_m3(make_m3_extended_fixture(), export = FALSE)
  expect_s3_class(result, "biblio_module_result")
  expect_true(result$status %in% c("success", "warning"))
})

test_that("run_m3 with export = TRUE writes to tempdir", {
  tmp_dir <- file.path(tempdir(), paste0("m3_test_", as.integer(Sys.time())))
  cfg     <- biblio_config()
  cfg$output_dir <- tmp_dir

  expect_no_error(
    run_m3(make_m3_minimal_fixture(), config = cfg, export = TRUE)
  )
})
