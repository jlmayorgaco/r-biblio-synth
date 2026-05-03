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
                    "profiles", "experiments", "spatial", "regional",
                    "economic", "temporal_dynamics") %in% names(result$data)))
})

test_that("run_m3 artifacts contain plots and tables", {
  result <- run_m3(make_m3_minimal_fixture(), export = FALSE)
  expect_true(is.list(result$artifacts$plots))
  expect_true(is.list(result$artifacts$tables))
})

test_that("run_m3 attaches a report", {
  result <- run_m3(make_m3_minimal_fixture(), export = FALSE)
  expect_true(length(result$artifacts$reports) > 0)
  expect_true(length(result$artifacts$reports[[1]]$tex) > 0)
})

test_that("run_m3 works on extended fixture without error", {
  result <- run_m3(make_m3_extended_fixture(), export = FALSE)
  expect_s3_class(result, "biblio_module_result")
  expect_true(result$status %in% c("success", "warning"))
})

test_that("run_m3 regional and economic tables use canonical fields", {
  result <- run_m3(make_m3_extended_fixture(), export = FALSE)

  expect_true(all(c("region", "total_production", "share") %in% names(result$artifacts$tables$regional)))
  expect_true(all(c("country", "production", "gdp_usd", "hdi") %in% names(result$artifacts$tables$economic)))
})

test_that("run_m3 with export = TRUE writes to tempdir", {
  tmp_dir <- file.path(tempdir(), paste0("m3_test_", as.integer(Sys.time())))
  cfg     <- biblio_config()
  cfg$output_dir <- tmp_dir

  expect_no_error({
    result <- run_m3(make_m3_minimal_fixture(), config = cfg, export = TRUE)
    expect_true(any(grepl("\\.png$", result$artifacts$manifest$plots, ignore.case = TRUE)))
    expect_true(any(grepl("\\.svg$", result$artifacts$manifest$plots, ignore.case = TRUE)))
    expect_true(any(grepl("\\.pdf$", result$artifacts$manifest$plots, ignore.case = TRUE)))
    expect_true(any(grepl("\\.txt$", result$artifacts$manifest$reports, ignore.case = TRUE)))
    expect_true(any(grepl("\\.tex$", result$artifacts$manifest$reports, ignore.case = TRUE)))
  })
})

test_that("run_m3 attaches IEEE export metadata to rendered plots", {
  result <- run_m3(make_m3_minimal_fixture(), export = FALSE)
  plot_obj <- result$artifacts$plots$productivity$plots$bar_production
  expect_s3_class(plot_obj, "ggplot")
  expect_true(is.list(attr(plot_obj, "ieee_export_spec", exact = TRUE)))
  expect_true(nzchar(plot_obj$labels$caption))
})

test_that("run_m3 report avoids raw NA placeholders in journal-facing narrative", {
  result <- run_m3(make_m3_extended_fixture(), export = FALSE)
  report <- build_m3_report(result, biblio_config())

  expect_false(any(grepl(": NA$", report$lines)))
  expect_true(any(grepl("Interpretive note:", report$lines)))
})

test_that("run_m3 exports explicit placeholders for missing core plot sections", {
  result <- run_m3(make_m3_minimal_fixture(), export = FALSE)
  placeholder_plot <- result$artifacts$plots$economic$plots$insufficient_data

  expect_s3_class(placeholder_plot, "ggplot")
  expect_match(placeholder_plot$labels$title, "unavailable", ignore.case = TRUE)
})
