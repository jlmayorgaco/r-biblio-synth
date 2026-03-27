# ============================================================================
# test-module-m3-compute-growth-dynamics.R
# ============================================================================
test_that("m3_compute_growth_dynamics returns expected keys", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_growth_dynamics(pd)
  expect_type(result, "list")
  expect_true(all(c("annual_productivity", "annual_citations",
                    "growth_summary", "status") %in% names(result)))
})

test_that("returns success when annual data is available", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_growth_dynamics(pd)
  expect_match(result$status, "success")
})

test_that("annual_productivity has country, PY, article_count columns", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_growth_dynamics(pd)
  if (nrow(result$annual_productivity) > 0) {
    expect_true(all(c("country", "PY", "article_count") %in%
                      names(result$annual_productivity)))
  }
})

test_that("growth_summary has available_years field", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_growth_dynamics(pd)
  expect_true("available_years" %in% names(result$growth_summary$productivity))
})

test_that("graceful degradation for data without PY", {
  fixture_no_py <- make_m3_minimal_fixture() %>% dplyr::select(-PY)
  pd     <- prepare_m3_country_data(fixture_no_py)
  result <- m3_compute_growth_dynamics(pd)
  expect_match(result$status, "success")  # should succeed but with no annual data
  expect_equal(nrow(result$annual_productivity), 0)
})
