# ============================================================================
# test-module-m3-compute-change-points.R
# ============================================================================
test_that("m3_compute_change_points returns expected keys", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_change_points(pd)
  expect_type(result, "list")
  expect_true(all(c("productivity_change_points", "change_point_summary",
                    "status") %in% names(result)))
})

test_that("returns success for valid data (with or without change points found)", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_change_points(pd)
  expect_match(result$status, "success")
})

test_that("graceful degradation when no annual data present", {
  fixture_no_py <- make_m3_minimal_fixture() %>% dplyr::select(-PY)
  pd     <- prepare_m3_country_data(fixture_no_py)
  result <- m3_compute_change_points(pd)
  expect_match(result$status, "success")
  expect_equal(nrow(result$productivity_change_points), 0)
})

test_that("detected change points have required columns", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_change_points(pd)
  cp <- result$productivity_change_points
  if (nrow(cp) > 0) {
    expect_true(all(c("country", "change_point_year") %in% names(cp)))
  }
})
