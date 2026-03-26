# ============================================================================
# test-module-m1-compute-bradford.R
# ============================================================================



test_that("compute_m1_bradford returns list with status", {
  fixture <- make_extended_biblio_fixture()
  result <- compute_m1_bradford(fixture)
  expect_type(result, "list")
  expect_true("status" %in% names(result))
  expect_equal(result$status, "success")
})

test_that("compute_m1_bradford returns bradford_table", {
  fixture <- make_extended_biblio_fixture()
  result <- compute_m1_bradford(fixture)
  expect_true("bradford_table" %in% names(result))
  expect_true(nrow(result$bradford_table) > 0)
})
