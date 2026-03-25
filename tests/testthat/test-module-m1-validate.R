# ============================================================================
# test-module-m1-validate.R - Tests for M1 validation
# ============================================================================

context("module m1 - validate")

test_that("validate_m1_input succeeds on minimal fixture", {
  fixture <- make_minimal_biblio_fixture()
  result <- validate_m1_input(fixture)
  expect_true(result$ok)
  expect_equal(result$n_rows, 3)
})

test_that("validate_m1_input detects missing columns", {
  fixture <- tibble::tibble(x = 1, y = 2)
  result <- validate_m1_input(fixture)
  expect_false(result$ok)
  expect_true(length(result$missing_columns) > 0)
})

test_that("validate_m1_input rejects non-data-frame", {
  result <- validate_m1_input("not a dataframe")
  expect_false(result$ok)
})
