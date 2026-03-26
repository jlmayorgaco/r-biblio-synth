# ============================================================================
# test-core-validation.R - Tests for core validation
# ============================================================================



test_that("validate_is_data_frame passes for data.frame", {
  df <- data.frame(a = 1, b = 2)
  expect_true(validate_is_data_frame(df))
})

test_that("validate_is_data_frame fails for non-data.frame", {
  expect_error(validate_is_data_frame("not a df"))
})

test_that("validate_required_columns passes with all columns", {
  df <- data.frame(a = 1, b = 2, c = 3)
  result <- validate_required_columns(df, c("a", "b"))
  expect_true(result$ok)
  expect_equal(length(result$missing_columns), 0)
})

test_that("validate_required_columns detects missing columns", {
  df <- data.frame(a = 1, b = 2)
  result <- validate_required_columns(df, c("a", "c", "d"))
  expect_false(result$ok)
  expect_true("c" %in% result$missing_columns)
  expect_true("d" %in% result$missing_columns)
})
