# ============================================================================
# test-module-m3-validate.R
# ============================================================================
test_that("validate_m3_input passes with AU_CO column", {
  df <- tibble::tibble(AU_CO = c("UNITED STATES", "CHINA"))
  result <- validate_m3_input(df)
  expect_true(result$ok)
  expect_length(result$missing_columns, 0)
})

test_that("validate_m3_input fails for non-data.frame", {
  result <- validate_m3_input("not a data frame")
  expect_false(result$ok)
})

test_that("validate_m3_input fails when neither AU_CO nor C1 present", {
  df <- tibble::tibble(AU = "Smith J", PY = 2020L)
  result <- validate_m3_input(df)
  expect_false(result$ok)
  expect_match(result$error, "AU_CO|C1")
})

test_that("validate_m3_input passes with only C1 column", {
  df <- tibble::tibble(C1 = "Univ. Test, USA")
  result <- validate_m3_input(df)
  expect_true(result$ok)
})
