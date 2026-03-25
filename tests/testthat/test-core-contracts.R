# ============================================================================
# test-core-contracts.R - Tests for core contracts
# ============================================================================

context("core - contracts")

test_that("new_module_result returns expected structure", {
  result <- new_module_result()
  expect_true(inherits(result, "biblio_module_result"))
  expect_true("module_id" %in% names(result))
  expect_true("status" %in% names(result))
  expect_true("data" %in% names(result))
  expect_true("diagnostics" %in% names(result))
  expect_true("artifacts" %in% names(result))
})

test_that("validate_module_result accepts valid object", {
  result <- new_module_result()
  expect_true(validate_module_result(result))
})

test_that("validate_module_result rejects invalid object", {
  expect_false(validate_module_result(list()))
})

test_that("get_available_modules contains m1", {
  modules <- get_available_modules()
  expect_true("m1" %in% modules)
})
