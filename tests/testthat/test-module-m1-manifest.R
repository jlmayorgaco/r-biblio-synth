# ============================================================================
# test-module-m1-manifest.R - Tests for M1 manifest
# ============================================================================



test_that("build_m1_manifest returns biblio_artifact_manifest", {
  result <- new_module_result(module_id = "m1")
  manifest <- build_m1_manifest(result)
  expect_true(inherits(manifest, "biblio_artifact_manifest"))
})

test_that("build_m1_manifest has expected fields", {
  result <- new_module_result(module_id = "m1")
  manifest <- build_m1_manifest(result)
  expect_true("module_id" %in% names(manifest))
  expect_true("generated_at" %in% names(manifest))
  expect_true("status" %in% names(manifest))
  expect_equal(manifest$module_id, "m1")
})
