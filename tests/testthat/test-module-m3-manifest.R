# ============================================================================
# test-module-m3-manifest.R
# ============================================================================
test_that("build_m3_manifest returns a biblio_artifact_manifest", {
  result   <- run_m3(make_m3_minimal_fixture(), export = FALSE)
  exported <- list(plots = character(), files = character(),
                   tables = character(), reports = character())
  manifest <- build_m3_manifest(result, exported)
  expect_s3_class(manifest, "biblio_artifact_manifest")
})

test_that("manifest module_id is m3", {
  result   <- run_m3(make_m3_minimal_fixture(), export = FALSE)
  exported <- list(plots = character(), files = character(),
                   tables = character(), reports = character())
  manifest <- build_m3_manifest(result, exported)
  expect_equal(manifest$module_id, "m3")
})

test_that("manifest status matches result status", {
  result   <- run_m3(make_m3_minimal_fixture(), export = FALSE)
  exported <- list(plots = character(), files = character(),
                   tables = character(), reports = character())
  manifest <- build_m3_manifest(result, exported)
  expect_equal(manifest$status, result$status)
})
