# ============================================================================
# test-module-m3-prepare-country-data.R
# ============================================================================
test_that("prepare_m3_country_data returns success for minimal fixture", {
  pd <- make_m3_prepared_minimal()
  expect_equal(pd$status, "success")
})

test_that("country_summary is non-empty and has expected columns", {
  pd <- make_m3_prepared_minimal()
  expect_true(nrow(pd$country_summary) > 0)
  expect_true(all(c("country", "article_count") %in% names(pd$country_summary)))
})

test_that("citations are propagated to country_summary when TC present", {
  pd <- make_m3_prepared_minimal()
  expect_true("total_citations" %in% names(pd$country_summary))
  expect_true(sum(pd$country_summary$total_citations) > 0)
})

test_that("country_doc_level has doc_id and country columns", {
  pd <- make_m3_prepared_minimal()
  expect_true(all(c("doc_id", "country") %in% names(pd$country_doc_level)))
})

test_that("multi-country documents produce multiple country-doc rows", {
  pd <- make_m3_prepared_extended()
  # document 1 has "UNITED STATES;CHINA" → 2 rows
  doc1 <- pd$country_doc_level[pd$country_doc_level$doc_id == 1, ]
  expect_gte(nrow(doc1), 2)
})

test_that("annual aggregation works when PY is present", {
  pd <- make_m3_prepared_extended()
  expect_true(nrow(pd$country_annual) > 0)
  expect_true(all(c("country", "PY", "article_count") %in% names(pd$country_annual)))
})

test_that("m3_normalize_country_names unifies USA variants", {
  result <- m3_normalize_country_names(c("USA", "U.S.A.", "UNITED STATES OF AMERICA"))
  expect_true(all(result == "UNITED STATES"))
})

test_that("m3_normalize_country_names handles empty input gracefully", {
  result <- m3_normalize_country_names(character(0))
  expect_length(result, 0)
})

test_that("prepare_m3_country_data returns error for empty input", {
  pd <- prepare_m3_country_data(tibble::tibble())
  expect_match(pd$status, "error")
})
