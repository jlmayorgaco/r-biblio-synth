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

test_that("country_summary share is computed per country", {
  pd <- make_m3_prepared_extended()
  expected <- pd$country_summary$article_count / sum(pd$country_summary$article_count)
  expect_equal(pd$country_summary$share, expected)
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

test_that("C1 parsing does not treat affiliation fragments as countries", {
  fixture <- tibble::tibble(
    AU = c("Author A", "Author B"),
    PY = c(2020L, 2021L),
    TC = c(45L, 18L),
    C1 = c(
      "[Author A] Digital Power Lab, Technical University of C, Aalborg, Denmark; [Author C] Department of Electrical Engineering, University of Toronto, Toronto, Canada",
      "[Author B] Power Systems Group, Technical University Delft, Delft, Netherlands"
    )
  )

  pd <- prepare_m3_country_data(fixture)
  countries <- pd$country_summary$country

  expect_true(all(c("DENMARK", "CANADA", "NETHERLANDS") %in% countries))
  expect_false(any(grepl("DIGITAL POWER|TECHNICAL UNIVERSITY", countries)))
})

test_that("AU_CO values are normalized and validated before aggregation", {
  fixture <- tibble::tibble(
    AU = c("Author A", "Author B"),
    PY = c(2020L, 2021L),
    TC = c(12L, 30L),
    AU_CO = c("CA; USA; Digital Power", "Syrian Arab Republic; Technical University of C")
  )

  pd <- prepare_m3_country_data(fixture)
  countries <- pd$country_summary$country

  expect_true(all(c("CANADA", "UNITED STATES", "SYRIA") %in% countries))
  expect_false(any(grepl("DIGITAL POWER|TECHNICAL UNIVERSITY", countries)))
})
