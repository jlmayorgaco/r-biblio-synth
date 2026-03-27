# ============================================================================
# test-module-m3-compute-rankings.R
# ============================================================================
test_that("m3_compute_rankings returns expected keys", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_rankings(pd)
  expect_type(result, "list")
  expect_true(all(c("production_rankings", "citations_rankings",
                    "rank_summary", "status") %in% names(result)))
})

test_that("production_rankings has rank, label, value columns", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_rankings(pd)
  expect_true(all(c("rank", "label", "value") %in%
                    names(result$production_rankings)))
})

test_that("ranks are strictly increasing from 1", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_rankings(pd)
  ranks  <- result$production_rankings$rank
  expect_equal(ranks, seq_len(nrow(result$production_rankings)))
})

test_that("values are in decreasing order", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_rankings(pd)
  vals   <- result$production_rankings$value
  expect_true(all(diff(vals) <= 0))
})

test_that("top_heavy proportion is in [0, 1]", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_rankings(pd)
  th <- result$rank_summary$production$top_heavy
  expect_true(is.na(th) || (th >= 0 && th <= 1))
})

test_that("m3_compute_rankings returns error stub for empty data", {
  empty_pd <- list(country_summary = tibble::tibble())
  result   <- m3_compute_rankings(empty_pd)
  expect_match(result$status, "error")
})
