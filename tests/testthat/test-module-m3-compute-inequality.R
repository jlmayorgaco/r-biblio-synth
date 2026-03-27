# ============================================================================
# test-module-m3-compute-inequality.R
# ============================================================================
test_that("m3_compute_inequality returns expected keys", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_inequality(pd)
  expect_type(result, "list")
  expect_true(all(c("production_inequality", "citations_inequality",
                    "inequality_summary", "status") %in% names(result)))
})

test_that("Gini coefficients are in [0, 1]", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_inequality(pd)
  g_prod <- result$inequality_summary$production$gini
  g_cit  <- result$inequality_summary$citations$gini
  expect_true(is.na(g_prod) || (g_prod >= 0 && g_prod <= 1))
  expect_true(is.na(g_cit)  || (g_cit  >= 0 && g_cit  <= 1))
})

test_that("HHI is in (0, 1]", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_inequality(pd)
  hhi <- result$inequality_summary$production$hhi
  expect_true(is.na(hhi) || (hhi > 0 && hhi <= 1))
})

test_that("top5_share is in [0, 1]", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_inequality(pd)
  s5 <- result$inequality_summary$production$top5_share
  expect_true(is.na(s5) || (s5 >= 0 && s5 <= 1))
})

test_that("Lorenz data has cumulative_entities and cumulative_values", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_inequality(pd)
  lorenz <- result$production_inequality
  expect_true(all(c("cumulative_entities", "cumulative_values") %in% names(lorenz)))
})

test_that("m3_compute_inequality returns error stub for empty data", {
  empty_pd <- list(country_summary = tibble::tibble())
  result   <- m3_compute_inequality(empty_pd)
  expect_match(result$status, "error")
})
