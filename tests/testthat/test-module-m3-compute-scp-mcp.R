# ============================================================================
# test-module-m3-compute-scp-mcp.R
# ============================================================================
test_that("m3_compute_scp_mcp returns expected keys", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_scp_mcp(pd)
  expect_type(result, "list")
  expect_true(all(c("scp_mcp", "scp_mcp_summary", "status") %in% names(result)))
})

test_that("m3_compute_scp_mcp returns success for valid data", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_scp_mcp(pd)
  expect_equal(result$status, "success")
})

test_that("SCP + MCP equals article_count for each country", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_scp_mcp(pd)
  scp_mcp_df <- result$scp_mcp
  expect_true(all((scp_mcp_df$scp + scp_mcp_df$mcp) == scp_mcp_df$article_count))
})

test_that("MCP ratio is between 0 and 100 for all countries", {
  pd     <- make_m3_prepared_extended()
  result <- m3_compute_scp_mcp(pd)
  ratios <- result$scp_mcp$mcp_ratio
  expect_true(all(is.na(ratios) | (ratios >= 0 & ratios <= 100)))
})

test_that("single-country documents produce SCP for that country", {
  pd     <- make_m3_prepared_minimal()
  result <- m3_compute_scp_mcp(pd)
  # All docs in minimal fixture have single country
  expect_true(sum(result$scp_mcp$mcp) == 0)
})

test_that("m3_compute_scp_mcp returns error stub for empty data", {
  empty_pd <- list(country_doc_level = tibble::tibble(),
                   country_summary   = tibble::tibble())
  result   <- m3_compute_scp_mcp(empty_pd)
  expect_match(result$status, "error")
})
