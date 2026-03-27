# ============================================================================
# m3_table_scp_mcp.R - SCP/MCP tables for M3
# ============================================================================

#' Build SCP/MCP collaboration tables
#'
#' @param scp_mcp_data Output from \code{m3_compute_scp_mcp}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list of tibbles: \code{full}, \code{top_scp}, \code{top_mcp}, \code{top_ratio}
#' @export
m3_table_scp_mcp <- function(scp_mcp_data, config = biblio_config()) {
  empty <- list(full = tibble::tibble(), top_scp = tibble::tibble(),
                top_mcp = tibble::tibble(), top_ratio = tibble::tibble())
  if (!is.list(scp_mcp_data) || nrow(scp_mcp_data$scp_mcp) == 0) return(empty)

  full <- scp_mcp_data$scp_mcp %>%
    dplyr::rename(
      Country      = country,
      Articles     = article_count,
      SCP          = scp,
      MCP          = mcp,
      `MCP Ratio (%)` = mcp_ratio
    )

  top_scp <- scp_mcp_data$top_countries_by_scp %>%
    dplyr::rename(Rank = rank, Country = label, SCP = value)

  top_mcp <- scp_mcp_data$top_countries_by_mcp %>%
    dplyr::rename(Rank = rank, Country = label, MCP = value)

  top_ratio <- scp_mcp_data$top_countries_by_mcp_ratio %>%
    dplyr::rename(Rank = rank, Country = label, `MCP Ratio (%)` = value)

  list(full = full, top_scp = top_scp, top_mcp = top_mcp, top_ratio = top_ratio)
}
