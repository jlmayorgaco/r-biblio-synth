# ---- MCP (multi-institution papers) share ----------------------------------

m5i_build_mcp_timeseries <- function(df_iy_or_docs) {
  # Accept documents (with Institution_List) OR aggregated (won’t have lists).
  if (!"Institution_List" %in% names(df_iy_or_docs)) {
    warning("[m5i_build_mcp_timeseries] Expecting document-level data with Institution_List; returning empty.")
    return(data.frame(Year = integer(), MCP_share_pct = numeric()))
  }
  df <- df_iy_or_docs
  rows <- lapply(seq_len(nrow(df)), function(i) {
    y <- df$Year[i]; insts <- df$Institution_List[[i]]
    if (all(is.na(insts))) return(data.frame(Year = y, multi = 0L))
    data.frame(Year = y, multi = as.integer(length(unlist(insts)) > 1))
  })
  mc <- dplyr::bind_rows(rows) %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(MCP_share_pct = 100 * mean(multi, na.rm = TRUE), .groups = "drop")
  mc
}
