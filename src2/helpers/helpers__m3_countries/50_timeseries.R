m3c_build_gini_timeseries <- function(df) {
  stopifnot(all(c("country","year","TP") %in% names(df)))
  if (!("TC" %in% names(df))) {
    if ("citations" %in% names(df)) {
      df <- df %>%
        dplyr::group_by(country, year) %>%
        dplyr::summarise(
          TP = dplyr::n(),
          TC = sum(ifelse(is.na(citations), 0, citations), na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      df <- df %>% dplyr::mutate(TC = 0)
    }
  } else {
    df <- df %>% dplyr::select(country, year, TP, TC) %>% dplyr::distinct()
  }

  ts_tp <- df %>% dplyr::group_by(year) %>%
    dplyr::summarise(Gini_TP = ineq::Gini(TP), .groups = "drop")
  ts_tc <- df %>% dplyr::group_by(year) %>%
    dplyr::summarise(Gini_TC = ineq::Gini(TC), .groups = "drop")

  list(tp = ts_tp, tc = ts_tc)
}

m3c_build_mcp_timeseries <- function(df, year_col = "year") {
  stopifnot(is.data.frame(df))
  if (!year_col %in% names(df)) stop(sprintf("build_mcp_timeseries: year column '%s' not found.", year_col))
  if (!all(c("MCP","SCP","TP") %in% names(df))) stop("build_mcp_timeseries: df must contain MCP, SCP, TP.")

  df %>%
    dplyr::group_by(.data[[year_col]]) %>%
    dplyr::summarise(
      MCP_docs   = sum(.data$MCP, na.rm = TRUE),
      SCP_docs   = sum(.data$SCP, na.rm = TRUE),
      Total_docs = sum(.data$TP,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Year          = suppressWarnings(as.integer(as.character(.data[[year_col]]))),
      MCP_share     = ifelse(Total_docs > 0, MCP_docs / Total_docs, NA_real_),
      MCP_share_pct = 100 * MCP_share
    ) %>%
    dplyr::select(Year, MCP_docs, SCP_docs, Total_docs, MCP_share, MCP_share_pct) %>%
    dplyr::arrange(Year)
}
