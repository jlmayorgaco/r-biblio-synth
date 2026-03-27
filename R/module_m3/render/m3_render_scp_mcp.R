# ============================================================================
# m3_render_scp_mcp.R - SCP/MCP collaboration plots for M3
# ============================================================================

#' Render SCP/MCP collaboration plots
#'
#' @param scp_mcp_data Output from \code{m3_compute_scp_mcp}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list with \code{status} and \code{plots}
#' @export
m3_render_scp_mcp <- function(scp_mcp_data, config = biblio_config()) {
  stub <- list(status = "stub", plots = list())

  if (!is.list(scp_mcp_data) || nrow(scp_mcp_data$scp_mcp) == 0) {
    return(stub)
  }

  top_n  <- config$top_n_countries
  df     <- scp_mcp_data$scp_mcp
  df_top <- df %>%
    dplyr::arrange(dplyr::desc(article_count)) %>%
    dplyr::slice(seq_len(min(top_n, nrow(.)))) %>%
    dplyr::mutate(label_clean = substr(trimws(country), 1, 25))

  plots <- list()

  # 1. Stacked bar: SCP + MCP
  df_long <- df_top %>%
    dplyr::select(label_clean, scp, mcp) %>%
    tidyr::pivot_longer(cols = c(scp, mcp),
                        names_to  = "type",
                        values_to = "count") %>%
    dplyr::mutate(type = toupper(type))

  plots$stacked_bar <- ggplot2::ggplot(
    df_long,
    ggplot2::aes(x = reorder(label_clean, count, sum), y = count, fill = type)
  ) +
    ggplot2::geom_col(color = "black", linewidth = 0.15) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c(SCP = ieee_colors$blue, MCP = ieee_colors$orange),
      name   = "Type"
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::labs(
      title = "SCP vs MCP by Country",
      x     = NULL,
      y     = "Publications"
    ) +
    ieee_theme() +
    ggplot2::theme(legend.position = "bottom")

  # 2. MCP ratio ranking
  df_ratio <- df %>%
    dplyr::filter(!is.na(mcp_ratio)) %>%
    dplyr::arrange(dplyr::desc(mcp_ratio)) %>%
    dplyr::slice(seq_len(min(top_n, nrow(.)))) %>%
    dplyr::mutate(label_clean = substr(trimws(country), 1, 25))

  if (nrow(df_ratio) > 0) {
    plots$mcp_ratio <- ggplot2::ggplot(
      df_ratio,
      ggplot2::aes(x = reorder(label_clean, mcp_ratio), y = mcp_ratio)
    ) +
      ggplot2::geom_col(fill = ieee_colors$purple, color = "black", linewidth = 0.2) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", mcp_ratio)),
                         hjust = -0.15, size = 2.0) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(limits = c(0, 115),
                                  expand = ggplot2::expansion(mult = c(0, 0))) +
      ggplot2::labs(
        title = "MCP Ratio by Country (%)",
        x     = NULL,
        y     = "MCP Ratio (%)"
      ) +
      ieee_theme()
  }

  list(status = "success", plots = plots)
}
