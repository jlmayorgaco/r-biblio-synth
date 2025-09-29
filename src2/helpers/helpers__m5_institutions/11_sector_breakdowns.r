# ---- Sector breakdowns (stub) -----------------------------------------------

m5i_plot_sector_breakdowns <- function(df_iy, out_base = NULL) {
  # Expect df_iy to have an optional 'sector' column (e.g., university, industry, gov)
  if (!"sector" %in% names(df_iy)) return(NULL)
  p <- ggplot2::ggplot(df_iy %>% dplyr::group_by(sector) %>%
                         dplyr::summarise(TP = sum(TP), TC = sum(TC), .groups="drop"),
                       aes(x = reorder(sector, -TP), y = TP)) +
    ggplot2::geom_col(fill = "grey70", color = "black") +
    ggplot2::labs(x = NULL, y = "TP", title = "Publications by Sector") +
    m5i_ieee_theme()
  if (!is.null(out_base)) m5i_ieee_save(p, out_base)
  p
}
