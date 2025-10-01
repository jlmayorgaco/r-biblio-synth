# ---- Quadrants (TP vs TC) ---------------------------------------------------

m5i_build_quadrants <- function(df_iy, cfg) {
  inst <- df_iy %>% dplyr::group_by(institution) %>%
    dplyr::summarise(TP = sum(TP), TC = sum(TC), .groups="drop")
  med_tp <- stats::median(inst$TP, na.rm = TRUE)
  med_tc <- stats::median(inst$TC, na.rm = TRUE)
  inst <- inst %>%
    dplyr::mutate(
      quadrant = dplyr::case_when(
        TP >= med_tp & TC >= med_tc ~ "Q1: High-High",
        TP >= med_tp & TC <  med_tc ~ "Q2: High-Low",
        TP <  med_tp & TC >= med_tc ~ "Q3: Low-High",
        TRUE                        ~ "Q4: Low-Low"
      )
    )
  list(metrics = list(median_TP = med_tp, median_TC = med_tc),
       data = inst)
}

m5i_save_quadrants <- function(qb, out_dir, w_in = 3.42, h_in = 2.4, dpi = 600) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  p <- ggplot2::ggplot(qb$data, aes(TP, TC)) +
    ggplot2::geom_point(size = 0.9) +
    ggplot2::geom_vline(xintercept = qb$metrics$median_TP, linetype = 2, linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = qb$metrics$median_TC, linetype = 2, linewidth = 0.3) +
    ggplot2::labs(x = "Publications (TP)", y = "Citations (TC)", title = "Institutions: TP vs TC (Quadrants)") +
    m5i_ieee_theme()
  ggplot2::ggsave(file.path(out_dir, "M5_quadrants.png"), p, width = w_in, height = h_in, dpi = dpi, bg = "white")
  ggplot2::ggsave(file.path(out_dir, "M5_quadrants.svg"), p, width = w_in, height = h_in, dpi = dpi, bg = "white")
  p
}
