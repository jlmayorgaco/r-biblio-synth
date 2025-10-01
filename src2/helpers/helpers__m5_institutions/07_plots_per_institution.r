# ---- Per-institution IEEE plots (optional) ----------------------------------

m5i_build_institution_plot_entries <- function(df_iy) {
  split(df_iy, df_iy$institution)
}

m5i_save_institution_plots_ieee <- function(entries, out_dir,
                                            width_in = 3.42, height_in = 2.2, dpi = 600) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  for (nm in names(entries)) {
    d <- entries[[nm]] %>% dplyr::arrange(year)
    p <- ggplot2::ggplot(d, aes(x = year, y = TP)) +
      ggplot2::geom_col(fill = "grey70", color = "black", width = 0.8) +
      ggplot2::labs(x = "Year", y = "TP", title = paste0(nm, " — Publications by Year")) +
      m5i_ieee_theme()
    ggplot2::ggsave(file.path(out_dir, paste0(gsub("[^a-z0-9]+","_",tolower(nm)), "_TP.png")),
                    p, width = width_in, height = height_in, dpi = dpi, bg = "white")
  }
  invisible(TRUE)
}
