m3c_build_quadrants <- function(df_cy, cfg_quad) {
  quad_bubble_plot(
    df = df_cy,
    year_col = "year",
    country_col = "country",
    N_years = cfg_quad$N_years,
    top_n = cfg_quad$top_n,
    layout = cfg_quad$layout,
    show_arrows = cfg_quad$show_arrows,
    zero_offset = cfg_quad$zero_offset,
    quadrant_alpha = cfg_quad$quadrant_alpha,
    point_size = cfg_quad$point_size,
    arrow_vs_dot_mult = cfg_quad$arrow_vs_dot_mult,
    add_metrics = cfg_quad$add_metrics
  )
}

m3c_save_quadrants <- function(qb, out_dir, w_in, h_in, dpi) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  ggplot2::ggsave(file.path(out_dir, "quad_tp_tc.png"),   qb$plots$tp_tc,   width=w_in, height=h_in, units="in", dpi=dpi)
  ggplot2::ggsave(file.path(out_dir, "quad_scp_mcp.png"), qb$plots$scp_mcp, width=w_in, height=h_in, units="in", dpi=dpi)
  ggplot2::ggsave(file.path(out_dir, "quad_tp_tc.svg"),   qb$plots$tp_tc,   width=w_in, height=h_in, units="in", device="svg")
  ggplot2::ggsave(file.path(out_dir, "quad_scp_mcp.svg"), qb$plots$scp_mcp, width=w_in, height=h_in, units="in", device="svg")
}
