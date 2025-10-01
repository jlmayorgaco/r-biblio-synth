# helpers/helpers__m3_countries/60_country_plots.R

.m3c_connect_mode <- function(n_rows) if (is.finite(n_rows) && n_rows >= 2) "line" else "none"

m3c_build_country_plot_entries <- function(df_cy_norm) {
  stopifnot(is.data.frame(df_cy_norm))
  entries <- list(); idx <- 1L

  # Only iterate real country names
  countries <- unique(df_cy_norm$country)
  countries <- countries[!is.na(countries) & nzchar(countries)]

  for (cname in countries) {
    df_c <- df_cy_norm %>% dplyr::filter(country == cname) %>% dplyr::arrange(year)
    if (!nrow(df_c)) next

    brks <- m3c_year_breaks(df_c$year)
    slug <- m3c_slugify(cname)
    if (!nzchar(slug)) next  # avoid "character_0" style folders
    out_dir <- file.path("by_countries", slug)

    # Choose connect mode based on number of points
    cm <- .m3c_connect_mode(nrow(df_c))

    p_tp <- plot_scatter(df=df_c, x_col=year, y_col=TP,
                         title=paste0(cname," — Total Publications (TP)"),
                         xlabel="Year", ylabel="TP", x_breaks=brks, connect=cm) +
            ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

    p_tc <- plot_scatter(df=df_c, x_col=year, y_col=TC,
                         title=paste0(cname," — Total Citations (TC)"),
                         xlabel="Year", ylabel="TC", x_breaks=brks, connect=cm) +
            ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

    p_tc_adj <- plot_scatter(df=df_c, x_col=year, y_col=TC_adj,
                         title=paste0(cname," — Citations per Year (TC_adj)"),
                         xlabel="Year", ylabel="TC per year", x_breaks=brks, connect=cm) +
                ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

    p_mcp <- plot_scatter(df=df_c, x_col=year, y_col=MCP_ratio,
                          title=paste0(cname," — MCP Ratio"),
                          xlabel="Year", ylabel="MCP ratio",
                          x_breaks=brks, y_percent=TRUE, y_percent_breaks=seq(0,1,0.2),
                          y_limits=c(0,1), connect=cm) +
             ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

    entries[[idx]] <- list(country=cname, metric="TP",       plot=p_tp,     filename=file.path(out_dir, paste0(slug,"_tp.png")));     idx <- idx+1L
    entries[[idx]] <- list(country=cname, metric="TC",       plot=p_tc,     filename=file.path(out_dir, paste0(slug,"_tc.png")));     idx <- idx+1L
    entries[[idx]] <- list(country=cname, metric="TC_adj",   plot=p_tc_adj, filename=file.path(out_dir, paste0(slug,"_tc_adj.png"))); idx <- idx+1L
    entries[[idx]] <- list(country=cname, metric="MCP_ratio",plot=p_mcp,    filename=file.path(out_dir, paste0(slug,"_mcp.png")));    idx <- idx+1L
  }
  entries
}

m3c_save_country_plots_ieee <- function(entries, root_out, width_in, height_in, dpi) {
  save_countries_plots_ieee(
    plots_by_countries = entries,
    root_out  = root_out,
    width_in  = width_in,
    height_in = height_in,
    dpi       = dpi
  )
}
