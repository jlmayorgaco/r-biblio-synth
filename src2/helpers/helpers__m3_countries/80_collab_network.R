# helpers/helpers__m3_countries/80_collab_network.R

# Ensure we have a character, delimiter-separated column with country names.
.m3c_ensure_country_list <- function(df) {
  # If Country_List exists & is character, keep it
  if ("Country_List" %in% names(df) && is.character(df$Country_List)) {
    return(df)
  }
  # Otherwise, synthesize from .countries (list-column) if available
  if (".countries" %in% names(df)) {
    df$Country_List <- vapply(df$.countries, function(v) {
      v <- v[!is.na(v) & nzchar(v)]
      if (!length(v)) "" else paste(v, collapse = "; ")
    }, FUN.VALUE = character(1))
    return(df)
  }
  # Fallback: try any column with "country" in the name (as character)
  cand <- grep("country", names(df), ignore.case = TRUE, value = TRUE)
  if (length(cand)) {
    take <- cand[1]
    df$Country_List <- as.character(df[[take]])
    return(df)
  }
  # Last resort: empty column (won't crash, just yields empty graph)
  df$Country_List <- ""
  df
}

m3c_build_collab_network <- function(df_docs, self_country_col, cfg) {
  # 1) Guarantee a proper Country_List column for the clustering helper
  df_docs <- .m3c_ensure_country_list(df_docs)

  # 2) Always pass Country_List explicitly to avoid auto-detect snafus
  country_cluster_plot(
    df = df_docs,
    country_col = "Country_List",
    min_edge_weight = cfg$min_edge_weight,
    min_degree = cfg$min_degree,
    label_top = cfg$label_top,
    community_method = cfg$community_method,
    title = cfg$title,
    arrange = cfg$arrange,
    spread = cfg$spread,
    intra_scale = cfg$intra_scale,
    top_k_legend = cfg$top_k_legend,
    base_size = cfg$base_size
  )
}

m3c_reconstruct_cc_cfg <- function(self_country_col, cfg) {
  # Reflect the column we actually used ("Country_List")
  cc_config(
    country_col      = "Country_List",
    sep_regex        = ";|,|\\|",
    min_edge_weight  = cfg$min_edge_weight,
    min_degree       = cfg$min_degree,
    label_top        = cfg$label_top,
    community_method = cfg$community_method,
    title            = cfg$title,
    base_size        = cfg$base_size,
    top_k_legend     = cfg$top_k_legend,
    spread           = cfg$spread,
    intra_scale      = cfg$intra_scale,
    arrange          = cfg$arrange,
    legend_upper     = TRUE,
    seed             = 42
  )
}

m3c_save_collab_plot <- function(cc, out_dir, cfg) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_png <- file.path(out_dir, cfg$out_png)
  out_svg <- file.path(out_dir, cfg$out_svg)
  ggplot2::ggsave(out_png, plot = cc$plot, width = 7.5, height = 7, units = "in", dpi = 600)
  ggplot2::ggsave(out_svg, plot = cc$plot, width = 7.5, height = 7, units = "in", device = "svg")
}
