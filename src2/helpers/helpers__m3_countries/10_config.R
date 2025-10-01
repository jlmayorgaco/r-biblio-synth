m3c_default_cfg <- function() {
  list(
    report_path   = "results2/M3",
    countries_csv = "src2/data/countries.csv",
    ieee = list(width_in = 3.2, height_in = 1.8, dpi = 300),
    quadrants = list(
      out_dir = file.path("results2/M3","figures","quadrants"),
      w_in = 7.16, h_in = 3.6, dpi = 600,
      N_years = 5, top_n = 10, layout = "twocol",
      show_arrows = TRUE, zero_offset = 0.01,
      quadrant_alpha = 0.08, point_size = 2, arrow_vs_dot_mult = 2,
      add_metrics = TRUE
    ),
    collab = list(
      min_edge_weight = 2, min_degree = 2, label_top = 15,
      community_method = "louvain", arrange = "auto",
      spread = 5, intra_scale = 4, top_k_legend = 5, base_size = 9,
      title = "International Co-authorship Network — Communities",
      out_png = "country_collab_clusters_w.png",
      out_svg = "country_collab_clusters_w.svg"
    )
  )
}

m3c_slugify <- function(x) {
  x <- trimws(x); x <- gsub("[^A-Za-z0-9]+", "_", x)
  tolower(gsub("^_+|_+$", "", x))
}

m3c_year_breaks <- function(y) {
  y <- sort(unique(as.integer(as.character(y))))
  r <- range(y); span <- diff(r)
  if (span <= 10) seq(r[1], r[2], 1L)
  else if (span <= 35) seq(floor(r[1]/5)*5, ceiling(r[2]/5)*5, 5L)
  else seq(floor(r[1]/10)*10, ceiling(r[2]/10)*10, 10L)
}


