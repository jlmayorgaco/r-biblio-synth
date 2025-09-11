# ================================================================
# Save queued plots in PNG (300 dpi) and SVG — IEEE single column
# ================================================================
# Reqs: ggplot2 (and optionally svglite for high-quality SVG device)

save_countries_plots_ieee <- function(plots_by_countries,
                            root_out = "results2/M3",
                            width_in = 3.5,      # IEEE single column ~3.5 in
                            height_in = 2.2,     # tweak to your taste (2.0–2.4)
                            dpi = 300) {

  stopifnot(requireNamespace("ggplot2", quietly = TRUE))


  for (i in seq_along(plots_by_countries)) {
    entry <- plots_by_countries[[i]]
    p       <- entry$plot
    relpath <- entry$filename           # e.g. "by_countries/usa/usa_tp.png"
    # Ensure extension handling
    rel_no_ext <- sub("\\.png$", "", relpath, ignore.case = TRUE)

    # Final paths
    png_path <- file.path(root_out, paste0(rel_no_ext, ".png"))
    svg_path <- file.path(root_out, paste0(rel_no_ext, ".svg"))

    # Make sure dirs exist
    dir.create(dirname(png_path), recursive = TRUE, showWarnings = FALSE)

    # --- Save PNG ---
    try({
      ggplot2::ggsave(
        filename = png_path,
        plot     = p,
        width    = width_in,
        height   = height_in,
        dpi      = dpi,
        units    = "in",
        limitsize = FALSE
      )
      message("[PNG] Saved: ", png_path)
    }, silent = TRUE)

    # --- Save SVG ---
    # If you have svglite installed, it generally produces cleaner SVGs:
    # device = "svglite"
    svg_device <- if (requireNamespace("svglite", quietly = TRUE)) "svglite" else "svg"
    try({
      ggplot2::ggsave(
        filename = svg_path,
        plot     = p,
        width    = width_in,
        height   = height_in,
        units    = "in",
        device   = svg_device,
        limitsize = FALSE
      )
      message("[SVG] Saved: ", svg_path)
    }, silent = TRUE)
  }
}

