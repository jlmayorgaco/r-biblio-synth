# ============================================================================
# test-ieee-plot-export.R - IEEE visual export contract
# ============================================================================

test_that("IEEE no-data plots carry layout metadata without warnings", {
  p <- expect_warning(
    ieee_no_data_plot(
      title = "Section unavailable",
      message = "The section lacks enough validated data for a meaningful figure.",
      layout = "full"
    ),
    NA
  )

  expect_s3_class(p, "ggplot")
  expect_equal(attr(p, "ieee_layout", exact = TRUE), "full")

  prepared <- ieee_prepare_plot_for_export(
    p,
    module_id = "m3",
    section_id = "temporal_dynamics",
    plot_id = "insufficient_data",
    config = biblio_config(verbose = FALSE, cache_enabled = FALSE)
  )
  spec <- ieee_get_plot_export_spec(
    prepared,
    config = biblio_config(verbose = FALSE, cache_enabled = FALSE),
    section_id = "temporal_dynamics",
    plot_id = "insufficient_data"
  )

  expect_equal(spec$figure_type, "full")
  expect_gt(spec$width, 7)
  expect_gt(spec$height, 4)
})

test_that("plot exporter writes journal-grade raster and vector artifacts", {
  tmp_dir <- tempfile("ieee_plot_export_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  p <- ggplot2::ggplot(data.frame(x = 1:4, y = c(1, 3, 2, 5)), ggplot2::aes(x, y)) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point(size = 1.7) +
    ggplot2::labs(title = "Export Smoke Test", x = "Year", y = "Articles") +
    ieee_theme_timeseries(base_size = 8)

  out <- expect_warning(
    export_plot_artifact(p, file.path(tmp_dir, "smoke"), width = 3.5, height = 2.95, dpi = 150),
    NA
  )

  expect_true(file.exists(unname(out["png"])))
  expect_true(file.exists(unname(out["pdf"])))
  if (!is.na(unname(out["svg"]))) expect_true(file.exists(unname(out["svg"])))
  if (!is.na(unname(out["eps"]))) expect_true(file.exists(unname(out["eps"])))
})
