# Default configuration for M5_Institutions

m5i_default_cfg <- function() {
  list(
    # Where to place module outputs by default
    report_path = file.path("results2", "M5"),

    # IEEE-ish figure defaults
    ieee = list(
      width_in  = 7.16,  # double-column width
      height_in = 4.2,
      dpi       = 600
    ),

    # (Optional) places for specific outputs if you add them later
    figures_dir = file.path("results2", "M5", "figures"),
    jsons_dir   = file.path("results2", "M5", "jsons"),

    # Example knobs (safe defaults so the module won’t crash)
    min_docs_per_inst = 2L,
    top_n_bars        = 10L,

    # If you later add an institutions metadata file, point it here
    institutions_csv = file.path("data", "institutions.csv")
  )
}
