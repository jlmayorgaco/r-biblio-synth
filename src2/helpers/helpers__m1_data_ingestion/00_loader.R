# ============================================================================ #
# helpers__m1_data_ingestion/00_loader.R
# Loader for all M1 helpers (id: m1i_*). Safe to source multiple times.
# ============================================================================ #

m1i_source_helpers <- function(dir = file.path("helpers__m1_data_ingestion")) {
  if (!dir.exists(dir)) stop("[M1] helpers dir not found: ", dir)

  files <- c(
    "01_utils.R",
    "02_discover.R",
    "03_convert.R",
    "04_merge.R",
    "05_mapping.R",
    "06_eda.R",
    "07_prisma.R"
  )
  for (f in files) {
    fp <- file.path(dir, f)
    if (!file.exists(fp)) stop("[M1] Missing helper file: ", fp)
    sys.source(fp, envir = topenv())
  }
  invisible(TRUE)
}
