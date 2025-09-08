# ================================================================
# io_helpers.r — Input/Output Utility Functions
# ================================================================
suppressPackageStartupMessages({
  library(jsonlite)
  library(ggplot2)
})

# ------------------------------------------------
# Internal: ensure directory exists
# ------------------------------------------------
.ensure_dir <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
}

# ------------------------------------------------
# Core Helpers (SRP: each does one thing)
# ------------------------------------------------

# Save list/data.frame as JSON
export_json <- function(data, path, pretty = TRUE, quiet = FALSE) {
  .ensure_dir(path)
  jsonlite::write_json(data, path, pretty = pretty, auto_unbox = TRUE)
  if (!quiet) cat("[io_helpers] JSON saved:", path, "\n")
}

# Save data.frame as CSV
export_csv <- function(data, path, quiet = FALSE) {
  .ensure_dir(path)
  utils::write.csv(data, path, row.names = FALSE)
  if (!quiet) cat("[io_helpers] CSV saved:", path, "\n")
}

# Load JSON
load_json <- function(path) {
  if (!file.exists(path)) stop("[io_helpers] File not found:", path)
  jsonlite::fromJSON(path)
}

# Load CSV
load_csv <- function(path) {
  if (!file.exists(path)) stop("[io_helpers] File not found:", path)
  utils::read.csv(path, stringsAsFactors = FALSE)
}

# Save ggplot with defaults (PNG/SVG/PDF)
save_plot <- function(plot, filename, device = NULL,
                      width = 3, height = 2, dpi = 300,
                      quiet = FALSE) {
  if (is.null(device)) {
    ext <- tools::file_ext(filename)
    device <- ifelse(ext %in% c("png", "svg", "pdf"), ext, "png")
  }
  .ensure_dir(filename)
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    device = device,
    width = width,
    height = height,
    dpi = dpi
  )
  if (!quiet) cat("[io_helpers] Plot saved:", filename, "\n")
}

# ------------------------------------------------
# Optional: Group under a list for namespaced usage
# ------------------------------------------------
io_helpers <- list(
  export_json = export_json,
  export_csv  = export_csv,
  load_json   = load_json,
  load_csv    = load_csv,
  save_plot   = save_plot
)
