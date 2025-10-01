# Loads all helper files in deterministic order
m3c_source_helpers <- function(dir = "src2/helpers/helpers__m3_countries") {
  stopifnot(dir.exists(dir))
  files <- list.files(dir, pattern = "^[0-9]{2}_.+\\.R$", full.names = TRUE)
  files <- files[order(basename(files))]
  for (f in files) sys.source(f, envir = parent.frame())
  invisible(TRUE)
}
