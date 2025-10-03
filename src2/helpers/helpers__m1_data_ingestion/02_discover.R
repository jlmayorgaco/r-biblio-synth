# ============================================================================ #
# helpers__m1_data_ingestion/02_discover.R
# Auto-discover Scopus/WoS files
# ============================================================================ #

m1i_discover_sources <- function(input_dir, auto_discover = TRUE) {
  if (!isTRUE(auto_discover)) {
    return(list(scopus_files = character(0), wos_files = character(0)))
  }
  if (is.null(input_dir) || !dir.exists(input_dir)) {
    return(list(scopus_files = character(0), wos_files = character(0)))
  }

  scopus_files <- list.files(input_dir, pattern = "^scopus_.*\\.bib$", full.names = TRUE, ignore.case = TRUE)
  wos_files    <- list.files(input_dir, pattern = "^wos_.*\\.txt$",    full.names = TRUE, ignore.case = TRUE)

  list(scopus_files = sort(scopus_files), wos_files = sort(wos_files))
}
