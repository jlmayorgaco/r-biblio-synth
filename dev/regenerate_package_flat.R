# ============================================================================
# regenerate_package_flat.R - Rebuild dev/generated/000_package_flat.R from split sources
# ============================================================================

args <- commandArgs(trailingOnly = TRUE)
project_root <- if (length(args) >= 1L) normalizePath(args[1], winslash = "/", mustWork = FALSE) else normalizePath(getwd(), winslash = "/", mustWork = FALSE)

bootstrap_path <- file.path(project_root, "R", "core", "bootstrap.R")
if (!file.exists(bootstrap_path)) {
  stop("Could not find R/core/bootstrap.R under: ", project_root)
}

source(bootstrap_path, local = TRUE)

all_r_files <- rbiblio_collect_r_files(project_root = project_root, include_flat = FALSE)
r_root <- normalizePath(file.path(project_root, "R"), winslash = "/", mustWork = FALSE)
rel_paths <- normalizePath(all_r_files, winslash = "/", mustWork = FALSE)
prefix <- paste0(r_root, "/")
rel_paths[startsWith(rel_paths, prefix)] <- substring(rel_paths[startsWith(rel_paths, prefix)], nchar(prefix) + 1L)
rel_paths <- gsub("\\\\", "/", rel_paths)
bootstrap_source_path <- normalizePath(file.path(project_root, "R", "core", "bootstrap.R"), winslash = "/", mustWork = FALSE)
r_files <- unique(c(bootstrap_source_path, all_r_files[grepl("/", rel_paths, fixed = TRUE)]))
generated_dir <- file.path(project_root, "dev", "generated")
flat_path <- file.path(generated_dir, "000_package_flat.R")
legacy_flat_path <- file.path(project_root, "R", "000_package_flat.R")

header <- c(
  "# ============================================================================",
  "# 000_package_flat.R - Generated flat package source (development artifact)",
  "#",
  paste0("# Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "# Source of truth: split files under nested R/** directories.",
  "# Root-level files remain separately loadable by the package and are not duplicated here.",
  "# Regenerate via: Rscript dev/regenerate_package_flat.R",
  "# ============================================================================",
  ""
)

chunks <- lapply(r_files, function(path) {
  rel <- normalizePath(path, winslash = "/", mustWork = FALSE)
  project_root_norm <- normalizePath(project_root, winslash = "/", mustWork = FALSE)
  project_prefix <- paste0(project_root_norm, "/")
  if (startsWith(rel, project_prefix)) {
    rel <- substring(rel, nchar(project_prefix) + 1L)
  }
  c(
    paste0("# >>> BEGIN FILE: ", gsub("\\\\", "/", rel)),
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    paste0("# <<< END FILE: ", gsub("\\\\", "/", rel)),
    ""
  )
})

dir.create(dirname(flat_path), recursive = TRUE, showWarnings = FALSE)
writeLines(c(header, unlist(chunks, use.names = FALSE)), flat_path, useBytes = TRUE)
message("Regenerated flat package file: ", flat_path)

if (file.exists(legacy_flat_path)) {
  unlink(legacy_flat_path, force = TRUE)
  message("Removed legacy in-package flat artifact: ", legacy_flat_path)
}
