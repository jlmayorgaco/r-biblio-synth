# ============================================================================
# 000_source_split_tree.R - Load nested R/** sources into the package namespace
# ============================================================================

rbiblio_find_project_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    has_desc <- file.exists(file.path(path, "DESCRIPTION"))
    has_r_dir <- dir.exists(file.path(path, "R"))
    if (has_desc && has_r_dir) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) {
      break
    }
    path <- parent
  }
  NULL
}

rbiblio_source_split_tree <- function(pkg_env = environment()) {
  if (exists(".__rbiblio_split_tree_loaded__.", envir = pkg_env, inherits = FALSE)) {
    return(invisible(TRUE))
  }

  project_root <- rbiblio_find_project_root(getwd())
  if (is.null(project_root)) {
    warning(
      "RBiblioSynth split-tree loader could not locate the project root. ",
      "Nested R sources were not loaded."
    )
    return(invisible(FALSE))
  }

  r_dir <- normalizePath(file.path(project_root, "R"), winslash = "/", mustWork = FALSE)
  files <- list.files(r_dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  files <- normalizePath(files, winslash = "/", mustWork = FALSE)

  rel <- files
  prefix <- paste0(r_dir, "/")
  rel[startsWith(rel, prefix)] <- substring(rel[startsWith(rel, prefix)], nchar(prefix) + 1L)
  rel <- gsub("\\\\", "/", rel)

  files <- files[grepl("/", rel, fixed = TRUE)]
  files <- files[!grepl("(^|/)(validate|test)[^/]*\\.R$", files, ignore.case = TRUE)]
  files <- sort(files)

  for (path in files) {
    sys.source(path, envir = pkg_env)
  }

  assign(".__rbiblio_split_tree_loaded__.", TRUE, envir = pkg_env)
  invisible(TRUE)
}

rbiblio_source_split_tree(environment())
