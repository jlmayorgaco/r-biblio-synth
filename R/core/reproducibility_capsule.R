# ============================================================================
# reproducibility_capsule.R - Reproducibility metadata for journal artifacts
# ============================================================================

#' Build a reproducibility capsule
#'
#' @param result Module, pipeline, or B-SLR result.
#' @param config Configuration list.
#' @param protocol Optional B-SLR protocol.
#' @return List with session, config, hashes, modules, and timestamps.
#' @export
build_reproducibility_capsule <- function(result,
                                          config = biblio_config(),
                                          protocol = NULL) {
  config <- merge_biblio_config(config)

  modules <- if (inherits(result, "biblio_module_result") &&
                 identical(result$module_id %||% "", "bslr")) {
    result$data$modules %||% list()
  } else if (is.list(result) && !is.null(result$module_results)) {
    result$module_results
  } else if (is.list(result) && !is.null(result$data$modules)) {
    result$data$modules
  } else {
    list(result = result)
  }

  artifact_files <- biblio_collect_artifact_files(result)
  input_hashes <- biblio_collect_input_hashes(modules)
  data_hashes <- biblio_collect_data_hashes(modules)
  session <- utils::capture.output(utils::sessionInfo())

  summary <- data.frame(
    generated_at = as.character(Sys.time()),
    package = "RBiblioSynth",
    result_module = result$module_id %||% "unknown",
    n_modules = length(modules),
    n_artifact_files = length(artifact_files),
    n_input_hashes = length(input_hashes),
    n_data_hashes = length(data_hashes),
    status = "success",
    stringsAsFactors = FALSE
  )

  list(
    status = "success",
    generated_at = Sys.time(),
    summary = summary,
    config = config,
    protocol = protocol,
    module_status = biblio_module_status_table(modules),
    input_hashes = input_hashes,
    data_hashes = data_hashes,
    artifact_hashes = biblio_hash_files(artifact_files),
    session_info = session
  )
}

biblio_module_status_table <- function(modules) {
  if (!is.list(modules) || length(modules) == 0) {
    return(data.frame(module_id = character(), status = character(), n_rows = numeric(), stringsAsFactors = FALSE))
  }
  rows <- lapply(names(modules), function(nm) {
    x <- modules[[nm]]
    data.frame(
      module_id = x$module_id %||% nm,
      status = x$status %||% "unknown",
      n_rows = suppressWarnings(as.numeric(x$inputs$n_rows %||% x$inputs$n_total %||% NA_real_)),
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}

biblio_collect_artifact_files <- function(x) {
  files <- character()
  walk <- function(obj) {
    if (is.character(obj)) {
      candidates <- obj[file.exists(obj)]
      if (length(candidates) > 0) {
        files <<- c(files, candidates)
      }
      return(invisible(NULL))
    }
    if (is.list(obj) && length(obj) > 0) {
      lapply(obj, walk)
    }
    invisible(NULL)
  }
  walk(x$artifacts %||% x)
  unique(files)
}

biblio_collect_input_hashes <- function(modules) {
  m0 <- modules$m0 %||% list()
  source_summary <- m0$data$source_summary %||% data.frame()
  if (!is.data.frame(source_summary) || nrow(source_summary) == 0) {
    return(data.frame(path = character(), md5 = character(), exists = logical(), stringsAsFactors = FALSE))
  }

  path_cols <- intersect(names(source_summary), c("file", "path", "source_file", "source_path"))
  if (length(path_cols) == 0) {
    return(data.frame(path = character(), md5 = character(), exists = logical(), stringsAsFactors = FALSE))
  }
  paths <- unique(as.character(source_summary[[path_cols[1]]]))
  paths <- paths[nzchar(paths)]
  biblio_hash_files(paths)
}

biblio_collect_data_hashes <- function(modules) {
  rows <- list()
  for (module_id in names(modules)) {
    data_obj <- modules[[module_id]]$data %||% list()
    if (!is.list(data_obj)) next
    for (nm in names(data_obj)) {
      obj <- data_obj[[nm]]
      if (!is.data.frame(obj)) next
      rows[[length(rows) + 1L]] <- data.frame(
        module_id = module_id,
        object = nm,
        rows = nrow(obj),
        cols = ncol(obj),
        md5 = biblio_hash_object(obj),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) {
    return(data.frame(module_id = character(), object = character(), rows = integer(), cols = integer(), md5 = character(), stringsAsFactors = FALSE))
  }
  dplyr::bind_rows(rows)
}

biblio_hash_files <- function(paths) {
  paths <- unique(as.character(paths))
  paths <- paths[nzchar(paths)]
  if (length(paths) == 0) {
    return(data.frame(path = character(), md5 = character(), exists = logical(), stringsAsFactors = FALSE))
  }
  exists <- file.exists(paths)
  md5 <- rep(NA_character_, length(paths))
  if (any(exists)) {
    md5[exists] <- as.character(tools::md5sum(paths[exists]))
  }
  data.frame(path = paths, md5 = md5, exists = exists, stringsAsFactors = FALSE)
}

biblio_hash_object <- function(x) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  tryCatch({
    saveRDS(x, path)
    as.character(tools::md5sum(path))
  }, error = function(e) NA_character_)
}

reproducibility_capsule_table <- function(capsule) {
  capsule$summary %||% data.frame()
}
