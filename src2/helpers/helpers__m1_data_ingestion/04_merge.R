# ============================================================================ #
# helpers__m1_data_ingestion/04_merge.R
# Merge Scopus + WoS with dedup (DOI/title heuristic)
# ============================================================================ #

m1i_merge_sources <- function(M_scopus, M_wos, dbsource_fallback = NULL) {
  # set DB markers when single source
  if (is.null(M_scopus) && is.null(M_wos)) stop("[M1] Nothing to merge: both sources are NULL.")

  if (!is.null(M_scopus) && is.null(M_wos)) {
    attr(M_scopus, "DB") <- "SCOPUS"
    return(M_scopus)
  }
  if (is.null(M_scopus) && !is.null(M_wos)) {
    attr(M_wos, "DB") <- "ISI"
    return(M_wos)
  }

  cat("[M1] Merging Scopus + WoS and removing duplicates ...\n")

  # Prefer bibliometrix native merger when available
  if ("mergeDbSources" %in% getNamespaceExports("bibliometrix")) {
    out <- tryCatch(
      bibliometrix::mergeDbSources(M_scopus, M_wos, remove.duplicated = TRUE),
      error = function(e) NULL
    )
    if (!is.null(out)) return(out)
  }

  # Fallback: bind by shared columns and de-duplicate by DOI/Title
  shared <- intersect(names(M_scopus), names(M_wos))
  all <- tryCatch(
    rbind(M_scopus[, shared, drop = FALSE], M_wos[, shared, drop = FALSE]),
    error = function(e) stop("[M1] Could not bind Scopus and WoS frames: ", conditionMessage(e))
  )

  DOI <- tolower(ifelse(is.na(all$DI) | all$DI == "", ifelse("DO" %in% names(all), all$DO, ""), all$DI))
  TI  <- if ("TI" %in% names(all)) all$TI else if ("Title" %in% names(all)) all$Title else ""
  TitleNorm <- stringr::str_squish(tolower(ifelse(is.na(TI), "", TI)))
  key <- ifelse(DOI != "", paste0("doi:", DOI), paste0("ti:", TitleNorm))
  keep <- !duplicated(key)
  all <- all[keep, , drop = FALSE]

  # mark DB attribute
  attr(all, "DB") <- "MULTI"
  class(all) <- unique(c("bibliometrixData", class(all)))
  all
}
