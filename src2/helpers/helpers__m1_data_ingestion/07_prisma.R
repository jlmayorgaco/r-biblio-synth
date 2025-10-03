# ============================================================================ #
# helpers__m1_data_ingestion/07_prisma.R
# PRISMA counters (auto + manual overrides)
# ============================================================================ #

m1i_compute_prisma_counts <- function(df, filters, manual_overrides) {

  n_identified_db    <- nrow(df)                 # post-merge count (approximate)
  n_identified_other <- as.integer(manual_overrides$identified_other %||% 0)

  DOI <- tolower(ifelse(is.na(df$DOI), "", df$DOI))
  TitleNorm <- stringr::str_squish(tolower(ifelse(is.na(df$Title), "", df$Title)))
  key <- ifelse(DOI != "", paste0("doi:", DOI), paste0("ti:", TitleNorm))

  n_unique     <- length(unique(key))
  n_duplicates <- max((n_identified_db + n_identified_other) - n_unique, 0)

  lang_keep  <- filters$language %||% "English"
  types_keep <- filters$doc_types_keep %||% c("Article","Conference Paper")

  lang_ok <- df$Language %in% lang_keep
  type_ok <- df$Document_Type %in% types_keep

  n_auto_excluded <- sum(!(lang_ok & type_ok), na.rm = TRUE)
  n_other_removed <- as.integer(manual_overrides$removed_other %||% 0)

  n_screened <- as.integer(manual_overrides$screened %||%
                             (n_unique - n_duplicates - n_auto_excluded - n_other_removed))
  if (is.na(n_screened) || n_screened < 0) n_screened <- 0

  n_sought       <- as.integer(manual_overrides$sought %||% NA_integer_)
  n_notretrieved <- as.integer(manual_overrides$not_retrieved %||% 0)
  n_assessed     <- as.integer(manual_overrides$assessed %||%
                                 if (!is.na(n_sought)) n_sought - n_notretrieved else NA_integer_)
  n_excluded_scr <- as.integer(manual_overrides$excluded_screen %||% NA_integer_)
  n_excluded_ft  <- as.integer(manual_overrides$excluded_fulltext %||% NA_integer_)
  n_included     <- as.integer(manual_overrides$included %||% NA_integer_)

  list(
    identified = list(db = n_identified_db, other = n_identified_other),
    removed    = list(duplicates = n_duplicates, auto = max(n_auto_excluded, 0), other = max(n_other_removed, 0)),
    screening  = list(screened = n_screened, excluded = n_excluded_scr),
    eligibility= list(sought = n_sought, not_retrieved = n_notretrieved, assessed = n_assessed),
    included   = list(studies = n_included),
    filters    = filters
  )
}
