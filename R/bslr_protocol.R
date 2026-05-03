# ============================================================================
# bslr_protocol.R - B-SLR protocol templates and validation
# ============================================================================

#' Create a B-SLR protocol template
#'
#' Returns a structured template that captures the human-defined parts of the
#' Bibliometric-Systematic Literature Review workflow. The protocol is designed
#' to sit on top of `M0`-`M3`, not replace them.
#'
#' @param title Review title.
#' @return A named list representing a B-SLR protocol.
#' @export
bslr_protocol_template <- function(title = "B-SLR Study") {
  list(
    title = title,
    review_topic = NULL,
    overview_scan_date = NULL,
    research_gap = NULL,
    research_question = NULL,
    objectives = character(),
    inclusion_criteria = character(),
    exclusion_criteria = character(),
    search = list(
      primary_database = NULL,
      secondary_databases = character(),
      queries = list(primary = NULL, by_database = list()),
      search_string_validation = NULL,
      database_rationale = NULL,
      first_run_date = NULL,
      crosscheck_date = NULL,
      time_span = list(start = NULL, end = NULL),
      language = "English",
      document_types = c("article"),
      notes = character()
    ),
    screening = list(
      reviewers = character(),
      conflict_rule = "discussion and consensus",
      screening_tool = NULL,
      quality_tool = NULL,
      agreement_target = 0.80,
      notes = character()
    ),
    data_extraction = list(
      fields = c("aim", "theory_lens", "method", "data_context", "key_findings", "limitations", "contribution"),
      notes = character()
    ),
    bibliometric = list(
      approach = "bibliographic_coupling",
      secondary_approach = "co_citation",
      min_cluster_size = 5L,
      resolution = 1,
      ranking_rule = "hybrid",
      sample_top_n = 5L,
      cluster_labeling_process = NULL,
      sample_selection_criteria = NULL,
      notes = character()
    ),
    slr = list(
      holistic_questions = character(),
      cluster_questions = character(),
      notes = character()
    ),
    theorising = list(
      modes = c("research_agenda", "taxonomy", "conceptual_framework"),
      perimeter = NULL,
      rationale = NULL,
      contribution_goal = NULL,
      notes = character()
    )
  )
}

#' Read a B-SLR protocol from disk or memory
#'
#' @param protocol Either a file path (JSON or YAML) or a list.
#' @return A normalized protocol list.
#' @export
bslr_read_protocol <- function(protocol) {
  if (is.character(protocol) && length(protocol) == 1 && file.exists(protocol)) {
    ext <- tolower(tools::file_ext(protocol))
    parsed <- switch(
      ext,
      json = jsonlite::fromJSON(protocol, simplifyVector = FALSE),
      yml = {
        if (!requireNamespace("yaml", quietly = TRUE)) {
          cli::cli_abort("Package 'yaml' is required to read YAML B-SLR protocols.")
        }
        yaml::read_yaml(protocol)
      },
      yaml = {
        if (!requireNamespace("yaml", quietly = TRUE)) {
          cli::cli_abort("Package 'yaml' is required to read YAML B-SLR protocols.")
        }
        yaml::read_yaml(protocol)
      },
      cli::cli_abort("Unsupported B-SLR protocol format: {ext}.")
    )
    return(bslr_merge_protocol_with_template(parsed))
  }

  if (is.list(protocol)) {
    return(bslr_merge_protocol_with_template(protocol))
  }

  cli::cli_abort("protocol must be a list or a JSON/YAML path.")
}

#' Validate a B-SLR protocol
#'
#' @param protocol Protocol list or JSON/YAML path.
#' @return A validation list with completeness and missing field metadata.
#' @export
validate_bslr_protocol <- function(protocol) {
  protocol <- bslr_read_protocol(protocol)

  required_checks <- list(
    review_topic = protocol$review_topic,
    research_question = protocol$research_question,
    inclusion_criteria = protocol$inclusion_criteria,
    exclusion_criteria = protocol$exclusion_criteria,
    primary_database = protocol$search$primary_database,
    primary_query = protocol$search$queries$primary,
    first_run_date = protocol$search$first_run_date,
    time_span_start = protocol$search$time_span$start,
    time_span_end = protocol$search$time_span$end,
    language = protocol$search$language,
    document_types = protocol$search$document_types,
    bibliometric_approach = protocol$bibliometric$approach
  )

  human_review_checks <- list(
    reviewers = protocol$screening$reviewers,
    conflict_rule = protocol$screening$conflict_rule,
    screening_tool = protocol$screening$screening_tool,
    quality_tool = protocol$screening$quality_tool
  )

  journal_grade_checks <- list(
    overview_scan_date = protocol$overview_scan_date,
    objectives = protocol$objectives,
    secondary_databases = protocol$search$secondary_databases,
    crosscheck_date = protocol$search$crosscheck_date,
    search_string_validation = protocol$search$search_string_validation,
    database_rationale = protocol$search$database_rationale,
    data_extraction_fields = protocol$data_extraction$fields,
    cluster_labeling_process = protocol$bibliometric$cluster_labeling_process,
    sample_selection_criteria = protocol$bibliometric$sample_selection_criteria,
    theorising_perimeter = protocol$theorising$perimeter,
    theorising_rationale = protocol$theorising$rationale,
    contribution_goal = protocol$theorising$contribution_goal
  )

  required_missing <- names(required_checks)[vapply(required_checks, bslr_is_empty_field, logical(1))]
  human_missing <- names(human_review_checks)[vapply(human_review_checks, bslr_is_empty_field, logical(1))]
  journal_missing <- names(journal_grade_checks)[vapply(journal_grade_checks, bslr_is_empty_field, logical(1))]

  completeness_total <- length(required_checks) + length(human_review_checks) + length(journal_grade_checks)
  completeness_done <- completeness_total - length(required_missing) - length(human_missing) - length(journal_missing)
  completeness <- safe_divide(completeness_done, completeness_total, default = 0)

  maturity_band <- if (length(required_missing) > 0) {
    "protocol_incomplete"
  } else if (length(human_missing) > 0) {
    "core_protocol_ready_human_review_incomplete"
  } else if (length(journal_missing) > 0) {
    "full_review_ready_journal_enrichment_incomplete"
  } else {
    "journal_protocol_ready"
  }

  list(
    ok = length(required_missing) == 0,
    protocol = protocol,
    completeness = completeness,
    missing_required = required_missing,
    missing_human_review = human_missing,
    missing_journal_grade = journal_missing,
    human_gate_ready = length(human_missing) == 0,
    journal_ready = length(required_missing) == 0 && length(human_missing) == 0 && length(journal_missing) == 0,
    maturity_band = maturity_band,
    required_actions = c(
      if (length(required_missing) > 0) {
        sprintf("Populate protocol field(s): %s", paste(required_missing, collapse = ", "))
      },
      if (length(human_missing) > 0) {
        sprintf("Add human-review metadata: %s", paste(human_missing, collapse = ", "))
      },
      if (length(journal_missing) > 0) {
        sprintf("Enrich journal-grade protocol fields: %s", paste(journal_missing, collapse = ", "))
      }
    )
  )
}

#' Convert a B-SLR protocol to M0 search metadata
#'
#' @param protocol A B-SLR protocol list or path.
#' @return A normalized search metadata list.
#' @export
bslr_protocol_to_search_metadata <- function(protocol) {
  protocol <- bslr_read_protocol(protocol)

  list(
    title = protocol$title %||% NULL,
    review_topic = protocol$review_topic %||% NULL,
    overview_scan_date = protocol$overview_scan_date %||% NULL,
    research_gap = protocol$research_gap %||% NULL,
    research_question = protocol$research_question %||% NULL,
    objectives = protocol$objectives %||% character(),
    inclusion_criteria = protocol$inclusion_criteria %||% character(),
    exclusion_criteria = protocol$exclusion_criteria %||% character(),
    primary_database = protocol$search$primary_database %||% NULL,
    secondary_databases = protocol$search$secondary_databases %||% character(),
    queries = protocol$search$queries %||% list(),
    first_run_date = protocol$search$first_run_date %||% NULL,
    crosscheck_date = protocol$search$crosscheck_date %||% NULL,
    time_span = protocol$search$time_span %||% list(start = NULL, end = NULL),
    language = protocol$search$language %||% NULL,
    document_types = protocol$search$document_types %||% character(),
    reviewers = protocol$screening$reviewers %||% character(),
    conflict_rule = protocol$screening$conflict_rule %||% NULL,
    screening_tool = protocol$screening$screening_tool %||% NULL,
    quality_tool = protocol$screening$quality_tool %||% NULL,
    agreement_target = protocol$screening$agreement_target %||% NULL,
    data_extraction_fields = protocol$data_extraction$fields %||% character(),
    bibliometric_approach = protocol$bibliometric$approach %||% NULL,
    secondary_approach = protocol$bibliometric$secondary_approach %||% NULL,
    min_cluster_size = protocol$bibliometric$min_cluster_size %||% NULL,
    resolution = protocol$bibliometric$resolution %||% NULL,
    ranking_rule = protocol$bibliometric$ranking_rule %||% NULL,
    cluster_labeling_process = protocol$bibliometric$cluster_labeling_process %||% NULL,
    sample_selection_criteria = protocol$bibliometric$sample_selection_criteria %||% NULL,
    theorising_modes = protocol$theorising$modes %||% character(),
    theorising_perimeter = protocol$theorising$perimeter %||% NULL,
    theorising_rationale = protocol$theorising$rationale %||% NULL,
    contribution_goal = protocol$theorising$contribution_goal %||% NULL
  )
}

#' Apply B-SLR protocol metadata onto M0 source specifications
#'
#' @param sources Named list of M0 source specs.
#' @param protocol B-SLR protocol list or path.
#' @return Sources enriched with query/search metadata.
#' @export
bslr_apply_protocol_to_sources <- function(sources, protocol) {
  protocol <- bslr_read_protocol(protocol)

  if (!is.list(sources) || length(sources) == 0) {
    return(sources)
  }

  primary_db <- tolower(trimws(as.character(protocol$search$primary_database %||% "")))
  secondary_dbs <- unique(tolower(trimws(as.character(protocol$search$secondary_databases %||% character()))))
  query_map <- protocol$search$queries$by_database %||% list()
  primary_query <- protocol$search$queries$primary %||% NULL

  for (nm in names(sources)) {
    spec <- sources[[nm]] %||% list()
    db <- tolower(trimws(as.character(spec$db %||% "")))

    if (is.null(spec$query) || !nzchar(trimws(as.character(spec$query)))) {
      if (db %in% names(query_map) && !bslr_is_empty_field(query_map[[db]])) {
        spec$query <- query_map[[db]]
      } else if (identical(db, primary_db) && !bslr_is_empty_field(primary_query)) {
        spec$query <- primary_query
      }
    }

    if (is.null(spec$search_date) || !nzchar(trimws(as.character(spec$search_date)))) {
      spec$search_date <- if (identical(db, primary_db)) {
        protocol$search$first_run_date %||% NA_character_
      } else if (db %in% secondary_dbs) {
        protocol$search$crosscheck_date %||% protocol$search$first_run_date %||% NA_character_
      } else {
        spec$search_date %||% NA_character_
      }
    }

    if (is.null(spec$prisma_role)) {
      spec$prisma_role <- if (db %in% c(primary_db, secondary_dbs)) "database" else m0_resolve_prisma_role(spec)
    }

    sources[[nm]] <- spec
  }

  sources
}

bslr_merge_protocol_with_template <- function(protocol) {
  template <- bslr_protocol_template(protocol$title %||% "B-SLR Study")
  m0_recursive_fill(template, protocol %||% list())
}

bslr_is_empty_field <- function(x) {
  if (is.null(x)) return(TRUE)
  if (is.character(x) && length(x) == 1) return(!nzchar(trimws(x)))
  if (is.character(x) || is.numeric(x) || is.logical(x)) return(length(x) == 0)
  if (is.list(x)) return(length(x) == 0)
  FALSE
}
