# ============================================================================
# bslr_run.R - Official B-SLR workflow layer on top of M0-M3
# ============================================================================

#' Run the official B-SLR workflow
#'
#' This workflow adds a human-guided methodological layer on top of the public
#' `M0`-`M3` core. It validates the user-provided protocol, requires the human
#' review perimeter to be stated explicitly, then orchestrates M0-M3 and
#' prepares bibliometric mapping, sample ranking, and SLR/theorising scaffolds.
#'
#' @param sources Named list of bibliographic source specifications accepted by
#'   \code{\link{run_m0}}.
#' @param protocol B-SLR protocol list or path to a JSON/YAML file.
#' @param screening_ledger Optional screening ledger passed to \code{\link{run_m0}}.
#' @param prisma_spec PRISMA specification or \code{"auto"}.
#' @param modules Optional downstream modules to execute. Supported values:
#'   \code{"m1"}, \code{"m2"}, \code{"m3"}.
#' @param config Configuration list.
#' @param export Logical. If TRUE, export workflow artifacts.
#' @return A \code{biblio_module_result} representing the B-SLR workflow.
#' @export
run_bslr <- function(sources,
                     protocol,
                     screening_ledger = NULL,
                     prisma_spec = "auto",
                     modules = c("m1", "m2", "m3"),
                     config = biblio_config(),
                     export = TRUE) {
  config <- merge_biblio_config(config)
  protocol_validation <- validate_bslr_protocol(protocol)
  protocol <- protocol_validation$protocol

  if (!isTRUE(protocol_validation$ok) && isTRUE(config$validate_strict)) {
    cli::cli_abort(
      "B-SLR protocol validation failed. Missing required field(s): {paste(protocol_validation$missing_required, collapse = ', ')}"
    )
  }

  search_metadata <- bslr_protocol_to_search_metadata(protocol)
  enriched_sources <- bslr_apply_protocol_to_sources(sources, protocol)

  module_results <- list()
  module_results$m0 <- run_m0(
    sources = enriched_sources,
    config = config,
    prisma_spec = prisma_spec,
    screening_ledger = screening_ledger,
    search_metadata = search_metadata,
    export = export
  )

  bib_data <- m0_get_bib_data(module_results$m0)
  if (is.null(bib_data) || !is.data.frame(bib_data) || nrow(bib_data) == 0) {
    cli::cli_abort("B-SLR aborted: M0 did not produce a usable refined dataset.")
  }

  modules <- unique(tolower(as.character(modules)))
  if ("m1" %in% modules) {
    module_results$m1 <- run_m1(bib_data, config = config, export = export)
  }
  if ("m2" %in% modules) {
    module_results$m2 <- run_m2(m0_prepare_m2_input(module_results$m0), config = config, export = export)
  }
  if ("m3" %in% modules) {
    module_results$m3 <- run_m3(bib_data, config = config, export = export)
  }

  bibliometric_map <- bslr_build_bibliometric_map(bib_data, protocol, config)
  checkpoints <- bslr_build_checkpoints(protocol_validation, module_results$m0, bibliometric_map, screening_ledger)
  slr_scaffolds <- bslr_build_slr_scaffolds(protocol, bibliometric_map)
  journal_assessment <- bslr_build_journal_grade_assessment(
    protocol_validation = protocol_validation,
    m0_result = module_results$m0,
    bibliometric_map = bibliometric_map,
    checkpoints = checkpoints,
    scaffolds = slr_scaffolds
  )
  parity <- run_bibliometrix_parity(bib_data, module_results, config)
  claim_ledger <- build_claim_ledger(module_results, journal_assessment, config)
  reproducibility <- build_reproducibility_capsule(
    result = list(module_id = "bslr", data = list(modules = module_results), artifacts = list()),
    config = config,
    protocol = protocol
  )
  quality_gate <- bslr_build_quality_gate(
    protocol_validation = protocol_validation,
    m0_result = module_results$m0,
    module_results = module_results,
    journal_assessment = journal_assessment,
    claim_ledger = claim_ledger,
    parity = parity,
    reproducibility = reproducibility
  )
  journal_assessment$quality_gate <- quality_gate
  journal_assessment$claim_summary <- data.frame(
    claims = if (is.data.frame(claim_ledger)) nrow(claim_ledger) else 0L,
    strong = if (is.data.frame(claim_ledger) && "strength" %in% names(claim_ledger)) sum(claim_ledger$strength == "strong", na.rm = TRUE) else 0L,
    moderate = if (is.data.frame(claim_ledger) && "strength" %in% names(claim_ledger)) sum(claim_ledger$strength == "moderate", na.rm = TRUE) else 0L,
    inconclusive = if (is.data.frame(claim_ledger) && "strength" %in% names(claim_ledger)) sum(claim_ledger$strength == "inconclusive", na.rm = TRUE) else 0L,
    stringsAsFactors = FALSE
  )
  journal_assessment$claim_ledger <- claim_ledger
  journal_assessment$bibliometrix_parity <- parity
  journal_assessment$reproducibility <- reproducibility

  methods_report <- bslr_build_methods_report(
    protocol = protocol,
    protocol_validation = protocol_validation,
    m0_result = module_results$m0,
    bibliometric_map = bibliometric_map,
    checkpoints = checkpoints,
    journal_assessment = journal_assessment
  )
  methods_template_report <- bslr_build_methods_template_report(
    protocol = protocol,
    protocol_validation = protocol_validation,
    m0_result = module_results$m0,
    bibliometric_map = bibliometric_map,
    journal_assessment = journal_assessment
  )
  executive_report <- bslr_build_executive_report(
    protocol,
    module_results,
    bibliometric_map,
    checkpoints,
    journal_assessment = journal_assessment
  )
  manuscript_report <- bslr_build_manuscript_scaffold_report(
    protocol,
    bibliometric_map,
    slr_scaffolds,
    module_results = module_results,
    journal_assessment = journal_assessment
  )
  journal_grade_report <- bslr_build_journal_grade_report(
    protocol = protocol,
    module_results = module_results,
    bibliometric_map = bibliometric_map,
    journal_assessment = journal_assessment,
    scaffolds = slr_scaffolds
  )
  tables <- bslr_build_tables(
    protocol_validation,
    checkpoints,
    bibliometric_map,
    slr_scaffolds,
    journal_assessment = journal_assessment
  )
  tables$claim_ledger <- claim_ledger
  tables$quality_gate_summary <- quality_gate$summary %||% data.frame()
  tables$quality_gate_checklist <- quality_gate$checklist %||% data.frame()
  tables$bibliometrix_parity_summary <- parity$summary %||% data.frame()
  tables$bibliometrix_parity_divergences <- parity$divergences %||% data.frame()
  tables$reproducibility_summary <- reproducibility$summary %||% data.frame()
  tables$reproducibility_module_status <- reproducibility$module_status %||% data.frame()
  tables$reproducibility_input_hashes <- reproducibility$input_hashes %||% data.frame()
  tables$reproducibility_data_hashes <- reproducibility$data_hashes %||% data.frame()
  if (is.data.frame(module_results$m0$data$screening_template)) {
    tables$screening_ledger_template <- module_results$m0$data$screening_template
  }
  screening_summary <- module_results$m0$data$screening_summary %||% list()
  if (is.data.frame(screening_summary$consensus)) {
    tables$screening_consensus <- screening_summary$consensus
  }
  if (is.data.frame(screening_summary$stage_counts)) {
    tables$screening_stage_counts <- screening_summary$stage_counts
  }
  if (is.data.frame(screening_summary$exclusion_reasons)) {
    tables$screening_exclusion_reasons <- screening_summary$exclusion_reasons
  }
  if (is.data.frame(screening_summary$decision_matrix)) {
    tables$screening_decision_matrix <- screening_summary$decision_matrix
  }
  rendered <- bslr_render_plots(bibliometric_map, checkpoints, config)

  status <- bslr_compute_status(protocol_validation, module_results, bibliometric_map)
  result <- new_module_result(
    module_id = "bslr",
    module_name = "Bibliometric-Systematic Literature Review Workflow",
    status = status,
    inputs = list(
      n_sources = length(enriched_sources),
      requested_modules = modules
    ),
    data = list(
      protocol = protocol,
      protocol_validation = protocol_validation,
      search_metadata = search_metadata,
      checkpoints = checkpoints,
      journal_assessment = journal_assessment,
      claim_ledger = claim_ledger,
      quality_gate = quality_gate,
      bibliometrix_parity = parity,
      reproducibility = reproducibility,
      bibliometric_map = bibliometric_map,
      slr_scaffolds = slr_scaffolds,
      modules = module_results
    ),
    diagnostics = list(
      warnings = character(),
      checks = list(protocol = protocol_validation),
      notes = unique(c(
        journal_assessment$pending_actions %||% protocol_validation$required_actions %||% character(),
        quality_gate$blocking_issues %||% character()
      ))
    )
  )

  result$artifacts$plots <- rendered$plots %||% list()
  result$artifacts$tables <- tables
  result$artifacts$reports <- list(
    methods = methods_report,
    methods_template = methods_template_report,
    executive = executive_report,
    manuscript = manuscript_report,
    journal_grade = journal_grade_report,
    quality_gate = bslr_build_quality_gate_report(quality_gate),
    claim_ledger = bslr_build_claim_ledger_report(claim_ledger)
  )

  class(result) <- unique(c("bslr_workflow_result", class(result)))

  if (export) {
    exported <- export_bslr(result, config)
    manifest <- bslr_build_manifest(result, exported, config)
    result <- attach_manifest_to_result(result, manifest)
  }
  result
}

bslr_compute_status <- function(protocol_validation, module_results, bibliometric_map) {
  statuses <- vapply(module_results, function(x) x$status %||% "unknown", character(1))
  screening_ok <- isTRUE(module_results$m0$data$screening_summary$ok %||% FALSE)
  if (any(statuses == "error") || grepl("^error", bibliometric_map$status %||% "")) {
    return("error")
  }
  if (!isTRUE(protocol_validation$ok) ||
      length(protocol_validation$missing_human_review %||% character()) > 0 ||
      length(protocol_validation$missing_journal_grade %||% character()) > 0 ||
      !screening_ok ||
      any(statuses == "warning") ||
      identical(bibliometric_map$status, "warning")) {
    return("warning")
  }
  "success"
}
