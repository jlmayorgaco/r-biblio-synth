# Journal-Grade Backlog

## Purpose
This backlog turns the current review notes into an execution queue focused on a journal-oriented, methodologically rigorous release of `M0`-`M3` plus the additive `B-SLR` layer.

Status legend:
- `queued`: accepted and waiting
- `in_progress`: currently being implemented
- `blocked`: requires human input or external data
- `done`: implemented and verified

## Queue

### JG-001 `M0` multi-source CSV ingestion
- Status: `done`
- Goal: make `M0` the official, robust entrypoint for `Scopus` and `WoS` CSV exports, including multi-file exports and large files.
- Scope:
  - support `file`, `files`, and `file_pattern`
  - support `schema = scopus_csv | wos_csv | generic_csv`
  - support `chunk_size_rows`, `delimiter`, and `encoding`
  - normalize each chunk to the canonical bibliometrix-compatible schema before merge
  - preserve source provenance through file-level and record-level load
- Acceptance criteria:
  - `run_m0()` accepts multi-file `Scopus` and `WoS` CSV specs
  - repeated header rows inside chunked files are removed safely
  - downstream modules consume one canonical merged data frame
  - tests cover `files`, `file_pattern`, and chunked reads

### JG-002 `M0` PRISMA full-review mode
- Status: `done`
- Goal: move PRISMA from counts-derived reporting to review-grade methodology when human review metadata are available.
- Scope:
  - formal `screening_ledger` template
  - `search_metadata` enforcement for journal-grade mode
  - reviewer agreement including `Krippendorff's Alpha`
  - publication-grade `PRISMA` diagram in `PNG/SVG/PDF`
  - explicit partial-mode reporting when metadata are incomplete
- Acceptance criteria:
  - `counts_only` and `full_review` modes are both tested
  - no inconsistent PRISMA counts across diagram, report, and JSON
  - methods text includes search, deduplication, screening, eligibility, inclusion, and limitations
  - Scopus acceptance gate completes in `full-review` mode with reviewer agreement statistics

### JG-003 `B-SLR` journal-grade workflow hardening
- Status: `done`
- Goal: keep the current architecture and add a robust methodological layer that requires human judgment where needed and automates the rest.
- Scope:
  - stricter protocol validation
  - human-gate readiness assessment
  - methods template, journal-grade report, and action plan
  - stronger checkpoint outputs and reporting
- Acceptance criteria:
  - `run_bslr()` clearly differentiates partial vs journal-grade readiness
  - missing human inputs are surfaced as required actions, not hidden
  - journal-grade protocol can render a full manuscript bundle in source, HTML, and PDF form

### JG-004 `M1` semantic cleanup
- Status: `done`
- Goal: make `M1` outputs interpretable, normalized, and publication-ready.
- Scope:
  - full country name normalization
  - keyword cleanup and synonym consolidation
  - better citation tables with title, DOI, and source labels
  - human-readable topic labels
  - removal or demotion of exploratory outputs that do not meet paper quality
- Acceptance criteria:
  - no country codes in narrative outputs where full names are expected
  - no `term###` style topic labels in the main report
  - report tables are readable without manual post-processing

### JG-005 `M2` journal closure
- Status: `done`
- Goal: close `M2` as a robust temporal engine with interpretable model selection, strong diagnostics, and publication-grade outputs.
- Scope:
  - headline interpretable growth model vs flexible benchmark
  - advanced structural break and forecasting hypotheses
  - cleaned JSON exports and stronger manuscript-facing report narrative
  - bulletproof `IEEE` plots and ranking tables
- Acceptance criteria:
  - reports surface model limitations honestly
  - forecasting comparison uses consistent evidence
  - plots and tables are paper-ready without manual resizing

### JG-006 `M3` no-data handling and narrative polish
- Status: `done`
- Goal: keep `M3` analytically rich while removing empty or misleading outputs.
- Scope:
  - pre-validation in every renderer
  - placeholder policy for insufficient data
  - stronger quadrants, share trends, rank mobility, and emergence narratives
  - no runtime dependency installation attempts
- Acceptance criteria:
  - no broken or empty charts in core outputs
  - manifest reports omitted sections with reasons

### JG-007 Automated paper assembly
- Status: `done`
- Goal: assemble methods, results, appendices, tables, and figures into a reproducible journal-oriented package.
- Scope:
  - `Quarto` or `LaTeX` assembly templates
  - section scaffolds for methods, results, discussion, limitations, and appendices
  - artifact manifest integrated into the manuscript bundle
- Acceptance criteria:
  - one command can assemble a draft paper bundle from `M0`-`M3` and `B-SLR`
  - figures and tables are consistently referenced and publication-ready

### JG-008 Package and release hygiene
- Status: `done`
- Goal: remove the remaining developer-facing rough edges before calling the core release-ready.
- Scope:
  - reduce `load_all()` conflict noise in normal development sessions
  - harden `R/000_package_flat.R` regeneration workflow
  - align exports, manifests, and generated docs
  - run `R CMD check --no-manual` cleanly for the official core
- Acceptance criteria:
  - `pkgload::load_all('.')` does not emit misleading conflict guidance in a clean session
  - `R CMD check --no-manual` is clean with `0 ERROR`, `0 WARNING`, and `0 NOTE`

### JG-009 Paper assembly v2
- Status: `done`
- Goal: turn the existing manuscript bundle into a submission-grade package.
- Scope:
  - strengthen Quarto/LaTeX templates
  - add cross-references for figures, tables, and appendices
  - add methods/results/discussion scaffolds that consume `M0`-`M3` and `B-SLR` artifacts directly
  - improve appendix assembly for hypotheses, diagnostics, and PRISMA materials
- Acceptance criteria:
  - the paper bundle renders to a coherent journal-style manuscript without manual reorganization
  - figures/tables are referenced consistently and appendices are publication-ready

### JG-010 Example acceptance gate
- Status: `done`
- Goal: make the Scopus power-systems example the standing end-to-end acceptance gate.
- Scope:
  - rerun `examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR`
  - verify `M0` PRISMA consistency, `M1` semantics, `M2` journal report, and `M3` narrative quality
  - snapshot key artifacts and core report facts
- Acceptance criteria:
  - the example completes end-to-end with no broken core artifacts
  - the generated reports and figures are suitable for journal-oriented review without ad hoc cleanup
  - the gate emits a reproducible snapshot and renders the submission bundle in HTML and PDF
