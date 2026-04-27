# RBiblioSynth: Bibliometric Analysis and Report Automation

[![R](https://img.shields.io/badge/R-%3E%3D4.0-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

**RBiblioSynth** is a modular R framework for bibliometric analysis and systematic review automation with an official public core of `M0`-`M3`:

- **M0 Data Orchestrator**: multi-source loading, harmonization, deduplication, organized tables, PRISMA automation
- **M1 Main Information**: descriptive analysis, author metrics, citations, sources, keywords, Bradford/Lotka
- **M2 Annual Production**: growth, forecasting, changepoints, diagnostics
- **M3 Countries**: production, citations, SCP/MCP, regional, economic, and spatial analysis

The package is designed around reproducible module contracts, IEEE-style plotting defaults, and an end-to-end `run_pipeline()` entrypoint.

---

## Core Features

- Multi-source ingest for Scopus, Web of Science, OpenAlex, CSV, and Excel
- Configurable deduplication with DOI, normalized title-year, and fuzzy title matching
- Canonical result contracts for `M0`-`M3`
- Full and fractional counting modes across organized data and country analysis
- Pipeline report bundle with Quarto/LaTeX-ready assets and artifact manifest

---

## Quick Start

```r
# Install in development mode, then load
# devtools::load_all()
library(RBiblioSynth)

# Define one or more sources
sources <- list(
  scopus = list(file = "data/scopus.bib", db = "scopus", format = "bibtex")
)

# Run the official end-to-end core
pipeline_result <- run_pipeline(
  sources,
  modules = c("m0", "m1", "m2", "m3"),
  config = biblio_config(
    counting_mode = "full",
    dedup_method = "title_year_normalized",
    report_format = "latex_bundle"
  )
)

# Access module results
m0_result <- pipeline_result$modules$m0
m1_result <- pipeline_result$modules$m1
m2_result <- pipeline_result$modules$m2
m3_result <- pipeline_result$modules$m3
```

---

## Public API

- `run_m0()` loads and merges bibliographic sources.
- `run_m1()`, `run_m2()`, and `run_m3()` expose stable module-level execution.
- `run_pipeline()` is the recommended end-to-end entrypoint.
- `bootstrap.R` is retained only as a contributor convenience for local development.

---

## Reproducibility

- Canonical fields used across modules include `country`, `article_count`, `total_citations`, `year`, `share`, `rank`, and `status`.
- `M0` stores source provenance for deduplicated records.
- `report_format` can generate a report bundle with Quarto and LaTeX-ready files.

---

## Installation

```r
# Development install from a local checkout
# devtools::load_all()
library(RBiblioSynth)
```

---

## Citation

`Citation metadata will be added once the software paper or archival release is finalized.`

---

## License

MIT License

## Acknowledgments

This package extends `bibliometrix` with reproducible orchestration, stronger contracts, and publication-oriented outputs for bibliometric workflows.
