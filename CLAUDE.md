# CLAUDE.md - RBiblioSynth Code Review

## Project Overview
- **Name**: RBiblioSynth - Bibliometric Analysis and Report Automation
- **Language**: R (R6 classes)
- **Purpose**: Modular framework for automating bibliometric analysis and systematic review reporting
- **Dependencies**: bibliometrix, ggplot2, jsonlite, dplyr, zoo, lomb, WaveletComp

---

## Architecture Summary

### Module Structure
| Module | Status | Quality | Notes |
|--------|--------|---------|-------|
| **M0** Data Orchestrator | **Working** | Good | Load, merge, organize sources; PRISMA diagram & report |
| M1 Main Information | Working | Good | Overview, doc types, authors, citations, countries, keywords, Bradford |
| M2 Annual Production | Working | Good | EDA, regression models, harmonic analysis |
| M3 Countries | Working | Good | Production, citations, SCP/MCP, inequality, clustering |

### M0 Data Orchestrator (NEW)
- **Purpose**: Orchestrate data loading from multiple bibliometric sources, merge them, and prepare organized data for downstream modules.
- **Features**:
  - Load from Scopus, Web of Science, OpenAlex, and generic CSV/Excel
  - Automatic column harmonization across different database exports
  - Deduplication by DOI and normalized title+year
  - Pre-organize data into author, country, source, keyword, citation, annual tables
  - PRISMA 2020 flow diagram generation from JSON/YAML specification
  - PRISMA report generation (text + LaTeX)
- **API for downstream modules**:
  - `m0_get(m0_result, "bib_merged")` - Get unified bibliometrix data frame
  - `m0_get(m0_result, "countries")` - Get pre-organized country data
  - `m0_get(m0_result, "authors")` - Get pre-organized author data
  - `m0_get_bib_data(m0_result)` - Validated access to merged data
  - `m0_is_valid(m0_result)` - Check M0 result validity

### Main.r Implementation Pattern
```r
# Define sources
sources <- list(
  scopus = list(file = "data/scopus.bib", db = "scopus", format = "bibtex"),
  wos = list(file = "data/wos.bib", db = "wos", format = "bibtex")
)

# Run M0 orchestrator
m0_result <- run_m0(sources, config = config, prisma_spec = "prisma.json", export = TRUE)

# Use M0 data in downstream modules
bib_data <- m0_get_bib_data(m0_result)
m1_result <- run_m1(bib_data, config = config, export = TRUE)
m3_result <- run_m3(bib_data, config = config, export = TRUE)
```

### PRISMA JSON Specification
```json
{
  "title": "Review Title",
  "identification": { "records_database": 1250, "records_other": 45, "duplicates_removed": 180 },
  "screening": { "records_screened": 1115, "excluded_screening": 780 },
  "eligibility": { "fulltext_assessed": 335, "excluded_fulltext": 195, "excluded_reasons": {...} },
  "included": { "studies_included": 140, "by_type": { "Journal article": 95 } },
  "quality": { "tool": "MMAT", "low_risk": 110, "high_risk": 15, "unclear": 15 }
}
```

---

## File Structure

```
R/
├── core/
│   ├── bootstrap.R          # Auto-install & load all functions
│   ├── config.R             # biblio_config(), merge_biblio_config()
│   ├── contracts.R          # new_module_result(), validate_module_result()
│   ├── module_registry.R    # get_available_modules(), get_module_metadata()
│   ├── paths.R              # build_output_path(), build_artifact_path()
│   ├── result_builders.R    # attach_artifacts_to_result(), attach_report_to_result()
│   └── validation.R         # validate_required_columns()
├── module_m0/               # Data Orchestrator (NEW)
│   ├── m0_run.R             # run_m0() - main entry point
│   ├── m0_load_sources.R    # m0_load_all_sources(), m0_load_single_source()
│   ├── m0_merge_sources.R   # m0_merge_sources(), m0_deduplicate()
│   ├── m0_organize_data.R   # m0_organize_for_modules() - pre-process for M1-M4
│   ├── m0_prisma_report.R   # m0_read_prisma_spec(), m0_build_prisma_report()
│   ├── m0_prisma_diagram.R  # m0_render_prisma_diagram() - base R graphics
│   ├── m0_helpers.R         # m0_get(), m0_is_valid(), m0_prisma_template()
│   ├── m0_validate.R        # m0_validate_sources(), m0_validate_merged()
│   └── m0_manifest.R        # m0_build_manifest()
├── module_m1/               # Main Information
├── module_m2/               # Annual Production
├── module_m3/               # Countries
├── services/
│   ├── json_service.R       # write_json_artifact()
│   ├── plot_export_service.R # export_plot_artifact()
│   └── report_service.R     # write_text_report()
└── style/
    └── ieee_theme.R         # ieee_theme(), get_biblio_palette()
```

---

## Contract Pattern (All Modules)

```r
result <- run_mN(input, config = biblio_config(), export = TRUE)
# Returns: biblio_module_result with:
#   $module_id, $module_name, $status
#   $inputs, $data, $diagnostics
#   $artifacts$plots, $artifacts$tables, $artifacts$reports, $artifacts$manifest
```

---

## Quick Reference

### Run M0
```r
source("R/core/bootstrap.R")
sources <- list(scopus = list(file = "data/scopus.bib", db = "scopus"))
m0_result <- run_m0(sources, prisma_spec = "prisma.json")
bib_data <- m0_get_bib_data(m0_result)
```

### Generate PRISMA Template
```r
prisma <- m0_prisma_template("My Review")
jsonlite::write_json(prisma, "prisma.json", auto_unbox = TRUE, pretty = TRUE)
```

### Access Organized Data
```r
m0_get(m0_result, "authors")      # Author-level aggregation
m0_get(m0_result, "countries")    # Country-level aggregation
m0_get(m0_result, "sources")      # Journal/source aggregation
m0_get(m0_result, "keywords")     # Keyword frequency
m0_get(m0_result, "annual")       # Annual production
```
