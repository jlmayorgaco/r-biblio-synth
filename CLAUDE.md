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
| M1_Main_Information | Working | Medium | Large monolithic file (1000+ lines), extensive plotting |
| M2_Annual_Production | Working | Medium | R6 class structure with submodules |
| M3_Authors | Broken | Low | Empty/placeholder, commented-out code |
| M3_Most_Prod_Authors | Partial | Low | Incomplete implementation |
| M4_Countries | Partial | Low | Just helper files |
| Config | Working | Good | Theme system present |

### Main.r Implementation Pattern
- The example at examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/main.r demonstrates a concrete orchestration flow for M1_Main_Information.
- Core steps:
  - Define a config list (output_dir, export flags, plotting parameters, verbosity).
  - Load bibliographic data from a BibTeX source and convert to a data frame using bibliometrix::convert2df.
  - Call a main runner (e.g., run_m1) with bib_data and config, exporting artifacts if requested.
  - Print a structured summary of results (overview, doc types, top authors, top cited papers, top countries/sources, Bradford zones, manifest).
- Pattern sketch (high level):
  - config <- list(...)
  - bib_data <- bibliometrix::convert2df(file = tmp_file, dbsource = "scopus", format = "bibtex")
  - m1_result <- run_m1(bib_data, config = config, export = TRUE)
  - Print or inspect m1_result$data and m1_result$artifacts
- Implications:
  - This pattern acts as a reference for a minimal M1 orchestration that could be encapsulated into a dedicated R module.
  - It highlights dependencies on the non-existent SystematicReviewClass.r in current examples, and on a stable package scaffold.
- Recommendations moving forward:
  - Implement a minimal SystematicReviewClass.r or replace the dependency with a safe stub for examples.
  - Establish a proper R package layout (DESCRIPTION/NAMESPACE) to host run_m1 and related interfaces.
  - Create a lightweight M1_Main_Information module with a defined contract (input bib_data, config; output artifacts and data slots).

---

## Critical Issues

### 1. BROKEN REFERENCES - CRITICAL
- `examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/main.r` references `src/SystematicReviewClass.r` which **DOES NOT EXIST**
- This makes the main example completely non-functional

### 2. EMPTY STUBS
- `src/M3_Authors/_helpers.r` - EMPTY (0 lines)
- `src/M3_Authors/_utils.r` - EMPTY (0 lines)
- `src/03_utils/io/io_paths.r` - EMPTY (0 lines)
- `src/03_utils/modules/module_m5/module_m5_tables_utils.r` - Has code but orphaned

### 3. CODE DUPLICATION
- `_helpers.r` in M2_Annual_Production contains duplicate function definitions (797 lines with repeated functions like `calculate_moving_average`, `serialize_model`, etc.)

### 4. COMMENTED-OUT CODE
- `src/M3_Authors/M3_Authors.r` has 90% of methods commented out
- Example file imports reference non-existent classes

### 5. NO PACKAGE STRUCTURE
- No `DESCRIPTION` file
- No `NAMESPACE` file
- No `Roxygen` documentation
- No tests directory

---

## Code Quality Issues

### Naming Inconsistencies
- Mixed snake_case and camelCase: `fn_m1_main_information` vs `ieee_theme`
- Unclear file naming: `_helpers.r`, `__m1_report.r`, `_plots.r`
- Module naming: `M3_Most_Prod_Authors` vs `M3_Authors`

### Hardcoded Values
- Paths like `"results/M1_Main_Information/figures"`
- Magic numbers in plotting: `dpi = 600`, `window_size = c(1, 5, 10)`
- Hardcoded model parameters in regression functions

### No Error Handling Strategy
- Basic tryCatch in some places but inconsistent
- Silent failures possible
- No validation framework

### No Version Control
- No `.Rbuildignore`
- No `.Rinstignore`
- `_temp/` directory committed to git

---

## Missing Features (Per README)

### Unimplemented Modules
- [ ] **M4**: Institutions (collaboration networks, quadrants)
- [ ] **M5**: Authors & Documents (h-index, g-index)
- [ ] **M6**: Clustering & Themes
- [ ] **M7**: Conceptual & Social Structure
- [ ] **M8**: Automated Bibliometric Report

### LLM Integration (Roadmap)
- [ ] **M9**: LLM Report Generator
- [ ] **M10**: Zotero Integration
- [ ] **M11**: Multi-PDF Analysis
- [ ] **M12**: Auto LaTeX Paper Report

---

## Recommendations

### Immediate Fixes
1. Create `SystematicReviewClass.r` or remove broken references
2. Either delete empty stub files or implement them
3. Remove duplicate function definitions in _helpers.r
4. Clean up commented-out code in M3_Authors

### Short-term
1. Add proper R package structure (DESCRIPTION, NAMESPACE)
2. Create README files for each module
3. Add basic unit tests
4. Document all public methods with roxygen

### Long-term
1. Implement missing modules (M4-M8)
2. Add CI/CD via GitHub Actions
3. Create proper dependency management
4. Add logging framework instead of `message()` calls
5. Refactor monolithic M1_Main_Information.r into smaller modules
