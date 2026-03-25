# ROADMAP.md - RBiblioSynth Project Roadmap

## Epic 1: Critical Bug Fixes (P0)

### Ticket 1.1: Fix Missing SystematicReviewClass.r
- **Priority**: P0 - CRITICAL
- **Description**: The main example references `src/SystematicReviewClass.r` which does not exist. Either create the class or update examples to use existing modules directly.
- **Files**: `examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/main.r`, `examples/OPEN_ALEX_LEISMANIASIS/main.r`
- **Effort**: L

### Ticket 1.2: Remove or Implement Empty Stub Files
- **Priority**: P0 - CRITICAL
- **Description**: Several files are completely empty: `src/M3_Authors/_helpers.r`, `src/M3_Authors/_utils.r`, `src/03_utils/io/io_paths.r`
- **Action**: Either delete these files or implement them
- **Effort**: S

### Ticket 1.3: Remove Duplicate Function Definitions
- **Priority**: P0 - CRITICAL
- **Description**: `_helpers.r` has 797 lines with duplicate function definitions (calculate_moving_average, serialize_model, get_anomalies, etc.)
- **Files**: `src/M2_Annual_Production/_helpers.r`
- **Effort**: M

---

## Epic 2: Code Quality & Architecture (P1)

### Ticket 2.1: Add R Package Structure
- **Priority**: P1 - HIGH
- **Description**: Create proper R package structure with DESCRIPTION, NAMESPACE, and Roxygen documentation
- **Files**: Create `DESCRIPTION`, `NAMESPACE`, `.Rbuildignore`
- **Effort**: L

### Ticket 2.2: Standardize Module Architecture
- **Priority**: P1 - HIGH
- **Description**: Currently inconsistent - some modules use R6 classes (M2, M3_Most_Prod_Authors), others are functional scripts (M1). Decide on one approach.
- **Files**: All modules in `src/`
- **Effort**: XL

### Ticket 2.3: Fix Inconsistent Naming Conventions
- **Priority**: P1 - HIGH
- **Description**: Mix of snake_case (`fn_m1_main_information`), camelCase (`ieee_theme`), and unclear naming (`_helpers.r`, `__m1_report.r`)
- **Action**: Create naming conventions document and refactor
- **Effort**: M

### Ticket 2.4: Remove Hardcoded Paths
- **Priority**: P1 - HIGH
- **Description**: Hardcoded paths like `"results/M1_Main_Information/figures"` scattered throughout code
- **Files**: Multiple in `src/M1_Main_Information/`, `src/M2_Annual_Production/`
- **Effort**: M

### Ticket 2.5: Add Error Handling Strategy
- **Priority**: P1 - HIGH
- **Description**: Inconsistent tryCatch usage, silent failures possible
- **Action**: Create validation framework and consistent error handling
- **Effort**: M

---

## Epic 3: Module Implementation (P2)

### Ticket 3.1: Implement M3_Authors Module
- **Priority**: P2 - MEDIUM
- **Description**: M3_Authors is currently 90% commented out. Implement author-level metrics: h-index, g-index, author collaboration networks
- **Files**: `src/M3_Authors/M3_Authors.r`, `src/M3_Authors/_helpers.r`, `src/M3_Authors/_utils.r`
- **Effort**: L

### Ticket 3.2: Implement M4_Countries/Institutions Module
- **Priority**: P2 - MEDIUM
- **Description**: Per README - collaboration networks, quadrants, institutional indicators
- **Files**: `src/M4_Countries/`
- **Effort**: L

### Ticket 3.3: Implement M5_Authors_Documents Module
- **Priority**: P2 - MEDIUM
- **Description**: Per README - h-index, g-index, most cited, author growth
- **Files**: Create `src/M5_Authors_Documents/`
- **Effort**: L

### Ticket 3.4: Implement M6_Clustering_Themes Module
- **Priority**: P2 - MEDIUM
- **Description**: Per README - co-word analysis, Louvain, topic evolution
- **Files**: Create `src/M6_Clustering_Themes/`
- **Effort**: XL

### Ticket 3.5: Implement M7_Conceptual_Social_Structure Module
- **Priority**: P2 - MEDIUM
- **Description**: Per README - collaboration networks, betweenness centrality
- **Files**: Create `src/M7_Conceptual_Social_Structure/`
- **Effort**: XL

### Ticket 3.6: Implement M8_Automated_Report Module
- **Priority**: P2 - MEDIUM
- **Description**: Per README - automated JSON + CSV + plots report generation
- **Files**: Create `src/M8_Automated_Report/`
- **Effort**: L

---

## Epic 4: LLM Integration (P3)

### Ticket 4.1: Implement M9_LLM_Report_Generator
- **Priority**: P3 - LOW
- **Description**: Combine bibliometric insights with text analysis using LLMs
- **Files**: Create `src/M9_LLM_Report_Generator/`
- **Effort**: XL

### Ticket 4.2: Implement M10_Zotero_Integration
- **Priority**: P3 - LOW
- **Description**: Zotero API integration for tagging, notes, thematic structuring
- **Files**: Create `src/M10_Zotero_Integration/`
- **Effort**: L

### Ticket 4.3: Implement M11_MultiPDF_Analysis
- **Priority**: P3 - LOW
- **Description**: Full-text analysis of PDFs for summaries, gaps, synthesis
- **Files**: Create `src/M11_MultiPDF_Analysis/`
- **Effort**: XL

### Ticket 4.4: Implement M12_Auto_LaTeX_Paper
- **Priority**: P3 - LOW
- **Description**: IEEE-ready automated paper generation
- **Files**: Create `src/M12_Auto_LaTeX_Paper/`
- **Effort**: XL

---

## Epic 5: DevOps & Infrastructure (P2)

### Ticket 5.1: Add GitHub Actions CI/CD
- **Priority**: P2 - MEDIUM
- **Description**: Create workflows for R CMD check, testthat tests, linting
- **Files**: Create `.github/workflows/`
- **Effort**: M

### Ticket 5.2: Add Unit Tests
- **Priority**: P2 - MEDIUM
- **Description**: No tests exist. Add testthat framework and basic unit tests
- **Files**: Create `tests/`
- **Effort**: XL

### Ticket 5.3: Add .gitignore for R
- **Priority**: P2 - MEDIUM
- **Description**: Add proper `.Rbuildignore`, `.Rinstignore`, ignore `_temp/`, `.RData`, etc.
- **Effort**: S

### Ticket 5.4: Add Logging Framework
- **Priority**: P2 - MEDIUM
- **Description**: Replace `message()` calls with proper logging (logr, logger packages)
- **Effort**: M

---

## Epic 6: Documentation (P2)

### Ticket 6.1: Add Module READMEs
- **Priority**: P2 - MEDIUM
- **Description**: Each module should have its own README explaining purpose, inputs, outputs
- **Effort**: M

### Ticket 6.2: Add Roxygen Documentation
- **Priority**: P2 - MEDIUM
- **Description**: Document all public functions with roxygen2
- **Effort**: L

### Ticket 6.3: Clean up _temp Directory
- **Priority**: P2 - MEDIUM
- **Description**: Remove or properly ignore `_temp/` directory which contains experimental code
- **Effort**: S

---

## Effort Legend
- **S**: Small (1-2 days)
- **M**: Medium (1 week)
- **L**: Large (2-3 weeks)
- **XL**: Extra Large (1+ month)

---

## Priority Legend
- **P0**: Critical - Must fix immediately
- **P1**: High - Should fix before release
- **P2**: Medium - Important for quality
- **P3**: Low - Nice to have