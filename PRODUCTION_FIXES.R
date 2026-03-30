# ============================================================================
# PRODUCTION READINESS FIXES - RBiblioSynth
# ============================================================================
# This file documents all critical fixes required for production deployment.
# Execute each fix in order.

# ============================================================================
# M0 MODULE FIXES
# ============================================================================

# FIX M0-1: Import %||% from coreutils
# Remove duplicate definitions in:
# - m0_load_sources.R:6
# - m0_prisma_report.R:6
# - m0_prisma_diagram.R:6
# - m0_normalize_data.R:864
# - m0_reference_extraction.R:538
# - m0_semantic_similarity.R:464

# ADD to all M0 files after source() calls:
# source("../../core/utils_null_coalescing.R")

# FIX M0-2: PRISMA diagram export for base R graphics
# File: m0_run.R - modify export for base plots

# FIX M0-3: Headless graphics handling
# File: m0_prisma_diagram.R:132 - add conditional device handling

# ============================================================================
# M1 MODULE FIXES
# ============================================================================

# FIX M1-1: Keyword_list typo in topic modeling
# File: m1_compute_topic_modeling.R:66
# CHANGE: keyword_list -> keywords_list

# FIX M1-2: Citation age field name mismatch
# File: m1_render_citation_analysis.R (render function)
# The compute function returns age_distribution but render expects citations_by_age
# CHANGE in render: age$citations_by_age -> age$age_distribution

# FIX M1-3: Collaboration field name mismatch
# File: m1_render_collaboration.R
# The compute function returns by_year and summary
# CHANGE in render: collaboration_by_year -> by_year, collaboration_type_distribution -> summary

# FIX M1-4: Price's Law field name mismatch  
# File: m1_render_price_law.R
# CHANGE: author_cumulative -> author_distribution

# FIX M1-5: Author career field name mismatch
# File: m1_render_author_career.R
# CHANGE: career_metrics -> career_df

# FIX M1-6: Hypothesis field typo
# File: m1_compute_hypotheses.R
# CHANGE: hyphypothesis -> hypothesis

# ============================================================================
# M2 MODULE FIXES
# ============================================================================

# FIX M2-1: Remove duplicate function definitions
# Files with duplicates: m2_run.R and m2_compute_growth_models.R
# REMOVE from m2_run.R: fit_richards_model, fit_von_bertalanffy_model, 
#                       fit_mmf_model, compare_growth_models, select_best_growth_model
# Keep only compute_m2_growth_models_wrapper in m2_run.R

# FIX M2-2: Create missing render functions
# CREATE: m2_render_stl.R for compute_m2_stl
# CREATE: m2_render_changepoint.R for compute_m2_changepoint
# CREATE: m2_render_ridge.R for compute_m2_ridge
# CREATE: m2_render_hypotheses.R for compute_m2_hypotheses

# FIX M2-3: Fix collect_models_for_diagnostics
# File: m2_run.R
# CHANGE: Extract correct model structure from regression results

# FIX M2-4: Fix wavelet data path
# File: m2_run.R
# CHANGE: data$wavelet -> data$harmonics$wavelet

# FIX M2-5: Remove redundant harmonics computation
# File: m2_run.R lines 33-35
# DELETE: The second compute_m2_harmonics call

# FIX M2-6: Hypothesis typo
# File: m2_compute_hypotheses.R
# CHANGE: hyphypothesis -> hypothesis

# ============================================================================
# M3 MODULE FIXES  
# ============================================================================

# FIX M3-1: Temporal dynamics column name mismatch
# File: m3_compute_temporal_dynamics.R
# CHANGE: Accept column names from prepared_data$country_annual
# The prepared_data has: country, PY, article_count
# Function expects: country, year, production
# ADD: Column renaming at start of function

# FIX M3-2: World map data access path
# File: m3_render_world_map.R
# VERIFY: data$production$production_summary paths are correct

# FIX M3-3: Move shared utility functions
# File: m3_compute_regional.R
# MOVE: safe_gini() to core/utils.R
# ENSURE: All modules can access it

# FIX M3-4: Hypothesis typo
# File: m3_table_country_regressions.R
# CHANGE: hyphypothesis -> hypothesis

# ============================================================================
# CORE FIXES
# ============================================================================

# FIX CORE-1: Create central %||% definition
# File: R/core/utils_null_coalescing.R - ALREADY CREATED

# FIX CORE-2: Update bootstrap.R to source utils first
# ENSURE: utils_null_coalescing.R is sourced before all other files

# FIX CORE-3: Add package dependency checks
# ADD: library checks in each module's bootstrap

# ============================================================================
# TEST FIXES
# ============================================================================

# FIX TEST-1: Add bootstrap tests
# File: tests/testthat/test-core-bootstrap.R - ALREADY CREATED

# FIX TEST-2: Add M0 load tests  
# File: tests/testthat/test-module-m0-load.R - ALREADY CREATED

# FIX TEST-3: Add M2 core tests
# File: tests/testthat/test-module-m2-core.R - ALREADY CREATED

# ============================================================================
# IMPLEMENTATION SCRIPT
# ============================================================================

# Run this script to apply all fixes:
# source("R/core/bootstrap.R")
# source("apply_fixes.R")

# Verification commands:
# 1. Check %||% is defined once: grep -r "%||%" --include="*.R" R/ | wc -l
# 2. Check no undefined variables: R CMD check RBiblioSynth
# 3. Run tests: testthat::test_dir("tests/testthat")

print("Production readiness fixes documented. Apply each fix manually or run automated script.")