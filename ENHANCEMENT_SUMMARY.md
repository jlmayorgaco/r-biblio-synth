# RBiblioSynth Enhancement Summary

## New Features Added

### M1 Module - Main Information (12 new analyses + hypothesis testing)

**New Compute Files:**
- `m1_compute_lotka.R` - Lotka's Law analysis with MLE power-law fitting
- `m1_compute_author_indices.R` - h-index, g-index, m-index, i10-index
- `m1_compute_collaboration.R` - Collaboration Index/Coefficient
- `m1_compute_price_law.R` - Price's square root law
- `m1_compute_keyword_cooccurrence.R` - Network analysis with centrality measures
- `m1_compute_hypotheses.R` - 12 hypothesis tests for M1

**New Render Files:**
- `m1_render_lotka.R` - Lotka's Law visualization
- `m1_render_author_indices.R` - Author impact indices visualization
- `m1_render_collaboration.R` - Collaboration trend visualization
- `m1_render_price_law.R` - Price's Law visualization
- `m1_render_keyword_cooccurrence.R` - Network visualization

**New Table Files:**
- `m1_table_lotka.R`
- `m1_table_author_indices.R`
- `m1_table_collaboration.R`
- `m1_table_price_law.R`
- `m1_table_keyword_cooccurrence.R`

### M2 Module - Annual Production (22+ growth models + hypothesis testing)

**Enhanced Files:**
- `m2_compute_regression.R` - 22+ growth models with auto-selection
- `m2_compute_ridge.R` - Ridge regression with cross-validation
- `m2_compute_changepoint.R` - PELT change-point detection
- `m2_compute_stl.R` - STL time series decomposition
- `m2_compute_hypotheses.R` - 10 hypothesis tests for M2

**Enhanced Render Files:**
- `m2_render_regression.R` - Best model plot with equation display
- `m2_render_eda.R` - Enhanced EDA plots

### M3 Module - Countries (Per-country regression + hypothesis testing)

**New Compute Files:**
- `m3_compute_country_regressions.R` - Per-country regression analysis
- `m3_compute_hypotheses.R` - 12 hypothesis tests for M3

**New Render Files:**
- `m3_render_country_regressions.R` - Country regression visualization
- `m3_render_collaboration_indices.R` - Collaboration indices visualization

**New Table Files:**
- `m3_table_country_regressions.R`
- `m3_table_collaboration_indices.R`

---

## Hypothesis Testing Framework

### M1 Hypotheses (12 tests)
1. **H01.1**: Author productivity follows Lotka's Law (alpha = 2)
2. **H01.2**: Citation distribution follows power law
3. **H01.3**: Bradford's Law applies to source distribution
4. **H01.4**: Collaboration rate has increased over time
5. **H01.5**: Author productivity inequality follows Price's Law
6. **H01.6**: Keywords follow Zipf's distribution
7. **H01.7**: Citation distribution is highly concentrated (Gini > 0.7)
8. **H01.8**: Document types follow expected proportions
9. **H01.9**: International collaboration correlates with citation impact
10. **H01.10**: Core sources (top 1/3) contain 1/3 of articles
11. **H01.11**: Keywords are concentrated (top 20% = 80% of occurrences)
12. **H01.12**: Top 10% authors produce > 50% of articles

### M2 Hypotheses (10 tests)
1. **H02.1**: Growth is linear (vs non-linear)
2. **H02.2**: No structural breaks in production trend
3. **H02.3**: Production follows exponential growth
4. **H02.4**: Residuals are normally distributed
5. **H02.5**: Variance is homoscedastic
6. **H02.6**: Growth rate is constant
7. **H02.7**: No significant outliers
8. **H02.8**: Production follows logistic S-curve
9. **H02.9**: YoY growth rate is stationary
10. **H02.10**: Trend is accelerating/decelerating

### M3 Hypotheses (12 tests)
1. **H03.1**: All countries show increasing research interest
2. **H03.2**: No difference in growth patterns between countries
3. **H03.3**: Production concentration is stable over time
4. **H03.4**: International collaboration is uniformly distributed
5. **H03.5**: Country size correlates with publication output
6. **H03.6**: Higher production correlates with citation impact
7. **H03.7**: Leading countries maintain dominance over time
8. **H03.8**: New entrants have different growth patterns
9. **H03.9**: SCP/MCP ratio is consistent across countries
10. **H03.10**: Geographic distance affects collaboration strength
11. **H03.11**: Research topic is declining in some countries
12. **H03.12**: Countries cluster into distinct growth patterns (bunches)

---

## Usage

```r
# Load the package
source("R/core/bootstrap.R")

# Run M1 with hypothesis testing
m1_result <- run_m1(bib_data, config, export = TRUE)

# Access hypothesis results
m1_result$data$hypotheses$hyphypotheses

# Run M2 with hypothesis testing
m2_result <- run_m2(annual_ts, config, export = TRUE)

# Run M3 with per-country regression and hypothesis testing
m3_result <- run_m3(bib_data, config, export = TRUE)
```

---

## File Structure

```
R/
├── core/
│   └── bootstrap.R (updated - added reshape2)
├── module_m1/
│   ├── compute/
│   │   ├── m1_compute_lotka.R
│   │   ├── m1_compute_author_indices.R
│   │   ├── m1_compute_collaboration.R
│   │   ├── m1_compute_price_law.R
│   │   ├── m1_compute_keyword_cooccurrence.R
│   │   └── m1_compute_hypotheses.R
│   ├── render/
│   │   ├── m1_render_lotka.R
│   │   ├── m1_render_author_indices.R
│   │   ├── m1_render_collaboration.R
│   │   ├── m1_render_price_law.R
│   │   └── m1_render_keyword_cooccurrence.R
│   ├── tables/
│   │   ├── m1_table_lotka.R
│   │   ├── m1_table_author_indices.R
│   │   ├── m1_table_collaboration.R
│   │   ├── m1_table_price_law.R
│   │   └── m1_table_keyword_cooccurrence.R
│   └── m1_run.R (updated)
├── module_m2/
│   ├── compute/
│   │   ├── m2_compute_regression.R (enhanced)
│   │   ├── m2_compute_ridge.R
│   │   ├── m2_compute_changepoint.R
│   │   ├── m2_compute_stl.R
│   │   └── m2_compute_hypotheses.R
│   └── m2_run.R (updated)
├── module_m3/
│   ├── compute/
│   │   ├── m3_compute_country_regressions.R
│   │   ├── m3_compute_collaboration_indices.R
│   │   └── m3_compute_hypotheses.R
│   ├── render/
│   │   ├── m3_render_country_regressions.R
│   │   └── m3_render_collaboration_indices.R
│   ├── tables/
│   │   ├── m3_table_country_regressions.R
│   │   └── m3_table_collaboration_indices.R
│   └── m3_run.R (updated)
└── utils/
    ├── render_hypotheses.R
    └── table_hypotheses.R
```

---

## Testing

Run the validation script:
```r
source("R/validate_integration.R")
```

Run the full test:
```r
source("R/test_new_analyses.R")
```

Run the example pipeline:
```r
source("examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/main.r")
```