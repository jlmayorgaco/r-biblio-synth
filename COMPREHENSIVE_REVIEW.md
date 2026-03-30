# RBiblioSynth Comprehensive Review
## Missing Features, Metrics, Statistics, Bugs & Issues

---

# M0 - Data Orchestrator

## ✅ What Exists

| Feature | Status | Quality |
|---------|--------|---------|
| Load Scopus | ✅ Working | Good |
| Load WoS | ✅ Working | Good |
| Load OpenAlex | ✅ Working | Basic |
| Load Generic CSV/XLSX | ✅ Working | Good |
| Merge sources | ✅ Working | Good |
| DOI deduplication | ✅ Working | Good |
| Title+Year deduplication | ✅ Working | Good |
| Column harmonization | ✅ Working | Good |
| Organize for modules | ✅ Working | Good |
| PRISMA diagram | ✅ Working | Basic |
| PRISMA report | ✅ Working | Basic |

## ❌ Missing Features

### Data Quality
- [ ] **Missing data imputation** - No handling for missing values
- [ ] **Outlier detection** - No detection of anomalous citations/years
- [ ] **Author name validation** - No duplicate author detection
- [ ] **Institution standardization** - No affiliation normalization
- [ ] **Country code validation** - Missing ISO country code handling

### Deduplication
- [ ] **Fuzzy title matching** - Only exact matches
- [ ] **Author disambiguation** - Critical for author analysis
- [ ] **Journal abbreviation expansion** - No standardization
- [ ] **DOI validation** - No checksum verification

### Metadata Extraction
- [ ] **Abstract processing** - No text mining
- [ ] **Reference extraction** - No CR field parsing
- [ ] **Funding information** - No grant extraction
- [ ] **ORCID handling** - No author ID linking

### PRISMA
- [ ] **Interactive PRISMA** - No flowchart generation
- [ ] **PRISMA 2020 checklist** - Missing checklist validation
- [ ] **Automated counting** - No automatic screening counts

## 🐛 Known Issues

1. `m0_normalize_country_name()` - Incomplete country name mapping
2. OpenAlex CSV - Limited column mapping
3. No encoding validation for non-UTF8 files

---

# M1 - Main Information

## ✅ What Exists

| Feature | Status | Quality |
|---------|--------|---------|
| Overview metrics | ✅ Working | Good |
| Document types | ✅ Working | Good |
| Authors | ✅ Working | Good |
| Author indices (h/g/m/i10) | ✅ Working | Good |
| Citations | ✅ Working | Good |
| Countries | ✅ Working | Good |
| Sources | ✅ Working | Good |
| Keywords | ✅ Working | Good |
| Bradford's Law | ✅ Working | Good |
| Lotka's Law | ✅ Working | Good |
| Collaboration Index | ✅ Working | Good |
| Price's Law | ✅ Working | Good |
| Keyword co-occurrence | ✅ Working | Good |
| Hypothesis testing (12) | ✅ Working | Good |

## ❌ Missing Features

### Author Analysis
- [ ] **Author name disambiguation** - Critical for accuracy
- [ ] **Author ID handling** - ORCID integration
- [ ] **First author analysis** - First author metrics
- [ ] **Corresponding author** - Corresponding author analysis
- [ ] **Author position analysis** - Sequencing importance
- [ ] **Career stage estimation** - Early/mid/late career
- [ ] **Author mobility** - Institution changes
- [ ] **Co-author network** - Team composition

### Citation Analysis
- [ ] **Citation velocity** - Rate of citations over time
- [ ] **Citation timing** - When citations arrive
- [ ] **Cited reference analysis** - What papers cite
- [ ] **Reference aging** - How references age
- [ ] **h-index evolution** - h(t) curves
- [ ] **Citation diversity** - How diverse are citing sources

### Keyword Analysis
- [ ] **Burst detection (Kleinberg)** - HIGH PRIORITY
- [ ] **Temporal keyword evolution** - Keywords over time
- [ ] **Keyword stemming** - Word normalization
- [ ] **Topic modeling (LDA)** - Latent topics
- [ ] **Keyword clustering** - Thematic grouping
- [ ] **Trend analysis** - Emerging vs declining

### Source Analysis
- [ ] **Impact Factor integration** - Journal IF
- [ ] **Journal tier classification** - Q1/Q2/Q3/Q4
- [ ] **APC analysis** - Article processing charges
- [ ] **Open access metrics** - OA percentage
- [ ] **Publisher analysis** - Publisher concentration

### Additional Metrics
- [ ] **m-index** - h-index / age
- [ ] **hI-index (hIa)** - h-index normalized for co-authors
- [ ] **e-index** - Excess citations for h-index
- [ ] **R-index** - Square root of h-core citations
- [ ] **AR-index** - Age-weighted R-index
- [ ] **hc-index** - Contemporary h-index

## 🧪 Missing Statistics

- [ ] Bootstrap confidence intervals for h-index
- [ ] Confidence intervals for Lotka's Law
- [ ] Significance tests for collaboration metrics
- [ ] Effect sizes for comparisons

## 🐛 Known Issues

1. `m1_compute_lotka.R` - MLE may not converge for small samples
2. Keyword co-occurrence - Slow for large datasets (>10k keywords)
3. Author indices - Single-authored papers may cause issues
4. Bradford zones - May not work for small datasets

---

# M2 - Annual Production

## ✅ What Exists

| Feature | Status | Quality |
|---------|--------|---------|
| EDA | ✅ Working | Good |
| Linear regression | ✅ Working | Good |
| 22+ growth models | ✅ Working | Good |
| Model comparison | ✅ Working | Good |
| Harmonic analysis | ✅ Working | Good |
| Residual analysis | ✅ Working | Good |
| Ridge regression | ✅ Working | Good |
| Change-point detection | ✅ Working | Good |
| STL decomposition | ✅ Working | Good |
| Hypothesis testing (10) | ✅ Working | Good |

## ❌ Missing Features

### Model Validation
- [ ] **Time series cross-validation** - Walk-forward validation
- [ ] **Out-of-sample testing** - Holdout validation
- [ ] **Prediction intervals** - Uncertainty bounds
- [ ] **Confidence bands** - For fitted curves
- [ ] **Model stability tests** - Parameter stability

### Forecasting
- [ ] **ARIMA models** - Standard time series
- [ ] **Exponential smoothing (ETS)** - Holt-Winters
- [ ] **Prophet-like forecasting** - Seasonal + trend
- [ ] **Uncertainty quantification** - Probabilistic forecasts
- [ ] **Scenario analysis** - Best/worst case

### Seasonality
- [ ] **Multiple seasonality** - Weekly/monthly/yearly
- [ ] **Seasonality tests** - Formal tests
- [ ] **Calendar effects** - Month/day effects

### Advanced Models
- [ ] **Neural network models** - LSTM/RNN
- [ ] **GAM (Generalized Additive)** - Smooth trends
- [ ] **State space models** - Kalman filter
- [ ] **Bayesian models** - Posterior distributions

## 🧪 Missing Statistics

- [ ] Normality tests on all model residuals
- [ ] Bootstrap standard errors
- [ ] Jackknife estimates
- [ ] Information criteria comparison table
- [ ] Diebold-Mariano test (forecast comparison)

## 🐛 Known Issues

1. Growth models - Some may produce negative predictions
2. Changepoint detection - May miss multiple changes
3. Exponential models - Log-transform may fail for zeros
4. Harmonic analysis - May identify spurious periods for short series

---

# M3 - Countries

## ✅ What Exists

| Feature | Status | Quality |
|---------|--------|---------|
| Production analysis | ✅ Working | Good |
| Citations by country | ✅ Working | Good |
| SCP/MCP analysis | ✅ Working | Good |
| Inequality (Gini, etc.) | ✅ Working | Good |
| Rankings | ✅ Working | Good |
| Distribution tests | ✅ Working | Good |
| Growth dynamics | ✅ Working | Good |
| Change points | ✅ Working | Good |
| Country profiles | ✅ Working | Good |
| Similarity/clustering | ✅ Working | Good |
| Collaboration indices | ✅ Working | Good |
| Country regressions | ✅ Working | Good |
| Harmonic analysis per country | ✅ Working | Good |
| Hypothesis testing (12) | ✅ Working | Good |

## ❌ Missing Features

### Geographic Analysis
- [ ] **World map visualization** - Choropleth maps
- [ ] **Regional aggregation** - Continent-level
- [ ] **Distance matrices** - Geographic distance
- [ ] **Geospatial clustering** - Spatial autocorrelation
- [ ] **Hot spot analysis** - Local clusters

### Network Analysis
- [ ] **Complete collaboration network** - Graph structure
- [ ] **Community detection** - Modularity, Louvain
- [ ] **Centrality measures** - Betweenness, closeness, eigenvector
- [ ] **Network evolution** - Changes over time
- [ ] **Core-periphery structure** - Center vs periphery

### Economic Correlation
- [ ] **GDP correlation** - Economic development
- [ ] **R&D investment** - Research funding
- [ ] **Population normalization** - Per capita metrics
- [ ] **Development indices** - HDI correlation
- [ ] **Sector analysis** - Industry/academic split

### Additional Metrics
- [ ] **MCR (Mean Country Rank)** - Citation ranking
- [ ] **MCI (Mean Country Impact)** - Impact factor
- [ ] **Co-authorship strength** - Weighted networks
- [ ] **Temporal networks** - Dynamic networks

## 🧪 Missing Statistics

- [ ] Moran's I for spatial autocorrelation
- [ ] Geary's C for local clustering
- [ ] Bootstrap for country-level metrics
- [ ] Panel data models (fixed/random effects)

## 🐛 Known Issues

1. Country extraction from C1 - May miss affiliations
2. Country name standardization - Incomplete
3. Small country samples - Unstable estimates
4. MCP ratio - May be biased for small samples

---

# Priority Issues

## 🔴 Critical (Must Fix)

1. **M0: Author disambiguation** - Affects all author analyses
2. **M0: Country name standardization** - Incomplete mappings
3. **M1: Keyword burst detection** - Requested feature
4. **M2: Time series cross-validation** - Model validation
5. **M3: World map visualization** - Essential for presentation

## 🟡 High Priority

1. M1: Bootstrap confidence intervals
2. M1: Author ID/ORCID handling
3. M2: Prediction intervals
4. M2: ARIMA models
5. M3: Collaboration network
6. M3: GDP correlation

## 🟢 Medium Priority

1. M0: Fuzzy title matching
2. M0: Reference extraction
3. M1: Topic modeling (LDA)
4. M1: Citation velocity
5. M2: Multiple seasonality
6. M3: Regional aggregation

## 🔵 Low Priority

1. M0: Funding extraction
2. M0: PRISMA automation
3. M1: Author mobility
4. M2: Neural network models
5. M3: Centrality measures

---

# Bugs & Issues Summary

## M0 Bugs
```r
# Issue 1: Country normalization incomplete
m0_normalize_country_name("P.R. China")  # Returns "P.R. CHINA" (should be "CHINA")

# Issue 2: OpenAlex CSV limited columns
# Missing: references, keywords, abstract

# Issue 3: No encoding detection
# Non-UTF8 files may fail silently
```

## M1 Bugs
```r
# Issue 1: Lotka MLE convergence
compute_m1_lotka(small_sample)  # May fail with error

# Issue 2: Keyword co-occurrence performance
# O(n²) for n keywords - slow for >5000 keywords

# Issue 3: Single-author papers in author indices
# May produce division by zero
```

## M2 Bugs
```r
# Issue 1: Negative predictions
some_growth_models$predictions  # May be negative for declining trends

# Issue 2: Zero log-transform
log(0)  # Fails silently in exponential models

# Issue 3: STL with short series
compute_m2_stt(less_than_2_periods)  # Error
```

## M3 Bugs
```r
# Issue 1: Country extraction regex
# May miss: "Univ of X, Y, Z" (multiple countries)

# Issue 2: MCP ratio with zero MCPs
# Division by zero possible

# Issue 3: Collaboration index for single-country papers
# May produce NaN values
```

---

# Test Coverage

| Module | Coverage | Missing |
|--------|----------|---------|
| M0 | ❌ 0% | All tests |
| M1 | ✅ ~70% | New functions (lotka, price_law, keyword_cooc) |
| M2 | ⚠️ ~40% | New regression models, ridge, changepoint, STl |
| M3 | ✅ ~80% | New collaboration indices, country regressions |

---

# Recommended Actions

## Immediate (Week 1-2)
1. Add keyword burst detection (Kleinberg algorithm)
2. Add world map visualization for M3
3. Fix country name normalization in M0
4. Add tests for new M1 functions

## Short-term (Week 3-4)
1. Add ARIMA/time series cross-validation to M2
2. Add bootstrap confidence intervals to M1
3. Add prediction intervals to M2 models
4. Add geographic distance to M3

## Medium-term (Month 2-3)
1. Add author disambiguation to M0
2. Add LDA topic modeling to M1
3. Add complete collaboration network to M3
4. Add GDP/development correlation to M3

## Long-term (Month 3+)
1. Add neural network models to M2
2. Add reference extraction to M0
3. Add career stage analysis to M1
4. Add dynamic networks to M3