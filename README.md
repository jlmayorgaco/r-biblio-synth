# RBiblioSynth : Bibliometric Analysis and Report Automation

[![R](https://img.shields.io/badge/R-%3E%3D4.0-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

**RBiblioSynth** is a comprehensive modular R framework for **IEEE Q1 Journal-quality bibliometric analysis** and systematic review automation. It provides:

- **M0 Data Orchestrator**: Multi-source loading, merging, PRISMA automation
- **M1 Main Information**: Descriptive analysis, author metrics, citation networks, topic modeling
- **M2 Annual Production**: 30+ growth models, 8 forecasting methods, changepoint detection
- **M3 Countries**: Spatial statistics, economic correlations, collaboration networks

The framework combines **Bibliometrix (R)**, **custom R6 classes**, and **rigorous statistical methods** to produce IEEE-style outputs (plots, tables, LaTeX reports).

---

## Key Features

### Beyond bibliometrix

| Feature | bibliometrix | RBiblioSynth |
|---------|--------------|--------------||Growth Models | 2-3 | **30+** (Bass, Gompertz, Weibull, Richards, von Bertalanffy, MMF) |
| Forecasting | Basic | **8 methods** (ARIMA, SARIMA, ETS, TBATS, Prophet, State Space, Ensemble) |
| Spatial Statistics | None | **Full suite** (Moran's I, Geary's C, LISA, Getis-Ord Gi*) |
| Economic Correlation | None | **GDP, HDI, R&D** integration |
| Bootstrap CIs | None | **BCA method** for all metrics |
| Hypothesis Tests | Limited | **40+ formal tests** with FDR correction |

### Unique Capabilities

1. **30+ Growth Models**: Bass diffusion, Gompertz, Weibull, Richards, von Bertalanffy, MMF with comparison
2. **Ensemble Forecasting**: AIC-weighted model averaging
3. **Spatial Bibliometrics**: First implementation of Moran's I, LISA for bibliometric data
4. **PRISMA Automation**: Automatic PRISMA 2020 flow diagrams from JSON/YAML specs
5. **Bootstrap Confidence Intervals**: BCA method for robust inference
6. **QAP Network Tests**: Statistical significance for collaboration networks

---

## Quick Start

```r
# Load package
source("R/core/bootstrap.R")

# Define data sources
sources <- list(
  scopus = list(file = "data/scopus.bib", db = "scopus", format = "bibtex")
)

# Run M0 Data Orchestrator
m0_result <- run_m0(sources)
bib_data <- m0_get_bib_data(m0_result)

# Run M1 Main Information
m1_result <- run_m1(bib_data)

# Run M2 Annual Production
annual_data <- m0_get(m0_result, "annual")
m2_result <- run_m2(annual_data)

# Run M3 Countries
m3_result <- run_m3(bib_data)
```

---

## Module Details

### M0: Data Orchestrator
- Multi-source loading (Scopus, WoS, OpenAlex, Generic CSV)
- Automatic deduplication (DOI + fuzzy title matching)
- PRISMA 2020 diagram generation
- Quality validation (ORCID, DOI, email extraction)

### M1: Main Information
- Author productivity and indices (h-index, g-index, m-quotient, i10)
- Citation analysis with distribution fitting
- Topic modeling (LDA with coherence/perplexity)
- Bradford's Law and Lotka's Law analysis
- Keyword co-occurrence networks
- Kleinberg burst detection

### M2: Annual Production
- **30+ Growth Models**: Bass, Gompertz, Weibull, Richards, von Bertalanffy, MMF
- **8 Forecasting Methods**: ARIMA, SARIMA, ETS, TBATS, Prophet, State Space, Naive, Ensemble
- Changepoint Detection: PELT, CUSUM, Binary Segmentation
- Harmonic Analysis: FFT, Lomb-Scargle
- Model Diagnostics: AIC/BIC comparison, cross-validation
- 15+ Statistical Tests: Normality, autocorrelation, heteroscedasticity

### M3: Countries
- Spatial Statistics: Moran's I, Geary's C, LISA, Getis-Ord Gi*
- Economic Correlation: GDP, HDI, R&D expenditure
- Collaboration Indices: Salton, Jaccard, Affinity
- Temporal Dynamics: Rank mobility, Markov transitions, NELSOP
- QAP Network Tests: Statistical significance for correlations
- 12 Formal Hypothesis Tests

---

## Roadmap (Modules)

### Bibliometric (R6 in R)
- [x] **M0**: Data Orchestrator (load, merge, organize sources; PRISMA diagram & report)
- [x] **M1**: Main Information (overview, doc types, authors, citations, countries, keywords, Bradford, Lotka)
- [x] **M2**: Annual Production (30+ growth models, forecasting, changepoint, diagnostics)
- [x] **M3**: Countries (spatial stats, economic correlation, QAP tests, temporal dynamics)
- [ ] **M4**: Institutions (collaboration networks, quadrants, indicators)
- [ ] **M5**: Authors & Documents (h-index evolution, citation networks)
- [ ] **M6**: Clustering & Themes (co-word, Louvain, topic evolution)
- [ ] **M7**: Conceptual & Social Structure (collaboration networks, betweenness)
- [ ] **M8**: Automated Bibliometric Report (JSON + CSV + plots)

### LLM / AI Integration
- [ ] **M9**: LLM Report Generator (combine bibliometric insights with text analysis)
- [ ] **M10**: Zotero Integration (tagging, notes, thematic structuring)

---

## Statistical Rigor

All modules include:
- **Bootstrap Confidence Intervals**: BCA method for all statistics
- **Multiple Testing Correction**: FDR (Benjamini-Hochberg)
- **Effect Sizes**: Cohen's d, eta-squared
- **Power Analysis**: Sample size considerations
- **Formal Hypothesis Tests**: 40+ tests with interpretations

---

## Visualization

```r
# Treemap for hierarchical display
render_m1_treemap(m1_data, type = "countries")

# Sankey diagram for collaboration flows
render_m3_sankey(m3_data, type = "country")

# Three-field plot (Authors-Keywords-Sources)
render_m1_three_field(data)

# Time series with forecast bands
m2_result$artifacts$plots$forecasting
```

---

## Installation

```r
# From GitHub
devtools::install_github("yourusername/RBiblioSynth")
library(RBiblioSynth)
```

---

## Citation

```bibtex
@article{rbibliosynth2026,
  title = {RBiblioSynth: A Comprehensive R Framework for Bibliometric Analysis},
  author = {Mayorga},
  journal = {SoftwareX},
  year = {2026},
  doi = {10.1016/j.softx.2026.XXXXX}
}
```

---

## License

MIT License

## Acknowledgments

This package extends `bibliometrix` with advanced statistical methods and rigorously tested implementations suitable for IEEE Q1 journal publications.