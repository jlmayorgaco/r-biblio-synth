# ROADMAP.md - RBiblioSynth Project Roadmap

## Completed Modules

### M0: Data Orchestrator ✅
- Load from Scopus, WoS, OpenAlex, CSV/Excel
- Column harmonization across database exports
- Deduplication by DOI and title+year
- Pre-organize data for M1-M4 (authors, countries, sources, keywords, citations, annual)
- PRISMA 2020 flow diagram generation (JSON/YAML spec)
- PRISMA report (text + LaTeX)
- Public API for downstream modules: `m0_get()`, `m0_get_bib_data()`, `m0_is_valid()`

### M1: Main Information ✅
- Overview, document types, authors, citations, countries, sources, keywords, Bradford analysis
- h-index, g-index, Lotka's law, collaboration indices
- IEEE-ready plots and JSON exports

### M2: Annual Production ✅
- EDA, regression models (linear, logistic, Gompertz, power law)
- Harmonic analysis, growth dynamics
- Change point detection

### M3: Countries ✅
- Production, citations, SCP/MCP analysis
- Gini, Lorenz, inequality metrics
- Country clustering and profiles
- Collaboration indices

---

## Next Modules (P2)

### M4: Institutions
- Collaboration networks, quadrants
- Institutional indicators
- Affiliation parsing and meta cleaning

### M5: Authors & Documents
- h-index, g-index, most cited, author growth
- Author collaboration networks

### M6: Clustering & Themes
- Co-word analysis, Louvain, topic evolution
- Emerging themes detection

### M7: Conceptual & Social Structure
- Collaboration networks, betweenness centrality
- Keyword co-occurrence networks

### M8: Automated Bibliometric Report
- JSON + CSV + plots combined report generation

---

## LLM Integration (P3)

### M9: LLM Report Generator
- Combine bibliometric insights with text analysis

### M10: Zotero Integration
- Tagging, notes, thematic structuring

### M11: Multi-PDF Analysis
- Full-text analysis of PDFs for summaries, gaps, synthesis

### M12: Auto LaTeX Paper Report
- IEEE-ready, modular sections

---

## DevOps (P2)

### GitHub Actions CI/CD
- R CMD check, testthat tests, linting

### Unit Tests
- testthat framework for all modules

### Logging Framework
- Replace message() calls with proper logging

---

## Priority Legend
- **P0**: Critical - Must fix immediately
- **P1**: High - Should fix before release
- **P2**: Medium - Important for quality
- **P3**: Low - Nice to have
