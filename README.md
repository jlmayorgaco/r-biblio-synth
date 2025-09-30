# RBiblioSynth : Bibliometric Analysis and Report Automation

## Overview
RBiblioSynth is a modular framework for automating bibliometric analysis and systematic review reporting.  
It combines **Bibliometrix (R)**, **custom R6 classes**, and **LLM-powered PDF/text analysis** to streamline the literature review process.  
The main focus is **power systems frequency estimation** as an example dataset, but the framework is designed to be domain-agnostic.

The goal is to **analyze, detect, and report trends, anomalies, and patterns** in bibliometric data and integrate them with enriched insights from academic papers, producing IEEE-style outputs (plots, indices, LaTeX reports).

---

## Key Features
- **Modular R6 Architecture**: Independent modules for ingestion, cleaning, analysis, and visualization.
- **Regression Modeling**: Logistic, Gompertz, power law, and linear models for annual production and other indicators.
- **Exploratory Data Analysis (EDA)**: Automated detection of anomalies, outliers, and change points.
- **Country/Institution Analysis**: Global, regional, and institutional publication/collaboration indicators with Gini and Lorenz analysis.
- **Clustering & Thematic Analysis**: Emerging themes, co-word analysis, and topic evolution.
- **Automated Plots**: IEEE-ready visualizations (quadrants, growth curves, Lorenz, networks).
- **LLM-Powered PDF Analysis**: Combine bibliometric insights with textual analysis of PDFs for qualitative review.
- **Integrated Literature Review Report**: Generate structured LaTeX/Markdown/PDF reports combining quantitative + qualitative results.
- **Zotero & PDF Pipelines**: Enriched workflows for integrating user libraries and annotated PDFs.

---

## Roadmap (Modules)

### Bibliometric (R6 in R)
- [x] **M0**: Setup & Config (shared utils, IO, plotting, IEEE themes)
- [x] **M1**: Data Ingestion (Scopus/WoS/IEEE .bib + PRISMA deduplication)
- [x] **M2**: Annual Production (growth, models, regression, trends, anomalies)
- [x] **M3**: Countries (TP, TC, SCP, MCP, Gini, fits, top/bottom analysis)
- [?] **M4**: Institutions (collaboration networks, quadrants, indicators)
- [ ] **M5**: Authors & Documents (h-index, g-index, most cited, author growth)
- [ ] **M6**: Clustering & Themes (co-word, Louvain, topic evolution)
- [ ] **M7**: Conceptual & Social Structure (collaboration networks, betweenness)
- [ ] **M8**: Automated Bibliometric Report (JSON + CSV + plots)

### LLM / AI Integration
- [ ] **M9**: LLM Report Generator (combine bibliometric insights with text analysis)
- [ ] **M10**: Zotero Integration (tagging, notes, thematic structuring)
- [ ] **M11**: Multi-PDF Analysis (summaries, gaps, synthesis from full texts)
- [ ] **M12**: Auto LaTeX Paper Report (IEEE-ready, modular sections)

---

## Current Focus
- Finalizing **M3 (Countries)** with inequality metrics (Gini, Lorenz) and normality tests.  
- Structuring **M4 (Institutions)** using robust affiliation parsing and meta cleaning.  
- Preparing **baseline IEEE-style paper** combining quantitative bibliometric results with qualitative review.  

---

## Project Goals
RBiblioSynth aims to:
1. **Automate systematic literature reviews** through a hybrid bibliometric + qualitative approach.
2. Provide **reproducible, IEEE-ready outputs** (plots, tables, LaTeX reports).
3. **Accelerate research** by identifying trends, emerging themes, gaps, and potential benchmarks.
4. Enable **scalability to other domains** beyond power systems frequency estimation.

---

## Next Steps
- [ ] Complete **M3 (Countries)** analysis module.  
- [ ] Implement **M4 (Institutions)** collaboration quadrants.  
- [ ] Integrate **qualitative LLM review pipeline** for enriched insights.  
- [ ] Draft **conference paper version** (6 pages, IEEE template).  
