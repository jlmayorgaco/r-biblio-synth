# M1 Feature Compliance Matrix

| FID | Legacy Feature | Legacy Functions | Belongs M1? | Modern M1? | Status | Action |
|-----|----------------|------------------|-------------|------------|--------|--------|
| F0 | Main bundle | fn_m1_main_information | Yes | run_m1() | Present | OK |
| F0a | Annual production | author_prod_over_time | **M2** | Excluded | Moved | Document |
| F1 | Overview indicators | extract_main_information | Yes | compute_m1_overview | Present | OK |
| F1a | Document types extract | extract_document_types | Yes | compute_m1_doc_types | Present | OK |
| F2a | Doc types pie chart | fn_m1_mtrc1_articles_types_pie | Yes | render_m1_doc_types | Present | OK |
| F2b | Doc types txt report | fn_m1_mtrc1_generate_document_types_report | Yes | build_m1_report | Present | OK |
| F2c | Doc types tex report | fn_m1_mtrc1_generate_document_types_report | Yes | build_m1_report | Present | OK |
| F3a | Authors productivity | fn_m1_mtrc3_analyze_and_plot_most_prod_authors | Yes | compute_m1_authors | Present | OK |
| F3b | Authors bar plot | fn_m1_mtrc3_analyze_and_plot_most_prod_authors | Yes | render_m1_authors | Present | OK |
| F3c | Authors Lorenz | fn_m1_mtrc3_generate_lorenz_curve | Yes | render_m1_authors | Present | OK |
| F4a | Top cited papers | fn_m1_mtrc4_analyze_and_plot_most_cited_papers | Yes | compute_m1_citations | Present | OK |
| F4b | PaperID from DOI | PaperID mapping | Yes | compute_m1_citations | Present | OK |
| F4c | Citations per year | fn_m1_mtrc4_analyze_and_plot_citations_per_year | Yes | compute_m1_citations | Present | OK |
| F4d | Dual bar+line plot | generate_bar_plot_with_line | Yes | render_m1_citations | **MISSING** | Implement |
| F4e | Paper bubble chart | fn_m1_mtrc4_generate_bubble_chart | Optional | Missing | Deferred | Bucket B |
| F5a | Countries by articles | fn_m1_mtrc5_analyze_and_plot_most_prod_countries | Yes | compute_m1_countries | Present | OK |
| F5b | Countries bar plot | fn_m1_mtrc5 | Yes | render_m1_countries | Present | OK |
| F5c | SCP/MCP stacked | generate_dual_bar_plot_horizontal | Yes | render_m1_countries | Present | OK |
| F5d | Countries Lorenz | generate_lorenz_curve | Yes | render_m1_countries | Present | OK |
| F5e | World map | generate_world_map | Optional | Missing | Deferred | Bucket B |
| F5f | Treemap | generate_treemap | Optional | Missing | Deferred | Bucket B |
| F5g | TC per country | analyze_and_plot_tc_per_country | Yes | compute_m1_countries | Present | OK |
| F5h | Avg citations country | fn_m1_mtrc5_analyze_and_plot_tc_per_country | Yes | render_m1_countries | **MISSING** | Implement |
| F5i | Country bubble trend | R6 bubble countries | Advanced | Missing | M3 | Deferred |
| F6a | Top sources | fn_m1_mtrc6_analyze_and_plot_most_rel_sources | Yes | compute_m1_sources | Present | OK |
| F6b | Sources bar plot | fn_m1_mtrc6 | Yes | render_m1_sources | Present | OK |
| F6c | Sources Lorenz | generate_lorenz_curve | Yes | render_m1_sources | Present | OK |
| F7a | Bradford | analyze_bradford_law | Yes | compute_m1_bradford | Present | OK |
| F7b | Bradford zones plot | analyze_bradford_law | Yes | render_m1_bradford | Present | OK |
| F8a | Keywords compute | extract_bibliographic_data | Yes | compute_m1_keywords | Present | OK |
| F8b | Keywords bar plot | N/A | Yes | render_m1_keywords | Present | OK |
| F8c | Keywords ggwordcloud | wc_3x3_* functions | Optional | Missing | Bucket B | Optional |
| F8d | wordcloud2 HTML | wc_3x3_* functions | Optional | Missing | Bucket B | Optional |
| F9a | JSON export | save_json | Yes | write_json_artifact | Present | OK |
| F9b | Plot export | save_plot | Yes | export_plot_artifact | Present | OK |
| F9c | Report export | generate_document_types_report | Yes | write_text_report | Present | OK |
| F9d | Manifest | N/A | Yes | build_m1_manifest | Present | OK |
