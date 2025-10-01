m3c_build_themes <- function(df_docs) {
  t9 <- themes_country_year(
    df            = df_docs,
    country_col   = NULL,
    sep_regex     = ";|,|\\|",
    min_docs_cell = 5,
    n_top         = 12,
    use_tfidf     = TRUE
  )

  payload1 <- build_general_and_specific(t9$top_wide)
  save_general_specific_json(payload1, "M9_general_specific_themes.json")

  payload2 <- build_general_and_specific_from_themes(t9$themes)
  save_general_specific_json(payload2)

  list(
    groups   = t9$groups,
    themes   = t9$themes,
    top      = t9$top,
    top_wide = t9$top_wide
  )
}
