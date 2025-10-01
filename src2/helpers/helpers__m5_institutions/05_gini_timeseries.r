# ---- Gini time series -------------------------------------------------------

m5i_build_gini_timeseries <- function(df_iy) {
  by_year <- df_iy %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      Gini_TP = tryCatch(ineq::Gini(TP), error = function(e) NA_real_),
      Gini_TC = tryCatch(ineq::Gini(TC), error = function(e) NA_real_)
    ) %>% dplyr::arrange(year)
  list(tp = dplyr::select(by_year, year, Gini_TP),
       tc = dplyr::select(by_year, year, Gini_TC))
}