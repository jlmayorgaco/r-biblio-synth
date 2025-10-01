# ---- Indicators + inequality -----------------------------------------------

m5i_compute_indicators <- function(df_iy) {
  by_inst <- df_iy %>%
    dplyr::group_by(institution) %>%
    dplyr::summarise(TP = sum(TP), TC = sum(TC), TC_per_TP = ifelse(TP > 0, TC/TP, NA_real_), .groups="drop")
  list(raw = df_iy, inst = by_inst)
}

m5i_compute_inequality <- function(indicators) {
  tp <- indicators$inst$TP; tc <- indicators$inst$TC
  list(Gini_TP = tryCatch(ineq::Gini(tp), error = function(e) NA_real_),
       Gini_TC = tryCatch(ineq::Gini(tc), error = function(e) NA_real_))
}
