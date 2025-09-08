# ================================================================
# M2_Production — Module 2: Annual Research Production
# ================================================================
M2_Production <- R6::R6Class(
  "M2_Production",
  inherit = M0_Abstract_Module,
  public = list(
    initialize = function(df, year_col = "Year", tc_col = "Times_Cited") {
      stopifnot(year_col %in% colnames(df), tc_col %in% colnames(df))
      self$input <- df
      self$params <- list(year_col = year_col, tc_col = tc_col)
      self$results <- list(); self$plots <- list()
      self$module_index = "M2"
    },

    run = function(run_harmonic = TRUE) {
      cat("[M2][run] Starting Annual Production analysis...\n")

      # --- Aggregate annual TP & TC ---
      df_annual_tp <- private$aggregate_tp(self$input, self$params$year_col)
      df_annual_tc <- private$aggregate_tc(self$input, self$params$year_col, self$params$tc_col)

      # --- Fit models (via registry) ---
      fit_tp <- fit_models(df_annual_tp)
      fit_tc <- fit_models(df_annual_tc)

      best_model_tp <- fit_tp$best_model
      best_model_tc <- fit_tc$best_model

      cat("[M2][run] Best TP model:", fit_tp$best_name, " (R² =", fit_tp$best_r2, ")\n")
      cat("[M2][run] Best TC model:", fit_tc$best_name, " (R² =", fit_tc$best_r2, ")\n")

      # --- Correlation ---
      corr_tp_tc <- cor(df_annual_tp$Value, df_annual_tc$Value, use = "pairwise.complete.obs")
      cat("[M2][run] Correlation TP vs TC =", corr_tp_tc, "\n")

      # --- Plots for TP ---
      self$plots$p1 <- plot_regression(df_annual_tp, best_model_tp, fit_tp$best_name, "TP vs Year")
      self$plots$p1_error_acf  <- plot_resid_acf(df_annual_tp, best_model_tp)
      self$plots$p1_error_dw   <- plot_resid_dw(df_annual_tp, best_model_tp)
      self$plots$p1_error_hist <- plot_resid_hist(df_annual_tp, best_model_tp)
      self$plots$p1_error_qq   <- plot_resid_qq(df_annual_tp, best_model_tp)

      # --- Plots for TC ---
      self$plots$p2 <- plot_regression(df_annual_tc, best_model_tc, fit_tc$best_name, "TC vs Year")
      self$plots$p2_error_acf  <- plot_resid_acf(df_annual_tc, best_model_tc)
      self$plots$p2_error_dw   <- plot_resid_dw(df_annual_tc, best_model_tc)
      self$plots$p2_error_hist <- plot_resid_hist(df_annual_tc, best_model_tc)
      self$plots$p2_error_qq   <- plot_resid_qq(df_annual_tc, best_model_tc)

      # --- TP vs TC correlation plot ---
      self$plots$p3 <- plot_tp_vs_tc(df_annual_tp, df_annual_tc, corr_tp_tc)

      # --- Model comparison overlay (Logistic vs Gompertz for TP) ---
      self$plots$tp_model_comparison <- plot_model_comparison(
        df_annual_tp,
        list(best_model_tp, fit_tp$other_models$gompertz),
        c("Logistic", "Gompertz"),
        "TP Model Comparison"
      )

      # --- Structural break / Change point analysis ---
      tp_breaks <- tryCatch({
        bp <- strucchange::breakpoints(Value ~ Year, data = df_annual_tp)
        bp_years <- df_annual_tp$Year[bp$breakpoints]
        list(
          breakpoints = bp_years,
          nbreaks     = bp$nbreaks,
          rss         = bp$rss,
          BIC         = BIC(bp),
          AIC         = AIC(bp)
        )
      }, error = function(e) NULL)

      tp_changepoints <- tryCatch({
        cp <- changepoint::cpt.meanvar(df_annual_tp$Value, method = "PELT")
        cp_years <- df_annual_tp$Year[changepoint::cpts(cp)]
        list(
          cpts     = cp_years,
          pen.value= cp@pen.value,
          test.stat= cp@test.stat
        )
      }, error = function(e) NULL)

      tc_breaks <- tryCatch({
        bp <- strucchange::breakpoints(Value ~ Year, data = df_annual_tc)
        bp_years <- df_annual_tc$Year[bp$breakpoints]
        list(
          breakpoints = bp_years,
          nbreaks     = bp$nbreaks,
          rss         = bp$rss,
          BIC         = BIC(bp),
          AIC         = AIC(bp)
        )
      }, error = function(e) NULL)

      tc_changepoints <- tryCatch({
        cp <- changepoint::cpt.meanvar(df_annual_tc$Value, method = "PELT")
        cp_years <- df_annual_tc$Year[changepoint::cpts(cp)]
        list(
          cpts     = cp_years,
          pen.value= cp@pen.value,
          test.stat= cp@test.stat
        )
      }, error = function(e) NULL)

      # --- Harmonic Analysis ---
      harmonic_results <- NULL
      if (run_harmonic) {
        harmonic_results <- list(
          tp = private$harmonic_analysis(df_annual_tp, best_model_tp, "TP"),
          tc = private$harmonic_analysis(df_annual_tc, best_model_tc, "TC")
        )
        # keep plots separate (not JSON)
        self$plots$tp_harmonic <- harmonic_results$tp$plot
        self$plots$tc_harmonic <- harmonic_results$tc$plot
        # drop plot objects from results
        harmonic_results$tp$plot <- NULL
        harmonic_results$tc$plot <- NULL
      }

      # --- Save results (JSON-safe only) ---
      self$results <- list(
        df_tp           = df_annual_tp,
        df_tc           = df_annual_tc,
        best_model_tp   = best_model_tp$summary(),
        best_model_tc   = best_model_tc$summary(),
        other_models_tp = lapply(fit_tp$other_models, function(m) m$summary()),
        other_models_tc = lapply(fit_tc$other_models, function(m) m$summary()),
        corr_tp_tc      = corr_tp_tc,
        tp_breaks       = tp_breaks,
        tp_changepoints = tp_changepoints,
        tc_breaks       = tc_breaks,
        tc_changepoints = tc_changepoints,
        harmonic        = harmonic_results
      )

      cat("[M2][run] Completed successfully.\n")
      invisible(self)
    }
  ),

  private = list(
    aggregate_tp = function(df, year_col) {
      df %>% dplyr::filter(!is.na(.data[[year_col]])) %>%
        dplyr::group_by(.data[[year_col]]) %>%
        dplyr::summarise(Value = dplyr::n(), .groups = "drop") %>%
        dplyr::rename(Year = .data[[year_col]])
    },

    aggregate_tc = function(df, year_col, tc_col) {
      df %>% dplyr::filter(!is.na(.data[[year_col]]), !is.na(.data[[tc_col]])) %>%
        dplyr::group_by(.data[[year_col]]) %>%
        dplyr::summarise(Value = sum(.data[[tc_col]], na.rm = TRUE), .groups = "drop") %>%
        dplyr::rename(Year = .data[[year_col]])
    },

    harmonic_analysis = function(df, model, label) {
      cat("[M2][harmonic] Running harmonic analysis for", label, "...\n")
      ts_data <- stats::ts(df$Value, start = min(df$Year), frequency = 1)

      # --- FFT spectrum ---
      spec <- stats::spectrum(ts_data, plot = FALSE)
      dominant_freq <- spec$freq[which.max(spec$spec)]
      period <- ifelse(dominant_freq > 0, 1 / dominant_freq, NA)

      # --- Residuals ---
      resid <- tryCatch(get_residuals(model), error = function(e) rep(NA, nrow(df)))
      resid_spec <- tryCatch(stats::spectrum(resid, plot = FALSE), error = function(e) NULL)

      # --- Statistical tests ---
      fisher_p <- tryCatch(geneCycle::Fisher.g.test(ts_data)$p.value, error = function(e) NA)
      bartlett_p <- tryCatch(Box.test(resid, type = "Ljung-Box")$p.value, error = function(e) NA)
      bgtest_p <- tryCatch(lmtest::bgtest(resid ~ 1)$p.value, error = function(e) NA)

      # --- JSON-safe summaries ---
      spec_df <- data.frame(freq = spec$freq, spec = spec$spec)
      resid_spec_df <- if (!is.null(resid_spec)) {
        data.frame(freq = resid_spec$freq, spec = resid_spec$spec)
      } else NULL

      # --- Derived flags ---
      is_periodic <- ifelse(!is.na(fisher_p) && fisher_p < 0.05, TRUE, FALSE)
      dominant_period_in_years <- ifelse(!is.na(period), round(period, 1), NA)

      # --- Plot (not stored in results, only in self$plots) ---
      plot_obj <- ggplot2::ggplot(spec_df, ggplot2::aes(x = freq, y = spec)) +
        ggplot2::geom_line(color = "black") +
        ggplot2::labs(
          x = "Frequency",
          y = "Spectral Density",
          title = paste0("Harmonic Spectrum - ", label,
                         " (Dominant Period ≈ ", dominant_period_in_years, " yrs)")
        ) +
        ggplot2::theme_minimal(base_size = 8, base_family = "Times New Roman")

      return(list(
        dominant_freq            = dominant_freq,
        dominant_period          = period,
        dominant_period_in_years = dominant_period_in_years,
        is_periodic              = is_periodic,
        fisher_p                 = fisher_p,
        bartlett_p               = bartlett_p,
        bgtest_p                 = bgtest_p,
        spectrum                 = spec_df,
        resid_spectrum           = resid_spec_df,
        plot                     = plot_obj  # removed later before JSON
      ))
    }
  )
)
