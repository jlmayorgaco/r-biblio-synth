# ================================================================
# M2_Production — Module 2: Annual Research Production
# Uses standardized columns from M1: Year, Times_Cited
# Compact JSON: no raw dfs, no big arrays (plots kept in self$plots)
# ================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

M2_Production <- R6::R6Class(
  "M2_Production",
  inherit = M0_Abstract_Module,

  public = list(

    initialize = function(df,
                          year_col     = "Year",
                          tc_col       = "Times_Cited",
                          json_compact = TRUE) {

      # ---- Hard checks with clear messages ----
      if (!(year_col %in% names(df))) {
        stop(sprintf("[M2][init] year_col '%s' not found. Available: %s",
                     year_col, paste(names(df), collapse = ", ")))
      }
      # TC is optional; we allow skipping TC modeling if missing
      tc_missing <- !(tc_col %in% names(df))

      # ---- Ensure types & normalized names ----
      df[[year_col]] <- suppressWarnings(as.integer(df[[year_col]]))
      if (!tc_missing) df[[tc_col]] <- suppressWarnings(as.numeric(df[[tc_col]]))

      # Normalize to internal names for consistency
      if (year_col != "Year")        df$Year        <- df[[year_col]] else df$Year        <- df$Year
      if (!tc_missing) {
        if (tc_col != "Times_Cited") df$Times_Cited <- df[[tc_col]]   else df$Times_Cited <- df$Times_Cited
      }

      self$input  <- df
      self$params <- list(
        year_col     = "Year",
        tc_col       = if (!tc_missing) "Times_Cited" else NULL,
        json_compact = json_compact
      )
      self$results <- list()
      self$plots   <- list()
      self$module_index <- "M2"
    },

    run = function(run_harmonic = TRUE) {
      cat("[M2][run] Starting Annual Production analysis...\n")

      # --- Aggregate annual TP ---
      df_annual_tp <- private$aggregate_tp(self$input, self$params$year_col)

      # --- Aggregate annual TC (optional) ---
      tc_available <- !is.null(self$params$tc_col) &&
                      any(!is.na(self$input[[self$params$tc_col]]))
      df_annual_tc <- if (tc_available) {
        private$aggregate_tc(self$input, self$params$year_col, self$params$tc_col)
      } else NULL
      if (!tc_available) {
        message("[M2][run] Times_Cited not available or all NA. Skipping TC modeling.")
      }

      # --- Fit models ---
      fit_tp <- fit_models(df_annual_tp)
      best_model_tp <- fit_tp$best_model

      fit_tc <- NULL; best_model_tc <- NULL
      if (!is.null(df_annual_tc) && nrow(df_annual_tc) >= 3) {
        fit_tc <- fit_models(df_annual_tc)
        best_model_tc <- fit_tc$best_model
      }

      cat("[M2][run] Best TP model:", fit_tp$best_name, " (R² =", fit_tp$best_r2, ")\n")
      if (!is.null(fit_tc)) {
        cat("[M2][run] Best TC model:", fit_tc$best_name, " (R² =", fit_tc$best_r2, ")\n")
      } else {
        cat("[M2][run] TC model: skipped\n")
      }

      # --- Correlation (only if TC exists) ---
      corr_tp_tc <- NA_real_
      if (!is.null(df_annual_tc) && nrow(df_annual_tc) > 0) {
        corr_tp_tc <- suppressWarnings(
          cor(df_annual_tp$Value, df_annual_tc$Value, use = "pairwise.complete.obs")
        )
        cat("[M2][run] Correlation TP vs TC =", corr_tp_tc, "\n")
      }

      # --- Plots for TP (not serialized) ---
      self$plots$p1            <- plot_regression(df_annual_tp, best_model_tp, fit_tp$best_name, "TP vs Year")
      self$plots$p1_error_acf  <- plot_resid_acf(df_annual_tp, best_model_tp)
      self$plots$p1_error_dw   <- plot_resid_dw(df_annual_tp, best_model_tp)
      self$plots$p1_error_hist <- plot_resid_hist(df_annual_tp, best_model_tp)
      self$plots$p1_error_qq   <- plot_resid_qq(df_annual_tp, best_model_tp)

      # --- Plots for TC (if available) ---
      if (!is.null(best_model_tc)) {
        self$plots$p2            <- plot_regression(df_annual_tc, best_model_tc, fit_tc$best_name, "TC vs Year")
        self$plots$p2_error_acf  <- plot_resid_acf(df_annual_tc, best_model_tc)
        self$plots$p2_error_dw   <- plot_resid_dw(df_annual_tc, best_model_tc)
        self$plots$p2_error_hist <- plot_resid_hist(df_annual_tc, best_model_tc)
        self$plots$p2_error_qq   <- plot_resid_qq(df_annual_tc, best_model_tc)
        self$plots$p3            <- plot_tp_vs_tc(df_annual_tp, df_annual_tc, corr_tp_tc)
      }

      # --- Model comparison overlay (Logistic vs Gompertz for TP) ---
      self$plots$tp_model_comparison <- plot_model_comparison(
        df_annual_tp,
        list(best_model_tp, fit_tp$other_models$gompertz %||% NULL),
        c("Logistic", "Gompertz"),
        "TP Model Comparison"
      )

      # --- Structural breaks / Change points ---
      tp_breaks        <- private$compute_breaks(df_annual_tp)
      tp_changepoints  <- private$compute_cpts(df_annual_tp)
      tc_breaks        <- if (!is.null(df_annual_tc)) private$compute_breaks(df_annual_tc) else NULL
      tc_changepoints  <- if (!is.null(df_annual_tc)) private$compute_cpts(df_annual_tc) else NULL

      # --- Harmonic Analysis (compact JSON; plots stored separately) ---
      harmonic_results <- NULL
      if (isTRUE(run_harmonic)) {
        harmonic_results <- list(
          tp = private$harmonic_analysis(df_annual_tp, best_model_tp, "TP"),
          tc = if (!is.null(best_model_tc)) private$harmonic_analysis(df_annual_tc, best_model_tc, "TC") else NULL
        )
        # keep plots separate (not JSON)
        self$plots$tp_harmonic <- harmonic_results$tp$plot
        if (!is.null(harmonic_results$tc)) self$plots$tc_harmonic <- harmonic_results$tc$plot
        harmonic_results$tp$plot <- NULL
        if (!is.null(harmonic_results$tc)) harmonic_results$tc$plot <- NULL
      }

      # --- Compact harmonic payload (drop big arrays) ---
      if (!is.null(harmonic_results) && isTRUE(self$params$json_compact)) {
        harmonic_results$tp$spectrum       <- NULL
        harmonic_results$tp$resid_spectrum <- NULL
        if (!is.null(harmonic_results$tc)) {
          harmonic_results$tc$spectrum       <- NULL
          harmonic_results$tc$resid_spectrum <- NULL
        }
      }

      # --- Summaries for JSON (compact) ---
      best_model_tp_summary <- tryCatch(best_model_tp$summary(), error = function(e) NULL)
      best_model_tc_summary <- tryCatch(if (!is.null(best_model_tc)) best_model_tc$summary() else NULL,
                                        error = function(e) NULL)

      self$results <- list(
        best_model_tp   = best_model_tp_summary,
        best_model_tc   = best_model_tc_summary,   # NULL if skipped
        other_models_tp = lapply(fit_tp$other_models, function(m) tryCatch(m$summary(), error = function(e) NULL)),
        other_models_tc = if (!is.null(fit_tc)) lapply(fit_tc$other_models, function(m) tryCatch(m$summary(), error = function(e) NULL)) else NULL,
        corr_tp_tc      = corr_tp_tc,              # NA if TC missing
        tp_breaks       = tp_breaks,
        tp_changepoints = tp_changepoints,
        tc_breaks       = tc_breaks,               # NULL if TC missing
        tc_changepoints = tc_changepoints,         # NULL if TC missing
        harmonic        = harmonic_results,        # compacted
        notes           = list(tc_available = tc_available)
      )

      cat("[M2][run] Completed successfully.\n")
      invisible(self)
    }
  ),

  private = list(

    aggregate_tp = function(df, year_col) {
      df %>%
        dplyr::filter(!is.na(.data[[year_col]])) %>%
        dplyr::group_by(.data[[year_col]]) %>%
        dplyr::summarise(Value = dplyr::n(), .groups = "drop") %>%
        dplyr::rename(Year = .data[[year_col]]) %>%
        dplyr::arrange(.data$Year)
    },

    aggregate_tc = function(df, year_col, tc_col) {
      df %>%
        dplyr::filter(!is.na(.data[[year_col]]), !is.na(.data[[tc_col]])) %>%
        dplyr::group_by(.data[[year_col]]) %>%
        dplyr::summarise(Value = sum(.data[[tc_col]], na.rm = TRUE), .groups = "drop") %>%
        dplyr::rename(Year = .data[[year_col]]) %>%
        dplyr::arrange(.data$Year)
    },

    compute_breaks = function(df_year_value) {
      tryCatch({
        bp <- strucchange::breakpoints(Value ~ Year, data = df_year_value)
        bp_years <- if (!is.null(bp$breakpoints)) df_year_value$Year[bp$breakpoints] else integer(0)
        list(
          breakpoints = bp_years,
          nbreaks     = bp$nbreaks %||% length(bp_years),
          rss         = bp$rss %||% NA_real_,
          BIC         = tryCatch(BIC(bp), error = function(e) NA_real_),
          AIC         = tryCatch(AIC(bp), error = function(e) NA_real_)
        )
      }, error = function(e) NULL)
    },

    compute_cpts = function(df_year_value) {
      tryCatch({
        cp <- changepoint::cpt.meanvar(df_year_value$Value, method = "PELT")
        cp_years <- df_year_value$Year[changepoint::cpts(cp)]
        list(
          cpts      = cp_years,
          pen.value = cp@pen.value %||% NA,
          test.stat = cp@test.stat  %||% NA
        )
      }, error = function(e) NULL)
    },

    harmonic_analysis = function(df, model, label) {
      cat("[M2][harmonic] Running harmonic analysis for", label, "...\n")
      ts_data <- stats::ts(df$Value, start = min(df$Year), frequency = 1)

      # --- FFT spectrum (only for dominant period) ---
      spec <- stats::spectrum(ts_data, plot = FALSE)
      dominant_freq <- spec$freq[which.max(spec$spec)]
      period <- ifelse(dominant_freq > 0, 1 / dominant_freq, NA)

      # --- Residuals spectrum (for flags/tests) ---
      resid <- tryCatch(get_residuals(model), error = function(e) rep(NA_real_, nrow(df)))
      resid_spec <- tryCatch(stats::spectrum(resid, plot = FALSE), error = function(e) NULL)

      # --- Statistical tests ---
      fisher_p   <- tryCatch(geneCycle::Fisher.g.test(ts_data)$p.value,   error = function(e) NA_real_)
      bartlett_p <- tryCatch(Box.test(resid, type = "Ljung-Box")$p.value, error = function(e) NA_real_)
      bgtest_p   <- tryCatch(lmtest::bgtest(resid ~ 1)$p.value,           error = function(e) NA_real_)

      # --- Arrays (dropped from JSON in compact mode) ---
      spec_df <- data.frame(freq = spec$freq, spec = spec$spec)
      resid_spec_df <- if (!is.null(resid_spec)) {
        data.frame(freq = resid_spec$freq, spec = resid_spec$spec)
      } else NULL

      # --- Derived flags ---
      is_periodic <- isTRUE(!is.na(fisher_p) && fisher_p < 0.05)
      dominant_period_in_years <- ifelse(!is.na(period), round(period, 1), NA)

      # --- Plot (stored in self$plots, not JSON) ---
      plot_obj <- ggplot2::ggplot(spec_df, ggplot2::aes(x = freq, y = spec)) +
        ggplot2::geom_line(color = "black") +
        ggplot2::labs(
          x = "Frequency",
          y = "Spectral Density",
          title = paste0("Harmonic Spectrum - ", label,
                         " (Dominant Period ≈ ", dominant_period_in_years, " yrs)")
        ) +
        ggplot2::theme_minimal(base_size = 8, base_family = "Times New Roman")

      list(
        dominant_freq            = dominant_freq,
        dominant_period          = period,
        dominant_period_in_years = dominant_period_in_years,
        is_periodic              = is_periodic,
        fisher_p                 = fisher_p,
        bartlett_p               = bartlett_p,
        bgtest_p                 = bgtest_p,
        spectrum                 = spec_df,        # removed if json_compact=TRUE
        resid_spectrum           = resid_spec_df,  # removed if json_compact=TRUE
        plot                     = plot_obj        # removed from JSON above
      )
    }
  )
)
