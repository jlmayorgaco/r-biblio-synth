# ============================================================================
# m2_advanced_report_helpers.R - Report fragments for optional M2 analytics
# ============================================================================

m2_advanced_report_lines <- function(advanced) {
  if (!is.list(advanced) || !identical(advanced$status %||% "", "success")) {
    reason <- advanced$reason %||% "Advanced M2 analytics were not available."
    return(c("Advanced Journal Analytics", paste0("  Status: ", advanced$status %||% "unavailable"), paste0("  Reason: ", reason)))
  }

  gr <- advanced$growth_regimes$summary %||% tibble::tibble()
  fv <- advanced$forecast_validation$leaderboard %||% tibble::tibble()
  cp <- advanced$changepoint_consensus$table %||% tibble::tibble()
  hyp <- advanced$hypotheses$table %||% tibble::tibble()

  headline <- if (is.data.frame(gr) && nrow(gr) > 0) {
    sprintf(
      "  Headline advanced model: %s; capacity %.2f; inflection %.1f; distance to saturation %.1f%%.",
      gr$headline_model[1],
      suppressWarnings(as.numeric(gr$capacity[1])),
      suppressWarnings(as.numeric(gr$inflection_year[1])),
      100 * suppressWarnings(as.numeric(gr$distance_to_saturation[1]))
    )
  } else {
    "  Headline advanced model: not estimable."
  }

  forecast <- if (is.data.frame(fv) && nrow(fv) > 0) {
    best <- fv[which.min(fv$mase), , drop = FALSE]
    sprintf(
      "  Best rolling-origin forecast: %s at horizon %s (MASE %.3f, RMSE %.3f).",
      best$model[1],
      best$horizon[1],
      suppressWarnings(as.numeric(best$mase[1])),
      suppressWarnings(as.numeric(best$rmse[1]))
    )
  } else {
    "  Rolling-origin forecast validation: not estimable."
  }

  breaks <- if (is.data.frame(cp) && nrow(cp) > 0) {
    top <- cp[order(-cp$support_count, cp$breakpoint_year), , drop = FALSE][1, , drop = FALSE]
    sprintf(
      "  Strongest changepoint consensus: %s with %d supporting method(s), decision=%s.",
      top$breakpoint_year[1],
      as.integer(top$support_count[1]),
      top$decision[1]
    )
  } else {
    "  Changepoint consensus: not estimable."
  }

  hyp_line <- if (is.data.frame(hyp) && nrow(hyp) > 0) {
    sprintf(
      "  Advanced hypotheses: %d supported / %d not supported / %d inconclusive.",
      sum(hyp$decision == "supported", na.rm = TRUE),
      sum(hyp$decision == "not_supported", na.rm = TRUE),
      sum(hyp$decision == "inconclusive", na.rm = TRUE)
    )
  } else {
    "  Advanced hypotheses: not available."
  }

  c("Advanced Journal Analytics", headline, forecast, breaks, hyp_line)
}

m2_advanced_report_tex <- function(advanced) {
  lines <- m2_advanced_report_lines(advanced)
  c(
    "\\subsection*{Advanced Journal Analytics}",
    paste(m0_escape_latex(lines[-1]), collapse = "\\\\")
  )
}
