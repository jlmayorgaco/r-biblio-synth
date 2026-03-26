# ============================================================================
# module_m2/compute/m2_compute_harmonics.R - Harmonic analysis (FIXED)
# ============================================================================

#' @export
compute_m2_harmonics <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) < 10) {
    return(list(status = "error", fft = list(), lomb = list(), harmonic_models = list(), r_squared = data.frame()))
  }

  years <- as.numeric(input$Year)
  articles <- as.numeric(input$Articles)

  # FFT (FIXED: use actual time step)
  dt <- mean(diff(years))
  fft_result <- m2_perform_fft(articles, dt = dt)

  # Lomb-Scargle (FIXED: use lomb:: namespace)
  lomb_result <- tryCatch({
    lomb::lsp(articles, times = years, type = "period", ofac = 10, plot = FALSE)
  }, error = function(e) NULL)

  lomb_serialized <- if (!is.null(lomb_result)) {
    list(frequency = lomb_result$freq, power = lomb_result$power, type = lomb_result$type)
  } else {
    list()
  }

  # Harmonic regression
  freq <- fft_result$frequencies
  harmonic_models <- m2_fit_harmonic_regression(years, articles, freq[1:min(10, length(freq))])

  # R2 vs frequency
  r2_table <- m2_evaluate_harmonic_r2(harmonic_models, articles)

  # Wavelet analysis
  wavelet_result <- tryCatch({
    WaveletComp::analyze.wavelet(
      my.data = data.frame(Time = years, Signal = articles),
      my.series = "Signal",
      verbose = FALSE
    )
  }, error = function(e) NULL)

  wavelet_serialized <- if (!is.null(wavelet_result)) {
    list(
      time = wavelet_result$axis.1,
      period = wavelet_result$axis.2,
      power = wavelet_result$Power
    )
  } else {
    list()
  }

  list(
    status = "success",
    fft = fft_result,
    lomb = lomb_serialized,
    wavelet = wavelet_serialized,
    harmonic_models = lapply(harmonic_models, function(m) list(frequency = m$frequency, r2 = m$r2)),
    r_squared_table = r2_table
  )
}

#' Perform FFT (FIXED: use actual time step)
m2_perform_fft <- function(y, dt = 1) {
  n <- length(y)
  fft_res <- fft(y)
  # Correct frequency calculation with time step
  frequencies <- (0:(floor(n / 2))) / (n * dt)
  magnitude <- Mod(fft_res[1:length(frequencies)])
  phase <- Arg(fft_res[1:length(frequencies)])
  list(frequencies = frequencies, magnitude = round(magnitude, 4), phase = round(phase, 4))
}

#' Fit harmonic regression models
m2_fit_harmonic_regression <- function(x, y, frequencies) {
  models <- list()
  for (freq in frequencies[frequencies > 0]) {
    fit <- tryCatch(
      lm(y ~ sin(2 * pi * freq * x) + cos(2 * pi * freq * x)),
      error = function(e) NULL)
    if (!is.null(fit)) {
      preds <- predict(fit)
      ss_res <- sum((y - preds)^2)
      ss_tot <- sum((y - mean(y))^2)
      r2 <- if (ss_tot > 0) 1 - ss_res / ss_tot else 0
      models[[as.character(round(freq, 6))]] <- list(frequency = freq, fit = fit, r2 = round(r2, 6))
    }
  }
  models
}

#' Evaluate R2 vs frequency
m2_evaluate_harmonic_r2 <- function(models, y) {
  if (length(models) == 0) return(data.frame(Frequency = numeric(), R2 = numeric()))
  do.call(rbind, lapply(models, function(m) {
    data.frame(Frequency = round(m$frequency, 6), R2 = m$r2)
  }))
}
