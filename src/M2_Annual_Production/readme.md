src/
├── M2_Annual_Production/
│   ├── _helpers.r                     # Shared helper functions (e.g., calculations, anomalies)
│   ├── _utils.r                       # Utility functions (e.g., file handling, formatting)
│   ├── _settings.r                    # Plot styling, JSON settings, etc.
│   ├── m2_m0_eda.r                    # Class for Metric 0: EDA (Exploratory Data Analysis)
│   ├── m2_m1_regression.r             # Class for Metric 1: Regression Models
│   ├── m2_m2_moving_averages.r        # Class for Metric 2: Moving Averages
│   ├── m2_m3_growth_differences.r     # Class for Metric 3: Annual Growth Differences
│   ├── m2_m4_fft.r                    # Class for Metric 4: Fourier Transform
│   ├── m2_m5_harmonics_regression.r   # Class for Metric 5: Harmonic Regression
│   ├── m2_m6_acf.r                    # Class for Metric 6: Autocorrelation Analysis
│   ├── m2_m7_lomb_scargle.r           # Class for Metric 7: Lomb-Scargle Periodogram
│   ├── m2_m8_wavelet.r                # Class for Metric 8: Wavelet Power Spectrum
│   ├── m2_m9_loess_trend.r            # Class for Metric 9: Loess Trend Analysis
│   ├── m2_m10_custom_metric.r         # Placeholder for additional custom metrics
└── main.r                             # Entry point script