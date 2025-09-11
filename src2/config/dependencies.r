# ================================================================
# dependencies.r — Package Management (silent version)
# ================================================================

# CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Quiet installer/loader
# Quiet installer/loader
install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat("[deps] Installing:", pkg, "\n")
      tryCatch(
        suppressPackageStartupMessages(
          suppressWarnings(
            install.packages(pkg, dependencies = TRUE, quiet = TRUE)
          )
        ),
        error = function(e) cat("[deps] Failed to install:", pkg, "\n")
      )
    }
    
    # Load silently (no startup messages, no S3 overwrite logs)
    suppressPackageStartupMessages(
      suppressWarnings(
        library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
      )
    )
  }
}


# Master list of required packages
packages <- c(
  # 📊 Data manipulation
  "dplyr","tibble","tidyr","broom","reshape2",
  
  # 📈 Visualization
  "ggplot2","plotly","ggrepel","RColorBrewer","patchwork","ggsci",
  "treemapify","ggfortify","extrafont","ggwordcloud","wordcloud2",
  "grid","gridSVG","gridExtra",
  
  # 🌍 Maps
  "rworldmap","maps","geosphere","countrycode",
  
  # 📦 Export
  "jsonlite","htmlwidgets","webshot","png",
  
  # 🧩 Text mining
  "tm","stopwords","bibliometrix",
  
  # 📊 Stats
  "car","MASS","nortest","tseries","lmtest","BayesFactor","psych",
  "wavelets","fitdistrplus","ineq","moments",
  
  # 🛠 Optimization & regression
  "nls2","minpack.lm",
  
  # 📊 Model performance
  "Metrics","rlang",
  
  # ⏱ Time series
  "forecast","changepoint","lomb","WaveletComp","signal","splines",
  
  # ⚙️ Robustness
  "boot","robustbase",
  
  # 🔄 Autocorrelation/spectral
  "orcutt","spectral","waveslim",
  
  # Diagnostics
  "e1071",
  
  # 📋 Reporting
  "kableExtra","knitr","pander",
  
  # 📊 Networks
  "ggraph","tidygraph","igraph", "concaveman",
  
  # Misc
  "purrr","skimr","GGally","ggbrace","DescTools","text2vec","cowplot", "latex2exp", "patchwork"
)

# Install + load
install_and_load(packages)
