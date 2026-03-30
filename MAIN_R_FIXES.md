# MAIN.R FIXES APPLIED

## Problem
main.r was failing because:
1. `config` was created as a plain `list()` instead of using `biblio_config()`
2. This caused issues when functions called `merge_biblio_config(config)` expecting proper structure
3. The validation script also had path issues when not run from project root

## Fixes Applied

### 1. main.r - Line 46-58 (Configuration)
**Before:**
```r
config <- list(
  output_dir     = "results",
  export_plots   = TRUE,
  ...
)
```

**After:**
```r
config <- biblio_config(
  output_dir     = "results",
  export_plots   = TRUE,
  ...
)
```

### 2. main.r - Line 79-80 (Loading Config)
**Before:**
```r
load_config <- biblio_config()
load_config$verbose <- FALSE
```

**After:**
```r
load_config <- biblio_config(verbose = FALSE)
```

### 3. R/validate_integration.R - Bootstrap Loading (Lines 44-52)
**Before:**
```r
tryCatch({
  source("R/core/bootstrap.R")
  ...
}
```

**After:**
```r
tryCatch({
  # Determine script location for reliable sourcing
  script_path <- tryCatch({
    normalizePath(dirname(sys.frame(1)$ofile), mustWork = FALSE)
  }, error = function(e) {
    getwd()
  })
  
  # Try multiple paths to find bootstrap
  bootstrap_paths <- c(
    file.path(script_path, "core", "bootstrap.R"),
    file.path(script_path, "..", "R", "core", "bootstrap.R"),
    file.path(getwd(), "R", "core", "bootstrap.R"),
    "R/core/bootstrap.R"
  )
  
  bootstrap_loaded <- FALSE
  for (bp in bootstrap_paths) {
    if (file.exists(bp)) {
      source(bp)
      ...
    }
  }
  ...
}
```

### 4. R/validate_integration.R - Key Files Check (Lines 185-189)
**Before:**
```r
check_file("R/core/bootstrap.R", "Bootstrap")
check_file("R/module_m1/m1_run.R", "M1 Runner")
...
```

**After:**
```r
# Get script directory for reliable file checking
script_dir <- tryCatch({
  normalizePath(dirname(sys.frame(1)$ofile), mustWork = FALSE)
}, error = function(e) {
  getwd()
})

# Build paths relative to script or working directory
key_files <- list(
  list(path = file.path(script_dir, "core", "bootstrap.R"), 
       alt = file.path(getwd(), "R", "core", "bootstrap.R"), 
       desc = "Bootstrap"),
  ...
)

for (kf in key_files) {
  if (file.exists(kf$path)) {
    check_file(kf$path, kf$desc)
  } else if (file.exists(kf$alt)) {
    check_file(kf$alt, kf$desc)
  } else {
    # Try relative path as fallback
    rel_path <- gsub(paste0(getwd(), "/"), "", kf$alt, fixed = TRUE)
    check_file(rel_path, kf$desc)
  }
}
```

## How to Test

### Test main.r:
```r
setwd("C:/Users/walla/Documents/Github/r-biblio-synth/examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR")
source("main.r")
```

### Test validation:
```r
setwd("C:/Users/walla/Documents/Github/r-biblio-synth")
source("R/validate_integration.R")
```

### Run all tests:
```r
setwd("C:/Users/walla/Documents/Github/r-biblio-synth")
testthat::test_dir("tests/testthat")
```

## Expected Results

- All 68 functional tests should PASS
- Key Files section should now PASS (no more "file not found" errors)
- main.r should run successfully through M1, M2, M3
- No more "replacement has length zero" errors

## Key Insight

The "file not found" errors in validation were **false positives** - the actual runner functions (run_m1, run_m2, run_m3) were passing, proving the files exist. The issue was the validation script's path handling when run from different working directories.