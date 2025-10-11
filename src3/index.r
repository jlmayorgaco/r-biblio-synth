#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(optparse)
  library(targets)
  library(renv)
})

option_list <- list(
  make_option("--config", type="character", default="src2/00_config/defaults.yml"),
  make_option("--models", type="character", default="src2/00_config/models.yml"),
  make_option("--clean",  action="store_true", default=FALSE),
  make_option("--workers", type="integer", default=parallel::detectCores()-1)
)
opt <- parse_args(OptionParser(option_list = option_list))

Sys.setenv(RBIB_CFG = normalizePath(opt$config),
           RBIB_MODELS = normalizePath(opt$models))

# Reproducibility
if (file.exists("renv.lock")) renv::activate()

# Targets configuration
tar_option_set(workspace_on_error = TRUE)

if (opt$clean) tar_destroy()

# Parallel plan (optional)
future::plan(future::multisession, workers = opt$workers)

# Run pipeline
source("src2/99_pipeline/targets.R")
targets::tar_make()  # or tar_make_clustermq()
