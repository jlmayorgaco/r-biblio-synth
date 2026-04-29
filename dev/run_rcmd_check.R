args <- commandArgs(trailingOnly = TRUE)
project_root <- if (length(args) >= 1L) normalizePath(args[1], winslash = "/", mustWork = FALSE) else normalizePath(getwd(), winslash = "/", mustWork = FALSE)

old_force_suggests <- Sys.getenv("_R_CHECK_FORCE_SUGGESTS_", unset = NA_character_)
on.exit({
  if (is.na(old_force_suggests)) {
    Sys.unsetenv("_R_CHECK_FORCE_SUGGESTS_")
  } else {
    Sys.setenv(`_R_CHECK_FORCE_SUGGESTS_` = old_force_suggests)
  }
}, add = TRUE)

Sys.setenv(`_R_CHECK_FORCE_SUGGESTS_` = "false")

r_bin <- file.path(R.home("bin"), "R")
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(project_root)

build_output <- system2(r_bin, c("CMD", "build", "."), stdout = TRUE, stderr = TRUE)
cat(paste(build_output, collapse = "\n"), sep = "\n")
build_status <- attr(build_output, "status")
if (!is.null(build_status) && is.finite(build_status) && build_status != 0L) {
  quit(status = as.integer(build_status))
}

tarballs <- list.files(project_root, pattern = "^RBiblioSynth_.*[.]tar[.]gz$", full.names = TRUE)
if (length(tarballs) == 0L) {
  stop("R CMD build did not produce a source tarball.")
}
tarball <- tarballs[which.max(file.info(tarballs)$mtime)]

check_output <- system2(r_bin, c("CMD", "check", "--no-manual", basename(tarball)), stdout = TRUE, stderr = TRUE)
cat("\n", paste(check_output, collapse = "\n"), sep = "")
check_status <- attr(check_output, "status")
if (is.null(check_status) || !is.finite(check_status)) {
  check_status <- 0L
}
quit(status = as.integer(check_status))
