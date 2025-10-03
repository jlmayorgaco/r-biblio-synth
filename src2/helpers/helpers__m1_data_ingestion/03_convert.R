# ============================================================================ #
# helpers__m1_data_ingestion/03_convert.R
# Safe wrapper around bibliometrix::convert2df
# ============================================================================ #

m1i_safe_convert <- function(files, db, format) {
  if (length(files) == 0) return(NULL)
  tryCatch({
    suppressMessages(
      suppressWarnings(
        bibliometrix::convert2df(file = files, dbsource = db, format = format)
      )
    )
  }, error = function(e) {
    stop("[M1] Error reading ", toupper(db), " files: ", conditionMessage(e))
  })
}
