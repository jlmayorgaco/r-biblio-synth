# Make internal package objects available to the test environment without
# widening the public API surface.
pkg_ns <- asNamespace("RBiblioSynth")
pkg_objects <- ls(pkg_ns, all.names = TRUE)
pkg_objects <- pkg_objects[!grepl("^\\.", pkg_objects)]

for (nm in pkg_objects) {
  if (!exists(nm, envir = globalenv(), inherits = FALSE)) {
    assign(nm, get(nm, envir = pkg_ns), envir = globalenv())
  }
}
