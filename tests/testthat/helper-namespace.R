# Make internal package objects available to the test environment without
# polluting the global environment with exported symbols. Exported functions are
# already reachable through the attached development package when `load_all()`
# runs with helpers enabled.
pkg_ns <- asNamespace("RBiblioSynth")
pkg_exports <- getNamespaceExports("RBiblioSynth")
pkg_objects <- ls(pkg_ns, all.names = TRUE)
pkg_objects <- pkg_objects[!grepl("^\\.", pkg_objects)]
pkg_objects <- setdiff(pkg_objects, pkg_exports)

for (nm in pkg_objects) {
  if (!exists(nm, envir = globalenv(), inherits = FALSE)) {
    assign(nm, get(nm, envir = pkg_ns), envir = globalenv())
  }
}
