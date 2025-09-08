extract_country_from_affiliation <- function(aff_str) {
  if (is.na(aff_str) || aff_str == "") return(NA)
  aff_parts <- unlist(strsplit(aff_str, ";"))
  countries <- sapply(aff_parts, function(x) {
    parts <- strsplit(x, ",")[[1]]
    trimws(tail(parts, 1))
  })
  countries <- unique(countries)
  countries[countries != ""]
}
