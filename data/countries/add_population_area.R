# ============================================================================
# data/countries/add_population_area.R
# Add population and area - simplified version
# ============================================================================

library(readr)

BASE_DIR <- "data/countries"
ref_file <- file.path(BASE_DIR, "countries_reference.csv")

cat("Adding population and area to countries_reference.csv...\n\n")

ref <- read_csv(ref_file)

# Add columns if they don't exist
if (!"population" %in% names(ref)) ref$population <- NA
if (!"area_km2" %in% names(ref)) ref$area_km2 <- NA

# Key countries with population and area
key_data <- list(
  "USA" = c(331002651, 9833517),
  "CHN" = c(1402112000, 9596961),
  "IND" = c(1380004385, 3287263),
  "BRA" = c(212559417, 8515767),
  "PAK" = c(220892340, 881912),
  "NGA" = c(206139589, 923768),
  "BGD" = c(164689383, 147570),
  "RUS" = c(144104080, 17098242),
  "MEX" = c(128932753, 1964375),
  "JPN" = c(125836021, 377975),
  "DEU" = c(83783942, 357022),
  "GBR" = c(67886011, 243610),
  "FRA" = c(67390000, 643801),
  "ITA" = c(60461826, 301339),
  "CAN" = c(37742154, 9984670),
  "KOR" = c(51804417, 100210),
  "AUS" = c(25499884, 7692024),
  "ESP" = c(46754778, 505990),
  "COL" = c(50882891, 1141748),
  "ARG" = c(45101774, 2780400),
  "ZAF" = c(59308690, 1221037),
  "SAU" = c(34813871, 2149690),
  "TUR" = c(84339067, 783562),
  "THA" = c(112487396, 513120),
  "VNM" = c(97338579, 331212),
  "UKR" = c(43733762, 603550),
  "POL" = c(38338289, 312696),
  "ECU" = c(17643054, 283561),
  "PER" = c(32971854, 1285216),
  "MYS" = c(32365999, 329847),
  "NLD" = c(17441139, 41850),
  "CHL" = c(19116209, 756102),
  "BOL" = c(11856690, 1098581),
  "PRY" = c(7132538, 406752),
  "URY" = c(3473730, 176215),
  "VEN" = c(28435940, 916445),
  "EGY" = c(102334404, 1001450),
  "IRN" = c(83992949, 1648195),
  "IRQ" = c(40262475, 438317),
  "IDN" = c(273523621, 1904568),
  "PHL" = c(115559009, 300000)
)

# Add population and area
count <- 0
for (iso3 in names(key_data)) {
  idx <- which(ref$iso3 == iso3)
  if (length(idx) > 0) {
    ref$population[idx] <- key_data[[iso3]][1]
    ref$area_km2[idx] <- key_data[[iso3]][2]
    count <- count + 1
  }
}

# Save
write_csv(ref, ref_file)

cat("Updated: ", nrow(ref), " rows\n")
cat("Countries with population: ", sum(!is.na(ref$population)), "\n")
cat("Countries with area: ", sum(!is.na(ref$area_km2)), "\n\n")

cat("Sample:\n")
print(head(ref %>% filter(!is.na(population)), 3))
