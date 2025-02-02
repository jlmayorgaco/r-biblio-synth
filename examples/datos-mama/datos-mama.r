# Load necessary libraries
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)  # For north arrow and scale bar
library(stringi)    # For character normalization

cat("[INFO] Starting the script...\n")

# Step 1: Load shapefile
shapefile_path <- "/Users/jorge.mayorga/Documents/GitHub/r-biblio-synth/examples/datos-mama/Municipios.shp"

# Debug: Check if the shapefile exists
if (!file.exists(shapefile_path)) {
  stop("[ERROR] Shapefile not found! Ensure the file path is correct.")
}

cat("[INFO] Reading shapefile...\n")
cordoba_map <- st_read(shapefile_path, options = "ENCODING=LATIN1")

# Debug: Check if column Municipio exists
if (!"MPIO_CNMBR" %in% names(cordoba_map)) {
  stop("[ERROR] Column 'MPIO_CNMBR' not found in shapefile.")
}

# Debug: Inspect encoding and structure
cat("[INFO] Inspecting shapefile encoding and structure...\n")
cordoba_map$MPIO_CNMBR <- as.character(cordoba_map$MPIO_CNMBR)
cat("[INFO] Encoding of cordoba_map$MPIO_CNMBR before fix:\n")
print(Encoding(cordoba_map$MPIO_CNMBR))

cordoba_map$MPIO_CNMBR <- iconv(cordoba_map$MPIO_CNMBR, from = "LATIN1", to = "UTF-8", sub = "")
cat("[INFO] Sample names after encoding fix:\n")
print(head(cordoba_map$MPIO_CNMBR))

# Rename column for consistency
cat("[INFO] Renaming column 'MPIO_CNMBR' to 'Municipio'...\n")
cordoba_map <- cordoba_map %>% rename(Municipio = MPIO_CNMBR)

# Debug: Confirm column renaming
cat("[INFO] Column names after renaming:\n")
print(names(cordoba_map))

# Step 2: Filter municipalities in Córdoba
department_column <- "DEPTO"
if (!department_column %in% names(cordoba_map)) {
  stop(paste("[ERROR] Column", department_column, "not found in shapefile."))
}

cat("[INFO] Filtering municipalities for the department 'CORDOBA'...\n")
cordoba_map <- cordoba_map %>%
  filter(trimws(tolower(!!sym(department_column))) == "cordoba")

# Debug: Confirm filtering
cat("[INFO] Number of municipalities in Córdoba:", nrow(cordoba_map), "\n")
if (nrow(cordoba_map) == 0) {
  stop("[ERROR] No municipalities found for the department 'CORDOBA'.")
}

# Step 3: Load incidence data
cat("[INFO] Loading incidence data...\n")
data <- data.frame(
  Municipio = c("Cotorra", "Canalete", "La Apartada", "Cereté", "Puerto libertador",
                "Chinú", "Montería", "Lorica", "Chimá", "Planeta Rica", "Tierralta",
                "Sahagún", "San Bernardo del Viento", "Ciénaga de Oro", "San Antero",
                "Purísima", "Montelíbano", "San Pelayo", "Puerto Escondido",
                "San Andrés Sotavento", "Buenavista", "San Carlos", "Pueblo Nuevo",
                "Ayapel", "Los Córdobas", "Valencia", "Moñitos", "Momil", "Tuchín",
                "San José de Uré"),
  Incidencia = c(88.4, 67.1, 63.8, 58.3, 48.8, 44.9, 44.5, 38.3, 37.1, 35.2,
                 31.3, 31.3, 30.2, 28.5, 27.6, 27.2, 26.7, 23.8, 23.5, 17.9,
                 17.7, 17.3, 15.7, 14.4, 9.9, 7.9, 6.2, 4.8, 3.5, 0)
)

# Debug: Normalize names for matching
cat("[INFO] Normalizing names for matching...\n")
normalize_names <- function(x) {
  x <- trimws(tolower(x))                # Convert to lowercase and trim spaces
  x <- stri_replace_all_regex(x, "[\u00e1]", "a")  # Replace accented characters
  x <- stri_replace_all_regex(x, "[\u00e9]", "e")
  x <- stri_replace_all_regex(x, "[\u00ed]", "i")
  x <- stri_replace_all_regex(x, "[\u00f3]", "o")
  x <- stri_replace_all_regex(x, "[\u00fa]", "u")
  return(x)
}

data$Municipio <- normalize_names(data$Municipio)
cordoba_map$Municipio <- normalize_names(cordoba_map$Municipio)

# Debug: Check matching municipalities
cat("[INFO] Checking unmatched municipalities...\n")
unmatched_in_data <- setdiff(data$Municipio, cordoba_map$Municipio)
unmatched_in_shapefile <- setdiff(cordoba_map$Municipio, data$Municipio)

if (length(unmatched_in_data) > 0) {
  cat("[WARNING] Municipalities in data but not in shapefile:\n")
  print(unmatched_in_data)
}

if (length(unmatched_in_shapefile) > 0) {
  cat("[WARNING] Municipalities in shapefile but not in data:\n")
  print(unmatched_in_shapefile)
}

# Manual replacement for specific cases
cat("[INFO] Applying manual fixes for unmatched names...\n")
cordoba_map$Municipio <- gsub("mo\\?itos", "moñitos", cordoba_map$Municipio, ignore.case = TRUE)

# Step 4: Merge datasets
cat("[INFO] Merging datasets...\n")
cordoba_map <- cordoba_map %>%
  left_join(data, by = "Municipio")

# Debug: Ensure labels are centered
cat("[INFO] Calculating proper label positions...\n")
cordoba_map <- cordoba_map %>%
  mutate(label_position = st_point_on_surface(geometry))

# Step 5: Create the map
cat("[INFO] Creating the map...\n")
ggplot_map <- ggplot(cordoba_map) +
  geom_sf(aes(fill = Incidencia), color = "black", size = 0.2) +
  geom_sf_text(data = cordoba_map, aes(label = paste("", "\n", Incidencia), geometry = label_position), size = 5.5, color = "black") +
  scale_fill_gradient(
    low = "#e5f5e0",
    high = "#006d2c",
    na.value = "gray",
    name = "Incidencia\npor 100k habitantes"
  ) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),  # Remove X and Y labels
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0.5)
  ) +
  labs(
    title = "Mapa de Incidencia por 100k habitantes en Córdoba, Colombia"
  )

# Save the map
cat("[INFO] Saving the map...\n")
ggsave("cordoba_map_for_word.png", plot = ggplot_map, width = 8.5, height = 11, dpi = 300)

cat("[INFO] Map saved successfully!\n")
