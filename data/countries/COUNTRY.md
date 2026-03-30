# COUNTRY.md - Dataset de países para análisis M3

## Resumen

Este documento especifica los datasets de referencia de países para enriquecer el
análisis bibliométrico en M3 (Countries). Incluye datos geográficos, económicos y
de desarrollo para todos los países del mundo con series temporales desde 1970.

---

## Estructura de archivos

```
data/
└── countries/
    ├── countries_reference.csv              # Referencia geográfica y clasificación
    ├── indicators_metadata.csv             # Catálogo de indicadores
    ├── countries_indicators/
    │   ├── econ/
    │   │   ├── gdp_growth.csv
    │   │   └── gdp_pc_ppp.csv
    │   ├── innovation/
    │   │   ├── rd_gdp.csv
    │   │   ├── gerd.csv
    │   │   ├── rd_business.csv
    │   │   ├── researchers_pm.csv
    │   │   ├── rd_personnel_pm.csv
    │   │   ├── articles.csv
    │   │   ├── citable_docs.csv
    │   │   ├── sjr_rank.csv
    │   │   ├── patents_res.csv
    │   │   └── patents_pct.csv
    │   ├── human_capital/
    │   │   ├── hdi.csv
    │   │   ├── tertiary_enroll.csv
    │   │   ├── education_gdp.csv
    │   │   └── doctoral_grads.csv
    │   ├── technology/
    │   │   ├── internet_users.csv
    │   │   ├── high_tech_exports.csv
    │   │   ├── gini.csv
    │   │   ├── ip_receipts.csv
    │   │   └── urban_pop.csv
    │   └── projections/
    │       ├── gdp_forecast.csv
    │       └── population_forecast.csv
    └── update_indicators.R                # Script de actualización manual
```

---

## Dataset 1: countries_reference.csv

**Descripción**: Referencia geográfica estática y clasificación de países.

### Columnas

| Columna | Tipo | Descripción | Ejemplo |
|---------|------|-------------|---------|
| `iso2` | chr | Código ISO 2 letras | "CO", "US" |
| `iso3` | chr | Código ISO 3 letras (merge key) | "COL", "USA" |
| `country_name` | chr | Nombre oficial inglés | "Colombia", "United States" |
| `continent` | chr | Continente | "Americas", "Europe", "Asia", "Africa", "Oceania" |
| `region` | chr | Subregión UN geoscheme | "South America", "Western Europe" |
| `income_group` | chr | Clasificación Banco Mundial | "High income", "Upper middle income", "Lower middle income", "Low income" |
| `latitude` | dbl | Latitud centroide | 4.5709 |
| `longitude` | dbl | Longitud centroide | -74.2973 |
| `population` | int | Población más reciente | 51268632 |
| `area_km2` | dbl | Área en km² | 1141748 |

### Fuentes

| Dato | Fuente | Método |
|------|--------|--------|
| Códigos ISO | ISO 3166 | Descarga directa |
| Nombres países | World Bank | API |
| Continente/Región | UN geoscheme | CSV oficial UN |
| Coordenadas | Natural Earth | shapefile |
| Income group | World Bank | API |
| Población/Área | World Bank | API |

### Ejemplo de contenido

```csv
iso2,iso3,country_name,continent,region,income_group,latitude,longitude,population,area_km2
CO,COL,Colombia,Americas,South America,Upper middle income,4.5709,-74.2973,51268632,1141748
US,USA,United States,Americas,Northern America,High income,37.0902,-95.7129,331002651,9833517
CN,CHN,China,Asia,Eastern Asia,Upper middle income,35.8617,104.1954,1402112000,9596961
```

---

## Dataset 2: countries_indicators/ (Formato largo)

**Descripción**: Indicadores en formato long/panel con series temporales desde 1970.

### Estructura general

Cada archivo CSV tiene 3 columnas:

| Columna | Tipo | Descripción |
|---------|------|-------------|
| `iso3` | chr | Código ISO 3 letras |
| `year` | int | Año (1970–2050 para proyecciones) |
| `value` | dbl | Valor del indicador |

### Ejemplo: gdp_pc_ppp.csv

```csv
iso3,year,value
USA,1960,15733.12
USA,1961,15901.45
USA,1962,16165.78
COL,1960,4120.33
COL,1961,4198.67
```

---

## Catálogo de indicadores: indicators_metadata.csv

| indicator | variable | description | unit | source | start_year | tier |
|-----------|----------|-------------|------|--------|------------|------|
| GDP per capita PPP | `gdp_pc_ppp` | GDP per capita purchasing power parity | USD | World Bank WDI | 1960 | 1 |
| GDP growth | `gdp_growth` | Annual GDP growth rate | % | World Bank WDI | 1960 | 3 |
| R&D intensity | `rd_gdp` | Research & development expenditure | % GDP | UNESCO UIS | 1996 | 1 |
| GERD | `gerd` | Gross domestic R&D expenditure | USD const 2015 | UNESCO UIS | 1970 | 2 |
| GERD business sector | `rd_business` | Business sector R&D | % total R&D | UNESCO UIS | 1990 | 3 |
| Researchers | `researchers_pm` | Researchers per million inhabitants | per million | UNESCO UIS | 1990 | 1 |
| R&D personnel | `rd_personnel_pm` | Total R&D personnel | per million | UNESCO UIS | 1990 | 2 |
| Scientific articles | `articles` | Scientific & technical articles published | count | UNESCO UIS | 1975 | 1 |
| SJR citable docs | `citable_docs` | SCImago citable documents (Scopus) | count | SCImago | 1999 | 1 |
| SJR rank | `sjr_rank` | SCImago country rank by documents | rank | SCImago | 1999 | 2 |
| Patents resident | `patents_res` | Patent applications resident | count | WIPO/UNESCO | 1980 | 2 |
| Patents PCT | `patents_pct` | PCT international applications | count | WIPO | 1990 | 3 |
| High-tech exports | `high_tech_exports` | High-technology exports | % manuf exports | World Bank | 1996 | 2 |
| HDI | `hdi` | Human Development Index | 0-1 | UNDP | 1990 | 1 |
| Tertiary enrollment | `tertiary_enroll` | Tertiary education enrollment | % gross | UNESCO UIS | 1970 | 2 |
| Education expenditure | `education_gdp` | Public education spending | % GDP | UNESCO UIS | 1970 | 3 |
| Doctoral graduates | `doctoral_grads` | PhD graduates | per million | UNESCO UIS | 1970 | 3 |
| Internet users | `internet_users` | Individuals using the internet | % population | World Bank | 1990 | 1 |
| Gini coefficient | `gini` | Income inequality (0=perfect eq) | 0-100 | World Bank/UN | 1985 | 3 |
| IP royalties | `ip_receipts` | IP royalty and license receipts | USD | World Bank | 1970 | 3 |
| Urban population | `urban_pop` | Urban population | % total | World Bank | 1960 | 3 |
| Manufacturing exports | `mfg_exports` | Manufactured goods exports | USD | UN Comtrade | 1970 | 3 |

### Tiers explicados

**Tier 1 (Esenciales)**: Indicadores con alta cobertura (>90% países, >80% años)
- Usar para correlación principal con producción bibliométrica
-GDP per capita, R&D intensity, Researchers, Articles, HDI, Internet users

**Tier 2 (Alto valor)**: Indicadores con buena cobertura (>75% países)
- Usar para análisis secundario y segmentación
- GERD, Patents, Tertiary enrollment, High-tech exports

**Tier 3 (Suplementarios)**: Indicadores con cobertura moderada (>50% países)
- Usar con cautela, documentar missing data
- Gini, IP royalties, Doctoral graduates, Manufacturing exports

---

## Proyecciones: projections/

### gdp_forecast.csv

Proyecciones de GDP per capita PPP del IMF World Economic Outlook.

| Columna | Descripción |
|---------|-------------|
| `iso3` | Código ISO 3 letras |
| `year` | Año (2025–2050) |
| `value` | GDP per capita PPP proyectado (USD) |

### population_forecast.csv

Proyecciones de población de UN DESA Population Prospects.

| Columna | Descripción |
|---------|-------------|
| `iso3` | Código ISO 3 letras |
| `year` | Año (2025–2050) |
| `value` | Población proyectada |

---

## Fuentes de datos detalladas

### World Bank WDI
- **API**: `https://api.worldbank.org/v2/`
- **Paquete R**: `wbstats::wb_data()`
- **Indicadores**: GDP, growth, internet, urban, Gini, high-tech exports
- **Cobertura**: Todos países, 1960–2024
- **Frecuencia**: Anual (publicados en Abril)
- **URL**: https://data.worldbank.org

### UNESCO UIS
- **API**: `http://api.uis.unesco.org/`
- **Indicadores**: R&D, researchers, education, articles
- **Cobertura**: Todos países, 1970–2023
- **Notas**: Datos heterocedásticos antes de 1990
- **URL**: http://data.uis.unesco.org

### SCImago Journal & Country Rank
- **URL**: https://www.scimagojr.com/
- **Indicadores**: citable_docs, sjr_rank, citations
- **Cobertura**: 1999–2023
- **Formato**: CSV descarga manual (requiere navegador)
- **Nota**: No tiene API pública

### WIPO Statistics
- **URL**: https://stats.wipo.int/
- **API**: `https://www.wipo.int/ipstats/rest/`
- **Indicadores**: patents_resident, patents_pct
- **Cobertura**: 1980–2023
- **URL API docs**: https://www.wipo.int/ipstats/en/help/

### UNDP Human Development Report
- **URL**: https://hdr.undp.org/
- **Indicadores**: HDI y componentes
- **Cobertura**: 1990–2022
- **Formato**: CSV descarga anual
- **URL datos**: https://hdr.undp.org/data/indicators

### IMF World Economic Outlook
- **URL**: https://www.imf.org/en/Publications/WEO/
- **Indicadores**: Proyecciones GDP, inflación, comercio
- **Cobertura**: 2025–2030 (publicación más reciente)
- **Frecuencia**: Semestral (Abril, Octubre)

### UN DESA Population Prospects
- **URL**: https://population.un.org/
- **Indicadores**: Proyecciones población por edad, sexo, país
- **Cobertura**: 2025–2050
- **Frecuencia**: Bianual

---

## Procedimiento de actualización manual

### Script: update_indicators.R

```r
# ============================================================================
# data/countries/update_indicators.R
# Actualización manual de indicadores de países
# Frecuencia: Anual (Q1 para datos del año anterior disponible)
# ============================================================================

#' Actualizar todos los indicadores de países
#'
#' Este script descarga datos de múltiples fuentes y genera los archivos CSV
#' en formato largo. Ejecutar manualmente una vez al año.
#'
#' @param output_dir Directorio base (default: data/countries)
#' @param dry_run Si TRUE, solo muestra qué se descargaría
#' @return Invisiblemente TRUE si tiene éxito
#' @export
update_country_indicators <- function(output_dir = "data/countries",
                                       dry_run = FALSE) {
  
  cat("============================================================\n")
  cat("Actualizando indicadores de países\n")
  cat("Fecha: ", Sys.Date(), "\n")
  cat("============================================================\n\n")
  
  # 1. World Bank WDI ----
  cat("[1/6] World Bank WDI...\n")
  wdi_indicators <- list(
    gdp_pc_ppp      = "NY.GDP.PCAP.PP.CD",
    gdp_growth      = "NY.GDP.MKTP.KD.ZG",
    internet_users   = "IT.NET.USER.ZS",
    urban_pop        = "SP.URB.TOTL.IN.ZS",
    high_tech_exports = "TX.VAL.TECH.CD"
  )
  # fetch_wdi(wdi_indicators, output_dir, dry_run)
  
  # 2. UNESCO UIS ----
  cat("[2/6] UNESCO UIS...\n")
  unesco_indicators <- list(
    rd_gdp           = list(code = "RD_GERD_GDP", edu = FALSE),
    gerd             = list(code = "RD_GERD_TOTL", edu = FALSE),
    rd_business      = list(code = "RD_GERD_BV", edu = FALSE),
    researchers_pm    = list(code = "RP_RSRS", edu = FALSE),
    rd_personnel_pm  = list(code = "RP_RSSE", edu = FALSE),
    articles         = list(code = "BS_SCI_SCIE", edu = FALSE),
    tertiary_enroll  = list(code = "SE_TER_ENRR", edu = TRUE),
    education_gdp    = list(code = "SE_XPD_TOTL_GD_ZS", edu = TRUE),
    doctoral_grads   = list(code = "SE_SCU_GRAD", edu = TRUE)
  )
  # fetch_unesco(unesco_indicators, output_dir, dry_run)
  
  # 3. WIPO Patents ----
  cat("[3/6] WIPO Patents...\n")
  wipo_indicators <- list(
    patents_res = "patents_resident",
    patents_pct = "pct_applications"
  )
  # fetch_wipo(wipo_indicators, output_dir, dry_run)
  
  # 4. SCImago ----
  cat("[4/6] SCImago (requiere descarga manual)...\n")
  cat("   Por favor descarga desde: https://www.scimagojr.com/countryrank.php\n")
  cat("   Guarda como: countries_indicators/innovation/scimago_raw.csv\n")
  # process_scimago(output_dir, dry_run)
  
  # 5. UNDP HDI ----
  cat("[5/6] UNDP HDI...\n")
  # fetch_hdi(output_dir, dry_run)
  
  # 6. IMF + UN Population ----
  cat("[6/6] IMF projections + UN Population...\n")
  # fetch_imf_projections(output_dir, dry_run)
  
  # Generar release notes
  if (!dry_run) {
    generate_changelog(output_dir)
  }
  
  cat("\n============================================================\n")
  cat("Actualización completa\n")
  cat("============================================================\n")
  
  invisible(TRUE)
}

# Funciones auxiliares (implementar según APIs) ----

#' Descargar indicadores del World Bank
fetch_wdi <- function(indicators, output_dir, dry_run) {
  # install.packages("wbstats")
  # library(wbstats)
  # 
  # data <- wb_data(indicator = unlist(indicators), start_date = 1970, end_date = 2024)
  # # Pivot to long format and save
}

#' Descargar indicadores de UNESCO UIS
fetch_unesco <- function(indicators, output_dir, dry_run) {
  # UNESCO tiene API REST:
  # base_url <- "http://api.uis.unesco.org/ Indicator=CODE&Country=ALL&start=1970&end=2023"
}

#' Descargar patentes de WIPO
fetch_wipo <- function(indicators, output_dir, dry_run) {
  # WIPO API: https://www.wipo.int/ipstats/rest/
}

#' Procesar descarga manual de SCImago
process_scimago <- function(output_dir, dry_run) {
  # Requiere descarga manual desde navegador
  # scimago <- read.csv("scimago_raw.csv")
  # # Mapear a iso3 y pivotar
}

#' Descargar HDI de UNDP
fetch_hdi <- function(output_dir, dry_run) {
  # HDR tiene CSV en: https://hdr.undp.org/data/indicators/
}

#' Generar changelog de la actualización
generate_changelog <- function(output_dir) {
  changelog_file <- file.path(output_dir, "CHANGELOG.md")
  entry <- sprintf(
    "## %s\n\n- Actualizado: %s\n- Fuentes: World Bank, UNESCO, WIPO, SCImago, UNDP\n\n",
    Sys.Date(), Sys.time()
  )
  cat(entry, file = changelog_file, append = TRUE)
}
```

### Frecuencia de actualización

| Indicador | Frecuencia | Momento | Método |
|-----------|------------|---------|--------|
| World Bank WDI | Anual | Abril | Automático (wbstats) |
| UNESCO UIS | Anual | Junio | API REST |
| WIPO | Anual | Abril | API REST |
| SCImago | Anual | Septiembre | Manual (navegador) |
| UNDP HDI | Anual | Octubre | CSV descarga |
| IMF WEO | Semestral | Abril, Octubre | Automático |
| UN Population | Bianual | Enero, Julio | Automático |

---

## Integración con M3

### Paso 1: Merge con datos de M0

```r
# Cargar referencia de países
countries_ref <- read_csv("data/countries/countries_reference.csv")

# Obtener datos de países de M0 (ya normalizados)
m0_countries <- m0_get(m0_result, "countries")

# Enriquecer con coordenadas para mapas
m0_countries_geo <- m0_countries %>%
  left_join(countries_ref %>% select(iso3, latitude, longitude, continent, income_group),
            by = c("country_iso3" = "iso3"))

# Merge con indicadores del último año
latest_year <- 2023
indicators_latest <- load_latest_indicators("data/countries", latest_year)

m0_enriched <- m0_countries_geo %>%
  left_join(indicators_latest, by = c("country_iso3" = "iso3"))
```

### Paso 2: Cargar indicadores (helper)

```r
#' Cargar indicadores del último año disponible
#'
#' @param base_dir Directorio data/countries
#' @param year Año específico (NULL = más reciente)
#' @return DataFrame ancho con iso3 y todos los indicadores
load_latest_indicators <- function(base_dir, year = NULL) {
  
  indicators_dir <- file.path(base_dir, "countries_indicators")
  
  # Lista de archivos de indicadores (excluyendo projections)
  indicator_files <- list.files(indicators_dir, recursive = TRUE,
                                pattern = "\\.csv$", full.names = TRUE)
  indicator_files <- indicator_files[!grepl("projections", indicator_files)]
  
  # Leer y combinar
  all_data <- purrr::map_dfr(indicator_files, function(f) {
    var_name <- basename(f) %>% tools::file_path_sans_ext()
    read_csv(f) %>%
      mutate(variable := !!var_name)
  })
  
  # Pivot a ancho
  wide <- all_data %>%
    select(iso3, year, variable, value) %>%
    pivot_wider(names_from = variable, values_from = value)
  
  if (is.null(year)) {
    # Tomar último año disponible por indicador
    wide <- wide %>%
      group_by(iso3) %>%
      summarise(across(-year, ~ last(.x[!is.na(.x)])),
                .groups = "drop")
  } else {
    wide <- wide %>% filter(year == !!year)
  }
  
  wide
}
```

### Paso 3: Análisis de correlación

```r
# Scatter: Productividad bibliométrica vs riqueza económica
m0_enriched %>%
  filter(!is.na(n_articles), !is.na(gdp_pc_ppp)) %>%
  ggplot(aes(x = log(gdp_pc_ppp), y = log(n_articles))) +
  geom_point(aes(size = total_citations, color = continent), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Publicaciones vs GDP per cápita",
    x = "Log(GDP per cápita PPP, USD)",
    y = "Log(Número de publicaciones)"
  ) +
  theme_minimal()

# Por continente
m0_enriched %>%
  group_by(continent) %>%
  filter(n() >= 5) %>%
  ggplot(aes(x = rd_gdp, y = log(articles))) +
  geom_point(aes(color = continent)) +
  geom_smooth(method = "lm") +
  facet_wrap(~continent, scales = "free")
```

### Paso 4: Mapas coropléticos

```r
library(rnaturalearth)
library(sf)

# Obtener geometrías
world_sf <- ne_countries(scale = "medium", returnclass = "sf")

# Unir con datos M3 enriquecidos
world_merged <- world_sf %>%
  left_join(m0_enriched, by = c("adm0_a3" = "country_iso3"))

# Mapa de R&D intensity
ggplot(world_merged) +
  geom_sf(aes(fill = rd_gdp)) +
  scale_fill_viridis_c(name = "R&D (% GDP)",
                        na.value = "grey90") +
  labs(title = "Intensidad de I+D por país",
       subtitle = "Datos más recientes disponibles") +
  theme_minimal()

# Mapa de productividad bibliométrica
ggplot(world_merged) +
  geom_sf(aes(fill = log(n_articles))) +
  scale_fill_viridis_c(name = "Log(Publicaciones)",
                        na.value = "grey90") +
  labs(title = "Producción científica por país",
       subtitle = "Log de publicaciones en el período del estudio") +
  theme_minimal()
```

### Paso 5: Time-series para países específicos

```r
# Cargar series temporales de un indicador
rd_series <- read_csv("data/countries/countries_indicators/innovation/rd_gdp.csv")

# Plot de paísesseleccionados
selected_countries <- c("USA", "CHN", "DEU", "JPN", "KOR", "BRA", "IND")

rd_series %>%
  filter(iso3 %in% selected_countries, year >= 1996) %>%
  left_join(countries_ref %>% select(iso3, country_name), by = "iso3") %>%
  ggplot(aes(x = year, y = value, color = country_name)) +
  geom_line() +
  geom_point() +
  labs(title = "Intensidad de R&D (1980-2023)",
       x = "Año", y = "R&D (% del PIB)",
       color = "País") +
  theme_minimal()
```

---

## Validación y calidad de datos

### Checks de calidad

```r
#' Validar indicador
#'
#' @param df DataFrame largo (iso3, year, value)
#' @param indicator Nombre del indicador
#' @return Lista con checks de calidad
validate_indicator <- function(df, indicator) {
  
  checks <- list()
  
  # 1. Completitud: % de países con datos por año
  completeness <- df %>%
    group_by(year) %>%
    summarise(pct_countries = sum(!is.na(value)) / n() * 100, .groups = "drop")
  checks$completeness <- completeness
  
  # 2. Outliers: cambios >50% YoY
  outliers <- df %>%
    arrange(iso3, year) %>%
    group_by(iso3) %>%
    mutate(yoy_change = abs(value - lag(value)) / lag(value) * 100) %>%
    filter(yoy_change > 50) %>%
    select(iso3, year, value, yoy_change)
  checks$outliers <- outliers
  
  # 3. Rango válido
  valid_range <- df %>%
    summarise(
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      n_missing = sum(is.na(value)),
      n_total = n(),
      pct_missing = n_missing / n_total * 100
    )
  checks$valid_range <- valid_range
  
  checks
}

#' Validar todos los indicadores
#'
#' @param base_dir Directorio data/countries
#' @return DataFrame con resumen de quality checks
validate_all_indicators <- function(base_dir = "data/countries") {
  
  indicators_dir <- file.path(base_dir, "countries_indicators")
  
  indicator_files <- list.files(indicators_dir, recursive = TRUE,
                                pattern = "\\.csv$", full.names = TRUE)
  
  results <- map_dfr(indicator_files, function(f) {
    var_name <- basename(f) %>% tools::file_path_sans_ext()
    df <- read_csv(f)
    
    summary <- df %>%
      summarise(
        indicator = var_name,
        n_countries = n_distinct(iso3),
        year_min = min(year, na.rm = TRUE),
        year_max = max(year, na.rm = TRUE),
        n_obs = n(),
        pct_missing = sum(is.na(value)) / n() * 100,
        value_min = min(value, na.rm = TRUE),
        value_max = max(value, na.rm = TRUE)
      )
    
    summary
  })
  
  results %>%
    arrange(desc(pct_missing))
}
```

### Missing data handling

| Situación | Estrategia |
|-----------|------------|
| <5% missing post-1990 | Interpolar linealmente |
| 5-20% missing | Documentar, usar con cautela |
| >20% missing | No usar para correlación, solo descriptivo |
| Pre-1990 para UNESCO | Marcar como "low coverage era" |
| Países sin datos | Mergear con flag `has_data = FALSE` |

---

## Formato CSV

### Especificaciones técnicas

| Aspecto | Especificación |
|---------|----------------|
| Encoding | UTF-8 sin BOM |
| Separador | Coma (`,`) |
| Decimal | Punto (`.`) |
| Nombres archivo | Minúsculas, guiones bajos, sin espacios |
| Nombres columnas | Minúsculas, guiones bajos, sin tildes |
| Headers | Siempre primera fila |
| Filas vacías | Eliminadas |

### countries_reference.csv ejemplo

```csv
iso2,iso3,country_name,continent,region,income_group,latitude,longitude,population,area_km2
CO,COL,Colombia,Americas,South America,Upper middle income,4.5709,-74.2973,51268632,1141748
US,USA,United States,Americas,Northern America,High income,37.0902,-95.7129,331002651,9833517
CN,CHN,China,Asia,Eastern Asia,Upper middle income,35.8617,104.1954,1402112000,9596961
DE,DEU,Germany,Europe,Western Europe,High income,51.1657,10.4515,83783942,357022
JP,JPN,Japan,Asia,Eastern Asia,High income,36.2048,138.2529,126476461,377975
```

### countries_indicators/econ/gdp_pc_ppp.csv ejemplo

```csv
iso3,year,value
USA,1960,15733.12
USA,1961,15901.45
USA,1962,16165.78
USA,1963,16424.87
COL,1960,4120.33
COL,1961,4198.67
DEU,1960,11298.45
DEU,1961,11583.23
```

---

## API Status (Updated 2026-03-29)

| Source | Status | Notes |
|--------|--------|-------|
| **World Bank WDI** | ✅ WORKING | Primary source, all indicators accessible |
| **UNESCO UIS** | ❌ API 404 | Changed API endpoint, requires manual download |
| **UNDP HDI** | ⚠️ WORKAROUND | Excel download URL confirmed working |
| **WIPO Patents** | ⚠️ UNKNOWN | API format changed, needs verification |
| **SCImago** | ❌ No API | Requires manual browser download |
| **IMF WEO** | ❌ No API | Manual download required |
| **UN Population** | ⚠️ PARTIAL | File listing accessible, CSV path needs verification |

### Workarounds Implemented

#### UNDP HDI
- **New URL found**: `https://hdr.undp.org/sites/default/files/2025_HDR/HDR25_Statistical_Annex_HDI_Table.xlsx`
- **Script**: `update_indicators.R` now includes `fetch_hdi_undp()` function
- **Coverage**: 1990-2024, 193 countries

#### UN Population
- **File listing**: `https://population.un.org/wpp/Download/Files/` accessible
- **CSV path**: Needs verification (try WPP2024.csv pattern)
- **Script**: `update_indicators.R` includes `fetch_un_population()` with path testing

#### UNESCO UIS
- **Manual download**: http://data.uis.unesco.org (requires browser)
- **Affected indicators**: gerd, rd_business, researchers_pm, rd_personnel_pm, doctoral_grads
- **Placeholder files**: Created with instructions for manual population

---

## Roadmap de implementación

### Fase 1: Referencia estática ✅
- [x] Estructura de directorios
- [x] countries_reference.csv con códigos ISO, coordenadas, income group
- [x] 296 países/territorios incluidos
- [x] 41 países con población y área (datos clave)

### Fase 2: Indicadores económicos base ✅
- [x] GDP per capita PPP (World Bank) - 203 países, 6832 obs
- [x] GDP growth (World Bank) - 214 países, 11214 obs
- [x] Validación completada

### Fase 3: Indicadores de innovación
- [x] R&D % GDP (World Bank) - 153 países, 2366 obs
- [x] Scientific articles (World Bank) - 197 países, 5320 obs
- [ ] GERD total (UNESCO) - Placeholder + manual download
- [ ] Researchers per million (UNESCO) - Placeholder + manual download
- [ ] Citable docs (SCImago) - Placeholder + manual download
- [ ] SJR rank (SCImago) - Placeholder + manual download
- [ ] Patents resident (WIPO) - Placeholder + needs verification
- [ ] Patents PCT (WIPO) - Placeholder + needs verification

### Fase 4: Indicadores sociales
- [x] Tertiary enrollment (World Bank) - 203 países, 4939 obs
- [x] Education expenditure (World Bank) - 203 países, 5127 obs
- [⚠️] HDI (UNDP) - Workaround: Excel download URL confirmed
- [ ] Doctoral graduates (UNESCO) - Placeholder + manual download
- [x] Internet users (World Bank) - 213 países, 6078 obs
- [x] Gini coefficient (World Bank) - 171 países, 2402 obs

### Fase 5: Tecnología y comercio ✅
- [x] High-tech exports (World Bank) - 186 países, 2683 obs
- [x] IP royalties (World Bank) - 171 países, 3851 obs
- [x] Urban population (World Bank) - 217 países, 14105 obs
- [ ] Manufacturing exports (UN Comtrade) - pendiente

### Fase 6: Proyecciones
- [ ] IMF GDP forecasts (2025-2050) - Manual download required
- [⚠️] UN Population forecasts (2025-2050) - Workaround: file listing accessible

---

## Notas finales

- **Sin agregados regionales**: Solo países individuales, no "OECD", "EU27", etc.
- **Series temporales completas**: 1970–2024 (o más reciente disponible)
- **Proyecciones incluidas**: 2025–2050 donde estén disponibles
- **Formato CSV puro**: Sin Excel, sin archivos binarios
- **Actualización manual**: Script documentado para ejecución anual
- **Merge key**: Siempre `iso3` (3 letras) para consistencia
- **Placeholder files**: Indicadores sin API tienen archivos con estructura y comentarios para población manual

---

## Changelog

### v0.2.0 (2026-03-29)
- Actualizado status de APIs (UNESCO 404, UNDP workaround encontrado)
- Creado update_indicators.R con funciones actualizadas
- Creado 11 archivos placeholder para indicadores pendientes
- Documentado workarounds para UNDP HDI y UN Population
- Creada función fetch_hdi_undp() con descarga Excel
- Creada función fetch_un_population() con testing de paths

### v0.1.0 (2026-03-28)
- Creado estructura de directorios
- Definido catálogo de 21 indicadores
- Documentado fuentes y procedimientos de actualización
- Implementado integración con M3

---

## Referencias

- World Bank (2024). World Development Indicators. https://data.worldbank.org
- UNESCO UIS (2024). UNESCO Institute for Statistics. http://data.uis.unesco.org (API 404)
- WIPO (2024). World Intellectual Property Organization Statistics. https://stats.wipo.int
- SCImago (2024). SCImago Journal & Country Rank. https://www.scimagojr.com
- UNDP (2024). Human Development Report. https://hdr.undp.org
- IMF (2024). World Economic Outlook. https://www.imf.org/en/Publications/WEO
- UN DESA (2024). World Population Prospects. https://population.un.org
