# M1_Main_Information - Análisis Profundo de Arquitectura

## Estado Actual

### Archivos en M1
| Archivo | Líneas | Propósito |
|---------|--------|-----------|
| `M1_Main_Information.r` | 1003 | Clase principal + funciones métricas |
| `_helpers.r` | 286 | Funciones utilitarias (limpieza, validación, JSON) |
| `_plots.r` | 1045 | Todas las funciones de plotting |
| `_report.r` | ? | Reportes |
| `__m1_bubble_paper_tc_chart.r` | 298 | Gráficos de burbujas |
| `__m1_keywords_clouds_charts.r` | ? | Nubes de keywords |
| `__m1_scp_mcp_stacked_bar_chart.r` | ? | Gráficos SCP/MCP |
| `__m1_bubble_countries.r` | ? | Mapas de países |
| `__m1_bubble_countries_full.r` | ? | Mapas completos |

---

## DIAGNÓSTICO DE CÓDIGO

### 🔴 PROBLEMAS CRÍTICOS

#### 1. **Monolithic Main File (1003 líneas)**
El archivo `M1_Main_Information.r` hace TODO:
- Análisis bibliométrico
- Extracción de datos
- Validación
- Llamadas a plotting
- Generación de métricas
- Funciones de helpers inline

**Problema**: Violación directa del **Single Responsibility Principle (SOLID)**. Una clase/archivo no debería hacer 10 cosas diferentes.

```r
# EJEMPLO DE LO QUE NO DEBERÍA ESTAR EN EL MAIN:
fn_m1_main_information <- function(bib_data) { ... }  # Análisis principal
fn_m1_mtrc1_articles_types_pie <- function(...) { ... }  # Métrica 1
fn_m1_mtrc3_analyze_and_plot_most_prod_authors <- function(...) { ... }  # Métrica 3
fn_m1_mtrc4_analyze_and_plot_most_cited_papers <- function(...) { ... }  # Métrica 4
fn_m1_mtrc5_analyze_and_plot_most_prod_countries <- function(...) { ... }  # Métrica 5
fn_m1_mtrc5_analyze_and_plot_tc_per_country <- function(...) { ... }  # Más métricas
fn_m1_mtrc5_countries_tp_vs_tc_plot_bubble_chart <- function(...) { ... }
analyze_bradford_law <- function(...) { ... }
```

**SOLUCIÓN**: Cada métrica debería ser un archivo/clase separada.

---

#### 2. **Hardcoded Paths everywhere**

```r
# En _plots.r línea 29
save_plot <- function(..., output_dir = "results/M1_Main_Information/figures")

# En _plots.r línea 879
filename = paste0("results/M1_Main_Information/figures/", file_name, ".png")

# En helpers.r línea 27
save_json <- function(data, filename, path = "results/M1_Main_Information/jsons/")
```

**Problema**: 
- No reutilizable entre módulos
- Cambiar estructura = romper TODO
- No testeable

**SOLUCIÓN**: Usar configuración inyectada o parámetros.

---

#### 3. **Nombres Inconsistentes**

| Función | Convencioń |
|---------|------------|
| `fn_m1_main_information` | snake_case con prefijo |
| `ieee_theme` | camelCase |
| `save_plot` | snake_case |
| `THEME_COLORS` | SCREAMING_SNAKE |
| `IEEE_MAYORGA_THEME_COLORS` | SCREAMING_SNAKE |

**También en archivos**:
- `_helpers.r` (underscore)
- `__m1_report.r` (double underscore)
- `_plots.r` (single underscore)

**SOLUCIÓN**: Estandarizar. Ejemplo: `kebab-case` para archivos, `snake_case` para funciones.

---

#### 4. **Duplicación de Funciones de Validación**

```r
# En _helpers.r (línea 232)
validate_data <- function(data, required_columns) { ... }

# En _plots.r (línea 424-426)
if (!all(c(value_col, entity_col) %in% colnames(data))) {
  stop("[ERROR] Missing required columns...")
}

# En __m1_bubble_paper_tc_chart.r (línea 4)
validate_and_prepare_data <- function(data) { ... }
```

**SOLUCIÓN**: Un solo módulo de validación.

---

#### 5. **Mensajes Inconsistentes**

```r
message("[INFO] ...")
message("[DEBUG] ...")
message("[ERROR] ...")
warning("[WARNING] ...")
stop("[ERROR] ...")
```

**Problema**: No hay estándar. Mezcla de formatos.

**SOLUCIÓN**: Crear una función helper `log_message(level, msg)`.

---

#### 6. **Librerías Cargadas en Cualquier Lado**

```r
# Al inicio de M1_Main_Information.r (líneas 6-20)
library(ggwordcloud)
library(ggplot2)
library(jsonlite)
library(bibliometrix)
# ... 10+ librerías

# En _plots.r (líneas 6-18)
library(ggplot2)
library(dplyr)
library(forcats)
# ... otra vez las mismas librerías

# En _helpers.r (línea 11)
library(zoo)  # Carga aquí

# En _helpers.r (línea 78)
library(lomb)  # Y aquí
```

**Problema**:
- Carga redundante
- No se sabe qué necesita qué
- Namespace pollution

**SOLUCIÓN**: Un solo punto de carga (DESCRIPTION + namespace.R).

---

#### 7. **Magical Numbers**

```r
# En M1_Main_Information.r
dpi = 600                    # Línea 140
dpi = 600                    # Línea 184
dpi = 900                    # Línea 147
window_size = c(1, 5, 10)   # Línea 54
top_n = 15                   # Línea 435
threshold = 0.8              # Líneas 132, 304
```

**SOLUCIÓN**: Constantes en archivo de configuración.

---

#### 8. **Comentarios vs Documentación**

```r
# Esto NO es documentación:
# ---------------------------------------------------------------------------- #
# Function: Clean Whitespace
# ---------------------------------------------------------------------------- #
clean_whitespace <- function(df) { ... }
```

**SOLUCIÓN**: Usar roxygen2:
```r
#' @title Clean Whitespace
#' @description Removes extra whitespace from data frame columns
#' @param df A data frame
#' @return Data frame with cleaned columns
clean_whitespace <- function(df) { ... }
```

---

### 🟡 PROBLEMAS MEDIOS

#### 9. **Mix de Estilos: Funcional vs Orientado a Objetos**

```r
# Estilo funcional (M1)
fn_m1_main_information <- function(bib_data) { ... }
save_plot <- function(...) { ... }

# Estilo R6 (M2)
M2_Annual_Production <- setRefClass(
  "M2_Annual_Production",
  fields = list(...),
  methods = list(...)
)
```

**SOLUCIÓN**: Elegir UNO. Recomendación: **R6** para modularidad, o **S3** para simplicidad.

---

#### 10. **Error Handling Inconsistente**

```r
# tryCatch en algunos lugares
tryCatch({
  jsonlite::write_json(...)
}, error = function(e) {
  stop("[ERROR] Failed to save JSON: ", e$message)
})

# Stop directo en otros
if (length(missing_cols) > 0) {
  stop("[ERROR] Missing required columns...")
}

# Ninguno en otros tantos
data$Articles <- as.numeric(data$Articles)  # Silently converts NA
```

**SOLUCIÓN**: Wrapper de error handling consistente.

---

#### 11. **Funciones con Efectos Secundarios**

```r
# Esta función GUARDA, no solo genera
save_plot <- function(plot, filename_prefix, ...) {
  # ... genera el plot ...
  ggsave(...)  # Side effect: archivo guardado
  message("[INFO] ...")  # Side effect: log
}
```

**SOLUCIÓN**: Separar generación de guardado:
```r
generate_plot(...)  # Retorna el plot
save_plot(plot, ...)  # Lo guarda
```

---

#### 12. **Exposición de Variables Globales**

```r
# En M1_Main_Information.r
source('../../src/Config/PlotThemes.r')  # Define THEME_COLORS global

# Después se usa:
fill = THEME_COLORS$Main[1]
```

**Problema**: Dependencia implícita global. Difícil de testear.

**SOLUCIÓN**: Inyectar dependencias.

---

## ANÁLISIS DE ARQUITECTURA ACTUAL

### Estructura Actual
```
src/
├── M1_Main_Information/
│   ├── M1_Main_Information.r      # TODO (1003 líneas)
│   ├── _helpers.r                  # Utilidades mezcladas
│   ├── _plots.r                    # Todo el plotting
│   ├── _report.r
│   ├── __m1_*.r                    # Scripts adicionales
│   └── _plots/
├── M2_Annual_Production/
│   ├── M2_Annual_Production.r      # R6 class
│   ├── _helpers.r                  # Duplicado
│   ├── m2_*.r                      # Submódulos
│   └── plots/
└── Config/
    └── PlotThemes.r                # Temas
```

### Problema de Dependencias
```
M1_Main_Information.r
    ├── carga librerías (6-20)
    ├── source _helpers.r
    │   └── library(zoo)  ⚠️
    ├── source _plots.r
    │   └── library(ggplot2) ⚠️ (otra vez)
    ├── source __m1_*.r
    └── source Config/PlotThemes.r
        └── THEME_COLORS (global)
```

---

## ARQUITECTURA IDEAL - PROPUESTA

### Principios SOLID Aplicados

| Principio | Aplicación |
|----------|------------|
| **S**ingle Responsibility | Cada clase/archivo = UNA responsabilidad |
| **O**pen/Closed | Abierto a extensión, cerrado a modificación |
| **L**iskov Substitution | Interfaces consistentes |
| **I**nterface Segregation | Múltiplos interfaces pequeñas |
| **D**ependency Inversion | Depender de abstracciones, no concreciones |

---

### Estructura Propuesta

```
R/
├── DESCRIPTION                      # Dependencias del paquete
├── NAMESPACE                       # Exports
├── README.Rmd                      # Documentación
│
├── R/
│   ├── 00_pkg_utils.R              # Carga de librerías (UNA SOLA VEZ)
│   │
│   ├── config/
│   │   ├── config.R                # Constantes globales
│   │   ├── paths.R                 # Manejo de paths
│   │   └── logging.R               # Sistema de logs
│   │
│   ├── utils/
│   │   ├── validation.R             # Validación de datos
│   │   ├── io.R                    # Lectura/escritura
│   │   ├── plotting.R              # Temas y utilities de plots
│   │   └── helpers.R               # Utilidades misceláneas
│   │
│   ├── themes/
│   │   ├── ieee.R                  # Tema IEEE
│   │   ├── colors.R                # Paletas de colores
│   │   └── plot_utils.R            # Funciones de plot reusable
│   │
│   ├── m1_main/
│   │   ├── m1_main.R               # Clase/funciones principales
│   │   ├── m1_metrics.R             # Definiciones de métricas
│   │   └── m1_runner.R             # Orquestación
│   │
│   ├── m1_metrics/
│   │   ├── m1_m0_overview.R        # Métrica 0: Overview
│   │   ├── m1_m1_doc_types.R      # Métrica 1: Tipos de documento
│   │   ├── m1_m2_authors.R         # Métrica 2: Autores
│   │   ├── m1_m3_citations.R       # Métrica 3: Citas
│   │   ├── m1_m4_countries.R       # Métrica 4: Países
│   │   ├── m1_m5_sources.R         # Métrica 5: Fuentes
│   │   ├── m1_m6_keywords.R        # Métrica 6: Keywords
│   │   └── m1_m7_bradford.R       # Métrica 7: Bradford
│   │
│   ├── m2_annual/
│   │   ├── m2_annual.R             # Clase principal
│   │   ├── m2_eda.R                # EDA submodule
│   │   ├── m2_regression.R         # Regression submodule
│   │   └── m2_harmonics.R          # Harmonics submodule
│   │
│   └── ...
│
├── inst/
│   └── templates/                  # Templates para reportes
│
├── tests/
│   ├── testthat/
│   │   ├── test_validation.R
│   │   ├── test_m1_metrics.R
│   │   └── test_plotting.R
│   └── testthat.R
│
└── vignettes/
    └── introduction.Rmd
```

---

### Detalle de Arquitectura por Componente

#### 1. **Capa de Configuración (config/)**
```r
# config.R
get_config <- function() {
  list(
    dpi = getOption("biblio.dpi", default = 600),
    output_dir = getOption("biblio.output_dir", default = "results"),
    theme = getOption("biblio.theme", default = "ieee"),
    log_level = getOption("biblio.log_level", default = "INFO")
  )
}

# paths.R
get_output_path <- function(module, type = c("figures", "jsons", "reports")) {
  config <- get_config()
  file.path(config$output_dir, module, type)
}
```

#### 2. **Capa de Validación (utils/validation.R)**
```r
# Validate input schema
validate_dataframe <- function(df, schema) {
  errors <- c()
  
  for (col in names(schema$required)) {
    if (!col %in% colnames(df)) {
      errors <- c(errors, sprintf("Missing required column: %s", col))
    } else if (!inherits(df[[col]], schema$required[[col]])) {
      errors <- c(errors, sprintf("Column %s must be of type %s", col, schema$required[[col]]))
    }
  }
  
  if (length(errors) > 0) {
    stop(paste(errors, collapse = "\n"))
  }
  
  invisible(TRUE)
}
```

#### 3. **Capa de Logging (config/logging.R)**
```r
log_debug <- function(msg) log_message("DEBUG", msg)
log_info  <- function(msg) log_message("INFO", msg)
log_warn  <- function(msg) log_message("WARN", msg)
log_error <- function(msg) log_message("ERROR", msg)

log_message <- function(level, msg) {
  config <- get_config()
  if (should_log(level, config$log_level)) {
    cat(sprintf("[%s] [%s] %s\n", Sys.time(), level, msg))
  }
}
```

#### 4. **Patrón de Métricas (m1_metrics/)**
```r
# INTERFAZ CONSISTENTE para todas las métricas
Metric <- R6Class("Metric",
  public = list(
    name = NULL,
    description = NULL,
    
    initialize = function(name, description) {
      self$name <- name
      self$description <- description
    },
    
    validate = function(data) {
      # Validate inputs
    },
    
    calculate = function(data) {
      # Calculate metric
    },
    
    plot = function(result) {
      # Generate plot
    },
    
    save = function(result, output_dir) {
      # Save results
    },
    
    run = function(data, output_dir) {
      log_info(sprintf("Running metric: %s", self$name))
      self$validate(data)
      result <- self$calculate(data)
      self$plot(result)
      self$save(result, output_dir)
      invisible(result)
    }
  )
)
```

---

### Ejemplo de Implementación de Métrica

```r
# m1_metrics/m1_m1_doc_types.R

M1_M1_DocTypesMetric <- R6Class("M1_M1_DocTypesMetric",
  inherit = Metric,
  
  public = list(
    initialize = function() {
      super$initialize(
        name = "Document Types",
        description = "Distribution of document types in the corpus"
      )
    },
    
    validate = function(data) {
      required_cols <- c("DT")
      missing <- setdiff(required_cols, colnames(data))
      if (length(missing) > 0) {
        stop("Missing columns: ", paste(missing, collapse = ", "))
      }
    },
    
    calculate = function(data) {
      table(data$DT) |>
        data.frame(
          Document_Type = _,
          Count = Freq
        ) |>
        dplyr::arrange(dplyr::desc(Count))
    },
    
    plot = function(result) {
      ggplot(result, aes(x = "", y = Count, fill = Document_Type)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        labs(title = "Document Types Distribution") +
        theme_minimal() +
        theme(legend.position = "right")
    },
    
    save = function(result, output_dir) {
      path_fig <- file.path(output_dir, "figures")
      path_json <- file.path(output_dir, "jsons")
      
      dir.create(path_fig, recursive = TRUE)
      dir.create(path_json, recursive = TRUE)
      
      ggsave(file.path(path_fig, "M1_G1_DOCUMENT_TYPES_PIE.png"), 
             self$plot(result), width = 6, height = 4, dpi = 600)
      
      jsonlite::write_json(result, file.path(path_json, "document_types.json"))
    }
  )
)
```

---

## RECOMENDACIONES DE REFACTORING

### Fase 1: Extraer Helpers (Inmediato)

**Archivo**: `M1_Main_Information/_helpers.r`

Separar en:
```
utils/
├── cleaning.R         # clean_whitespace, extract_*
├── validation.R       # validate_data, preprocess_data
├── io.R              # save_json, load_data
└── metrics.R         # calculate_metrics_top_*, get_top_*
```

### Fase 2: Extraer Métricas (Corto plazo)

**De**: `M1_Main_Information.r` (1003 líneas)

**A**:
```
m1_metrics/
├── m1_m0_overview.R
├── m1_m1_doc_types.R
├── m1_m2_authors.R
├── m1_m3_citations.R
├── m1_m4_countries.R
├── m1_m5_sources.R
├── m1_m6_keywords.R
└── m1_m7_bradford.R
```

### Fase 3: Unificar Sistema de Logging (Medio plazo)

```r
# Crear logger.R
logger <- function(level, message, ...) {
  config <- get_biblio_config()
  
  prefix <- switch(level,
    DEBUG = "🔍",
    INFO  = "ℹ️ ",
    WARN  = "⚠️ ",
    ERROR = "❌"
  )
  
  msg <- sprintf(paste0(prefix, " ", message), ...)
  
  if (level == "ERROR") stop(msg) else message(msg)
}
```

### Fase 4: Inyección de Dependencias (Largo plazo)

**Antes**:
```r
save_plot <- function(plot, ...) {
  ggsave(plot, ...)  # Dependencia hardcoded
}
```

**Después**:
```r
# Constructor injection
M1_Main_Analysis <- R6Class("M1_Main_Analysis",
  fields = list(
    .saver = "function"  # Dependency injection
  ),
  
  initialize = function(saver = ggsave) {
    self$.saver <- saver
  },
  
  save_plot = function(plot, ...) {
    self$.saver(plot, ...)  # Usa lo que se inyectó
  }
)
```

---

## CHECKLIST DE CALIDAD

| Categoría | Checkpoint | Estado |
|-----------|------------|--------|
| **Archivos** | Un archivo por responsabilidad | ❌ |
| **Nombres** | Convenciones consistentes | ❌ |
| **Paths** | No hardcoded | ❌ |
| **Librerías** | Una sola carga | ❌ |
| **Errores** | Manejo consistente | ❌ |
| **Logs** | Sistema centralizado | ❌ |
| **Tests** | Cobertura básica | ❌ |
| **Docs** | Roxygen2 | ❌ |
| **Constantes** | En config | ❌ |
| **Arquitectura** | Capas definidas | ❌ |

---

## RESUMEN

### Lo Que Està Bien
- ✅ El código PRODUCE resultados (plots, análisis)
- ✅ Ideas de visualizaciones son buenas
- ✅ Tema IEEE está bien implementado
- ✅ Uso de bibliometrix es correcto

### Lo Que Hay Que Arreglar
- ❌ Arquitectura monolithic
- ❌ Código duplicado
- ❌ Paths hardcoded
- ❌ Sin tests
- ❌ Sin документация (roxygen)
- ❌ Mixing de paradigmas
- ❌ Sistema de logs inexistente

### Próximos Pasos
1. **Inmediato**: Eliminar hardcoded paths → usar configuración
2. **Corto**: Extraer helpers a archivos separados
3. **Medio**: Crear clase base para métricas
4. **Largo**: Reescribir con inyección de dependencias

---

*Documento generado para revisión de arquitectura del proyecto RBiblioSynth - Módulo M1*
