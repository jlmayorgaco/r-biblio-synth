# RBiblioSynth — Análisis de Arquitectura y Software

> Autor del análisis: Claude Sonnet 4.6
> Fecha: 2026-03-25
> Alcance: Análisis profundo de código, arquitectura, patrones, bugs, deuda técnica y roadmap de tickets.

---

## 1. Resumen Ejecutivo

RBiblioSynth es un framework R para análisis bibliométrico y automatización de revisiones sistemáticas. Tiene un núcleo funcional real (M1, M2) y varios módulos en estado de prototipo o vacíos (M3, M4). El código mezcla dos paradigmas OOP incompatibles, tiene duplicación masiva, funciones definidas múltiples veces con firmas incompatibles, y bugs que hacen que la mayoría de los módulos fallen en tiempo de ejecución.

**Estado de salud actual:**

| Dimensión | Calificación | Comentario |
|-----------|-------------|------------|
| Funcionalidad | 3/10 | Solo M1 y partes de M2 ejecutan sin errores |
| Arquitectura | 3/10 | Mezcla de OOP, sin DI, sin separación de capas |
| Mantenibilidad | 2/10 | Duplicación masiva, sin tests, paths hardcodeados |
| Calidad científica | 5/10 | Buenos modelos, pero bugs en implementación (scoping R) |
| Extensibilidad | 2/10 | Agregar un módulo requiere tocar múltiples archivos |
| Documentación | 1/10 | Sin roxygen, sin docstrings, sin README por módulo |

---

## 2. Árbol del Proyecto (Estado post Phase 0)

```
r-biblio-synth/
│
├── src/                                   ← Código fuente principal
│   ├── Config/
│   │   ├── PaletteGenerator.r             ✅ Funciona — define generate_palette_png()
│   │   └── PlotThemes.r                   ✅ Funciona — define THEME_COLORS, LABEL_MAPPING, ieee_theme
│   │
│   ├── SystematicReviewClass.r            ✅ Creado en Phase 0 — facade orquestador
│   │
│   ├── M1_Main_Information/               ✅ Funciona (monolito ~1200 líneas)
│   │   ├── M1_Main_Information.r          ⚠️  1200+ líneas — viola SRP gravemente
│   │   ├── _helpers.r                     ✅ clean_whitespace, save_json, validate_data
│   │   ├── _plots.r                       ✅ save_plot() + funciones de visualización
│   │   ├── _report.r                      ✅ Generación de reportes TXT/LaTeX
│   │   ├── __m1_report.r                  ❌ VACÍO (1 línea)
│   │   ├── __m1_bubble_countries.r        ✅ Funciones de bubble chart por país
│   │   ├── __m1_bubble_countries_full.r   ✅ Versión extendida
│   │   ├── __m1_bubble_paper_tc_chart.r   ⚠️  No verificado si se usa
│   │   ├── __m1_keywords_clouds_charts.r  ✅ Word clouds
│   │   └── __m1_scp_mcp_stacked_bar_chart.r ✅ Stacked bar charts
│   │
│   ├── M2_Annual_Production/              ✅ El módulo mejor estructurado
│   │   ├── M2_Annual_Production.r         ✅ Clase orquestadora (setRefClass)
│   │   ├── _helpers.r                     ✅ Limpiado en Phase 0 (797→324 líneas)
│   │   ├── _utils.r                       ✅ Utilidades I/O + modelos de crecimiento
│   │   ├── _performances.r                ✅ 57 métricas de rendimiento del modelo
│   │   ├── _settings.r                    ✅ ieee_theme, plot_colors, export_settings
│   │   ├── m2_m0_eda.r                    ✅ Clase EDA (setRefClass)
│   │   ├── m2_m1_regression.r             ⚠️  Funciona parcialmente — bugs BUG-09
│   │   ├── m2_m2_harmonics.r              ⚠️  Bug crítico de scoping (BUG-04)
│   │   └── plots/
│   │       ├── _plots.r                   ✅ Entry point que sourcea todo
│   │       ├── Helpers.r                  ✅ calculate_residuals, find_peaks
│   │       ├── R_IEEE_Plot.r              ✅ Clase base R6 para plots IEEE
│   │       ├── Regression_Model_Gompertz_Plot.r ✅ Plot especializado
│   │       ├── Plots_m2_m1_regression_plots.r  ✅ 12 subclases R6 de plots
│   │       └── Plots_m2_m2_harmonics_plots.r   ✅ 4 subclases R6 de plots
│   │
│   ├── M3_Authors/                        ❌ Stub — no ejecuta nada real
│   │   ├── M3_Authors.r                   ❌ Todos los métodos comentados
│   │   ├── _settings.r                    ❌ VACÍO
│   │   └── m3_m0_eda.r                    ⚠️  Clase definida pero run() vacío
│   │
│   ├── M3_Most_Prod_Authors/              ❌ Broken — falla en runtime
│   │   ├── M3_Most_Prod_Authors.r         ❌ Sources archivos vacíos, llama ensure_directory() inexistente
│   │   ├── _helpers.r                     ❌ VACÍO
│   │   ├── _settings.r                    ❌ VACÍO
│   │   ├── _utils.r                       ❌ VACÍO
│   │   └── m3_m0_eda.r                    ❌ run() llama biblioAnalysis() con df incorrecto
│   │
│   ├── M4_Countries/                      ⚠️  Un solo archivo con funciones
│   │   └── _helpers.r                     ⚠️  extract_country_data() — funciona, sin clase
│   │
│   └── 03_utils/
│       └── modules/module_m5/
│           └── module_m5_tables_utils.r   ⚠️  HUÉRFANO — no importado por nadie
│
├── examples/
│   ├── SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/
│   │   ├── main.r                         ✅ Funciona tras crear SystematicReviewClass.r
│   │   ├── main2.r                        ❌ Referencia src2/index.r que NO EXISTE
│   │   ├── data.r                         ❓ No analizado
│   │   └── temp/main2.r                   ❌ Igual que main2.r
│   ├── OPEN_ALEX_LEISMANIASIS/
│   │   └── main.r                         ✅ Funciona tras crear SystematicReviewClass.r
│   ├── POWER-FREQUENCY-ESTIMATOR-2025/    ⚠️  Solo resultados, sin código fuente
│   └── datos-mama/
│       └── datos-mama.r                   ❓ Script independiente, no analizado
│
├── data/
│   ├── countries.csv
│   └── world_bank/*.csv                   (9 datasets)
│
├── archived/                              ← Movido de _temp/ en Phase 0
│
├── DESCRIPTION                            ✅ Creado en Phase 0
├── NAMESPACE                              ✅ Creado en Phase 0
├── .Rbuildignore                          ✅ Creado en Phase 0
├── .gitignore                             ✅ Creado en Phase 0
├── README.md
├── LICENSE
├── CLAUDE.md
└── ROADMAP.md
```

---

## 3. Diagnóstico por Capa

### 3.1 Capa de Configuración (`src/Config/`)

**Estado: Funciona, pero con problemas de diseño.**

- `PlotThemes.r` llama a `generate_palette_png()` al momento de ser sourced — efecto secundario que genera un PNG en disco. Viola el principio de que un `source()` no debería tener efectos en el filesystem.
- `THEME_COLORS`, `ieee_theme`, `plot_colors`, `export_settings`, `LABEL_MAPPING` son **variables globales mutables**. Cualquier módulo puede sobreescribirlas accidentalmente. `ieee_theme` está definido tanto en `Config/PlotThemes.r` como en `M2_Annual_Production/_settings.r` — **dos definiciones distintas del mismo nombre**.
- No existe mecanismo de validación de que los colores sean hexadecimales válidos.

### 3.2 Módulo M1 — Main Information

**Estado: Funciona. Arquitectura monolítica crítica.**

- `M1_Main_Information.r` tiene ~1200 líneas que mezclan: análisis bibliométrico, transformación de datos, generación de plots, serialización JSON, y generación de reportes. **Viola SRP completamente**.
- Las funciones `fn_m1_*` son funciones libres (no métodos de clase), haciendo el módulo no testeable unitariamente sin correr todo el pipeline.
- `save_json()` en `_helpers.r` tiene path hardcodeado por defecto: `"results/M1_Main_Information/jsons/"`. Funciona solo si el CWD es el directorio del ejemplo.
- `save_plot()` en `_plots.r` tiene `output_dir = "results/M1_Main_Information/figures"` hardcodeado.
- `extrafont::font_import()` se llama en `_plots.r` al cargarse — esto tarda varios minutos la primera vez y es innecesario en cada ejecución.
- `library(ggplot2)`, `library(dplyr)`, etc. se importan múltiples veces (en `_helpers.r`, en `_plots.r`, en `M1_Main_Information.r`). Impacta el tiempo de carga.

### 3.3 Módulo M2 — Annual Production

**Estado: El módulo mejor estructurado. Tiene bugs subsanables.**

**Fortalezas:**
- Clase orquestadora clara (`M2_Annual_Production`) con subclases por métrica (`M2_M0_EDA`, `M2_M1_Regression`, `M2_M2_Harmonic`).
- Buena separación entre análisis y visualización (plots/ separado).
- Base class R6 para plots IEEE (`R_IEEE_Plot`) — buen uso de herencia.
- 57 métricas de performance del modelo (impresionante).

**Problemas:**
- Mezcla `setRefClass` (para las clases de análisis) con `R6Class` (para los plots). Dos paradigmas OOP en el mismo módulo.
- `perform_fft()`, `calculate_lomb_scargle()`, `calculate_wavelet_power()` están definidas tanto en `_helpers.r` como en `m2_m2_harmonics.r` con **firmas e implementaciones diferentes**. La última en ser sourced gana — comportamiento impredecible dependiendo del orden de carga.
- `m2_m2_harmonics.r` tiene un bug de scoping en R (ver BUG-04).
- `create_clean_directory()` es llamada en `m2_m1_regression.r::save_plot()` pero **no está definida en ningún archivo fuente** analizado.
- `toJSON()` usado sin prefijo `jsonlite::` — dependencia implícita de que el paquete esté attached.

### 3.4 Módulo M3 — Authors

**Estado: Roto. No ejecuta análisis real.**

- `M3_Authors.r` tiene el 100% de los métodos comentados.
- `m3_m0_eda.r` tiene `run()` con cuerpo vacío (solo un `message()`).
- `M3_Most_Prod_Authors.r` sourceta `_helpers.r`, `_utils.r`, `_settings.r` que son todos vacíos → `ensure_directory()` no estará definida → crash al crear instancia.
- `M3_M0_EDA` está definida **dos veces** con **interfaces incompatibles**:
  - En `src/M3_Authors/m3_m0_eda.r`: `new(df, author_col, year_col)`
  - En `src/M3_Most_Prod_Authors/m3_m0_eda.r`: `new(df, author_col, articles_col, fractionalized_col)`
- `M3_Most_Prod_Authors/m3_m0_eda.r::run()` llama `bibliometrix::biblioAnalysis(.self$df)` con `.self$df` que es el dataframe de autores productivos (ya procesado), no el dataframe bibliográfico crudo → error de tipo.
- El módulo tiene `print(.self$df)` en producción (debug code).
- Naming confuso: tanto `M3_Authors` como `M3_Most_Prod_Authors` cubren análisis de autores. No está claro cuál es cuál.

### 3.5 Módulo M4 — Countries

**Estado: Un archivo suelto. Sin clase, sin orquestación.**

- Solo existe `_helpers.r` con `extract_country_data()`.
- No hay clase `M4_Countries`.
- No hay integración con el pipeline.
- El código de análisis geográfico más avanzado estaba en `_temp/` (ahora en `archived/`).

### 3.6 Utilidades Huérfanas

- `src/03_utils/modules/module_m5/module_m5_tables_utils.r`: Tiene funciones bien escritas (`m5_top_cited`, `m5_top_recent`, `m5_risers_recent`) pero **no es importado por ningún módulo**. Es código muerto funcional.

---

## 4. Bugs Críticos (Clasificados por Severidad)

### CRÍTICO — Causan fallo en tiempo de ejecución

#### BUG-01: Colisión de `perform_fft` con implementaciones incompatibles
**Archivos:** `src/M2_Annual_Production/_helpers.r` y `src/M2_Annual_Production/m2_m2_harmonics.r`

La versión en `_helpers.r`:
```r
# Devuelve solo magnitude y phase
perform_fft <- function(y) {
  fft_result <- fft(y)
  list(magnitude = Mod(fft_result), phase = Arg(fft_result))
}
```
La versión en `m2_m2_harmonics.r`:
```r
# Devuelve frequencies, magnitude y phase (one-sided)
perform_fft <- function(y) {
  n <- length(y)
  fft_result <- fft(y)
  frequencies <- seq(0, 1, length.out = n / 2 + 1)
  magnitude <- Mod(fft_result[1:(n / 2 + 1)])
  phase <- Arg(fft_result[1:(n / 2 + 1)])
  list(frequencies = frequencies, magnitude = magnitude, phase = phase)
}
```
`m2_m2_harmonics.r` es sourced después → gana. `M2_M0_EDA::run()` llama `perform_fft(y)` esperando devuelva `$magnitude` — funciona. `M2_M2_Harmonic::run()` llama `perform_fft(y)` esperando `$frequencies` — funciona. Pero el comportamiento es frágil y dependiente del orden de `source()`.

#### BUG-02: `calculate_lomb_scargle` con firmas incompatibles
**Archivos:** `_helpers.r` (signature: `times, values`) vs `m2_m2_harmonics.r` (signature: `x, y`).
Internamente ambas llaman `lsp(values/y, times=times/x, ...)` — equivalentes en resultado pero el override silencioso es una bomba de tiempo.

#### BUG-03: `calculate_wavelet_power` con tipos de retorno incompatibles
**Archivos:** `_helpers.r` devuelve objeto WaveletComp crudo; `m2_m2_harmonics.r` devuelve `list(time, period, power)`.
`M2_M2_Harmonic::run()` asume la versión de harmonics (gana al ser sourced después). Si el orden de source cambia, el crash es garantizado.

#### BUG-04: Bug de scoping en R en `fit_harmonic_regression`
**Archivo:** `src/M2_Annual_Production/m2_m2_harmonics.r` línea 298

```r
fit_harmonic_regression <- function(x, y, frequencies) {
  models <- list()
  for (freq in frequencies) {
    # PROBLEMA: 'freq' en la string NO es capturado por closure
    formula <- as.formula("y ~ sin(2 * pi * freq * x) + cos(2 * pi * freq * x)")
    fit <- tryCatch(lm(formula, data = data.frame(x = x, y = y)), ...)
    # Cuando lm() evalúa 'freq', usa el environment actual
    # Al terminar el loop, freq = last(frequencies) para TODOS los modelos
    models[[as.character(freq)]] <- list(frequency = freq, fit = fit)
  }
  models
}
```
**Todos los modelos son idénticos** — ajustados con la última frecuencia del vector. El análisis armónico produce resultados científicamente incorrectos.

**Fix correcto:**
```r
fit_harmonic_regression <- function(x, y, frequencies) {
  lapply(frequencies, function(freq) {
    formula <- as.formula(
      sprintf("y ~ sin(2 * pi * %f * x) + cos(2 * pi * %f * x)", freq, freq)
    )
    fit <- tryCatch(lm(formula, data = data.frame(x = x, y = y)), error = function(e) NULL)
    if (!is.null(fit)) list(frequency = freq, fit = fit) else NULL
  }) |> Filter(Negate(is.null), x = _)
}
```

#### BUG-05: `M3_Most_Prod_Authors` crash al instanciar
**Archivo:** `src/M3_Most_Prod_Authors/M3_Most_Prod_Authors.r`

El constructor llama `ensure_directory()` que es definida en `_utils.r` de M2. Pero `M3_Most_Prod_Authors.r` sourceta su propio `_utils.r` (vacío). `ensure_directory()` no existe en el scope → Error: `could not find function "ensure_directory"`.

#### BUG-06: `M3_M0_EDA` definida dos veces con interfaces incompatibles
**Archivos:** `src/M3_Authors/m3_m0_eda.r` y `src/M3_Most_Prod_Authors/m3_m0_eda.r`

Si ambos se sourced en la misma sesión, el segundo sobreescribe al primero. El código que usaba la primera definición falla silenciosamente con el constructor incorrecto.

#### BUG-07: `create_clean_directory()` llamada pero no definida
**Archivo:** `src/M2_Annual_Production/m2_m1_regression.r` línea 96

```r
create_clean_directory(one_column_path)    # ← función no definida
create_clean_directory(double_column_path)
```
`M2_M1_Regression::save_plot()` falla con `Error: could not find function "create_clean_directory"`.

#### BUG-08: `main2.r` referencia `src2/index.r` inexistente
**Archivo:** `examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/main2.r`

```r
source(here::here("src2", "index.r"))  # src2/ no existe en el repo
```
`AnalysisPipeline` es un diseño alternativo (pipeline API) que nunca fue terminado ni integrado.

---

### MAYOR — Comportamiento incorrecto o degradado

#### BUG-09: `toJSON()` sin prefijo de namespace
**Archivos:** `m2_m1_regression.r`, `m2_m0_eda.r`, `m2_m2_harmonics.r`

```r
json_data <- toJSON(.self$results, pretty = TRUE, auto_unbox = TRUE)
```
Funciona solo si `jsonlite` está attached via `library(jsonlite)` en algún punto anterior. Si no, error. Debería ser `jsonlite::toJSON()`.

#### BUG-10: `M2_M1_Regression::save_json` usa `write()` para JSON
```r
write(json_data, file = file.path(output_path, "m1_regression.json"))
```
`write()` en R es para vectores atómicos, no para strings grandes. Para JSONs complejos debería ser `writeLines()` o directamente `jsonlite::write_json()`.

#### BUG-11: `M3_M0_EDA::run()` en M3_Authors tiene cuerpo vacío
```r
run = function() {
  message(" ==> M3_M0 :: run")
  # Cuerpo vacío — no calcula nada
}
```

#### BUG-12: `biblioAnalysis()` calculado dos veces en el mismo pipeline
En `SystematicReviewClass.r`, `do_m1_main_information()` y `do_m2_author_prod_over_time_regression()` ambas llaman `biblioAnalysis()` — operación costosa ejecutada innecesariamente dos veces.

#### BUG-13: `__m1_report.r` está vacío pero es sourced
```r
source('../../src/M1_Main_Information/__m1_report.r')  # Archivo de 1 línea
```
No causa error (source de vacío es válido) pero es engañoso y un stub que debería implementarse o eliminarse.

---

### MENOR — Malas prácticas que no fallan pero degradan calidad

#### BUG-14: `print()` en código de producción
**Archivo:** `src/M3_Most_Prod_Authors/m3_m0_eda.r` línea 44
```r
print(.self$df)   # Debug code en producción
```

#### BUG-15: Parámetros ggplot2 deprecados
- `geom_bar(..., size = 0.25)` → debería ser `linewidth = 0.25` (deprecado desde ggplot2 3.4.0)
- `aes(y = ..density..)` → debería ser `aes(y = after_stat(density))` (deprecado)

#### BUG-16: `library()` dentro de funciones
Aparece en ~15 lugares:
```r
calculate_moving_average <- function(data, column, window_size) {
  library(zoo)   # ← NUNCA hagas esto dentro de una función
  ...
}
```
Efectos: recarga el paquete en cada llamada, genera warnings, viola el principio de separación de efectos secundarios.

---

## 5. Malas Prácticas Detectadas

### 5.1 Violaciones de Principios SOLID

| Principio | Violación | Impacto |
|-----------|-----------|---------|
| **SRP** | `M1_Main_Information.r` (~1200 líneas) mezcla análisis, plots, JSON y reportes | No testeable, imposible de mantener |
| **OCP** | Agregar un modelo de regresión requiere modificar `get_regression_models()` en `_utils.r` Y actualizar `model_function_map` en el mismo archivo | Frágil, propenso a regresiones |
| **LSP** | `M3_M0_EDA` tiene dos implementaciones incompatibles con la misma interfaz contractual | Runtime crashes |
| **ISP** | Todas las clases deben implementar `save_report()` aunque no tenga sentido (M2_M0_EDA tiene `message("[INFO] Report not implemented")`) | Interface bloat |
| **DIP** | Módulos dependen de funciones globales (`ensure_directory`, `ieee_theme`, `THEME_COLORS`) vía `source()` en lugar de inyección | Acoplamiento fuerte, imposible testear aisladamente |

### 5.2 Violaciones DRY (Don't Repeat Yourself)

| Función | Definiciones | Archivos |
|---------|-------------|---------|
| `perform_fft` | 2 (incompatibles) | `_helpers.r`, `m2_m2_harmonics.r` |
| `calculate_lomb_scargle` | 2 | `_helpers.r`, `m2_m2_harmonics.r` |
| `calculate_wavelet_power` | 2 (retorno incompatible) | `_helpers.r`, `m2_m2_harmonics.r` |
| `fit_model` | 2 (antes de Phase 0 eran 3) | `_helpers.r`, `_utils.r` |
| `get_regression_models` | 2 (diferente cantidad de modelos) | `_helpers.r`, `_utils.r` |
| `model_function_map` | 2 (diferente contenido) | `_helpers.r`, `_utils.r` |
| `ieee_theme` | 2 (definiciones distintas) | `Config/PlotThemes.r`, `M2/_settings.r` |
| `format_number` | 2 (implementaciones distintas) | `M2/_utils.r`, `M2/_performances.r` |
| `save_json()` | 2 (firmas distintas) | `M1/_helpers.r`, `M2/_utils.r` |

### 5.3 Anti-patrones Detectados

**God Object / Monolith**
```
M1_Main_Information.r (~1200 líneas)
 ├── fn_m1_main_information()        ← análisis
 ├── fn_m1_mtrc1_articles_types_pie() ← visualización
 ├── fn_m1_mtrc3_analyze_and_plot_most_prod_authors() ← análisis + visualización
 ├── fn_m1_mtrc3_generate_lorenz_curve() ← visualización
 ├── fn_m1_mtrc4_analyze_and_plot_most_cited_papers() ← análisis + visualización
 └── ... (más funciones mezcladas)
```

**Magic Numbers / Strings sin constantes**
```r
window_sizes <- c(1, 5, 10)              # Hardcoded
dpi = 900                                 # Hardcoded
output_dir = "results/M1_Main_Information/figures"  # Hardcoded
top_cited_papers <- head(data, 10)        # Hardcoded
```

**Debug code en producción**
```r
print(.self$df)
message("[DEBUG] Filename prefix: ", filename_prefix)
message("[DEBUG] Width: ", width)
print(plot)  # Imprime el objeto ggplot al console
```

**Efectos secundarios al cargar**
```r
# PlotThemes.r — ejecutado al hacer source()
generate_palette_png(THEME_COLORS, "PALETTE_VISUALIZATION__IEEE_MAYORGA_THEME_COLORS.png")
# Crea un archivo PNG en disco solo por importar el módulo
```

**Mezcla de paradigmas OOP sin razón**
- Módulos de análisis: `setRefClass` (R5/Reference Classes)
- Módulos de plots: `R6Class`
- Ninguna justificación técnica para la mezcla

**Silent failures que ocultan bugs**
```r
tryCatch(
  fn_m1_mtrc1_articles_types_pie(...),
  error = function(e) warning("[WARNING] M1 pie chart failed: ", e$message)
  # El pipeline continúa aunque falle el análisis principal
)
```

**Global mutable state**
```r
# Cualquier archivo puede hacer esto y romper todo:
ieee_theme <- theme_minimal()  # Sobreescribe el tema global
THEME_COLORS <- list()         # Rompe todos los plots
```

---

## 6. Cómo Deberían Ser M1, M2, M3

### 6.1 Arquitectura Ideal: Capas

```
┌─────────────────────────────────────────────────────┐
│                   CAPA DE APLICACIÓN                │
│           SystematicReview (Facade / Pipeline)       │
└───────────────────────┬─────────────────────────────┘
                        │ inyecta config + dependencies
┌───────────────────────▼─────────────────────────────┐
│                    CAPA DE MÓDULOS                   │
│   M1Analyzer  │  M2Analyzer  │  M3Analyzer  │  ...  │
│               │              │               │        │
│  run()        │  run()       │  run()        │        │
│  results()    │  results()   │  results()    │        │
└───────┬───────┴──────┬───────┴───────┬───────────────┘
        │              │               │
┌───────▼──────────────▼───────────────▼───────────────┐
│                 CAPA DE SERVICIOS                     │
│  PlotService  │  JsonService  │  ReportService        │
│  (ggplot2)    │  (jsonlite)   │  (LaTeX/HTML)         │
└───────┬───────┴──────┬────────┴───────┬───────────────┘
        │              │                │
┌───────▼──────────────▼────────────────▼───────────────┐
│                  CAPA DE INFRAESTRUCTURA              │
│  FileSystem  │  BibLoader  │  Config  │  Logger       │
└───────────────────────────────────────────────────────┘
```

### 6.2 Diseño Ideal de M1 — Main Information

**Principio: Un archivo por responsabilidad, una clase por concepto.**

```
src/M1_Main_Information/
├── M1_Analyzer.r          ← Clase principal: análisis bibliométrico
├── M1_DataExtractor.r     ← Extrae y normaliza datos del objeto bibliometrix
├── M1_Plots.r             ← PlotService: genera ggplot objects (sin guardar)
├── M1_Exporter.r          ← Guarda plots en disco (PNG/SVG/EPS)
└── M1_Reporter.r          ← Genera JSON y reportes TXT/LaTeX
```

**Clase canónica M1:**
```r
# M1_Analyzer.r
M1_Analyzer <- R6Class("M1_Analyzer",
  public = list(
    initialize = function(bib_data, config = M1Config$new()) {
      private$.bib_data <- bib_data
      private$.config   <- config
      private$.results  <- NULL
    },

    run = function() {
      res             <- bibliometrix::biblioAnalysis(private$.bib_data, sep = ";")
      summary_data    <- summary(res, pause = FALSE, verbose = FALSE)
      private$.results <- M1DataExtractor$new(summary_data, private$.bib_data)$extract()
      invisible(self)
    },

    results = function() {
      if (is.null(private$.results)) stop("Call run() first.")
      private$.results
    }
  ),
  private = list(.bib_data = NULL, .config = NULL, .results = NULL)
)

# Uso limpio:
analyzer <- M1_Analyzer$new(bib_data)
analyzer$run()
results <- analyzer$results()

plotter  <- M1_Plots$new(results)
exporter <- M1_Exporter$new(output_dir = "results/M1")
exporter$save(plotter$pie_chart())
exporter$save(plotter$lorenz_curve())
```

**Reglas para M1:**
- `M1_Analyzer` solo analiza — no sabe nada de plots ni archivos
- `M1_Plots` solo crea objetos ggplot — no guarda archivos
- `M1_Exporter` solo guarda — recibe ggplot objects
- `M1_Reporter` solo serializa JSON — recibe listas de datos
- Sin `library()` dentro de funciones
- Sin paths hardcodeados — todo vía config o parámetro
- Funciones puras donde sea posible (mismo input → mismo output)

### 6.3 Diseño Ideal de M2 — Annual Production

M2 ya tiene la mejor estructura del proyecto. El diseño ideal es una evolución:

```
src/M2_Annual_Production/
├── M2_Analyzer.r              ← Orquestador (ya existe como M2_Annual_Production)
├── analysis/
│   ├── M2_EDA.r               ← EDA (ya existe como M2_M0_EDA)
│   ├── M2_Regression.r        ← Regresión (ya existe como M2_M1_Regression)
│   └── M2_Harmonics.r         ← Armónicos (ya existe, con bugs)
├── models/
│   ├── growth_models.r        ← TODAS las funciones de modelos (un solo lugar)
│   └── model_registry.r       ← Registro/mapa de modelos (el model_function_map)
├── plots/
│   ├── base/R_IEEE_Plot.r     ← Clase base (ya existe)
│   └── ...                    ← Subclases de plots
└── M2_Config.r                ← Configuración del módulo
```

**Fix crítico para modelos — eliminar duplicación:**
```r
# models/growth_models.r — UN SOLO LUGAR para todas las funciones de modelos
linear_growth       <- function(t, a, b)        { a * t + b }
exponential_growth  <- function(t, r, N0, t0)   { N0 * exp(r * (t - t0)) }
logarithmic_growth  <- function(t, a, b)         { a * log(t) + b }
logistic_growth     <- function(t, K, r, t0)     { K / (1 + exp(-r * (t - t0))) }
gompertz_growth     <- function(t, N0, Nmax, k, t0, y0) {
  y0 + N0 * exp(log(Nmax / N0) * exp(-k * (t - t0)))
}
# ... etc

# models/model_registry.r — registro centralizado
MODEL_REGISTRY <- list(
  Linear       = list(fn = linear_growth,      params = c("a", "b")),
  Exponential  = list(fn = exponential_growth,  params = c("r", "N0", "t0")),
  Logistic     = list(fn = logistic_growth,     params = c("K", "r", "t0")),
  Gompertz     = list(fn = gompertz_growth,     params = c("N0", "Nmax", "k", "t0", "y0")),
  # Para agregar un nuevo modelo: UNA SOLA LÍNEA AQUÍ
)
```

**Fix para fit_harmonic_regression (BUG-04):**
```r
# Correcto: interpolación de string para capturar freq en el ambiente de formulación
fit_harmonic_regression <- function(x, y, frequencies) {
  lapply(frequencies, function(freq) {
    formula <- as.formula(
      sprintf("y ~ sin(2*pi*%f*x) + cos(2*pi*%f*x)", freq, freq)
    )
    fit <- tryCatch(
      lm(formula, data = data.frame(x = x, y = y)),
      error = function(e) NULL
    )
    if (!is.null(fit)) list(frequency = freq, fit = fit) else NULL
  }) |> Filter(Negate(is.null), x = _)
}
```

### 6.4 Diseño Ideal de M3 — Authors

M3 debe implementarse desde cero con estructura similar a M2:

```
src/M3_Authors/
├── M3_Analyzer.r              ← Orquestador (reemplaza los dos M3 actuales)
├── analysis/
│   ├── M3_EDA.r               ← EDA de autores (n_authors, coauthorship, etc.)
│   ├── M3_Productivity.r      ← Índices de productividad (h-index, g-index, m-index)
│   └── M3_Network.r           ← Red de coautoría (igraph)
├── plots/
│   ├── M3_Plots_Productivity.r ← Bar charts de productividad
│   └── M3_Plots_Network.r      ← Visualización de redes
└── M3_Config.r
```

**Interfaz canónica:**
```r
M3_Analyzer <- R6Class("M3_Analyzer",
  public = list(
    initialize = function(bib_data, config = M3Config$new()) {
      private$.bib_data <- bib_data
      private$.config   <- config
    },

    run = function() {
      private$.results <- list(
        eda          = M3_EDA$new(private$.bib_data)$run(),
        productivity = M3_Productivity$new(private$.bib_data)$run(),
        network      = M3_Network$new(private$.bib_data)$run()
      )
      invisible(self)
    },

    results = function() private$.results
  ),
  private = list(.bib_data = NULL, .config = NULL, .results = NULL)
)
```

**Métricas que debe calcular M3:**
- `total_authors`: número de autores únicos
- `single_author_papers`: papers de un solo autor
- `multi_author_papers`: papers multi-autor
- `h_index` por autor: número H donde H papers tienen al menos H citas
- `g_index` por autor: mayor G donde G papers tienen en total ≥ G² citas
- `m_index` por autor: h-index / años de carrera bibliométrica
- `collaboration_rate`: proporción de papers multi-autor
- `coauthorship_network`: grafo igraph de relaciones autor-autor
- `betweenness_centrality`: autores clave en la red de colaboración
- `top_N_authors_by_*`: tablas de autores más productivos, más citados, más colaborativos

### 6.5 Contrato Universal para Módulos (Interfaz Compartida)

Todo módulo M1..Mn debe implementar:

```r
# Contrato mínimo — todas las clases de módulo deben seguirlo
ModuleContract <- R6Class("ModuleContract",
  public = list(
    # Ejecuta el análisis — devuelve invisible(self) para chaining
    run     = function() stop("Not implemented"),

    # Devuelve los resultados como lista R (no modifica estado)
    results = function() stop("Not implemented"),

    # Valida que los datos de entrada sean correctos
    validate = function() stop("Not implemented")
  )
)
```

---

## 7. Buenas Prácticas a Adoptar

### 7.1 Gestión de Dependencias

```r
# MAL — library() dentro de funciones
calculate_moving_average <- function(data, column, window_size) {
  library(zoo)
  ...
}

# BIEN — use namespace explícito o declara al tope del archivo
calculate_moving_average <- function(data, column, window_size) {
  dplyr::mutate(data,
    Moving_Avg = zoo::rollmeanr(get(column), k = window_size, fill = NA)
  )
}
```

### 7.2 Paths — Nunca Hardcodeados

```r
# MAL
save_plot <- function(plot, filename) {
  path <- "results/M1_Main_Information/figures"  # hardcoded
  ...
}

# BIEN — recibe el path como parámetro con valor por defecto
save_plot <- function(plot, filename, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  ...
}
```

### 7.3 Logging Consistente

```r
# MAL — inconsistente, mezclado con debug
message("[DEBUG] Filename prefix: ", filename_prefix)
message("[INFO] Plot saved")
cat("=== Running metrics ===\n")
print(df)

# BIEN — usar un logger con niveles
# Instalar: install.packages("logger")
library(logger)
log_threshold(INFO)

log_info("Plot saved at {path}")
log_debug("Input shape: {nrow(df)} x {ncol(df)}")
log_warn("Missing column {col}, skipping")
log_error("Failed to fit model: {msg}")
```

### 7.4 Estandarizar en R6

```r
# ELIMINAR setRefClass — usar R6 en todo el proyecto
# ANTES (setRefClass):
M2_Annual_Production <- setRefClass("M2_Annual_Production",
  fields = list(df = "data.frame"),
  methods = list(run = function() { ... })
)

# DESPUÉS (R6):
M2_Analyzer <- R6Class("M2_Analyzer",
  public = list(
    initialize = function(df) { private$.df <- df },
    run = function() { ... ; invisible(self) }
  ),
  private = list(.df = NULL)
)
```

### 7.5 Tests para Funciones Puras

```r
# tests/testthat/test-growth-models.R
test_that("logistic_growth returns K at infinity", {
  expect_equal(logistic_growth(1e6, K = 100, r = 0.5, t0 = 2000), 100, tolerance = 1e-6)
})

test_that("calculate_moving_average length matches input", {
  df <- data.frame(Year = 2000:2020, Articles = rnorm(21, 50, 10))
  result <- calculate_moving_average(df, "Articles", window_size = 3)
  expect_equal(nrow(result), nrow(df))
})

test_that("get_anomalies returns empty df when no anomalies", {
  df <- data.frame(Year = 2000:2020, Articles = rep(50, 21))
  result <- get_anomalies(df, "Articles", "Year")
  expect_equal(nrow(result), 0)
})
```

### 7.6 No Silent Failures

```r
# MAL — oculta el bug, el análisis continúa con datos incorrectos
tryCatch(
  fn_m1_mtrc1_articles_types_pie(data),
  error = function(e) warning("failed: ", e$message)
)

# BIEN — solo wrappear lo verdaderamente opcional
# Para análisis críticos: dejar que el error propague
fn_m1_mtrc1_articles_types_pie(data)  # Si falla, debe fallar

# Para plots opcionales: loggear y continuar CON DISTINCIÓN CLARA
tryCatch(
  fn_m1_mtrc1_articles_types_pie(data),
  error = function(e) {
    log_warn("Optional plot M1_G2 failed: {e$message}. Continuing.")
  }
)
```

---

## 8. Tickets de Trabajo

### Categorías
- `[BUG]` — Bug que causa fallo o resultado incorrecto
- `[DEBT]` — Deuda técnica que degrada calidad
- `[FEAT]` — Feature nueva
- `[REFACTOR]` — Refactorización sin cambio de funcionalidad

---

### BUGS — Prioridad Crítica

---

#### BUG-001 · `fit_harmonic_regression` produce resultados científicamente incorrectos
**Archivo:** `src/M2_Annual_Production/m2_m2_harmonics.r:298`
**Severidad:** Crítica (resultados científicamente inválidos)
**Síntoma:** Todos los modelos armónicos son idénticos entre sí — todos ajustados con la última frecuencia del vector.
**Causa raíz:** Bug de scoping de R en `for` loop con `as.formula(string)`. La variable `freq` en el string no se captura por value.
**Fix:** Usar `sprintf()` para interpolar el valor numérico en la fórmula, o usar `local()`.
**Criterio de aceptación:** Los modelos armónicos para `freq=0.1` y `freq=0.5` producen ajustes distintos.

---

#### BUG-002 · `M3_Most_Prod_Authors` falla al instanciar (`ensure_directory` no definida)
**Archivo:** `src/M3_Most_Prod_Authors/M3_Most_Prod_Authors.r`
**Severidad:** Crítica (crash instantáneo)
**Síntoma:** `Error: could not find function "ensure_directory"` al hacer `M3_Most_Prod_Authors$new(...)`.
**Causa raíz:** El constructor llama `ensure_directory()` que debería venir de `_utils.r`, pero el archivo está vacío. La función `ensure_directory` solo está definida en `M2_Annual_Production/_utils.r`.
**Fix:** Crear un `src/utils/fs_utils.r` compartido con `ensure_directory()` y sourcing desde ambos módulos, O copiar la función al `_utils.r` de M3.
**Criterio de aceptación:** `M3_Most_Prod_Authors$new(df)` no lanza error.

---

#### BUG-003 · `M3_M0_EDA` definida dos veces con interfaces incompatibles
**Archivos:** `src/M3_Authors/m3_m0_eda.r` y `src/M3_Most_Prod_Authors/m3_m0_eda.r`
**Severidad:** Crítica
**Síntoma:** Si ambos archivos son sourced, la segunda definición sobreescribe la primera con una interfaz de constructor diferente.
**Fix:** Renombrar: `M3_Authors_EDA` y `M3_MostProdAuthors_EDA`, o unificar en una sola clase con parámetros opcionales.
**Criterio de aceptación:** Ambas clases pueden coexistir en la misma sesión R sin conflicto.

---

#### BUG-004 · `create_clean_directory()` llamada pero nunca definida
**Archivo:** `src/M2_Annual_Production/m2_m1_regression.r:96`
**Severidad:** Crítica
**Síntoma:** `M2_M1_Regression$save_plot()` lanza `Error: could not find function "create_clean_directory"`.
**Fix:** Implementar `create_clean_directory()` en `M2_Annual_Production/_utils.r` (wrapper de `dir.create` que limpia el directorio antes).
**Criterio de aceptación:** `m2$save_plot("results/M2")` crea los directorios `OneColumn/` y `DoubleColumn/` correctamente.

---

#### BUG-005 · `perform_fft`, `calculate_lomb_scargle`, `calculate_wavelet_power` definidas dos veces
**Archivos:** `_helpers.r` y `m2_m2_harmonics.r`
**Severidad:** Mayor (comportamiento frágil, dependiente del orden de source)
**Fix:** Eliminar las 3 funciones de `_helpers.r` (son simples wrappers) y dejar solo las implementaciones completas en `m2_m2_harmonics.r`. Renombrar en `_helpers.r` con prefijo `_simple_` si se necesitan versiones básicas.
**Criterio de aceptación:** Solo existe una definición de cada función. Tests confirman comportamiento correcto.

---

#### BUG-006 · `main2.r` referencia `src2/index.r` inexistente
**Archivo:** `examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/main2.r`
**Severidad:** Mayor (el script no puede ejecutarse)
**Fix:** Decidir: (a) eliminar `main2.r` y mover a `archived/`, (b) implementar `src2/index.r` con la API `AnalysisPipeline`.
**Criterio de aceptación:** O el archivo es removido, o `Rscript main2.r` ejecuta sin error.

---

#### BUG-007 · `toJSON()` sin namespace en 3 archivos
**Archivos:** `m2_m1_regression.r`, `m2_m0_eda.r`, `m2_m2_harmonics.r`
**Severidad:** Menor
**Fix:** Reemplazar `toJSON(...)` por `jsonlite::toJSON(...)` en todos los casos.
**Criterio de aceptación:** `grep -r "toJSON" src/` solo muestra llamadas con `jsonlite::` prefix.

---

#### BUG-008 · `write()` para guardar JSON (tipo incorrecto)
**Archivo:** `src/M2_Annual_Production/m2_m1_regression.r:140`
**Severidad:** Menor
**Fix:** `write(json_data, ...)` → `writeLines(json_data, ...)` o mejor `jsonlite::write_json(...)` directamente.
**Criterio de aceptación:** El JSON guardado es válido y parseable.

---

#### BUG-009 · `biblioAnalysis()` ejecutado dos veces en el mismo pipeline
**Archivo:** `src/SystematicReviewClass.r`
**Severidad:** Menor (performance)
**Fix:** Cachear resultado en campo `private$.analysis_cache` y reutilizarlo.
**Criterio de aceptación:** `biblioAnalysis` se llama exactamente una vez por instancia de `SystematicReview`.

---

#### BUG-010 · `print()` y debug messages en código de producción
**Archivos:** `M3_Most_Prod_Authors/m3_m0_eda.r:44`, `M1/_plots.r`, múltiples
**Severidad:** Menor
**Fix:** Eliminar todos los `print()` y `message("[DEBUG]...")` del código de producción.
**Criterio de aceptación:** `grep -r "print(" src/` no devuelve resultados. `grep -r "\[DEBUG\]" src/` no devuelve resultados.

---

### DEUDA TÉCNICA — Prioridad Alta

---

#### DEBT-001 · Estandarizar paradigma OOP en R6 en todo el proyecto
**Alcance:** `src/M2_Annual_Production/` (4 clases setRefClass), `src/M3_*/` (3 clases setRefClass)
**Motivación:** `setRefClass` y `R6Class` tienen semántica diferente. Mezclarlos en el mismo módulo (M2 usa setRefClass para análisis y R6 para plots) genera confusión y distintos patrones de herencia.
**Trabajo:** Migrar `M2_Annual_Production`, `M2_M0_EDA`, `M2_M1_Regression`, `M2_M2_Harmonic`, `M3_Authors`, `M3_Most_Prod_Authors`, `SystematicReview` de `setRefClass` a `R6Class`.
**Criterio de aceptación:** `grep -r "setRefClass" src/` no devuelve resultados. Todos los tests pasan.

---

#### DEBT-002 · Eliminar `library()` dentro de funciones
**Alcance:** 15+ ocurrencias en `_helpers.r`, `m2_m2_harmonics.r`, `M4_Countries/_helpers.r`, etc.
**Motivación:** Viola principio de side-effect-free functions. Impacta performance. Hace las funciones no testeables en aislamiento.
**Fix:** Mover todas las declaraciones `library()` al top-level del archivo o usar `requireNamespace()` + notación `::`.
**Criterio de aceptación:** `grep -rn "library(" src/` solo aparece en los tops de archivo, nunca dentro de definiciones de función.

---

#### DEBT-003 · Refactorizar M1_Main_Information.r (monolito ~1200 líneas)
**Motivación:** Viola SRP. No testeable. No mantenible.
**Propuesta de división:**
```
M1_Main_Information.r    ← Eliminar, reemplazar por:
M1_DataExtractor.r       ← extract_bibliographic_data(), extract_document_types()
M1_MetricsCalculator.r   ← calculate_bradford_law(), calculate_lorenz_gini()
M1_Plots.r               ← Todas las fn_m1_mtrc*_plot() como métodos de clase
M1_Reporter.r            ← save_json(), generate_report()
M1_Analyzer.r            ← Orquestador (solo wiring)
```
**Criterio de aceptación:** Ningún archivo individual supera 200 líneas. `R CMD check` pasa.

---

#### DEBT-004 · Eliminar paths hardcodeados — parametrizar via Config
**Alcance:** `M1/_plots.r:29`, `M1/_helpers.r:27`, `M1/_report.r:11`, múltiples
**Fix:** Crear `M1Config` y `M2Config` R6 clases con defaults, pasadas vía constructor.
```r
M1Config <- R6Class("M1Config",
  public = list(
    output_dir_figures = "results/M1_Main_Information/figures",
    output_dir_jsons   = "results/M1_Main_Information/jsons",
    output_dir_reports = "results/M1_Main_Information/reports",
    dpi                = 600,
    top_n_authors      = 10,
    top_n_papers       = 10
  )
)
```
**Criterio de aceptación:** `grep -rn '"results/' src/` devuelve 0 resultados.

---

#### DEBT-005 · Consolidar funciones compartidas en `src/shared/`
**Motivación:** `ensure_directory`, `save_json`, `save_plot_generic`, `format_number` son reimplementadas en múltiples módulos.
**Propuesta:**
```
src/shared/
├── fs_utils.r      ← ensure_directory, create_clean_directory, save_file
├── io_utils.r      ← save_json (una sola implementación canónica)
├── plot_utils.r    ← save_plot genérico (recibe ggplot + config)
└── math_utils.r    ← format_number, normalize, etc.
```
**Criterio de aceptación:** Las funciones compartidas tienen una única definición. Todos los módulos las importan de `src/shared/`.

---

#### DEBT-006 · Unificar módulos M3_Authors y M3_Most_Prod_Authors
**Motivación:** Dos módulos para el mismo dominio (análisis de autores) con código que debería ser compartido. `M3_M0_EDA` definida dos veces con interfaces diferentes.
**Fix:** Crear `src/M3_Authors/` como único módulo con subclases para EDA, productividad y red.
**Criterio de aceptación:** Un solo directorio `M3_Authors/` con clase única `M3_Analyzer`. El directorio `M3_Most_Prod_Authors/` eliminado o movido a `archived/`.

---

#### DEBT-007 · Implementar logging con `logger` o `futile.logger`
**Motivación:** `message()` no tiene niveles, no se puede silenciar, no se puede filtrar, no guarda a archivo.
**Fix:**
```r
# Agregar a DESCRIPTION: Imports: logger
library(logger)
log_threshold(INFO)  # Configurable: DEBUG, INFO, WARN, ERROR

log_info("M1 analysis started for {nrow(bib_data)} documents")
log_debug("Bradford law computed: {nrow(bradford_law)} sources")
log_warn("Missing DOI in {sum(is.na(bib_data$DI))} documents")
log_error("Failed to save plot: {conditionMessage(e)}")
```
**Criterio de aceptación:** `grep -rn "^message(" src/` devuelve 0 en archivos no-test.

---

#### DEBT-008 · Eliminar `font_import()` de la carga del módulo
**Archivo:** `src/M1_Main_Information/_plots.r:20-23`
**Motivación:** `extrafont::font_import()` tarda 1-5 minutos en ejecutarse. Se ejecuta cada vez que M1 es sourced.
**Fix:** Documentar que se debe ejecutar una vez manualmente (`extrafont::font_import()`), y reemplazar la llamada automática por un check:
```r
if (!isTRUE(getOption("fonts_imported"))) {
  message("[INFO] Run extrafont::font_import() once to enable custom fonts.")
}
extrafont::loadfonts(device = "postscript", quiet = TRUE)
```
**Criterio de aceptación:** Sourcear `_plots.r` no tarda más de 1 segundo.

---

#### DEBT-009 · Actualizar parámetros deprecados de ggplot2
**Alcance:** Múltiples archivos en M1 y M2
**Items:**
- `geom_bar(..., size = 0.25)` → `linewidth = 0.25`
- `geom_line(..., size = 1)` → `linewidth = 1`
- `aes(y = ..density..)` → `aes(y = after_stat(density))`
- `geom_bar(stat = "identity", size = ...)` → `linewidth`

**Criterio de aceptación:** `R CMD check` sin warnings de ggplot2 deprecation.

---

#### DEBT-010 · Integrar `module_m5_tables_utils.r` o documentarlo
**Archivo:** `src/03_utils/modules/module_m5/module_m5_tables_utils.r`
**Motivación:** Código bien escrito (funciones `m5_top_cited`, `m5_top_recent`) pero sin importador.
**Fix:** (a) Integrarlo en M1 donde se usan top papers, o (b) moverlo a `src/M5_Documents/` como parte del módulo M5, o (c) mover a `archived/` si no se va a usar.
**Criterio de aceptación:** El archivo tiene al menos un `source()` que lo importa, o está en `archived/`.

---

### FEATURES — Módulos Nuevos y Funcionalidades

---

#### FEAT-001 · Implementar M3_Authors completo (h-index, g-index, redes)
**Descripción:** El módulo M3 es el único faltante funcional entre los análisis bibliométricos estándar.
**Métricas requeridas:**
- h-index por autor
- g-index por autor
- m-index (h / career_years)
- Tasa de colaboración
- Grafo de coautoría (igraph)
- Centralidad (betweenness, degree, closeness)
- Clustering de comunidades de autores
- Top N autores por cada métrica

**Entregables:**
- `M3_Analyzer` R6 class
- 3 subclases de análisis (EDA, Productivity, Network)
- 4 plots (bar chart productividad, scatter h vs publicaciones, grafo de red, heatmap colaboración)
- JSON con todos los resultados
- Tests unitarios para funciones puras

**Criterio de aceptación:** `do_m3_authors()` en `SystematicReviewClass` ejecuta sin error con los datasets de ejemplo.

---

#### FEAT-002 · Implementar M4_Countries completo
**Descripción:** El módulo tiene solo `extract_country_data()`. Falta toda la lógica de análisis y visualización.
**Funcionalidades requeridas:**
- Producción por país (total papers, total citas, MCP, SCP)
- Clasificación por cuadrante (alta/baja producción vs alta/baja citación)
- Mapa geográfico coropletico (rnaturalearth + ggplot2)
- Evolución temporal por país
- Red de colaboración internacional entre países
- Índice de impacto por país (TC / TP)

**Criterio de aceptación:** `do_m4_countries()` genera al menos 3 plots y un JSON con métricas por país.

---

#### FEAT-003 · Implementar suite de tests con testthat
**Descripción:** El proyecto tiene cero tests. Toda funcionalidad es probada manualmente.
**Alcance mínimo:**
```
tests/
└── testthat/
    ├── test-growth-models.r         ← Tests para todas las funciones de crecimiento
    ├── test-statistical-helpers.r   ← Tests para calculate_*, detect_*, identify_*
    ├── test-m1-data-extraction.r    ← Tests para extract_bibliographic_data(), etc.
    ├── test-m2-eda.r                ← Tests para M2_M0_EDA
    ├── test-m2-regression.r         ← Tests para M2_M1_Regression con datos sintéticos
    └── test-systematic-review.r     ← Tests de integración con dataset minimal
```
**Criterio de aceptación:** `devtools::test()` ejecuta y pasa. Coverage ≥ 60% en funciones puras.

---

#### FEAT-004 · CI/CD con GitHub Actions
**Descripción:** Ningún proceso de integración continua.
**Propuesta:**
```yaml
# .github/workflows/R-CMD-check.yml
on: [push, pull_request]
jobs:
  check:
    runs-on: ubuntu-latest
    steps:
      - uses: r-lib/actions/setup-r@v2
      - uses: r-lib/actions/setup-r-dependencies@v2
      - run: R CMD check --no-manual .
  test:
    runs-on: ubuntu-latest
    steps:
      - run: Rscript -e "devtools::test()"
```
**Criterio de aceptación:** PR no puede mergearse si `R CMD check` falla o tests fallan.

---

#### FEAT-005 · Implementar `AnalysisPipeline` (API fluida de `main2.r`)
**Descripción:** `main2.r` referencia una API más moderna (`AnalysisPipeline`) que está sin implementar.
**Propuesta:**
```r
pipeline <- AnalysisPipeline$new(
  bib_file = "data/scopus.bib",
  modules  = c("M1", "M2", "M3", "M4"),
  out_dir  = "results"
)$setTitle("My Review")$run()
```
Esta API es más fluida y configurable que `SystematicReviewClass`. Debería reemplazarla eventualmente.
**Criterio de aceptación:** `Rscript main2.r` ejecuta sin error con dataset de ejemplo.

---

#### FEAT-006 · Reportes automatizados M8 (LaTeX + HTML)
**Descripción:** El README menciona M8 como "Automated Bibliometric Report" pero no existe.
**Propuesta:**
- Template LaTeX con secciones para M1-M4
- Template R Markdown / Quarto
- Auto-llenado con resultados del pipeline
- Exportación a PDF y HTML

**Criterio de aceptación:** `systematicReview$do_m8_report()` genera un PDF con las figuras y tablas del análisis.

---

#### FEAT-007 · Cache de `biblioAnalysis` para evitar recómputo
**Descripción:** `biblioAnalysis()` tarda 10-30 segundos en datasets grandes. Se ejecuta múltiples veces innecesariamente.
**Fix:**
```r
# En SystematicReview
.get_biblio_analysis = function() {
  if (is.null(private$.analysis_cache)) {
    private$.analysis_cache <- bibliometrix::biblioAnalysis(self$bib_data, sep = ";")
  }
  private$.analysis_cache
}
```
**Criterio de aceptación:** `do_m1_main_information()` + `do_m2_author_prod_over_time_regression()` llaman `biblioAnalysis` exactamente una vez.

---

#### FEAT-008 · Documentación roxygen2 para funciones públicas
**Descripción:** Ninguna función tiene documentación formal. `R CMD check` falla con warnings sobre funciones sin documentar.
**Alcance mínimo:** Documentar las 20 funciones más usadas de M1 y M2.
**Formato:**
```r
#' Calculate Moving Average
#'
#' Computes a rolling mean over a time series.
#'
#' @param data data.frame with Year and Articles columns
#' @param column character. Column name to compute average over.
#' @param window_size integer. Window size in years.
#' @return data.frame with Year and Articles (moving average) columns.
#' @examples
#' df <- data.frame(Year = 2000:2020, Articles = rnorm(21, 50, 10))
#' calculate_moving_average(df, "Articles", 5)
#' @export
calculate_moving_average <- function(data, column, window_size) { ... }
```
**Criterio de aceptación:** `devtools::document()` genera `NAMESPACE` y páginas de ayuda. `R CMD check` sin warnings de documentación.

---

#### FEAT-009 · Soporte multi-fuente en SystematicReview (WoS, PubMed)
**Descripción:** Actualmente solo soporta Scopus (.bib) y OpenAlex (.csv).
**Propuesta:** Detección automática por formato/columnas:
```r
init = function() {
  loader <- BibLoader$detect(.self$bib_path)  # Factory pattern
  .self$bib_data <- loader$load()
}
```
**Criterio de aceptación:** `setBibPath("data/wos.txt")` carga datos de Web of Science correctamente.

---

#### FEAT-010 · Implementar M5 Documents (citaciones, clustering de documentos)
**Descripción:** `module_m5_tables_utils.r` ya tiene las tablas. Falta la clase y los plots.
**Funcionalidades:**
- Top N documentos más citados (integrar tablas existentes)
- Análisis de co-citación
- Clustering de documentos por similitud temática (LDA o TF-IDF + k-means)
- Mapa de calor de co-citas

---

## 9. Orden de Ejecución Recomendado

```
Sprint 1 (Foundations):
  BUG-001  → fix fit_harmonic_regression
  BUG-002  → fix M3_Most_Prod_Authors instantiation
  BUG-004  → implement create_clean_directory
  BUG-007  → fix toJSON namespace
  DEBT-002 → remove library() inside functions
  DEBT-008 → fix font_import() slow load
  DEBT-009 → fix deprecated ggplot2 params

Sprint 2 (Architecture):
  DEBT-001 → standardize R6
  DEBT-003 → refactor M1 monolith
  DEBT-004 → eliminate hardcoded paths
  DEBT-005 → create src/shared/
  DEBT-006 → unify M3 modules
  BUG-003  → fix M3_M0_EDA dual definition

Sprint 3 (Quality):
  FEAT-003 → test suite
  FEAT-004 → CI/CD
  FEAT-008 → roxygen documentation
  DEBT-007 → logging framework

Sprint 4 (New Modules):
  FEAT-001 → M3_Authors complete
  FEAT-002 → M4_Countries complete
  FEAT-007 → biblioAnalysis cache
  FEAT-005 → AnalysisPipeline API

Sprint 5 (Reports & Extensions):
  FEAT-006 → M8 automated report
  FEAT-009 → multi-source support
  FEAT-010 → M5 Documents
```

---

## 10. Checklist de Salud del Proyecto

| Check | Estado Actual | Target |
|-------|--------------|--------|
| `R CMD check` sin errores | ❌ | ✅ Sprint 3 |
| `R CMD check` sin warnings | ❌ | ✅ Sprint 3 |
| Test coverage ≥ 60% | ❌ 0% | ✅ Sprint 3 |
| Ningún `library()` dentro de funciones | ❌ | ✅ Sprint 1 |
| Ningún path hardcodeado | ❌ | ✅ Sprint 2 |
| Un solo paradigma OOP (R6) | ❌ | ✅ Sprint 2 |
| Cada función definida una sola vez | ❌ | ✅ Sprint 2 |
| `M1_Main_Information.r` < 200 líneas | ❌ (~1200) | ✅ Sprint 2 |
| M3 ejecuta sin error | ❌ | ✅ Sprint 4 |
| M4 ejecuta sin error | ❌ | ✅ Sprint 4 |
| CI/CD verde en cada PR | ❌ | ✅ Sprint 3 |
| Documentación roxygen completa | ❌ | ✅ Sprint 3 |
| Sin `print()` en producción | ❌ | ✅ Sprint 1 |
| Sin deprecated ggplot2 | ❌ | ✅ Sprint 1 |
| Cache de biblioAnalysis | ❌ | ✅ Sprint 4 |
| Logging framework unificado | ❌ | ✅ Sprint 3 |
