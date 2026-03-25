# PROMPT: Ordenar y Limpiar la Casa - RBiblioSynth

## Contexto
Eres un Senior Software Engineer y Code Reviewer con experiencia en proyectos R y metodologías ágiles. Tu tarea es transformar este proyecto caótico de bibliometría en una base sólida y mantenible.

## Estado Actual del Proyecto
- Proyecto R de análisis bibliométrico
- Múltiples archivos vacíos o a medio terminar
- Código duplicado
- Referencias rotas (ejemplo principal no funciona)
- Sin estructura de paquete R
- Sin tests
- Sin CI/CD
- Mezcla de patrones arquitectónicos (R6 classes + scripts funcionales)

## Tu Misión: Phase 0 - Limpieza Fundamental

Ejecuta las siguientes tareas en orden EXACTO:

### Fase 1: Evaluación y Diagnóstico
1. Haz un inventario completo de TODOS los archivos en el proyecto
2. Identifica TODOS los archivos vacíos o con 0 líneas útiles
3. Identifica TODO el código duplicado
4. Lista todas las referencias rotas (archivos que se importan pero no existen)
5. Documenta qué módulos realmente funcionan vs cuáles son stubs

### Fase 2: Eliminar lo Muerto
1. **ELIMINA** todos los archivos vacíos:
   - `src/M3_Authors/_helpers.r`
   - `src/M3_Authors/_utils.r`
   - `src/03_utils/io/io_paths.r`
   
2. **ELIMINA** o mueve a `/archived/` el directorio `_temp/` completo
3. **ELIMINA** cualquier código comentado que no se vaya a usar

### Fase 3: Arreglar Lo Roto (Prioridad Máxima)
1. **CREA** el archivo `src/SystematicReviewClass.r` que falta y hace falta para que funcione el ejemplo principal
   - O reescribe el ejemplo para no depender de él
2. **ARREGLA** las referencias en:
   - `examples/SCOPUS_POWER_SYSTEMS_FREQUENCY_ESTIMATOR/main.r`
   - `examples/OPEN_ALEX_LEISMANIASIS/main.r`

### Fase 4: Estructura de Paquete R
1. **CREA** `DESCRIPTION` con:
   - Package name
   - Title
   - Description
   - Author, Maintainer, License
   - Dependencies (debes inferirlas del código: bibliometrix, ggplot2, jsonlite, dplyr, zoo, lomb, WaveletComp, etc.)
   
2. **CREA** `.Rbuildignore` básico:
   ```
   ^_temp$
   ^\.DS_Store$
   ^examples/
   ^data/
   ^_bookdown_
   ^README\.md$
   ^CLAUDE\.md$
   ^ROADMAP\.md$
   ```

3. **CREA** `NAMESPACE` básico (puedes empezar vacío o con exports mínimos)

### Fase 5: Limpiar Código Duplicado
1. En `src/M2_Annual_Production/_helpers.r`:
   - Identifica funciones duplicadas
   - Consolida en UNA definición por función
   - Elimina redundancias

### Fase 6: Git
1. **CREA** o actualiza `.gitignore` apropiadamente
2. **NO** hagas commit de:
   - `_temp/`
   - `.RData`
   - `.Rhistory`
   - resultados (`results/`)
   - PDFs, plots generados

## Restricciones Importantes
- NO agregar nuevas features
- NO implementar módulos que no existan
- NO escribir tests aún (eso viene después)
- NO agregar documentación roxygen aún
- Mantener la funcionalidad existente que SÍ funciona

## Entregables de Esta Fase
1. Proyecto que pueda ejecutar `R CMD build` sin errores
2. Ejemplo principal que CORRA sin errores
3. Sin archivos vacíos
4. Sin código duplicado
5. DESCRIPTION funcional
6. Estructura de paquete R básica

## Al Finalizar
Proporciona un resumen de:
- Archivos eliminados
- Archivos creados/modificados
- Estado actual del proyecto (qué funciona, qué no)
- Siguientes pasos recomendados

---

**Este prompt debe ejecutarse COMPLETO antes de cualquier otra tarea de desarrollo.**
