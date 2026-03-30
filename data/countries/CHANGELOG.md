# Changelog - Indicadores de países

## Formato

Agregar entradas en formato:

```
## YYYY-MM-DD

- [indicador] Actualizado: descripción del cambio
- Fuente: nombre de la fuente
```

---

## Historial

### v0.2.0 (2026-03-29)

Indicadores pobjetados con datos reales:
- countries_reference.csv: 296 países con coordenadas, income group, 41 con población/área
- gdp_pc_ppp.csv: 203 países, 6832 obs (World Bank WDI)
- gdp_growth.csv: 214 países, 11214 obs (World Bank WDI)
- rd_gdp.csv: 153 países, 2366 obs (World Bank WDI)
- articles.csv: 197 países, 5320 obs (World Bank WDI)
- tertiary_enroll.csv: 203 países, 4939 obs (World Bank WDI)
- education_gdp.csv: 203 países, 5127 obs (World Bank WDI)
- internet_users.csv: 213 países, 6078 obs (World Bank WDI)
- gini.csv: 171 países, 2402 obs (World Bank WDI)
- high_tech_exports.csv: 186 países, 2683 obs (World Bank WDI)
- ip_receipts.csv: 171 países, 3851 obs (World Bank WDI)
- urban_pop.csv: 217 países, 14105 obs (World Bank WDI)

Indicadores pendientes (requieren fuentes alternativas):
- hdi.csv, doctoral_grads.csv, gerd.csv, researchers_pm.csv
- citable_docs.csv, sjr_rank.csv, patents_res.csv, patents_pct.csv
- rd_business.csv, rd_personnel_pm.csv, gdp_forecast.csv, population_forecast.csv

### v0.1.0 (2026-03-28)

- Versión inicial con estructura de directorios
- 21 indicadores definidos en indicators_metadata.csv
- Script update_indicators.R creado
- Placeholders CSV listos para poblar
