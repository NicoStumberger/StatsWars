# StatsWars

## Análisis probabilístico de conflictos armados post-1945

Scripts R y documentos Quarto utilizados en la tesis de maestría:

> *Análisis probabilístico de conflictos armados: frecuencia, 
> intervalo, duración e intensidad mediante modelos de conteo y supervivencia*

Tesis presentada para obtener el título de Magíster en Generación y 
Análisis de Información Estadística — Universidad Nacional de Tres de 
Febrero (UNTREF).

**Autor:** Nicolás Stumberger  
**Director:** Jorge E. Sagula

---

## Descripción

La investigación analiza la dinámica del conflicto armado a través de 
cuatro dimensiones — frecuencia, intervalo, duración e intensidad — 
mediante modelos estadísticos de conteo y supervivencia, evaluando el 
efecto de factores estructurales, geográficos e históricos sobre cada 
dimensión. El análisis se realiza en tres fuentes de datos independientes 
como estrategia de triangulación cuantitativa.

**Datasets utilizados:**
- **SDQ** — The Statistics of Deadly Quarrels (Richardson, 1960): 1807–1949
- **COW** — Correlates of War Project: 1816–2014
- **UCDP** — Uppsala Conflict Data Program: 1939–2024

---

## Estructura del repositorio

### Preparación de datos

| Script | Descripción |
|--------|-------------|
| `StatsWars_00_DataPrep_SDQ.R` | Limpieza y construcción de variables — SDQ |
| `StatsWars_00_DataPrep_COW.R` | Limpieza y construcción de variables — COW |
| `StatsWars_00_DataPrep_UCDP.R` | Limpieza y construcción de variables — UCDP |
| `StatsWars_01_DataPrep_ForModel.R` | Integración y preparación final para modelos |

### Análisis exploratorio

| Script | Descripción |
|--------|-------------|
| `StatsWars_02_EDA.R` | Análisis exploratorio de datos — tres fuentes |

### Modelos estadísticos

| Script | Descripción |
|--------|-------------|
| `StatsWars_03_Modelos_Frecuencia.R` | Modelos de conteo — familia Poisson |
| `StatsWars_03_Modelos_TiempoEntre.R` | Modelos de supervivencia — intervalo entre conflictos |
| `StatsWars_03_Modelos_Duracion.R` | Modelos de supervivencia — duración de conflictos |
| `StatsWars_03_Modelos_Intensidad.R` | Modelos lineales y logísticos — intensidad |

### Anexos técnicos (Quarto)

| Archivo | Descripción |
|---------|-------------|
| `StatsWars_04_Anexo1_Frecuencia_reducido.qmd` | Anexo 1 — Dimensión frecuencia |
| `StatsWars_04_Anexo2_Intervalo_v05_reducido.qmd` | Anexo 2 — Dimensión intervalo |
| `StatsWars_04_Anexo3_Duracion_reducido.qmd` | Anexo 3 — Dimensión duración |
| `StatsWars_04_Anexo4_Intensidad_reducido.qmd` | Anexo 4 — Dimensión intensidad |

---

## Requisitos

R 4.x con los siguientes paquetes principales:
```r
install.packages(c(
  "survival", "flexsurv", "MASS", "broom",
  "ggplot2", "dplyr", "tidyr", "HistData",
  "lmtest", "car", "patchwork"
))
```

---

## Fuentes de datos

Los datasets no están incluidos en este repositorio y deben descargarse 
desde sus fuentes originales:

- **SDQ:** disponible en el paquete `HistData` de R
- **COW:** https://correlatesofwar.org/data-sets/cow-war/
- **UCDP:** https://ucdp.uu.se/downloads/
