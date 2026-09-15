# StatsWars

## Análisis probabilístico de conflictos armados

Scripts R y documentos Quarto utilizados en la tesis de maestría:

> *Análisis probabilístico de los conflictos armados*  
> *Modelos de conteo, supervivencia y regresión*

Tesis presentada para obtener el título de Magíster en Generación y
Análisis de Información Estadística — Universidad Nacional de Tres de
Febrero (UNTREF).

**Autor:** Nicolás Stumberger  
**Director:** Jorge E. Sagula

---

## Descripción

La literatura sobre relaciones internacionales asocia el orden institucional
surgido en 1945 con buena parte de la estabilidad interestatal de la posguerra.
Esa lectura convive con explicaciones basadas en la distribución de poder del
sistema y carece de verificación empírica sistemática. Este trabajo evalúa si
ese orden produjo efectos estadísticamente detectables sobre la dinámica del
conflicto armado, una vez controlados los factores estructurales y geográficos.

El conflicto se analiza en cuatro dimensiones —**frecuencia** anual de
ocurrencia, **intervalo** entre conflictos consecutivos, **duración** e
**intensidad**— y el análisis se replica en tres fuentes con criterios, unidades
y coberturas distintas, como estrategia de triangulación cuantitativa.

**Predictores:** fases del sistema internacional, shocks históricos de 1815,
1914, 1939, 1945, 1991 y 2001, y pertenencia al Rimland.

**Datasets utilizados:**
- **SDQ** — The Statistics of Deadly Quarrels (Richardson, 1960): 1807–1949
- **COW** — Correlates of War Project: 1816–2014
- **UCDP** — Uppsala Conflict Data Program: 1939–2024

### El intervalo como verificación interna

Si la frecuencia anual sigue una distribución de Poisson de tasa λ, los tiempos
entre eventos consecutivos deben seguir una Exponencial de media 1/λ: la
dimensión continua contrasta a la discreta. Esa correspondencia se verifica
mediante la ley de los grandes números en las tres fuentes, y mediante el
teorema central del límite en COW y UCDP. De ahí que el Anexo 2 incluya
material que no aparece en los demás.

### Resultados principales

- El shock de **1945** no produce un efecto significativo y robusto sobre
  ninguna dimensión. En COW, la única fuente con cobertura balanceada a ambos
  lados del umbral, el efecto es prácticamente nulo (irr = 1,01).
- El período posbipolar iniciado en **1991** sí resulta relevante y convergente:
  en UCDP el intervalo se triplica (tr = 3,08), la duración se reduce un 70%
  (tr = 0,30) y la probabilidad de guerra mayor cae un 58% (or = 0,417).
  La frecuencia anual, en cambio, no registra cambios atribuibles a esta fase.
- La pertenencia al **Rimland** duplica la tasa de conflictos en COW
  (irr = 2,30), acorta los intervalos en UCDP y eleva la intensidad, sin
  afectar la duración.

La disociación entre 1945 y 1991 resulta más compatible con la teoría de la
estabilidad hegemónica que con la lectura institucionalista del orden de
posguerra. Si el período de estabilidad observado corresponde a una Pax, los
datos la ubican en 1991 y no en 1945. La tesis propone denominarla
**Pax Unipolar**.

---

## Estructura del repositorio

### Preparación de datos

| Script | Descripción |
|--------|-------------|
| `StatsWars_00_Metadata.R` | Metadatos y parámetros comunes |
| `StatsWars_00_DataPrep_SDQ.R` | Limpieza y construcción de variables — SDQ |
| `StatsWars_00_DataPrep_COW.R` | Limpieza y construcción de variables — COW |
| `StatsWars_00_DataPrep_UCDP.R` | Limpieza y construcción de variables — UCDP |
| `StatsWars_01_DataPrep_ForModel.R` | Integración y preparación final para modelos |

### Análisis exploratorio y figuras

| Script | Descripción |
|--------|-------------|
| `StatsWars_02_EDA.R` | Análisis exploratorio de datos — tres fuentes |
| `StatsWars_05_Graficos.R` | Generación de las figuras de la tesis |

### Modelos estadísticos

| Script | Descripción |
|--------|-------------|
| `StatsWars_03_Modelos_Frecuencia.R` | Conteo — Poisson, quasi-Poisson y binomial negativa |
| `StatsWars_03_Modelos_TiempoEntre.R` | Supervivencia, intervalo — exponencial, Weibull y Cox |
| `StatsWars_03_Modelos_Duracion.R` | Supervivencia, duración — exponencial, Weibull y Cox |
| `StatsWars_03_Modelos_Intensidad_log_deaths.R` | Intensidad — regresión lineal (escala log) y logística |

### Anexos técnicos (Quarto)

| Archivo | Descripción |
|---------|-------------|
| `StatsWars_04_Anexo1_Frecuencia.qmd` | Anexo 1 — Frecuencia: Poisson, quasi-Poisson y binomial negativa |
| `StatsWars_04_Anexo2_Intervalo.qmd` | Anexo 2 — Intervalo: exponencial, Weibull y Cox; verificación LGN y TCL |
| `StatsWars_04_Anexo3_Duracion.qmd` | Anexo 3 — Duración: exponencial, Weibull y Cox |
| `StatsWars_04_Anexo4_Intensidad.qmd` | Anexo 4 — Intensidad: regresión lineal y logística |

---

## Requisitos

R 4.x con los siguientes paquetes principales:

```r
install.packages(c(
  "survival", "survminer", "flexsurv", "MASS", "broom",
  "ggplot2", "dplyr", "tidyr", "HistData",
  "lmtest", "car", "patchwork",
  "flextable", "officer", "svglite"
))
```

Los anexos se renderizan con Quarto a formato Word.

---

## Fuentes de datos

Los datasets no están incluidos en este repositorio y deben descargarse
desde sus fuentes originales:

- **SDQ:** disponible en el paquete `HistData` de R
- **COW:** https://correlatesofwar.org/data-sets/cow-war/
- **UCDP:** https://ucdp.uu.se/downloads/