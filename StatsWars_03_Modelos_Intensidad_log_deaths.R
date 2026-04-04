library(tidyverse)
library(readr)
library(lubridate)
library(survival)

# Input ------------------------------------------------------------------
anu_frec_cow <- readRDS("data_gold/anu_frec_cow.rds")
anu_frec_sdq <- readRDS("data_gold/anu_frec_sdq.rds")
anu_frec_ucdp <- readRDS("data_gold/anu_frec_ucdp.rds")
anu_frec_ucdp_War <- readRDS("data_gold/anu_frec_ucdp_War.rds")

conf_durint_cow <- readRDS("data_gold/conf_durint_cow.rds")
conf_durint_sdq <- readRDS("data_gold/conf_durint_sdq.rds")
conf_durint_ucdp <- readRDS("data_gold/conf_durint_ucdp.rds")

# Preparacion de datasets ------------------------------------------------
# La variable dependiente es log_deaths:
# - double
# Ambas deben ser:
# - positivas
# - sin ceros problemáticos (si hay ceros, se filtran)

# SDQ
sdq <- conf_durint_sdq |>
  mutate(log_deaths = as.numeric(log_deaths)) |>
  filter(!is.na(log_deaths))

# COW
cow <- conf_durint_cow |>
  mutate(log_deaths = as.numeric(log_deaths)) |>
  filter(!is.na(log_deaths))

# UCDP
ucdp <- conf_durint_ucdp |>
  mutate(
    intensity_level = factor(
      intensity_level,
      levels = c("Minor", "War"),
      ordered = TRUE
    )
  )

# Revision de supuestos --------------------------------------------------

## SUPUESTOS PARA MODELOS LINEALES (LM) -----------------------------------
# Solo aplican a SDQ y COW

m_lm_sdq <- lm(log_deaths ~ 1, data = sdq)
m_lm_cow <- lm(log_deaths ~ 1, data = cow)


### ✔ Normalidad de residuos -----------------------------------------------

qqnorm(residuals(m_lm_sdq))
qqline(residuals(m_lm_sdq))
# Parece una regresion logistica, muchos puntos paralelos al eje X.
# Pero tambien varios (un tercio) siguiendo la diagonal
shapiro.test(residuals(m_lm_sdq)) # opcional
# H0: Los residuos son normales.
# Claramente no normal

qqnorm(residuals(m_lm_cow))
qqline(residuals(m_lm_cow))
# es lineal pero ligeramente pandeado (forma de U abierta) sobre la diagonal.
shapiro.test(residuals(m_lm_cow)) # opcional
# H0: Los residuos son normales.
# Claramente no normal

# Cómo interpretarlo
# - No estás buscando normalidad perfecta.
# - Lo que querés es:
# - que la distribución no sea grotescamente asimétrica,
# - que el log haya “domado” lo peor de la cola.

# sperable y no invalida el LM sobre log_deaths.

### ✔ Homocedasticidad -----------------------------------------------------

# Ejemplo con phase
m_lm_phase_sdq <- lm(log_deaths ~ phase, data = sdq)
m_lm_phase_cow <- lm(log_deaths ~ phase, data = cow)

# Gráfico de residuos vs ajustados
plot(m_lm_phase_sdq, which = 1)
plot(m_lm_phase_cow, which = 1)

# Test de Breusch–Pagan
library(lmtest)
bptest(m_lm_phase_sdq)
bptest(m_lm_phase_cow)

# Qué esperar
# - En el gráfico:
# - nube de puntos más o menos horizontal,
# - puede haber algo de abanico, pero no brutal.
# - En BP:
# - si p > 0.05 → no evidencia fuerte de heterocedasticidad,
# - si p < 0.05 → hay heterocedasticidad, pero no es el fin del mundo.
# Cómo interpretarlo
# - Si BP no es significativo → perfecto, seguís.
# - Si BP es significativo:
# - podés mencionarlo,
# - pero no estás obligado a cambiar de familia.
# - LM sobre log_deaths sigue siendo defendible como aproximación.

# Heterocedasticidad esperable. No invalida

### ✔ Influencia -------------------------------------------------------------

# SDQ
cooks_sdq <- cooks.distance(m_lm_phase_sdq)
plot(cooks_sdq, main = "Cook's distance SDQ")
abline(h = 4 / length(cooks_sdq), col = "red", lty = 2)
which(cooks_sdq > 4 / length(cooks_sdq))

# COW
cooks_cow <- cooks.distance(m_lm_phase_cow)
plot(cooks_cow, main = "Cook's distance COW")
abline(h = 4 / length(cooks_cow), col = "red", lty = 2)
which(cooks_cow > 4 / length(cooks_cow))

# Qué esperar
# - Vas a ver algunos puntos influyentes (sobre todo en COW).
# - Son las guerras enormes: eso es real, no un error.
# Cómo interpretarlo
# - No es un problema que haya casos influyentes; lo sería si fueran errores.
# - Podés hacer un chequeo rápido:
# - correr el modelo con y sin esos casos
# - ver si los signos y significancias cambian

# Muchos casos influyentes. Esperable

### ✔ Multicolinealidad ------------------------------------------------------
library(car)

# Ejemplo COW full
m_lm_full_cow <- lm(log_deaths ~ phase + rimland + shock_1945, data = cow)
vif(m_lm_full_cow)

# Ejemplo SDQ full (sin rimland)
m_lm_full_sdq <- lm(log_deaths ~ phase + shock_1945, data = sdq)
vif(m_lm_full_sdq)

# Qué esperar
# - VIF < 5 → todo bien.
# - VIF entre 5 y 10 → colinealidad moderada, pero manejable.

# ✔ Interpretación correcta
# - No hay colinealidad severa.
# - shock_1945 en COW tiene algo de correlación temporal con phase, pero es manejable.
# - El modelo full es válido.

## SUPUESTOS PARA REGRESION LOGISTICA ---------------------------------------
# (UCDP)

m_ucdp_logit <- glm(
  intensity_level ~ phase + shock_1945 + shock_1991 + rimland,
  data = ucdp,
  family = binomial
)


### ✔ Linealidad del logit --------------------------------

# Solo aplica si tenés predictores continuos.
# En tu caso:
# - phase → categórica
# - rimland → binaria
# - shock_1945 / shock_1991 → binaria
# → No aplica.

### ✔ Independencia de observaciones ------------------------------------------------------
# UCDP no tiene clustering temporal por conflicto (cada fila es un conflicto distinto).
# → Supuesto cumplido.

### ✔ Ausencia de multicolinealidad ------------------------------------------------------

library(car)
vif(m_ucdp_logit)

### ✔ Proporcionalidad de odds ------------------------------------------------------

# Solo aplica a modelos ordinales con ≥ 3 categorías

### ✔ Incluencia ------------------------------------------------------

plot(cooks.distance(m_ucdp_logit))

# SDQ --------------------------------------------------------------------

## Modelo base ------------------------------------------------------------
m_base_sdq <- lm(log_deaths ~ 1, data = sdq)
summary(m_base_sdq)
# Como modelo, sin predictores.

## Modelo Fase ------------------------------------------------------------

m_phase_sdq <- lm(log_deaths ~ phase, data = sdq)
summary(m_phase_sdq)

# ✔ Interpretación clave
# - Solo una fase es significativa: ruptura del sistema.
# - Su coeficiente es 2.51, lo que implica:
# \exp (2.51)\approx 12.3
# → Los conflictos en la fase de ruptura del sistema son 12 veces más intensos que en la fase de referencia.

# SDQ captura un salto enorme de intensidad en la fase de ruptura del sistema,
# pero no diferencias claras entre las fases anteriores.

# ✔ R² = 0.51
# Esto es altísimo para datos de conflicto.
# La fase explica más de la mitad de la variación en intensidad.

## Modelo Shocks ----------------------------------------------------------

m_1945_sdq <- lm(log_deaths ~ shock_1945, data = sdq)
summary(m_1945_sdq)

# El coeficiente es negativo y marginalmente significativo (p = 0.047).
# Después de 1945, los conflictos SDQ son 39% menos intensos
# El R² es 0.005 → el efecto es estadísticamente significativo pero sustantivamente débil.
# “Hay una reducción moderada en intensidad después de 1945, pero el shock no explica casi nada comparado con la fase.”

## Modelo full ------------------------------------------------------------

m_full_sdq <- lm(log_deaths ~ phase + shock_1945, data = sdq)
summary(m_full_sdq)

# El shock 1945 domina completamente la estructura temporal
# Después de 1945, la intensidad cae un 93%.
# Las fases pierden su interpretación original.
# Fase y shock_1945 están capturando la misma estructura temporal, pero shock_1945 lo hace de manera más directa y más potente.

# “Cuando se controla por el shock de 1945, las diferencias entre fases desaparecen. Esto indica
# que la caída en la intensidad está asociada principalmente al quiebre estructural de la posguerra,
# más que a las fases del sistema internacional.”

# ✔ R² = 0.55
# El modelo full explica más de la mitad de la variación en intensidad.
# Es el mejor modelo.

## Comparacion modelos ----------------------------------------------------

AIC(m_base_sdq, m_phase_sdq, m_1945_sdq, m_full_sdq)

# COW --------------------------------------------------------------------

## Modelo base ------------------------------------------------------------
m_base_cow <- lm(log_deaths ~ 1, data = cow)
summary(m_base_cow)

## Modelo Fase ------------------------------------------------------------
m_phase_cow <- lm(log_deaths ~ phase, data = cow)
summary(m_phase_cow)

#  Multipolaridad clásica (≈ 1815–1914) Conflictos ~32% menos intensos que en bipolaridad.
# Ruptura del sistema (1914–1945) Conflictos ~3 veces más intensos.
#  Unipolaridad tardía tendencia a menor intensidad, pero no significativa
# ✔ R² = 0.044
# Muy bajo, pero esperable

## Modelo Shock -----------------------------------------------------------
m_1945_cow <- lm(log_deaths ~ shock_1945, data = cow)
summary(m_1945_cow)

# ✔ Interpretación
# - No significativo (p = 0.11)
# - Efecto pequeño y sin dirección clara

## Modelo Rimland ---------------------------------------------------------
m_rimland_cow <- lm(log_deaths ~ rimland, data = cow)
summary(m_rimland_cow)
# Los conflictos en zonas rimland son ~40% más intensos
# ✔ R² = 0.009
# → efecto estadístico, pero no estructural.

# “El rimland muestra un efecto positivo y significativo sobre la intensidad,
# aunque explica una proporción muy pequeña de la variación.
# Esto sugiere que los conflictos en zonas geopolíticamente sensibles pueden
# ser más severos, pero no de manera sistemática.”

## Modelo full ------------------------------------------------------------

m_full_cow <- lm(log_deaths ~ phase + rimland + shock_1945, data = cow)
summary(m_full_cow)

# Ruptura del sistema sigue siendo el predictor dominante
# Los conflictos de 1914–1945 son ~14 veces más intensos
#  Rimland sigue siendo significativo ~34% más intensos
# Shock 1945 se vuelve significativo.
# Los conflictos posteriores a 1945 son 6 veces más intensos. Contraintuitivo.

# ✔ R² = 0.058
# Sigue siendo bajo, pero mejora respecto a modelos simples

## Comparacion modelos ----------------------------------------------------

AIC(m_base_cow, m_phase_cow, m_rimland_cow, m_1945_cow, m_full_cow)

# UCDP -------------------------------------------------------------------

## Modelo base ------------------------------------------------------------

m_base_ucdp <- glm(intensity_level ~ 1, data = ucdp, family = binomial)
summary(m_base_ucdp)

# Interpretación:
# - Esto es el log‑odds de que un conflicto sea War.
# - Convertido a probabilidad

# En UCDP, solo ~19% de los conflictos son War (≥1000 muertes)

## Modelo Fase ------------------------------------------------------------

m_phase_ucdp <- glm(intensity_level ~ phase, data = ucdp, family = binomial)

summary(m_phase_ucdp)

# - Ninguna fase es significativa al 5%
# - La única con una tendencia marginal es unipolaridad tardía / multipolaridad emergente (p ≈ 0.058), indicando menor probabilidad de War
# La deviance apenas baja de 292.99 → 288.17.

## Modelo Rimland ---------------------------------------------------------

m_rimland_ucdp <- glm(intensity_level ~ rimland, data = ucdp, family = binomial)

summary(m_rimland_ucdp)

# Interpretación:
# - No significativo.
# - Rimland no predice si un conflicto es Minor o War

# Modelos Shocks ---------------------------------------------------------

m_1945_ucdp <- glm(intensity_level ~ shock_1945, data = ucdp, family = binomial)

summary(m_1945_ucdp)

# nterpretación:
# - No significativo.
# - No hay evidencia de un quiebre en intensidad en 1945

m_1991_ucdp <- glm(intensity_level ~ shock_1991, data = ucdp, family = binomial)

summary(m_1991_ucdp)
# - único predictor significativo en UCDP
# Convertido a odds ratio:
# Después de 1991, la probabilidad de que un conflicto sea War cae ~58%
# ✔ AIC = 290.03
# Es el mejor modelo de todos.

## Comparacion de modelos -------------------------------------------------

AIC(m_base_ucdp, m_phase_ucdp, m_rimland_ucdp, m_1945_ucdp, m_1991_ucdp)
# Shock 1991 es el único predictor que mejora sustancialmente el modelo.
