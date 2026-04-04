library(tidyverse)
library(readr)
library(lubridate)
library(survival)
library(patchwork)


# Input ------------------------------------------------------------------
time_between_cow <- readRDS("data_gold/time_between_cow.rds")
time_between_sdq <- readRDS("data_gold/time_between_sdq.rds")
time_between_ucdp <- readRDS("data_gold/time_between_ucdp.rds")

# SDQ --------------------------------------------------------------------

## 1. Preparar datos ------------------------------------------------------
sdq <- time_between_sdq |>
  filter(!is.na(time_between)) |>
  filter(time_between > 0) |>
  mutate(
    phase = factor(phase),
    shock_1939 = factor(shock_1939),
    shock_1945 = factor(shock_1945)
  )


## Modelo Base ------------------------------------------------------------
# Exponencial
m_sdq_base <- survreg(
  Surv(time_between) ~ 1,
  data = sdq,
  dist = "exponential"
)
summary(m_sdq_base)
# - El modelo predice tiempo esperado entre eventos, no hazard directamente
# - El coeficiente β afecta el tiempo esperado multiplicándolo por exp(β)
# - El hazard ratio se obtiene como: HR = exp(−β)
# Porque hazard = 1 / tiempo esperado.
# Entonces:
# - β > 0 → tiempo esperado ↑ → hazard ↓
# - β < 0 → tiempo esperado ↓ → hazard ↑
# Y el hazard ratio te dice:
# “Cuántas veces más (o menos) probable es que ocurra el próximo conflicto en un momento dado.”
# - HR > 1 → mayor hazard → intervalos más cortos
# - HR < 1 → menor hazard → intervalos más largos
exp(2.5887) # = 13.3 meses entre conflictos (tiempo esperado)
# En SDQ, el tiempo típico entre conflictos interestatales es de 13 meses.

## 2. Modelo A — Fase -----------------------------------------------------
# Exponencial
m_sdq_phase <- survreg(
  Surv(time_between) ~ phase,
  data = sdq,
  dist = "exponential"
)
summary(m_sdq_phase)

# multipolaridad clásica beta = 0.1086.
# tiempo esperado entre conflictos se multiplica por exp(2.5887 + 0.1086) = 14.8 meses.
# Hazard ratio = exp(−0.1086) = 0.897 → hazard disminuye un 10.3%.
# No significativo.

## 3. Modelo B — Shocks (1939 y 1945) -------------------------------------
# Shock 1939
m_sdq_1939 <- survreg(
  Surv(time_between) ~ shock_1939,
  data = sdq,
  dist = "exponential"
)
summary(m_sdq_1939)
# No significativo.

# Shock 1945
m_sdq_1945 <- survreg(
  Surv(time_between) ~ shock_1945,
  data = sdq,
  dist = "exponential"
)
summary(m_sdq_1945)
# No significativo.

## Modelo C - Full --------------------------------------------------------
m_sdq_full <- survreg(
  Surv(time_between) ~ phase + shock_1945,
  data = sdq,
  dist = "exponential"
)
summary(m_sdq_full)
# No significativo.

## Weibull ----------------------------------------------------------------

m_sdq_weibull <- survreg(
  Surv(time_between) ~ phase + shock_1945,
  data = sdq,
  dist = "weibull"
)

summary(m_sdq_weibull)
AIC(m_sdq_base, m_sdq_phase, m_sdq_1939, m_sdq_1945, m_sdq_full, m_sdq_weibull)

lr_sdq <- 2 * (logLik(m_sdq_weibull) - logLik(m_sdq_full))
pchisq(lr_sdq, df = 1, lower.tail = FALSE)

# Weibull mejora muchísimo el ajuste (AIC) en SDQ, pero NO vuelve significativos a los predictores
# shape ≈ 2.1 → hazard fuertemente creciente
# Cuanto más tiempo pasa sin un conflicto interestatal, más probable es que ocurra uno.
# “El proceso generador de los intervalos en SDQ NO es exponencial. Tiene hazard creciente.”
# - Weibull mejora el ajuste por la forma de la distribución, no por los predictores
# - Los predictores (phase, shock) no explican el tiempo entre conflictos
# - Lo que explica la estructura temporal es el shape, no las covariables.
# El sistema de los conflictos en diadas clásico tiene un patrón temporal interno
# (hazard creciente), pero ese patrón no depende de las fases ni de los shocks.
# - el hazard aumenta con el tiempo
# - los intervalos largos son “peligrosos”
# - el sistema se vuelve más propenso a un conflicto cuanto más tiempo pasa desde el último

## Comparacion de modelos -------------------------------------------------
AIC(
  m_sdq_base,
  m_sdq_phase,
  m_sdq_1939,
  m_sdq_1945,
  m_sdq_full
)

# ✔ Resultado central
# Ningún predictor explica el tiempo entre conflictos.
# - phase → no significativo
# - shock_1939 → no significativo
# - shock_1945 → no significativo
# - modelo full → no mejora nada
# - AIC: el modelo base es prácticamente igual al resto
# el riesgo de un nuevo conflicto no cambia según fase ni shocks.

# COW --------------------------------------------------------------------
## 1. Preparar datos ------------------------------------------------------
cow <- time_between_cow |>
  filter(!is.na(time_between)) |>
  filter(time_between > 0) |>
  mutate(
    phase = factor(phase),
    shock_1939 = factor(shock_1939),
    shock_1945 = factor(shock_1945),
    rimland = factor(rimland)
  )


## Modelo base ------------------------------------------------------------
# Exponencial

m_cow_base <- survreg(
  Surv(time_between) ~ 1,
  data = cow,
  dist = "exponential"
)
summary(m_cow_base)

# Intercepto 1.3614 -> exp(1.3614) = 3.9 meses entre conflictos (tiempo esperado)

## 2. Modelo A — Fase -----------------------------------------------------
# Exponencial
m_cow_phase <- survreg(
  Surv(time_between) ~ phase,
  data = cow,
  dist = "exponential"
)
summary(m_cow_phase)

# phaseruptura_sistema beta = 0.55474 -> exp(1.3614 + 0.55474) = 6.8 meses entre conflictos (tiempo esperado)
# Hazard ratio = exp(−0.55474) = 0.574 → hazard disminuye un 42.6%.
# Intervalos más largos en fase de ruptura del sistema. Significativo.

# unipolaridad_tardia_multipolaridad_emergente beta = 0.54433 -> exp(1.3614 + 0.54433) = 6.7 meses entre conflictos (tiempo esperado)
# Hazard ratio = exp(−0.54433) = 0.580 → hazard disminuye un 42.0%.
# Intervalos más largos en fase de unipolaridad tardía / multipolaridad emergente. Significativo.

## 3. Modelo B — Shocks (1939 y 1945) -------------------------------------
# Shock 1939
m_cow_1939 <- survreg(
  Surv(time_between) ~ shock_1939,
  data = cow,
  dist = "exponential"
)
summary(m_cow_1939)
# No significativo.

# Shock 1945
m_cow_1945 <- survreg(
  Surv(time_between) ~ shock_1945,
  data = cow,
  dist = "exponential"
)
summary(m_cow_1945)
# No significativo.

## 4. Modelo C - Rimland --------------------------------------------------

# Explonencial
m_cow_rimland <- survreg(
  Surv(time_between) ~ rimland,
  data = cow,
  dist = "exponential"
)
summary(m_cow_rimland)

# Beta = 0.1658 -> exp(1.3614 + 0.1658) = 4.6 meses entre conflictos (tiempo esperado).
# Hazard ratio = exp(−0.1658) = 0.847 → hazard disminuye un 15.3%.
# Intervalos más largos en rimland. p ≈ 0.076 (tendencia/marginal).

## Modelo D - Full --------------------------------------------------------

m_cow_full <- survreg(
  Surv(time_between) ~ phase + rimland + shock_1945,
  data = cow,
  dist = "exponential"
)
summary(m_cow_full)

# Controlado por fase, rimland sigue teniendo coeficiente positivo (intervalos más largos), p ≈ 0.10 (tendencia/marginal).
# Controlado por otros factores, unipolaridad_tardia_multipolaridad_emergente sigue siendo significativo.

## Comparacion de modelos -----------------------------------------------------------
AIC(m_cow_base, m_cow_phase, m_cow_rimland, m_cow_1939, m_cow_1945, m_cow_full)

## Conclusiones -----------------------------------------------------------
# ✔ Resultado central
# La fase sí importa. Rimland casi importa. Los shocks no importan.
# - phaseruptura_sistema → coef > 0 → intervalos más largos → menor hazard
# - unipolaridad_tardia_multipolaridad_emergente → coef > 0 → intervalos más largos
# - rimland → coef > 0, p ≈ 0.076 → intervalos más largos (tendencia)
# - shocks → no significativos
# - modelo full mejora AIC respecto al base

# El hallazgo más interesante:
# rimland tiene un coeficiente positivo → intervalos más largos → menor hazard.

# Esto es lo opuesto a lo que uno esperaría teóricamente, pero tiene sentido empírico:
# - COW registra guerras interestatales grandes
# - rimland tiene muchos conflictos, pero no necesariamente guerras interestatales grandes
# - por eso el tiempo entre “grandes guerras” en rimland es mayor

# UCDP -------------------------------------------------------------------

## 1. Preparacion de datos ------------------------------------------------
ucdp <- time_between_ucdp |>
  filter(!is.na(time_between)) |>
  filter(time_between > 0) |>
  mutate(
    phase = factor(phase),
    shock_1945 = factor(shock_1945),
    shock_1991 = factor(shock_1991),
    rimland = factor(rimland)
  )


## Modelos base -----------------------------------------------------------
# Exponencial
m_ucdp_base <- survreg(
  Surv(time_between) ~ 1,
  data = ucdp,
  dist = "exponential"
)
summary(m_ucdp_base)
# Intercepto 1.4678 -> exp(1.4678) = 4.34 meses entre conflictos (tiempo esperado)

## 2. Modela A - Fase -----------------------------------------------------
# Exponencial
m_ucdp_phase <- survreg(
  Surv(time_between) ~ phase,
  data = ucdp,
  dist = "exponential"
)
summary(m_ucdp_phase)

# phaseruptura_sistema beta = 1.3228 -> exp(1.4678 + 1.3228) = 16.3 meses entre conflictos (tiempo esperado)
# Hazard ratio = exp(−1.3228) = 0.266 → hazard disminuye un 73.4%.
# - intervalos 10 meses más largos
# - muy significativo

## 3. Modelo B- Shocks 1945 y 1991 ------------------------------------------------
# Exponencial
m_ucdp_1945 <- survreg(
  Surv(time_between) ~ shock_1945,
  data = ucdp,
  dist = "exponential"
)
summary(m_ucdp_1945)
# Beta = -2.143 -> exp(1.4678 - 2.143) = 0.5 meses entre conflictos (tiempo esperado)
# Hazard ratio = exp(2.143) = 8.53 → hazard aumenta un 753%.
# - intervalos 3.8 meses más cortos
# - extremadamente significativo

# Exponencial
m_ucdp_1991 <- survreg(
  Surv(time_between) ~ shock_1991,
  data = ucdp,
  dist = "exponential"
)
summary(m_ucdp_1991)
# No significativo.

## 4. Modelo C - Rimland --------------------------------------------------
# Exponencial
m_ucdp_rimland <- survreg(
  Surv(time_between) ~ rimland,
  data = ucdp,
  dist = "exponential"
)
summary(m_ucdp_rimland)

# Beta = -0.2211 -> exp(1.4678 - 0.2211) = 3.48 meses entre conflictos (tiempo esperado).
# Hazard ratio = exp(0.2211) = 1.247 → hazard aumenta un 24.7%.
# Intervalos más cortos (0.9 meses) en rimland. p ≈ 0.11 (marginal/tendencia).

## Modelo D - Full --------------------------------------------------------

m_ucdp_full <- survreg(
  Surv(time_between) ~ phase + rimland + shock_1945 + shock_1991,
  data = ucdp,
  dist = "exponential"
)
summary(m_ucdp_full)
# Controlado por otros factores, shock_1945 sigue siendo extremadamente significativo.
# Controlado por otros factores, unipolaridad tardia sigue siendo significativo.
# Controlado por otros factores, rimland sigue siendo marginalmente significativo (p ≈ 0.10).

## Conclusiones -----------------------------------------------------------

AIC(
  m_ucdp_base,
  m_ucdp_phase,
  m_ucdp_rimland,
  m_ucdp_1945,
  m_ucdp_1991,
  m_ucdp_full
)

# UCDP captura conflictos intraestatales, que tienen una dinámica completamente distinta:
# - shock_1945 reduce drásticamente el tiempo entre conflictos
# → hazard mucho mayor
# → posguerra = proliferación de conflictos internos
# - rimland → intervalos más cortos
# → hazard mayor
# → zonas geopolíticas sensibles tienen más recurrencia
# - fases del sistema → afectan el ritmo de conflictos internos
# → ruptura del sistema = intervalos largos (pocos conflictos internos)
# → pos-2001 = intervalos más largos (menos guerras civiles grandes)

# Revision de supuestos  ----------------------------------------------

##  1. Distribución básica de intervalos (asimetría) ----------------------

p_sdq <- ggplot(sdq, aes(time_between)) +
  geom_histogram(bins = 30) +
  ggtitle("SDQ")
p_cow <- ggplot(cow, aes(time_between)) +
  geom_histogram(bins = 30) +
  ggtitle("COW")
p_ucdp <- ggplot(ucdp, aes(time_between)) +
  geom_histogram(bins = 30) +
  ggtitle("UCDP")

p_sdq / p_cow / p_ucdp


## 2. Tendencia temporal (hazard constante ≈ sin tendencia) ---------------

ggplot(sdq, aes(year, time_between)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  ggtitle("SDQ — Intervalos a lo largo del tiempo")

ggplot(cow, aes(year, time_between)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  ggtitle("COW — Intervalos a lo largo del tiempo")

ggplot(ucdp, aes(year, time_between)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  ggtitle("UCDP — Intervalos a lo largo del tiempo")


## 3. Correlación año–intervalo (esperado ≈ 0 si hazard constante) --------

cor(sdq$year, sdq$time_between, use = "complete.obs")
cor(cow$year, cow$time_between, use = "complete.obs")
cor(ucdp$year, ucdp$time_between, use = "complete.obs")


##  4. Comparación exponencial vs Weibull (shape ≈ 1) ---------------------

# SDQ
exp_sdq <- survreg(Surv(time_between) ~ 1, data = sdq, dist = "exponential")
wei_sdq <- survreg(Surv(time_between) ~ 1, data = sdq, dist = "weibull")

# COW
exp_cow <- survreg(Surv(time_between) ~ 1, data = cow, dist = "exponential")
wei_cow <- survreg(Surv(time_between) ~ 1, data = cow, dist = "weibull")

# UCDP
exp_ucdp <- survreg(Surv(time_between) ~ 1, data = ucdp, dist = "exponential")
wei_ucdp <- survreg(Surv(time_between) ~ 1, data = ucdp, dist = "weibull")

summary(wei_sdq)
summary(wei_cow)
summary(wei_ucdp)


## 5. Extraer el parámetro de forma (shape) --------------------------------
shape_sdq <- 1 / wei_sdq$scale
shape_cow <- 1 / wei_cow$scale
shape_ucdp <- 1 / wei_ucdp$scale

shape_sdq
shape_cow
shape_ucdp


## 6. Comparación formal de modelos (LR test) -----------------------------

# SDQ
lr_sdq <- 2 * (logLik(wei_sdq) - logLik(exp_sdq))
pchisq(lr_sdq, df = 1, lower.tail = FALSE)

# COW
lr_cow <- 2 * (logLik(wei_cow) - logLik(exp_cow))
pchisq(lr_cow, df = 1, lower.tail = FALSE)

# UCDP
lr_ucdp <- 2 * (logLik(wei_ucdp) - logLik(exp_ucdp))
pchisq(lr_ucdp, df = 1, lower.tail = FALSE)


##  7. Comparación AIC ----------------------------------------------------

AIC(exp_sdq, wei_sdq)
AIC(exp_cow, wei_cow)
AIC(exp_ucdp, wei_ucdp)
# Si AIC(exponencial) ≈ AIC(weibull) → exponencial es suficiente

## 8. Gráfico de supervivencia empírica vs exponencial teórica ------------
library(survminer)

# SDQ
fit_sdq <- survfit(Surv(time_between) ~ 1, data = sdq)
ggsurvplot(fit_sdq) + ggtitle("SDQ — Curva empírica de supervivencia")

# COW
fit_cow <- survfit(Surv(time_between) ~ 1, data = cow)
ggsurvplot(fit_cow) + ggtitle("COW — Curva empírica de supervivencia")

# UCDP
fit_ucdp <- survfit(Surv(time_between) ~ 1, data = ucdp)
ggsurvplot(fit_ucdp) + ggtitle("UCDP — Curva empírica de supervivencia")

# La exponencial debería verse como una curva suave, convexa, sin quiebres.

## 9. Gráfico log(-log(S(t))) vs log(t) -----------------------------------

# Este es el test visual clásico para Weibull:
# - Si es línea recta → Weibull
# - Si es curva horizontal → Exponencial (shape = 1)

plot(fit_sdq, fun = "cloglog", main = "SDQ — log(-log(S)) vs log(t)")
plot(fit_cow, fun = "cloglog", main = "COW — log(-log(S)) vs log(t)")
plot(fit_ucdp, fun = "cloglog", main = "UCDP — log(-log(S)) vs log(t)")
