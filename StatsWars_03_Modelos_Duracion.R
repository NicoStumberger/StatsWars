library(tidyverse)
library(readr)
library(lubridate)
library(survival)
library(survminer)

# Input ------------------------------------------------------------------
anu_frec_cow <- readRDS("data_gold/anu_frec_cow.rds")
anu_frec_sdq <- readRDS("data_gold/anu_frec_sdq.rds")
anu_frec_ucdp <- readRDS("data_gold/anu_frec_ucdp.rds")
anu_frec_ucdp_War <- readRDS("data_gold/anu_frec_ucdp_War.rds")

conf_durint_cow <- readRDS("data_gold/conf_durint_cow.rds")
conf_durint_sdq <- readRDS("data_gold/conf_durint_sdq.rds")
conf_durint_ucdp <- readRDS("data_gold/conf_durint_ucdp.rds")


# Revision de supuestos --------------------------------------------------

## a) Tiempos positivos ---------------------------------------------------
# - Exponencial → NO acepta tiempos 0
# - Weibull → NO acepta tiempos 0
# - Cox → acepta 0, pero no es recomendable

## b) Forma del hazard (Exponencial vs Weibull) ----------------------------

# ✔ Qué exige cada modelo
# - Exponencial: hazard constante en el tiempo
# - Weibull: hazard monótono (creciente o decreciente)
# - Cox: hazard no especificado, pero requiere riesgos proporcionale
# Se testea luego de correr el modelo

# 1. Parámetro scale en Weibull
# En survreg():
# - scale > 1 → hazard creciente
# - scale < 1 → hazard decreciente
# - scale = 1 → equivalente a Exponencial

# 2. Comparación de AIC entre Exponencial y Weibull

# 3. Inspección visual de curvas Kaplan–Meier
fit_cow <- survfit(Surv(months) ~ phase, data = conf_durint_cow)
ggsurvplot(fit_cow)
fit_sdq <- survfit(Surv(months) ~ phase, data = conf_durint_sdq)
ggsurvplot(fit_sdq)
fit_ucdp <- survfit(Surv(months) ~ phase, data = conf_durint_ucdp)
ggsurvplot(fit_ucdp)

# - curvas separadas → Exponencial falla
# - curvas que se cruzan → Cox puede violar PH
# - colas largas → Exponencial no sirve

##  c) Ausencia de censura -------------------------------------------------
# ✔ Qué exige el modelo
# Los modelos de supervivencia permiten censura, pero si no la hay:
# - los modelos son más simples
# - la interpretación es más directa
# - no hay que especificar Surv(time, event), solo Surv(time)

# Tus datasets ya están reconstruidos a nivel de conflicto completo:
# - todos los conflictos tienen fecha de inicio y fin
# - no hay conflictos “abiertos”
# - no hay censura a derecha

# SDQ --------------------------------------------------------------------

## 1. Preparar datos ------------------------------------------------------
sdq <- conf_durint_sdq |>
  filter(!is.na(months)) |>
  mutate(
    phase = factor(phase),
    shock_1939 = factor(shock_1939),
    shock_1945 = factor(shock_1945)
  )


## Modelo Base ------------------------------------------------------------
# Exponencial
m_exp_base_sdq <- survreg(Surv(months) ~ 1, data = sdq, dist = "exponential")
summary(m_exp_base_sdq)
# Qué mirar:
# - (Intercept) → log de la duración media
# - Loglik(intercept only) → lo usaremos para comparar con modelos con fase
# - AIC(m_exp_base_sdq) → para tabla comparativa

# La duración media estimada por el modelo Exponencial es ~27 meses.
# Haciendo la cuenta:
# exp(3.305) = 27.25
# Eso significa:
# - no hay covariables
# - solo se estima el parámetro de escala temporal
# - el hazard es constante: h(t)=\lambda
# En la parametrización de survreg():
# Intercept=log(media)
# Porque en la distribución Exponencial:
# media=1\lambda

# 2. Modelo base Weibull (sin covariables)
m_wei_base_sdq <- survreg(Surv(months) ~ 1, data = sdq, dist = "weibull")
summary(m_wei_base_sdq)
# Qué mirar:
# - (Intercept) → duración media en escala Weibull
# - Log(scale) → forma del hazard
# - 0 → hazard creciente
# - < 0 → hazard decreciente
# - AIC(m_wei_base_sdq) → para comparar con Exponencial

# Es decir:
# - no hay covariables
# - solo se estima la duración típica y la forma del hazard
# - Weibull permite hazard creciente o decreciente
# - es más flexible que Exponencia

# En survreg() con Weibull, el intercepto es:
# Intercept=log(mediana)
# ⚠️ Ojo:
# En Weibull, el intercepto NO es el log de la media, sino del tiempo característico (location parameter), que se interpreta como una mediana transformada.
# exp(3.1651) = ~24 meses

# log(scale) es el parametro clave de Weibull
# - scale > 1 → hazard creciente
# - scale < 1 → hazard decreciente
# - scale = 1 → equivalente a Exponencial
# scale = 1.25
# El hazard aumenta con el tiempo.
# Cuanto más dura un conflicto, más probable es que termine.
# Esto es típico de conflictos interestatales y diádicos:
# los conflictos largos tienden a resolverse porque se agotan recursos, presión internacional, desgaste, etc.

# 3. Modelo base Cox (sin covariables)
m_cox_base_sdq <- coxph(Surv(months) ~ 1, data = sdq)
summary(m_cox_base_sdq)
# Qué mirar:
# - loglik → log-likelihood del modelo vacío
# - AIC(m_cox_base_sdq) → para comparar con Exponencial y Weibull
# - Concordance → aunque sin covariables no es muy informativo

AIC(m_exp_base_sdq, m_wei_base_sdq, m_cox_base_sdq)

# Conclusión:
# Weibull es el baseline correcto para SDQ.
# Exponencial es demasiado restrictivo.
# Cox vacío no captura la estructura temporal.

## 2. Modelo A — Fase -----------------------------------------------------
# Exponencial
m_exp_phase_sdq <- survreg(
  Surv(months) ~ phase,
  data = sdq,
  dist = "exponential"
)
summary(m_exp_phase_sdq)

# Es un modelo Exponencial paramétrico:
# - hazard constante en el tiempo
# - parametrización en log‑tiempo
# - coeficientes interpretados como cambios en el log de la duración esperada
# Este modelo es el más restrictivo de supervivencia.

# Interpretación
# En la fase de referencia (bipolaridad - porque es la primera alfabéticamente o la referencia que quedó),
# el modelo Exponencial estima una duración media de ~9.8 meses (exp(2.28) = ~9.8)
# - coeficiente positivo → aumenta la duración esperada
# - coeficiente negativo → reduce la duración esperad
# phaseentreguerras = 0.85 -> exp(0.85) = 2.35 (veces más que en la bipolaridad)
# 9.8 * 2.35 = 23.0 meses
# phasemultipolaridad_clasica = 0.92 -> exp(0.92) = 2.52
# phaseruptura_sistema = 1.14-> exp(1.14) = 3.14
# 9.8 * 1.14 = 30.7 meses

# Chi‑square = 29.03
# Es un test de razón de verosimilitud:
# - H0: todos los coeficientes = 0
# - H1: al menos uno ≠ 0
# p = 2.2e‑06 → rechazamos H0
# La fase del sistema internacional explica significativamente la duración de los conflictos en SDQ.

# Weibull
m_wei_phase_sdq <- survreg(Surv(months) ~ phase, data = sdq, dist = "weibull")
summary(m_wei_phase_sdq)


# Cox
m_cox_phase_sdq <- coxph(Surv(months) ~ phase, data = sdq)
summary(m_cox_phase_sdq)
# Interpretación es distinta porque Cox trabaja en hazard ratios, no en duración esperada.
# El modelo de Cox:
# - no asume forma del hazard
# - estima razones de riesgo (hazard ratios)
# - interpreta coeficientes como cambios en la probabilidad instantánea de terminar el conflict
# Clave:
# - coeficiente negativo → hazard ratio < 1 → menor riesgo → conflictos más largos
# - coeficiente positivo → hazard ratio > 1 → mayor riesgo → conflictos más corto

# Todas las fases tienen menor hazard que la fase de referencia.
# Es decir: los conflictos duran más en todas estas fases.
# Interpretacion:
# Los conflictos en entreguerras tienen un 46.6% menos de probabilidad de terminar en cada instante que en la fase de referencia.
# (1 – 0.5344 = 0.466) = duración más larga.
# Los conflictos multipolaridad_clasica duran más: el riesgo de finalización es 49.3% menor
# ruptura_sistema es la fase con conflictos más largos: el riesgo de finalización es 58.1% menor
# Todos los intervalos están por debajo de 1: Los efectos son estadísticamente significativos y robustos
# Tests globales del modelo
# - Likelihood ratio test = 17.79, p = 5e‑04
# - Wald test = 20.22, p = 2e‑04
# - Score test = 20.93, p = 1e‑04
# Interpretacion:
# La fase del sistema internacional explica significativamente la duración de los conflictos en SDQ.

# La concordancia mide capacidad predictiva:
# - 0.5 = azar
# - 0.6 = aceptable
# - 0.7 = buena
# - 0.8+ = excelente
# 0.541 es baja, pero normal en conflictos armados (alta variabilidad)
# El modelo distingue ligeramente entre conflictos más y menos duraderos, pero la predicción individual es limitada.

# Test de riesgos proporcionales
cox.zph(m_cox_phase_sdq)
# El test de Schoenfeld residuals evalúa el supuesto fundamental del modelo Cox:
# Los riesgos proporcionales (PH): el efecto de cada covariable debe ser constante en el tiempo.
# Si este supuesto se viola, el modelo Cox no es válido para interpretar hazard ratios como constantes.

# H0: Es proporcional
# H1: no es proporional.
# El efecto de la fase NO es proporcional en el tiempo.
# El modelo Cox viola el supuesto de riesgos proporcionales.
# La violación es general, no solo en una categoría.
# Los HR no son constantes en el tiempo, por lo que:
# - no pueden interpretarse como efectos fijos
# - no pueden compararse directamente entre fases
# - no deben usarse como resultado principal

## 3. Modelo B — Shocks (1939 y 1945) -------------------------------------
# Shock 1939
m_exp_39_sdq <- survreg(
  Surv(months) ~ shock_1939,
  data = sdq,
  dist = "exponential"
)
summary(m_exp_39_sdq)
# Después de 1939, los conflictos duran aproximadamente un 16% menos que antes de 1939.
# exp(-0.1704) = 0.843 (el post shock dura un 84% de la base, q es lo mismo q un 16% menos)
# (1 – 0.843 = 0.157)

m_wei_39_sdq <- survreg(Surv(months) ~ shock_1939, data = sdq, dist = "weibull")
summary(m_wei_39_sdq)
# Antes de 1939, la duración típica de un conflicto diádico en SDQ es ~24 meses.
# Después de 1939, los conflictos duran ~11% menos que antes de 1939.
# Pero no es significativo
# El hazard es creciente.
# Cuanto más dura un conflicto, más probable es que termine.

m_cox_39_sdq <- coxph(Surv(months) ~ shock_1939, data = sdq)
summary(m_cox_39_sdq)
# El efecto podría ser una reducción del 15% o un aumento del 24%.
# El modelo no puede descartar nada.

cox.zph(m_cox_39_sdq)

# Shock 1945
m_exp_45_sdq <- survreg(
  Surv(months) ~ shock_1945,
  data = sdq,
  dist = "exponential"
)
summary(m_exp_45_sdq)
# Antes de 1945, la duración media estimada de un conflicto diádico en SDQ es ~28 meses.
# Después de 1945, los conflictos duran solo el 31% de lo que duraban antes.
# Es decir: una reducción del 69% en la duración.
# 28.3 * 0.314 = 8.9 meses
# Es significativo

m_wei_45_sdq <- survreg(Surv(months) ~ shock_1945, data = sdq, dist = "weibull")
summary(m_wei_45_sdq)
# Antes de 1945, la duración típica de un conflicto diádico en SDQ es ~25 meses
# Después de 1945, los conflictos duran solo el 33% de lo que duraban antes.
# Es decir: una reducción del 67% en la duración.
# Es significativo.
# El hazard es creciente: cuanto más dura un conflicto, más probable es que termine.

m_cox_45_sdq <- coxph(Surv(months) ~ shock_1945, data = sdq)
summary(m_cox_45_sdq)
# En Cox:
# - coeficiente positivo → mayor hazard
# - mayor hazard → conflictos más corto
# Después de 1945, la probabilidad instantánea de que un conflicto termine es 2.33 veces mayor que antes de 1945
# Es significativo.
# IC: Incluso en el extremo más conservador, el hazard aumenta un 70%.
# En el extremo superior, aumenta más del triple.
# Concordance = 0.519
# - 0.5 = azar
# - 0.519 ≈ muy bajo
# Interpretación:
# El modelo distingue apenas un poco mejor que el azar entre conflictos más y menos duraderos.

cox.zph(m_cox_45_sdq)
# El supuesto de riesgos proporcionales está violado.
# El efecto de 1945 NO es constante en el tiempo.
# El modelo Cox NO es válido para interpretar el efecto de 1945 en SDQ

# Conclusion:
# - El efecto de 1945 es real, grande y robusto.
# - Pero Cox no es el modelo adecuado para estimarlo.
# - Weibull es el modelo correcto para SDQ.

## Comparacion de modelos -------------------------------------------------
AIC(
  m_wei_base_sdq,
  m_wei_phase_sdq,
  m_wei_39_sdq,
  m_wei_45_sdq
)
# La estructura sistémica (entreguerras, multipolaridad clásica, ruptura sistémica, etc.)
# es el predictor más fuerte de cuánto duran los conflictos diádicos
# 1945 sí marca un cambio estructural en la duración de los conflictos diádicos,
# pero su efecto es mucho menor que el de la fase sistémica

# COW --------------------------------------------------------------------
## 1. Preparar datos ------------------------------------------------------
cow <- conf_durint_cow |>
  filter(!is.na(months)) |>
  filter(months != 0) |>
  mutate(
    phase = factor(phase),
    rimland = factor(rimland),
    shock_1939 = factor(shock_1939),
    shock_1945 = factor(shock_1945)
  )


## Modelo base ------------------------------------------------------------
# Exponencial
m_exp_base_cow <- survreg(Surv(months) ~ 1, data = cow, dist = "exponential")
summary(m_exp_base_cow)
# Qué mirar:
# - (Intercept) → log de la duración media
# - Loglik(intercept only) → lo usaremos para comparar con modelos con fase
# - AIC(m_exp_base_sdq) → para tabla comparativa
# En la parametrización de survreg():
# Intercept=log(media)
# Porque en la distribución Exponencial:
# media=1\lambda

# 2. Modelo base Weibull (sin covariables)
m_wei_base_cow <- survreg(Surv(months) ~ 1, data = cow, dist = "weibull")
summary(m_wei_base_cow)
# Qué mirar:
# - (Intercept) → duración media en escala Weibull
# - Log(scale) → forma del hazard
# - > 0 → hazard creciente
# - < 0 → hazard decreciente
# - AIC(m_wei_base_) → para comparar con Exponencial

# Es decir:
# - no hay covariables
# - solo se estima la duración típica y la forma del hazard
# - Weibull permite hazard creciente o decreciente
# - es más flexible que Exponencia

# En survreg() con Weibull, el intercepto es:
# Intercept=log(mediana)
# ⚠️ Ojo:
# En Weibull, el intercepto NO es el log de la media, sino del tiempo característico
# (location parameter), que se interpreta como una mediana transformada.

# log(scale) es el parametro clave de Weibull
# - scale > 1 → hazard creciente
# - scale < 1 → hazard decreciente
# - scale = 1 → equivalente a Exponencial

# El hazard aumenta con el tiempo.
# Cuanto más dura un conflicto, más probable es que termine.
# Esto es típico de conflictos interestatales y diádicos:
# los conflictos largos tienden a resolverse porque se agotan recursos, presión internacional, desgaste, etc.

# 3. Modelo base Cox (sin covariables)
m_cox_base_cow <- coxph(Surv(months) ~ 1, data = cow)
summary(m_cox_base_cow)
# Qué mirar:
# - loglik → log-likelihood del modelo vacío
# - AIC(m_cox_base_sdq) → para comparar con Exponencial y Weibull
# - Concordance → aunque sin covariables no es muy informativo

AIC(m_exp_base_cow, m_wei_base_cow, m_cox_base_cow)

# Conclusión:
# Weibull el mejor modelo.

## 2. Modelo A — Fase -----------------------------------------------------
# Exponencial
m_exp_phase_cow <- survreg(
  Surv(months) ~ phase,
  data = cow,
  dist = "exponential"
)
summary(m_exp_phase_cow)
# En la fase de bipolaridad, la duración media estimada
# de un conflicto interestatal en COW es ~37 meses
# Todas las fases duran menos que la bipolaridad
# Es significativo. La fase del sistema internacional explica
# fuertemente la duración de los conflictos interestatales en COW

# Weibull
m_wei_phase_cow <- survreg(Surv(months) ~ phase, data = cow, dist = "weibull")
summary(m_wei_phase_cow)
# En la fase de bipolaridad, la duración típica de una guerra interestatal
# en COW es ~32 meses.
# Todas las fases duran menos que la bipolaridad.
# El hazard es creciente: cuanto más dura una guerra, más probable es que termine.
# Es significativo.

# Cox
m_cox_phase_cow <- coxph(Surv(months) ~ phase, data = cow)
summary(m_cox_phase_cow)
# - estima hazard ratios
# - interpreta coeficientes como cambios en la probabilidad instantánea de terminar la guerra
# Todas las fases tienen mayor hazard que la bipolaridad.
# Es decir: todas las fases duran menos que la bipolaridad.
# Concordance: 0.549 = predice apenas mejor q el azar.

# Test de riesgos proporcionales
cox.zph(m_cox_phase_cow)

# El modelo Cox con fase en COW viola el supuesto de riesgos proporcionales.
# El efecto de la fase cambia a lo largo de la duración de la guerra.
# Weibull es el modelo correcto para COW.

## 3. Modelo B — Shocks (1939 y 1945) -------------------------------------
# Shock 1939
m_exp_39_cow <- survreg(
  Surv(months) ~ shock_1939,
  data = cow,
  dist = "exponential"
)
summary(m_exp_39_cow)
# Antes de 1939, la duración media estimada de una guerra interestatal en COW es ~18 meses.
# Después de 1939, las guerras interestatales duran ~85% más que antes. ~33.5 meses
# Es significativo

m_wei_39_cow <- survreg(Surv(months) ~ shock_1939, data = cow, dist = "weibull")
summary(m_wei_39_cow)
# Antes de 1939, la duración típica de una guerra interestatal en COW es ~16 meses.
# Después de 1939, las guerras interestatales duran ~82% más que antes ~29 meses
# Es significativo.
# El hazard es creciente: cuanto más dura una guerra, más probable es que termine

m_cox_39_cow <- coxph(Surv(months) ~ shock_1939, data = cow)
summary(m_cox_39_cow)
# Después de 1939, el hazard es 36% menor que antes de 1939. Guerras más largas después de 1939.
# Es significativo.
# Incluso en el extremo más conservador, el hazard cae un 25%.
# En el extremo superior, cae un 45%
# Concordance = 0.54. El modelo predice un poco mejor que el azar, pero no mucho.

cox.zph(m_cox_39_cow)
# No hay evidencia de violación del supuesto de riesgos proporcionales.
# El modelo Cox con shock_1939 en COW es válido

# Shock 1945
m_exp_45_cow <- survreg(
  Surv(months) ~ shock_1945,
  data = cow,
  dist = "exponential"
)
summary(m_exp_45_cow)
# Antes de 1945, la duración media estimada de una guerra interestatal en COW es ~18 meses
# Después de 1945, las guerras interestatales duran ~88% más que antes. ~33.9 meses
# Es significativo

m_wei_45_cow <- survreg(Surv(months) ~ shock_1945, data = cow, dist = "weibull")
summary(m_wei_45_cow)
# Antes de 1945, la duración típica de una guerra interestatal en COW es ~16 meses.
# Después de 1945, las guerras duran ~85% más que antes. ~29.5 meses
# Es significativo.
# El hazard es creciente: cuanto más dura una guerra, más probable es que termine

m_cox_45_cow <- coxph(Surv(months) ~ shock_1945, data = cow)
summary(m_cox_45_cow)
# Después de 1945, el hazard es 36.7% menor que antes de 1945.
# Las guerras duran mucho más después de 1945.
# Es singnificativo.
# Incluso en el extremo más conservador, el hazard cae un 26%.
# En el extremo superior, cae un 46%
# Concordance = 0.547. El modelo predice un poco mejor que el azar, pero no mucho.

cox.zph(m_cox_45_cow)
# No hay evidencia de violación del supuesto de riesgos proporcionales.
# El modelo Cox con shock_1945 en COW es válido.

## 4. Modelo C - Rimland --------------------------------------------------

# Explonencial
m_exp_rim_cow <- survreg(
  Surv(months) ~ rimland,
  data = cow,
  dist = "exponential"
)
summary(m_exp_rim_cow)
# Las guerras interestatales fuera del rimland duran típicamente ~25 meses.
# Las guerras en el rimland duran solo un 7% menos que las guerras fuera del rimland.
# No es significativo.

# Weibull
m_wei_rim_cow <- survreg(Surv(months) ~ rimland, data = cow, dist = "weibull")
summary(m_wei_rim_cow)
# Las guerras interestatales fuera del rimland duran típicamente ~21 meses.
# Las guerras en el rimland duran solo un 7.5% menos que las guerras fuera del rimland
# No es significativo

# Cox
m_cox_rim_cow <- coxph(Surv(months) ~ rimland, data = conf_durint_cow)
summary(m_cox_rim_cow)
# Las guerras en el rimland tienen un hazard 5.3% mayor que las guerras fuera del rimland.
# No es significativo
# Concordance = 0.509

cox.zph(m_cox_rim_cow)
# No hay evidencia de violación del supuesto de riesgos proporcionales.
# El modelo Cox con rimland en COW es válido.

## Comparacion de modelos -----------------------------------------------------------
AIC(
  m_wei_base_cow,
  m_wei_phase_cow,
  m_wei_39_cow,
  m_cox_39_cow,
  m_wei_45_cow,
  m_cox_45_cow
)

## Conclusiones -----------------------------------------------------------
#  los modelos Weibull con shocks sistémicos ofrecen el mejor ajuste para
#  explicar la duración de las guerras interestatales en COW

# UCDP -------------------------------------------------------------------

## 1. Preparacion de datos ------------------------------------------------
ucdp <- conf_durint_ucdp |>
  filter(!is.na(months), months > 0) |>
  mutate(
    phase = factor(phase),
    rimland = factor(rimland),
    shock_1991 = factor(shock_1991)
  )


## Modelos base -----------------------------------------------------------
# Exponencial
m_exp_base_ucdp <- survreg(
  Surv(months) ~ 1,
  data = ucdp,
  dist = "exponential"
)
summary(m_exp_base_ucdp)
# La duración típica de un conflicto UCDP es de ~172 meses (~14.3 años)

# Weibull
m_wei_base_ucdp <- survreg(
  Surv(months) ~ 1,
  data = ucdp,
  dist = "weibull"
)
summary(m_wei_base_ucdp)
# La duración típica de un conflicto UCDP es de ~110 meses (~9.2 años)
# El hazard es fuertemente creciente.
# Cuanto más dura un conflicto UCDP, más probable es que termine.

m_cox_base_ucdp <- coxph(Surv(months) ~ 1, data = ucdp)
summary(m_cox_base_ucdp)

## 2. Modela A - Fase -----------------------------------------------------
# Exponencial
m_exp_phase_ucdp <- survreg(
  Surv(months) ~ phase,
  data = ucdp,
  dist = "exponential"
)
summary(m_exp_phase_ucdp)
# Durante la bipolaridad, la duración típica de un conflicto UCDP es ~219 meses (~18.2 años)
# En ruptura sistema los conflictos duran ~39% menos que en la bipolaridad, pero el efecto NO es significativo.
# En Unipolaridad temprana los conflictos duran ~64% menos que en la bipolaridad. Efecto muy fuerte y altamente significativo. - ~79 meses
# Unipolaridad tardía / multipolaridad emergente los conflictos duran ~79% menos que en la bipolaridad. Efecto masivo y extremadamente significativo. - ~46 meses
# Significancia global.

# Weibull
m_wei_phase_ucdp <- survreg(Surv(months) ~ phase, data = ucdp, dist = "weibull")
summary(m_wei_phase_ucdp)

# Durante la bipolaridad, la duración típica de un conflicto UCDP es ~156 meses (~13 años).
# Ruptura del sistema Los conflictos duran ~39% menos que en la bipolaridad, pero el efecto NO es significativo.
# Unipolaridad temprana Los conflictos duran ~63% menos que en la bipolaridad. Efecto fuerte y significativo. - ~57 meses
# Unipolaridad tardía / multipolaridad emergente Los conflictos duran ~78% menos que en la bipolaridad. Efecto masivo y altamente significativo. - ~34 meses
# El hazard es crecientemente acelerado: cuanto más dura un conflicto UCDP, más probable es que termine.
# Significancia global.

# Cox
m_cox_phase_ucdp <- coxph(Surv(months) ~ phase, data = ucdp)
summary(m_cox_phase_ucdp)
# Ruptura del sistema los conflictos duran ~37% menos que en la bipolaridad, pero el efecto NO es significativo
# Unipolaridad temprana los conflictos duran ~53% menos que en la bipolaridad. Efecto fuerte y altamente significativo
# Unipolaridad tardía Los conflictos duran ~65% menos que en la bipolaridad. Efecto masivo y extremadamente significativo
# Significancia global.
# Concordance = 0.57. predice mejor que el azar y captura estructura temporal real.

cox.zph(m_cox_phase_ucdp)
# Riesgos proporcionales (PH): el efecto de cada fase debe ser constante en el tiempo.
# El modelo Cox con fase para UCDP viola el supuesto de riesgos proporcionales.

# Qué vamos a mirar:
# - ¿Alguna fase tiene HR < 1 (más larga) o HR > 1 (más corta)?
# - ¿El test de riesgos proporcionales viola supuestos?
# - ¿Weibull mejora AIC vs Exponencial?

## 3. Modelo B- Shocks 1945 y 1991 ------------------------------------------------
# Exponencial
m_exp_45_ucdp <- survreg(
  Surv(months) ~ shock_1945,
  data = ucdp,
  dist = "exponential"
)
summary(m_exp_45_ucdp)
# La media de duracion de conflicto pre shock es de 83 meses.
# Post shock 2.07 veces mas que la base ~172 meses.
# No significativo

# Weibull
m_wei_45_ucdp <- survreg(
  Surv(months) ~ shock_1945,
  data = ucdp,
  dist = "weibull"
)
summary(m_wei_45_ucdp)
# Hazard altamente creciente 84%
# Conflictos pre duran 81 meses.
# Post duran 37% mas. ~111 meses
# No significativo
# Global no significativo

# Cox
m_cox_45_ucdp <- coxph(Surv(months) ~ shock_1945, data = ucdp)
summary(m_cox_45_ucdp)
# Después de 1945, el hazard es ~24% menor, conflictos algo más largos.
# NO es significativo
cox.zph(m_cox_45_ucdp)
# Viola supuestos

# Exponencial
m_exp_91_ucdp <- survreg(
  Surv(months) ~ shock_1991,
  data = ucdp,
  dist = "exponential"
)
summary(m_exp_91_ucdp)
# Hazard constante.
# Pre 1991, conflictos duran una media de 221 meses.
# Post 1991, conflictos duran un 70% menos. ~66 meses
# Es significativo el shock.
# Significancia global.

# Weibull
m_wei_91_ucdp <- survreg(
  Surv(months) ~ shock_1991,
  data = ucdp,
  dist = "weibull"
)
summary(m_wei_91_ucdp)
# Hazard creciente 73%
# Pre 1991 156 meses. Post 70% menos ~47 meses
# Es significativo

# Cox
m_cox_91_ucdp <- coxph(Surv(months) ~ shock_1991, data = ucdp)
summary(m_cox_91_ucdp)
# Post 1991, el hazard es 2.37 veces mayor, lo que significa que los conflictos duran muchísimo menos
# Es significativo
# Predice mejor q el azar. Concordance >0.5

cox.zph(m_cox_91_ucdp)
# Viola supuesto de Hazar Proporcionales (cada etapa debe mantener el hazar constante).
# Weibull mejor modelo.

# Qué esperamos:
# - Tu EDA mostró ligera reducción en duración después de 1991.
# - Pero probablemente no será significativo

## 4. Modelo C - Rimland --------------------------------------------------
# Exponencial
m_exp_rim_ucdp <- survreg(
  Surv(months) ~ rimland,
  data = ucdp,
  dist = "exponential"
)
summary(m_exp_rim_ucdp)
# Media fuera de Rimland 160 meses.
# Rimland 12% mas que fuera ~180 meses.
# No significativo

# Weibull
m_wei_rim_ucdp <- survreg(Surv(months) ~ rimland, data = ucdp, dist = "weibull")
summary(m_wei_rim_ucdp)
# Hazar creciente 84%.
# Media fuera Rimland 101 meses. 17% mas en Rimland ~118 meses.
# No es significativo.

# Cox
m_cox_rim_ucdp <- coxph(Surv(months) ~ rimland, data = ucdp)
summary(m_cox_rim_ucdp)
# En Rimland  los conflictos duran 14% menos.
# No significativo
# Apenas mejor q el azar

cox.zph(m_cox_rim_ucdp)
# No viola supuestos.
# Constante en el tiempo. No viola supuestos

# Qué esperamos según tu EDA:
# - Rimland no explica duración en UCDP.
# - HR ≈ 0.83, p ≈ 0.12 (no significativo)

## Conclusiones -----------------------------------------------------------

AIC(
  m_exp_base_ucdp,
  m_wei_base_ucdp,
  m_exp_phase_ucdp,
  m_wei_phase_ucdp,
  m_exp_91_ucdp,
  m_wei_91_ucdp
)

# El mejor modelo para UCDP es Weibull + shock_1991.
# El segundo mejor es Weibull + fase.
