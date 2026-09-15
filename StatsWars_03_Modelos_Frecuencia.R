library(tidyverse)
library(readr)
library(lubridate)
library(MASS)


# Input ------------------------------------------------------------------
anu_frec_cow <- readRDS("Data/data_gold/anu_frec_cow.rds")
anu_frec_sdq <- readRDS("Data/data_gold/anu_frec_sdq.rds")
anu_frec_ucdp <- readRDS("Data/data_gold/anu_frec_ucdp.rds")
anu_frec_ucdp_War <- readRDS("Data/data_gold/anu_frec_ucdp_War.rds")

conf_durint_cow <- readRDS("Data/data_gold/conf_durint_cow.rds")
conf_durint_sdq <- readRDS("Data/data_gold/conf_durint_sdq.rds")
conf_durint_ucdp <- readRDS("Data/data_gold/conf_durint_ucdp.rds")


# Revision Supuestos Poisson ---------------------------------------------

## 1. La variable dependiente es un conteo no negativo --------------------

# Ya se cumple

## 2. La media ≈ varianza (equidispersión) --------------------------------

# SDQ
mean(anu_frec_sdq$n_conflictos_sdq, na.rm = TRUE)
var(anu_frec_sdq$n_conflictos_sdq, na.rm = TRUE)

# COW
mean(anu_frec_cow$n_conflictos_cow, na.rm = TRUE)
var(anu_frec_cow$n_conflictos_cow, na.rm = TRUE)

# UCDP
mean(anu_frec_ucdp$n_conflictos_ucdp, na.rm = TRUE)
var(anu_frec_ucdp$n_conflictos_ucdp, na.rm = TRUE)

# ✔ Cómo interpretarlo
# - Si varianza ≈ media → Poisson está bien.
# - Si varianza >> media → hay sobredispersión → usar quasi‑Poisson o NegBin.
# - Si varianza << media → hay subdispersión (raro en conflictos)

# Variaza > media en los 3 datasets. Sobredispersion -> usar variante

## 3. Independencia entre observaciones (años) ----------------------------

# ✔ Cómo evaluarlo
# Mirás la serie temporal:
# - ¿Hay autocorrelación fuerte?
# - ¿Hay tendencia?
# - ¿Hay ciclos?
# Ya lo hiciste en el EDA.

# ✔ Qué viste
# - Las series son erráticas, con picos aislados.
# - No hay autocorrelación fuerte ni tendencia sistemática.
# - No hay ciclos regulares.
# 👉 Conclusión:
# La independencia entre años es razonable.

acf(anu_frec_sdq$n_conflictos_sdq)

acf(anu_frec_cow$n_conflictos_cow)

acf(anu_frec_ucdp$n_conflictos_ucdp)

# Como lo interpreto?
# La mayoria de las barritas estan dentro de los margenes.
# Todos los datasets tiene las primeras barritas mas altas.

## 4. Linealidad en el link log -------------------------------------------

# Este supuesto se evalúa después del modelo, no antes.
# Pero podés anticipar problemas:
# - Si una variable categórica tiene muy pocos casos → inestabilidad.
# - Si una variable tiene valores extremos → problemas de ajuste.
# Ya lo detectaste con:
# - fases pequeñas (unipolaridad_temprana)
# - geo_zone con categorías chicas (Heartland)
# 👉 Conclusión:
# Tus decisiones de colapsar categorías ya resuelven este supuesto.

## Checklist final --------------------------------------------------------

# ✔ 1. Confirmar que la variable dependiente es un conteo
# (ya está)
# ✔ 2. Calcular media y varianza
# → decidir si Poisson, quasi‑Poisson o NegBin
# ✔ 3. Revisar autocorrelación (opcional)
# → ACF simple
# ✔ 4. Confirmar que las variables explicativas no tienen categorías con N muy bajo
# → ya lo resolviste con geo_zone colapsada y shock seleccionados

# SDQ --------------------------------------------------------------------

## 1. Preparación del dataset ---------------------------------------------

df <- anu_frec_sdq |>
  filter(!is.na(n_conflictos_sdq)) |>
  mutate(
    phase = factor(phase),
    shock_1939 = factor(shock_1939),
    shock_1945 = factor(shock_1945)
  )


## 2. Modelo base (Poisson) ------------------------------------------------

m0 <- glm(n_conflictos_sdq ~ 1, family = poisson, data = df)
summary(m0)

# Interpretación esperada:
# - Te da la tasa promedio de conflictos por año.
# - Sirve como referencia para comparar AIC.

# Intercepto = 1.695 → exp(1.695) ≈ 5.45 díadas por año en promedio.
# Nada más que eso: sirve como referencia.

## 3. Modelo por fase -----------------------------------------------------

m_phase <- glm(n_conflictos_sdq ~ phase, family = poisson, data = df)
summary(m_phase)
exp(coef(m_phase)) # razones de tasas

# Interpretación:
# - Cada coeficiente es una razón de tasas respecto a la categoría base.
# - Si exp(coef) > 1 → esa fase tiene más conflictos que la base.
# - Si exp(coef) < 1 → menos conflictos

# entreguerras exp(coef) = 0.63 -> 37% menos conflictos que la base
# multipolaridad_clásica exp(coef) = 0.61 -> 39% menos conflictos que la base
# ruptura_sistema exp(coef) = 1.88 -> 88% mas conflictos que la base

# Conclusión:
# - ruptura_sistema es la única fase con un aumento fuerte y significativo.
# - entreguerras y multipolaridad_clásica tienen tasas significativamente menores.
# - Este modelo reduce mucho el AIC (858 vs 1056), lo cual indica que fase explica muy bien la frecuencia.

##  4. Modelo por shocks ---------------------------------------------------

# Shock 1939
m_39 <- glm(n_conflictos_sdq ~ shock_1939, family = poisson, data = df)
summary(m_39)
exp(coef(m_39))

# exp(coef) = 2.45
# Interpretación:
# - Después de 1939, SDQ registra 2.45 veces más díadas por año.
# Esto es totalmente consistente con tu EDA:
# → SDQ explota en guerras mundiales porque cuenta díadas.

# Shock 1945
m_45 <- glm(n_conflictos_sdq ~ shock_1945, family = poisson, data = df)
summary(m_45)
exp(coef(m_45))

# Interpretación:
# - exp(coef) indica si después del shock hay más o menos conflictos.
# - Ya sabemos por el EDA que 1945 no cambia nada, pero lo incluimos por hipótesis teórica.

# exp(coef) = 1.57
# Interpretación:
# - Después de 1945, SDQ registra 57% más díadas.
# Pero ojo:
# - El AIC del modelo es peor que el modelo base.
# - El efecto desaparece cuando controlás por fase.
# Esto confirma tu hallazgo clave:
# 1945 no cambia nada sustantivo.

##  5. Modelo combinado ---------------------------------------------------

m_full <- glm(
  n_conflictos_sdq ~ phase + shock_1939 + shock_1945,
  family = poisson,
  data = df
)
summary(m_full)
exp(coef(m_full))

# Interpretación profunda:
# Cuando controlás por fase, los shocks pierden todo efecto.
# Y ruptura_sistema también pierde significancia porque:
# - la fase ya captura la estructura temporal,
# - y shock_1939 absorbía parte del mismo patrón.

# Conclusion
# La fase explica la frecuencia.
# Los shocks no.

## 6. Chequeo de sobredispersión ------------------------------------------

# Poisson deviance / df
deviance(m_full) / df.residual(m_full)

# Interpretación:
# - Si > 1.5 → sobredispersión clara
# - Si > 2 → usar NegBin sí o sí

# Deviance/df = 3.32 → sobredispersión fuerte.
# Correcto pasar a quasi‑Poisson y NegBin.

## 7. Modelos robustos ----------------------------------------------------

# Quasi‑Poisson
m_full_qp <- glm(
  n_conflictos_sdq ~ phase + shock_1939 + shock_1945,
  family = quasipoisson,
  data = df
)
summary(m_full_qp)
exp(coef(m_full_qp))

# - Los coeficientes son iguales.
# - Los errores estándar aumentan.
# - Ninguna variable es significativa.
# Esto confirma que el Poisson clásico inflaba la significancia

# Negative Binomial
m_full_nb <- MASS::glm.nb(
  n_conflictos_sdq ~ phase + shock_1939 + shock_1945,
  data = df
)
summary(m_full_nb)
exp(coef(m_full_nb))

# Interpretación:
# - Quasi‑Poisson ajusta los errores estándar.
# - NegBin ajusta la varianza y suele ser el mejor para conteos sobredispersos.

# Resultados:
# - Ninguna variable es significativa.
# - exp(coef) es igual que en Poisson, pero con errores más grandes.
# - AIC = 738, el mejor de todos los modelos.

# Interpretación:
# - La estructura de fases sí cambia las tasas, pero la variabilidad interna del SDQ es tan grande que, una vez corregida la sobredispersión, las diferencias dejan de ser estadísticamente significativas.
# Esto es típico en SDQ porque:
# - cuenta díadas,
# - tiene picos extremos,
# - y la varianza es enorme

## 8. Comparación de modelos -----------------------------------------------

AIC(m0, m_phase, m_39, m_45, m_full, m_full_nb)
# Interpretación:
# - El modelo con menor AIC es el mejor.
# - NegBin suele ganar cuando hay sobredispersión.

# Conclusión:
# El mejor modelo es el Negative Binomial completo (m_full_nb).

## Conclusiones -----------------------------------------------------------

# CONCLUSIONES SUSTANTIVAS PARA SDQ
# ✔ 1. La fase explica la frecuencia de conflictos
# - entreguerras y multipolaridad_clásica tienen tasas más bajas.
# - ruptura_sistema tiene tasas más altas, pero pierde significancia en NB.
# ✔ 2. Los shocks NO explican la frecuencia
# - shock_1939 y shock_1945 pierden significancia cuando controlás por fase.
# - Esto refuerza tu hallazgo clave:
# 1945 no cambia nada.
# ✔ 3. El modelo Negative Binomial es el adecuado
# - sobredispersión fuerte
# - mejor AIC
# - estimaciones más realistas
# ✔ 4. SDQ es un dataset ruidoso
# - la varianza enorme hace que las diferencias pierdan significancia
# - pero los patrones descriptivos siguen siendo válido

# COW --------------------------------------------------------------------
# Unidad de analisis = Ano
# Variable dependiente = n_conflictos_cow

## 1. Preparacion del dataset -----------------------------------------------

# Convierto a formato long porque quiero modelar tasa de Rimland/No Rimland
df_long <- anu_frec_cow |>
  dplyr::select(year, n_rimland_1, n_rimland_0, phase, shock_1945) |>
  pivot_longer(
    cols = c(n_rimland_1, n_rimland_0),
    names_to = "zone",
    values_to = "n_conflicts"
  ) |>
  mutate(
    zone = ifelse(zone == "n_rimland_1", "Rimland", "No_Rimland"),
    zone = factor(zone),
    phase = factor(phase),
    shock_1945 = factor(shock_1945)
  )


## 2. Modelo base ---------------------------------------------------------

m0 <- glm(n_conflicts ~ 1, family = poisson, data = df_long)
summary(m0)

# Intercepto = 0.661 → exp(0.661) ≈ 1.94 conflictos por zona‑año

## 3. Modelo por zona (Rimland vs No_Rimland) ------------------------------

m_zone <- glm(n_conflicts ~ zone, family = poisson, data = df_long)
summary(m_zone)
exp(coef(m_zone))

# Interpretación:
# - exp(coef)["zoneRimland"] > 1 → Rimland tiene mayor tasa
# - exp(coef)["zoneRimland"] < 1 → Rimland tiene menor tasa

# Resultado:
# zoneRimland = 0.8331, p < 2e‑16
# exp(0.8331) = 2.30
# Interpretación:
# La tasa de conflictos en Rimland es 2.3 veces mayor que en No Rimland.

# Y es altamente significativo.
# AIC:
# - m0 = 1396
# - m_zone = 1281 → gran mejora
# Esto ya te permite afirmar, con evidencia sólida:
# Rimland es sistemáticamente más conflictivo que el resto del mundo.

## 4. Modelo por fase -----------------------------------------------------

m_phase <- glm(n_conflicts ~ phase, family = poisson, data = df_long)
summary(m_phase)
exp(coef(m_phase))

# Ninguna fase es significativa.
# exp(coef) ≈ 0.7–1.5, pero sin significancia.
# Interpretación:
# En COW, las fases del sistema internacional no explican la frecuencia anual de conflictos por zona.

# Esto es interesante porque contrasta con SDQ.

## 5. Modelo por shock 1945 -----------------------------------------------

m_45 <- glm(n_conflicts ~ shock_1945, family = poisson, data = df_long)
summary(m_45)
exp(coef(m_45))

# Shock_1945 no es significativo (p = 0.849).
# Interpretación:
# 1945 no cambia nada en la frecuencia de conflictos.

# Esto refuerza tu hallazgo central.

## 6. Modelo combinado (zona + fase + shock) ------------------------------

m_full <- glm(
  n_conflicts ~ zone + phase + shock_1945,
  family = poisson,
  data = df_long
)
summary(m_full)
exp(coef(m_full))

# Interpretación:
# - zoneRimland te da la razón de tasas Rimland vs No_Rimland, controlando por fase y shock.
# - Los coeficientes de fase te dicen qué fases tienen más o menos conflictos.
# - shock_1945 debería ser no significativo

# Resultado clave:
# zoneRimland = 0.8331, p < 2e‑16
# exp = 2.30
# Y lo más importante:
# ✔ Rimland sigue siendo significativo
# ✔ El efecto es idéntico al modelo simple
# ✔ Fase no aporta nada
# ✔ Shock 1945 no aporta nada
# Interpretación:
# Incluso controlando por fase y por 1945, Rimland mantiene una tasa de conflictos 2.3 veces mayor que No Rimland.

## 7. Chequeo de sobredispersión -------------------------------------------

deviance(m_full) / df.residual(m_full)

# - 1.5 → sobredispersión
# - 2 → usar Negative Binomial sí o sí

# Deviance/df = 1.23
# Esto es muy bueno.
# No hay sobredispersión seria.
# Interpretación:
# El modelo Poisson es adecuado para COW.

##  8. Modelos robustos ----------------------------------------------------

# Quasi‑Poisson
m_full_qp <- glm(
  n_conflicts ~ zone + phase + shock_1945,
  family = quasipoisson,
  data = df_long
)
summary(m_full_qp)
exp(coef(m_full_qp))

# Negative Binomial
m_full_nb <- MASS::glm.nb(
  n_conflicts ~ zone + phase + shock_1945,
  data = df_long
)
summary(m_full_nb)
exp(coef(m_full_nb))

# Ambos modelos:
# - mantienen el coeficiente de Rimland prácticamente igual
# - mantienen la significancia
# - no cambian la interpretación
# - NB tiene el mejor AIC (1278.7), pero solo marginalmente mejor que Poisson (1282.1)
# Interpretación:
# Los resultados son robustos a la especificación del modelo.

## 9. Comparación de modelo -----------------------------------------------

AIC(m0, m_zone, m_phase, m_45, m_full, m_full_nb)

# Conclusión:
# - El mejor modelo es m_full_nb, pero solo marginalmente mejor que m_zone.
# - La variable que realmente explica la frecuencia es zone (Rimland)

## Conclusiones -----------------------------------------------------------

# ✔ 1. Rimland importa, y mucho
# La tasa de conflictos en Rimland es 2.3 veces mayor que en No Rimland.
# Este efecto es fuerte, significativo y robusto a todas las especificaciones.

# ✔ 2. Las fases del sistema internacional NO explican la frecuencia en COW
# Esto contrasta con SDQ y UCDP, y es un hallazgo interesante.
# ✔ 3. 1945 no cambia nada
# Ni en frecuencia, ni en duración, ni en intensidad.
# Este es un hallazgo central de tu tesis.
# ✔ 4. El modelo Poisson funciona bien
# COW no tiene sobredispersión fuerte.
# NB mejora un poco el AIC, pero no cambia la interpretación.

# UCDP -------------------------------------------------------------------

## 1. Preparacion del dataset ---------------------------------------------
df_long <- anu_frec_ucdp |>
  dplyr::select(year, n_rimland_1, n_rimland_0, phase, shock_2001) |>
  pivot_longer(
    cols = c(n_rimland_1, n_rimland_0),
    names_to = "zone",
    values_to = "n_conflicts"
  ) |>
  mutate(
    zone = ifelse(zone == "n_rimland_1", "Rimland", "No_Rimland"),
    zone = factor(zone),
    phase = factor(phase),
    shock_2001 = factor(shock_2001)
  )

# df_long <- anu_frec_ucdp_War |>
#   dplyr::select(year, n_rimland_1, n_rimland_0, phase, shock_2001) |>
#   pivot_longer(
#     cols = c(n_rimland_1, n_rimland_0),
#     names_to = "zone",
#     values_to = "n_conflicts"
#   ) |>
#   mutate(
#     zone = ifelse(zone == "n_rimland_1", "Rimland", "No_Rimland"),
#     zone = factor(zone),
#     phase = factor(phase),
#     shock_2001 = factor(shock_2001)
#   )

## 2. Modelo base ---------------------------------------------------------
m0 <- glm(n_conflicts ~ 1, family = poisson, data = df_long)
summary(m0)

# Intercepto = 0.651 → exp(0.651) ≈ 1.92 conflictos por zona‑año.

## 3. Modelo por zona (Rimland vs No_Rimland) -----------------------------
m_zone <- glm(n_conflicts ~ zone, family = poisson, data = df_long)
summary(m_zone)
exp(coef(m_zone))

# Resultado:
# zoneRimland = 0.1787, p = 0.121
# exp = 1.20
# Interpretación:
# Rimland tiene un 20% más de conflictos que No_Rimland, pero NO es significativo.

# Esto es un contraste fuerte con COW, donde Rimland era 2.3 veces más conflictivo.
# Implicación:
# - En UCDP, la geografía no estructura la frecuencia.
# - El patrón Rimland es dataset‑dependiente, no universal.

## 4. Modelo por fase -----------------------------------------------------
m_phase <- glm(n_conflicts ~ phase, family = poisson, data = df_long)
summary(m_phase)
exp(coef(m_phase))

# Resultados:
# - ruptura_sistema: exp = 0.40, significativo
# - unipolaridad_tardia_multipolaridad_emergente: exp = 0.71, significativo
# - unipolaridad_temprana: no significativo
# Interpretación:
# UCDP muestra menos conflictos en ruptura sistémica y en la fase tardía/multipolaridad emergente.

# Esto es interesante porque:
# - SDQ mostraba más conflictos en ruptura sistémica.
# - COW no mostraba efecto de fase.
# - UCDP muestra menos.
# Esto refuerza tu tesis sobre la importancia de la estructura del dataset.

## 5. Modelo por shock 2001 -----------------------------------------------
m_2001 <- glm(n_conflicts ~ shock_2001, family = poisson, data = df_long)
summary(m_2001)
exp(coef(m_2001))

# Resultado:
# shock_2001 = -0.282, p = 0.0439
# exp = 0.75
# Interpretación:
# Después de 2001, la frecuencia de conflictos baja un 25%.

# Este es el único shock con efecto visible en UCDP.

## 6. Modelo combinado (zona + fase + shock) ------------------------------
m_full <- glm(
  n_conflicts ~ zone + phase + shock_2001,
  family = poisson,
  data = df_long
)
summary(m_full)
exp(coef(m_full))

# Interpretación:
# - Rimland no importa en UCDP.
# - Fase sí importa, pero solo ruptura sistémica.
# - Shock 2001 pierde significancia cuando controlás por fase

## 7. Chequeo de sobredispersion ------------------------------------------
deviance(m_full) / df.residual(m_full)
# Deviance/df = 1.84
# → sobredispersión moderada
# → Poisson inflaría significancia
# → NB es apropiado

## 8. Modelos robustos ----------------------------------------------------
# Quasi‑Poisso
m_full_qp <- glm(
  n_conflicts ~ zone + phase + shock_2001,
  family = quasipoisson,
  data = df_long
)
summary(m_full_qp)
exp(coef(m_full_qp))

# Resultados:
# - zoneRimland: exp = 1.21, p = 0.216 → NO significativo
# - ruptura_sistema: exp = 0.39, p = 0.037 → significativo
# - shock_2001: exp = 0.54, p = 0.476 → NO significativo
# - AIC = 583.3 → mejor modelo

# Interpretación final:
# En UCDP, la geografía (Rimland) no explica la frecuencia de conflictos.
# La única variable robusta es la fase “ruptura sistémica”, que muestra menos conflictos.
# El shock de 2001 no tiene efecto una vez controlada la fase.

# Negative Binomial
m_full_nb <- MASS::glm.nb(
  n_conflicts ~ zone + phase + shock_2001,
  data = df_long
)
summary(m_full_nb)
exp(coef(m_full_nb))

# Conclusión:
# - El mejor modelo es Negative Binomial completo.
# - Pero la única variable robusta es ruptura sistémica.
# - Rimland NO importa.
# - Shock 2001 NO importa al controlar por fase.

## 9. Comparacion de modelo -----------------------------------------------
AIC(m0, m_zone, m_phase, m_2001, m_full, m_full_nb)

## Conclusiones -----------------------------------------------------------

### UCDP Minor y War -------------------------------------------------------

# ✔ 1. Rimland NO es un predictor de frecuencia en UCDP
# La tasa de conflictos en Rimland es 1.20 veces la de No_Rimland, pero esta diferencia no es significativa.

# Esto contrasta con COW y es un hallazgo clave sobre dependencia del dataset.
# ✔ 2. La fase “ruptura sistémica” sí importa
# UCDP registra un 60% menos de conflictos en ruptura sistémica.

# Esto es consistente con tu EDA.
# ✔ 3. Shock 2001 pierde significancia al controlar por fase
# El descenso post‑2001 no es robusto.

# ✔ 4. El modelo Negative Binomial es el adecuado
# Sobredispersión moderada → NB mejora el ajuste.

### UCDP War -------------------------------------------------------

# ✔ 1. UCDP ≥1000 muertes NO reproduce el patrón Rimland de COW
# Rimland tiene 48% más conflictos que No_Rimland, pero esta diferencia no es significativa.

# Esto es un hallazgo clave:
# la geografía importa en COW, pero no en UCDP.
# ✔ 2. Las fases del sistema internacional NO explican la frecuencia de conflictos grandes
# Esto contrasta con UCDP completo, donde ruptura sistémica sí tenía efecto.
# ✔ 3. Shock 2001 NO afecta la frecuencia de conflictos grandes
# Esto confirma que su efecto es sobre intensidad, no frecuencia.
# ✔ 4. El dataset es extremadamente escaso
# Muchos años tienen 0 conflictos grandes por zona.
# Esto limita la capacidad de detectar efectos.
# ✔ 5. Poisson es adecuado
# No hay sobredispersión.
# NB no aporta nada.
