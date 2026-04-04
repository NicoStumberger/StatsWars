library(tidyverse)
library(readr)
library(lubridate)
library(survival)
library(survminer)

# Input ------------------------------------------------------------------
anu_frec_cow <- readRDS("data_gold/anu_frec_cow.rds")
anu_frec_sdq <- readRDS("data_gold/anu_frec_sdq.rds")
anu_frec_ucdp <- readRDS("data_gold/anu_frec_ucdp.rds")

conf_durint_cow <- readRDS("data_gold/conf_durint_cow.rds")
conf_durint_sdq <- readRDS("data_gold/conf_durint_sdq.rds")
conf_durint_ucdp <- readRDS("data_gold/conf_durint_ucdp.rds")

time_between_cow <- readRDS("data_gold/time_between_cow.rds")
time_between_sdq <- readRDS("data_gold/time_between_sdq.rds")
time_between_ucdp <- readRDS("data_gold/time_between_ucdp.rds")


# EDA 1 - FRECUENCIA ---------------------------------------------------------------------
# Objetivo: ver patrones temporales, diferencias entre fases, shocks y zonas geopolíticas.

## Paso 1.1 — Serie temporal básica ---------------------------------------
ggplot(anu_frec_cow, aes(year, n_conflictos_cow)) +
  geom_line() +
  geom_point() +
  theme_minimal()

ggplot(anu_frec_ucdp, aes(year, n_conflictos_ucdp)) +
  geom_line() +
  geom_point() +
  theme_minimal()

ggplot(anu_frec_sdq, aes(year, n_conflictos_sdq)) +
  geom_line() +
  geom_point() +
  theme_minimal()

# Qué buscás:
# - rupturas visibles: Si, por lo menos 4 picos por encima de 15 conflictos
# - picos: 1885, 1912-1918, 1940-1941, 2015.
# - períodos de estabilidad: largos con pocos conflictos
# - diferencias entre datasets: similares, erraticos.
#   SDQ pareciera marcar tendencia creciente. Picos en Guerra Mundial
#   COW no tiene grandes picos
#   UCDP tiene pico en 2015.

anu_frec_all <- anu_frec_cow |>
  select(year, n_conflictos_cow) |>
  full_join(anu_frec_ucdp |> select(year, n_conflictos_ucdp), by = "year") |>
  full_join(anu_frec_sdq |> select(year, n_conflictos_sdq), by = "year")

anu_frec_all_long <- anu_frec_all |>
  pivot_longer(
    cols = starts_with("n_conflictos"),
    names_to = "dataset",
    values_to = "n_conflictos"
  )

anu_frec_all_long$dataset <- recode(
  anu_frec_all_long$dataset,
  "n_conflictos_cow" = "COW",
  "n_conflictos_ucdp" = "UCDP",
  "n_conflictos_sdq" = "SDQ"
)


ggplot(anu_frec_all_long, aes(x = year, y = n_conflictos, color = dataset)) +
  geom_line() +
  geom_point(size = 1.5) +
  theme_minimal() +
  labs(
    title = "Frecuencia anual de conflictos por dataset",
    x = "Año",
    y = "Número de conflictos",
    color = "Dataset"
  )

## Paso 1.2 — Frecuencia por fase -----------------------------------------

anu_frec_sdq |>
  # filter(!is.na(phase)) |>
  group_by(phase) |>
  summarise(
    mean_conflicts = mean(n_conflictos_sdq, na.rm = TRUE),
    n = n()
  )

anu_frec_sdq |>
  filter(!is.na(phase)) |>
  ggplot(aes(phase, n_conflictos_sdq)) +
  geom_boxplot() +
  theme_minimal()

# SDQ muestra 2 fases con más frecuencia:
# ruptura_sistema (cuenta díadas, no conflictos), marcadamente superior, y
# bipolaridad, pero con pocos anos (4)

anu_frec_cow %>%
  group_by(phase) %>%
  summarise(
    mean_conflicts = mean(n_conflictos_cow, na.rm = TRUE),
    n = n()
  )

anu_frec_cow |>
  filter(!is.na(phase)) |>
  ggplot(aes(phase, n_conflictos_cow)) +
  geom_boxplot() +
  theme_minimal()

# COW muestra una fase con frecuencia superior: unipolaridad_temprana, no muchos anos (10)
# Por que ruptura_sistema no muestra mayor frecuencia?
# H: cuenta conflicto, no evento.

anu_frec_ucdp |>
  group_by(phase) |>
  summarise(
    mean_conflicts = mean(n_conflictos_ucdp, na.rm = TRUE),
    n = n()
  )

anu_frec_ucdp |>
  filter(!is.na(phase)) |>
  ggplot(aes(phase, n_conflictos_ucdp)) +
  geom_boxplot() +
  theme_minimal()

# UCDP muestra 2 fases con mayor frecuencia:
# bipolaridad (consistente con SDQ) y
# unipolaridad_temprana (consistente con COW), tampoco muchos registros (10)
# Por que ruptura_sistema no muestra mayor frecuencia?
# H: cuenta conflicto, no evento.

# Qué buscás:
# - si las fases están bien definidas: algunas más que otras
# - si hay solapamiento fuerte:
#   COW en todas, salvo unipolaridad_temprana.
#   SDQ en entreguerras y multipolaridad_clasica.
#   UCDP en ruptura_sistema y unipolaridad_tardia_multipolaridad_emergente
# - si alguna fase tiene muy pocos años: unipolaridad_temprana
#   y bipolaridad en el caso de SDQ

## Paso 1.3 — Frecuencia antes/después de shocks --------------------------

# SDQ
ggplot(anu_frec_sdq, aes(factor(shock_1815), n_conflictos_sdq)) +
  geom_boxplot()

ggplot(anu_frec_sdq, aes(factor(shock_1914), n_conflictos_sdq)) +
  geom_boxplot()

ggplot(anu_frec_sdq, aes(factor(shock_1939), n_conflictos_sdq)) +
  geom_boxplot()

ggplot(anu_frec_sdq, aes(factor(shock_1945), n_conflictos_sdq)) +
  geom_boxplot()

# COW

ggplot(anu_frec_cow, aes(factor(shock_1914), n_conflictos_cow)) +
  geom_boxplot()

ggplot(anu_frec_cow, aes(factor(shock_1939), n_conflictos_cow)) +
  geom_boxplot()

ggplot(anu_frec_cow, aes(factor(shock_1945), n_conflictos_cow)) +
  geom_boxplot()

ggplot(anu_frec_cow, aes(factor(shock_1991), n_conflictos_cow)) +
  geom_boxplot()

ggplot(anu_frec_cow, aes(factor(shock_2001), n_conflictos_cow)) +
  geom_boxplot()

# UCDP
ggplot(anu_frec_ucdp, aes(factor(shock_1945), n_conflictos_ucdp)) +
  geom_boxplot()

ggplot(anu_frec_ucdp, aes(factor(shock_1991), n_conflictos_ucdp)) +
  geom_boxplot()

ggplot(anu_frec_ucdp, aes(factor(shock_2001), n_conflictos_ucdp)) +
  geom_boxplot()

# Qué buscás:
# - shocks que generan rupturas claras:
#   SDQ: shock_39
#   UCDP: shock_2001
# - shocks que no aportan nada (los descartás):todos los demas

## Paso 1.4 — Rimland vs no rimland (solo COW/UCDP) -----------------------

ggplot(anu_frec_cow, aes(year)) +
  geom_line(aes(y = n_rimland_1, color = "Rimland")) +
  geom_line(aes(y = n_rimland_0, color = "No Rimland")) +
  theme_minimal()


ggplot(anu_frec_ucdp, aes(year)) +
  geom_line(aes(y = n_rimland_1, color = "Rimland")) +
  geom_line(aes(y = n_rimland_0, color = "No Rimland")) +
  theme_minimal()

# Qué buscás:
# - si rimland tiene sistemáticamente más conflictos:
#   COW: es asi. UCDP: hay picos en Rimland pero los valles son similares
# - si hay períodos donde se invierte: habria q evaluar los valles.
# - si vale la pena modelarlo: SI

## Paso 1.5 — Geo zones (solo COW/UCDP) -----------------------------------

anu_frec_cow_zone_long <- anu_frec_cow |>
  select(!c(n_conflictos_cow, n_rimland_0)) |>
  pivot_longer(starts_with("n_"), names_to = "zone", values_to = "n")

ggplot(anu_frec_cow_zone_long, aes(year, n, color = zone)) +
  geom_line() +
  theme_minimal()

anu_frec_ucdp_zone_long <- anu_frec_ucdp |>
  select(!c(n_conflictos_ucdp, n_rimland_0)) |>
  pivot_longer(starts_with("n_"), names_to = "zone", values_to = "n")

ggplot(anu_frec_ucdp_zone_long, aes(year, n, color = zone)) +
  geom_line() +
  theme_minimal()

# Qué buscás:
# - si las zonas tienen patrones distintos: tiene patrones distintos
# - si alguna categoría es muy pequeña (colapsar): no se termina de visualizar

table(conf_durint_cow$geo_zone)
table(conf_durint_ucdp$geo_zone)


# EDA 2 - DURACION ---------------------------------------------------------------------
# Objetivo: ver si la duración muestra patrones claros por fase, rimland y geo_zone.

## Paso 2.1 — Distribución general ----------------------------------------

ggplot(conf_durint_sdq, aes(months)) +
  geom_histogram(bins = 50)

ggplot(conf_durint_cow, aes(months)) +
  geom_histogram(bins = 50)

ggplot(conf_durint_ucdp, aes(months)) +
  geom_histogram(bins = 50)

# Qué buscás:
# - colas largas: es asi
# - valores extremos: hay
# - si supervivencia es adecuada (lo será): como saberlo?

## Paso 2.2 — Duración por fase -------------------------------------------

conf_durint_sdq |>
  filter(!is.na(phase)) |>
  ggplot(aes(phase, months)) +
  geom_boxplot()

conf_durint_cow |>
  filter(!is.na(phase)) |>
  ggplot(aes(phase, months)) +
  geom_boxplot()

conf_durint_ucdp |>
  filter(!is.na(phase)) |>
  ggplot(aes(phase, months)) +
  geom_boxplot()

# Qué buscás:
# - si la bipolaridad tiene conflictos más largos: mas valores extremos
# - si la multipolaridad clásica tiene conflictos cortos: no

## Paso 2.3 — Kaplan-Meier por fase ---------------------------------------

fit_sdq <- survfit(Surv(months) ~ phase, data = conf_durint_sdq)
ggsurvplot(fit_sdq)

fit_cow <- survfit(Surv(months) ~ phase, data = conf_durint_cow)
ggsurvplot(fit_cow)

fit_ucdp <- survfit(Surv(months) ~ phase, data = conf_durint_ucdp)
ggsurvplot(fit_ucdp)


# Qué buscás:
# - separación clara entre curvas: algunos dataset mas q otros. Pero hay
# - cruces (problema para Cox): hay algunos cruces
# - fases que no aportan nada: como id?

## Paso 2.4 — Rimland vs no rimland (COW/UCDP) ----------------------------

ggplot(conf_durint_cow, aes(factor(rimland), months)) +
  geom_boxplot()

ggplot(conf_durint_ucdp, aes(factor(rimland), months)) +
  geom_boxplot()

# Qué buscás:
# - si rimland tiene conflictos más largos: no conclusivo
# - si vale la pena modelarlo: parece q no.

# Ver si rimland aporta algo en duración
coxph(Surv(months) ~ rimland, data = conf_durint_cow)
#            coef exp(coef) se(coef)    z     p
# rimland 0.05221   1.05360  0.08028 0.65 0.515

# Likelihood ratio test=0.43  on 1 df, p=0.514
# n= 736, number of events= 736
#    (4 observations deleted due to missingness)

coxph(Surv(months) ~ rimland, data = conf_durint_ucdp)

#            coef exp(coef) se(coef)      z     p
# rimland -0.1885    0.8282   0.1214 -1.552 0.121

# Likelihood ratio test=2.4  on 1 df, p=0.1213
# n= 281, number of events= 281
#    (22 observations deleted due to missingness)

## Paso 2.5 —  Test formal de riesgos proporcionales (Cox.zph) ------------

coxph(Surv(months) ~ phase, data = conf_durint_cow) |> cox.zph()
#        chisq df     p
# phase   11.8  5 0.038
# GLOBAL  11.8  5 0.038

coxph(Surv(months) ~ phase, data = conf_durint_ucdp) |> cox.zph()
#        chisq df       p
# phase   27.5  3 4.7e-06
# GLOBAL  27.5  3 4.7e-06

## Paso 2.6 — Duracion por SHOCK ------------------------------------------

conf_durint_cow |>
  filter(months < quantile(months, 0.95, na.rm = TRUE)) |>
  ggplot(aes(x = factor(shock_1945), y = months)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    x = "Shock 1945 (0 = antes, 1 = después)",
    y = "Duración en meses"
  )


conf_durint_ucdp |>
  ggplot(aes(x = factor(shock_1991), y = months)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    x = "Shock 1991 (0 = antes, 1 = después)",
    y = "Duración en meses"
  )

# Qué mirar:
# - ¿La mediana baja después del shock? → conflictos más cortos.
#   Sube apenas luego de 1945 en COW.
#   Baja apenas luego de 1991 en UCDP.
#   Paricera ser no concluyente.
# - ¿La dispersión cambia? → más homogéneos o más extremos.
#  quizas un poco mas homogeneos luego de 1991 segun UCDP.

# EDA 3 - INTENSIDAD ---------------------------------------------------------------------

## Paso 3.1 — Distribución general ----------------------------------------
conf_durint_sdq |>
  filter(!is.na(log_deaths)) |>
  filter(log_deaths > 0) |>
  ggplot(aes(log_deaths)) +
  geom_histogram(bins = 50)

conf_durint_cow |>
  filter(!is.na(log_deaths)) |>
  filter(log_deaths > 0) |>
  ggplot(aes(log_deaths)) +
  geom_histogram(bins = 50)

# Qué buscás:
# - si la distribución es log-normal: que seria log-normal
# - si hay outliers: hay
# - si conviene GLM gamma: ??

hist(log(conf_durint_cow$log_deaths)) # es asimetrica
# Asimetrica: usar GAMMA
# Normal: log normal

hist(log(conf_durint_sdq$log_deaths)) # es asimetrica
# Asimetrica: usar GAMMA
# Normal: log normal

## Paso 3.2 — Intensidad por fase -----------------------------------------

conf_durint_sdq |>
  filter(!is.na(phase)) |>
  ggplot(aes(phase, log_deaths)) +
  geom_boxplot()

conf_durint_cow |>
  filter(!is.na(phase)) |>
  filter(log_deaths > 0) |>
  filter(log_deaths < quantile(log_deaths, 0.95, na.rm = TRUE)) |>
  ggplot(aes(phase, log_deaths)) +
  geom_boxplot()

# Hay pocos muy altos outliers como para identificar diferencias.

# Qué buscás:
# - si la bipolaridad es más intensa:
#   bipolaridad, entreguerras y multipolaridad tienen altos outliers.
#   ruptura_sistema parece tener una media superior al resto.
# - si la unipolaridad reduce intensidad:
#   pareciera que unipolaridad (tanto temprana como tardía) tiene medias menores, sin altos outliers

conf_durint_ucdp |>
  filter(!is.na(phase)) |>
  ggplot(aes(x = phase, fill = intensity_level)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    # title = "Proporción de Minor vs War por fase",
    y = "Proporción",
    x = "Fase"
  )

# Qué te muestra:
# - En qué fases aumenta la proporción de conflictos tipo War: bipolaridad
# - Si la unipolaridad reduce la proporción de War: NO
# - Si la bipolaridad tiene más War que Minor: SI

ggplot(conf_durint_ucdp, aes(intensity_level, months)) +
  geom_boxplot() +
  theme_minimal()

# Pareciera q los conflictos Minor tienden a ser mas largos.

## Paso 3.3 — Rimland vs no rimland ---------------------------------------
conf_durint_cow |>
  filter(log_deaths < quantile(log_deaths, 0.99, na.rm = TRUE)) |>
  ggplot(aes(factor(rimland), log_deaths)) +
  geom_boxplot()
# altos outliers

conf_durint_ucdp |>
  ggplot(aes(x = rimland, fill = intensity_level)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    # title = "Proporción de Minor vs War por Rimland/No Rimland",
    y = "Proporción",
    x = "Fase"
  )

# Qué buscás:
# - si rimland es sistemáticamente más intenso: hay q quitar outliers
# - si vale la pena modelarlo: parace q no

## Paso 3.4 — Numero de eventos -------------------------------------------

ggplot(conf_durint_cow, aes(n_events)) +
  geom_histogram(bins = 40)


ggplot(conf_durint_ucdp, aes(n_events)) +
  geom_histogram(bins = 40)

# Ver si n_events correlaciona con intensidad
conf_durint_cow |>
  filter(deaths < quantile(deaths, 0.99, na.rm = TRUE)) |>
  filter(n_events < quantile(n_events, 0.99, na.rm = TRUE)) |>
  ggplot(aes(n_events, deaths)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm")


## Paso 3.5 - Intensidad por SHOCK ----------------------------------------

conf_durint_cow |>
  filter(log_deaths < quantile(log_deaths, 0.8, na.rm = TRUE)) |>
  ggplot(aes(x = factor(shock_1945), y = log_deaths)) +
  geom_boxplot() +
  theme_minimal()
# No muy claro

conf_durint_ucdp |>
  ggplot(aes(x = factor(shock_2001), fill = intensity_level)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
# Pareciera tener una mayor proporcion de War antes de 2001

# Qué mirar:
# - ¿La proporción de War baja después del shock 2001? SI
# - ¿Las muertes medianas bajan después del shock 1945? Nada claro

table(conf_durint_cow$shock_1945)


# EDA 4 - INTERVALO ENTRE CONFLICTOS ----------------------------------------

## 1. EDA general — resumen numérico ---------------------------------------

list(
  SDQ = summary(time_between_sdq$time_between),
  COW = summary(time_between_cow$time_between),
  UCDP = summary(time_between_ucdp$time_between)
)


## 2. Distribución — histogramas comparado --------------------------------
library(patchwork)

p_sdq <- ggplot(time_between_sdq, aes(time_between)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(title = "SDQ — Distribución de intervalos (meses)")

p_cow <- ggplot(time_between_cow, aes(time_between)) +
  geom_histogram(bins = 30, fill = "firebrick") +
  labs(title = "COW — Distribución de intervalos (meses)")

p_ucdp <- ggplot(time_between_ucdp, aes(time_between)) +
  geom_histogram(bins = 30, fill = "darkgreen") +
  labs(title = "UCDP — Distribución de intervalos (meses)")

p_sdq / p_cow / p_ucdp

# Qué deberías ver:
# - colas largas (propio de procesos de Poisson/Exponencial)
# - asimetría fuerte
# - muchos intervalos cortos
# - SDQ un poco raleado
# Esto es perfecto para un modelo exponencial.

## 3. Boxplots para detectar outlier --------------------------------------

bind_rows(
  time_between_sdq |> mutate(dataset = "SDQ"),
  time_between_cow |> mutate(dataset = "COW"),
  time_between_ucdp |> mutate(dataset = "UCDP")
) |>
  ggplot(aes(x = dataset, y = time_between, fill = dataset)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Comparación de intervalos entre datasets (escala log)")
# Usamos log10 porque los intervalos pueden ser enormes.

# SDQ totalmente aplanado. Es como una medicion en otra escala.

## 4. Tendencia temporal — ¿los intervalos se acortan o alargan? ----------

ggplot(time_between_sdq, aes(year, time_between)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  labs(title = "SDQ — Intervalos a lo largo del tiempo")

ggplot(time_between_cow, aes(year, time_between)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  labs(title = "COW — Intervalos a lo largo del tiempo")

ggplot(time_between_ucdp, aes(year, time_between)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  labs(title = "UCDP — Intervalos a lo largo del tiempo")


# Qué buscar:
# - ¿hay aceleración del sistema? no
# - ¿hay períodos de calma prolongada? no
# - ¿hay clustering temporal? no
# Esto te ayuda a justificar shocks y phases

## 5. Comparación por phase ------------------------------------------------

time_between_sdq |>
  filter(!is.na(phase)) |>
  ggplot(aes(phase, time_between)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "SDQ — Intervalos por fase (log)")

time_between_cow |>
  filter(!is.na(phase)) |>
  ggplot(aes(phase, time_between)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "COW — Intervalos por fase (log)")

time_between_ucdp |>
  filter(!is.na(phase)) |>
  ggplot(aes(phase, time_between)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "UCDP — Intervalos por fase (log)")

# Interpretación esperada:
# - rupturas del sistema → intervalos más cortos
# - multipolaridad clásica → intervalos largos
# - pos-1991 → intervalos largos en UCDP

## 6. Comparación por rimland (solo COW y UCDP) ---------------------------

time_between_cow |>
  ggplot(aes(factor(rimland), time_between)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "COW — Intervalos rimland vs no-rimland")

time_between_ucdp |>
  ggplot(aes(factor(rimland), time_between)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "UCDP — Intervalos rimland vs no-rimland")

# Qué esperar:
# - rimland → intervalos más cortos (mayor hazard)
# - no rimland → intervalos más largos

## 7. Comparación por shock -----------------------------------------------

ggplot(time_between_sdq, aes(factor(shock_1939), time_between)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "SDQ — Intervalos antes/después de 1939")

ggplot(time_between_sdq, aes(factor(shock_1945), time_between)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "SDQ — Intervalos antes/después de 1945")

ggplot(time_between_cow, aes(factor(shock_1939), time_between)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "COW — Intervalos antes/después de 1939")

ggplot(time_between_cow, aes(factor(shock_1945), time_between)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "COW — Intervalos antes/después de 1945")

ggplot(time_between_ucdp, aes(factor(shock_1945), time_between)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "UCDP — Intervalos antes/después de 1945")

ggplot(time_between_ucdp, aes(factor(shock_1991), time_between)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "UCDP — Intervalos antes/después de 1939")


## 8. Correlación simple con year ------------------------------------------

cor.test(time_between_sdq$time_between, time_between_sdq$year)
# se acelera
cor.test(time_between_cow$time_between, time_between_cow$year)
# no significativo
cor.test(time_between_ucdp$time_between, time_between_ucdp$year)
# no significativo

## Conclusiones -----------------------------------------------------------

# - Distribución fuertemente asimétrica → ✔
# - Muchos intervalos cortos y pocos largos → ✔
# - No hay evidencia de hazard decreciente o creciente sistemático → ✔
# - Diferencias claras por phase, rimland, shocks → Nada claro
# - No hay necesidad de Weibull (a menos que veas tendencia temporal fuerte). Igual podemos modelar
