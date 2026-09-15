# =============================================================================
# StatsWars_05_Graficos.R
# Visualizaciones para el cuerpo de la tesis
#
# Análisis Probabilístico de Conflictos Armados
# Nicolás R. Stumberger — UNTREF, Maestría en Estadística
#
# Filosofía: Tufte (1983, 2001) — alta proporción datos/tinta,
#   etiquetado directo, sin chartjunk, máxima información por píxel.
#
# Exportación: PNG (300 dpi, cumple UNTREF ≥150 dpi) + SVG (vectorial,
#   insertable en Word como objeto editable).
#
# Paquetes requeridos:
#   install.packages(c("tidyverse", "scales", "ggrepel",
#                      "survival", "broom", "patchwork"))
#
# Estructura:
#   0. Setup: tema y paleta
#   1. GRÁFICO X.1  — Cobertura temporal de los tres datasets
#   2. GRÁFICO XI.1 — Frecuencia anual de conflictos (series temporales)
#   3. GRÁFICO XI.2 — Distribución de muertes: cola larga
#   4. GRÁFICO XI.3 — Efecto del Rimland (dot plot)
#   5. GRÁFICO XI.4 — Funciones de supervivencia (Kaplan-Meier)
#   6. GRÁFICO XI.5 — Forest plot: shocks de 1945 y 1991
#   7. Exportar
# =============================================================================

library(tidyverse)
library(scales)
library(ggrepel)
library(survival)
library(broom)
library(patchwork)


# =============================================================================
# 0. SETUP: Tema Tufte y paleta
# =============================================================================

# Colores principales por dataset
COL_SDQ <- "#A93226" # terracota oscuro  — Richardson / SDQ
COL_COW <- "#1A3A5C" # azul noche        — COW
COL_UCDP <- "#1E8449" # verde bosque      — UCDP

# Colores de fases (muy suaves, solo para fondos)
COL_FASES <- c(
  "multipolaridad_clasica" = "#F7F7F2",
  "ruptura_sistema" = "#FEF0E7",
  "entreguerras" = "#EEF7FB",
  "bipolaridad" = "#EBF3FD",
  "unipolaridad_temprana" = "#EAFAF3",
  "unipolaridad_tardia_multipolaridad_emergente" = "#FDF2F0"
)

# Fuente: serif (LaTeX-feel) o sans. Cambiar a "sans" si no hay serif disponible.
FUENTE <- "serif"

# Tema base inspirado en Tufte
theme_arma <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = FUENTE) +
    theme(
      # Grilla: solo horizontal, muy suave
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "#E5E5E0", linewidth = 0.3),
      # Ejes
      axis.line.x = element_line(color = "#444444", linewidth = 0.4),
      axis.ticks.x = element_line(color = "#444444", linewidth = 0.3),
      axis.ticks.length = unit(3, "pt"),
      axis.ticks.y = element_blank(),
      # Texto
      axis.title = element_text(size = rel(0.82), color = "#333333"),
      axis.text = element_text(size = rel(0.78), color = "#444444"),
      plot.title = element_text(
        size = rel(1.0),
        face = "bold",
        color = "#111111",
        hjust = 0,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        size = rel(0.82),
        color = "#555555",
        hjust = 0,
        margin = margin(b = 8)
      ),
      plot.caption = element_text(
        size = rel(0.70),
        color = "#888888",
        hjust = 0,
        margin = margin(t = 8)
      ),
      strip.text = element_text(
        size = rel(0.82),
        face = "bold",
        color = "#222222",
        hjust = 0
      ),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = rel(0.80), color = "#444444"),
      legend.key.size = unit(10, "pt"),
      plot.margin = margin(14, 18, 12, 14),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# Helper: bandas de fase como lista de geoms
# xmin_cap / xmax_cap: recortar al rango del gráfico
bandas_fase <- function(xmin_cap = 1800, xmax_cap = 2030, alpha = 0.55) {
  fases_df <- tibble(
    xmin = c(1815, 1914, 1919, 1939, 1946, 1992, 2002),
    xmax = c(1913, 1918, 1938, 1945, 1991, 2001, 2024),
    phase = c(
      "multipolaridad_clasica",
      "ruptura_sistema",
      "entreguerras",
      "ruptura_sistema",
      "bipolaridad",
      "unipolaridad_temprana",
      "unipolaridad_tardia_multipolaridad_emergente"
    )
  ) |>
    filter(xmax >= xmin_cap, xmin <= xmax_cap) |>
    mutate(
      xmin = pmax(xmin, xmin_cap),
      xmax = pmin(xmax, xmax_cap),
      fill = COL_FASES[phase]
    )

  # Un annotate() por fase: recibe escalares → no hay conflicto de longitud
  # Funciona en facets porque annotate se dibuja en todos los paneles
  purrr::map(seq_len(nrow(fases_df)), function(i) {
    annotate(
      "rect",
      xmin = fases_df$xmin[i],
      xmax = fases_df$xmax[i],
      ymin = -Inf,
      ymax = Inf,
      fill = fases_df$fill[i],
      alpha = alpha
    )
  })
}

lineas_shock <- function(años = c(1945, 1991), ...) {
  geom_vline(
    xintercept = años,
    linetype = "dashed",
    color = "#666666",
    linewidth = 0.38,
    ...
  )
}

# =============================================================================
# 1. INPUT — Cargar datos gold
# =============================================================================

anu_frec_sdq <- readRDS("Data/data_gold/anu_frec_sdq.rds")
anu_frec_cow <- readRDS("Data/data_gold/anu_frec_cow.rds")
anu_frec_ucdp <- readRDS("Data/data_gold/anu_frec_ucdp.rds")

conf_durint_sdq <- readRDS("Data/data_gold/conf_durint_sdq.rds")
conf_durint_cow <- readRDS("Data/data_gold/conf_durint_cow.rds")
conf_durint_ucdp <- readRDS("Data/data_gold/conf_durint_ucdp.rds")

time_between_sdq <- readRDS("Data/data_gold/time_between_sdq.rds")
time_between_cow <- readRDS("Data/data_gold/time_between_cow.rds")
time_between_ucdp <- readRDS("Data/data_gold/time_between_ucdp.rds")

# =============================================================================
# TABLA — Resumen de hipótesis y resultados
# Requiere: install.packages(c("flextable", "officer"))
# =============================================================================
library(flextable)
library(officer)

hip_df <- tribble(
  ~Código , ~Hipótesis                                                               , ~SDQ            , ~COW            , ~UCDP           , ~`Resultado global` ,
  "H 1.1" , "Frecuencia anual compatible con familia Poisson"                        , "Confirmada"    , "Confirmada"    , "Confirmada"    , "Confirmada"        ,
  "H 1.2" , "Factores estructurales, geográficos e históricos afectan la tasa anual" , "Parcial"       , "Parcial"       , "Parcial"       , "Parcial"           ,
  "H 1.3" , "El orden de 1945 redujo la frecuencia anual de conflictos"              , "No confirmada" , "No confirmada" , "No confirmada" , "No confirmada"     ,
  "H 2.1" , "Intervalo entre conflictos compatible con familia Exponencial"          , "Parcial"       , "Confirmada"    , "Parcial"       , "Parcial"           ,
  "H 2.2" , "Factores estructurales, geográficos e históricos afectan el intervalo"  , "Parcial"       , "Parcial"       , "Parcial"       , "Parcial"           ,
  "H 2.3" , "El orden de 1945 aumentó el intervalo entre conflictos"                 , "No confirmada" , "No confirmada" , "No confirmada" , "No confirmada"     ,
  "H 3.1" , "Duración compatible con proceso de supervivencia paramétrico"           , "Parcial"       , "Parcial"       , "Parcial"       , "Parcial"           ,
  "H 3.2" , "Factores estructurales, geográficos e históricos afectan la duración"   , "Parcial"       , "Parcial"       , "Parcial"       , "Parcial"           ,
  "H 3.3" , "El orden de 1945 produjo un cambio en la duración de los conflictos"    , "Confirmada*"   , "No confirmada" , "No confirmada" , "No confirmada"     ,
  "H 4.1" , "Intensidad varía según la estructura del sistema internacional"         , "Parcial"       , "Parcial"       , "Parcial"       , "Parcial"           ,
  "H 4.2" , "Geografía estratégica y shocks históricos afectan la intensidad"        , "Parcial"       , "Parcial"       , "Parcial"       , "Parcial"           ,
  "H 4.3" , "El orden de 1945 produjo un cambio en la intensidad de los conflictos"  , "No confirmada" , "No confirmada" , "No confirmada" , "No confirmada"
)

cols_resultado <- c("SDQ", "COW", "UCDP", "Resultado global")

ft <- flextable(hip_df) |>
  # Ancho de columnas
  width(j = "Código", width = 0.7) |>
  width(j = "Hipótesis", width = 3.8) |>
  width(j = cols_resultado, width = 1.1) |>
  # Alineación
  align(j = cols_resultado, align = "center", part = "all") |>
  align(j = "Código", align = "center", part = "all") |>
  # Fuente general
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 10, part = "all") |>
  fontsize(size = 11, part = "header") |>
  bold(part = "header") |>
  # --- Colores SDQ ---
  bg(j = "SDQ", i = ~ SDQ == "Confirmada", bg = "#A8D5E8") |>
  bg(j = "SDQ", i = ~ SDQ == "Confirmada*", bg = "#A8D5E8") |>
  bg(j = "SDQ", i = ~ SDQ == "Parcial", bg = "#A8DBC0") |>
  bg(j = "SDQ", i = ~ SDQ == "Parcial*", bg = "#C8E6F5") |>
  bg(j = "SDQ", i = ~ SDQ == "No confirmada", bg = "#E8A8AB") |>
  # --- Colores COW ---
  bg(j = "COW", i = ~ COW == "Confirmada", bg = "#A8D5E8") |>
  bg(j = "COW", i = ~ COW == "Confirmada*", bg = "#A8D5E8") |>
  bg(j = "COW", i = ~ COW == "Parcial", bg = "#A8DBC0") |>
  bg(j = "COW", i = ~ COW == "Parcial*", bg = "#C8E6F5") |>
  bg(j = "COW", i = ~ COW == "No confirmada", bg = "#E8A8AB") |>
  # --- Colores UCDP ---
  bg(j = "UCDP", i = ~ UCDP == "Confirmada", bg = "#A8D5E8") |>
  bg(j = "UCDP", i = ~ UCDP == "Confirmada*", bg = "#A8D5E8") |>
  bg(j = "UCDP", i = ~ UCDP == "Parcial", bg = "#A8DBC0") |>
  bg(j = "UCDP", i = ~ UCDP == "Parcial*", bg = "#C8E6F5") |>
  bg(j = "UCDP", i = ~ UCDP == "No confirmada", bg = "#E8A8AB") |>
  # --- Colores Resultado global ---
  bg(
    j = "Resultado global",
    i = ~ `Resultado global` == "Confirmada",
    bg = "#A8D5E8"
  ) |>
  bg(
    j = "Resultado global",
    i = ~ `Resultado global` == "Confirmada*",
    bg = "#A8D5E8"
  ) |>
  bg(
    j = "Resultado global",
    i = ~ `Resultado global` == "Parcial",
    bg = "#A8DBC0"
  ) |>
  bg(
    j = "Resultado global",
    i = ~ `Resultado global` == "Parcial*",
    bg = "#C8E6F5"
  ) |>
  bg(
    j = "Resultado global",
    i = ~ `Resultado global` == "No confirmada",
    bg = "#E8A8AB"
  ) |>
  # Texto negro en celdas coloreadas
  color(j = cols_resultado, color = "#111111") |>
  bold(j = cols_resultado) |>
  # Separadores entre grupos de hipótesis
  border(i = 3, border.bottom = fp_border(color = "#444444", width = 1.5)) |>
  border(i = 6, border.bottom = fp_border(color = "#444444", width = 1.5)) |>
  border(i = 9, border.bottom = fp_border(color = "#444444", width = 1.5)) |>
  # Borde exterior
  border_outer(border = fp_border(color = "#222222", width = 2)) |>
  # Padding
  padding(padding = 5, part = "all") |>
  # Nota al pie
  add_footer_lines(
    values = c(
      "* Con cautela: efecto significativo en modelos simple y full, pero cobertura temporal limitada (SDQ: sólo 4 años post-1945).",
      "Azul: confirmada. Verde: parcial. Rojo: no confirmada.",
      "Fuente: elaboración propia."
    )
  ) |>
  fontsize(size = 9, part = "footer") |>
  font(fontname = "Times New Roman", part = "footer") |>
  color(color = "#555555", part = "footer")

# Exportar como .docx
doc <- read_docx() |>
  body_add_par(
    "Tabla — Resumen de hipótesis y resultados",
    style = "heading 2"
  ) |>
  body_add_flextable(ft)

print(doc, target = "graficos/tabla_hipotesis.docx")
message("✅ tabla_hipotesis.docx exportada en graficos/")


# =============================================================================
# GRÁFICO 7.1 — Cobertura temporal de los tres datasets
# Ubicación: Capítulo 7, Sección 7.b.5 (Triangulación cuantitativa)
# Mensaje: qué cubre cada fuente, dónde se puede comparar, dónde no.
# =============================================================================

# Calcular n y tasas para etiquetas del gráfico X.1
n_sdq <- nrow(conf_durint_sdq)
n_cow <- nrow(conf_durint_cow)
n_ucdp <- nrow(conf_durint_ucdp)

años_sdq <- 1949 - 1807 + 1 # 143
años_cow <- 2014 - 1816 + 1 # 199
años_ucdp <- 2024 - 1939 + 1 #  86

tasa_sdq <- round(n_sdq / años_sdq, 1)
tasa_cow <- round(n_cow / años_cow, 1)
tasa_ucdp <- round(n_ucdp / años_ucdp, 1)

# Datos de cobertura
# Datos de cobertura — CORREGIDO: UCDP desde 1939
cobertura_df <- tibble(
  dataset = factor(
    c(
      "SDQ\n(Richardson, 1960)",
      "COW\n(Singer & Small, 1972)",
      "UCDP\n(Uppsala, 2024)"
    ),
    levels = c(
      "UCDP\n(Uppsala, 2024)",
      "COW\n(Singer & Small, 1972)",
      "SDQ\n(Richardson, 1960)"
    )
  ),
  y = c(1, 2, 3),
  xmin = c(1807, 1816, 1939),
  xmax = c(1949, 2014, 2024),
  col = c(COL_SDQ, COL_COW, COL_UCDP),
  umbral = c("≥1 muerte", "≥1.000 muertes/12 m", "≥25 muertes/año"),
  n_label = c(
    paste0(
      "SDQ (Richardson, 1960)\n",
      n_sdq,
      " conflictos (díadas) en ",
      años_sdq,
      " años ≈ ",
      tasa_sdq,
      "/año"
    ),
    paste0(
      "COW (Singer & Small, 1972)\n",
      n_cow,
      " conflictos en ",
      años_cow,
      " años ≈ ",
      tasa_cow,
      "/año"
    ),
    paste0(
      "UCDP (Uppsala, 2024)\n",
      n_ucdp,
      " conflictos en ",
      años_ucdp,
      " años ≈ ",
      tasa_ucdp,
      "/año"
    )
  )
)

# Shocks: todos centrados sobre su línea, alturas escalonadas para 1939/1945
shocks_x1 <- tibble(
  x = c(1914, 1939, 1945, 1991),
  y_lab = c(4.50, 5.10, 4.50, 4.80), # 1939 arriba, 1945 abajo — sin overlap
  hjust = c(0.5, 0.5, 0.5, 0.5),
  label = c("I GM\n1914", "II GM\n1939", "ONU\n1945", "Fin Guerra Fría\n1991")
)

# Etiquetas de fases — "Ruptura" eliminada, reemplazada por una sola etiqueta con conectores
fases_label <- tibble(
  x = c(1864, 1928, 1968, 1996, 2013),
  label = c(
    "Multipolaridad\nclásica",
    "Entre-\nguerras",
    "Bipolaridad",
    "Unip.\ntemp.",
    "Unip. tardía /\nmultip. emergente"
  )
)

# Etiqueta única de "Ruptura del sistema" con segmentos hacia ambas franjas
ruptura_x <- 1928 # posición horizontal de la etiqueta (entre las dos rupturas)
ruptura_y <- -0.55 # debajo del eje
ruptura_fin1_x <- 1916 # centro de la franja 1914–1918
ruptura_fin2_x <- 1942 # centro de la franja 1939–1945

p_x1 <- ggplot() +
  # Bandas de fase
  bandas_fase(xmin_cap = 1807, xmax_cap = 2024, alpha = 0.55) +
  # Barras de cobertura
  geom_segment(
    data = cobertura_df,
    aes(x = xmin, xend = xmax, y = y, yend = y),
    linewidth = 13,
    lineend = "round",
    color = cobertura_df$col
  ) +
  # Años dentro de la barra en blanco
  geom_text(
    data = cobertura_df,
    aes(x = xmin + 2, y = y, label = xmin),
    hjust = 0,
    size = 4.2,
    family = FUENTE,
    color = "white",
    fontface = "bold"
  ) +
  geom_text(
    data = cobertura_df,
    aes(x = xmax - 2, y = y, label = xmax),
    hjust = 1,
    size = 4.2,
    family = FUENTE,
    color = "white",
    fontface = "bold"
  ) +
  # Umbral (izquierda)
  geom_text(
    data = cobertura_df,
    aes(x = xmin - 12, y = y, label = umbral),
    hjust = 1,
    size = 4.0,
    family = FUENTE,
    color = "#555555"
  ) +
  # Nombre + n + tasa (derecha)
  geom_text(
    data = cobertura_df,
    aes(x = xmax + 12, y = y, label = n_label),
    hjust = 0,
    size = 4.0,
    family = FUENTE,
    color = "#333333",
    lineheight = 1.15
  ) +
  # Líneas verticales de shocks
  geom_vline(
    xintercept = c(1914, 1939, 1945, 1991),
    linetype = "dashed",
    color = "#777777",
    linewidth = 0.38
  ) +
  # Etiquetas de shocks
  geom_text(
    data = shocks_x1,
    aes(x = x, y = y_lab, label = label, hjust = hjust),
    size = 3.8,
    family = FUENTE,
    color = "#444444",
    lineheight = 0.88
  ) +
  # Etiquetas de fases normales (abajo)
  geom_text(
    data = fases_label,
    aes(x = x, y = -0.25, label = label),
    hjust = 0.5,
    size = 3.2,
    family = FUENTE,
    color = "#888888",
    lineheight = 0.85
  ) +
  # Etiqueta única "Ruptura del sistema"
  annotate(
    "text",
    x = 1928,
    y = ruptura_y - 0.35,
    label = "Ruptura\ndel sistema",
    hjust = 0.5,
    size = 3.2,
    family = FUENTE,
    color = "#888888",
    lineheight = 0.85
  ) +
  # Segmento izquierdo
  annotate(
    "segment",
    x = 1922,
    xend = 1916,
    y = ruptura_y - 0.15,
    yend = -0.08,
    color = "#AAAAAA",
    linewidth = 0.4
  ) +
  # Segmento derecho
  annotate(
    "segment",
    x = 1934,
    xend = 1942,
    y = ruptura_y - 0.15,
    yend = -0.08,
    color = "#AAAAAA",
    linewidth = 0.4
  ) +
  scale_x_continuous(
    limits = c(1760, 2100),
    breaks = seq(1820, 2020, 20),
    expand = c(0, 0)
  ) +
  scale_y_continuous(limits = c(-1.2, 5.8), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_arma(base_size = 15) +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line.x = element_line(color = "#BBBBBB")
  )

p_x1


# =============================================================================
# GRÁFICO 8.1 — Frecuencia anual de conflictos (tres paneles)
# Ubicación: Capítulo 8, Sección 8.1 (Frecuencia)
# Mensaje: el no-efecto de 1945 es visible antes de que el modelo lo confirme.
# =============================================================================

# --- Pre-gráfico XI.1 ---

eventos_labels <- tribble(
  ~dataset           , ~year , ~label                   ,
  "SDQ (1807–1949)"  ,  1853 , "Crimea\n1853"           ,
  "SDQ (1807–1949)"  ,  1914 , "I GM\n1914"             ,
  "SDQ (1807–1949)"  ,  1939 , "II GM\n1939"            ,
  "COW (1816–2014)"  ,  1950 , "Korea\n1950"            ,
  "COW (1816–2014)"  ,  1965 , "Vietnam\n1965"          ,
  "COW (1816–2014)"  ,  1992 , "Balcanes\n1992"         ,
  "UCDP (1939–2024)" ,  1994 , "Rwanda\n1994"           ,
  "UCDP (1939–2024)" ,  2015 , "Estado\nIslámico\n2015" ,
  "UCDP (1939–2024)" ,  2022 , "Ucrania\n2022"
)

frec_largo <- bind_rows(
  anu_frec_sdq |>
    select(year, n = n_conflictos_sdq) |>
    mutate(dataset = "SDQ (1807–1949)"),
  anu_frec_cow |>
    select(year, n = n_conflictos_cow) |>
    mutate(dataset = "COW (1816–2014)"),
  anu_frec_ucdp |>
    select(year, n = n_conflictos_ucdp) |>
    mutate(dataset = "UCDP (1939–2024)")
) |>
  mutate(
    dataset = factor(
      dataset,
      levels = c("SDQ (1807–1949)", "COW (1816–2014)", "UCDP (1939–2024)")
    )
  )

COL_DATASET <- c(
  "SDQ (1807–1949)" = COL_SDQ,
  "COW (1816–2014)" = COL_COW,
  "UCDP (1939–2024)" = COL_UCDP
)

tips <- bind_rows(
  anu_frec_sdq |>
    select(year, y_tip = n_conflictos_sdq) |>
    mutate(dataset = "SDQ (1807–1949)"),
  anu_frec_cow |>
    select(year, y_tip = n_conflictos_cow) |>
    mutate(dataset = "COW (1816–2014)"),
  anu_frec_ucdp |>
    select(year, y_tip = n_conflictos_ucdp) |>
    mutate(dataset = "UCDP (1939–2024)")
) |>
  mutate(dataset = factor(dataset, levels = levels(frec_largo$dataset)))

offset_df <- frec_largo |>
  group_by(dataset) |>
  summarise(offset = max(n, na.rm = TRUE) * 0.60)

eventos_frec <- eventos_labels |>
  mutate(dataset = factor(dataset, levels = levels(frec_largo$dataset))) |>
  left_join(tips, by = c("dataset", "year")) |>
  left_join(offset_df, by = "dataset") |>
  mutate(y_label = y_tip + offset)


# --- Gráfico XI.1 ---

p_xi1 <- ggplot(frec_largo, aes(x = year, y = n)) +
  bandas_fase(xmin_cap = 1807, xmax_cap = 2024) +
  geom_area(aes(fill = dataset), alpha = 0.18) +
  geom_line(aes(color = dataset), linewidth = 0.6) +
  lineas_shock(años = c(1945, 1991)) +
  geom_segment(
    data = eventos_frec,
    aes(
      x = year,
      xend = year,
      y = y_label - offset * 0.12,
      yend = y_tip + 0.3,
      color = dataset
    ),
    linewidth = 0.35,
    alpha = 0.6
  ) +
  geom_text(
    data = eventos_frec,
    aes(x = year, y = y_label, label = label, color = dataset),
    size = 3.2,
    family = FUENTE,
    lineheight = 0.88,
    fontface = "bold"
  ) +
  facet_wrap(~dataset, ncol = 1, scales = "free_y") +
  scale_color_manual(values = COL_DATASET) +
  scale_fill_manual(values = COL_DATASET) +
  scale_x_continuous(
    limits = c(1807, 2024),
    breaks = c(
      1807,
      1816,
      1840,
      1860,
      1880,
      1900,
      1920,
      1939,
      1949,
      1960,
      1980,
      2000,
      2014,
      2024
    ),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    x = NULL,
    y = "Conflictos iniciados por año"
  ) +
  theme_arma(base_size = 14) +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.4, "lines")
  )

p_xi1


# =============================================================================
# GRÁFICO 8.2 — Distribución de muertes: cola larga
# Ubicación: Capítulo 8, Sección 8.4 (Intensidad)
# Mensaje: la escala logarítmica revela estructura; en escala original
#          la cola domina y ninguna distribución ordinaria ajusta bien.
# Nota: SDQ filtrado al percentil 95 para visualización (no afecta modelos)
# =============================================================================

# Límite visual: p90 de SDQ (sin filtrar datos, solo recorte de vista)
x_max <- quantile(
  log(conf_durint_sdq$deaths[conf_durint_sdq$deaths > 0] + 1),
  0.90,
  na.rm = TRUE
)

muertes_df <- bind_rows(
  conf_durint_sdq |>
    filter(!is.na(deaths), deaths > 0) |>
    mutate(log_deaths = log(deaths + 1), dataset = "SDQ"),
  conf_durint_cow |>
    filter(!is.na(deaths), deaths > 0) |>
    mutate(log_deaths = log(deaths + 1), dataset = "COW")
) |>
  mutate(dataset = factor(dataset, levels = c("SDQ", "COW")))

# Medianas en escala original
medianas <- muertes_df |>
  group_by(dataset) |>
  summarise(
    mediana_ln = median(log_deaths, na.rm = TRUE),
    mediana_orig = format(
      round(exp(median(log_deaths, na.rm = TRUE))),
      big.mark = ".",
      decimal.mark = ","
    )
  )

COL_MUERTES <- c("SDQ" = COL_SDQ, "COW" = COL_COW)

p_xi2 <- ggplot(
  muertes_df,
  aes(x = log_deaths, fill = dataset, color = dataset)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 45,
    alpha = 0.35,
    position = "identity"
  ) +
  geom_density(linewidth = 0.7, alpha = 0) +
  geom_vline(
    data = medianas,
    aes(xintercept = mediana_ln, color = dataset),
    linetype = "dotted",
    linewidth = 0.6
  ) +

  # --- Etiquetas SDQ / COW — más grandes ---
  annotate(
    "text",
    x = 5.0,
    y = 0.85,
    label = "SDQ",
    color = COL_SDQ,
    fontface = "bold",
    size = 5.5, # ← más grande
    family = FUENTE
  ) +
  annotate(
    "text",
    x = 12.5,
    y = 0.18,
    label = "COW",
    color = COL_COW,
    fontface = "bold",
    size = 5.5, # ← más grande
    family = FUENTE
  ) +

  # --- Etiquetas de mediana — separadas verticalmente ---
  annotate(
    "text",
    x = medianas$mediana_ln[medianas$dataset == "SDQ"] - 0.15,
    y = 0.38,
    label = paste0(
      "Mediana SDQ: ",
      medianas$mediana_orig[medianas$dataset == "SDQ"],
      " muertes"
    ),
    color = COL_SDQ,
    hjust = 1, # a la izquierda de la línea
    size = 2.5,
    family = FUENTE
  ) +
  annotate(
    "text",
    x = medianas$mediana_ln[medianas$dataset == "COW"] + 0.15,
    y = 0.30,
    label = paste0(
      "Mediana COW: ",
      medianas$mediana_orig[medianas$dataset == "COW"],
      " muertes"
    ),
    color = COL_COW,
    hjust = 0, # a la derecha de la línea
    size = 2.5,
    family = FUENTE
  ) +

  scale_x_continuous(
    breaks = log(c(100, 1e3, 1e4, 1e5, 1e6) + 1),
    labels = c("100", "1.000", "10.000", "100.000", "1M"),
    name = "Muertes por conflicto (escala logarítmica, etiquetas en escala original)"
  ) +
  scale_fill_manual(values = COL_MUERTES) +
  scale_color_manual(values = COL_MUERTES) +
  labs(y = "Densidad") +
  coord_cartesian(xlim = c(log(100 + 1), x_max)) + # ← recorte sin filtrar datos
  theme_arma() +
  theme(legend.position = "none")

p_xi2

summary(conf_durint_sdq$deaths)
# Si max ≈ 7 → deaths era log10 (magnitud Richardson)
# Si max ≈ millones → deaths son muertes crudas → Imagen 2 es correcta
cat("P90 SDQ en muertes:", round(exp(x_max)), "\n")


# =============================================================================
# GRÁFICO 8.3 — Efecto del Rimland: dot plot
# Ubicación: Capítulo XI, Sección XI.2 (Frecuencia) y/o XI.5 (Intensidad)
# Mensaje: el efecto geográfico en COW es robusto en frecuencia e intensidad.
#          En UCDP no alcanza significancia estadística.
#
# NOTA: valores extraídos de los modelos Poisson (frecuencia) y LM/logístico
#       (intensidad) documentados en Capítulo XI y Anexos 1 y 4.
# =============================================================================

rimland_data <- tribble(
  ~dataset , ~dimension            , ~estimacion , ~ci_low    , ~ci_high   , ~p_val ,
  # Frecuencia (IRR)
  "COW"    , "Frecuencia\n(IRR)"   , 2.30        , 1.50       , 3.60       , 0.001  , # robusto: Poisson, qP, NB
  "UCDP"   , "Frecuencia\n(IRR)"   , 1.00        , 0.70       , 1.40       , 0.960  , # TODO: confirmar CI de UCDP
  # Intensidad
  "COW"    , "Intensidad\n(exp β)" , exp(0.335)  , exp(0.083) , exp(0.587) , 0.009  ,
  "UCDP"   , "Intensidad\n(OR)"    , 1.298       , 0.726      , 2.325      , 0.383
) |>
  mutate(
    dataset = factor(dataset, levels = c("COW", "UCDP")),
    dimension = factor(
      dimension,
      levels = c("Frecuencia\n(IRR)", "Intensidad\n(exp β)", "Intensidad\n(OR)")
    ),
    sig = ifelse(p_val < 0.05, "Significativo", "No significativo"),
    col_punto = case_when(
      dataset == "COW" ~ COL_COW,
      dataset == "UCDP" ~ COL_UCDP
    )
  )

p_xi3 <- ggplot(rimland_data, aes(x = estimacion, y = dataset)) +
  # Línea de efecto nulo
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "#BBBBBB",
    linewidth = 0.5
  ) +
  # IC
  geom_errorbarh(
    aes(xmin = ci_low, xmax = ci_high, color = dataset),
    height = 0.18,
    linewidth = 0.65,
    na.rm = TRUE
  ) +
  # Punto central
  geom_point(aes(color = dataset, shape = sig), size = 4) +
  # Etiqueta del valor
  geom_text(
    aes(x = ci_high + 0.05, label = round(estimacion, 2), color = dataset),
    hjust = 0,
    size = 2.8,
    family = FUENTE
  ) +
  facet_wrap(~dimension, ncol = 1, scales = "free_x") +
  scale_color_manual(values = c("COW" = COL_COW, "UCDP" = COL_UCDP)) +
  scale_shape_manual(
    values = c("Significativo" = 16, "No significativo" = 1),
    name = NULL
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  labs(
    title = "Gráfico XI.3 — Efecto del Rimland sobre frecuencia e intensidad",
    subtitle = paste(
      "Punto sólido: significativo (p<0,05). Punto hueco: no significativo.",
      "Segmento: intervalo de confianza 95%. Línea punteada: efecto nulo (ratio = 1).",
      sep = "\n"
    ),
    caption = "Fuente: modelos Poisson (Anexo 1) y LM/logístico (Anexo 4). COW y UCDP. Elaboración propia.",
    x = "Razón de efecto respecto a conflictos fuera del Rimland",
    y = NULL
  ) +
  theme_arma() +
  theme(
    panel.grid.major.x = element_line(color = "#EEEEEE", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

p_xi3


# =============================================================================
# GRÁFICO XI.4 — Funciones de supervivencia (Kaplan-Meier)
# Ubicación: Capítulo XI, Secciones XI.3 (Intervalo) y XI.4 (Duración)
# Mensaje: el shock de 1991 acorta tanto el intervalo como la duración en UCDP;
#          el efecto es visualmente inmediato en las curvas.
# =============================================================================

# --- Preparar datos de supervivencia ---

# Intervalo: reemplazar ceros por 0.5 (limitación de resolución temporal)
tb_cow <- time_between_cow |>
  mutate(
    t = pmax(time_between, 0.5),
    post91 = factor(shock_1991, labels = c("Pre-1991", "Post-1991"))
  )
tb_ucdp <- time_between_ucdp |>
  mutate(
    t = pmax(time_between, 0.5),
    post91 = factor(shock_1991, labels = c("Pre-1991", "Post-1991"))
  )

# Duración
dur_cow <- conf_durint_cow |>
  filter(!is.na(months), months > 0) |>
  mutate(
    t = months,
    post91 = factor(shock_1991, labels = c("Pre-1991", "Post-1991"))
  )
dur_ucdp <- conf_durint_ucdp |>
  filter(!is.na(months), months > 0) |>
  mutate(
    t = months,
    post91 = factor(shock_1991, labels = c("Pre-1991", "Post-1991"))
  )

# --- Estimar KM y convertir a data frame con broom::tidy ---
tidy_km <- function(data, dataset_label, dim_label) {
  fit <- survfit(Surv(t) ~ post91, data = data)
  tidy(fit) |>
    mutate(
      dataset = dataset_label,
      dimension = dim_label,
      strata = str_replace(strata, "post91=", "")
    )
}

km_df <- bind_rows(
  tidy_km(tb_cow, "COW", "Intervalo entre conflictos"),
  tidy_km(tb_ucdp, "UCDP", "Intervalo entre conflictos"),
  tidy_km(dur_cow, "COW", "Duración de conflictos"),
  tidy_km(dur_ucdp, "UCDP", "Duración de conflictos")
) |>
  mutate(
    dataset = factor(dataset, levels = c("COW", "UCDP")),
    dimension = factor(
      dimension,
      levels = c("Intervalo entre conflictos", "Duración de conflictos")
    ),
    strata = factor(strata, levels = c("Pre-1991", "Post-1991"))
  )

# Paleta KM: gris (pre) / rojo (post)
COL_KM <- c("Pre-1991" = "#999999", "Post-1991" = "#C0392B")
LTY_KM <- c("Pre-1991" = "dashed", "Post-1991" = "solid")

p_xi4 <- ggplot(
  km_df,
  aes(x = time, y = estimate, color = strata, linetype = strata)
) +
  geom_step(linewidth = 0.7) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = strata),
    alpha = 0.07,
    color = NA
  ) +
  facet_grid(dataset ~ dimension, scales = "free_x") +
  scale_color_manual(values = COL_KM) +
  scale_fill_manual(values = COL_KM) +
  scale_linetype_manual(values = LTY_KM) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25)
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Gráfico XI.4 — Funciones de supervivencia antes y después de 1991",
    subtitle = paste(
      "Estimador Kaplan-Meier. Pre-1991 (gris, punteado) vs. Post-1991 (rojo, sólido).",
      "Banda sombreada: intervalo de confianza 95%.",
      sep = "\n"
    ),
    caption = "Fuente: COW y UCDP. Elaboración propia.",
    x = "Tiempo (meses)",
    y = "Probabilidad de supervivencia S(t)"
  ) +
  theme_arma() +
  theme(
    panel.grid.major.y = element_line(color = "#EEEEEE", linewidth = 0.3),
    legend.position = "bottom"
  )

p_xi4


# =============================================================================
# GRÁFICO XI.5 — Forest plot: efectos de los shocks 1945 y 1991
# Ubicación: Capítulo XI, Sección XI.6
# Mensaje: 1945 → nulo en todas las dimensiones y fuentes;
#          1991 → significativo en duración e intensidad (UCDP).
#
# FUENTE DE LOS VALORES:
#   Extraídos manualmente de los modelos documentados en Capítulo XI y Anexos.
#   Campos marcados con TODO deben completarse con el output de los modelos
#   de duración (Anexo 3) que no están disponibles en este script.
#   Para los modelos de intervalo: ver Anexo 2 (sección A2.3).
#   Para frecuencia: ver Anexo 1 (sección A1.3).
#   Para intensidad: ver Anexo 4 (sección A4.3).
#
# Métrica por dimensión:
#   Frecuencia → IRR  (Incidence Rate Ratio, Poisson)
#   Intervalo  → TR   (Time Ratio, modelos de supervivencia paramétricos)
#   Duración   → TR   (ídem)
#   Intensidad → exp(β) para SDQ/COW; OR para UCDP
#
# Referencia de efecto nulo: 1 en todos los casos (escala logarítmica en eje x)
# =============================================================================

forest_raw <- tribble(
  ~shock , ~dataset , ~dimension   , ~est        , ~lo         , ~hi         , ~p    , ~metrica ,
  # ---- SHOCK 1945 ----
  # Frecuencia
  "1945" , "COW"    , "Frecuencia" , 1.010       , 0.820       ,  1.240      , 0.940 , "IRR"    ,
  # IRR SDQ: absorbido por fases en modelo conjunto — no incluir
  # Intervalo
  "1945" , "SDQ"    , "Intervalo"  , 2.075       , 0.665       ,  6.474      , 0.209 , "TR"     ,
  "1945" , "COW"    , "Intervalo"  , 1.369       , 0.169       , 11.090      , 0.769 , "TR"     ,
  # Duración — TODO: completar con output de Anexo 3
  # "1945", "SDQ",  "Duración",  TODO,   TODO,    TODO,    TODO,   "TR",
  # "1945", "COW",  "Duración",  TODO,   TODO,    TODO,    TODO,   "TR",
  # Intensidad
  "1945" , "SDQ"    , "Intensidad" , exp(-0.490) , exp(-0.973) , exp(-0.006) , 0.047 , "exp(β)" ,
  "1945" , "COW"    , "Intensidad" , exp(0.195)  , exp(-0.046) , exp(0.435)  , 0.113 , "exp(β)" ,
  # ---- SHOCK 1991 ----
  # Intervalo
  "1991" , "UCDP"   , "Intervalo"  , 0.299       , 0.232       ,  0.386      , 0.001 , "TR"     ,
  # Duración — TODO: completar con output de Anexo 3
  # "1991", "UCDP", "Duración",  TODO,   TODO,    TODO,    TODO,   "TR",
  # Intensidad
  "1991" , "UCDP"   , "Intensidad" , 0.417       , 0.201       ,  0.864      , 0.012 , "OR"
) |>
  mutate(
    shock = factor(shock, levels = c("1945", "1991")),
    dataset = factor(dataset, levels = c("SDQ", "COW", "UCDP")),
    dimension = factor(
      dimension,
      levels = c("Frecuencia", "Intervalo", "Duración", "Intensidad")
    ),
    sig = ifelse(p < 0.05, "p < 0,05", "p ≥ 0,05"),
    # Etiqueta: Dataset + dimensión
    label = paste0(dataset, " · ", dimension)
  )

# Paleta de significancia
COL_SIG <- c("p < 0,05" = "#A93226", "p ≥ 0,05" = "#999999")

# Etiquetas de panel
shock_labels <- c(
  "1945" = "Shock de 1945 — ONU / inicio de la bipolaridad",
  "1991" = "Shock de 1991 — fin de la Guerra Fría"
)

p_xi5 <- ggplot(
  forest_raw,
  aes(x = est, y = fct_rev(label), color = sig, shape = sig)
) +
  # Línea de efecto nulo
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "#BBBBBB",
    linewidth = 0.5
  ) +
  # IC horizontal
  geom_errorbarh(
    aes(xmin = lo, xmax = hi),
    height = 0.22,
    linewidth = 0.6,
    na.rm = TRUE
  ) +
  # Punto central
  geom_point(size = 3.2) +
  # Valor numérico a la derecha del IC
  geom_text(
    aes(x = hi + 0.02, label = formatC(est, digits = 2, format = "f")),
    hjust = 0,
    size = 2.5,
    family = FUENTE,
    color = "#444444",
    na.rm = TRUE
  ) +
  facet_wrap(
    ~shock,
    ncol = 2,
    scales = "free",
    labeller = as_labeller(shock_labels)
  ) +
  scale_x_log10(
    breaks = c(0.1, 0.25, 0.5, 1, 2, 5, 10),
    labels = c("0,1", "0,25", "0,5", "1", "2", "5", "10")
  ) +
  scale_color_manual(values = COL_SIG, name = NULL) +
  scale_shape_manual(
    values = c("p < 0,05" = 16, "p ≥ 0,05" = 1),
    name = NULL
  ) +
  labs(
    title = "Gráfico XI.5 — Efectos de los shocks históricos sobre las dimensiones del conflicto",
    subtitle = paste(
      "Escala logarítmica: 1 = efecto nulo. Punto sólido rojo: significativo (p<0,05).",
      "Punto hueco gris: no significativo. Segmento: intervalo de confianza 95%.",
      sep = "\n"
    ),
    caption = paste(
      "Fuente: modelos Poisson (Anexo 1), supervivencia Exponencial/Weibull/Cox (Anexos 2–3),",
      "LM y logístico (Anexo 4). SDQ, COW, UCDP. Elaboración propia.",
      "IRR: Incidence Rate Ratio. TR: Time Ratio. OR: Odds Ratio. exp(β): efecto multiplicativo en log-muertes.",
      sep = "\n"
    ),
    x = "Estimación (escala logarítmica; línea punteada = efecto nulo)",
    y = NULL
  ) +
  theme_arma() +
  theme(
    panel.grid.major.x = element_line(color = "#EEEEEE", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

p_xi5

# =============================================================================
# GRÁFICO 8.5 — Heatmap de efectos
# Filas: Dataset × Dimensión (12 combinaciones)
# Columnas: Shock 1945 | Shock 1991 | Unip. temprana | Unip. tardía | Rimland
# Color: dirección + significancia
# =============================================================================

heatmap_data <- tribble(
  ~dataset , ~dimension   , ~predictor       , ~efecto , ~sig           ,
  # SDQ · Frecuencia
  "SDQ"    , "Frecuencia" , "Shock 1945"     ,       0 , "no_sig"       ,
  "SDQ"    , "Frecuencia" , "Shock 1991"     , NA      , "na"           ,
  "SDQ"    , "Frecuencia" , "Unip. temprana" , NA      , "na"           ,
  "SDQ"    , "Frecuencia" , "Unip. tardía"   , NA      , "na"           ,
  "SDQ"    , "Frecuencia" , "Rimland"        , NA      , "na"           ,
  # COW · Frecuencia
  "COW"    , "Frecuencia" , "Shock 1945"     ,       0 , "no_sig"       ,
  "COW"    , "Frecuencia" , "Shock 1991"     , NA      , "na"           ,
  "COW"    , "Frecuencia" , "Unip. temprana" ,       0 , "no_sig"       ,
  "COW"    , "Frecuencia" , "Unip. tardía"   ,       0 , "no_sig"       ,
  "COW"    , "Frecuencia" , "Rimland"        ,       1 , "sig_pos"      ,
  # UCDP · Frecuencia
  "UCDP"   , "Frecuencia" , "Shock 1945"     ,       0 , "no_sig"       ,
  "UCDP"   , "Frecuencia" , "Shock 1991"     ,       1 , "marginal_pos" ,
  "UCDP"   , "Frecuencia" , "Unip. temprana" ,      -1 , "sig"          ,
  "UCDP"   , "Frecuencia" , "Unip. tardía"   ,       0 , "no_sig"       ,
  "UCDP"   , "Frecuencia" , "Rimland"        ,       0 , "no_sig"       ,
  # SDQ · Intervalo
  "SDQ"    , "Intervalo"  , "Shock 1945"     ,       0 , "no_sig"       ,
  "SDQ"    , "Intervalo"  , "Shock 1991"     , NA      , "na"           ,
  "SDQ"    , "Intervalo"  , "Unip. temprana" , NA      , "na"           ,
  "SDQ"    , "Intervalo"  , "Unip. tardía"   , NA      , "na"           ,
  "SDQ"    , "Intervalo"  , "Rimland"        , NA      , "na"           ,
  # COW · Intervalo
  "COW"    , "Intervalo"  , "Shock 1945"     ,       0 , "no_sig"       ,
  "COW"    , "Intervalo"  , "Shock 1991"     , NA      , "na"           ,
  "COW"    , "Intervalo"  , "Unip. temprana" , NA      , "na"           ,
  "COW"    , "Intervalo"  , "Unip. tardía"   , NA      , "na"           ,
  "COW"    , "Intervalo"  , "Rimland"        ,       0 , "no_sig"       ,
  # UCDP · Intervalo
  "UCDP"   , "Intervalo"  , "Shock 1945"     ,       0 , "no_sig"       ,
  "UCDP"   , "Intervalo"  , "Shock 1991"     ,      -1 , "sig"          ,
  "UCDP"   , "Intervalo"  , "Unip. temprana" , NA      , "na"           ,
  "UCDP"   , "Intervalo"  , "Unip. tardía"   ,      -1 , "sig"          ,
  "UCDP"   , "Intervalo"  , "Rimland"        ,       0 , "no_sig"       ,
  # SDQ · Duración
  "SDQ"    , "Duración"   , "Shock 1945"     ,       0 , "no_sig"       ,
  "SDQ"    , "Duración"   , "Shock 1991"     , NA      , "na"           ,
  "SDQ"    , "Duración"   , "Unip. temprana" , NA      , "na"           ,
  "SDQ"    , "Duración"   , "Unip. tardía"   , NA      , "na"           ,
  "SDQ"    , "Duración"   , "Rimland"        , NA      , "na"           ,
  # COW · Duración
  "COW"    , "Duración"   , "Shock 1945"     ,       1 , "marginal_pos" ,
  "COW"    , "Duración"   , "Shock 1991"     , NA      , "na"           ,
  "COW"    , "Duración"   , "Unip. temprana" , NA      , "na"           ,
  "COW"    , "Duración"   , "Unip. tardía"   , NA      , "na"           ,
  "COW"    , "Duración"   , "Rimland"        , NA      , "na"           ,
  # UCDP · Duración
  "UCDP"   , "Duración"   , "Shock 1945"     ,       0 , "no_sig"       ,
  "UCDP"   , "Duración"   , "Shock 1991"     ,      -1 , "sig"          ,
  "UCDP"   , "Duración"   , "Unip. temprana" ,       0 , "no_sig"       ,
  "UCDP"   , "Duración"   , "Unip. tardía"   ,      -1 , "sig"          ,
  "UCDP"   , "Duración"   , "Rimland"        , NA      , "na"           ,
  # SDQ · Intensidad
  "SDQ"    , "Intensidad" , "Shock 1945"     ,       0 , "no_sig"       ,
  "SDQ"    , "Intensidad" , "Shock 1991"     , NA      , "na"           ,
  "SDQ"    , "Intensidad" , "Unip. temprana" , NA      , "na"           ,
  "SDQ"    , "Intensidad" , "Unip. tardía"   , NA      , "na"           ,
  "SDQ"    , "Intensidad" , "Rimland"        , NA      , "na"           ,
  # COW · Intensidad
  "COW"    , "Intensidad" , "Shock 1945"     ,       0 , "no_sig"       ,
  "COW"    , "Intensidad" , "Shock 1991"     , NA      , "na"           ,
  "COW"    , "Intensidad" , "Unip. temprana" ,       0 , "no_sig"       ,
  "COW"    , "Intensidad" , "Unip. tardía"   ,      -1 , "marginal"     ,
  "COW"    , "Intensidad" , "Rimland"        ,       1 , "sig_pos"      ,
  # UCDP · Intensidad
  "UCDP"   , "Intensidad" , "Shock 1945"     ,       0 , "no_sig"       ,
  "UCDP"   , "Intensidad" , "Shock 1991"     ,      -1 , "sig"          ,
  "UCDP"   , "Intensidad" , "Unip. temprana" ,       0 , "no_sig"       ,
  "UCDP"   , "Intensidad" , "Unip. tardía"   ,      -1 , "marginal"     ,
  "UCDP"   , "Intensidad" , "Rimland"        ,       0 , "no_sig"
) |>
  mutate(
    dataset = factor(dataset, levels = c("SDQ", "COW", "UCDP")),
    dimension = factor(
      dimension,
      levels = c("Frecuencia", "Intervalo", "Duración", "Intensidad")
    ),
    predictor = factor(
      predictor,
      levels = c(
        "Shock 1945",
        "Shock 1991",
        "Unip. temprana",
        "Unip. tardía",
        "Rimland"
      )
    ),
    fila = paste0(dataset, "\n", dimension),
    fila = factor(
      fila,
      levels = rev(c(
        "SDQ\nFrecuencia",
        "COW\nFrecuencia",
        "UCDP\nFrecuencia",
        "SDQ\nIntervalo",
        "COW\nIntervalo",
        "UCDP\nIntervalo",
        "SDQ\nDuración",
        "COW\nDuración",
        "UCDP\nDuración",
        "SDQ\nIntensidad",
        "COW\nIntensidad",
        "UCDP\nIntensidad"
      ))
    )
  ) |>
  mutate(
    label_cell = case_when(
      sig == "na" ~ "",
      sig == "no_sig" ~ "—",
      sig == "sig_caut" ~ "✓*",
      sig == "sig" ~ "▼",
      sig == "marginal" ~ "▽",
      sig == "sig_pos" ~ "▲",
      sig == "marginal_pos" ~ "△",
      TRUE ~ ""
    ),
    text_color = case_when(
      sig %in% c("sig", "sig_pos") ~ "white",
      TRUE ~ "#555555"
    )
  )

COL_HEAT2 <- c(
  "sig" = "#1A5276",
  "marginal" = "#AED6F1",
  "sig_pos" = "#922B21",
  "marginal_pos" = "#F1948A",
  "no_sig" = "#F0F0EE",
  "na" = "#FAFAF8"
)

COL_HEAT2_labels <- c(
  "sig" = "Reduce  (sig.  p < 0,05)",
  "marginal" = "Reduce  (marginal  p < 0,10)",
  "sig_pos" = "Amplifica  (sig.  p < 0,05)",
  "marginal_pos" = "Amplifica  (marginal  p < 0,10)",
  "no_sig" = "Sin efecto significativo",
  "na" = "No aplica"
)

sep_y <- c(3.5, 6.5, 9.5)

p_xi5 <- ggplot(heatmap_data, aes(x = predictor, y = fila, fill = sig)) +
  geom_hline(yintercept = sep_y, color = "#CCCCCC", linewidth = 0.6) +
  geom_vline(xintercept = 2.5, color = "#CCCCCC", linewidth = 0.6) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(
    aes(label = label_cell, color = text_color),
    size = 5,
    family = FUENTE
  ) +
  scale_fill_manual(
    values = COL_HEAT2,
    labels = COL_HEAT2_labels,
    name = NULL,
    guide = guide_legend(
      nrow = 2,
      keywidth = unit(14, "pt"),
      keyheight = unit(14, "pt"),
      label.theme = element_text(size = 10, family = FUENTE, color = "#444444"),
      override.aes = list(color = NA)
    )
  ) +
  scale_color_identity() +
  annotate(
    "text",
    x = 5.7,
    y = c(1.5, 4.5, 7.5, 10.5),
    label = c("Intensidad", "Duración", "Intervalo", "Frecuencia"),
    hjust = 0,
    size = 3.8,
    family = FUENTE,
    color = "#444444",
    fontface = "bold"
  ) +
  scale_x_discrete(position = "top") +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_arma(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 8),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 11),
    panel.grid = element_blank(),
    plot.margin = margin(14, 90, 14, 14)
  )

p_xi5

ggsave(
  "graficos/XI5_heatmap_efectos.png",
  p_xi5,
  width = 10,
  height = 9,
  dpi = 300,
  bg = "white"
)
ggsave("graficos/XI5_heatmap_efectos.svg", p_xi5, width = 10, height = 9)


# =============================================================================
# 7. EXPORTAR
# =============================================================================

dir.create("Graficos", showWarnings = FALSE)

# --- PNG (300 dpi — cumple el requisito UNTREF de ≥150 dpi) ---
ggsave(
  "graficos/X1_cobertura_temporal.png",
  p_x1,
  width = 13,
  height = 8,
  dpi = 300,
  bg = "white"
)
ggsave(
  "graficos/XI1_frecuencia_anual.png",
  p_xi1,
  width = 11,
  height = 10,
  dpi = 300,
  bg = "white"
)

ggsave(
  "graficos/XI2_distribucion_muertes.png",
  p_xi2,
  width = 8,
  height = 4.5,
  dpi = 300,
  bg = "white"
)
ggsave(
  "graficos/XI3_rimland_dotplot.png",
  p_xi3,
  width = 7,
  height = 5.5,
  dpi = 300,
  bg = "white"
)
ggsave(
  "graficos/XI4_kaplan_meier.png",
  p_xi4,
  width = 10,
  height = 7.0,
  dpi = 300,
  bg = "white"
)
ggsave(
  "graficos/XI5_forest_shocks.png",
  p_xi5,
  width = 13,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

# --- SVG (vectorial, insertable en Word como objeto editable sin pérdida) ---
ggsave("graficos/X1_cobertura_temporal.svg", p_x1, width = 13, height = 5)
ggsave("graficos/XI1_frecuencia_anual.svg", p_xi1, width = 11, height = 6)
ggsave("graficos/XI2_distribucion_muertes.svg", p_xi2, width = 10, height = 5)
ggsave("graficos/XI3_rimland_dotplot.svg", p_xi3, width = 7, height = 5.5)
ggsave("graficos/XI4_kaplan_meier.svg", p_xi4, width = 10, height = 7.0)
ggsave("graficos/XI5_forest_shocks.svg", p_xi5, width = 13, height = 5.5)

# --- EMF (opcional: editable en Word/PowerPoint nativo en Windows) ---
# Requiere: install.packages("devEMF")
# library(devEMF)
# emf("graficos/X1_cobertura_temporal.emf",  width = 11, height = 4.0); print(p_x1);  dev.off()
# emf("graficos/XI1_frecuencia_anual.emf",   width = 10, height = 8.5); print(p_xi1); dev.off()
# emf("graficos/XI2_distribucion_muertes.emf",width = 8, height = 4.5); print(p_xi2); dev.off()
# emf("graficos/XI3_rimland_dotplot.emf",    width = 7,  height = 5.5); print(p_xi3); dev.off()
# emf("graficos/XI4_kaplan_meier.emf",       width = 10, height = 7.0); print(p_xi4); dev.off()
# emf("graficos/XI5_forest_shocks.emf",      width = 13, height = 5.5); print(p_xi5); dev.off()

message("✅ Gráficos exportados en graficos/")
message("   PNG → alta resolución (300 dpi)")
message(
  "   SVG → vectorial editable (insertar en Word con 'Insertar → Imagen')"
)
