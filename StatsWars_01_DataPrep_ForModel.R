library(tidyverse)
library(readr)
library(lubridate)

# Input ------------------------------------------------------------------
full_sdq <- readRDS(file = "data_silver/full_sdq.rds")
full_cow <- readRDS(file = "data_silver/full_cow.rds")
full_ucdp_prio <- readRDS(file = "data_silver/full_ucdp_prio.rds")

# Data prep --------------------------------------------------------------

## Por AÑO (para Frecuencia) -------------------------------------------------------

# Por año
# Construir datasets por año (los tres)

anu_frec_sdq <- full_sdq |>
  group_by(year) |>

  summarise(
    n_conflictos_sdq = n()
  ) |>
  ungroup() |>
  # completar años faltantes
  complete(
    year = full_seq(1807:1949, 1),
    fill = list(
      n_conflictos_sdq = 0
    )
  ) |>
  # Crear variable PHASE
  mutate(
    phase = case_when(
      year >= 1815 & year <= 1913 ~ "multipolaridad_clasica",
      year >= 1914 & year <= 1918 ~ "ruptura_sistema",
      year >= 1919 & year <= 1938 ~ "entreguerras",
      year >= 1939 & year <= 1945 ~ "ruptura_sistema",
      year >= 1946 & year <= 1991 ~ "bipolaridad",
      year >= 1992 & year <= 2001 ~ "unipolaridad_temprana",
      year >= 2002 &
        year <= 2024 ~ "unipolaridad_tardia_multipolaridad_emergente",
      TRUE ~ NA_character_
    )
  ) |>
  # Crear variables de SHOCKS
  mutate(
    shock_1815 = ifelse(year >= 1815, 1, 0),
    shock_1914 = ifelse(year >= 1914, 1, 0),
    shock_1939 = ifelse(year >= 1939, 1, 0),
    shock_1945 = ifelse(year >= 1945, 1, 0),
    shock_1991 = ifelse(year >= 1991, 1, 0),
    shock_2001 = ifelse(year >= 2001, 1, 0)
  )


anu_frec_cow <- full_cow |>
  filter(!is.na(year)) |>
  group_by(year) |>
  summarise(
    n_conflictos_cow = n(),
    n_rimland_1 = sum(rimland, na.rm = TRUE),
    n_rimland_0 = n() - sum(rimland, na.rm = TRUE),
    n_conflicts_offshore = sum(geo_zone == "Offshore", na.rm = TRUE),
    n_conflicts_africa = sum(geo_zone == "Africa", na.rm = TRUE),
    n_conflicts_heartland = sum(geo_zone == "Heartland", na.rm = TRUE)
  ) |>
  ungroup() |>
  # completar años faltantes
  complete(
    year = full_seq(1816:2014, 1),
    fill = list(
      n_conflictos_cow = 0
    )
  ) |>
  # Crear variable PHASE
  mutate(
    phase = case_when(
      year >= 1815 & year <= 1913 ~ "multipolaridad_clasica",
      year >= 1914 & year <= 1918 ~ "ruptura_sistema",
      year >= 1919 & year <= 1938 ~ "entreguerras",
      year >= 1939 & year <= 1945 ~ "ruptura_sistema",
      year >= 1946 & year <= 1991 ~ "bipolaridad",
      year >= 1992 & year <= 2001 ~ "unipolaridad_temprana",
      year >= 2002 &
        year <= 2024 ~ "unipolaridad_tardia_multipolaridad_emergente",
      TRUE ~ NA_character_
    )
  ) |>
  # Crear variables de SHOCKS
  mutate(
    shock_1815 = ifelse(year >= 1815, 1, 0),
    shock_1914 = ifelse(year >= 1914, 1, 0),
    shock_1939 = ifelse(year >= 1939, 1, 0),
    shock_1945 = ifelse(year >= 1945, 1, 0),
    shock_1991 = ifelse(year >= 1991, 1, 0),
    shock_2001 = ifelse(year >= 2001, 1, 0)
  )

anu_frec_ucdp <- full_ucdp_prio |>
  group_by(year) |>
  summarise(
    n_conflictos_ucdp = n(),
    n_rimland_1 = sum(rimland, na.rm = TRUE),
    n_rimland_0 = n() - sum(rimland, na.rm = TRUE),
    n_conflicts_offshore = sum(geo_zone == "Offshore", na.rm = TRUE),
    n_conflicts_africa = sum(geo_zone == "Africa", na.rm = TRUE),
    n_conflicts_heartland = sum(geo_zone == "Heartland", na.rm = TRUE)
  ) |>
  ungroup() |>
  # completar años faltantes
  complete(
    year = full_seq(1949:2024, 1),
    fill = list(
      n_conflictos_ucdp = 0
    )
  ) |>
  # Crear variable PHASE
  mutate(
    phase = case_when(
      year >= 1815 & year <= 1913 ~ "multipolaridad_clasica",
      year >= 1914 & year <= 1918 ~ "ruptura_sistema",
      year >= 1919 & year <= 1938 ~ "entreguerras",
      year >= 1939 & year <= 1945 ~ "ruptura_sistema",
      year >= 1946 & year <= 1991 ~ "bipolaridad",
      year >= 1992 & year <= 2001 ~ "unipolaridad_temprana",
      year >= 2002 &
        year <= 2024 ~ "unipolaridad_tardia_multipolaridad_emergente",
      TRUE ~ NA_character_
    )
  ) |>
  # Crear variables de SHOCKS
  mutate(
    shock_1815 = ifelse(year >= 1815, 1, 0),
    shock_1914 = ifelse(year >= 1914, 1, 0),
    shock_1939 = ifelse(year >= 1939, 1, 0),
    shock_1945 = ifelse(year >= 1945, 1, 0),
    shock_1991 = ifelse(year >= 1991, 1, 0),
    shock_2001 = ifelse(year >= 2001, 1, 0)
  )

# UCDP filtro solo War
anu_frec_ucdp_War <- full_ucdp_prio |>
  filter(intensity_level == "War") |>
  group_by(year) |>
  summarise(
    n_conflictos_ucdp = n(),
    n_rimland_1 = sum(rimland, na.rm = TRUE),
    n_rimland_0 = n() - sum(rimland, na.rm = TRUE),
    n_conflicts_offshore = sum(geo_zone == "Offshore", na.rm = TRUE),
    n_conflicts_africa = sum(geo_zone == "Africa", na.rm = TRUE),
    n_conflicts_heartland = sum(geo_zone == "Heartland", na.rm = TRUE)
  ) |>
  ungroup() |>
  # completar años faltantes
  complete(
    year = full_seq(1949:2024, 1),
    fill = list(
      n_conflictos_ucdp = 0
    )
  ) |>
  # Crear variable PHASE
  mutate(
    phase = case_when(
      year >= 1815 & year <= 1913 ~ "multipolaridad_clasica",
      year >= 1914 & year <= 1918 ~ "ruptura_sistema",
      year >= 1919 & year <= 1938 ~ "entreguerras",
      year >= 1939 & year <= 1945 ~ "ruptura_sistema",
      year >= 1946 & year <= 1991 ~ "bipolaridad",
      year >= 1992 & year <= 2001 ~ "unipolaridad_temprana",
      year >= 2002 &
        year <= 2024 ~ "unipolaridad_tardia_multipolaridad_emergente",
      TRUE ~ NA_character_
    )
  ) |>
  # Crear variables de SHOCKS
  mutate(
    shock_1815 = ifelse(year >= 1815, 1, 0),
    shock_1914 = ifelse(year >= 1914, 1, 0),
    shock_1939 = ifelse(year >= 1939, 1, 0),
    shock_1945 = ifelse(year >= 1945, 1, 0),
    shock_1991 = ifelse(year >= 1991, 1, 0),
    shock_2001 = ifelse(year >= 2001, 1, 0)
  )


## Por CONFLICTO (para Intensidad y Duración) ----------------------------------------------------------

# SDQ
conf_durint_sdq <- full_sdq |>
  select(
    id,
    year,
    months,
    deaths,
    log_deaths
  ) |>
  # Crear variable PHASE
  mutate(
    phase = case_when(
      year >= 1815 & year <= 1913 ~ "multipolaridad_clasica",
      year >= 1914 & year <= 1918 ~ "ruptura_sistema",
      year >= 1919 & year <= 1938 ~ "entreguerras",
      year >= 1939 & year <= 1945 ~ "ruptura_sistema",
      year >= 1946 & year <= 1991 ~ "bipolaridad",
      year >= 1992 & year <= 2001 ~ "unipolaridad_temprana",
      year >= 2002 &
        year <= 2024 ~ "unipolaridad_tardia_multipolaridad_emergente",
      TRUE ~ NA_character_
    )
  ) |>
  # Crear variables de SHOCKS
  mutate(
    shock_1815 = ifelse(year >= 1815, 1, 0),
    shock_1914 = ifelse(year >= 1914, 1, 0),
    shock_1939 = ifelse(year >= 1939, 1, 0),
    shock_1945 = ifelse(year >= 1945, 1, 0),
    shock_1991 = ifelse(year >= 1991, 1, 0),
    shock_2001 = ifelse(year >= 2001, 1, 0)
  )

# COW
conf_durint_cow <- full_cow |>
  select(
    war_num,
    year,
    months,
    deaths,
    n_events,
    rimland,
    geo_zone
  ) |>
  # Crear variable PHASE
  mutate(
    phase = case_when(
      year >= 1815 & year <= 1913 ~ "multipolaridad_clasica",
      year >= 1914 & year <= 1918 ~ "ruptura_sistema",
      year >= 1919 & year <= 1938 ~ "entreguerras",
      year >= 1939 & year <= 1945 ~ "ruptura_sistema",
      year >= 1946 & year <= 1991 ~ "bipolaridad",
      year >= 1992 & year <= 2001 ~ "unipolaridad_temprana",
      year >= 2002 &
        year <= 2024 ~ "unipolaridad_tardia_multipolaridad_emergente",
      TRUE ~ NA_character_
    ),
    log_deaths = log(deaths + 1)
  ) |>
  # Crear variables de SHOCKS
  mutate(
    shock_1815 = ifelse(year >= 1815, 1, 0),
    shock_1914 = ifelse(year >= 1914, 1, 0),
    shock_1939 = ifelse(year >= 1939, 1, 0),
    shock_1945 = ifelse(year >= 1945, 1, 0),
    shock_1991 = ifelse(year >= 1991, 1, 0),
    shock_2001 = ifelse(year >= 2001, 1, 0)
  )


# UCDP
conf_durint_ucdp <- full_ucdp_prio |>
  select(
    conflict_id,
    year,
    months,
    intensity_level,
    n_events,
    rimland,
    geo_zone
  ) |>
  # Crear variable PHASE
  mutate(
    phase = case_when(
      year >= 1815 & year <= 1913 ~ "multipolaridad_clasica",
      year >= 1914 & year <= 1918 ~ "ruptura_sistema",
      year >= 1919 & year <= 1938 ~ "entreguerras",
      year >= 1939 & year <= 1945 ~ "ruptura_sistema",
      year >= 1946 & year <= 1991 ~ "bipolaridad",
      year >= 1992 & year <= 2001 ~ "unipolaridad_temprana",
      year >= 2002 &
        year <= 2024 ~ "unipolaridad_tardia_multipolaridad_emergente",
      TRUE ~ NA_character_
    )
  ) |>
  # Crear variables de SHOCKS
  mutate(
    shock_1815 = ifelse(year >= 1815, 1, 0),
    shock_1914 = ifelse(year >= 1914, 1, 0),
    shock_1939 = ifelse(year >= 1939, 1, 0),
    shock_1945 = ifelse(year >= 1945, 1, 0),
    shock_1991 = ifelse(year >= 1991, 1, 0),
    shock_2001 = ifelse(year >= 2001, 1, 0)
  )


## Por TIEMPO ENTRE conflictos --------------------------------------------

time_between_sdq <- full_sdq |>
  # 1. Crear fecha estándar (primer día del año)
  mutate(date_start = ymd(paste0(year, "-01-01"))) |>

  # 2. Ordenar por fecha de inicio
  arrange(date_start) |>

  # 3. Calcular el tiempo entre conflictos (en meses)
  mutate(time_between = interval(lag(date_start), date_start) %/% months(1)) |>

  # 4. Eliminar el primer conflicto (no tiene intervalo previo)
  filter(!is.na(time_between)) |>

  # 5. Seleccionar solo las variables relevantes
  select(
    id,
    date_start,
    year,
    time_between
  ) |>
  # Crear variable PHASE
  mutate(
    phase = case_when(
      year >= 1815 & year <= 1913 ~ "multipolaridad_clasica",
      year >= 1914 & year <= 1918 ~ "ruptura_sistema",
      year >= 1919 & year <= 1938 ~ "entreguerras",
      year >= 1939 & year <= 1945 ~ "ruptura_sistema",
      year >= 1946 & year <= 1991 ~ "bipolaridad",
      year >= 1992 & year <= 2001 ~ "unipolaridad_temprana",
      year >= 2002 &
        year <= 2024 ~ "unipolaridad_tardia_multipolaridad_emergente",
      TRUE ~ NA_character_
    )
  ) |>
  # Crear variables de SHOCKS
  mutate(
    shock_1815 = ifelse(year >= 1815, 1, 0),
    shock_1914 = ifelse(year >= 1914, 1, 0),
    shock_1939 = ifelse(year >= 1939, 1, 0),
    shock_1945 = ifelse(year >= 1945, 1, 0),
    shock_1991 = ifelse(year >= 1991, 1, 0),
    shock_2001 = ifelse(year >= 2001, 1, 0)
  )


time_between_cow <- full_cow |>
  # 1. Crear fecha estándar si no existe
  mutate(
    date_start = case_when(
      !is.na(date_start) ~ date_start, # si ya existe
      TRUE ~ ymd(paste0(year, "-01-01"))
    )
  ) |>

  # 2. Ordenar por fecha
  arrange(date_start) |>

  # 3. Calcular tiempo entre conflictos en meses
  mutate(time_between = interval(lag(date_start), date_start) %/% months(1)) |>

  # 4. Eliminar primer conflicto
  filter(!is.na(time_between)) |>

  # 5. Seleccionar variables relevantes
  select(
    war_num,
    date_start,
    year,
    rimland,
    time_between
  ) |>

  # 6. Crear PHASE
  mutate(
    phase = case_when(
      year >= 1815 & year <= 1913 ~ "multipolaridad_clasica",
      year >= 1914 & year <= 1918 ~ "ruptura_sistema",
      year >= 1919 & year <= 1938 ~ "entreguerras",
      year >= 1939 & year <= 1945 ~ "ruptura_sistema",
      year >= 1946 & year <= 1991 ~ "bipolaridad",
      year >= 1992 & year <= 2001 ~ "unipolaridad_temprana",
      year >= 2002 &
        year <= 2024 ~ "unipolaridad_tardia_multipolaridad_emergente",
      TRUE ~ NA_character_
    )
  ) |>

  # 7. Crear SHOCKS
  mutate(
    shock_1815 = ifelse(year >= 1815, 1, 0),
    shock_1914 = ifelse(year >= 1914, 1, 0),
    shock_1939 = ifelse(year >= 1939, 1, 0),
    shock_1945 = ifelse(year >= 1945, 1, 0),
    shock_1991 = ifelse(year >= 1991, 1, 0),
    shock_2001 = ifelse(year >= 2001, 1, 0)
  )

time_between_ucdp <- full_ucdp_prio |>
  # 1. Crear fecha estándar si no existe
  mutate(
    date_start = case_when(
      !is.na(date_start) ~ date_start,
      TRUE ~ ymd(paste0(year, "-01-01"))
    )
  ) |>

  # 2. Ordenar por fecha
  arrange(date_start) |>

  # 3. Calcular tiempo entre conflictos en meses
  mutate(time_between = interval(lag(date_start), date_start) %/% months(1)) |>

  # 4. Eliminar primer conflicto
  filter(!is.na(time_between)) |>

  # 5. Seleccionar variables relevantes
  select(
    conflict_id,
    date_start,
    year,
    rimland,
    time_between
  ) |>

  # 6. Crear PHASE
  mutate(
    phase = case_when(
      year >= 1815 & year <= 1913 ~ "multipolaridad_clasica",
      year >= 1914 & year <= 1918 ~ "ruptura_sistema",
      year >= 1919 & year <= 1938 ~ "entreguerras",
      year >= 1939 & year <= 1945 ~ "ruptura_sistema",
      year >= 1946 & year <= 1991 ~ "bipolaridad",
      year >= 1992 & year <= 2001 ~ "unipolaridad_temprana",
      year >= 2002 &
        year <= 2024 ~ "unipolaridad_tardia_multipolaridad_emergente",
      TRUE ~ NA_character_
    )
  ) |>

  # 7. Crear SHOCKS
  mutate(
    shock_1815 = ifelse(year >= 1815, 1, 0),
    shock_1914 = ifelse(year >= 1914, 1, 0),
    shock_1939 = ifelse(year >= 1939, 1, 0),
    shock_1945 = ifelse(year >= 1945, 1, 0),
    shock_1991 = ifelse(year >= 1991, 1, 0),
    shock_2001 = ifelse(year >= 2001, 1, 0)
  )

# Guardar datasets limpios ------------------------------------------------
saveRDS(anu_frec_sdq, file = "data_gold/anu_frec_sdq.rds")
saveRDS(anu_frec_cow, file = "data_gold/anu_frec_cow.rds")
saveRDS(anu_frec_ucdp, file = "data_gold/anu_frec_ucdp.rds")
saveRDS(anu_frec_ucdp_War, file = "data_gold/anu_frec_ucdp_War.rds")
saveRDS(conf_durint_sdq, file = "data_gold/conf_durint_sdq.rds")
saveRDS(conf_durint_cow, file = "data_gold/conf_durint_cow.rds")
saveRDS(conf_durint_ucdp, file = "data_gold/conf_durint_ucdp.rds")
saveRDS(time_between_sdq, file = "data_gold/time_between_sdq.rds")
saveRDS(time_between_cow, file = "data_gold/time_between_cow.rds")
saveRDS(time_between_ucdp, file = "data_gold/time_between_ucdp.rds")


# Save CSV
write_csv(anu_frec_sdq, file = "data_gold/anu_frec_sdq.csv")
write_csv(anu_frec_cow, file = "data_gold/anu_frec_cow.csv")
write_csv(anu_frec_ucdp, file = "data_gold/anu_frec_ucdp.csv")
write_csv(anu_frec_ucdp_War, file = "data_gold/anu_frec_ucdp_War.csv")
write_csv(conf_durint_sdq, file = "data_gold/conf_durint_sdq.csv")
write_csv(conf_durint_cow, file = "data_gold/conf_durint_cow.csv")
write_csv(conf_durint_ucdp, file = "data_gold/conf_durint_ucdp.csv")
write_csv(time_between_sdq, file = "data_gold/time_between_sdq.csv")
write_csv(time_between_cow, file = "data_gold/time_between_cow.csv")
write_csv(time_between_ucdp, file = "data_gold/time_between_ucdp.csv")
