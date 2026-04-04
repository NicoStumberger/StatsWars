library(tidyverse)
library(readr)
library(lubridate)
library(HistData)


# Input ------------------------------------------------------------------

# Fuente:
data("Quarrels", package = "HistData")

quarrels <- Quarrels |>
  janitor::clean_names()


# Data prep --------------------------------------------------------------

# Seleccionar columnas relevantes y calcular duración en años

full_sdq <- quarrels |>
  select(
    id,
    year,
    months,
    log_deaths,
    deaths,
    international,
    colonial,
    revolution,
    nat_grp,
    grp_grp_dif,
    grp_grp_same,
    same_gov,
    eq_wealth,
    dif_wealth,
    same_language,
    dif_language,
    sim_relig,
    dif_relig,
    exchange_goods,
    minerals
  )

# Guardar datasets limpios ------------------------------------------------
saveRDS(full_sdq, file = "data_silver/full_sdq.rds")

# Save CSV
write_csv(full_sdq, file = "data_silver/full_sdq.csv")
