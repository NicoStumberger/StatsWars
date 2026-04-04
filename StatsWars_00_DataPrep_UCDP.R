library(tidyverse)
library(readr)
library(lubridate)
library(countrycode)
library(haven)

# Input ------------------------------------------------------------------
# Fuente: https://ucdp.uu.se/downloads/

UcdpPrioConflict_v25_1 <- read_csv("data_raw/UcdpPrioConflict_v25_1.csv") |>
  janitor::clean_names()

# Dyadic_v25_1 <- read_csv("data_raw/Dyadic_v25_1.csv") |>
#   janitor::clean_names()

GEDEvent_v25_1 <- read_csv("data_raw/GEDEvent_v25_1.csv") |>
  janitor::clean_names()

# NonState_v25_1 <- read_csv("data_raw/NonState_v25_1.csv") |>
#   janitor::clean_names()

# organizedviolencecy_v25_1 <- read_csv(
#   "data_raw/organizedviolencecy_v25_1.csv"
# ) |>
#   janitor::clean_names()

# Actor_v25_1 <- readxl::read_excel(
#   "data_raw/Actor_v25_1.xlsx"
# ) |>
#   janitor::clean_names()

UCDPConflictTerm <- read_csv(
  "data_raw/UCDPConflictTerminationDataset_v4_2024_Conflict.csv"
) |>
  janitor::clean_names()


## OUTSURCES ----------------------------------------------------------------
# Fuente: https://correlatesofwar.org/data-sets/

cow_religion <- read_csv("data_raw/WRP_national.csv") |>
  janitor::clean_names()

cow_contiguity <- read_csv("data_raw/contdird.csv") |>
  janitor::clean_names()

cow_trade <- read_csv("data_raw/Dyadic_COW_4.0.csv") |>
  janitor::clean_names()

cow_country_codes <- read_csv("data_raw/COW-country-codes.csv") |>
  janitor::clean_names() |>
  distinct()

# https://ourworldindata.org/grapher/political-regime?tab=table&overlay=download-data
outsource_political_regime <- read_csv("data_raw/political-regime.csv") |>
  janitor::clean_names() |>
  select(-entity) |>
  distinct(code, year, .keep_all = TRUE)

# https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.PCAP.CD&country=#advancedDownloadOptions
outsource_gdp <- read_csv("data_raw/databank.worldbank_GDPperCap.csv") |>
  janitor::clean_names()

# https://www.usitc.gov/data/gravity/dicl.htm
outsource_language <- read_csv("data_raw/dicl_database.csv") |>
  janitor::clean_names()

# https://mikedenly.com/datasets/global-resources-dataset
outsource_resources <- read_dta("data_raw/dfhsw_GRD_public_v1.dta")

# Data prep --------------------------------------------------------------

# Filtrar conflictos con intensidad >= 2 (>=1000 muertes) y que hayan terminado (ep_end == 1)
# Seleccionar columnas relevantes y calcular duración en años

ucdp <- UcdpPrioConflict_v25_1 |>
  select(
    conflict_id,
    location,
    side_a,
    side_a_id,
    side_a_2nd,
    side_b,
    side_b_id,
    side_b_2nd,
    incompatibility,
    territory_name,
    year,
    intensity_level,
    type_of_conflict,
    start_date,
    start_date2,
    ep_end,
    ep_end_date,
    gwno_a,
    gwno_a_2nd,
    gwno_b,
    gwno_b_2nd,
    gwno_loc,
    region
  ) |>
  mutate(
    months = as.numeric(ep_end_date - start_date, units = "days") /
      30.44
  )

# Función para calcular la moda en columnas con múltiples valores separados por comas
mode_split <- function(x) {
  vals <- x |>
    str_split(",\\s*") |>
    unlist() |>
    discard(~ .x %in% c("", NA, "NA"))

  if (length(vals) == 0) {
    return(NA_character_)
  }

  names(sort(table(vals), decreasing = TRUE))[1]
}

# Preparar listas de actores por conflicto
ucdp_clean <- ucdp |>
  mutate(
    side_a_list = str_split(side_a, ",\\s*"),
    side_b_list = str_split(side_b, ",\\s*"),
    side_a2_list = str_split(side_a_2nd, ",\\s*"),
    side_b2_list = str_split(side_b_2nd, ",\\s*")
  ) |>
  mutate(
    across(
      c(side_a_list, side_b_list, side_a2_list, side_b2_list),
      ~ lapply(.x, function(v) v[v != "" & v != "NA" & !is.na(v)])
    )
  )

# Correcciones manuales para códigos GWNO problemáticos
manual_fix <- tibble(
  gwno = c(55, 345, 678, 680, 751),
  iso3 = c("TWN", "PSE", "ESH", NA, "SSD")
)

manual_fix_cow <- tibble(
  gwno = c(55, 345, 678, 680, 751),
  cow = c(713, 666, 605, NA, 625)
)

## Religion ---------------------------------------------------------------

religion <- cow_religion |>
  # 1. Quedarse solo con year, state y columnas terminadas en pct
  select(
    year,
    state,
    ends_with("pct")
  ) |>
  select(
    -sumreligpct # excluir sumreligpct
  ) |>
  # 2. Pivot long: una fila por religión
  pivot_longer(
    cols = ends_with("pct"),
    names_to = "religion",
    values_to = "pct"
  ) |>

  # 3. Limpiar: pct negativos o NA no sirven
  mutate(
    pct = ifelse(pct < 0, NA, pct),
    religion = str_remove(religion, "pct$") # opcional: sacar el sufijo "pct"
  ) |>
  filter(!is.na(pct)) |>

  # 4. Elegir la religión con mayor pct por state-year
  group_by(state, year) |>
  slice_max(pct, n = 1, with_ties = FALSE) |>
  ungroup() |>
  rename(
    religion_pct = pct
  )

# generar todos los años posibles por estado
years_full <- expand_grid(
  state = unique(religion$state),
  year = seq(min(religion$year), max(religion$year), by = 1)
)

# unir con tu dataset original

religion_full <- years_full |>
  left_join(religion, by = c("state", "year"))

# completar NAs con valores con forward fill
religion_full <- religion_full |>
  arrange(state, year) |>
  group_by(state) |>
  fill(religion, religion_pct, .direction = "down") |>
  ungroup()


## Contiguidad ------------------------------------------------------------

contiguity <- cow_contiguity |>
  select(
    year,
    ccode_a = state1no,
    ccode_b = state2no,
    conttype
  ) |>
  distinct(year, ccode_a, ccode_b, .keep_all = TRUE)


## Trade ------------------------------------------------------------------

trade <- cow_trade |>
  select(
    year,
    ccode_a = ccode1,
    ccode_b = ccode2,
    smoothtotrade
  ) |>
  mutate(
    smoothtotrade = ifelse(smoothtotrade < 0, NA, smoothtotrade)
  )


## GDP perCap -------------------------------------------------------------

gdp_long <- outsource_gdp |>
  # 1. Quedarte solo con country_code + columnas de años
  select(country_code, starts_with("x")) |>

  # 2. Pivot long
  pivot_longer(
    cols = starts_with("x"),
    names_to = "year",
    values_to = "gdp_percap"
  ) |>

  # 3. Extraer año
  mutate(
    year = str_extract(year, "\\d{4}"),
    year = as.integer(year)
  ) |>

  # 4. Filtrar NAs en country_code o gdp_percap
  filter(
    !is.na(country_code),
    !is.na(gdp_percap)
  )


## Idioma -----------------------------------------------------------------

language <- outsource_language |>
  select(
    iso3_a = iso3_i,
    iso3_b = iso3_j,
    ling_prox = lps
  ) |>
  distinct(iso3_a, iso3_b, .keep_all = TRUE) |>
  filter(!is.na(ling_prox))


## Recursos naturales -----------------------------------------------------

resources_country_year <- outsource_resources |>
  group_by(COWcode, year) |>
  summarise(
    has_resources = as.integer(n() > 0),
    lootable_sites = sum(lootable == 1, na.rm = TRUE),
    nonlootable_sites = sum(lootable == 0, na.rm = TRUE),
    resource_value = sum(world_val_withmc, na.rm = TRUE),
    nonlootable_value = sum(world_val_withmc[lootable == 0], na.rm = TRUE),
    lootable_value = sum(world_val_withmc[lootable == 1], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    log_resource_value = log(resource_value + 1),
    log_nonlootable_value = log(nonlootable_value + 1),
    log_lootable_value = log(lootable_value + 1)
  )


## Rimland ----------------------------------------------------------------

rimland_iso3 <- c(
  "FRA",
  "GRC",
  "TUR",
  "IRN",
  "IRQ",
  "ISR",
  "SYR",
  "JOR",
  "LBN",
  "EGY",
  "SAU",
  "ARE",
  "OMN",
  "YEM",
  "PAK",
  "IND",
  "BGD",
  "THA",
  "PHL",
  "IDN",
  "MMR",
  "LAO",
  "KHM",
  "VNM",
  "CHN",
  "KOR",
  "PRK",
  "TWN"
)


heartland_iso3 <- c(
  "RUS",
  "KAZ",
  "UZB",
  "TKM",
  "KGZ",
  "TJK",
  "MNG",
  "BLR",
  "UKR"
)


offshore_iso3 <- c(
  "USA",
  "GBR",
  "CAN",
  "AUS",
  "NZL",
  "JPN"
)


africa_iso3 <- c(
  "DZA",
  "TUN",
  "MAR",
  "EGY",
  "SDN",
  "SSD",
  "ETH",
  "ERI",
  "SOM",
  "KEN",
  "UGA",
  "RWA",
  "BDI",
  "TZA",
  "ZMB",
  "ZWE",
  "MOZ",
  "AGO",
  "NAM",
  "BWA",
  "ZAF",
  "LSO",
  "SWZ",
  "GAB",
  "COG",
  "COD",
  "CAF",
  "TCD",
  "NGA",
  "BEN",
  "TGO",
  "GHA",
  "CIV",
  "GIN",
  "GNB",
  "SEN",
  "MLI",
  "BFA",
  "NER",
  "CPV"
)


geo_dict <- tibble(
  iso3 = c(rimland_iso3, heartland_iso3, offshore_iso3, africa_iso3),
  geo_zone = c(
    rep("rimland", length(rimland_iso3)),
    rep("heartland", length(heartland_iso3)),
    rep("offshore", length(offshore_iso3)),
    rep("africa", length(africa_iso3))
  )
)

geo_dict <- geo_dict |>
  distinct(iso3, .keep_all = TRUE)


territory_geo <- tribble(
  ~territory_name                                     , ~geo_zone   ,
  "Cambodia"                                          , "rimland"   ,
  "Indonesia"                                         , "rimland"   ,
  "Kurdistan"                                         , "rimland"   ,
  "Azerbaijan"                                        , "rimland"   ,
  "Israel"                                            , "rimland"   ,
  "Laos"                                              , "rimland"   ,
  "Estonia"                                           , "rimland"   ,
  "Latvia"                                            , "rimland"   ,
  "Lithuania"                                         , "rimland"   ,
  "Ukraine"                                           , "heartland" ,
  "Northern Cambodia"                                 , "rimland"   ,
  "Korfu Channel"                                     , "rimland"   ,
  "Vietnam"                                           , "rimland"   ,
  "Taiwan"                                            , "rimland"   ,
  "Kashmir"                                           , "rimland"   ,
  "Malagasy"                                          , "africa"    ,
  "Karen"                                             , "rimland"   ,
  "Arakan"                                            , "rimland"   ,
  "Mon"                                               , "rimland"   ,
  "Hyderabad"                                         , "rimland"   ,
  "Palestine"                                         , "rimland"   ,
  "Malaya"                                            , "rimland"   ,
  "Kachin"                                            , "rimland"   ,
  "Taiwan strait"                                     , "rimland"   ,
  "Korea"                                             , "rimland"   ,
  "Tibet"                                             , "rimland"   ,
  "South Moluccas"                                    , "rimland"   ,
  "Puerto Rico"                                       , "offshore"  ,
  "Suez"                                              , "rimland"   ,
  "Kenya"                                             , "africa"    ,
  "Morocco"                                           , "africa"    ,
  "Tunisia"                                           , "africa"    ,
  "Algeria"                                           , "africa"    ,
  "Cyprus"                                            , "rimland"   ,
  "South Vietnam"                                     , "rimland"   ,
  "Nagaland"                                          , "rimland"   ,
  "Karenni"                                           , "rimland"   ,
  "Cameroon"                                          , "africa"    ,
  "Common Border"                                     , "unknown"   ,
  "Common border"                                     , "unknown"   ,
  "Various"                                           , "unknown"   ,
  "Morocco/Mauritania"                                , "africa"    ,
  "Morocco/Spanish territories"                       , "africa"    ,
  "Oman"                                              , "rimland"   ,
  "Angola"                                            , "africa"    ,
  "Shan"                                              , "rimland"   ,
  "Katanga"                                           , "africa"    ,
  "South Kasai"                                       , "africa"    ,
  "Ogaden"                                            , "africa"    ,
  "Bizerte"                                           , "africa"    ,
  "North Borneo"                                      , "rimland"   ,
  "Aksai Chin"                                        , "rimland"   ,
  "Eritrea"                                           , "africa"    ,
  "West New Guinea"                                   , "rimland"   ,
  "Guinea-Bissau"                                     , "africa"    ,
  "Southern Sudan"                                    , "africa"    ,
  "Mozambique"                                        , "africa"    ,
  "Aden/South Yemen"                                  , "rimland"   ,
  "West Papua"                                        , "rimland"   ,
  "Mizoram"                                           , "rimland"   ,
  "Namibia"                                           , "africa"    ,
  "West Bank"                                         , "rimland"   ,
  "Golan Heights"                                     , "rimland"   ,
  "Biafra"                                            , "africa"    ,
  "Ussuri river"                                      , "rimland"   ,
  "Mindanao"                                          , "rimland"   ,
  "East Pakistan"                                     , "rimland"   ,
  "Northern Ireland"                                  , "rimland"   ,
  "Chittagong Hill Tracts"                            , "rimland"   ,
  "Northern Cyprus"                                   , "rimland"   ,
  "Balochistan"                                       , "rimland"   ,
  "East Timor"                                        , "rimland"   ,
  "Sahrawi Arab Democratic Republic (Western Sahara)" , "africa"    ,
  "Tripura"                                           , "rimland"   ,
  "Arabistan"                                         , "rimland"   ,
  "Basque"                                            , "rimland"   ,
  "Malvinas/Falkland Islands"                         , "offshore"  ,
  "Manipur"                                           , "rimland"   ,
  "Lake Chad"                                         , "africa"    ,
  "Punjab/Khalistan"                                  , "rimland"   ,
  "Eelam"                                             , "rimland"   ,
  "Agacher Strip"                                     , "africa"    ,
  "Aozou strip"                                       , "africa"    ,
  "Afar"                                              , "africa"    ,
  "Assam"                                             , "rimland"   ,
  "Aceh"                                              , "rimland"   ,
  "Bougainville"                                      , "offshore"  ,
  "Kuwait"                                            , "rimland"   ,
  "Azawad"                                            , "africa"    ,
  "Air and Azawad"                                    , "africa"    ,
  "Casamance"                                         , "africa"    ,
  "Nagorno-Karabakh"                                  , "rimland"   ,
  "Azerbaijan"                                        , "rimland"   ,
  "Slovenia"                                          , "rimland"   ,
  "Croatia"                                           , "rimland"   ,
  "Cabinda"                                           , "africa"    ,
  "Artsakh (Nagorno-Karabakh)"                        , "rimland"   ,
  "Serb"                                              , "rimland"   ,
  "Abkhazia"                                          , "rimland"   ,
  "South Ossetia"                                     , "rimland"   ,
  "Dniestr"                                           , "heartland" ,
  "Bihaca Krajina"                                    , "rimland"   ,
  "Croat"                                             , "rimland"   ,
  "Chechnya"                                          , "heartland" ,
  "South Yemen"                                       , "rimland"   ,
  "Cordillera del Condor"                             , "offshore"  ,
  "Bakassi"                                           , "africa"    ,
  "Eastern Niger"                                     , "africa"    ,
  "Anjouan"                                           , "africa"    ,
  "Kosovo"                                            , "rimland"   ,
  "Oromiya"                                           , "africa"    ,
  "Dagestan"                                          , "heartland" ,
  "Bodoland"                                          , "rimland"   ,
  "Wa"                                                , "rimland"   ,
  "Patani"                                            , "rimland"   ,
  "Northern Nigeria"                                  , "africa"    ,
  "Niger Delta"                                       , "africa"    ,
  "Southern Lebanon"                                  , "rimland"   ,
  "Kagera Salient"                                    , "africa"    ,
  "Kongo Kingdom"                                     , "africa"    ,
  "Caucasus Emirate"                                  , "rimland"   ,
  "Arssi"                                             , "africa"    ,
  "Sidamaland"                                        , "africa"    ,
  "Kokang"                                            , "rimland"   ,
  "Lahu"                                              , "rimland"   ,
  "Garoland"                                          , "rimland"   ,
  "Suez/Sinai"                                        , "rimland"   ,
  "Abyei"                                             , "africa"    ,
  "East Turkestan"                                    , "rimland"   ,
  "Hararghe"                                          , "africa"    ,
  "Sabah"                                             , "rimland"   ,
  "Durand line"                                       , "rimland"   ,
  "Rojava Kurdistan"                                  , "rimland"   ,
  "Crimea"                                            , "heartland" ,
  "Donetsk"                                           , "heartland" ,
  "Lugansk"                                           , "heartland" ,
  "Novorossiya"                                       , "heartland" ,
  "Islamic State"                                     , "rimland"   ,
  "Macina Empire"                                     , "africa"    ,
  "Northeastern Province and Coast"                   , "africa"    ,
  "Western South East Asia"                           , "rimland"   ,
  "Somaliland"                                        , "africa"    ,
  "Ambazonia"                                         , "africa"    ,
  "Amhara"                                            , "africa"    ,
  "Logone"                                            , "africa"    ,
  "Bab al Mandab and Gulf of Aden"                    , "rimland"   ,
  "Jubaland"                                          , "africa"
)

territory_geo <- territory_geo |> distinct(territory_name, .keep_all = TRUE)


# UCDP/PRIO full ----------------------------------------------------
full_ucdp_prio <- ucdp_clean |>
  group_by(conflict_id) |>
  summarise(
    # 1. Fecha de inicio del conflicto
    start_date = min(start_date, na.rm = TRUE),

    # 2. Año del inicio
    year = year(start_date),

    # 3. Número de episodios
    n_events = n_distinct(c(start_date, start_date2), na.rm = TRUE),

    # 4. Tipo de conflicto dominante
    type_of_conflict = names(sort(table(type_of_conflict), decreasing = TRUE))[
      1
    ],

    # 5. Región dominante (puede tener múltiples valores → usar mode_split)
    region = mode_split(region),

    # 6. Territorio principal del conflicto
    territory_name = mode_split(territory_name),

    # 7. Intensidad dominante
    intensity_level = names(sort(table(intensity_level), decreasing = TRUE))[1],

    # 8. Fecha de fin del conflicto
    end_date = if (all(is.na(ep_end_date))) {
      NA_Date_
    } else {
      max(ep_end_date, na.rm = TRUE)
    },

    # 9. Actores principales por NOMBRE
    side_a = mode_split(side_a),
    side_b = mode_split(side_b),

    # 10. Actores principales por ID interno UCDP
    side_a_id = mode_split(side_a_id),
    side_b_id = mode_split(side_b_id),

    # 11. Actores principales por ID GWNO
    gwno_a = mode_split(gwno_a),
    gwno_b = mode_split(gwno_b),

    # 12. Número total de actores distintos por lado (primarios + secundarios)
    n_actors_a = length(unique(unlist(c(side_a_list, side_a2_list)))),
    n_actors_b = length(unique(unlist(c(side_b_list, side_b2_list)))),
    n_actors = n_actors_a + n_actors_b
  ) |>
  ungroup() |>
  mutate(
    # 13. Duración en meses
    months = as.numeric(end_date - start_date, units = "days") / 30.44,

    # 14. Descripción textual del tipo de conflicto
    type_of_conflict = case_when(
      type_of_conflict == 1 ~ "extrasystemic",
      type_of_conflict == 2 ~ "interstate",
      type_of_conflict == 3 ~ "intrastate",
      type_of_conflict == 4 ~ "internationalized intrastate",
      TRUE ~ as.character(type_of_conflict)
    ),

    # 15. Descripción textual de región
    region_desc = case_when(
      region == 1 ~ "Europe",
      region == 2 ~ "Middle East",
      region == 3 ~ "Asia",
      region == 4 ~ "Africa",
      region == 5 ~ "Americas",
      TRUE ~ as.character(region)
    ),

    # 16. Descripción textual de intensidad
    intensity_level = case_when(
      intensity_level == 1 ~ "Minor",
      intensity_level == 2 ~ "War",
      TRUE ~ as.character(intensity_level)
    ),

    gwno_a = as.numeric(gwno_a),
    gwno_b = as.numeric(gwno_b),
    iso3_a = countrycode(gwno_a, origin = "gwn", destination = "iso3c"),
    iso3_b = countrycode(gwno_b, origin = "gwn", destination = "iso3c"),
    cow_a = countrycode(gwno_a, origin = "gwn", destination = "cown"),
    cow_b = countrycode(gwno_b, origin = "gwn", destination = "cown")
  ) |>
  rename(conflict_type = type_of_conflict) |>

  # Lado A
  left_join(manual_fix, by = c("gwno_a" = "gwno")) |>
  mutate(iso3_a = coalesce(iso3_a, iso3)) |>
  select(-iso3) |>

  # Lado B
  left_join(manual_fix, by = c("gwno_b" = "gwno")) |>
  mutate(iso3_b = coalesce(iso3_b, iso3)) |>
  select(-iso3) |>

  # Lado A
  left_join(manual_fix_cow, by = c("gwno_a" = "gwno")) |>
  mutate(cow_a = coalesce(cow_a, cow)) |>
  select(-cow) |>

  # Lado B
  left_join(manual_fix_cow, by = c("gwno_b" = "gwno")) |>
  mutate(cow_b = coalesce(cow_b, cow)) |>
  select(-cow) |>

  # Add religion data
  left_join(
    religion_full,
    by = c("cow_a" = "state", "year" = "year")
  ) |>
  rename(
    religion_a = religion,
    religion_pct_a = religion_pct
  ) |>
  left_join(
    religion_full,
    by = c("cow_b" = "state", "year" = "year")
  ) |>
  rename(
    religion_b = religion,
    religion_pct_b = religion_pct
  ) |>
  mutate(
    religion_dif = case_when(
      is.na(religion_a) | is.na(religion_b) ~ NA,
      religion_a == religion_b ~ 0,
      TRUE ~ 1
    )
  ) |>
  # Add contiguity data
  left_join(
    contiguity,
    by = c("cow_a" = "ccode_a", "cow_b" = "ccode_b", "year" = "year")
  ) |>
  # Add trade data
  left_join(
    trade,
    by = c("cow_a" = "ccode_a", "cow_b" = "ccode_b", "year" = "year")
  ) |>
  # Add political regime data
  left_join(
    outsource_political_regime,
    by = c("iso3_a" = "code", "year" = "year")
  ) |>
  rename(
    political_regime_a = political_regime
  ) |>
  left_join(
    outsource_political_regime,
    by = c("year" = "year", "iso3_b" = "code")
  ) |>
  rename(
    political_regime_b = political_regime
  ) |>
  mutate(
    political_regime_dif = case_when(
      is.na(political_regime_a) | is.na(political_regime_b) ~ NA,
      political_regime_a == political_regime_b ~ 0,
      TRUE ~ 1
    )
  ) |>
  # Add GDP per capita data
  left_join(
    gdp_long,
    by = c("iso3_a" = "country_code", "year" = "year")
  ) |>
  rename(
    gdp_percap_a = gdp_percap
  ) |>
  left_join(
    gdp_long,
    by = c("iso3_b" = "country_code", "year" = "year")
  ) |>
  rename(
    gdp_percap_b = gdp_percap
  ) |>
  mutate(
    gdp_gap_sq = (gdp_percap_a - gdp_percap_b)^2
  ) |>
  # Add language data
  left_join(
    language,
    by = c("iso3_a" = "iso3_a", "iso3_b" = "iso3_b")
  ) |>
  # Add resources data
  left_join(
    resources_country_year,
    by = c("cow_a" = "COWcode", "year" = "year")
  ) |>
  # Side A resources rename
  rename_with(
    ~ paste0(.x, "_a"),
    c(
      "has_resources",
      "lootable_sites",
      "nonlootable_sites",
      "resource_value",
      "nonlootable_value",
      "lootable_value",
      "log_resource_value",
      "log_nonlootable_value",
      "log_lootable_value"
    )
  ) |>
  left_join(
    resources_country_year,
    by = c("cow_b" = "COWcode", "year" = "year")
  ) |>
  # Side B resources rename
  rename_with(
    ~ paste0(.x, "_b"),
    c(
      "has_resources",
      "lootable_sites",
      "nonlootable_sites",
      "resource_value",
      "nonlootable_value",
      "lootable_value",
      "log_resource_value",
      "log_nonlootable_value",
      "log_lootable_value"
    )
  ) |>
  # Add Rimland
  left_join(territory_geo, by = "territory_name") |>
  rename(geo_zone_territory = geo_zone) |>
  left_join(geo_dict, by = c("iso3_a" = "iso3")) |>
  rename(geo_zone_a = geo_zone) |>
  mutate(
    geo_zone_region = case_when(
      region == 1 ~ "rimland",
      region == 2 ~ "rimland",
      region == 3 ~ "rimland",
      region == 4 ~ "africa",
      region == 5 ~ "offshore",
      TRUE ~ NA_character_
    ),
    geo_zone = case_when(
      !is.na(geo_zone_territory) &
        geo_zone_territory != "unknown" ~ geo_zone_territory,
      !is.na(geo_zone_a) ~ geo_zone_a,
      !is.na(geo_zone_region) ~ geo_zone_region,
      TRUE ~ "unknown"
    ),
    rimland = if_else(geo_zone == "rimland", 1, 0),
    # convertir geo_zone a Capitalized
    geo_zone = stringr::str_to_title(geo_zone)
  ) |>
  select(-geo_zone_territory, -geo_zone_a, -geo_zone_region) |>
  rename(
    date_start = start_date,
    date_end = end_date
  )


# UCDP/GED events ------------------------------------------------------

ged_events_clean <- GEDEvent_v25_1 |>
  select(
    id,
    # year,
    active_year,
    type_of_violence,
    conflict_id = conflict_new_id,
    conflict_name,
    side_a_new_id,
    side_a,
    side_b_new_id,
    side_b,
    where_coordinates,
    # latitude,
    # longitude,
    country,
    country_id,
    ,
    region_desc = region,
    date_start,
    date_end,
    deaths = best,
    gwno_a = gwnoa,
    gwno_b = gwnob
  ) |>
  filter(
    active_year == 1,
    deaths > 0
  ) |>
  mutate(
    conflict_type = case_when(
      type_of_violence == 1 ~ "state-based",
      type_of_violence == 2 ~ "non-state",
      type_of_violence == 3 ~ "one-sided",
      TRUE ~ as.character(type_of_violence)
    )
  )

ged_confilcts <- ged_events_clean |>
  group_by(
    conflict_id
  ) |>
  summarise(
    n_events = n_distinct(id),
    deaths = sum(deaths, na.rm = TRUE),
    conflict_type = conflict_type[which.max(table(conflict_type))],
    conflict_name = first(conflict_name),
    side_a_id = side_a_new_id[which.max(table(side_a_new_id))],
    side_b_id = side_b_new_id[which.max(table(side_b_new_id))],
    side_a = side_a[which.max(table(side_a))],
    side_b = side_b[which.max(table(side_b))],
    where_coordinates = where_coordinates[which.max(table(where_coordinates))],
    country = country[which.max(table(country))],
    country_id = country_id[which.max(table(country_id))],
    region_desc = region_desc[which.max(table(region_desc))],
    date_start = min(date_start, na.rm = TRUE),
    date_end = max(date_end, na.rm = TRUE),
    gwno_a = gwno_a[which.max(table(gwno_a))],
    gwno_b = gwno_b[which.max(table(gwno_b))],
    .groups = "drop"
  ) |>
  mutate(
    months = as.numeric(date_end - date_start, units = "days") / 30.44,
    year = year(date_start),
    territory_name = str_c(where_coordinates, country, sep = " - ")
  ) |>
  select(
    -where_coordinates
  )

## Rimland UCDP/GED --------------------------------------------------------

territory_dict <- tibble::tribble(
  ~territory_name                              , ~geo_zone_territory ,
  "Jubar Top - India"                          , "rimland"           ,
  "Galwan valley - India"                      , "rimland"           ,
  "Preah Vihear temple - Cambodia (Kampuchea)" , "rimland"           ,
  "Panamá city - Panama"                      , "offshore"          ,
  "Iraq - Iraq"                                , "rimland"           ,
  "Coangos military post - Ecuador"            , "offshore"          ,
  "Bakassi peninsula - Cameroon"               , "africa"            ,
  "Debub region - Eritrea"                     , "africa"            ,
  "Ras Dumayrah point - Djibouti"              , "africa"            ,
  "Majak village - South Sudan"                , "africa"            ,
  "Dand Wa Patan district - Afghanistan"       , "rimland"           ,
  "Pionerskiy village - Russia (Soviet Union)" , "heartland"         ,
  "Kek-Tash village - Kyrgyzstan"              , "heartland"         ,
  "Masyaf town - Iraq"                         , "rimland"
)

## UCDP/GED full ----------------------------------------------------

full_ucdp_ged <- ged_confilcts |>
  mutate(
    iso3_a = countrycode(gwno_a, origin = "gwn", destination = "iso3c"),
    iso3_b = countrycode(gwno_b, origin = "gwn", destination = "iso3c"),
    cow_a = countrycode(gwno_a, origin = "gwn", destination = "cown"),
    cow_b = countrycode(gwno_b, origin = "gwn", destination = "cown")
  ) |>
  # Add religion data
  left_join(
    religion_full,
    by = c("cow_a" = "state", "year" = "year")
  ) |>
  rename(
    religion_a = religion,
    religion_pct_a = religion_pct
  ) |>
  left_join(
    religion_full,
    by = c("cow_b" = "state", "year" = "year")
  ) |>
  rename(
    religion_b = religion,
    religion_pct_b = religion_pct
  ) |>
  mutate(
    religion_dif = case_when(
      is.na(religion_a) | is.na(religion_b) ~ NA,
      religion_a == religion_b ~ 0,
      TRUE ~ 1
    )
  ) |>
  # Add contiguity data
  left_join(
    contiguity,
    by = c("cow_a" = "ccode_a", "cow_b" = "ccode_b", "year" = "year")
  ) |>
  # Add trade data
  left_join(
    trade,
    by = c("cow_a" = "ccode_a", "cow_b" = "ccode_b", "year" = "year")
  ) |>
  # Add political regime data
  left_join(
    outsource_political_regime,
    by = c("iso3_a" = "code", "year" = "year")
  ) |>
  rename(
    political_regime_a = political_regime
  ) |>
  left_join(
    outsource_political_regime,
    by = c("year" = "year", "iso3_b" = "code")
  ) |>
  rename(
    political_regime_b = political_regime
  ) |>
  mutate(
    political_regime_dif = case_when(
      is.na(political_regime_a) | is.na(political_regime_b) ~ NA,
      political_regime_a == political_regime_b ~ 0,
      TRUE ~ 1
    )
  ) |>
  # Add GDP per capita data
  left_join(
    gdp_long,
    by = c("iso3_a" = "country_code", "year" = "year")
  ) |>
  rename(
    gdp_percap_a = gdp_percap
  ) |>
  left_join(
    gdp_long,
    by = c("iso3_b" = "country_code", "year" = "year")
  ) |>
  rename(
    gdp_percap_b = gdp_percap
  ) |>
  mutate(
    gdp_gap_sq = (gdp_percap_a - gdp_percap_b)^2
  ) |>
  # Add language data
  left_join(
    language,
    by = c("iso3_a" = "iso3_a", "iso3_b" = "iso3_b")
  ) |>
  # Add resources data
  left_join(
    resources_country_year,
    by = c("cow_a" = "COWcode", "year" = "year")
  ) |>
  # Side A resources rename
  rename_with(
    ~ paste0(.x, "_a"),
    c(
      "has_resources",
      "lootable_sites",
      "nonlootable_sites",
      "resource_value",
      "nonlootable_value",
      "lootable_value",
      "log_resource_value",
      "log_nonlootable_value",
      "log_lootable_value"
    )
  ) |>
  left_join(
    resources_country_year,
    by = c("cow_b" = "COWcode", "year" = "year")
  ) |>
  # Side B resources rename
  rename_with(
    ~ paste0(.x, "_b"),
    c(
      "has_resources",
      "lootable_sites",
      "nonlootable_sites",
      "resource_value",
      "nonlootable_value",
      "lootable_value",
      "log_resource_value",
      "log_nonlootable_value",
      "log_lootable_value"
    )
  ) |>
  left_join(territory_dict, by = "territory_name") |>
  rename(geo_zone = geo_zone_territory) |>
  mutate(
    rimland = if_else(geo_zone == "rimland", 1, 0),
    # convertir geo_zone a Capitalized
    geo_zone = stringr::str_to_title(geo_zone)
  )


full_ucdp_prio |>
  filter(date_start >= as.Date("1989-01-01")) |>
  summarise(n_conflicts = n_distinct(conflict_id))

# Guardar datasets limpios ------------------------------------------------
saveRDS(full_ucdp_prio, file = "data_silver/full_ucdp_prio.rds")

# Save CSV
write_csv(full_ucdp_prio, file = "data_silver/full_ucdp_prio.csv")
