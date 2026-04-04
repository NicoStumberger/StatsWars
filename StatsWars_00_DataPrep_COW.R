library(tidyverse)
library(readr)
library(lubridate)
library(haven)
library(countrycode)


# Input ------------------------------------------------------------------

# Fuente: correlatesofwar.org
# https://correlatesofwar.org/data-sets/cow-war/
# COW War data has 4 datasets regarding different types of wars:

cow_intra_state <- read_csv("data_raw/INTRA-STATE WARS v5.1 CSV.csv") |>
  janitor::clean_names()

cow_inter_state <- read_csv("data_raw/Inter-StateWarData_v4.0.csv") |>
  janitor::clean_names()

cow_inter_state_dyads <- read_csv("data_raw/directed_dyadic_war.csv") |> # Este creo q no uso
  janitor::clean_names()

cow_extra_state <- read_csv("data_raw/Extra-StateWarData_v4.0.csv") |>
  janitor::clean_names()

cow_non_state <- read_csv("data_raw/Non-StateWarData_v4.0.csv") |>
  janitor::clean_names()

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

## Conflictos Intra estatales ---------------------------------------------

# Preparar datasets individuales
# Conflictos intra-estatales
intra <- cow_intra_state |>
  select(
    war_num,
    start_yr1,
    start_mo1,
    start_dy1,
    end_yr1,
    end_mo1,
    end_dy1,
    start_yr2,
    start_yr3,
    start_yr4,
    deaths = total_b_deaths,
    ccode_a,
    side_a,
    side_b,
    months = w_durat_mo,
    type = war_type,
    region = v5region_num
  ) |>
  mutate(
    start_yr1 = na_if(start_yr1, -8),
    start_yr2 = na_if(start_yr2, -8),
    start_yr3 = na_if(start_yr3, -8),
    start_yr4 = na_if(start_yr4, -8)
  ) |>
  mutate(
    region_desc = case_when(
      region == 1 ~ "North America",
      region == 2 ~ "South America",
      region == 3 ~ "Europe",
      region == 4 ~ "Sub-Saharan Africa",
      region == 5 ~ "Middle East and North Africa",
      region == 6 ~ "Asia and Oceania",
      TRUE ~ "Other"
    ),
    type_desc = case_when(
      type == 4 ~ "Civil war for central control",
      type == 5 ~ "Civil war over local issues",
      type == 6 ~ "Regional internal",
      type == 7 ~ "Intercommunal",
      TRUE ~ "Other"
    ),
    start_yr1 = case_when(start_yr1 < 1 ~ NA, TRUE ~ start_yr1),
    start_mo1 = case_when(start_mo1 < 1 ~ 1, TRUE ~ start_mo1),
    start_dy1 = case_when(start_dy1 < 1 ~ 1, TRUE ~ start_dy1),
    end_yr1 = case_when(end_yr1 < 1 ~ NA, TRUE ~ end_yr1),
    end_mo1 = case_when(end_mo1 < 1 ~ 12, TRUE ~ end_mo1),
    end_dy1 = as.numeric(end_dy1),
    end_dy1 = case_when(is.na(end_dy1) | end_dy1 < 1 ~ 28, TRUE ~ end_dy1),
  ) |>
  mutate(
    start_date = make_date(start_yr1, start_mo1, start_dy1),
    end_date = make_date(end_yr1, end_mo1, end_dy1),
    year = year(start_date)
  ) |>
  rowwise() |>
  mutate(
    # Episodios válidos: start_yearX no es NA
    n_events = sum(!is.na(c(start_yr1, start_yr2, start_yr3, start_yr4))),

    # Actores válidos: side_a y side_b no son NA
    n_actors_a = sum(!is.na(c(side_a))),
    n_actors_b = sum(!is.na(c(side_b))),
    n_actors = n_actors_a + n_actors_b
  ) |>
  ungroup() |>
  select(
    -c(
      start_yr1,
      start_mo1,
      start_dy1,
      end_yr1,
      end_mo1,
      end_dy1,
      start_yr2,
      start_yr3,
      start_yr4
    )
  )


## Conlfictos Inter estatales ---------------------------------------------

# Conflictos inter-estatales
# Cada observacion es un estado participante en un conflicto. No un conlficto en si mismo.
# Para obtener los conflictos, hay que agrupar por war_num.
inter <- cow_inter_state |>
  select(
    war_num,
    start_year1,
    start_month1,
    start_day1,
    end_year1,
    end_month1,
    end_day1,
    deaths = bat_death,
    type = war_type,
    ccode,
    state_name,
    side,
    region = where_fought,
    deaths = bat_death
  ) |>
  mutate(
    region_desc = case_when(
      region == 1 ~ "W. Hemisphere",
      region == 2 ~ "Europe",
      region == 4 ~ "Africa",
      region == 6 ~ "Middle East",
      region == 7 ~ "Asia",
      region == 9 ~ "Oceania",
      region == 11 ~ "Europe & Middle East",
      region == 12 ~ "Europe & Asia",
      region == 13 ~ "W. Hemisphere & Asia",
      region == 14 ~ "Europe, Africa & Middle East",
      region == 15 ~ "Europe, Africa, Middle East, & Asia",
      region == 16 ~ "Africa, Middle East, Asia & Oceania",
      region == 17 ~ "Asia & Oceania",
      region == 18 ~ "Africa & Middle East",
      region == 19 ~ "Europe, Africa, Middle East, Asia & Oceania",
      TRUE ~ "Other"
    ),
    type_desc = case_when(
      type == 1 ~ "Inter-state war",
      TRUE ~ "Other"
    ),
    start_year1 = case_when(start_year1 < 1 ~ NA, TRUE ~ start_year1),
    start_month1 = case_when(start_month1 < 1 ~ 1, TRUE ~ start_month1),
    start_day1 = case_when(start_day1 < 1 ~ 1, TRUE ~ start_day1),
    end_year1 = case_when(end_year1 < 1 ~ NA, TRUE ~ end_year1),
    end_month1 = case_when(end_month1 < 1 ~ 12, TRUE ~ end_month1),
    end_day1 = case_when(end_day1 < 1 ~ 28, TRUE ~ end_day1),
  ) |>
  mutate(
    start_date = make_date(start_year1, start_month1, start_day1),
    end_date = make_date(end_year1, end_month1, end_day1)
  ) |>
  select(
    -c(start_year1, start_month1, start_day1, end_year1, end_month1, end_day1)
  ) |>
  group_by(
    war_num
  ) |>
  summarise(
    deaths = sum(deaths, na.rm = TRUE),
    start_date = min(start_date),
    end_date = max(end_date),
    type = min(type),
    type_desc = min(type_desc),
    region = min(region),
    region_desc = min(region_desc),
    n_actors = n(),
    .groups = "drop"
  ) |>
  mutate(
    months = as.numeric(end_date - start_date, units = "days") / 30.44,
    year = year(start_date)
  )

# Ahora deberia merge con dyads q esta a nivel de eventos.
# Agrupar por warnum, side (pivot long), ccount$max(count(ccode)) --> state q mas se repite por lado
# n() para contar numero de eventos (intensidad)
# Pivot wide: warnum, side_a, side_b

inter_events <- cow_inter_state_dyads |>
  group_by(
    war_num = warnum
  ) |>
  summarise(
    n_events = n(),
    .groups = "drop"
  )

inter_sides <- cow_inter_state |>
  select(
    war_num,
    ccode,
    side
  ) |>
  group_by(
    war_num,
    side
  ) |>
  summarise(
    top_ccode = ccode[which.max(tabulate(match(ccode, ccode)))],
    n_actors = n(),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = side,
    values_from = c(top_ccode, n_actors)
  ) |>
  rename(
    ccode_a = top_ccode_1,
    ccode_b = top_ccode_2,
    n_actors_a = n_actors_1,
    n_actors_b = n_actors_2
  )

# Merge inter
inter <- inter |>
  left_join(inter_events, by = "war_num") |>
  left_join(inter_sides, by = "war_num")


## Conflictos Extra estatales ---------------------------------------------

# Conflictos extra-estatales
extra <- cow_extra_state |>
  select(
    war_num,
    type = war_type,
    ccode_a = ccode1,
    ccode_b = ccode2,
    side_a,
    side_b,
    start_year1,
    start_month1,
    start_day1,
    end_year1,
    end_month1,
    end_day1,
    start_year2,
    region = where_fought
    # bat_death,
    # non_state_deaths
  ) |>
  mutate(
    region_desc = case_when(
      region == 1 ~ "W. Hemisphere",
      region == 2 ~ "Europe",
      region == 4 ~ "Africa",
      region == 6 ~ "Middle East",
      region == 7 ~ "Asia",
      TRUE ~ "Other"
    ),
    type_desc = case_when(
      type == 2 ~ "Colonial war",
      type == 3 ~ "Imperial war",
      TRUE ~ "Other"
    ),
    start_year2 = na_if(start_year2, -8),
    ccode_a = na_if(ccode_a, -8),
    ccode_b = na_if(ccode_b, -8),
    side_a = na_if(side_a, "-8"),
    side_b = na_if(side_b, "-8"),
    # bat_death = ifelse(bat_death < 0, NA, bat_death),
    # non_state_deaths = ifelse(non_state_deaths < 0, NA, non_state_deaths),
    start_year1 = case_when(start_year1 < 1 ~ NA, TRUE ~ start_year1),
    start_month1 = case_when(start_month1 < 1 ~ 1, TRUE ~ start_month1),
    start_day1 = case_when(start_day1 < 1 ~ 1, TRUE ~ start_day1),
    end_year1 = case_when(end_year1 < 1 ~ NA, TRUE ~ end_year1),
    end_month1 = case_when(end_month1 < 1 ~ 12, TRUE ~ end_month1),
    end_day1 = case_when(end_day1 < 1 ~ 28, TRUE ~ end_day1),
  ) |>
  mutate(
    start_date = make_date(start_year1, start_month1, start_day1),
    end_date = make_date(end_year1, end_month1, end_day1),
    months = as.numeric(end_date - start_date, units = "days") / 30.44,
    year = year(start_date)
  ) |>

  rowwise() |>
  mutate(
    # Episodios válidos: start_yearX no es NA
    n_events = sum(!is.na(c(start_year1, start_year2)))
  ) |>
  ungroup() |>
  select(
    -c(
      start_year1,
      start_month1,
      start_day1,
      end_year1,
      end_month1,
      end_day1,
      start_year2
    )
  )

extra_nactors <- cow_extra_state |>
  select(
    war_num,
    side_a,
    side_b
  ) |>
  pivot_longer(
    cols = c(side_a, side_b),
    names_to = "side",
    values_to = "actor"
  ) |>
  mutate(
    actor = na_if(actor, "-8")
  ) |>
  group_by(
    war_num,
    side
  ) |>
  summarise(
    n_actors = sum(!is.na(actor)),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = side,
    values_from = n_actors
  ) |>
  rename(
    n_actors_a = side_a,
    n_actors_b = side_b
  ) |>
  mutate(
    n_actors = n_actors_a + n_actors_b
  )

extra_sides_0 <- cow_extra_state |>
  select(
    war_num,
    side_a,
    side_b,
    bat_death,
    non_state_deaths
  ) |>
  mutate(
    side_a = na_if(side_a, "-8"),
    side_b = na_if(side_b, "-8"),
    bat_death = ifelse(bat_death < 0, NA, bat_death),
    non_state_deaths = ifelse(non_state_deaths < 0, NA, non_state_deaths)
  )

extra_sides_a <- extra_sides_0 |>
  select(
    war_num,
    actor = side_a,
    deaths = bat_death,
  )

extra_sides_b <- extra_sides_0 |>
  select(
    war_num,
    actor = side_b,
    deaths = non_state_deaths
  )

extra_sides_1 <- bind_rows(
  side_a = extra_sides_a,
  side_b = extra_sides_b,
  .id = "side"
) |>
  filter(!is.na(actor))

extra_deaths <- extra_sides_1 |>
  group_by(
    war_num
  ) |>
  summarise(
    deaths = sum(deaths, na.rm = TRUE)
  ) |>
  ungroup()

extra_sides <- extra_sides_1 |>
  group_by(war_num, side) |>
  slice_max(deaths, n = 1, with_ties = FALSE) |>
  ungroup() |>
  pivot_wider(
    names_from = side,
    values_from = c(actor, deaths)
  ) |>
  # mutate(
  #   deaths = rowSums(across(starts_with("deaths_")), na.rm = TRUE)
  # ) |>
  select(
    -c(deaths_side_a, deaths_side_b)
  ) |>
  rename(
    side_a = actor_side_a,
    side_b = actor_side_b
  )

extra_ext <- extra_deaths |>
  left_join(extra_sides, by = "war_num") |>
  left_join(extra_nactors, by = "war_num")

extra <- extra_ext |>
  left_join(extra, by = c("war_num", "side_a", "side_b"))


## Conflictos No estatales ------------------------------------------------

# Conflictos no estatales
non <- cow_non_state |>
  select(
    war_num,
    start_year,
    start_month,
    start_day,
    end_year,
    end_month,
    end_day,
    deaths = total_combat_deaths,
    type = war_type,
    region = where_fought,
    side_a1,
    side_a2,
    side_b1,
    side_b2,
    side_b3,
    side_b4,
    side_b5
  ) |>
  mutate(
    deaths = ifelse(deaths < 0, NA, deaths),
    side_a1 = na_if(side_a1, "-8"),
    side_a2 = na_if(side_a2, "-8"),
    side_b1 = na_if(side_b1, "-8"),
    side_b2 = na_if(side_b2, "-8"),
    side_b3 = na_if(side_b3, "-8"),
    side_b4 = na_if(side_b4, "-8"),
    side_b5 = na_if(side_b5, "-8"),
    region_desc = case_when(
      region == 1 ~ "W. Hemisphere",
      region == 2 ~ "Europe",
      region == 4 ~ "Africa",
      region == 6 ~ "Middle East",
      region == 7 ~ "Asia",
      region == 9 ~ "Oceania",
      TRUE ~ "Other"
    ),
    type_desc = case_when(
      type == 8 ~ "in non-state territory",
      type == 9 ~ "take place across state borders",
      TRUE ~ "Other"
    ),
    start_year = case_when(start_year < 1 ~ NA, TRUE ~ start_year),
    start_month = case_when(start_month < 1 ~ 1, TRUE ~ start_month),
    start_day = case_when(start_day < 1 ~ 1, TRUE ~ start_day),
    end_year = case_when(end_year < 1 ~ NA, TRUE ~ end_year),
    end_month = case_when(end_month < 1 ~ 12, TRUE ~ end_month),
    end_day = case_when(end_day < 1 ~ 28, TRUE ~ end_day),
  ) |>
  mutate(
    start_date = make_date(start_year, start_month, start_day),
    end_date = make_date(end_year, end_month, end_day),
    months = as.numeric(end_date - start_date, units = "days") / 30.44,
    year = year(start_date),
    n_actors_a = rowSums(!is.na(across(c(side_a1, side_a2)))),
    n_actors_b = rowSums(
      !is.na(across(c(side_b1, side_b2, side_b3, side_b4, side_b5)))
    )
  ) |>
  select(
    -c(start_year, start_month, start_day, end_year, end_month, end_day),
    # conservar solo side_a1 y side_b1
    -side_a2,
    -side_b2,
    -side_b3,
    -side_b4,
    -side_b5
  ) |>
  mutate(
    n_actors = n_actors_a + n_actors_b
  ) |>
  rename(
    side_a = side_a1,
    side_b = side_b1
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

rimland_regions <- c(
  "Europe",
  "Middle East",
  "Middle East and North Africa",
  "Europe & Middle East",
  "Asia",
  "Asia and Oceania",
  "Africa & Middle East",
  "W. Hemisphere & Asia"
)

heartland_countries <- c(
  "Kazakhstan",
  "Uzbekistan",
  "Turkmenistan",
  "Tajikistan",
  "Kyrgyzstan",
  "Mongolia",
  "Russia" # Rusia es híbrida, pero podés dejarla acá o en Rimland
)

rimland_countries <- c(
  "China",
  "Japan",
  "South Korea",
  "North Korea",
  "Taiwan",
  "India",
  "Pakistan",
  "Bangladesh",
  "Sri Lanka",
  "Nepal",
  "Vietnam",
  "Thailand",
  "Malaysia",
  "Indonesia",
  "Philippines",
  "Myanmar",
  "Cambodia",
  "Laos",
  "Singapore",
  "Turkey",
  "Iran",
  "Iraq",
  "Syria",
  "Lebanon",
  "Israel",
  "Jordan",
  "Saudi Arabia",
  "Yemen",
  "Oman",
  "Kuwait",
  "Qatar",
  "UAE",
  "Bahrain",
  "Egypt",
  "Libya",
  "Tunisia",
  "Algeria",
  "Morocco",
  "France",
  "Germany",
  "Italy",
  "Spain",
  "Portugal",
  "Greece",
  "Poland",
  "Hungary",
  "Austria",
  "Belgium",
  "Netherlands",
  "United Kingdom",
  "Ukraine",
  "Belarus",
  "Romania",
  "Bulgaria",
  "Serbia",
  "Croatia",
  "Bosnia",
  "Montenegro",
  "Albania",
  "North Macedonia"
)

rimland_keywords <- c(
  # Middle East
  "Kurds",
  "Kurd",
  "Druze",
  "Shiite",
  "Shi'ite",
  "Sunni",
  "Bedouin",
  "Palestin",
  "Houthi",
  "Maronite",
  "Aleppo",
  "Damascus",
  "Sidon",
  "Hejaz",
  "Asir",
  "Hasa",
  # Caucasus
  "Chechen",
  "Chechnya",
  "Dagestan",
  "Caucasus",
  "Murids",
  "Circass",
  "Ingush",
  # South/East Asia
  "Tamil",
  "Hindu",
  "Muslim",
  "Bengal",
  "Moro",
  "Khmer",
  "Karen",
  "Kachin",
  "Hmong",
  "Miao",
  "Hui",
  "Uighur",
  "Uyghur",
  "Tibet",
  "Xinjiang",
  "Shaanxi",
  "Gansu",
  "Sichuan",
  "Yunnan",
  "Guizhou",
  "Kashgar",
  "Khotan",
  "Tokugawa",
  "Taiping",
  "Nien",
  "Tonghak",
  "Satsuma",
  # Balkans
  "Bosni",
  "Serb",
  "Croat",
  "Montenegro",
  "Herzegovina",
  "Macedonia",
  "Alban",
  "Bulgar",
  "Roman",
  "Ukrain",
  "Belarus",
  "Polish",
  "Hungar",
  # Cities
  "Beirut",
  "Baghdad",
  "Tehran",
  "Istanbul",
  "Ankara",
  "Jerusalem",
  "Gaza",
  "Cairo",
  "Alexandria",
  "Tripoli",
  "Benghazi"
)

offshore_regions <- c(
  "North America",
  "South America",
  "W. Hemisphere",
  "Oceania"
)

africa_regions <- c(
  "Sub-Saharan Africa",
  "Africa"
)

heartland_keywords <- c(
  "Kazakh",
  "Kazaks",
  "Kazak",
  "Kyrgyz",
  "Kirghiz",
  "Uzbek",
  "Turkmen",
  "Tajik",
  "Tajiks",
  "Basmachi",
  "Steppe",
  "Siberia",
  "Mongol",
  "Mongolia",
  "Ural",
  "Samarkand",
  "Bukhara",
  "Kokand",
  "Turan"
)

offshore_keywords <- c(
  "Texas",
  "Buenos Aires",
  "Entre Rios",
  "Bahia",
  "Pernambuco",
  "Yucatan",
  "Haiti",
  "Dominican",
  "Maya",
  "Mapuche",
  "Aymara",
  "Quechua",
  "Guarani",
  "Maori",
  "Aborigines",
  "Australia",
  "Canada",
  "New Zealand",
  "Chile",
  "Argentina",
  "Peru",
  "Bolivia",
  "Brazil",
  "Colombia",
  "Venezuela",
  "Uruguay",
  "Paraguay",
  "Ecuador",
  "Panama",
  "Costa Rica",
  "Nicaragua",
  "Honduras",
  "Guatemala",
  "El Salvador",
  "Mexico"
)

africa_keywords <- c(
  "Zulu",
  "Xhosa",
  "Basuto",
  "Ashanti",
  "Tutsi",
  "Hutu",
  "Fulani",
  "Tarok",
  "Ogaden",
  "Somali",
  "Eritrea",
  "Ethiopia",
  "Sudan",
  "Dinka",
  "Nuer",
  "Biafra",
  "Igbo",
  "Hausa",
  "Tuareg",
  "Berber",
  "Kongo",
  "Congo",
  "Angola",
  "Mozambique",
  "Rwanda",
  "Burundi",
  "Chad",
  "Niger",
  "Mali",
  "FRELIMO",
  "RENAMO",
  "SPLA",
  "SPLM",
  "JEM",
  "Seleka",
  "Boko Haram"
)


## Unir datasets ----------------------------------------------------------

# Unir datasets en uno solo
cow <- bind_rows(
  intra = intra,
  inter = inter,
  extra = extra,
  non = non,
  .id = "conflict_type"
)

# Merge por ccode_a
merge_a_ccode <- cow |>
  left_join(cow_country_codes, by = c("ccode_a" = "c_code")) |>
  mutate(
    side_a = case_when(
      is.na(side_a) ~ state_nme,
      TRUE ~ side_a
    )
  ) |>
  select(-state_nme)

# Merge por nombre cuando ccode_a es NA
merge_a_name <- cow |>
  filter(is.na(ccode_a)) |>
  left_join(cow_country_codes, by = c("side_a" = "state_nme")) |>
  mutate(
    ccode_a = case_when(
      is.na(ccode_a) ~ c_code,
      TRUE ~ ccode_a
    )
  ) |>
  select(-c_code)

# Unir ambos resultados
merge_a <- merge_a_ccode |>
  rows_patch(merge_a_name, by = "war_num")

# Merge por ccode_b
merge_b_ccode <- merge_a |>
  left_join(cow_country_codes, by = c("ccode_b" = "c_code")) |>
  mutate(
    side_b = case_when(
      is.na(side_b) ~ state_nme,
      TRUE ~ side_b
    )
  ) |>
  select(-state_nme)

# Merge por nombre cuando ccode_b es NA
merge_b_name <- merge_a |>
  filter(is.na(ccode_b)) |>
  left_join(cow_country_codes, by = c("side_b" = "state_nme")) |>
  mutate(
    ccode_b = case_when(
      is.na(ccode_b) ~ c_code,
      TRUE ~ ccode_b
    )
  ) |>
  select(-c_code)

cow_2 <- merge_b_ccode |>
  rows_patch(merge_b_name, by = "war_num") |>
  rename(
    state_abb_a = state_abb.x,
    state_abb_b = state_abb.y
  )

# Unir con religion, contiguity y trade

full_cow <- cow_2 |>
  left_join(
    religion_full,
    by = c("year" = "year", "ccode_a" = "state")
  ) |>
  rename(
    religion_a = religion,
    religion_pct_a = religion_pct
  ) |>
  left_join(
    religion_full,
    by = c("year" = "year", "ccode_b" = "state")
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
  left_join(
    contiguity,
    by = c("year" = "year", "ccode_a" = "ccode_a", "ccode_b" = "ccode_b")
  ) |>
  left_join(
    trade,
    by = c("year" = "year", "ccode_a" = "ccode_a", "ccode_b" = "ccode_b")
  ) |>
  left_join(
    outsource_political_regime,
    by = c("year" = "year", "state_abb_a" = "code")
  ) |>
  rename(
    political_regime_a = political_regime
  ) |>
  left_join(
    outsource_political_regime,
    by = c("year" = "year", "state_abb_b" = "code")
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
  left_join(
    gdp_long,
    by = c("year" = "year", "state_abb_a" = "country_code")
  ) |>
  rename(
    gdp_percap_a = gdp_percap
  ) |>
  left_join(
    gdp_long,
    by = c("year" = "year", "state_abb_b" = "country_code")
  ) |>
  rename(
    gdp_percap_b = gdp_percap
  ) |>
  mutate(
    gdp_gap_sq = (gdp_percap_a - gdp_percap_b)^2
  ) |>
  left_join(
    language,
    by = c("state_abb_a" = "iso3_a", "state_abb_b" = "iso3_b")
  ) |>
  # Merge para side A
  left_join(
    resources_country_year,
    by = c("ccode_a" = "COWcode", "year" = "year")
  ) |>
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

  # Merge para side B
  left_join(
    resources_country_year,
    by = c("ccode_b" = "COWcode", "year" = "year")
  ) |>
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
  mutate(
    rimland = as.integer(
      region_desc %in%
        rimland_regions |
        side_a %in% rimland_countries |
        side_b %in% rimland_countries |
        str_detect(side_a, paste(rimland_keywords, collapse = "|")) |
        str_detect(side_b, paste(rimland_keywords, collapse = "|"))
    )
  ) |>
  mutate(
    geo_zone = case_when(
      # Rimland (región, país, keywords)
      region_desc %in% rimland_regions ~ "Rimland",
      side_a %in% rimland_countries | side_b %in% rimland_countries ~ "Rimland",
      str_detect(side_a, paste(rimland_keywords, collapse = "|")) |
        str_detect(side_b, paste(rimland_keywords, collapse = "|")) ~ "Rimland",

      # Heartland (país, keywords)
      side_a %in%
        heartland_countries |
        side_b %in% heartland_countries ~ "Heartland",
      str_detect(side_a, paste(heartland_keywords, collapse = "|")) |
        str_detect(
          side_b,
          paste(heartland_keywords, collapse = "|")
        ) ~ "Heartland",

      # Africa (región, keywords)
      region_desc %in% africa_regions ~ "Africa",
      str_detect(side_a, paste(africa_keywords, collapse = "|")) |
        str_detect(side_b, paste(africa_keywords, collapse = "|")) ~ "Africa",

      # Offshore (región, keywords)
      region_desc %in% offshore_regions ~ "Offshore",
      str_detect(side_a, paste(offshore_keywords, collapse = "|")) |
        str_detect(
          side_b,
          paste(offshore_keywords, collapse = "|")
        ) ~ "Offshore",

      # Fallback
      TRUE ~ "Offshore"
    )
  ) |>
  rename(
    cow_a = ccode_a,
    cow_b = ccode_b,
    iso3_a = state_abb_a,
    iso3_b = state_abb_b,
    date_start = start_date,
    date_end = end_date
  ) |>
  # Reemplazar -9 por NA en deaths
  mutate(
    deaths = ifelse(deaths < 0, NA, deaths)
  )

# Guardar datasets limpios ------------------------------------------------

saveRDS(full_cow, file = "data_silver/full_cow.rds")

# Save CSV
write_csv(full_cow, file = "data_silver/full_cow.csv")
