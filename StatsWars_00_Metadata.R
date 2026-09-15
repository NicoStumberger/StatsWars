library(tidyverse)

# Input datasets ------------------------------------------------
full_sdq <- readRDS(file = "data_silver/full_sdq.rds")
full_cow <- readRDS(file = "data_silver/full_cow.rds")
full_ucdp_prior <- readRDS(file = "data_silver/full_ucdp_prior.rds")

# Functions ---------------------------------------------------------------
coverage_table <- function(df) {
  tibble(
    variable = names(df),
    n_not_na = sapply(df, function(x) sum(!is.na(x))),
    total = nrow(df),
    coverage = round(n_not_na / total, 4)
  )
}


metadata_tabla <- function(df) {
  # Detectar tipo de variable
  detectar_tipo <- function(x) {
    if (is.numeric(x)) {
      return("numeric")
    }
    if (inherits(x, "Date")) {
      return("date")
    }
    if (is.character(x)) {
      return("character")
    }
    if (is.factor(x)) {
      return("factor")
    }
    if (is.logical(x)) {
      return("logical")
    }
    return("other")
  }

  # Porcentaje de completitud
  pct_completo <- function(x) {
    round(mean(!is.na(x)) * 100, 3)
  }

  # Estadisticas numericas seguras
  safe_mean <- function(x) {
    if (is.numeric(x)) round(mean(x, na.rm = TRUE), 6) else NA
  }
  safe_min <- function(x) {
    if (is.numeric(x)) round(min(x, na.rm = TRUE), 6) else NA
  }
  safe_max <- function(x) {
    if (is.numeric(x)) round(max(x, na.rm = TRUE), 6) else NA
  }

  # Convertir numeros a formato con coma (vectorizado)
  coma <- function(vec) {
    sapply(vec, function(x) {
      if (is.na(x)) {
        return("")
      }
      format(x, decimal.mark = ",", big.mark = "", scientific = FALSE)
    })
  }

  # Construccion del data frame final
  df_meta <- data.frame(
    variable = names(df),
    descripcion = "",
    fuente = "",
    tipo = sapply(df, detectar_tipo),
    unidad = "",
    porcentaje_completo = sapply(df, pct_completo),
    n_unicos = sapply(df, function(x) length(unique(x))),
    media = sapply(df, safe_mean),
    minimo = sapply(df, safe_min),
    maximo = sapply(df, safe_max),
    observaciones = "",
    stringsAsFactors = FALSE
  )

  # Aplicar formato con coma
  df_meta$porcentaje_completo <- coma(df_meta$porcentaje_completo)
  df_meta$n_unicos <- coma(df_meta$n_unicos)
  df_meta$media <- coma(df_meta$media)
  df_meta$minimo <- coma(df_meta$minimo)
  df_meta$maximo <- coma(df_meta$maximo)

  return(df_meta)
}

# Coverage ----------------------------------------------------------------
cov_cow <- coverage_table(full_cow)
cov_ucdp <- coverage_table(full_ucdp_prio)
cov_sdq <- coverage_table(full_sdq)


# Tabla de metadatos por dataset -----------------------------------------
metadata_sdq <- metadata_tabla(full_sdq)
metadata_cow <- metadata_tabla(full_cow)
metadata_ucdp <- metadata_tabla(full_ucdp_prio)


# Guardar tablas de cobertura ---------------------------------------------
write_csv(cov_cow, file = "data_metadata/coverage_cow.csv")
write_csv(cov_ucdp, file = "data_metadata/coverage_ucdp_prio.csv")
write_csv(cov_sdq, file = "data_metadata/coverage_sdq.csv")

# Guardar tablas de metadatos ---------------------------------------------
write.csv(
  metadata_sdq,
  "data_metadata/metadata_full_sdq.csv",
  row.names = FALSE
)
write.csv(
  metadata_cow,
  "data_metadata/metadata_full_cow.csv",
  row.names = FALSE
)
write.csv(
  metadata_ucdp,
  "data_metadata/metadata_full_ucdp_prio.csv",
  row.names = FALSE
)
