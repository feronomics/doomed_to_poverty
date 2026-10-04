## Poverty trap in Guatemala?
## PIs:  Fernando Sáenz
##      Javier Velásquez
##      Alejandro Milián
##
## Last modification: 02 Aug 2026
##
## Inputs:    data/processed/<RUN_ID>/income_2016.csv ... income_2019.csv
## Outputs:   data/processed/<RUN_ID>/data_2016-2019.csv
##
## Purpose: Merging
##
## Cambios (02 Aug 2026):
##  - Se conserva `n` (tamaño de celda) en lugar de descartarlo.
##  - Se propagan `n_eff` (tamaño efectivo de Kish) y `se_y1..se_y4`
##    (errores estándar dentro de celda) que produce build_cohorts().
##    Se conservan para diagnósticos de precisión de las cohortes.
##  - El filtro de completitud usa una lista explícita de columnas en
##    vez del rango posicional y1_1:y4_2 (frágil al reordenar).

library(tidyverse)

source("resources/pipeline_config.R")
source("resources/functions.R")

## =============== Append Data ===================================

files <- file.path(PROCESSED_DIR, c(
  "income_2016.csv",
  "income_2017.csv",
  "income_2018.csv",
  "income_2019.csv"
))

data_list <- lapply(files, read.table, sep = ",", header = TRUE)
data <- bind_rows(data_list)

## ============= Format variables ======================================

## Creating variable -- age 2019
data <- data |>
  mutate(substract = year - 2019, age_2019 = age - substract) |>
  select(-substract)

## Change variable names -- choosing simpler ones
## NOTA: ya NO se hace select(-n): `n` se necesita aguas abajo.
data <- data |>
  rename(
    y_lab    = avg_labor_income,
    y_ad     = avg_add_labor_income,
    y_nonlab = avg_no_labor_income,
    rem      = avg_remittances
  )

## Definiciones acumulativas de ingreso.
## La media de una suma es la suma de las medias, por eso esto es válido
## a nivel de cohorte. Los se_y1..se_y4 NO se construyen aquí: vienen de
## build_cohorts(), calculados a nivel individual para capturar las
## covarianzas entre componentes de ingreso.
data <- data |>
  mutate(
    y1 = y_lab,
    y2 = rowSums(across(c(y_lab, y_ad)), na.rm = TRUE),
    y3 = rowSums(across(c(y_lab, y_ad, y_nonlab)), na.rm = TRUE),
    y4 = rowSums(across(c(y_lab, y_ad, y_nonlab, rem)), na.rm = TRUE)
  )

## =================== Creating pairs ==================================

## Columnas que se duplican con sufijo _1 (año t) y _2 (año t+1).
pair_vars <- c(
  "weights_total", "n", "n_eff",
  "y1", "y2", "y3", "y4",
  "se_y1", "se_y2", "se_y3", "se_y4"
)

## Claves de emparejamiento entre cohortes de t y t+1.
key_vars <- c("age_2019", "education", "sample")

make_pair <- function(yr_t, yr_t1) {
  left <- data |>
    filter(year == yr_t) |>
    select(all_of(c(key_vars, pair_vars))) |>
    rename_with(~ paste0(., "_1"), all_of(pair_vars))

  right <- data |>
    filter(year == yr_t1) |>
    select(all_of(c(key_vars, pair_vars))) |>
    rename_with(~ paste0(., "_2"), all_of(pair_vars))

  inner_join(left, right, by = key_vars) |>
    mutate(comparison = paste0(yr_t, "-", yr_t1))
}

years_available <- sort(unique(data$year))
year_pairs <- data.frame(
  year_t  = head(years_available, -1),
  year_t1 = tail(years_available, -1)
)

data_wide <- bind_rows(mapply(
  make_pair,
  year_pairs$year_t,
  year_pairs$year_t1,
  SIMPLIFY = FALSE
))

## ============== Filtrar y ordenar columnas =======================

## El filtro cae SOLO sobre las medias de ingreso. Las columnas se_*
## pueden ser NA en celdas con n = 1 (no hay varianza estimable con una
## sola observación) sin que eso invalide la media. Ese caso se maneja
## dentro de loess_ci(), no aquí.
y_cols <- c("y1_1", "y2_1", "y3_1", "y4_1",
            "y1_2", "y2_2", "y3_2", "y4_2")

data_wide_completed <- data_wide |>
  filter(if_all(all_of(y_cols), ~ !is.na(.))) |>
  select(
    age_2019, education, sample, comparison,
    weights_total_1, weights_total_2,
    n_1, n_2, n_eff_1, n_eff_2,
    se_y1_1, se_y1_2, se_y2_1, se_y2_2,
    se_y3_1, se_y3_2, se_y4_1, se_y4_2,
    y1_1, y1_2, y2_1, y2_2, y3_1, y3_2, y4_1, y4_2
  )

## ============== Export  =======================================

write_csv(data_wide_completed, file.path(PROCESSED_DIR, "data_2016-2019.csv"))

log_msg(
  "Merged 2016-2019: ", nrow(data_wide_completed), " cohort pairs | ",
  sum(data_wide_completed$n_1 < 10 | data_wide_completed$n_2 < 10),
  " pairs with a cell below n = 10"
)