## Poverty trap in Guatemala?
## PIs:  Fernando Sáenz
##       Javier Velásquez
##       Alejandro Milián
##
## Last modification: 24 May 2026
##
## source: https://www.ine.gob.gt/encuesta-nacional-de-empleo-e-ingresos/
## Inputs:    data/raw/ENEI_2016.sav
## Outputs:   data/processed/income_2016.csv
##
## Purpose: Selecting all variables regarding this research, transforming income into 12-month and building
## pseaudo panel


library(tidyverse)
library(foreign)
library(Hmisc)
library(yaml)

source("resources/functions.R")
source("resources/pipeline_config.R")

## ============== Load data and configuration ===============================

## Load data
enei <- read.spss("data/raw/ENEI_2016.sav", to.data.frame = TRUE)

log_msg("Loaded ENEI_2016",".sav: ",nrow(enei)," rows, ",ncol(enei)," columns")

## Loaf config. Here we grouped all variables in their respective category
cfg <- read_yaml("resources/config_2016.yaml")
year <- 2016

## Group all variables
all_vars <- c(
  cfg$id_vars,
  cfg$demographic_vars,
  cfg$labor_vars,
  cfg$add_labor_vars,
  cfg$no_labor_vars,
  cfg$remittance_vars
)

## ============== Select variables of interest ===============================

## Choose variables of interest:
dat <- enei |>
  select(all_of(all_vars))

## ============== Rename variables ===============================

## Renaming demographic variables and creating monthly income:
## (we rename first so every step below works with readable names)
dat <- dat |>
  rename(
    area = DOMINIO,
    weights = FACTOR,
    gender = PPA02,

    age = PPA03,
    ethnicity = PPA06,
    education = P03A05A,
    employed = OCUPADOS
  )

## ============== Transform variables (types and income) ===============================

## Defensive: make sure every income column is numeric even if guess_max
## still mis-typed something (e.g. an all-NA logical column).
## (the exclusion list now uses the already-renamed names)
dat <- dat |>
  mutate(
    across(
      -c(NUMHOG, area, weights, gender, age, ethnicity, education, employed),
      as.numeric
    ),
    across(all_of(cfg$div_12), ~ .x / 12),
    across(all_of(cfg$div_3), ~ .x / 3)
  )

## ============== Build income components ===============================

## Income component groups kept as named vectors so they are defined once and
## reused both for rowSums() and for the select(-...) drop below.
dat_renamed <- dat %>%
  mutate(
    labor_income = rowSums(across(all_of(cfg$labor_vars)), na.rm = TRUE),
    add_labor_income = rowSums(across(all_of(cfg$add_labor_vars)), na.rm = TRUE),
    no_labor_income = rowSums(across(all_of(cfg$no_labor_vars)), na.rm = TRUE),
    remittances = ifelse(
      rowSums(across(all_of(cfg$remittance_vars)), na.rm = TRUE) <= 0,
      0,
      rowMeans(across(all_of(cfg$remittance_vars)), na.rm = TRUE)
    )
  ) %>%
  select(-all_of(c(cfg$labor_vars, cfg$add_labor_vars, cfg$no_labor_vars, cfg$remittance_vars)))

log_msg("Built income components for ", nrow(dat_renamed), " individuals")

## =================== Calculating Income Quantile ================================

## Calculate the household income (to later test different strata within the income dist.)
dat_hhincome <- dat_renamed %>%
  dplyr::group_by(NUMHOG) %>%
  dplyr::summarize(
    hh_labor_income = sum(labor_income, na.rm = TRUE),
    hh_weight = mean(weights, na.rm = TRUE),
    hh_size = n()
  ) %>%
  dplyr::mutate(hh_pc_income = hh_labor_income / hh_size)

## We compute the percentiles (Income distribution)
p <- wtd.quantile(
  dat_hhincome$hh_pc_income,
  weights = dat_hhincome$hh_weight,
  probs = c(0.50, 0.75, 0.90, 0.99)
)
p50 <- p[1]
p75 <- p[2]
p90 <- p[3]
p99 <- p[4]

## We assign each case to one of the strata
dat_hhincome <- dat_hhincome %>%
  mutate(
    quantile = case_when(
      hh_pc_income <= p50 ~ "p50",
      hh_pc_income > p50 & hh_pc_income <= p75 ~ "p75",
      hh_pc_income > p75 & hh_pc_income <= p90 ~ "p90",
      hh_pc_income > p90 & hh_pc_income <= p99 ~ "p99",
      TRUE ~ "+p99"
    )
  ) %>%
  select(-hh_weight)

## Merge with individual-level data
dat_final <- left_join(dat_renamed, dat_hhincome, by = "NUMHOG")

## ============== Recoding / Cleaning ===============================

## Filter the population in working age (and employed)
## OCUPADOS == 1 marks the employed population in the numeric coding.
dat_pet <- dat_final %>%
  filter(age >= AGE_MIN & age <= AGE_MAX & employed == "Población ocupada")

log_msg(age_filter_label(), nrow(dat_pet), " individuals")

##

dat_pet <- dat_pet %>%
  mutate(
    area = case_when(
      area == "RURAL NACIONAL" ~ "Rural",
      TRUE ~ "Urban" # URBANO METROPOLITANO, RESTO URBANO
    ),
    area = as.factor(area),

    ethnicity = ifelse(
      ethnicity %in% c("Maya", "Garífuna", "Xinka"),
      "Indigenous",
      "Non-indigenous" # Ladino, Extranjero
    ),
    ethnicity = as.factor(ethnicity),

    gender = ifelse(gender == "Hombre", "Male", "Female"),
    gender = as.factor(gender),

    education = ifelse(
      education %in% c("Ninguno", "Preprimaria", "Primaria"),
      "At most primary school",
      "Secondary school or above" # Básico, Diversificado, Superior, Maestría, Doctorado
    ),
    education = as.factor(education),

    employed = ifelse(employed == "Población ocupada", 1L, 0L)
  )

## ============== Generate the pseudopanel (for each strata) ===============================

## build_cohorts(): collapses already-filtered individual data into
## education x age cohort means. Replaces the ten near-identical blocks.
##   data        - a subset of dat_pet
##   sample_name - label written into the `sample` column

## Build every sub-sample. Each list entry is one stratum of the pseudopanel.
cohort_tables <- list(
  general = build_cohorts(dat_pet, "general", year),
  males = build_cohorts(filter(dat_pet, gender == "Male"), "males", year),
  females = build_cohorts(filter(dat_pet, gender == "Female"), "females", year),
  urban = build_cohorts(filter(dat_pet, area == "Urban"), "urban", year),
  rural = build_cohorts(filter(dat_pet, area == "Rural"), "rural", year),
  indigenous = build_cohorts(filter(dat_pet, ethnicity == "Indigenous"),"indigenous", year),
  `non-indigenous` = build_cohorts(filter(dat_pet, ethnicity == "Non-indigenous"),"non-indigenous",year),
  p99 = build_cohorts(filter(dat_pet, quantile %in% c("p50", "p75", "p90", "p99")),"p99",year), 
  p90 = build_cohorts(filter(dat_pet, quantile %in% c("p50", "p75", "p90")),"p90",year),
  p50 = build_cohorts(filter(dat_pet, quantile %in% c("p50")), "p50",year)
)

## ============== Small-cell report (log style) ===============================

small_cell_report <- report_small_cells(cohort_tables)

## ============== Append and write output ===============================

dat_clean <- dplyr::bind_rows(cohort_tables)

write_csv(dat_clean, file.path(PROCESSED_DIR, "income_2016.csv"))
log_msg("Wrote ",nrow(dat_clean)," cohort rows to ",file.path(PROCESSED_DIR, "income_2016.csv"))