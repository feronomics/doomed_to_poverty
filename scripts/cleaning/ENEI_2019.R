## Poverty trap in Guatemala?
## PIs:  Fernando Sáenz
##       Javier Velásquez
##       Alejandro Milián
##
## Last modification: 22 June 2026
##
## source: https://www.ine.gob.gt/encuesta-nacional-de-empleo-e-ingresos/
## Inputs:    data/raw/ENEI_2019.sav
## Outputs:   data/processed/income_2019.csv
##
## Purpose: Cleaning

source("resources/pipeline_config.R")

cfg  <- read_yaml("resources/config_2019.yaml")
enei <- read.spss("data/raw/ENEI_2019.sav", to.data.frame = TRUE)
year <- 2019

log_msg("Loaded ENEI_2019",".sav: ",nrow(enei)," rows, ",ncol(enei)," columns")

income_vars <- c(cfg$labor_vars, cfg$add_labor_vars, cfg$no_labor_vars, cfg$remittance_vars)

dat <- enei %>%
  select(all_of(c(cfg$id_vars, cfg$demographic_vars, income_vars))) %>%
  mutate(across(all_of(income_vars), as.numeric)) %>%
  mutate(across(all_of(cfg$div_12), ~ .x / 12),
         across(all_of(cfg$div_3),  ~ .x / 3))

dat_renamed <- dat %>%
  rename(area = DOMINIO, weights = FACTOR, gender = PPA02, age = PPA03,
         ethnicity = PPA06, education = P03A05A, employed = OCUPADOS) %>%
  mutate(
    labor_income     = rowSums(across(all_of(cfg$labor_vars)),     na.rm = TRUE),
    add_labor_income = rowSums(across(all_of(cfg$add_labor_vars)), na.rm = TRUE),
    no_labor_income  = rowSums(across(all_of(cfg$no_labor_vars)),  na.rm = TRUE),
    remittances      = ifelse(rowSums(across(all_of(cfg$remittance_vars)), na.rm = TRUE) <= 0, 0,
                              rowMeans(across(all_of(cfg$remittance_vars)), na.rm = TRUE))
  ) %>%
  select(-all_of(income_vars))

## =================== Calculating Income Quantile ================================

## Calculate the household income (to later test different strata within the income dist.)
dat_hhincome <- dat_renamed %>%
  dplyr::group_by(NUM_HOGAR) %>%
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
dat_final <- left_join(dat_renamed, dat_hhincome, by = c("NUM_HOGAR"))

## ============== Recoding / Cleaning ===============================

## Filter the population in working age (and employed)
dat_pet <- dat_final %>%
  filter(age >= AGE_MIN & age <= AGE_MAX & tolower(employed) == "población ocupada")

log_msg(
  age_filter_label(),
  nrow(dat_pet),
  " individuals"
)

## Recode de variables de estrato
dat_pet <- dat_pet %>%
  mutate(
    area      = ifelse(tolower(area) == "rural nacional", "Rural", "Urban"),
    ethnicity = ifelse(tolower(ethnicity) %in% c("ladino", "ladina", "extranjero", "extranjera"),
                       "Non-indigenous", "Indigenous"),
    gender    = ifelse(tolower(gender) == "hombre", "Male", "Female"),
    education = ifelse(tolower(education) %in% c("ninguno", "preprimaria", "primaria"),
                       "At most primary school", "Secondary school or above"),
    employed  = ifelse(tolower(employed) == "población ocupada", 1L, 0L)
  )

## ============== Generate the pseudopanel (for each strata) ===============================

## Build every sub-sample. Each list entry is one stratum of the pseudopanel.
cohort_tables <- list(
  general = build_cohorts(dat_pet, "general",year),
  males = build_cohorts(filter(dat_pet, gender == "Male"), "males",year),
  females = build_cohorts(filter(dat_pet, gender == "Female"), "females",year),
  urban = build_cohorts(filter(dat_pet, area == "Urban"), "urban",year),
  rural = build_cohorts(filter(dat_pet, area == "Rural"), "rural",year),
  indigenous = build_cohorts(filter(dat_pet, ethnicity == "Indigenous"),"indigenous",year),
  `non-indigenous` = build_cohorts(filter(dat_pet, ethnicity == "Non-indigenous"),"non-indigenous",year),
  p99 = build_cohorts(filter(dat_pet, quantile %in% c("p50", "p75", "p90", "p99")),"p99",year),
  p90 = build_cohorts(filter(dat_pet, quantile %in% c("p50", "p75", "p90")),"p90",year),
  p50 = build_cohorts(filter(dat_pet, quantile %in% c("p50")), "p50", year)
)

## ============== Small-cell report ===============================
small_cell_report <- report_small_cells(cohort_tables)

## ============== Append and write output ===============================

dat_clean <- dplyr::bind_rows(cohort_tables)

write_csv(dat_clean, file.path(PROCESSED_DIR, "income_2019.csv"))
log_msg("Wrote ", nrow(dat_clean), " cohort rows to ", file.path(PROCESSED_DIR, "income_2019.csv"))