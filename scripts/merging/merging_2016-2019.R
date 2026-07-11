## Poverty trap in Guatemala?
## PIs:  Fernando Sáenz
##      Javier Velásquez
##      Alejandro Milián
##
## Last modification: 15 May 2026
##
## Inputs:    Income 2016 - Income 2019
## Outputs:   data.csv
##
## Purpose: Merging

source("resources/pipeline_config.R")

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
data <- data |>
  rename(
    y_lab = avg_labor_income,
    y_ad = avg_add_labor_income,
    y_nonlab = avg_no_labor_income,
    rem = avg_remittances
  ) |>
  select(-n)

## Creating 

data <- data |>
  mutate(
    y1 = y_lab,
    y2 = rowSums(across(c(y_lab, y_ad)), na.rm = T),
    y3 = rowSums(across(c(y_lab, y_ad, y_nonlab)), na.rm = T),
    y4 = rowSums(across(c(y_lab, y_ad, y_nonlab, rem)), na.rm = T)
  )

## =================== Creating pairs ==================================

make_pair <- function(yr_t, yr_t1) {
  left <- data |>
    filter(year == yr_t) |>
    select(age_2019, education, sample, weights_total, y1, y2, y3, y4) |>
    rename_with(~ paste0(., "_1"), c(weights_total, y1, y2, y3, y4))

  right <- data |>
    filter(year == yr_t1) |>
    select(age_2019, education, sample, weights_total, y1, y2, y3, y4) |>
    rename_with(~ paste0(., "_2"), c(weights_total, y1, y2, y3, y4))

  inner_join(left, right, by = c("age_2019", "education", "sample")) |>
    mutate(comparison = paste0(yr_t, "-", yr_t1))
}

years_available <- sort(unique(data$year))
year_pairs <- data.frame(
  year_t = head(years_available, -1),
  year_t1 = tail(years_available, -1)
)

data_wide <- bind_rows(mapply(
  make_pair,
  year_pairs$year_t,
  year_pairs$year_t1,
  SIMPLIFY = FALSE
))

data_wide_completed <- data_wide |>
  filter(if_all(y1_1:y4_2, ~ !is.na(.))) |>
  select(
    age_2019,
    education,
    sample,
    comparison,
    weights_total_1,
    weights_total_2,
    y1_1,
    y1_2,
    y2_1,
    y2_2,
    y3_1,
    y3_2,
    y4_1,
    y4_2
  )

## ============== Export  =======================================
write_csv(data_wide_completed, file.path(PROCESSED_DIR, "data_2016-2019.csv"))
