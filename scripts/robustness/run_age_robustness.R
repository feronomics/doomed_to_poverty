## Poverty trap in Guatemala?
## PIs:  Fernando Sáenz
##       Javier Velásquez
##       Alejandro Milián
##
## Purpose: ROBUSTNESS CHECK -- re-run the full pipeline under several
## working-age cutoffs to show the capacity-curve results are not an
## artifact of the 14-60 cutoff used in the main specification.
##
## Run it from the project root with:
##
##   Rscript scripts/robustness/run_age_robustness.R
##
## Outputs:
##   output/robustness/age_cutoffs/comparison_capacity_curve_2016-2019.png
##   output/robustness/age_cutoffs/comparison_capacity_curve_2021-2024.png
##   output/robustness/age_cutoffs/comparison_summary.csv

library(tidyverse)

## ============== 1. Define the age-cutoff specs to test ====================
## `main` (14-60) is included so it appears in the comparison plot as the
## reference line; it is not re-run if it already exists, since it's also
## produced by the normal scripts/run_pipeline.R.
##
## Add more rows here for additional checks (e.g. 12-58, 16-62, ...).

specs <- tribble(
  ~run_id,                                ~age_min, ~age_max, ~label,
  "main",                                 14,       60,       "Main (14-60)",
  "robustness/age_cutoffs/age_v1_13-59",  13,       59,       "v1 (13-59)",
  "robustness/age_cutoffs/age_v2_15-61",  15,       61,       "v2 (15-61)",
  "robustness/age_cutoffs/age_v3_12-58",  12,       58,       "v3 (12-58)",
  "robustness/age_cutoffs/age_v4_16-62",  16,       62,       "v4 (16-62)"
)

comparison_dir <- "output/robustness/age_cutoffs"
dir.create(comparison_dir, recursive = TRUE, showWarnings = FALSE)

## ============== 2. Run (or skip if already run) each spec ==================
## Each spec sets its env vars, then runs the pipeline in THIS session via
## source(). pipeline_config.R re-reads AGE_MIN / AGE_MAX / RUN_ID from the
## environment every time the pipeline is sourced, so each spec picks up its
## own cutoff. Env vars are unset after each spec so nothing leaks forward.

for (i in seq_len(nrow(specs))) {

  spec <- specs[i, ]
  processed_path <- file.path("data/processed", spec$run_id, "data_2016-2019.csv")

  if (file.exists(processed_path)) {
    cat(sprintf("[skip] %s already exists, not re-running\n", spec$run_id))
    next
  }

  cat(sprintf("[run]  %s | age %s-%s\n", spec$run_id, spec$age_min, spec$age_max))

  Sys.setenv(AGE_MIN = spec$age_min, AGE_MAX = spec$age_max, RUN_ID = spec$run_id)

  err <- tryCatch(
    {
      source("scripts/run_pipeline.R")
      NULL
    },
    error = function(e) conditionMessage(e)
  )

  Sys.unsetenv(c("AGE_MIN", "AGE_MAX", "RUN_ID"))

  if (!is.null(err)) {
    warning(sprintf(
      "Spec '%s' failed -- %s", spec$run_id, err
    ))
  }
}

## ============== 3. Load the "general" sample from every spec ==============

load_general <- function(run_id, period_file) {
  path <- file.path("data/processed", run_id, period_file)
  if (!file.exists(path)) {
    warning(sprintf("Missing %s -- spec was not produced, skipping", path))
    return(NULL)
  }
  read_csv(path, show_col_types = FALSE) |>
    filter(sample == "general") |>
    mutate(run_id = run_id)
}

data_1619 <- bind_rows(lapply(specs$run_id, load_general, period_file = "data_2016-2019.csv")) |>
  left_join(specs, by = "run_id")

data_2124 <- bind_rows(lapply(specs$run_id, load_general, period_file = "data_2021-2024.csv")) |>
  left_join(specs, by = "run_id")

if (n_distinct(data_1619$run_id) < nrow(specs)) {
  warning(
    "Only ", n_distinct(data_1619$run_id), " of ", nrow(specs),
    " specs produced data for 2016-2019 -- check the [run] logs above for errors."
  )
}

## ============== 4. Overlay capacity curves across cutoffs ==================

source("resources/theme_fer.R")

x_grid <- seq(0, 7000, length.out = 100)

fit_spec <- function(df, label) {
  fit <- loess(y1_2 ~ y1_1, data = df)
  tibble(x = x_grid, y = predict(fit, newdata = tibble(y1_1 = x_grid)), label = label)
}

plot_capacity_by_cutoff <- function(data, period_title, filename) {
  plot_data <- data |>
    group_split(run_id) |>
    lapply(function(df) fit_spec(df, unique(df$label))) |>
    bind_rows()

  ggplot(plot_data, aes(x = x, y = y, color = label, linetype = label)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
    geom_line(linewidth = 0.9) +
    scale_y_quetzal() +
    scale_x_quetzal() +
    coord_fixed(ratio = 1) +
    theme_apa() +
    labs(
      x = "Income t", y = "Income t+1", color = "Age cutoff", linetype = "Age cutoff",
      title = paste0("Capacity Curve Robustness to Age Cutoff: ", period_title),
      subtitle = "General sample, labor income (y1)"
    )

  ggsave(file.path(comparison_dir, filename), width = 8, height = 8, dpi = 300)
}

plot_capacity_by_cutoff(data_1619, "2016-2019", "comparison_capacity_curve_2016-2019.png")
plot_capacity_by_cutoff(data_2124, "2021-2024", "comparison_capacity_curve_2021-2024.png")

## ============== 5. Numeric summary: predicted y_{t+1} at reference y_t ====

reference_points <- c(500, 1500, 3000, 5000)

summarize_spec <- function(df, label, period) {
  fit <- loess(y1_2 ~ y1_1, data = df)
  tibble(
    period         = period,
    age_cutoff     = label,
    n_cohorts      = nrow(df),
    y_t            = reference_points,
    predicted_y_t1 = predict(fit, newdata = tibble(y1_1 = reference_points))
  )
}

summary_1619 <- data_1619 |>
  group_split(run_id) |>
  lapply(function(df) summarize_spec(df, unique(df$label), "2016-2019")) |>
  bind_rows()

summary_2124 <- data_2124 |>
  group_split(run_id) |>
  lapply(function(df) summarize_spec(df, unique(df$label), "2021-2024")) |>
  bind_rows()

summary_table <- bind_rows(summary_1619, summary_2124) |>
  arrange(period, y_t, age_cutoff)

write_csv(summary_table, file.path(comparison_dir, "comparison_summary.csv"))

cat("\nRobustness check complete.\n")
cat("Figures:  ", file.path(comparison_dir, "comparison_capacity_curve_2016-2019.png"), "\n")
cat("          ", file.path(comparison_dir, "comparison_capacity_curve_2021-2024.png"), "\n")
cat("Table:    ", file.path(comparison_dir, "comparison_summary.csv"), "\n")