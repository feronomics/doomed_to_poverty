## Poverty trap in Guatemala?
## PIs:  Fernando Sáenz
##      Javier Velásquez
##      Alejandro Milián
##
## Date of creation:  15 May 2026
## Last modification: 05 Jul 2026
##
## Inputs:    data.csv
## Outputs:   tables and figures
##
## Purpose: Analysis
##
## Weighted LOESS: grouped-CV span and whole-cohort bootstrap intervals.
## ================ Load packages ===============================

library(tidyverse)

## =============== Load graphic resources ======================

source("resources/pipeline_config.R")
source("resources/theme_fer.R")
source("resources/functions.R")

## =============== Load Data ===================================

data <- read_csv(file.path(PROCESSED_DIR, "data_2021-2024.csv"))

## =============== Sample sizes  ====================================

## find a way to summarize this info. Groups of ten? just showing the number below 30?



## ============== General Sample ==================================

data_general <- filter(data, sample == "general")

# Prediction grid
grid <- data.frame(y1_1 = seq(0, 7000, length.out = 100),
                   y2_1 = seq(0, 7000, length.out = 100),
                   y3_1 = seq(0, 7000, length.out = 100),
                   y4_1 = seq(0, 7000, length.out = 100))

# LOESS + 95% CI for each income definition
ci_ylab    <- loess_ci(y1_2 ~ y1_1, data_general, grid, span_label = "2021-2024 General y1")
ci_yad     <- loess_ci(y2_2 ~ y2_1, data_general, grid, span_label = "2021-2024 General y2")
ci_ynonlab <- loess_ci(y3_2 ~ y3_1, data_general, grid, span_label = "2021-2024 General y3")
ci_rem     <- loess_ci(y4_2 ~ y4_1, data_general, grid, span_label = "2021-2024 General y4")

# Long format: one row per (x, income definition)
grid_long <- bind_rows(
  data.frame(x = grid$y1_1, ci_ylab,    def = "Labor income"),
  data.frame(x = grid$y2_1, ci_yad,     def = "Including in-kind"),
  data.frame(x = grid$y3_1, ci_ynonlab, def = "Including non-labor"),
  data.frame(x = grid$y4_1, ci_rem,     def = "Including remittances")
) |>
  mutate(def = factor(def, levels = c("Labor income",
                                      "Including in-kind",
                                      "Including non-labor",
                                      "Including remittances")))

# Graph:
ggplot(grid_long, aes(x = x)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, group = def),
              alpha = 0.15, linetype = 0) +
  geom_line(aes(y = fit, linetype = def), linewidth = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  scale_linetype_manual(
    name   = "Income definition",
    values = c("Labor income"          = "solid",
               "Including in-kind"     = "dashed",
               "Including non-labor"   = "dotted",
               "Including remittances" = "dotdash")
  ) +
  scale_y_quetzal() +
  scale_x_quetzal() +
  theme_apa() +
  labs(x = "Income t",
       y = "Income t+1",
       title = "Capacity Curve (General Sample) 2021-2024")

ggsave(file.path(OUTPUT_DIR_2124, "Figure1.png"), width = 7, height = 7, dpi = 300)

## ============== Plots ==========================================

# Males vs Females
plot_capacity_curve(
  samples = c("males", "females"),
  labels  = c("Males", "Females"),
  y_var   = "y1",
  title   = "Capacity Curve 2021-2024: Males vs. Females"
)

ggsave(file.path(OUTPUT_DIR_2124, "Figure2.png"), width = 7, height = 7, dpi = 300)

# Urban vs Rural
plot_capacity_curve(
  samples = c("urban", "rural"),
  labels  = c("Urban", "Rural"),
  y_var   = "y1",
  title   = "Capacity Curve 2021-2024: Urban vs. Rural"
)

ggsave(file.path(OUTPUT_DIR_2124, "Figure3.png"), width = 7, height = 7, dpi = 300)

# Indigenous vs Non-indigenous
plot_capacity_curve(
  samples = c("indigenous", "non-indigenous"),
  labels  = c("Indigenous", "Non-indigenous"),
  y_var   = "y1",
  title   = "Capacity Curve: Indigenous vs. Non-indigenous"
)

ggsave(file.path(OUTPUT_DIR_2124, "Figure4.png"), width = 7, height = 7, dpi = 300)

# Percentiles
plot_capacity_curve(
  samples = c("p99", "p90", "p50"),
  labels  = c("Bottom 99", "Bottom 90", "Bottom 50"),
  y_var   = "y1",
  title   = "Capacity Curve 2021-2024: By Percentile"
)

ggsave(file.path(OUTPUT_DIR_2124, "Figure5.png"), width = 7, height = 7, dpi = 300)
dump_span_log(file.path(OUTPUT_DIR_2124, "bandwidth_selection_summary.csv"))
