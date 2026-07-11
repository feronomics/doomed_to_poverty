## Poverty trap in Guatemala?
## PIs:  Fernando Sáenz
##      Javier Velásquez
##      Alejandro Milián
## 
## Date of creation:  15 May 2026
## Last modification: 15 May 2026
## 
## Inputs:    data.csv
## Outputs:   tables and figures  
##
## Purpose: Analysis


## ================ Load packages ===============================

library(tidyverse)


## =============== Load graphic resources and functions ======================

source("resources/pipeline_config.R")
source("resources/theme_fer.R")

OUTPUT_DIR_FIGURES <- file.path(OUTPUT_DIR, "Figures")
dir.create(OUTPUT_DIR_FIGURES, recursive = TRUE, showWarnings = FALSE)

## =============== Load Data ===================================

data <- read_csv(file.path(PROCESSED_DIR, "data_2016-2024.csv"))

## =============== Sample sizes  ====================================

## find a way to summarize this info. Groups of ten? just showing the number below 30?

## ============== General Sample ==================================
data_general <- filter(data, sample == "general")

# Suavizado LOESS

# Weights, factor de expansión.

loess_fit_ylab <- loess(y1_2 ~ y1_1, data_general)
loess_fit_yad <- loess(y2_2 ~ y2_1, data_general)
loess_fit_ynonlab <- loess(y3_2 ~ y3_1, data_general)
loess_fit_rem <- loess(y4_2 ~ y4_1, data_general)

# Predictions to plot
grid <- data.frame(y1_1 = seq(0, 7000, length.out = 100),
                   y2_1 = seq(0, 7000, length.out = 100),
                   y3_1 = seq(0, 7000, length.out = 100), 
                   y4_1 = seq(0, 7000, length.out = 100))

grid$fit_ylab <- predict(loess_fit_ylab, newdata = grid)
grid$fit_yad <- predict(loess_fit_yad, newdata = grid)
grid$fit_ynonlab <- predict(loess_fit_ynonlab, newdata = grid)
grid$fit_rem <- predict(loess_fit_rem, newdata = grid)

# Graph:  
ggplot(data = grid, aes(x = y1_1)) +
  geom_line(aes(y = fit_ylab,    linetype = "Labor income"),        linewidth = 0.9) +
  geom_line(aes(y = fit_yad,     linetype = "Including in-kind"),   linewidth = 0.9) +
  geom_line(aes(y = fit_ynonlab, linetype = "Including non-labor"), linewidth = 0.9) +
  geom_line(aes(y = fit_rem,     linetype = "Including remittances"), linewidth = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  scale_linetype_manual(
    name   = "Income definition",
    values = c("Labor income"          = "solid",
               "Including in-kind"     = "dashed",
               "Including non-labor"   = "dotted",
               "Including remittances" = "dotdash")
  ) +
  scale_y_quetzal() +
  scale_x_quetzal()+
  theme_apa()+
  labs(x = "Income t",
       y = "Income t+1",
       title = "Capacity Curve (General Sample)")

ggsave(file.path(OUTPUT_DIR_FIGURES, "Figure1.png"), width = 10, height = 7, dpi = 300)

## ============== Reusable plot function ==========================


## ============== Plots ==========================================

# Males vs Females
plot_capacity_curve(
  samples = c("males", "females"),
  labels  = c("Males", "Females"),
  y_var   = "y1",
  title   = "Capacity Curve: Males vs. Females"
)

ggsave(file.path(OUTPUT_DIR_FIGURES, "Figure2.png"), width = 10, height = 7, dpi = 300)

# Urban vs Rural
plot_capacity_curve(
  samples = c("urban", "rural"),
  labels  = c("Urban", "Rural"),
  y_var   = "y1",
  title   = "Capacity Curve: Urban vs. Rural"
)

ggsave(file.path(OUTPUT_DIR_FIGURES, "Figure3.png"), width = 10, height = 7, dpi = 300)

# Indigenous vs Non-indigenous
plot_capacity_curve(
  samples = c("indigenous", "non-indigenous"),
  labels  = c("Indigenous", "Non-indigenous"),
  y_var   = "y1",
  title   = "Capacity Curve: Indigenous vs. Non-indigenous"
)

ggsave(file.path(OUTPUT_DIR_FIGURES, "Figure4.png"), width = 10, height = 7, dpi = 300)

# Percentiles
plot_capacity_curve(
  samples = c("p99", "p90", "p50"),
  labels  = c("Bottom 99", "Bottom 90", "Bottom 50"),
  y_var   = "y1",
  title   = "Capacity Curve: By Percentile"
)

ggsave(file.path(OUTPUT_DIR_FIGURES, "Figure5.png"), width = 10, height = 7, dpi = 300)

