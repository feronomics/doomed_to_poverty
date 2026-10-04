# Descriptive statistics of cohort-transition rows (unweighted, as before).
# N counts transitions, not individuals or independent cohorts.
## Poverty trap in Guatemala? — Descriptive Statistics (Table 1)
## PIs:  Fernando Sáenz, Javier Velásquez, Alejandro Milián
## Fecha: 05 Jul 2026
##
## Inputs: data/processed/<RUN_ID>/data_2021-2024.csv
## Outputs: output/<RUN_ID>/2021-2024/table1_panelA.csv
##          output/<RUN_ID>/2021-2024/table1_panelB.csv
##
## Estructura:
##   Panel A: distribución completa de cada definición de ingreso (muestra general)
##   Panel B: media (DE) y N por submuestra, para comparar grupos
##
## NOTA: Solo uso las columnas que conozco de tu base (y1_1...y4_2, sample).
## Si data.csv tiene demográficos (edad, escolaridad, tamaño del hogar, etc.),
## agrégalos al vector `vars` del Panel A — el código no cambia.

library(tidyverse)

source("resources/pipeline_config.R")
table_dir <- file.path(OUTPUT_DIR, "2021-2024")
data <- read_csv(file.path(PROCESSED_DIR, "data_2021-2024.csv"))

## ============ Configuración ========================================

# Variables a describir y sus etiquetas para publicación
etiquetas <- c(
  y1_1 = "Labor income (t)",
  y1_2 = "Labor income (t+1)",
  y2_1 = "Incl. in-kind (t)",
  y2_2 = "Incl. in-kind (t+1)",
  y3_1 = "Incl. non-labor (t)",
  y3_2 = "Incl. non-labor (t+1)",
  y4_1 = "Incl. remittances (t)",
  y4_2 = "Incl. remittances (t+1)"
)
vars <- names(etiquetas)

# Submuestras a comparar en el Panel B (deben existir en la columna `sample`)
submuestras <- c("general", "males", "females", "urban", "rural",
                 "indigenous", "non-indigenous")

## ============ Panel A: distribución completa (muestra general) =====

panel_a <- data |>
  filter(sample == "general") |>
  select(all_of(vars)) |>
  pivot_longer(everything(), names_to = "var", values_to = "value") |>
  group_by(var) |>
  summarise(
    N       = sum(!is.na(value)),
    Mean    = mean(value, na.rm = TRUE),
    SD      = sd(value, na.rm = TRUE),
    Min     = min(value, na.rm = TRUE),
    P25     = quantile(value, 0.25, na.rm = TRUE),
    Median  = median(value, na.rm = TRUE),
    P75     = quantile(value, 0.75, na.rm = TRUE),
    Max     = max(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Variable = etiquetas[var],
    across(Mean:Max, ~ round(.x, 1))
  ) |>
  # Preservar el orden de `etiquetas`, no el alfabético
  arrange(match(var, vars)) |>
  select(Variable, N, Mean, SD, Min, P25, Median, P75, Max)

print(panel_a, n = Inf)
write_csv(panel_a, file.path(table_dir, "table1_panelA.csv"))

## ============ Panel B: media (DE) por submuestra ====================
## Convención estándar en economía: media con la DE entre paréntesis
## debajo (aquí en la misma celda), y N por grupo en la última fila.

panel_b <- data |>
  filter(sample %in% submuestras) |>
  select(sample, all_of(vars)) |>
  pivot_longer(-sample, names_to = "var", values_to = "value") |>
  group_by(sample, var) |>
  summarise(
    media = mean(value, na.rm = TRUE),
    de    = sd(value, na.rm = TRUE),
    n     = sum(!is.na(value)),
    .groups = "drop"
  ) |>
  mutate(celda = sprintf("%.1f (%.1f)", media, de)) |>
  select(sample, var, celda) |>
  pivot_wider(names_from = sample, values_from = celda) |>
  mutate(Variable = etiquetas[var]) |>
  arrange(match(var, vars)) |>
  select(Variable, any_of(submuestras))

# Fila de N por submuestra
fila_n <- data |>
  filter(sample %in% submuestras) |>
  count(sample) |>
  mutate(n = as.character(n)) |>   # el Panel B es texto: "media (DE)"
  pivot_wider(names_from = sample, values_from = n) |>
  mutate(Variable = "Cohort transitions (N)") |>
  select(Variable, any_of(submuestras))

panel_b <- bind_rows(panel_b, fila_n)

print(panel_b, n = Inf)
write_csv(panel_b, file.path(table_dir, "table1_panelB.csv"))

cat("\nRECORDATORIO: completar las notas al pie de la tabla (ver final del script).\n")