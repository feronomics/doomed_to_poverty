# Run from the project root after the main pipeline.
# Same population weights, sample and grid in every variant; only span varies.
library(tidyverse)
source("resources/functions.R")
source("resources/theme_fer.R")

comparison_dir <- "output/robustness/bandwidth"
dir.create(comparison_dir, recursive = TRUE, showWarnings = FALSE)
summaries <- list()
for (period in c("2016-2019", "2021-2024")) {
  path <- file.path("data/processed/main", paste0("data_", period, ".csv"))
  if (!file.exists(path)) stop("Run scripts/run_pipeline.R first: missing ", path)
  df <- read_csv(path, show_col_types = FALSE) |> filter(sample == "general")
  model <- fit_capacity(y1_2 ~ y1_1, df, span_label = paste(period, "bandwidth baseline"))
  deltas <- c(-0.05, -0.01, 0, 0.01, 0.05)
  spans <- model$span + deltas
  if (any(spans <= 0)) stop("Bandwidth perturbations must remain positive.")
  labels <- sprintf("%+.2f (span %.3f)", deltas, spans)
  labels[deltas == 0] <- sprintf("Selected (%.3f)", model$span)
  x_grid <- seq(0, 7000, length.out = 100)
  reference <- c(500, 1500, 3000, 5000)
  curves <- list()
  for (i in seq_along(spans)) {
    fit <- fit_capacity(y1_2 ~ y1_1, df, span = spans[i])
    curves[[i]] <- tibble(x = x_grid,
      y = predict_capacity(fit$fit, data.frame(y1_1 = x_grid)), label = labels[i])
    summaries[[length(summaries) + 1L]] <- tibble(period = period,
      delta = deltas[i], span = spans[i], selected_span = model$span,
      n_cohorts = length(unique(fit$data$.cohort)), n_pairs = nrow(fit$data),
      y_t = reference,
      predicted_y_t1 = predict_capacity(fit$fit, data.frame(y1_1 = reference)))
  }
  plot_data <- bind_rows(curves) |> mutate(label = factor(label, levels = labels))
  p <- ggplot(plot_data, aes(x, y, color = label, linetype = label)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    scale_x_quetzal() + scale_y_quetzal() + coord_fixed(ratio = 1) + theme_apa() +
    labs(x = "Income t", y = "Income t+1", color = "Bandwidth", linetype = "Bandwidth",
      title = paste("Capacity Curve Robustness to Bandwidth:", period),
      subtitle = "Population-weighted; grouped-CV selected span +/- 0.01 and 0.05")
  ggsave(file.path(comparison_dir, paste0("comparison_capacity_curve_", period, ".png")),
         plot = p, width = 8, height = 8, dpi = 300)
  write_csv(plot_data, file.path(comparison_dir, paste0("curve_predictions_", period, ".csv")))
}
write_csv(bind_rows(summaries), file.path(comparison_dir, "comparison_summary.csv"))
dump_span_log(file.path(comparison_dir, "bandwidth_selection_summary.csv"))
source("resources/latex_tables.R")
export_robustness_tex(read.csv(file.path(comparison_dir, "comparison_summary.csv")), comparison_dir, "bandwidth")
export_saved_bandwidth_tex(comparison_dir)
write_latex_index(comparison_dir)
