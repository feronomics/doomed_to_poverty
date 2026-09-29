# Optional integration check against existing processed CSVs (no file writes).
# Rscript tests/check_processed_data.R [path/to/data/processed/main]
source("resources/functions.R")
args <- commandArgs(trailingOnly = TRUE)
data_dir <- if (length(args)) args[1] else "data/processed/main"
for (period in c("2016-2019", "2021-2024", "2016-2024")) {
  path <- file.path(data_dir, paste0("data_", period, ".csv"))
  if (!file.exists(path)) next
  data <- read.csv(path)
  for (sample_name in unique(data$sample)) {
    df <- data[data$sample == sample_name, , drop = FALSE]
    definitions <- if (sample_name == "general") 1:4 else 1L
    for (j in definitions) {
      form <- as.formula(paste0("y", j, "_2 ~ y", j, "_1"))
      model <- fit_capacity(form, df, span_label = paste(period, sample_name, j))
      x <- df[[paste0("y", j, "_1")]]
      grid <- setNames(data.frame(as.numeric(quantile(x, c(.25, .5, .75)))), paste0("y", j, "_1"))
      ci <- loess_ci(form, df, grid, span = model$span, B = 30)
      stopifnot(all(is.finite(ci$fit)), all(is.finite(ci$lwr)),
                all(ci$lwr <= ci$upr), all(ci$span == model$span))
    }
  }
}
cat("Processed-data integration checks passed.\n")
