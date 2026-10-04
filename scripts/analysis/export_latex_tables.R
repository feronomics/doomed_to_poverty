# Run from the repository root. Existing Excel/CSV outputs are preserved.
# Uses saved data/results; does not rerun regressions or bootstrap intervals.
local({
  source("resources/pipeline_config.R", local = TRUE)
  source("resources/latex_tables.R", local = TRUE)
  source("resources/small_cell_reports.R", local = TRUE)
  export_small_cells_tex(collect_small_cell_reports(PROCESSED_DIR), OUTPUT_DIR)
  for (year in c(2016:2019, 2021:2024)) {
    annual <- read.csv(file.path(PROCESSED_DIR, paste0("income_", year, ".csv")))
    export_annual_descriptive_tex(annual, year, OUTPUT_DIR)
  }
  panels <- lapply(c("2016-2019", "2021-2024"), function(period)
    read.csv(file.path(PROCESSED_DIR, paste0("data_", period, ".csv"))))
  # Reconstruct the combined panel in memory to avoid using an older pooled file.
  panels[[1]]$age_2021 <- panels[[1]]$age_2019 + 2
  panels[[1]]$age_2019 <- NULL
  pooled <- rbind(panels[[1]][intersect(names(panels[[1]]), names(panels[[2]]))],
                  panels[[2]][intersect(names(panels[[1]]), names(panels[[2]]))])
  stopifnot(setequal(unique(pooled$comparison), c("2016-2017", "2017-2018", "2018-2019", "2021-2022", "2022-2023", "2023-2024")))
  for (period in c("2016-2019", "2021-2024", "2016-2024")) {
    panel <- switch(period, `2016-2019` = panels[[1]], `2021-2024` = panels[[2]], `2016-2024` = pooled)
    export_descriptive_tex(panel, period, OUTPUT_DIR)
    export_saved_bandwidth_tex(file.path(OUTPUT_DIR, period), OUTPUT_DIR, period)
  }
  write_latex_index(OUTPUT_DIR)
  for (type in c("age", "bandwidth")) {
    directory <- file.path("output/robustness", if (type == "age") "age_cutoffs" else "bandwidth")
    path <- file.path(directory, "comparison_summary.csv")
    if (file.exists(path)) {
      export_robustness_tex(read.csv(path), directory, type)
      export_saved_bandwidth_tex(directory)
      write_latex_index(directory)
    }
  }
  cat("LaTeX tables exported to", file.path(OUTPUT_DIR, "latex"), "and available robustness folders.\n")
})
