# Shared counts for the console report and Excel export.
summarize_cohort_cells <- function(cohort_tables, year) {
  if (length(year) != 1L || is.na(year)) stop("Supply one survey year.")
  rows <- lapply(names(cohort_tables), function(nm) {
    tbl <- cohort_tables[[nm]]
    if (!all(c("n", "se_y1") %in% names(tbl)))
      stop("Missing n or se_y1 for ", year, " / ", nm, "; rerun cleaning.")
    if (any(!is.finite(tbl$n) | tbl$n < 0 | tbl$n != floor(tbl$n)))
      stop("Invalid respondent counts for ", year, " / ", nm)
    total <- nrow(tbl)
    data.frame(year = as.integer(year), sample = nm, n_cohorts = total,
      `cells<30` = sum(tbl$n < 30), `cells<20` = sum(tbl$n < 20),
      `cells<10` = sum(tbl$n < 10), se_NA = sum(is.na(tbl$se_y1)),
      check.names = FALSE)
  })
  do.call(rbind, rows)
}

collect_small_cell_reports <- function(processed_dir,
    years = c(2016:2019, 2021:2024)) {
  reports <- lapply(years, function(year) {
    path <- file.path(processed_dir, paste0("income_", year, ".csv"))
    if (!file.exists(path)) stop("Missing cleaned data: ", path)
    df <- read.csv(path, check.names = FALSE)
    if (!all(c("sample", "year", "age", "education", "n", "se_y1") %in% names(df)))
      stop("Incomplete cohort columns in ", path, "; rerun cleaning.")
    if (anyNA(df$year) || any(df$year != year)) stop("Unexpected survey year in ", path)
    if (anyNA(df$sample) || anyDuplicated(df[c("sample", "education", "age")]))
      stop("Missing sample or duplicate cohort identifiers in ", path)
    expected <- c("general", "males", "females", "urban", "rural",
                  "indigenous", "non-indigenous", "p99", "p90", "p50")
    if (length(setdiff(unique(df$sample), expected))) stop("Unexpected subsample in ", path)
    tables <- setNames(lapply(expected, function(nm) df[df$sample == nm, , drop = FALSE]), expected)
    summarize_cohort_cells(tables, year)
  })
  do.call(rbind, reports)
}

export_small_cell_reports <- function(processed_dir, output_dir,
                                      years = c(2016:2019, 2021:2024)) {
  if (!requireNamespace("openxlsx", quietly = TRUE))
    stop('Excel export requires openxlsx. Run install.packages("openxlsx"), then rerun scripts/analysis/export_small_cell_reports.R.')
  report <- collect_small_cell_reports(processed_dir, years)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Small cells")
  openxlsx::writeData(wb, "Small cells", report, colNames = TRUE, rowNames = FALSE, headerStyle = NULL)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(output_dir, "small_cell_summary.xlsx")
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  cat("Small-cell workbook written to:", path, "\n")
  invisible(report)
}
