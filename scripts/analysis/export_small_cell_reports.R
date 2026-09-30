# Can run independently from already-cleaned data, without rerunning LOESS.
source("resources/pipeline_config.R")
source("resources/small_cell_reports.R")
export_small_cell_reports(PROCESSED_DIR, OUTPUT_DIR)
