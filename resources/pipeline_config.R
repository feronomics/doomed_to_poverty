## Poverty trap in Guatemala?
## PIs:  Fernando Sáenz
##       Javier Velásquez
##       Alejandro Milián
##
## Purpose: single source of truth for (a) the working-age cutoff used
## in every cleaning script, and (b) where a given run's cleaned data
## and figures get written.
##
## Every cleaning / merging / analysis script sources this file. By
## default (no environment variables set) it reproduces the ORIGINAL
## published cutoff (14-60) and writes into the "main" results tree:
##
##   data/processed/main/...
##   output/main/...
##
## To reproduce the pipeline under a DIFFERENT age cutoff (e.g. for a
## robustness check), set AGE_MIN / AGE_MAX / RUN_ID *before* sourcing
## this file. You normally don't need to do this by hand -- run
## scripts/robustness/run_age_cutoff_robustness.R, which does it for
## you, once per spec, in its own R subprocess so specs never bleed
## into each other.
##
## RUN_ID controls the sub-folder under data/processed/ and output/.
## Use "main" for the baseline, or "robustness/age_cutoffs/<label>"
## for a robustness variant (see the robustness driver for the naming
## convention: age_v1_13-59, age_v2_15-61, ...).

AGE_MIN <- as.numeric(Sys.getenv("AGE_MIN", unset = "14"))
AGE_MAX <- as.numeric(Sys.getenv("AGE_MAX", unset = "60"))
RUN_ID  <- Sys.getenv("RUN_ID", unset = "main")

AGE_LABEL <- paste0(AGE_MIN, "-", AGE_MAX)

PROCESSED_DIR   <- file.path("data/processed", RUN_ID)
OUTPUT_DIR      <- file.path("output", RUN_ID)
OUTPUT_DIR_1619 <- file.path(OUTPUT_DIR, "2016-2019")
OUTPUT_DIR_2124 <- file.path(OUTPUT_DIR, "2021-2024")
OUTPUT_DIR_ALL  <- file.path(OUTPUT_DIR, "2016-2024")

for (d in c(PROCESSED_DIR, OUTPUT_DIR, OUTPUT_DIR_1619, OUTPUT_DIR_2124, OUTPUT_DIR_ALL)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

## Small helper so cleaning scripts can log the cutoff that's actually
## in effect for this run, instead of a hardcoded "(14-60)" string.
age_filter_label <- function() {
  paste0("Working-age employed population (", AGE_MIN, "-", AGE_MAX, "): ")
}
