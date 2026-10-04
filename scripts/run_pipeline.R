## Poverty trap in Guatemala?
## PIs:  Fernando Sáenz
##       Javier Velásquez
##       Alejandro Milián
##
## Purpose: single-command entry point that runs the full pipeline
## (cleaning -> merging -> analysis) for ONE age-cutoff specification.
##
## Run it from the project root with:
##
##   Rscript scripts/run_pipeline.R
##
## With no environment variables set, this reproduces the MAIN
## published results (working age = 14-60) and writes into
## data/processed/main/ and output/main/.
##
## To run it under a different cutoff manually (rarely needed -- see
## scripts/robustness/run_age_robustness.R for the batteries of
## checks the study reports), set AGE_MIN / AGE_MAX / RUN_ID before
## calling Rscript, e.g. from a shell:
##
##   AGE_MIN=13 AGE_MAX=59 RUN_ID=robustness/age_cutoffs/age_v1_13-59 \
##     Rscript scripts/run_pipeline.R
##
## Each spec should be run in its OWN Rscript process (as above, or via
## the robustness driver, which does this for you) so that leftover
## variables from one cutoff never leak into the next.

source("resources/pipeline_config.R")
log_msg_start <- sprintf("Running pipeline | AGE_MIN=%s AGE_MAX=%s RUN_ID=%s",
                          AGE_MIN, AGE_MAX, RUN_ID)
cat(log_msg_start, "\n")

## ---- 1. Cleaning: builds data/processed/<RUN_ID>/income_YYYY.csv ---------
source("scripts/cleaning/clean_master.R")
source("scripts/analysis/export_small_cell_reports.R")

## ---- 2. Merging: builds the three panel files used by the analyses ---------
source("scripts/merging/merging_2016-2019.R")
source("scripts/merging/merging_2021-2024.R")
source("scripts/merging/merging_all.R")

## ---- 3. Analysis: builds Figures 1-5 for each period ----------------------
source("scripts/analysis/01 Analysis 2016-2019.R")
source("scripts/analysis/01 Analysis 2021-2024.R")
source("scripts/analysis/01 Analysis_General_Sample.R")

## ---- 4. Descriptive statistics for all three analysis windows --------
source("scripts/analysis/02 Descriptive Statistics 2016-2019.R")
source("scripts/analysis/02 Descriptive Statistics 2021-2024.R")
source("scripts/analysis/02 Descriptive Statistics 2016-2024.R")

source("scripts/analysis/export_latex_tables.R")
cat(sprintf("Done. Processed data in %s | Figures and LaTeX tables in %s\n", PROCESSED_DIR, OUTPUT_DIR))
