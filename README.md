# Doomed to Poverty? Empirical Evidence from Guatemala's Income Dynamics 2016-2025

The purpose of this package is to provide the reproduceble results of our investigation. This includes the all the scripts for cleaning, merging and provide outputs on the data


We keept a script per National Survey beacause of inconsistency the column names of the database
# Sources

All the data used on this research was from ENEI and ENCOVI. ENCOVI it's made every 5 years and ENEI it's like a tracker of whats happens in the mean time but only selection of special variables. 

# ENEI

The idea of ENEI it's to track the dynamic incomes, so we

## Weighted capacity curves and uncertainty

Run commands from the repository root:

```sh
Rscript scripts/run_pipeline.R
Rscript scripts/robustness/run_age_robustness.R
Rscript scripts/robustness/run_bandwidth_robustness.R
```

The main pipeline builds the 2016-2019 and 2021-2024 analyses. The combined
2016-2024 analysis is separate and requires its merged CSV. Both robustness
scripts write to their own folders under `output/robustness/`. The bandwidth
check requires the main processed data. The age driver reuses existing paired
CSVs only when both periods exist; rerun the pipeline to refresh main figures
after changing the estimation method.

All capacity-curve regressions retain the baseline population weight
`weights_total_1`. Survey weights also remain in the construction of cohort
income means. Cleaning and merging preserve `n`, `n_eff` and `se_y1` through
`se_y4` for diagnostics and future survey-design inference.

### Bandwidth selection

Each regression selects its LOESS span by minimizing population-weighted
held-out mean squared error. Ten folds (or fewer when there are fewer cohorts)
are assigned by education x reference age, keeping every transition of a
cohort in one fold. The same folds and observations score all candidates.
Candidates with any non-finite held-out prediction are rejected rather than
scored on a smaller subset. Seed 123 makes selection reproducible without
changing the caller's random-number state.

The initial grid is 0.15 to 1 in increments of 0.025. An upper-edge winner
extends the search in steps of 0.1 up to 3. Remaining boundary winners produce
a warning: the selected span is the best searched candidate, not proof of a
global optimum. This is grouped cross-validation.
Direct LOESS evaluation is used consistently. Validation includes held-out
extremes and records their count; displayed predictions are restricted to the
training income range. Inspect tail behavior and boundary warnings.

Each analysis exports `bandwidth_selection_summary.csv` and its corresponding
`bandwidth_selection_summary_grid.csv`, documenting selected spans and every
candidate's weighted validation MSE.

### Confidence intervals

The shared helper resamples entire cohorts with replacement, retaining all
their transitions and population weights, and refits 1,000 times by default.
It does not resample transitions independently or independently perturb the
same cohort-year mean in different transitions. Each replicate uses the
full-sample selected span. The 2.5th and 97.5th percentiles produce **pointwise
95% intervals conditional on that span and the observed cohort means**.

These are not simultaneous bands, do not include bandwidth-selection
uncertainty, and do not correct smoothing bias or measurement-error bias.
They also do not explicitly resample the underlying complex survey design or
propagate the stored within-cell standard errors. Adding that uncertainty
requires a coherent survey/cohort-year resampling design; the old independent
row-level noise has been removed. Cohort clustering does not address common
year shocks across different cohorts.

Predictions outside each bootstrap sample's income range are unavailable.
An interval is omitted when fewer than 80% of requested replicates provide a
finite prediction at that income. The helper returns per-income effective
replicate counts. Thus missing tail ribbons must not be interpreted as zero
uncertainty. `min_n` remains available for explicit small-cell sensitivity
checks; its default is zero (no size filter).

### Robustness outputs

Bandwidth robustness compares the selected span with exactly -0.05, -0.01,
0, +0.01 and +0.05, holding the data and weights fixed. It saves one overlay
per period, full curve predictions and predictions at Q500, Q1500, Q3000 and
Q5000. Spans above 1 are allowed.

Age robustness exports both independently retuned curves and an additional
`_fixed_bandwidth` plot using the main sample's selected span for every age
cutoff. The latter isolates changes in age eligibility from changes in
smoothing. Numeric summaries identify the mode and actual span. These
sensitivity overlays show fitted curves, not additional confidence bands.

### Validation

```sh
Rscript tests/test_loess_helpers.R
Rscript tests/check_processed_data.R
```

The first test uses synthetic data to check cohort grouping, weighted scoring,
complete-cohort resampling, support restrictions and reproducibility. The
second checks all existing period/subgroup processed datasets with 30
bootstrap replicates as an integration smoke test; production analyses use
1,000. Full pipeline/plotting also requires the project's existing R packages
(including tidyverse, foreign, haven, Hmisc and yaml).
