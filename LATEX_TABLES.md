# Publication tables

Run from the repository root in R:

```r
source("scripts/analysis/export_latex_tables.R")
```

This exports saved results without rerunning bootstraps and preserves all existing Excel, CSV and figure outputs. The full pipeline calls it automatically. Both robustness drivers also export their tables automatically.

## Locations and organization

- `output/main/latex/annual_descriptive`: one year and income definition per table, all ten subsamples. Moments and quantiles are separate to keep the tables narrow.
- `output/main/latex/descriptive`: period-level transition-panel statistics for 2016–2019, 2021–2024 and the combined six-transition sample. Includes general-sample moments/quantiles and subsample comparisons for each income definition.
- `output/main/latex/small_cells`: one table per survey year, containing all ten subsamples and all reported thresholds.
- `output/main/latex/bandwidth`: selected spans, weighted cross-validation MSE and sample sizes, separated by period.
- `output/robustness/age_cutoffs/latex` and `output/robustness/bandwidth/latex`: predictions at reference incomes, sample sizes, and bandwidth-selection diagnostics.
- Each `latex/search_grids` folder contains the full candidate-span results, split into tables of at most 25 rows.

For another RUN_ID, main-run tables follow `output/<RUN_ID>/latex`.

## Use in a paper

Add `\usepackage{booktabs,tabularx,threeparttable}` to your preamble. Copy the tables you need into the manuscript directory and include them with `\input{filename.tex}`. Each is a complete numbered table, with caption, unique label and methodological notes. No colors, vertical rules or tiny-font scaling are used.

Each output folder has a `preview.tex` document. Compile it from that folder with `pdflatex preview.tex`. It includes the summary tables; uncomment the search-grid input for full diagnostics. Individual tables do not force page breaks; the preview does, for easier inspection.

## Interpretation and provenance

Descriptive statistics describe the distribution of survey-weighted cohort means. They are not person-level income statistics. Annual tables count cohort cells; panel tables count cohort-transition observations. Regression population weights and all estimation settings are unchanged. The small-cell cutoffs are nested and must not be added together.

The combined descriptive panel is reconstructed in memory from the two period panels, excluding the 2019–2021 gap. Missing saved bandwidth logs produce an explicit warning rather than invented values. The initially missing combined-period bandwidth log was regenerated using the existing grouped-CV helper and the six-transition panel; no bootstrap or figures were regenerated as part of the table export.

The exporter needs only base R and saved project data. Compiling the `.tex` files requires a LaTeX installation. Source generation and numeric checks were performed; PDF compilation was unavailable in the development environment.
