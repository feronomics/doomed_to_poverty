# Plain LaTeX publication tables. Requires booktabs, tabularx, threeparttable.
# Existing CSV/XLSX outputs are not changed. No LaTeX or R add-on is needed
# to write the .tex files; a TeX distribution is needed to compile them.
tex_escape <- function(x) {
  mapping <- c("\\" = "\\textbackslash{}", "&" = "\\&", "%" = "\\%",
    "$" = "\\$", "#" = "\\#", "_" = "\\_", "{" = "\\{", "}" = "\\}",
    "~" = "\\textasciitilde{}", "^" = "\\textasciicircum{}",
    "<" = "\\ensuremath{<}", ">" = "\\ensuremath{>}")
  vapply(as.character(x), function(s) {
    if (is.na(s)) return("--")
    chars <- strsplit(s, "", fixed = TRUE)[[1]]
    paste0(ifelse(chars %in% names(mapping), mapping[chars], chars), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}
tex_number <- function(x, digits = 1) {
  vapply(x, function(v) if (is.na(v)) "--" else if (!is.finite(v)) "Invalid" else
    formatC(v, format = "f", digits = digits, big.mark = ","), character(1))
}
tex_id <- function(x) gsub("[^a-z0-9]+", "-", tolower(x))
sample_label <- function(x) {
  labels <- c(general = "General", males = "Men", females = "Women", urban = "Urban",
    rural = "Rural", indigenous = "Indigenous", `non-indigenous` = "Non-Indigenous",
    p99 = "Bottom 99 percent", p90 = "Bottom 90 percent", p50 = "Bottom 50 percent")
  out <- unname(labels[x]); out[is.na(out)] <- x[is.na(out)]; out
}
write_tex_table <- function(data, caption, label, notes, path, first_width = NULL) {
  stopifnot(ncol(data) >= 2L, nrow(data) <= 26L)
  align <- paste0("@{}", if (is.null(first_width)) "X" else first_width,
                  paste(rep("r", ncol(data) - 1), collapse = ""), "@{}")
  body <- apply(as.matrix(data), 1, function(row) paste0(paste(tex_escape(row), collapse = " & "), " \\\\"))
  text <- c("% Generated from project outputs. Do not edit by hand.",
    "\\begin{table}[!htbp]", "\\centering", "\\begin{threeparttable}",
    paste0("\\caption{", tex_escape(caption), "}"), paste0("\\label{tab:", tex_id(label), "}"),
    "\\small", "\\setlength{\\tabcolsep}{4pt}", "\\renewcommand{\\arraystretch}{1.15}",
    paste0("\\begin{tabularx}{\\linewidth}{", align, "}"), "\\toprule",
    paste0(paste(tex_escape(names(data)), collapse = " & "), " \\\\"), "\\midrule",
    body, "\\bottomrule", "\\end{tabularx}",
    "\\begin{tablenotes}[flushleft]", "\\footnotesize",
    paste0("\\item ", tex_escape(notes)), "\\end{tablenotes}",
    "\\end{threeparttable}", "\\end{table}")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(text, path, useBytes = TRUE)
  invisible(path)
}

export_descriptive_tex <- function(data, period, output_dir) {
  dir <- file.path(output_dir, "latex", "descriptive")
  income_names <- c("Labor income", "Including in-kind", "Including nonlabor", "Including remittances")
  common <- paste("Monthly income in quetzales. Unweighted descriptive statistics of survey-weighted cohort means.",
    "N counts cohort-transition rows, not individuals or independent cohorts.",
    "SD is dispersion across cohort means, not the standard error of a mean.")
  if (period == "2016-2024") common <- paste(common, "Six annual transitions are pooled; the 2019-2021 gap is excluded.")
  general <- data[data$sample == "general", , drop = FALSE]
  for (side in 1:2) {
    values <- lapply(1:4, function(j) general[[paste0("y", j, "_", side)]])
    x <- lapply(values, function(v) v[is.finite(v)])
    stat <- function(fun) vapply(x, function(v) if (!length(v)) NA_real_ else fun(v), numeric(1))
    location <- data.frame(Income = income_names, N = tex_number(lengths(x), 0),
      Mean = tex_number(stat(mean)), SD = tex_number(stat(sd)),
      Min = tex_number(stat(min)), Max = tex_number(stat(max)), check.names = FALSE)
    quantiles <- data.frame(Income = income_names,
      P25 = tex_number(stat(function(v) unname(quantile(v, .25)))),
      Median = tex_number(stat(median)),
      P75 = tex_number(stat(function(v) unname(quantile(v, .75)))), check.names = FALSE)
    time <- if (side == 1) "t" else "t+1"
    for (kind in c("moments", "quantiles")) {
      tab <- if (kind == "moments") location else quantiles
      name <- paste0("descriptive_", period, "_", kind, "_t", side)
      write_tex_table(tab, paste("General-sample income", kind, "at", time, "in", period), name,
                      common, file.path(dir, paste0(name, ".tex")))
    }
  }
  groups <- c("general", "males", "females", "urban", "rural", "indigenous", "non-indigenous", "p99", "p90", "p50")
  for (j in 1:4) {
    rows <- lapply(groups, function(group) {
      df <- data[data$sample == group, , drop = FALSE]
      a <- df[[paste0("y", j, "_1")]]; b <- df[[paste0("y", j, "_2")]]
      complete <- is.finite(a) & is.finite(b); a <- a[complete]; b <- b[complete]
      data.frame(Sample = sample_label(group), N = tex_number(length(a), 0),
        `Mean t` = tex_number(mean(a)), `SD t` = tex_number(sd(a)),
        `Mean t+1` = tex_number(mean(b)), `SD t+1` = tex_number(sd(b)), check.names = FALSE)
    })
    name <- paste0("descriptive_", period, "_subsamples_y", j)
    write_tex_table(do.call(rbind, rows), paste(income_names[j], "by subsample:", period), name,
      paste(common, "Both incomes must be observed. Subsamples overlap; bottom-income groups are cumulative."),
      file.path(dir, paste0(name, ".tex")))
  }
}

export_small_cells_tex <- function(report, output_dir) {
  for (year in unique(report$year)) {
    r <- report[report$year == year, , drop = FALSE]
    tab <- data.frame(Sample = sample_label(r$sample), Cohorts = tex_number(r$n_cohorts, 0),
      `n < 30` = tex_number(r[["cells<30"]], 0), `n < 20` = tex_number(r[["cells<20"]], 0),
      `n < 10` = tex_number(r[["cells<10"]], 0), `Missing SE` = tex_number(r$se_NA, 0), check.names = FALSE)
    name <- paste0("small_cells_", year)
    write_tex_table(tab, paste("Cohort cell sizes by subsample:", year), name,
      paste("Cells are education-by-age cohorts; n is the unweighted number of surveyed respondents.",
            "Cutoffs are strict and nested. Missing SE counts missing labor-income mean standard errors.",
            "Subsamples overlap and must not be added together."),
      file.path(output_dir, "latex", "small_cells", paste0(name, ".tex")))
  }
}

export_annual_descriptive_tex <- function(data, year, output_dir) {
  components <- c("avg_labor_income", "avg_add_labor_income", "avg_no_labor_income", "avg_remittances")
  incomes <- c("Labor income", "Including in-kind", "Including nonlabor", "Including remittances")
  for (j in 1:4) {
    # Match the cumulative income definitions in the merging scripts.
    y <- if (j == 1) data[[components[1]]] else rowSums(data[components[seq_len(j)]], na.rm = TRUE)
    rows <- lapply(unique(data$sample), function(group) {
      v <- y[data$sample == group]; v <- v[is.finite(v)]
      stat <- function(f) if (length(v)) f(v) else NA_real_
      data.frame(Sample = sample_label(group), N = tex_number(length(v), 0),
        Mean = tex_number(stat(mean)), SD = tex_number(stat(sd)),
        Min = tex_number(stat(min)), Max = tex_number(stat(max)),
        P25 = tex_number(stat(function(x) unname(quantile(x, .25)))),
        Median = tex_number(stat(median)), P75 = tex_number(stat(function(x) unname(quantile(x, .75)))),
        check.names = FALSE)
    })
    tab <- do.call(rbind, rows)
    for (kind in c("moments", "quantiles")) {
      cols <- if (kind == "moments") c("Sample", "N", "Mean", "SD", "Min", "Max") else c("Sample", "P25", "Median", "P75")
      name <- paste0("annual_", year, "_y", j, "_", kind)
      write_tex_table(tab[cols], paste(incomes[j], "by subsample:", year, kind), name,
        paste("Monthly quetzales. Unweighted distribution of survey-weighted cohort means before transition matching.",
          "N counts cohort cells, not individuals. SD is dispersion, not a standard error. Subsamples overlap.",
          "Cumulative income definitions and treatment of missing components follow the merging scripts."),
        file.path(output_dir, "latex", "annual_descriptive", paste0(name, ".tex")))
    }
  }
}

short_regression_label <- function(x) {
  x <- sub("^(2016-2019|2021-2024) ", "", x)
  x <- sub("^General Sample \\(2016-2024\\) - ", "", x)
  x <- sub("^Capacity Curve[^:]*: ", "", x)
  x <- sub(".* - (Labor income|Including in-kind|Including non-labor|Including remittances)", "\\1", x)
  x <- sub("General y1$", "General: labor", x)
  x <- sub("General y2$", "General: in-kind", x)
  x <- sub("General y3$", "General: nonlabor", x)
  x <- sub("General y4$", "General: remittances", x)
  x
}

export_bandwidth_tex <- function(records, grid, output_dir, context) {
  # Split rows by period and paginate. Shared tuning settings are in the notes.
  periods <- ifelse(grepl("2016-2019", records$regression), "2016-2019",
    ifelse(grepl("2021-2024", records$regression), "2021-2024", context))
  for (period in unique(periods)) {
    r <- records[periods == period, , drop = FALSE]
    for (page in seq_len(ceiling(nrow(r) / 13))) {
      s <- r[seq.int((page - 1) * 13 + 1, min(page * 13, nrow(r))), , drop = FALSE]
      tab <- data.frame(Regression = short_regression_label(s$regression),
        Span = tex_number(s$optimal_span, 3), `CV MSE` = tex_number(s$cv_mse, 1),
        Cohorts = tex_number(s$n_cohorts, 0), N = tex_number(s$n_rows, 0),
        Edge = ifelse(s$at_grid_boundary, "Yes", "No"), check.names = FALSE)
      name <- paste0("bandwidth_", tex_id(context), "_", period, "_", page)
      note <- paste("Population-weighted cohort-grouped cross-validation; MSE is in squared quetzales.",
        "N counts transitions. Edge indicates a search-boundary selection, not an interior optimum.",
        "Folds:", paste(unique(s$folds), collapse = ","), "; seed:", paste(unique(s$seed), collapse = ","),
        "; regression weights:", paste(unique(s$weight), collapse = ","))
      write_tex_table(tab, paste("LOESS bandwidth selection:", context, period), name, note,
        file.path(output_dir, "latex", "bandwidth", paste0(name, ".tex")))
    }
  }
  for (i in seq_along(unique(grid$regression))) {
    label <- unique(grid$regression)[i]; r <- grid[grid$regression == label, , drop = FALSE]
    for (page in seq_len(ceiling(nrow(r) / 25))) {
      s <- r[seq.int((page - 1) * 25 + 1, min(page * 25, nrow(r))), , drop = FALSE]
      tab <- data.frame(Span = tex_number(s$span, 3), `CV MSE` = tex_number(s$cv_mse, 1),
        Predicted = tex_number(s$n_predicted, 0), N = tex_number(s$n_rows, 0),
        Extrapolated = tex_number(s$n_extrapolated, 0), check.names = FALSE)
      name <- paste0("grid_", tex_id(context), "_", i, "_", page)
      write_tex_table(tab, paste("Bandwidth search:", label, "(part", page, ")"), name,
        paste("Weighted held-out MSE in squared quetzales. Predicted counts finite held-out predictions; N is the required count.",
          "Candidates with failed predictions are invalid. Extrapolated counts validation incomes outside training support."),
        file.path(output_dir, "latex", "search_grids", paste0(name, ".tex")))
    }
  }
}

export_robustness_tex <- function(data, output_dir, type) {
  if (!"mode" %in% names(data)) data$mode <- "bandwidth"
  for (period in unique(data$period)) for (mode in unique(data$mode)) {
    r <- data[data$period == period & data$mode == mode, , drop = FALSE]
    if (!nrow(r)) next
    id <- if (type == "age") "age_cutoff" else "delta"
    refs <- sort(unique(r$y_t))
    rows <- lapply(unique(r[[id]]), function(value) {
      s <- r[r[[id]] == value, , drop = FALSE]
      stopifnot(!anyDuplicated(s$y_t))
      label <- if (type == "age") as.character(value) else sprintf("%+.2f", value)
      out <- data.frame(Variant = label, Span = tex_number(s$span[1], 3), check.names = FALSE)
      for (ref in refs) out[[paste0("Q", ref)]] <- tex_number(s$predicted_y_t1[match(ref, s$y_t)])
      out
    })
    name <- paste0("robustness_", type, "_", period, "_", tex_id(mode))
    write_tex_table(do.call(rbind, rows), paste("Capacity-curve sensitivity:", type, period, mode), name,
      paste("Entries are predicted income at t+1 (quetzales); column headings give income at t.",
        "General sample, labor income, population-weighted LOESS. -- indicates no supported prediction.",
        if (type == "age") "Retuned selects a span for each cutoff; fixed_baseline holds the main-sample span fixed."
        else "Variant is the additive change from the selected span. Data and weights are held fixed."),
      file.path(output_dir, "latex", "robustness", paste0(name, ".tex")))
    counts <- unique(r[c(id, "span", "n_cohorts", "n_pairs")])
    names(counts) <- c("Variant", "Span", "Cohorts", "Transitions")
    counts$Span <- tex_number(counts$Span, 3)
    counts$Cohorts <- tex_number(counts$Cohorts, 0)
    counts$Transitions <- tex_number(counts$Transitions, 0)
    write_tex_table(counts, paste("Robustness sample sizes:", type, period, mode),
      paste0(name, "_samples"), "Cohorts are distinct education-by-reference-age groups. Transitions are regression observations.",
      file.path(output_dir, "latex", "robustness", paste0(name, "_samples.tex")))
  }
}

export_saved_bandwidth_tex <- function(directory, output_dir = directory, context = basename(directory)) {
  summary <- file.path(directory, "bandwidth_selection_summary.csv")
  grid <- file.path(directory, "bandwidth_selection_summary_grid.csv")
  if (!file.exists(summary) || !file.exists(grid)) {
    warning("No saved bandwidth results for ", directory, "; run its analysis to produce these tables.", call. = FALSE)
    return(invisible(NULL))
  }
  export_bandwidth_tex(read.csv(summary), read.csv(grid), output_dir, context)
  write_latex_index(output_dir)
}

write_latex_index <- function(output_dir) {
  dir <- file.path(output_dir, "latex")
  if (!dir.exists(dir)) return(invisible(NULL))
  files <- sort(list.files(dir, pattern = "[.]tex$", recursive = TRUE))
  files <- files[grepl("/", files)]
  ordinary <- files[!grepl("^search_grids/", files)]
  grids <- files[grepl("^search_grids/", files)]
  inputs <- function(paths) unlist(lapply(paths, function(p) c(paste0("\\input{", p, "}"), "\\clearpage")))
  writeLines(inputs(ordinary), file.path(dir, "tables.tex"))
  writeLines(inputs(grids), file.path(dir, "search_grids.tex"))
  writeLines(c("\\documentclass[11pt]{article}", "\\usepackage[margin=1in]{geometry}",
    "\\usepackage{booktabs,tabularx,threeparttable}", "\\begin{document}",
    "\\input{tables.tex}", "% Uncomment for full candidate search grids:",
    "% \\input{search_grids.tex}", "\\end{document}"), file.path(dir, "preview.tex"))
  writeLines(c("Compile preview.tex from this latex directory (pdflatex preview.tex).",
    "Tables use booktabs, tabularx and threeparttable. Each file is a complete table float.",
    "Copy the desired .tex file into your manuscript and use \\input{filename.tex}.",
    "tables.tex includes publication summaries; search_grids.tex includes the full tuning diagnostics.",
    "The preview separates tables onto pages; individual table files do not force page breaks.",
    "No CSV or Excel files are removed. Counts and estimates are unchanged by formatting."), file.path(dir, "README.txt"))
}
