log_msg <- function(..., level = "INFO") {
  cat(sprintf(
    "[%s] %-5s %s\n",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    level,
    paste0(...)
  ))
}

## ============== Generate the pseudopanel (for each strata) ===============================

## build_cohorts(): collapses already-filtered individual data into
## education x age cohort means. Replaces the ten near-identical blocks.
##   data        - a subset of dat_pet
##   sample_name - label written into the `sample` column
build_cohorts <- function(data, sample_name, year) {
  data %>%
    dplyr::group_by(education, age) %>%
    dplyr::summarize(
      n = n(),
      weights_total = sum(weights, na.rm = TRUE),
      avg_labor_income = weighted.mean(labor_income, weights, na.rm = TRUE),
      avg_add_labor_income = weighted.mean(
        add_labor_income,
        weights,
        na.rm = TRUE
      ),
      avg_no_labor_income = weighted.mean(
        no_labor_income,
        weights,
        na.rm = TRUE
      ),
      avg_remittances = weighted.mean(remittances, weights, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(year = year, sample = sample_name)
}


## report_small_cells(): para cada tabla de cohortes (education x age) cuenta
## cuántas celdas caen por debajo de 30 / 20 / 10 observaciones, imprime el
## reporte en formato log y devuelve el resumen.
##   cohort_tables : lista NOMBRADA de data frames de cohortes
##   year          : solo se usa en el encabezado del reporte
report_small_cells <- function(cohort_tables) {
  report <- purrr::imap_dfr(cohort_tables, function(tbl, nm) {
    tibble::tibble(
      sample = nm,
      n_cohorts = nrow(tbl),
      `cells<30` = sum(tbl$n < 30),
      `cells<20` = sum(tbl$n < 20),
      `cells<10` = sum(tbl$n < 10)
    )
  })

  year <- head(cohort_tables$general$year,1)

  cat("\n")
  log_msg("==================================================================")
  log_msg("SMALL-CELL REPORT ", year)
  log_msg("Cohorts defined by education x age. Counts = cohorts below threshold.")
  log_msg("==================================================================")

  cat(sprintf(
    "[%s] %-5s %-16s %10s %10s %10s %10s\n",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "INFO", "sample", "n_cohorts", "cells<30", "cells<20", "cells<10"
  ))

  for (i in seq_len(nrow(report))) {
    r <- report[i, ]
    lvl <- if (r$`cells<10` > 0) "WARN" else "INFO"
    cat(sprintf(
      "[%s] %-5s %-16s %10d %10d %10d %10d\n",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      lvl, r$sample, r$n_cohorts, r$`cells<30`, r$`cells<20`, r$`cells<10`
    ))
  }

  log_msg("------------------------------------------------------------------")
  log_msg("TOTAL across all sub-samples: ",
          "cells<30 = ", sum(report$`cells<30`), " | ",
          "cells<20 = ", sum(report$`cells<20`), " | ",
          "cells<10 = ", sum(report$`cells<10`))

  gen <- report[report$sample == "general", ]
  log_msg("GENERAL sample: ", gen$n_cohorts, " cohorts | ",
          gen$`cells<30`, " below 30 | ",
          gen$`cells<20`, " below 20 | ",
          gen$`cells<10`, " below 10")
  log_msg("==================================================================")
  cat("\n")

  report
}


plot_capacity_curve <- function(samples, labels, y_var, title) {

  # y_var: "y1" for labor income, "y2" for in-kind, etc.
  x_col <- paste0(y_var, "_1")
  y_col <- paste0(y_var, "_2")

  x_grid  <- seq(0, 7000, length.out = 100)
  newdata <- setNames(data.frame(x_grid), x_col)

  # Fit LOESS (default span) + 95% CI for each sample
  plot_data <- bind_rows(lapply(seq_along(samples), function(i) {
    df <- filter(data, sample == samples[i])
    ci <- loess_ci(as.formula(paste(y_col, "~", x_col)), df, newdata)
    data.frame(x = x_grid, ci, group = labels[i])
  })) |>
    mutate(group = factor(group, levels = labels))

  ggplot(plot_data, aes(x = x)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, group = group),
                alpha = 0.15, linetype = 0) +
    geom_line(aes(y = fit, linetype = group), linewidth = 1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(x = "Income t", y = "Income t+1", title = title) +
    scale_linetype_manual(
      name   = "Sample",
      values = c("solid", "dashed", "dotted", "dotdash")
    ) +
    scale_y_quetzal() +
    scale_x_quetzal() +
    theme_apa()
}


## =============== Helper: LOESS fit + 95% CI ======================

# Fits a LOESS model (default span) and returns fit, lower and upper
# bounds of the pointwise 95% confidence interval evaluated on newdata.
loess_ci <- function(formula, data, newdata) {
  fit  <- loess(formula, data = data)          # default span
  pred <- predict(fit, newdata = newdata, se = TRUE)
  tcrit <- qt(0.975, pred$df)
  data.frame(
    fit = pred$fit,
    lwr = pred$fit - tcrit * pred$se.fit,
    upr = pred$fit + tcrit * pred$se.fit
  )
}