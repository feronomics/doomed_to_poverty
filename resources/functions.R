## Poverty trap in Guatemala?
## PIs:  Fernando Sáenz
##       Javier Velásquez
##       Alejandro Milián
##
## Last modification: 02 Aug 2026
##
## Purpose: funciones compartidas por todo el pipeline.
##
## Cambios (02 Aug 2026):
##  - build_cohorts() ahora devuelve n_eff (tamaño efectivo de Kish) y
##    se_y1..se_y4 (error estándar de la media dentro de cada celda).
##  - loess_ci() pasa de CI analítico (predict(se = TRUE), que sólo mide
##    incertidumbre de suavizado) a bootstrap de dos etapas:
##      etapa 1: remuestreo de cohortes
##      etapa 2: perturbación de cada media por su propio se_
##  - plot_capacity_curve() ya no depende de un objeto `data` global.

## ============== Logging ===============================

log_msg <- function(..., level = "INFO") {
  cat(sprintf(
    "[%s] %-5s %s\n",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    level,
    paste0(...)
  ))
}


## ============== Error estándar dentro de celda ===============================

## cell_se(): error estándar de una media ponderada.
##
## Devuelve NA (sin warning) cuando la celda tiene menos de 2 observaciones
## válidas: con una sola observación no hay varianza estimable. Esta guarda
## es lo que evita la cascada de warnings de Hmisc::wtd.var en celdas chicas.
##
## Usa n_eff (Kish) en el denominador, no n: con pesos desiguales, N
## personas no aportan la precisión de N observaciones equiponderadas.
cell_se <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]
  w <- w[ok]

  if (length(x) < 2) return(NA_real_)

  n_eff <- sum(w)^2 / sum(w^2)
  v <- suppressWarnings(Hmisc::wtd.var(x, w, normwt = TRUE))

  if (!is.finite(v) || v < 0) return(NA_real_)

  sqrt(v / n_eff)
}


## ============== Construcción del pseudopanel ===============================

## build_cohorts(): colapsa datos individuales ya filtrados en medias por
## cohorte (education x age).
##   data        - un subconjunto de dat_pet
##   sample_name - etiqueta que se escribe en la columna `sample`
##   year        - año de la encuesta
##
## Las definiciones acumulativas de ingreso (y1..y4) se construyen a nivel
## INDIVIDUAL antes de colapsar. Esto importa sólo para los errores
## estándar: la varianza de una suma no es la suma de varianzas, hay
## covarianzas entre componentes de ingreso. Las medias sí son aditivas,
## por eso y1..y4 se siguen recomponiendo en los scripts de merging.
build_cohorts <- function(data, sample_name, year) {
  data %>%
    dplyr::mutate(
      ind_y1 = labor_income,
      ind_y2 = labor_income + add_labor_income,
      ind_y3 = labor_income + add_labor_income + no_labor_income,
      ind_y4 = labor_income + add_labor_income + no_labor_income + remittances
    ) %>%
    dplyr::group_by(education, age) %>%
    dplyr::summarize(
      n             = dplyr::n(),
      weights_total = sum(weights, na.rm = TRUE),
      n_eff         = sum(weights, na.rm = TRUE)^2 / sum(weights^2, na.rm = TRUE),

      avg_labor_income     = weighted.mean(labor_income, weights, na.rm = TRUE),
      avg_add_labor_income = weighted.mean(add_labor_income, weights, na.rm = TRUE),
      avg_no_labor_income  = weighted.mean(no_labor_income, weights, na.rm = TRUE),
      avg_remittances      = weighted.mean(remittances, weights, na.rm = TRUE),

      se_y1 = cell_se(ind_y1, weights),
      se_y2 = cell_se(ind_y2, weights),
      se_y3 = cell_se(ind_y3, weights),
      se_y4 = cell_se(ind_y4, weights),

      .groups = "drop"
    ) %>%
    dplyr::mutate(year = year, sample = sample_name)
}


## ============== Reporte de celdas pequeñas ===============================

## report_small_cells(): para cada tabla de cohortes (education x age) cuenta
## cuántas celdas caen por debajo de 30 / 20 / 10 observaciones, imprime el
## reporte en formato log y devuelve el resumen.
report_small_cells <- function(cohort_tables) {
  report <- purrr::imap_dfr(cohort_tables, function(tbl, nm) {
    tibble::tibble(
      sample     = nm,
      n_cohorts  = nrow(tbl),
      `cells<30` = sum(tbl$n < 30),
      `cells<20` = sum(tbl$n < 20),
      `cells<10` = sum(tbl$n < 10),
      `se_NA`    = sum(is.na(tbl$se_y1))
    )
  })

  year <- head(cohort_tables$general$year, 1)

  cat("\n")
  log_msg("==================================================================")
  log_msg("SMALL-CELL REPORT ", year)
  log_msg("Cohorts defined by education x age. Counts = cohorts below threshold.")
  log_msg("se_NA = cells with n < 2, where no within-cell SE is estimable.")
  log_msg("==================================================================")

  cat(sprintf(
    "[%s] %-5s %-16s %10s %10s %10s %10s %8s\n",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "INFO", "sample", "n_cohorts", "cells<30", "cells<20", "cells<10", "se_NA"
  ))

  for (i in seq_len(nrow(report))) {
    r <- report[i, ]
    lvl <- if (r$`cells<10` > 0) "WARN" else "INFO"
    cat(sprintf(
      "[%s] %-5s %-16s %10d %10d %10d %10d %8d\n",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      lvl, r$sample, r$n_cohorts, r$`cells<30`, r$`cells<20`,
      r$`cells<10`, r$se_NA
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


## =============== LOESS + intervalo de confianza por bootstrap ==============

## loess_ci(): ajusta un LOESS ponderado y devuelve fit / lwr / upr
## evaluados sobre newdata.
##
## El intervalo viene de un bootstrap de DOS ETAPAS:
##
##   Etapa 1 (entre cohortes): remuestreo con reemplazo de las filas del
##   pseudopanel. Responde a "¿qué tan distinta sería la curva si el
##   conjunto de cohortes fuera otro?".
##
##   Etapa 2 (dentro de cohorte): cada media se perturba con ruido normal
##   de desviación igual a su propio se_. Responde a "cada punto es él
##   mismo un estimado con error, no un clavo fijo".
##
## LIMITACIONES que deben reportarse en el paper:
##  - Los se_ suponen muestreo aleatorio simple. ENEI/ENCOVI son
##    estratificadas y por conglomerados, así que estos se_ SUBESTIMAN la
##    varianza real. Corregirlo requiere las variables de diseño (UPM,
##    estrato) de los .sav, que hoy no se cargan en los config_YYYY.yaml.
##  - Perturbar el eje X propaga incertidumbre pero NO corrige el sesgo de
##    atenuación (error de medición en X aplana la pendiente estimada).
##    Corregirlo exige otro camino: variables instrumentales o SIMEX.
##  - Celdas con se_ = NA (n < 2) se tratan como medidas sin error, lo cual
##    es falso al revés. Considerar filtrar por min_n.
##
## Argumentos:
##   formula  - p.ej. y1_2 ~ y1_1
##   df       - data frame del pseudopanel (una submuestra)
##   newdata  - grid de predicción
##   B        - número de réplicas bootstrap
##   span     - parámetro de suavizado del LOESS
##   min_n    - descarta pares de cohortes con menos de min_n obs. en
##              cualquiera de los dos años (0 = no filtrar)
loess_ci <- function(formula, df, newdata, B = 1000, span = 0.75, min_n = 0) {

  vars <- all.vars(formula)
  yv <- vars[1]                                   # p.ej. "y1_2"
  xv <- vars[2]                                   # p.ej. "y1_1"
  se_y <- sub("^(y[0-9]+)_2$", "se_\\1_2", yv)
  se_x <- sub("^(y[0-9]+)_1$", "se_\\1_1", xv)

  df <- as.data.frame(df)

  ## --- Guarda: fallar con un mensaje legible, no con un error críptico ---
  needed <- c(yv, xv, se_y, se_x, "weights_total_1")
  missing <- setdiff(needed, names(df))
  if (length(missing) > 0) {
    stop("loess_ci: faltan columnas en los datos: ",
         paste(missing, collapse = ", "),
         "\n  -> ¿se corrió el merging actualizado que propaga se_*?",
         call. = FALSE)
  }

  ## --- Filtro opcional de celdas chicas ---
  if (min_n > 0 && all(c("n_1", "n_2") %in% names(df))) {
    df <- df[df$n_1 >= min_n & df$n_2 >= min_n, , drop = FALSE]
  }

  ## Columna de pesos con nombre simple: loess() evalúa `weights` dentro
  ## del model frame, así que referirse a df$algo desde fuera provoca
  ## errores de scoping ("variable lengths differ").
  df$.w <- df$weights_total_1

  ## Celdas sin se_ estimable: se tratan como exactas (ver limitaciones).
  df[[se_y]][!is.finite(df[[se_y]])] <- 0
  df[[se_x]][!is.finite(df[[se_x]])] <- 0

  df <- df[is.finite(df[[yv]]) & is.finite(df[[xv]]) & is.finite(df$.w), , drop = FALSE]

  if (nrow(df) < 10) {
    stop("loess_ci: sólo ", nrow(df), " filas utilizables; muy pocas para ajustar.",
         call. = FALSE)
  }

  fit <- loess(formula, data = df, span = span, weights = .w)

  boot <- replicate(B, {
    d <- df[sample(nrow(df), replace = TRUE), , drop = FALSE]
    d[[yv]] <- d[[yv]] + rnorm(nrow(d), 0, d[[se_y]])
    d[[xv]] <- d[[xv]] + rnorm(nrow(d), 0, d[[se_x]])

    f <- try(loess(formula, data = d, span = span, weights = .w), silent = TRUE)
    if (inherits(f, "try-error")) {
      rep(NA_real_, nrow(newdata))
    } else {
      suppressWarnings(predict(f, newdata = newdata))
    }
  })

  n_fallidas <- sum(apply(boot, 2, function(col) all(is.na(col))))
  if (n_fallidas > B * 0.05) {
    log_msg(level = "WARN",
            "loess_ci: ", n_fallidas, "/", B,
            " réplicas fallaron (", round(100 * n_fallidas / B), "%).",
            " Considerar un span mayor.")
  }

  data.frame(
    fit = suppressWarnings(predict(fit, newdata = newdata)),
    lwr = apply(boot, 1, quantile, 0.025, na.rm = TRUE),
    upr = apply(boot, 1, quantile, 0.975, na.rm = TRUE)
  )
}


## =============== Gráfico comparativo de curvas de capacidad ================

## plot_capacity_curve(): superpone curvas de capacidad de varias
## submuestras con sus bandas bootstrap.
##
## `data_all` es explícito. Si se omite, se busca un objeto `data` en el
## entorno del llamador, para no romper los scripts de análisis existentes;
## pero pasarlo explícitamente es lo recomendado.
plot_capacity_curve <- function(samples, labels, y_var, title,
                                data_all = NULL,
                                x_grid = seq(0, 7000, length.out = 100),
                                B = 1000, span = 0.75, min_n = 0) {

  if (is.null(data_all)) {
    data_all <- get("data", envir = parent.frame())
  }

  x_col <- paste0(y_var, "_1")
  y_col <- paste0(y_var, "_2")
  newdata <- setNames(data.frame(x_grid), x_col)

  plot_data <- bind_rows(lapply(seq_along(samples), function(i) {
    df <- dplyr::filter(data_all, sample == samples[i])
    ci <- loess_ci(as.formula(paste(y_col, "~", x_col)), df, newdata,
                   B = B, span = span, min_n = min_n)
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