# Population-weighted capacity curves. CV and bootstrap units are whole
# education x reference-age cohorts, never individual transition rows.
# Intervals are pointwise, conditional on the selected span and observed
# cohort means; they are not survey-design or simultaneous confidence bands.

with_loess_seed <- function(seed, code) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
          else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
            rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed)
  force(code)
}

get_cohort_id <- function(data) {
  age_col <- grep("^age_[0-9]{4}$", names(data), value = TRUE)
  if (length(age_col) != 1L || !"education" %in% names(data))
    stop("Expected education and exactly one reference-age column (age_YYYY).")
  if (anyNA(data[, c("education", age_col)])) stop("Missing cohort identifiers.")
  as.character(interaction(data$education, data[[age_col]], drop = TRUE, sep = "__"))
}

prepare_loess_data <- function(formula, data, min_n = 0) {
  data <- as.data.frame(data)
  vars <- all.vars(formula)
  if (length(vars) != 2L) stop("Expected one outcome and one income regressor.")
  needed <- c(vars, "weights_total_1", if (min_n > 0) c("n_1", "n_2"))
  missing <- setdiff(needed, names(data))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))
  keep <- is.finite(data[[vars[1]]]) & is.finite(data[[vars[2]]]) &
    is.finite(data$weights_total_1) & data$weights_total_1 > 0
  if (min_n > 0) keep <- keep & !is.na(data$n_1) & !is.na(data$n_2) &
    data$n_1 >= min_n & data$n_2 >= min_n
  data <- data[keep, , drop = FALSE]
  if (nrow(data) < 10L) stop("Fewer than 10 usable transition rows.")
  data$.cohort <- get_cohort_id(data)
  if (length(unique(data$.cohort)) < 3L) stop("At least three cohorts are required.")
  data$.w <- data$weights_total_1
  data
}

cohort_folds <- function(cohort, k = 10, seed = 123) {
  ids <- sort(unique(cohort))
  k <- min(as.integer(k), length(ids))
  if (is.na(k) || k < 2L) stop("At least two folds are required.")
  assignments <- with_loess_seed(seed, sample(rep(seq_len(k), length.out = length(ids))))
  unname(assignments[match(cohort, ids)])
}

# Direct evaluation is used consistently in fitting and CV. CV includes
# held-out extremes (extrapolation) rather than silently discarding them.
# Public curve predictions are masked outside the fitting sample's support.
fit_weighted_loess <- function(formula, data, span) {
  if (length(span) != 1L || !is.finite(span) || span <= 0) stop("Invalid span.")
  data <- as.data.frame(data)
  data$.w <- data$weights_total_1
  stats::loess(formula, data = data, weights = .w, span = span,
               control = stats::loess.control(surface = "direct"))
}

predict_capacity <- function(fit, newdata) {
  pred <- as.numeric(stats::predict(fit, newdata = newdata))
  x <- as.numeric(fit$x[, 1])
  x_new <- newdata[[colnames(fit$x)[1]]]
  pred[!is.finite(pred) | x_new < min(x) | x_new > max(x)] <- NA_real_
  pred
}

select_loess_span <- function(formula, data,
                              span_grid = seq(0.15, 1, by = 0.025),
                              k = 10, seed = 123, max_span = 3) {
  data <- prepare_loess_data(formula, data)
  spans <- sort(unique(span_grid))
  if (!length(spans) || any(!is.finite(spans) | spans <= 0) ||
      !is.finite(max_span) || max_span < max(spans)) stop("Invalid span search grid.")
  folds <- cohort_folds(data$.cohort, k, seed)
  vars <- all.vars(formula)
  score <- function(span) {
    pred <- rep(NA_real_, nrow(data))
    extrapolated <- 0L
    for (f in sort(unique(folds))) {
      train <- data[folds != f, , drop = FALSE]
      test <- data[folds == f, , drop = FALSE]
      extrapolated <- extrapolated + sum(test[[vars[2]]] < min(train[[vars[2]]]) |
                                         test[[vars[2]]] > max(train[[vars[2]]]))
      p <- tryCatch(suppressWarnings({
        fit <- fit_weighted_loess(formula, train, span)
        as.numeric(stats::predict(fit, newdata = test))
      }), error = function(e) rep(NA_real_, nrow(test)))
      pred[folds == f] <- p
    }
    valid <- all(is.finite(pred))
    # A candidate with any failed prediction cannot win on an easier subset.
    mse <- if (valid) stats::weighted.mean((data[[vars[1]]] - pred)^2, data$.w) else Inf
    data.frame(span = span, cv_mse = mse, n_predicted = sum(is.finite(pred)),
               n_rows = nrow(data), n_extrapolated = extrapolated)
  }
  grid <- do.call(rbind, lapply(spans, score))
  choose <- function(g) {
    usable <- which(is.finite(g$cv_mse))
    if (!length(usable)) stop("No span predicted every held-out row successfully.")
    usable[which.min(g$cv_mse[usable])]
  }
  best <- choose(grid)
  # Expand an upper-edge optimum instead of silently calling span = 1 optimal.
  while (best == nrow(grid) && max(grid$span) < max_span) {
    upper <- min(max_span, max(grid$span) + 0.5)
    lower <- max(grid$span) + 0.1
    extra <- unique(c(if (lower <= upper) seq(lower, upper, by = 0.1), upper))
    grid <- rbind(grid, do.call(rbind, lapply(extra, score)))
    grid <- grid[order(grid$span), ]
    best <- choose(grid)
  }
  boundary <- best %in% c(1L, nrow(grid))
  if (boundary) warning("Selected span is at the search boundary; inspect the CV grid.", call. = FALSE)
  list(span = grid$span[best], grid = grid, at_boundary = boundary,
       n_cohorts = length(unique(data$.cohort)), k = length(unique(folds)),
       folds = data.frame(cohort = data$.cohort, fold = folds), seed = seed)
}

.span_log <- new.env(parent = emptyenv())
.span_log$records <- list()
.span_log$grids <- list()
record_span_choice <- function(label, sel) {
  best <- sel$grid[sel$grid$span == sel$span, , drop = FALSE]
  log_msg(sprintf("%s | selected span %.3f | weighted grouped-CV MSE %.3f",
                  label, sel$span, best$cv_mse))
  .span_log$records[[length(.span_log$records) + 1L]] <- data.frame(
    regression = label, optimal_span = sel$span, cv_mse = best$cv_mse,
    n_cohorts = sel$n_cohorts, n_rows = best$n_rows, folds = sel$k,
    seed = sel$seed, at_grid_boundary = sel$at_boundary,
    weight = "weights_total_1")
  .span_log$grids[[length(.span_log$grids) + 1L]] <- cbind(regression = label, sel$grid)
}

dump_span_log <- function(path) {
  if (!length(.span_log$records)) return(invisible(NULL))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(do.call(rbind, .span_log$records), path, row.names = FALSE)
  grid_path <- sub("\\.csv$", "_grid.csv", path)
  if (identical(grid_path, path)) grid_path <- paste0(path, "_grid.csv")
  utils::write.csv(do.call(rbind, .span_log$grids), grid_path, row.names = FALSE)
  .span_log$records <- list()
  .span_log$grids <- list()
}

fit_capacity <- function(formula, data, span = NULL, min_n = 0, span_label = NULL) {
  data <- prepare_loess_data(formula, data, min_n)
  if (is.null(span)) {
    sel <- select_loess_span(formula, data)
    span <- sel$span
    if (!is.null(span_label)) record_span_choice(span_label, sel)
  }
  list(fit = fit_weighted_loess(formula, data, span), span = span, data = data)
}

sample_cohort_rows <- function(cohort) {
  rows <- split(seq_along(cohort), cohort)
  unlist(rows[sample(names(rows), length(rows), replace = TRUE)], use.names = FALSE)
}

# No independent jitter of cohort-year means. Preserve se_* upstream for
# diagnostics and future design-based inference. Fixed-span percentile bands
# omit tuning uncertainty, survey-design resampling and smoothing-bias correction.
loess_ci <- function(formula, df, newdata, B = 1000, span = NULL, min_n = 0,
                      span_label = NULL, seed = 123, conf = 0.95) {
  if (length(B) != 1 || !is.finite(B) || B < 20 || B != as.integer(B))
    stop("B must be an integer of at least 20.")
  if (length(conf) != 1 || !is.finite(conf) || conf <= 0 || conf >= 1)
    stop("conf must lie between 0 and 1.")
  model <- fit_capacity(formula, df, span, min_n,
                        if (is.null(span_label)) paste(deparse(formula), collapse = " ") else span_label)
  df <- model$data
  boot <- with_loess_seed(seed, vapply(seq_len(B), function(b) {
    d <- df[sample_cohort_rows(df$.cohort), , drop = FALSE]
    tryCatch(suppressWarnings(
      predict_capacity(fit_weighted_loess(formula, d, model$span), newdata)
    ), error = function(e) rep(NA_real_, nrow(newdata)))
  }, numeric(nrow(newdata))))
  boot <- matrix(boot, nrow = nrow(newdata), ncol = B)
  effective <- rowSums(is.finite(boot))
  threshold <- ceiling(0.8 * B)
  quant <- function(x, p) {
    x <- x[is.finite(x)]
    if (length(x) < threshold) return(NA_real_)
    unname(stats::quantile(x, p))
  }
  point <- predict_capacity(model$fit, newdata)
  if (any(is.finite(point) & effective < threshold))
    warning("Some intervals omitted: fewer than 80% of bootstrap fits predict that income.", call. = FALSE)
  out <- data.frame(fit = point,
    lwr = apply(boot, 1, quant, p = (1 - conf) / 2),
    upr = apply(boot, 1, quant, p = 1 - (1 - conf) / 2),
    span = model$span, n_clusters = length(unique(df$.cohort)),
    B_effective = effective, B_requested = B)
  out$lwr[!is.finite(point)] <- out$upr[!is.finite(point)] <- NA_real_
  out
}
