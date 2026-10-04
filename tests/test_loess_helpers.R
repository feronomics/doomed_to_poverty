# Base-R regression tests; run Rscript tests/test_loess_helpers.R from root.
log_msg <- function(...) invisible(NULL)
source("resources/loess_helpers.R")
set.seed(42)
d <- expand.grid(age_2019 = 20:59, transition = 1:3, KEEP.OUT.ATTRS = FALSE)
d$education <- "primary"
d$y1_1 <- 100 + 80 * (d$age_2019 - 20) + 10 * d$transition
d$y1_2 <- 300 + 0.7 * d$y1_1 + 100 * sin(d$y1_1 / 400) + rnorm(nrow(d), 0, 40)
d$weights_total_1 <- ifelse(d$age_2019 %% 3 == 0, 20, 1)
d$n_1 <- d$n_2 <- 30
p <- prepare_loess_data(y1_2 ~ y1_1, d)
fold <- cohort_folds(p$.cohort)
stopifnot(all(vapply(split(fold, p$.cohort), function(x) length(unique(x)), integer(1)) == 1L))
set.seed(9)
before <- .Random.seed
sel <- suppressWarnings(select_loess_span(y1_2 ~ y1_1, d, c(0.4, 0.7, 1), max_span = 1))
stopifnot(identical(before, .Random.seed), all(sel$grid$n_predicted == nrow(d)))
# Independently reconstruct population-weighted held-out MSE.
pred <- numeric(nrow(d))
for (f in unique(fold)) {
  train <- d[fold != f, ]; train$.w <- train$weights_total_1
  fit <- loess(y1_2 ~ y1_1, train, weights = .w, span = 0.7,
               control = loess.control(surface = "direct"))
  pred[fold == f] <- predict(fit, d[fold == f, ])
}
expected <- weighted.mean((d$y1_2 - pred)^2, d$weights_total_1)
stopifnot(isTRUE(all.equal(sel$grid$cv_mse[sel$grid$span == 0.7], expected)))
# A common rescaling of population weights must not change selection scores.
d_scaled <- d; d_scaled$weights_total_1 <- d$weights_total_1 * 10
sel_scaled <- suppressWarnings(select_loess_span(y1_2 ~ y1_1, d_scaled, c(0.4, 0.7, 1), max_span = 1))
stopifnot(isTRUE(all.equal(sel$grid$cv_mse, sel_scaled$grid$cv_mse, tolerance = 1e-7)))
# Every sampled cohort contributes complete three-transition histories.
rows <- with_loess_seed(3, sample_cohort_rows(p$.cohort))
stopifnot(length(rows) == nrow(d), all(table(p$.cohort[rows]) %% 3 == 0))
grid <- data.frame(y1_1 = c(-100, 800, 1600, 2400, 10000))
ci <- suppressWarnings(loess_ci(y1_2 ~ y1_1, d, grid, B = 40, span = 0.7))
ci2 <- suppressWarnings(loess_ci(y1_2 ~ y1_1, d, grid, B = 40, span = 0.7))
stopifnot(identical(ci, ci2), all(is.na(ci$fit[c(1, 5)])),
          all(is.finite(ci$lwr[2:4])), all(ci$lwr[2:4] <= ci$upr[2:4]))
weighted <- fit_capacity(y1_2 ~ y1_1, d, span = 0.7)
equal <- d; equal$weights_total_1 <- 1
unweighted <- fit_capacity(y1_2 ~ y1_1, equal, span = 0.7)
stopifnot(any(abs(predict_capacity(weighted$fit, grid)[2:4] -
                  predict_capacity(unweighted$fit, grid)[2:4]) > 1e-4))
# Missing precision estimates are preserved, not converted to zero or used as jitter.
d$se_y1_1 <- NA_real_
stopifnot(all(is.na(prepare_loess_data(y1_2 ~ y1_1, d)$se_y1_1)))
cat("All grouped-CV, population-weight, bootstrap and reproducibility tests passed.\n")
