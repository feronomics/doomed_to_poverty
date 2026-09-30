## Pool the two annual-transition panels after running their merge scripts.
## A common reference age preserves cohort identity across both periods.
## No 2019-2021 pair: that is a two-year gap, not an annual transition.
source("resources/pipeline_config.R")

pre <- read.csv(file.path(PROCESSED_DIR, "data_2016-2019.csv"))
post <- read.csv(file.path(PROCESSED_DIR, "data_2021-2024.csv"))
required <- c("education", "sample", "comparison", "weights_total_1",
              "weights_total_2", "n_1", "n_2", "n_eff_1", "n_eff_2",
              paste0("se_y", rep(1:4, each = 2), "_", 1:2),
              paste0("y", rep(1:4, each = 2), "_", 1:2))
stopifnot(all(c(required, "age_2019") %in% names(pre)),
          all(c(required, "age_2021") %in% names(post)))
pre$age_2021 <- pre$age_2019 + 2
pre$age_2019 <- NULL
data_wide_completed <- rbind(pre[, c("age_2021", required)],
                             post[, c("age_2021", required)])
expected <- c("2016-2017", "2017-2018", "2018-2019",
              "2021-2022", "2022-2023", "2023-2024")
if (!setequal(unique(data_wide_completed$comparison), expected))
  stop("Combined panel must contain all six annual transitions and no gaps.")
if (anyDuplicated(data_wide_completed[c("age_2021", "education", "sample", "comparison")]))
  stop("Duplicate cohort-transition keys in combined panel.")
write.csv(data_wide_completed, file.path(PROCESSED_DIR, "data_2016-2024.csv"),
          row.names = FALSE, na = "NA")
cat("Merged 2016-2024:", nrow(data_wide_completed),
    "cohort-transition rows, six annual transitions; 2019-2021 excluded.\n")
