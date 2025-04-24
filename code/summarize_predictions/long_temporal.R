library(tidyverse)
library(tidymodels)
library(future)
library(furrr)
# plan(multicore, workers = 8)
source(here::here("code", "R", "utils.R"))
source(here::here("data", "lily", "code", "summary_fns.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE
if (!dir.exists(here::here("data", "lily", "data", "fingerprint_prediction_results"))) {
  dir.create(here::here(
    "data",
    "lily",
    "data",
    "fingerprint_prediction_results"
  ))
}


dirnums = c(100)
outfiles = paste("prediction_res_temporal2_", dirnums, "long.rds", sep = "")
pred_dirs = paste(
  here::here("data", "lily", "data", "fingerprint_res_temporal2"),
  paste0(dirnums, "long"),
  sep = "/"
)

purrr::pwalk(.l = list(outfile = outfiles,
                       dirnum = dirnums,
                       pred_dir = pred_dirs),
             .f = get_summarized_predictions_full,
             filenames_file = "fingerprint_folds_long_temporal.rds",
             individual = FALSE,
             exp = FALSE)

dirnums = c(2500, 5000, 8018)
outfiles = paste("prediction_res_temporal2", dirnums, "long.rds", sep = "")
pred_dirs = paste(
  here::here("data", "lily", "data", "fingerprint_res"),
  paste0(dirnums, "long"),
  sep = "/"
)
testdata_names = paste("dat_nzv_test_long_temporal", dirnums, sep = "")


purrr::pwalk(.l = list(outfile = outfiles,
                       dirnum = dirnums,
                       pred_dir = pred_dirs,
                       testdata_name = testdata_names),
             .f = get_summarized_predictions_full,
             filenames_file = "fingerprint_folds_long_temporal.rds",
             individual = TRUE,
             exp = FALSE,
             n_max = 8018)
