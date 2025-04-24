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




get_summarized_predictions_full(
  exp = FALSE,
  individual = FALSE,
  outfile = "prediction_res_100lasso.rds",
  dirnum = 100,
  filenames_file = "fingerprint_folds.rds",
  pred_dir = here::here("data", "lily", "data", "fingerprint_res", "100lasso")
)

get_summarized_predictions_full(
  exp = FALSE,
  individual = TRUE,
  outfile = "prediction_res_500lasso.rds",
  dirnum = 500,
  filenames_file = "fingerprint_folds.rds",
  pred_dir = here::here("data", "lily", "data", "fingerprint_res", "500lasso"),
  force = TRUE,
  n_max = 13367,
  no_nzv_dat = TRUE,
  testdata_name = "all_grid_cells.csv.gz"
)


# temporal
get_summarized_predictions_full(
  exp = FALSE,
  individual = FALSE,
  outfile = "prediction_res_temporal2_100lasso.rds",
  dirnum = 100,
  filenames_file = "fingerprint_folds_temporal2.rds",
  pred_dir = here::here("data", "lily", "data", "fingerprint_res_temporal2", "100lasso")
)

get_summarized_predictions_full(
  exp = FALSE,
  individual = TRUE,
  outfile = "prediction_res_temporal2_500lasso.rds",
  dirnum = 500,
  filenames_file = "fingerprint_folds_temporal2.rds",
  pred_dir = here::here("data", "lily", "data", "fingerprint_res_temporal2", "500lasso"),
  force = TRUE,
  n_max = 13367,
  no_nzv_dat = TRUE,
  testdata_name = "all_grid_cells_temporal2.csv.gz"
)

