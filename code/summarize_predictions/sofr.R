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


dirnums = 100
outfiles = paste("prediction_res_", paste0(dirnums,  c("fnl", "nlfnl")), ".rds", sep = "")
pred_dirs = paste(paste(
  here::here("data", "lily", "data", "fingerprint_res/"),
  sep = "/"), dirnums, c("fnl", "nlfnl"), sep = "")

purrr::pwalk(.l = list(outfile = outfiles,
                       dirnum = dirnums,
                       pred_dir = pred_dirs),
             .f = get_summarized_predictions_full,
             filenames_file = "fingerprint_folds.rds",
             individual = FALSE,
             exp = TRUE)

dirnums = 100
outfiles = paste("prediction_res_temporal2_", paste0(dirnums,  c("fnl", "nlfnl")), ".rds", sep = "")
pred_dirs = paste(paste(
  here::here("data", "lily", "data", "fingerprint_res_temporal2/"),
  sep = "/"), dirnums, c("fnl", "nlfnl"), sep = "")

purrr::pwalk(.l = list(outfile = outfiles,
                       dirnum = dirnums,
                       pred_dir = pred_dirs),
             .f = get_summarized_predictions_full,
             filenames_file = "fingerprint_folds_temporal2.rds",
             individual = FALSE,
             exp = TRUE)

dirnums = 500
outfiles = paste("prediction_res_", paste0(dirnums,  c("fnl", "nlfnl")), ".rds", sep = "")
pred_dirs = paste(paste(
  here::here("data", "lily", "data", "fingerprint_res/"),
  sep = "/"), dirnums, c("fnl", "nlfnl"), sep = "")

purrr::pwalk(.l = list(outfile = outfiles,
                       dirnum = dirnums,
                       pred_dir = pred_dirs),
             .f = get_summarized_predictions_full,
             filenames_file = "fingerprint_folds.rds",
             individual = TRUE,
             n_max = 13367,
             no_nzv_dat = TRUE,
             testdata_name = "all_grid_cells.csv.gz",
             exp = TRUE)

dirnums = 1000

purrr::pwalk(.l = list(outfile = outfiles,
                       dirnum = dirnums,
                       pred_dir = pred_dirs),
             .f = get_summarized_predictions_full,
             filenames_file = "fingerprint_folds.rds",
             individual = TRUE,
             n_max = 13367,
             no_nzv_dat = TRUE,
             testdata_name = "all_grid_cells.csv.gz",
             exp = TRUE)

dirnums = 500
outfiles = paste("prediction_res_temporal2_", paste0(dirnums,  c("fnl", "nlfnl")), ".rds", sep = "")
pred_dirs = paste(paste(
  here::here("data", "lily", "data", "fingerprint_res_temporal2/"),
  sep = "/"), dirnums, c("fnl", "nlfnl"), sep = "")

purrr::pwalk(.l = list(outfile = outfiles,
                       dirnum = dirnums,
                       pred_dir = pred_dirs),
             .f = get_summarized_predictions_full,
             filenames_file = "fingerprint_folds_temporal2.rds",
             individual = TRUE,
             n_max = 13367,
             no_nzv_dat = TRUE,
             testdata_name = "all_grid_cells_temporal2.csv.gz",
             exp = TRUE)

dirnums = 1000


purrr::pwalk(.l = list(outfile = outfiles,
                       dirnum = dirnums,
                       pred_dir = pred_dirs),
             .f = get_summarized_predictions_full,
             filenames_file = "fingerprint_folds_temporal2.rds",
             individual = TRUE,
             n_max = 13367,
             no_nzv_dat = TRUE,
             testdata_name = "all_grid_cells_temporal2.csv.gz",
             exp = TRUE)
