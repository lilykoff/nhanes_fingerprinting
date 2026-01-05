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
if (!dir.exists(here::here("data", "lily", "data", "fingerprint_prediction_results_sc"))) {
  dir.create(here::here(
    "data",
    "lily",
    "data",
    "fingerprint_prediction_results_sc"
  ))
}

##### ----- stepcount with 30 min of data ----- #####

# temporal, n = 100
dirnums = c(100)
outfiles = paste("prediction_res_temporal_sc_", dirnums, ".rds", sep = "")
pred_dirs = paste(
  here::here("data", "lily", "data", "fingerprint_res_temporal_sc"),
  dirnums,
  sep = "/"
)

purrr::pwalk(.l = list(outfile = outfiles,
                       dirnum = dirnums,
                       pred_dir = pred_dirs),
             .f = get_summarized_predictions_full,
             filenames_file = "folds_sc.rds",
             out_dir = here::here("data", "lily", "data", "fingerprint_prediction_results_sc"),
             individual = FALSE,
             exp = FALSE,
             n_max = NULL)

# random, n = 100
dirnums = c(100)
outfiles = paste("prediction_res_sc_", dirnums, ".rds", sep = "")
pred_dirs = paste(
  here::here("data", "lily", "data", "fingerprint_res_sc"),
  dirnums,
  sep = "/"
)

purrr::pwalk(.l = list(outfile = outfiles,
                       dirnum = dirnums,
                       pred_dir = pred_dirs),
             .f = get_summarized_predictions_full,
             filenames_file = "folds_sc.rds",
             out_dir = here::here("data", "lily", "data", "fingerprint_prediction_results_sc"),
             individual = FALSE,
             exp = FALSE,
             out_dir = here::here("data", "lily", "data", "fingerprint_prediction_results_sc"),
             n_max = NULL)

# random, n = 15374
dirnums = c(15374)

outfiles = paste("prediction_res_sc_", dirnums, ".rds", sep = "")

pred_dirs = paste(
  here::here("data", "lily", "data", "fingerprint_res_sc"),
  dirnums,
  sep = "/"
)

testdata_names = "dat_nzv_test_sc"
purrr::pwalk(.l = list(outfile = outfiles,
                       dirnum = dirnums,
                       pred_dir = pred_dirs,
                       testdata_name = testdata_names),
             .f = get_summarized_predictions_full,
             filenames_file = "folds_sc.rds",
             individual = TRUE,
             out_dir = here::here("data", "lily", "data", "fingerprint_prediction_results_sc"),
             exp = FALSE,
             n_max = 15374,
             no_nzv_dat = FALSE)

# random, n = 15374, wtd
dirnums = c(15374)

outfiles = paste("prediction_res_sc_", paste0(dirnums, "wtd"), ".rds", sep = "")

pred_dirs = paste(
  here::here("data", "lily", "data", "fingerprint_res_sc"),
  paste0(dirnums, "wtd"),
  sep = "/"
)

testdata_names = "dat_nzv_test_sc"
purrr::pwalk(.l = list(outfile = outfiles,
                       dirnum = dirnums,
                       pred_dir = pred_dirs,
                       testdata_name = testdata_names),
             .f = get_summarized_predictions_full,
             filenames_file = "folds_sc.rds",
             individual = TRUE,
             out_dir = here::here("data", "lily", "data", "fingerprint_prediction_results_sc"),
             exp = FALSE,
             n_max = 15374,
             no_nzv_dat = FALSE)


# temporal, n = 15374
dirnums = c(15374)

outfiles = paste("prediction_res_temporal_sc_", dirnums, ".rds", sep = "")

pred_dirs = paste(
  here::here("data", "lily", "data", "fingerprint_res_temporal_sc"),
  dirnums,
  sep = "/"
)

testdata_names = "dat_nzv_test_temporal_sc"
purrr::pwalk(.l = list(outfile = outfiles,
                       dirnum = dirnums,
                       pred_dir = pred_dirs,
                       testdata_name = testdata_names),
             .f = get_summarized_predictions_full,
             filenames_file = "folds_sc.rds",
             individual = TRUE,
             out_dir = here::here("data", "lily", "data", "fingerprint_prediction_results_sc"),
             exp = FALSE,
             n_max = 15374,
             no_nzv_dat = FALSE)

# temporal, n = 15374, wtd
dirnums = c(15374)

outfiles = paste("prediction_res_temporal_sc_", paste0(dirnums, "wtd"), ".rds", sep = "")

pred_dirs = paste(
  here::here("data", "lily", "data", "fingerprint_res_temporal_sc"),
  paste0(dirnums, "wtd"),
  sep = "/"
)

testdata_names = "dat_nzv_test_temporal_sc"
purrr::pwalk(.l = list(outfile = outfiles,
                       dirnum = dirnums,
                       pred_dir = pred_dirs,
                       testdata_name = testdata_names),
             .f = get_summarized_predictions_full,
             filenames_file = "folds_sc.rds",
             individual = TRUE,
             out_dir = here::here("data", "lily", "data", "fingerprint_prediction_results_sc"),
             exp = FALSE,
             n_max = 15374,
             no_nzv_dat = FALSE)

#### --- stepcount with 3 min of data ---- ####
dirnums = c(13367)

outfiles = paste("prediction_res_scs_", dirnums, ".rds", sep = "")

pred_dirs = paste(
  here::here("data", "lily", "data", "fingerprint_res_scs"),
  dirnums,
  sep = "/"
)

testdata_names = "dat_nzv_test_scs"
purrr::pwalk(.l = list(outfile = outfiles,
                       dirnum = dirnums,
                       pred_dir = pred_dirs,
                       testdata_name = testdata_names),
             .f = get_summarized_predictions_full,
             filenames_file = "fingerprint_folds.rds",
             individual = TRUE,
             out_dir = here::here("data", "lily", "data", "fingerprint_prediction_results_sc"),
             exp = FALSE,
             n_max = 13367,
             no_nzv_dat = FALSE)

# stepcount with 3 min of data, random, wtd
dirnums = c(13367)

outfiles = paste("prediction_res_scs_", paste0(dirnums, "wtd"), ".rds", sep = "")

pred_dirs = paste(
  here::here("data", "lily", "data", "fingerprint_res_scs"),
  paste0(dirnums, "wtd"),
  sep = "/"
)

testdata_names = "dat_nzv_test_scs"
purrr::pwalk(.l = list(outfile = outfiles,
                       dirnum = dirnums,
                       pred_dir = pred_dirs,
                       testdata_name = testdata_names),
             .f = get_summarized_predictions_full,
             filenames_file = "fingerprint_folds.rds",
             individual = TRUE,
             out_dir = here::here("data", "lily", "data", "fingerprint_prediction_results_sc"),
             exp = FALSE,
             n_max = 13367,
             no_nzv_dat = FALSE)

# temporal
dirnums = c(10770)

outfiles = paste("prediction_res_temporal_scs_", dirnums, ".rds", sep = "")

pred_dirs = paste(
  here::here("data", "lily", "data", "fingerprint_res_temporal_scs"),
  dirnums,
  sep = "/"
)

testdata_names = "dat_nzv_test_temporal_scs"
purrr::pwalk(.l = list(outfile = outfiles,
                       dirnum = dirnums,
                       pred_dir = pred_dirs,
                       testdata_name = testdata_names),
             .f = get_summarized_predictions_full,
             filenames_file = "fingerprint_folds_temporal2.rds",
             individual = TRUE,
             out_dir = here::here("data", "lily", "data", "fingerprint_prediction_results_sc"),
             exp = FALSE,
             n_max = 10770,
             no_nzv_dat = FALSE)

# stepcount with 3 min of data, random, wtd


outfiles = paste("prediction_res_temporal_scs_", paste0(dirnums, "wtd"), ".rds", sep = "")

pred_dirs = paste(
  here::here("data", "lily", "data", "fingerprint_res_temporal_scs"),
  paste0(dirnums, "wtd"),
  sep = "/"
)

testdata_names = "dat_nzv_test_temporal_scs"
purrr::pwalk(.l = list(outfile = outfiles,
                       dirnum = dirnums,
                       pred_dir = pred_dirs,
                       testdata_name = testdata_names),
             .f = get_summarized_predictions_full,
             filenames_file = "fingerprint_folds_temporal2.rds",
             individual = TRUE,
             out_dir = here::here("data", "lily", "data", "fingerprint_prediction_results_sc"),
             exp = FALSE,
             n_max = 10770,
             no_nzv_dat = FALSE)
