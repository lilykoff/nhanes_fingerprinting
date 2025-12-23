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
             individual = FALSE,
             exp = FALSE,
             n_max = NULL)


dirnums = c(13367)

outfiles = paste("prediction_res_sc_", dirnums, ".rds", sep = "")

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
             exp = FALSE,
             n_max = 13367,
             no_nzv_dat = FALSE)
