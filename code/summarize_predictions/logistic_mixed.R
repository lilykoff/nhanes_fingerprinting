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



outfiles = paste("prediction_res_mixedwtd_", 13367, ".rds", sep = "")
pred_dirs = paste(
  here::here("data", "lily", "data", "fingerprint_res"),
  "13367mixedwtd",
  sep = "/"
)


testdata_names = "dat_nzv_test_mixed"

purrr::pwalk(.l = list(outfile = outfiles,
                       dirnum = 13367,
                       pred_dir = pred_dirs,
                       testdata_name = testdata_names),
             .f = get_summarized_predictions_full,
             filenames_file = "fingerprint_folds.rds",
             individual = TRUE,
             no_nzv_dat = FALSE,
             exp = FALSE,
             n_max = 13367)

