library(tidyverse)
library(tidymodels)
library(future)
library(furrr)
# plan(multicore, workers = 8)
source(here::here("code", "R", "utils.R"))
source(here::here("data", "lily", "code", "summary_fns.R"))
fold = NULL
rm(list = c("fold"))
if (!dir.exists(here::here("data", "lily", "data", "fingerprint_prediction_results"))) {
  dir.create(here::here(
    "data",
    "lily",
    "data",
    "fingerprint_prediction_results"
  ))
}

get_input = function(default = NA_real_){
  input = as.numeric(Sys.getenv("INPUT", unset = as.character(default)))
  print(paste0("input is: ", input))
  input
}

size = get_input()

out = paste0("prediction_res_subsets_", "13367wtd_", size, "_v4.rds")

get_summarized_predictions_full_subset_par(
  outfile = out,
  dirnum = size,
  pred_dir = paste(
    here::here("data", "lily", "data", "fingerprint_res_subset", paste0(size, "wtd_v4")),
    sep = "/"
  ),
  filenames_file = "fingerprint_folds.rds",
  exp = FALSE,
  force = FALSE
)

