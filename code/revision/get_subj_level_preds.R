### goal: generate a plot of accuracy vs. missingness
library(tidyverse)
source(here::here("data", "lily", "code", "summary_fns.R"))

filenames = read_rds(here::here("data", "lily", "data", "fingerprint_folds.rds"))

pred_dir = here::here("data", "lily", "data", "fingerprint_res", "100")

all_preds = list.files(
    pred_dir,
    recursive = TRUE,
    full.names = TRUE,
    pattern = "rds"
)

summary = map_dfr(
    .x = all_preds,
    .f = function(file) {
      x = read_rds(file)
      r = get_summarized_predictions(x, rank = TRUE)
      rm(x)
      r
    }
)

write_rds(summary, here::here("data", "lily", "data", "subj_level_preds.rds"))

