library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))

filenames = read_rds(here::here("data", "lily", "data", "fingerprint_folds_temporal_30min.rds")) %>%
  mutate(id = as.character(id))

sc_preds = read_csv(here::here("data", "lily", "data", "all_grid_cells_temporal_sc.csv.gz")) %>%
  mutate(id = as.character(id))


xdf =
  sc_preds %>%
  filter(id %in% filenames$id)

rm(sc_preds)

outfiles = c(here::here("data", "lily", "data", "dat_nzv_train_temporal_30min_sc.rds"),
             here::here("data", "lily", "data", "dat_nzv_test_temporal_30min_sc.rds"))

if(!all(file.exists(outfiles))) {

  data_train = xdf %>% filter(data == "train")
  data_test = xdf %>% filter(data == "test")

  # first we want to remove columns with near zero variance
  nzv_trans =
    recipe(id ~ ., data = data_train) %>%
    step_nzv(all_predictors())

  nzv_estimates = prep(nzv_trans)

  nzv = colnames(juice(nzv_estimates))
  dat_nzv = data_train %>% dplyr::select(id, all_of(nzv))
  dat_nzv_test = data_test %>% dplyr::select(id, all_of(nzv))

  write_rds(dat_nzv, outfiles[1], compress = "xz")
  write_rds(dat_nzv_test, outfiles[2], compress = "xz")

}
