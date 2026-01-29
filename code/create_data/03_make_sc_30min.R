library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))

filenames = read_rds(here::here("data", "lily", "data", "fingerprint_folds_30min.rds")) %>%
  mutate(id = as.character(id))

sc_preds = read_csv(here::here("data", "lily", "data", "all_grid_cells_sc.csv.gz")) %>%
  mutate(id = as.character(id))


xdf =
  sc_preds %>%
  filter(id %in% filenames$id)

rm(sc_preds)

outfiles = c(here::here("data", "lily", "data", "dat_nzv_train_30min_sc.rds"),
             here::here("data", "lily", "data", "dat_nzv_test_30min_sc.rds"))

if(!all(file.exists(outfiles))) {

  set.seed(123)
  initialsplit = initial_split(xdf, prop = 3/4, strata = id)

  data_train = training(initialsplit)
  data_test = testing(initialsplit)

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
