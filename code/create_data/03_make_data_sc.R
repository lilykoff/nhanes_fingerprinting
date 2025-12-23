library(tidyverse)
library(tidymodels)

force = FALSE

filenames = readRDS(here::here("data", "lily", "data", "folds_sc.rds"))

outfiles = c(here::here("data", "lily", "data", "dat_nzv_train_sc.rds"),
             here::here("data", "lily", "data", "dat_nzv_test_sc.rds"))

if(!all(file.exists(outfiles))) {
  if(!file.exists(here::here("data", "lily", "data", "all_grid_cells_sc.csv.gz"))) {
    files = list.files(here::here("data", "lily", "data", "grid_cell_data_sc"), recursive = TRUE,
                     full.names = TRUE)


    xdf =
      files %>%
      map(readr::read_csv) %>%
      bind_rows()

    readr::write_csv(xdf, here::here("data", "lily", "data", "all_grid_cells_sc.csv.gz"))
  } else {
    xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells_sc.csv.gz"))
  }

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
  rm(dat_nzv_test)
  rm(dat_nzv)
  rm(data_train)
  rm(data_test)
}


outfiles = c(here::here("data", "lily", "data", "dat_nzv_train_temporal_sc.rds"),
             here::here("data", "lily", "data", "dat_nzv_test_temporal_sc.rds"))

if(!all(file.exists(outfiles))) {

  xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells_temporal_sc.csv.gz"))
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
  rm(dat_nzv_test)
  rm(dat_nzv)
  rm(data_train)
  rm(data_test)
}
