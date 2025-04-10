library(tidyverse)
library(tidymodels)

force = FALSE

xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells.csv.gz"))
# xdf = readr::read_csv(here::here("data", "lily", "data", "sample_grid_cells.csv.gz"))
filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds.rds"))

outfiles = c(here::here("data", "lily", "data", "dat_nzv_train.rds"),
                here::here("data", "lily", "data", "dat_nzv_test.rds"))

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
  rm(dat_nzv_test)
  rm(dat_nzv)
  rm(data_train)
  rm(data_test)
}

# make data for 2500, 5000, 10000
sizes = c(2500, 5000, 10000)
for(s in sizes){
  x = ceiling(nrow(filenames)/s)
  filenames = filenames %>%
    mutate(fold = rep(1:x, each = s)[1:nrow(filenames)])

  fsize = ceiling(nrow(filenames)/1000)
  x = ceiling(nrow(filenames)/fsize)
  filenames = filenames %>%
    mutate(fold2 = rep(1:x, each = fsize)[1:nrow(filenames)])
  folds = filenames %>%
    count(fold) %>%
    filter(n == s)
  for(f in folds$fold){
    outfiles = c(here::here("data", "lily", "data", paste0("dat_nzv_train_", s, "_", f, ".rds")),
                here::here("data", "lily", "data", paste0("dat_nzv_test_", s, "_", f, ".rds")))
    if(!all(file.exists(outfiles))){
      ids = filenames %>% filter(fold == f) %>% pull(id)
      xdf_temp = xdf %>% filter(id %in% ids)
      set.seed(123)
      initialsplit = initial_split(xdf_temp, prop = 3/4, strata = id)

      data_train = training(initialsplit)
      data_test = testing(initialsplit)

      nzv_trans =
        recipe(id ~ ., data = data_train) %>%
        step_nzv(all_predictors())

      nzv_estimates = prep(nzv_trans)

      nzv = colnames(juice(nzv_estimates))
      dat_nzv = data_train %>% dplyr::select(id, all_of(nzv))
      dat_nzv_test = data_test %>% dplyr::select(id, all_of(nzv))

      write_rds(dat_nzv, outfiles[1], compress = "xz")
      write_rds(dat_nzv_test, outfiles[2], compress = "xz")
      rm(xdf_temp)
      rm(data_train)
      rm(data_test)
      rm(dat_nzv)
      rm(dat_nzv_test)
    }
  }
}

rm(list = ls())

xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells_temporal.csv.gz"))
# xdf = readr::read_csv(here::here("data", "lily", "data", "sample_grid_cells_temporal.csv.gz"))
filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds_temporal.rds"))

outfiles = c(here::here("data", "lily", "data", "dat_nzv_train_temporal.rds"),
             here::here("data", "lily", "data", "dat_nzv_test_temporal.rds"))

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
  rm(dat_nzv_test)
  rm(dat_nzv)
  rm(data_train)
  rm(data_test)
}

# make data for 2500, 5000, 10000

sizes = c(2500, 5000, 10000)
for(s in sizes){
  x = ceiling(nrow(filenames)/s)
  filenames = filenames %>%
    mutate(fold = rep(1:x, each = s)[1:nrow(filenames)])

  fsize = ceiling(nrow(filenames)/1000)
  x = ceiling(nrow(filenames)/fsize)
  filenames = filenames %>%
    mutate(fold2 = rep(1:x, each = fsize)[1:nrow(filenames)])
  folds = filenames %>%
    count(fold) %>%
    filter(n == s)
  for(f in folds$fold){
    outfiles = c(here::here("data", "lily", "data", paste0("dat_nzv_train_temporal_", s, "_", f, ".rds")),
                 here::here("data", "lily", "data", paste0("dat_nzv_test_temporal_", s, "_", f, ".rds")))
    if(!all(file.exists(outfiles))){
      print(outfiles[1])
      ids = filenames %>% filter(fold == f) %>% pull(id)
      xdf_temp = xdf %>% filter(id %in% ids)

      data_train = xdf_temp %>% filter(data == "train")
      data_test = xdf_temp %>% filter(data == "test")

      nzv_trans =
        recipe(id ~ ., data = data_train) %>%
        step_nzv(all_predictors())

      nzv_estimates = prep(nzv_trans)

      nzv = colnames(juice(nzv_estimates))
      dat_nzv = data_train %>% dplyr::select(id, all_of(nzv))
      dat_nzv_test = data_test %>% dplyr::select(id, all_of(nzv))

      write_rds(dat_nzv, outfiles[1], compress = "xz")
      write_rds(dat_nzv_test, outfiles[2], compress = "xz")
      rm(xdf_temp)
      rm(data_train)
      rm(data_test)
      rm(dat_nzv)
      rm(dat_nzv_test)
    }
  }
}
