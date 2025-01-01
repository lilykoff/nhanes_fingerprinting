library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = TRUE

filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds.rds"))
ifold = get_fold()
if (!is.na(ifold)) {
  fdf = filenames %>%
    filter(ceiling(fold / 10) == ifold)
}

if(!file.exists(here::here("data", "lily", "data", "fingerprint_res", paste0("fold_", ifold, "_1000_over_pct.rds"))) | force){
  xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells.csv.gz"))
  # xdf = readr::read_csv(here::here("data", "lily", "data", "sample_grid_cells.csv.gz"))

  df =
    xdf %>%
    filter(id %in% fdf$id)

  set.seed(123)
  initialsplit = initial_split(df, prop = 3/4, strata = id)

  data_train = training(initialsplit)
  data_test = testing(initialsplit)

  fit_model = function(subject, train, test, p) {
    train$class <- ifelse(train$id == subject, 1, 0)
    # oversampling
    n_id = nrow(train %>% filter(class == 1))
    n_other = nrow(train %>% filter(class != 1))

    # p = 0.5

    oversamp_factor = (p * n_other) / (n_id - (p * n_id))

    oversampled_train = train %>%
      filter(class == 1) %>%
      slice_sample(n = floor(n_id * oversamp_factor), replace = TRUE) %>%
      bind_rows(filter(train, class == 0))

    tmp <- oversampled_train %>% dplyr::select(-id)
    tmp_test <- test %>% dplyr::select(-id)
    mod <-
      glm(class ~ ., data = tmp, family = binomial(link = "logit"))
    pred <- predict.glm(mod, newdata = tmp_test, type = "response")
    return(pred)
  }

  # first we want to remove columns with near zero variance
  nzv_trans =
    recipe(id ~ ., data = data_train) %>%
    step_nzv(all_predictors())

  nzv_estimates = prep(nzv_trans)

  nzv = colnames(juice(nzv_estimates))
  dat_nzv = data_train %>% dplyr::select(id, all_of(nzv))
  dat_nzv_test = data_test %>% dplyr::select(id, all_of(nzv))

  ids = unique(df$id)
  ## now fit models, get predictions
  all_predictions =
    map(.x = c(0.1, 0.25, 0.5, 0.75, 0.9),
    # map(.x = c(0.2, 0.3),
        .f = function(p){
          map_dfc(
            .x = ids,
            .f = fit_model,
            train = dat_nzv,
            test = dat_nzv_test,
            p = p,
            .progress = T
          ) %>%
            janitor::clean_names()
        })

  res =
    all_predictions %>%
    map(.f = function(x){
      row_sums = rowSums(x)
      x =
        x %>%
        bind_cols(sum = row_sums) %>%
        rowwise() %>%
        mutate(across(starts_with("x"), ~ .x / sum)) %>%
        dplyr::select(-sum) %>%
        ungroup() %>%
        bind_cols(true_subject = dat_nzv_test$id)

      colnames(x) =
        c(paste("x", ids, sep = ""), "true_subject")
      x
    }) %>%
  bind_rows(.id = "pct_oversample")

  fname = paste0("fold_", ifold, "_1000_over_pct.rds")

  saveRDS(res, here::here("data", "lily", "data", "fingerprint_res", fname))


}


