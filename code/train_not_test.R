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


fit_model = function(subject, train, test) {
  train$class <- ifelse(train$id == subject, 1, 0)
  # oversampling
  n_id = nrow(train %>% filter(class == 1))

  oversampled_train = train %>%
    filter(class == 1) %>%
    slice_sample(n = n_id * 100, replace = TRUE) %>%
    bind_rows(filter(train, class == 0))

  tmp = oversampled_train %>% dplyr::select(-id)
  tmp_test = test %>% dplyr::select(-id)
  mod =
    glm(class ~ ., data = tmp, family = binomial(link = "logit"))
  pred <- predict.glm(mod, newdata = tmp_test, type = "response")
  return(pred)
}

if(!dir.exists(here::here("data", "lily", "data", "fingerprint_res", "train_not_test"))){
  dir.create(here::here("data", "lily", "data", "fingerprint_res", "train_not_test"),
             recursive = TRUE)
}

if(!file.exists(here::here("data", "lily", "data", "fingerprint_res", "train_not_test", paste0("fold_", ifold, "_1000.rds"))) | force){
  xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells.csv.gz"))
  # xdf = readr::read_csv(here::here("data", "lily", "data", "sample_grid_cells.csv.gz"))
  ids = unique(fdf$id)
  props = seq(0.1, 0.9, 0.1)

  # for now just take 10 subs
  df =
    xdf %>%
    filter(id %in% fdf$id)
  ids = unique(df$id)

  fit_model_testprop = function(df, prop_test){
    ids_train = unique(df$id)
    set.seed(123)
    ids_test = sample(ids_train, size = length(ids_train) * prop_test, replace = FALSE)
    set.seed(123)
    initialsplit = initial_split(df, prop = 3/4, strata = id)

    # subject is in training but not in testing
    data_train = training(initialsplit)
    data_test = testing(initialsplit) %>%
      filter(id %in% ids_test)

    # first we want to remove columns with near zero variance
    nzv_trans =
      recipe(id ~ ., data = data_train) %>%
      step_nzv(all_predictors())

    nzv_estimates = prep(nzv_trans)

    nzv = colnames(juice(nzv_estimates))
    dat_nzv = data_train %>% dplyr::select(id, all_of(nzv))
    dat_nzv_test = data_test %>% dplyr::select(id, all_of(nzv))

    ## now fit models, get predictions
    all_predictions =
      map_dfc(
        .x = ids_train,
        .f = fit_model,
        train = dat_nzv,
        test = dat_nzv_test,
        .progress = T
      ) %>%
      janitor::clean_names()

    # column j is predicted probability that data in that row belong to subject j
    # normalize probabilities
    row_sums = rowSums(all_predictions)

    # normalize and add "true subject column"
    all_predictions =
      all_predictions %>%
      bind_cols(sum = row_sums) %>%
      rowwise() %>%
      mutate(across(starts_with("x"), ~ .x / sum)) %>%
      dplyr::select(-sum) %>%
      ungroup() %>%
      bind_cols(true_subject = dat_nzv_test$id)

    colnames(all_predictions) =
      c(paste("x", ids_train, sep = ""), "true_subject")

    all_predictions %>%
      group_by(true_subject) %>%
      mutate(sec = row_number()) %>%
      pivot_longer(cols = -c("true_subject", "sec")) %>%
      mutate(model = as.numeric(sub(".*x", "", name))) %>%
      rename(pred = value) %>%
      ungroup() %>%
      group_by(true_subject, model) %>%
      summarize(mean_pred = mean(pred, na.rm = TRUE)) %>%
      mutate(proptest = prop_test)

  }

  res = map_dfr(.x = props,
                .f = fit_model_testprop,
                df = df)


  fname = paste0("fold_", ifold, "_1000.rds")

  saveRDS(res, here::here("data", "lily", "data", "fingerprint_res", "train_not_test", fname))
}
