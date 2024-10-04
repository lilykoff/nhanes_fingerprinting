library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE

filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds.rds"))
ifold = get_fold()
if (!is.na(ifold)) {
  fdf = filenames %>%
    filter(fold == ifold)
}

if(!file.exists(here::here("data", "lily", "data", "fingerprint_res", paste0("fold_", ifold, ".rds"))) | force){
  xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells.csv.gz"))

  df =
    xdf %>%
    filter(id %in% fdf$id)

  set.seed(123)
  initialsplit = initial_split(df, prop = 3/4, strata = id)

  data_train = training(initialsplit)
  data_test = testing(initialsplit)

  fit_model = function(subject, train, test) {
    train$class <- ifelse(train$id == subject, 1, 0)
    test$class <- ifelse(test$id == subject, 1, 0)
    tmp <- train %>% dplyr::select(-id)
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
    map_dfc(
      .x = ids,
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
    c(paste("x", ids, sep = ""), "true_subject")

  fname = paste0("fold_", ifold, ".rds")

  saveRDS(all_predictions, here::here("data", "lily", "data", "fingerprint_res", fname))
}
