library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE

filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds.rds"))

get_input = function(default = NA_real_){
  input = as.numeric(Sys.getenv("INPUT", unset = as.character(default)))
  print(paste0("input is: ", input))
  input
}

ifold = get_fold()
size = get_input()



if (!is.na(ifold) & !is.na(size)) {
  x = ceiling(nrow(filenames)/size)
  filenames = filenames %>%
    mutate(fold = rep(1:x, each = size)[1:nrow(filenames)])
  fdf = filenames %>%
    filter(fold == ifold)
}


fit_model = function(subject, train, test) {
  train$class <- ifelse(train$id == subject, 1, 0)
  tmp <- train %>% dplyr::select(-id)
  tmp_test <- test %>% dplyr::select(-id)
  mod <-
    glm(class ~ ., data = tmp, family = binomial(link = "logit"))
  pred <- predict.glm(mod, newdata = tmp_test, type = "response")
  rm(mod)
  return(pred)
}

fit_model_trainprop = function(df, prop_train){
  ids_test = unique(df$id)
  set.seed(123)
  ids_train = sample(ids_test, size = length(ids_test) * prop_train, replace = FALSE)
  set.seed(123)
  initialsplit = initial_split(df, prop = 3/4, strata = id)

  # subject is in training but not in testing
  data_train = training(initialsplit) %>%
    filter(id %in% ids_train)
  data_test = testing(initialsplit)

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
    mutate(proptrain = prop_train)

}


outfile = here::here("data", "lily", "data", "fingerprint_res", paste0(size, "tnt"), paste0("fold_", ifold, ".rds"))
dir = dirname(outfile)
if(!file.exists(outfile) | force){
  if(!dir.exists(dir)){
    dir.create(dir, recursive = TRUE)
  }
  xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells.csv.gz"))
  # xdf = readr::read_csv(here::here("data", "lily", "data", "sample_grid_cells.csv.gz"))

  df =
    xdf %>%
    filter(id %in% fdf$id)

  res = map_dfr(.x = c(0.1, 0.25, 0.5, 0.75, 0.9),
                .f = fit_model_trainprop,
                df = df)

  write_rds(res, outfile, compress = "xz")
}

