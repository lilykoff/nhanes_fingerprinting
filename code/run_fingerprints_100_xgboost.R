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
    filter(fold == ifold)
}

# model spec
xgb_spec = boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  ## first three: model complexity
  sample_size = tune(),
  mtry = tune(),
  ## randomness
  learn_rate = tune()) %>%                           ## step size)
  set_engine("xgboost") %>%
  set_mode("classification")



if(!dir.exists(here::here("data", "lily", "data", "fingerprint_res_xgboost"))){
  dir.create(here::here("data", "lily", "data", "fingerprint_res_xgboost"), recursive = TRUE)
}

if(!file.exists(here::here("data", "lily", "data", "fingerprint_res_xgboost", paste0("fold_", ifold, "_100.rds"))) | force){
  xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells.csv.gz"))
  # xdf = readr::read_csv(here::here("data", "lily", "data", "sample_grid_cells.csv.gz"))

  df =
    xdf %>%
    filter(id %in% fdf$id)
  set.seed(123)
  is = initial_split(df, prop = 3/4, strata = id)
  test = testing(is)

  fit_model = function(subject, df) {
    df = df %>%
      mutate(label = factor(if_else(id == subject, 1, 0)))
    set.seed(123)
    initialsplit = initial_split(df, prop = 3/4, strata = id)
    train = training(initialsplit)
    nzv_trans =
      recipe(id ~ ., data = train) %>%
      step_nzv(all_predictors())

    nzv_estimates = prep(nzv_trans)

    nzv = colnames(juice(nzv_estimates))
    train = train %>% dplyr::select(label, id, all_of(nzv))

    cv_folds = train %>%
      rsample::vfold_cv(v = 5, strata = id)

    xgb_grid = grid_space_filling(
      tree_depth(),
      min_n(),
      loss_reduction(),
      sample_size = sample_prop(),
      finalize(mtry(), train),
      learn_rate(),
      size = 30 # CHANGE LATER
    )

    xgb_wf = workflow() %>%
      add_variables(outcomes = label,
                    predictors = starts_with("x")) %>%
      add_model(xgb_spec)

    # doParallel::registerDoParallel()
    set.seed(234)
    xgb_res = tune_grid(
      xgb_wf,
      resamples = cv_folds,
      grid = xgb_grid,
      control = control_grid(save_pred = FALSE)
    )

    best_acc = select_best(xgb_res, metric = "roc_auc")

    final_xgb = finalize_workflow(xgb_wf, best_acc)

    final_res = last_fit(final_xgb, initialsplit)

    preds = final_res %>%
      collect_predictions() %>%
      pull(.pred_1)

    return(preds)
  }



  ids = unique(df$id)
  ## now fit models, get predictions
  all_predictions =
    map_dfc(
      .x = ids,
      .f = fit_model,
      df = df,
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
    bind_cols(true_subject = test$id)

  colnames(all_predictions) =
    c(paste("x", ids, sep = ""), "true_subject")

  fname = paste0("fold_", ifold, "_100.rds")

  saveRDS(all_predictions, here::here("data", "lily", "data", "fingerprint_res_xgboost", fname))
}
