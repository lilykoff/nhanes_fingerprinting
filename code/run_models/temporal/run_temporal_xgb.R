library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = TRUE
n_cores = parallel::detectCores() - 1
if(!require("dials")) install.packages("dials")

filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds_temporal2.rds"))

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

fit_model = function(subject, train, test) {
  full_data = bind_rows(train, test)  # Combine the datasets
  full_data = full_data %>%
    mutate(label = factor(if_else(id == subject, 1, 0)))
  train_indices = 1:nrow(train)           # Indices for the training set
  test_indices = (nrow(train) + 1):nrow(full_data)
  # Manually create the initial_split object
  initialsplit = make_splits(
    x = list(analysis = train_indices, assessment = test_indices),
    data = full_data
  )

  train = training(initialsplit)

  nzv_trans =
    recipe(id ~ ., data = train) %>%
    step_nzv(all_predictors())

  nzv_estimates = prep(nzv_trans)

  nzv = colnames(juice(nzv_estimates))
  train = train %>% dplyr::select(label, id, all_of(nzv))

  cv_folds = train %>%
    rsample::vfold_cv(v = 5, strata = id)

  xgb_grid = dials::grid_space_filling(
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

  doParallel::registerDoParallel(cores = n_cores)
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
  rm(final_res); rm(xgb_res); rm(final_xgb)
  return(preds)
}

outfile_temporal = here::here("data", "lily", "data", "fingerprint_res_temporal2", paste0(size, "xgb"), paste0("fold_", ifold, ".rds"))
dir_temp = dirname(outfile_temporal)

if(!file.exists(outfile_temporal) | force){
  if(!dir.exists(dir_temp)){
    dir.create(dir_temp, recursive = TRUE)
  }
  xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells_temporal2.csv.gz"))

  df =
    xdf %>%
    filter(id %in% fdf$id)
  rm(xdf)
  data_train =
    df %>%
    filter(data == "train") %>%
    select(-data)

  data_test =
    df %>%
    filter(data == "test") %>%
    select(-data)

  ids = unique(df$id)
  ## now fit models, get predictions
  all_predictions =
    map_dfc(
      .x = ids,
      .f = fit_model,
      train = data_train,
      test = data_test,
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
    bind_cols(true_subject = data_test$id)

  colnames(all_predictions) =
    c(paste("x", ids, sep = ""), "true_subject")


  write_rds(all_predictions, outfile_temporal, compres = "xz")
}
