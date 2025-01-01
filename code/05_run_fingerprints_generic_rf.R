library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE
n_cores = parallel::detectCores() - 1

filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds.rds"))

get_input = function(default = NA_real_){
  input = as.numeric(Sys.getenv("INPUT", unset = as.character(default)))
  print(paste0("input is: ", input))
  input
}


rf_spec = rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 1000
) %>%
  set_engine("ranger") %>%
  set_mode("classification")


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

  rf_grid = grid_space_filling(
    finalize(mtry(), train),
    min_n(),
    size = 30
  )

  rf_wf = workflow() %>%
    add_variables(outcomes = label,
                  predictors = starts_with("x")) %>%
    add_model(rf_spec)

  doParallel::registerDoParallel(cores = n_cores)
  set.seed(234)
  rf_res = tune_grid(
    rf_wf,
    resamples = cv_folds,
    grid = rf_grid,
    control = control_grid(save_pred = FALSE)
  )

  best_acc = select_best(rf_res, metric = "roc_auc")

  final_rf = finalize_workflow(rf_wf, best_acc)

  final_res = last_fit(final_rf, initialsplit)

  preds = final_res %>%
    collect_predictions() %>%
    pull(.pred_1)
  rm(final_res); rm(rf_res); rm(final_rf)
  return(preds)
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



outfile = here::here("data", "lily", "data", "fingerprint_res", paste0(size, "rf"), paste0("fold_", ifold, ".rds"))
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

  set.seed(123)
  is = initial_split(df, prop = 3/4, strata = id)
  test = testing(is)


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


  saveRDS(all_predictions, outfile)
}

