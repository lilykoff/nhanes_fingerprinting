library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE
n_cores = parallel::detectCores() - 1
# each one takes about 10 min and 20G (30 to be safe)
# 1024 gb per user = 34 jobs at once
# 13367 total = 92 days / 34 = ~ 3 days

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
filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds_temporal.rds"))



if (!is.na(size)) {
  x = ceiling(nrow(filenames)/size)
  filenames = filenames %>%
    mutate(fold = rep(1:x, each = size)[1:nrow(filenames)])
}


fsize = ceiling(nrow(filenames)/1000)
x = ceiling(nrow(filenames)/fsize)
filenames = filenames %>%
  mutate(fold2 = rep(1:x, each = fsize)[1:nrow(filenames)])

# max(filenames$fold2)

folds = filenames %>%
  count(fold) %>%
  filter(n == size)

# f = folds$fold[1]
for(f in folds$fold){
  i = 1
  if (!is.na(ifold)) {
    ids = filenames %>%
      filter(fold == f & fold2 == ifold) %>% pull(id)
  }
  for(id in ids){
    print(paste0("id = ", id, " num = ", i, " fold = ", f))
    i = i + 1
    outfile = here::here("data", "lily", "data", "fingerprint_res_temporal", paste0(size, "rf"), paste0(id, ".rds"))
    dir = dirname(outfile)
    if(!dir.exists(dir)){
      dir.create(dir, recursive = TRUE)
    }

    if(!file.exists(outfile) | force){
      x = try({

        ids_all =
          filenames %>%
          filter(fold == f) %>%
          pull(id) %>%
          as.character()

        if (size == 11225) {
          dat_nzv = read_rds(here::here("data", "lily", "data", "dat_nzv_train_temporal.rds")) %>%
            mutate(id = as.character(id)) %>%
            filter(id %in% ids_all)
          dat_nzv_test = read_rds(here::here("data", "lily", "data", "dat_nzv_test_temporal.rds")) %>%
            mutate(id = as.character(id)) %>%
            filter(id %in% ids_all)
        } else if(size > 1000) {
          dat_nzv = read_rds(here::here("data", "lily", "data", paste0("dat_nzv_train_temporal_", size, "_", f, ".rds"))) %>%
            mutate(id = as.character(id)) %>%
            filter(id %in% ids_all)
          dat_nzv_test = read_rds(here::here("data", "lily", "data", paste0("dat_nzv_test_temporal_", size, "_", f, ".rds"))) %>%
            mutate(id = as.character(id)) %>%
            filter(id %in% ids_all)
        } else {
          xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells_temporal.csv.gz"))

          df =
            xdf %>%
            filter(id %in% ids_all)
          rm(xdf)
          data_train =
            df %>%
            filter(data == "train") %>%
            select(-data)

          data_test =
            df %>%
            filter(data == "test") %>%
            select(-data)

          nzv_trans =
            recipe(id ~ ., data = data_train) %>%
            step_nzv(all_predictors())

          nzv_estimates = prep(nzv_trans)

          nzv = colnames(juice(nzv_estimates))
          dat_nzv = data_train %>% dplyr::select(id, all_of(nzv))
          dat_nzv_test = data_test %>% dplyr::select(id, all_of(nzv))
          rm(data_train); rm(data_test)
        }



        preds = fit_model(subject = id, train = dat_nzv, test = dat_nzv_test)

        write_rds(preds, outfile, compress = "xz")
        rm(preds)
      })
      rm(x)
    }


  }
}

