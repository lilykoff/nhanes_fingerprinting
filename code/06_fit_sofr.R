library(tidyverse)
library(tidymodels)

library(refund)
library(mgcv)
library(tidyfun)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE
force2 = TRUE
n_cores = parallel::detectCores() - 1
# each one takes about 10 min and 20G (30 to be safe)
# 1024 gb per user = 34 jobs at once
# 13367 total = 92 days / 34 = ~ 3 days

get_input = function(default = NA_real_){
  input = as.numeric(Sys.getenv("INPUT", unset = as.character(default)))
  print(paste0("input is: ", input))
  input
}



fit_model = function(subject, train, test, linear = TRUE) {
  df_mat =
    train %>% select(starts_with("x")) %>%
    as.matrix()
  xdf =
    train %>%
    mutate(outcome = if_else(id == subject, 1, 0)) %>%
    select(-starts_with("x")) %>%
    mutate(mat = df_mat)

  df_mat_test =
    test %>% select(starts_with("x")) %>%
    as.matrix()

  xdf_test =
    test %>%
    select(-starts_with("x")) %>%
    mutate(mat = df_mat_test)
  rm(train); rm(test); rm(df_mat_test); rm(df_mat)
  if(linear){
    pfr_fit =
      pfr(
        outcome ~ lf(mat, argvals = seq(1, 432)),
        family = binomial(link = "logit"),
        method = "REML", data = xdf)
  } else {
    pfr_fit =
      pfr(
        outcome ~ af(mat, argvals = seq(1, 432)),
        family = binomial(link = "logit"),
        method = "REML", data = xdf)
  }


  preds = exp(predict(pfr_fit, newdata = xdf_test, type = "link")) %>% as.numeric()
  rm(pfr_fit)
  return(preds)
}

ifold = get_fold()
size = get_input()
filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds.rds"))

if(size < 500){
  if (!is.na(ifold) & !is.na(size)) {
    x = ceiling(nrow(filenames)/size)
    filenames = filenames %>%
      mutate(fold = rep(1:x, each = size)[1:nrow(filenames)])
    fdf = filenames %>%
      filter(fold == ifold)
  }
  outfile1 = here::here("data", "lily", "data", "fingerprint_res", paste0(size, "fnl"), paste0("fold_", ifold, ".rds"))
  outfile2 = here::here("data", "lily", "data", "fingerprint_res", paste0(size, "nlfnl"), paste0("fold_", ifold, ".rds"))

  dir1 = dirname(outfile1)
  dir2 = dirname(outfile2)

  if (!dir.exists(dir1)) {
    dir.create(dir1, recursive = TRUE)
  }
  if (!dir.exists(dir2)) {
    dir.create(dir2, recursive = TRUE)
  }
  xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells.csv.gz"))
  # xdf = readr::read_csv(here::here("data", "sample_grid_cells.csv.gz"))

  df =
    xdf %>%
    filter(id %in% fdf$id)
  rm(xdf)

  set.seed(123)
  initialsplit = initial_split(df, prop = 3/4, strata = id)

  data_train = training(initialsplit)
  data_test = testing(initialsplit)

  ids = unique(df$id)
  if(!file.exists(outfile1) || force){

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


    saveRDS(all_predictions, outfile1)
    rm(all_predictions)
  }
  if(!file.exists(outfile2) || force2){

    ## now fit models, get predictions
    all_predictions =
      map_dfc(
        .x = ids,
        .f = fit_model,
        train = data_train,
        test = data_test,
        linear = FALSE,
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


    saveRDS(all_predictions, outfile2)
    rm(all_predictions)
  }
} else {
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
      outfile1 = here::here("data", "lily", "data", "fingerprint_res", paste0(size, "fnl"), paste0(id, ".rds"))
      outfile2 = here::here("data", "lily", "data", "fingerprint_res", paste0(size, "nlfnl"), paste0(id, ".rds"))

      dir1 = dirname(outfile1)
      dir2 = dirname(outfile2)
      if(!dir.exists(dir1)){
        dir.create(dir1, recursive = TRUE)
      }
      if(!dir.exists(dir2)){
        dir.create(dir2, recursive = TRUE)
      }

      ids_all =
        filenames %>%
        filter(fold == f) %>%
        pull(id) %>%
        as.character()


      df = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells.csv.gz")) %>%
        filter(id %in% ids_all)

      set.seed(123)
      initialsplit = initial_split(df, prop = 3/4, strata = id)

      data_train = training(initialsplit)
      data_test = testing(initialsplit)

      rm(df)

      if(!file.exists(outfile1) || force){
        x = try({

          preds = fit_model(subject = id, train = data_train, test = data_test, linear = TRUE)

          write_rds(preds, outfile1, compress = "xz")
          rm(preds)
        })
        rm(x)
      }

      if(!file.exists(outfile2) || force2){
        x = try({

          preds = fit_model(subject = id, train = data_train, test = data_test, linear = FALSE)

          write_rds(preds, outfile2, compress = "xz")
          rm(preds)
        })
        rm(x)
      }
    }
  }
}


