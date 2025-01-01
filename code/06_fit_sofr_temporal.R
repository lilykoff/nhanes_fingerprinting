library(tidyverse)
library(refund)
library(mgcv)
library(tidyfun)
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




fit_model = function(subject, train, test) {
  pfr_fit =
    pfr(
      outcome ~ lf(mat, argvals = seq(1, 432)),
      family = binomial(link = "logit"),
      method = "REML", data = train)

  preds = exp(predict(pfr_fit, newdata = test, type = "link"))
  rm(prf_fit)
  return(preds)
}


fit_model_nl = function(subject, train, test) {
  pfr_fit =
    pfr(
      outcome ~ af(mat, argvals = seq(1, 432)),
      family = binomial(link = "logit"),
      method = "REML", data = train)

  preds = exp(predict(pfr_fit, newdata = test, type = "link"))
  rm(prf_fit)
  return(preds)
}

ifold = get_fold()
size = get_input()
filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds_temporal.rds"))

if(size < 500){
  if (!is.na(ifold) & !is.na(size)) {
    x = ceiling(nrow(filenames)/size)
    filenames = filenames %>%
      mutate(fold = rep(1:x, each = size)[1:nrow(filenames)])
    fdf = filenames %>%
      filter(fold == ifold)
  }
  outfile = here::here("data", "lily", "data", "fingerprint_res_temporal", paste0(size, "fnl"), paste0("fold_", ifold, ".rds"))
  dir = dirname(outfile)
  if(!file.exists(outfile) | force){
    if(!dir.exists(dir)){
      dir.create(dir, recursive = TRUE)
    }
    xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells_temporal.csv.gz"))

    df =
      xdf %>%
      filter(id %in% fdf$id)
    rm(xdf)
    set.seed(123)
    data_train =
      xdf %>% filter(data == "train")

    data_test=
      xdf %>% filter(data == "test")


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


    saveRDS(all_predictions, outfile)
  }

} else{
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
      outfile = here::here("data", "lily", "data", "fingerprint_res_temporal", paste0(size, "fnl"), paste0(id, ".rds"))
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


            df = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells_temporal.csv.gz")) %>%
              filter(id %in% ids_all)

            data_train =
              df %>%
              filter(data == "train") %>%
              select(-data)

            data_test =
              df %>%
              filter(data == "test") %>%
              select(-data)
            rm(df)

          preds = fit_model(subject = id, train = data_train, test = data_test)

          write_rds(preds, outfile, compress = "xz")
          rm(preds)
        })
        rm(x)
      }


    }
  }


}

