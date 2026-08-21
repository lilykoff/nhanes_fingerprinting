## fit model on subsets of n = 1000, predict on entire dataset

library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE

get_input = function(default = NA_real_){
  input = as.numeric(Sys.getenv("INPUT", unset = as.character(default)))
  print(paste0("input is: ", input))
  input
}


fit_model_fast = function(subject, train, test) {
  # Create class vector directly (faster than ifelse)
  class <- as.integer(train$id == subject)

  # Calculate weights once
  n1 <- sum(class)
  n0 <- length(class) - n1
  wts <- ifelse(class == 1, 1/n1, 1/n0)

  # Remove id columns without dplyr (faster)
  train_idx <- which(names(train) != "id")
  test_idx <- which(names(test) != "id")

  # Fit model with matrix input if possible
  mod <- glm.fit(
    x = cbind(1, as.matrix(train[, train_idx])),
    y = class,
    weights = wts,
    family = binomial()
  )

  # Manual prediction (faster than predict.glm)
  test_matrix <- cbind(1, as.matrix(test[, test_idx]))
  pred <- plogis(test_matrix %*% mod$coefficients)


  return(as.vector(pred))
}

ifold = get_fold()
# ifold = 1
size = get_input()
# size = 100
filenames = read_rds(here::here("data", "lily", "data", "fingerprint_folds.rds"))



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
  count(fold)
  # filter(n == size)

# f = folds$fold[1]
for(f in folds$fold){
  i = 1
  if (!is.na(ifold)) {
    ids = filenames %>%
      filter(fold == f & fold2 == ifold) %>% pull(id)
  }

  if(length(ids) > 0) {


    dat_nzv = read_rds(here::here("data", "lily", "data", "dat_nzv_train.rds")) %>%
      mutate(id = as.character(id))
    dat_nzv_test = read_rds(here::here("data", "lily", "data", "dat_nzv_test.rds")) %>%
      mutate(id = as.character(id))



    for(idx in ids){
      print(paste0("id = ", idx, " num = ", i, " fold = ", f))
      i = i + 1
      outfile = here::here("data", "lily", "data", "fingerprint_res_subset", paste0(size, "wtd_v4"), paste0(idx, ".rds"))
      dir = dirname(outfile)
      if(!dir.exists(dir)){
        dir.create(dir, recursive = TRUE)
      }


      if(!file.exists(outfile) || force){
        x = try({

          set.seed(idx)
          ids_to_sample = filenames %>% filter(id != idx) %>% pull(id) %>% as.character()
          set.seed(idx)
          ids_temp = sample(ids_to_sample, size - 1, replace = FALSE)
          train_temp = dat_nzv %>%
            filter(id %in% c(idx, ids_temp))
          preds = fit_model_fast(subject = idx, train = train_temp, test = dat_nzv_test)

          write_rds(preds, outfile, compress = "xz")
          rm(preds)
        })
        rm(x)
      }


    }
  }
}

