library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE
# each one takes about 10 min and 20G (30 to be safe)
# 1024 gb per user = 34 jobs at once
# 13367 total = 92 days / 34 = ~ 3 days

get_input = function(default = NA_real_){
  input = as.numeric(Sys.getenv("INPUT", unset = as.character(default)))
  print(paste0("input is: ", input))
  input
}

fit_model = function(subject, train, test) {
  train$class <- ifelse(train$id == subject, 1, 0)
  class_counts <- table(train$class)
  wts <- ifelse(train$class == 1,
                1 / class_counts["1"],
                1 / class_counts["0"])

  tmp <- train %>% dplyr::select(-id)
  tmp_test <- test %>% dplyr::select(-id)
  mod <-
    glm(class ~ ., data = tmp, weights = wts, family = binomial(link = "logit"))
  pred <- predict.glm(mod, newdata = tmp_test, type = "response")
  return(pred)
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
size = get_input()
filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds.rds"))



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

  if(length(ids) > 0) {


    ids_all =
      filenames %>%
      filter(fold == f) %>%
      pull(id) %>%
      as.character()

    if(size == 13367){
      dat_nzv = read_rds(here::here("data", "lily", "data", "dat_nzv_train.rds")) %>%
        mutate(id = as.character(id)) %>%
        filter(id %in% ids_all)
      dat_nzv_test = read_rds(here::here("data", "lily", "data", "dat_nzv_test.rds")) %>%
        mutate(id = as.character(id)) %>%
        filter(id %in% ids_all)
    } else {
      dat_nzv = read_rds(here::here("data", "lily", "data", paste0("dat_nzv_train_", size, "_", f, ".rds"))) %>%
        mutate(id = as.character(id)) %>%
        filter(id %in% ids_all)
      dat_nzv_test = read_rds(here::here("data", "lily", "data", paste0("dat_nzv_test_", size, "_", f, ".rds"))) %>%
        mutate(id = as.character(id)) %>%
        filter(id %in% ids_all)
    }

    for(id in ids){
      print(paste0("id = ", id, " num = ", i, " fold = ", f))
      i = i + 1
      outfile = here::here("data", "lily", "data", "fingerprint_res", paste0(size, "wtd"), paste0(id, ".rds"))
      dir = dirname(outfile)
      if(!dir.exists(dir)){
        dir.create(dir, recursive = TRUE)
      }

      if(!file.exists(outfile) | force){
        x = try({
          preds = fit_model_fast(subject = id, train = dat_nzv, test = dat_nzv_test) %>% janitor::clean_names()
          write_rds(preds, outfile, compress = "xz")
          rm(preds)
        })
        rm(x)
      }


    }
  }
}

