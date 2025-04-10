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
  tmp <- train %>% dplyr::select(-id)
  tmp_test <- test %>% dplyr::select(-id)
  mod <-
    glm(class ~ ., data = tmp, family = binomial(link = "logit"))
  pred <- predict.glm(mod, newdata = tmp_test, type = "response")
  rm(mod)
  return(pred)
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
    outfile = here::here("data", "lily", "data", "fingerprint_res_temporal", size, paste0(id, ".rds"))
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

        if(size == 11225){
          dat_nzv = read_rds(here::here("data", "lily", "data", "dat_nzv_train_temporal.rds")) %>%
            mutate(id = as.character(id)) %>%
            filter(id %in% ids_all)
          dat_nzv_test = read_rds(here::here("data", "lily", "data", "dat_nzv_test_temporal.rds")) %>%
            mutate(id = as.character(id)) %>%
            filter(id %in% ids_all)
        } else {
          dat_nzv = read_rds(here::here("data", "lily", "data", paste0("dat_nzv_train_temporal_", size, "_", f, ".rds"))) %>%
            mutate(id = as.character(id)) %>%
            filter(id %in% ids_all)
          dat_nzv_test = read_rds(here::here("data", "lily", "data", paste0("dat_nzv_test_temporal_", size, "_", f, ".rds"))) %>%
            mutate(id = as.character(id)) %>%
            filter(id %in% ids_all)
        }


        preds = fit_model(subject = id, train = dat_nzv, test = dat_nzv_test) %>% janitor::clean_names()

        write_rds(preds, outfile, compress = "xz")
        rm(preds)
      })
      rm(x)
    }


  }
}

