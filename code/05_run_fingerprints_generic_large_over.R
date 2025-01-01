library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE
fac = c(0.1, 0.25, 0.5, 0.75, 0.9, 1.5, 10, 100)

# each one takes about 10 min and 20G (30 to be safe)
# 1024 gb per user = 34 jobs at once
# 13367 total = 92 days / 34 = ~ 3 days

get_input = function(default = NA_real_){
  input = as.numeric(Sys.getenv("INPUT", unset = as.character(default)))
  print(paste0("input is: ", input))
  input
}


fit_model = function(subject, train, test, p) {
  train$class = if_else(train$id == subject, 1, 0)
  # oversampling
  n_id = nrow(train %>% filter(class == 1))
  n_other = nrow(train %>% filter(class != 1))

  # p = 0.5
  if(p < 1){
    oversamp_factor = (p * n_other) / (n_id - (p * n_id))
  } else {oversamp_factor = p}

  oversampled_train = train %>%
    filter(class == 1) %>%
    slice_sample(n = floor(n_id * oversamp_factor), replace = TRUE) %>%
    bind_rows(filter(train, class == 0))

  tmp = oversampled_train %>% dplyr::select(-id)
  tmp_test = test %>% dplyr::select(-id)
  mod =
    glm(class ~ ., data = tmp, family = binomial(link = "logit"))
  pred = predict.glm(mod, newdata = tmp_test, type = "response")
  rm(mod) # remove model
  return(pred %>% janitor::clean_names())
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
  for(id in ids){
    print(paste0("id = ", id, " num = ", i, " fold = ", f))
    i = i + 1
    outfile = here::here("data", "lily", "data", "fingerprint_res", paste0(size, "over"), paste0(id, ".rds"))
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

        preds =
          map(.x = fac,
              .f = fit_model,
              subject = id,
              train = dat_nzv,
              test = dat_nzv_test)

        write_rds(preds, outfile, compress = "xz")
        rm(preds)
      })
      rm(x)
    }


  }
}

