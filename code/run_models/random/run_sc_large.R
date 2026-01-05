library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE
source(here::here("data", "lily", "code", "fit_functions.R"))

get_input = function(default = NA_real_){
  input = as.numeric(Sys.getenv("INPUT", unset = as.character(default)))
  print(paste0("input is: ", input))
  input
}


ifold = get_fold()
size = get_input()
# size = 15374
filenames = read_rds(here::here("data", "lily", "data", "folds_sc.rds"))



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
  if(length(ids) > 0){

    ids_all =
      filenames %>%
      filter(fold == f) %>%
      pull(id) %>%
      as.character()


    outfiles =
      here::here("data", "lily", "data", "fingerprint_res_sc", size, paste0(ids, ".rds"))

    if(!all(file.exists(outfiles)) || force) {

    dat_nzv = read_rds(here::here("data", "lily", "data", "dat_nzv_train_sc.rds")) %>%
      mutate(id = as.character(id)) %>%
      filter(id %in% ids_all)
    dat_nzv_test = read_rds(here::here("data", "lily", "data", "dat_nzv_test_sc.rds")) %>%
      mutate(id = as.character(id)) %>%
      filter(id %in% ids_all)



    for(id in ids){
      print(paste0("id = ", id, " num = ", i, " fold = ", f))
      i = i + 1
      outfile = here::here("data", "lily", "data", "fingerprint_res_sc", size, paste0(id, ".rds"))
      dir = dirname(outfile)
      if(!dir.exists(dir)){
        dir.create(dir, recursive = TRUE)
      }

      if(!file.exists(outfile) || force){
        x = try({
          preds = fit_model(subject = id, train = dat_nzv, test = dat_nzv_test)

          write_rds(preds, outfile, compress = "xz")
          rm(preds)
          gc()
        })
        rm(x)
      }
    }
    }
  }
}

