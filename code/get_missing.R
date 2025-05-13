library(tidyverse)

get_missing = function(size, name, individual = FALSE, temporal = FALSE){
  if(temporal){
    results_path = here::here("data", "lily", "data", "fingerprint_res_temporal2", paste0(size, name))
    fname_path = here::here("data", "lily", "data", "fingerprint_folds_temporal2.rds")
  } else {
    results_path = here::here("data", "lily", "data", "fingerprint_res", paste0(size, name))
    fname_path = here::here("data", "lily", "data", "fingerprint_folds.rds")
  }
  filenames = readRDS(fname_path)

  if(individual){
    x = ceiling(nrow(filenames) / size)
    filenames = filenames %>%
      mutate(fold = rep(1:x, each = size)[1:nrow(filenames)])

    fsize = ceiling(nrow(filenames)/1000)
    x = ceiling(nrow(filenames)/fsize)
    filenames = filenames %>%
      mutate(fold2 = rep(1:x, each = fsize)[1:nrow(filenames)])

    folds = filenames %>%
      count(fold) %>%
      filter(n == size)

    filenames = filenames %>%
      filter(fold %in% folds$fold)

    files = list.files(results_path)
    ids = sub(".rds.*","",files)

    missing = filenames %>%
      mutate(id = as.character(id)) %>%
      mutate(miss = !(id %in% ids))


    x = missing %>%
      filter(miss) %>%
      select(fold2) %>%
      unlist()

    x %>% unique() %>% unname() %>% paste(., collapse = ",")

  } else {
    filenames = readRDS(fname_path)

    x = ceiling(nrow(filenames) / size)
    filenames = filenames %>%
      mutate(fold = rep(1:x, each = size)[1:nrow(filenames)])

    folds =
      filenames %>%
      count(fold) %>%
      filter(n == size)
    files = list.files(results_path)
    ids = sub(".*fold\\_(.+)\\.rds.*", "\\1", files)

    missing = filenames %>%
      filter(fold %in% folds$fold) %>%
      mutate(fold = as.character(fold)) %>%
      mutate(miss = !(fold %in% ids))

    missing %>% filter(miss) %>%
      select(fold) %>%
      unique() %>%
      unlist() %>%
      unname() %>%
      paste(., collapse = ",")

  }

}


# logistic regression models, random
dirs = c("100","250", "500", "1000", "2500", "5000", "10000", "13367")
for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res", dir), recursive = TRUE)
  if(as.numeric(dir)<= 1000) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(13367 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
          as.numeric(dir)*floor(13367/as.numeric(dir))))
    }
}


# logistic regression models, temporal
dirs = c("100", "500", "1000", "2500", "5000", "10770")
for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal2", dir), recursive = TRUE)
  if(as.numeric(dir)<= 1000) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(10770 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(10770/as.numeric(dir))))
  }
}

## xgboost
dirs = c("100", "500")
for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res", paste0(dir, "xgb")), recursive = TRUE)
  if(as.numeric(dir) <= 100) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(13367 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(13367/as.numeric(dir))))
  }
}

get_missing(size = 500, name = "xgb", individual = TRUE, temporal = FALSE)
get_missing(size = 500, name = "xgb", individual = TRUE, temporal = TRUE)
## xgboost, temporal
for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal2", paste0(dir, "xgb")), recursive = TRUE)
  if(as.numeric(dir)<= 100) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(10770 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(10770/as.numeric(dir))))
  }
}

get_missing(size = 500, name = "xgb", individual = TRUE, temporal = TRUE)

## rf
dirs = c("100", "500")
for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res", paste0(dir, "rf")), recursive = TRUE)
  if(as.numeric(dir) <= 100) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(13367 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(13367/as.numeric(dir))))
  }
}

get_missing(size = 500, name = "rf", individual = TRUE, temporal = FALSE)
get_missing(size = 500, name = "rf", individual = TRUE, temporal = TRUE)
get_missing(size = 500, name = "xgb", individual = TRUE, temporal = FALSE)
get_missing(size = 500, name = "xgb", individual = TRUE, temporal = TRUE)


## rf, temporal
for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal2", paste0(dir, "rf")), recursive = TRUE)
  if(as.numeric(dir)<= 100) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(10770 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(10770/as.numeric(dir))))
  }
}


# lasso
dirs = c("100","500")
for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res", paste0(dir, "lasso")), recursive = TRUE)
  if(as.numeric(dir) <= 100) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(13367 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(13367/as.numeric(dir))))
  }
}

# lasso temporal
for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal2", paste0(dir, "lasso")), recursive = TRUE)
  if(as.numeric(dir) <= 1000) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(10770 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(10770/as.numeric(dir))))
  }
}


# functional
dirs = c("100", "500", "1000")
for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res", paste0(dir, "fnl")), recursive = TRUE)
  if(as.numeric(dir) <= 100) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(13367 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(13367/as.numeric(dir))))
  }
}

for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res", paste0(dir, "nlfnl")), recursive = TRUE)
  if(as.numeric(dir) <= 100) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(13367 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(13367/as.numeric(dir))))
  }
}

dirs = c("100", "500", "1000")
for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal2", paste0(dir, "fnl")), recursive = TRUE)
  if(as.numeric(dir) <= 100) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(10770 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(10770/as.numeric(dir))))
  }
}

for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal2", paste0(dir, "nlfnl")), recursive = TRUE)
  if(as.numeric(dir) <= 100) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(10770 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(10770/as.numeric(dir))))
  }
}



## oversampling
dirs = c("100", "500", "1000", "10000")
for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res", paste0(dir, "over")), recursive = TRUE)
  if(as.numeric(dir) <= 1000) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(13367 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(13367/as.numeric(dir))))
  }
}

get_missing(size = 13367, name = "over", individual = TRUE, temporal = FALSE)
dirs = c("100", "500", "1000", "10770")
for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal2", paste0(dir, "over")), recursive = TRUE)
  if(as.numeric(dir)<= 1000) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(10770 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(10770/as.numeric(dir))))
  }
}



## long
dirs = c("100", "2500", "5000", "10129")
for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res", paste0(dir, "long")), recursive = TRUE)
  if(as.numeric(dir) <= 1000) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(10129 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(10129/as.numeric(dir))))
  }
}

dirs = c("100", "2500", "5000", "8018")
for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal2", paste0(dir, "long")), recursive = TRUE)
  if(as.numeric(dir) <= 1000) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(8018 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(8018/as.numeric(dir))))
  }
}


get_missing_long = function(name, size, individual = FALSE){
  results_path = here::here("data", "lily", "data", "fingerprint_res", paste0(size, name))
  fname_path = here::here("data", "lily", "data", "fingerprint_folds_long.rds")

  filenames = readRDS(fname_path)

  if(individual){
    x = ceiling(nrow(filenames) / size)
    filenames = filenames %>%
      mutate(fold = rep(1:x, each = size)[1:nrow(filenames)])

    fsize = ceiling(nrow(filenames)/1000)
    x = ceiling(nrow(filenames)/fsize)
    filenames = filenames %>%
      mutate(fold2 = rep(1:x, each = fsize)[1:nrow(filenames)])

    files = list.files(results_path)
    ids = sub(".rds.*","",files)

    missing = filenames %>%
      mutate(id = as.character(id)) %>%
      mutate(miss = !(id %in% ids))


    x = missing %>%
      filter(miss) %>%
      select(fold2) %>%
      unlist()

    x %>% unique() %>% unname() %>% paste(., collapse = ",")

  } else {
    x = ceiling(nrow(filenames) / size)
    filenames = filenames %>%
      mutate(fold = rep(1:x, each = size)[1:nrow(filenames)])
    files = list.files(results_path)
    ids = sub(".*fold\\_(.+)\\.rds.*", "\\1", files)

    missing = filenames %>%
      mutate(fold = as.character(fold)) %>%
      mutate(miss = !(fold %in% ids))

    missing %>% filter(miss) %>%
      select(fold) %>%
      unique() %>%
      unlist() %>%
      unname() %>%
      paste(., collapse = ",")

  }

}


# weighted
get_missing(size = 13367, name = "wtd", individual = TRUE, temporal = FALSE)

get_missing(size = 10770, name = "wtd", individual = TRUE, temporal = TRUE)

# train not test - not used
dirs = c("100","250", "500", "1000", "2500", "5000", "10000", "13367")
for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res", paste0(dir, "tnt")), recursive = TRUE)
  if(as.numeric(dir) <= 1000) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(13367 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(13367/as.numeric(dir))))
  }
}

