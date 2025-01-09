library(tidyverse)

# check files for regular models
dirs = c("100","250", "500", "1000", "2500", "5000", "10000", "13367")
dir = "100"
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


# temporal, regular models
dirs = c("100", "250", "500", "1000", "2500", "5000", "10000", "11225")
for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal", dir), recursive = TRUE)
  if(as.numeric(dir)<= 1000) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(11225 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(11225/as.numeric(dir))))
  }
}

## xgboost, regular
dirs = c("100","250", "500", "1000", "2500", "5000", "10000", "13367")
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

# find which are missing

size = 100

filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds.rds"))
x = ceiling(nrow(filenames) / size)
filenames = filenames %>%
  mutate(fold = rep(1:x, each = size)[1:nrow(filenames)])
files = list.files(here::here("data", "lily", "data", "fingerprint_res", paste0(size, "xgb")))
ids = sub(".*fold\\_(.+)\\.rds.*", "\\1", files)

missing = filenames %>%
  mutate(fold = as.character(fold)) %>%
  mutate(miss = !(fold %in% ids))

missing %>% filter(miss) %>% select(fold) %>% unique()

# functional
dirs = c("100","250", "500", "1000", "2500", "5000", "10000", "13367")
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

# find which are missing

size = 100

filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds.rds"))
x = ceiling(nrow(filenames) / size)
filenames = filenames %>%
  mutate(fold = rep(1:x, each = size)[1:nrow(filenames)])
files = list.files(here::here("data", "lily", "data", "fingerprint_res", paste0(size, "fnl")))
ids = sub(".*fold\\_(.+)\\.rds.*", "\\1", files)

missing = filenames %>%
  mutate(fold = as.character(fold)) %>%
  mutate(miss = !(fold %in% ids))

missing %>% filter(miss) %>% select(fold) %>% unique()


# xgboost, temporal
# temporal, regular models
dirs = c("100", "250", "500", "1000", "2500", "5000", "10000", "11225")
for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal", paste0(dir, "xgb")), recursive = TRUE)
  if(as.numeric(dir)<= 100) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(11225 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(11225/as.numeric(dir))))
  }
}

# randomforest
dirs = c("100","250", "500", "1000", "2500", "5000", "10000", "13367")
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

dirs = c("100", "250", "500", "1000", "2500", "5000", "10000", "11225")
for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal", paste0(dir, "_rf")), recursive = TRUE)
  if(as.numeric(dir)<= 100) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(11225 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(11225/as.numeric(dir))))
  }
}

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


## oversampling
dirs = c("100","250", "500", "1000", "2500", "5000", "10000", "13367")
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
dirs = c("100", "250", "500", "1000", "2500", "5000", "10000", "11225")
for (dir in dirs) {
  files = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal", paste0(dir, "over")), recursive = TRUE)
  if(as.numeric(dir)<= 1000) {
    print(
      paste0("dir = ", dir, " length files = ", length(files),"; target = ", floor(11225 / as.numeric(dir))))
  } else {
    print(paste0("dir = ", dir, " length files = ", length(files),"; target = ",
                 as.numeric(dir)*floor(11225/as.numeric(dir))))
  }
}



filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds_temporal.rds"))
x = ceiling(nrow(filenames) / size)
filenames = filenames %>%
  mutate(fold = rep(1:x, each = size)[1:nrow(filenames)])
files = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal", paste0(size, "xgb")))
ids = sub(".*fold\\_(.+)\\.rds.*", "\\1", files)

missing = filenames %>%
  mutate(fold = as.character(fold)) %>%
  mutate(miss = !(fold %in% ids))

missing %>% filter(miss) %>% select(fold) %>% unique()


# 2500 and 5000 missing a few

### other

size = 2500
filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds_temporal.rds"))
x = ceiling(nrow(filenames) / size)
filenames = filenames %>%
  mutate(fold = rep(1:x, each = size)[1:nrow(filenames)])

fsize = ceiling(nrow(filenames)/1000)
x = ceiling(nrow(filenames)/fsize)
filenames = filenames %>%
  mutate(fold2 = rep(1:x, each = fsize)[1:nrow(filenames)])

files = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal", size))
ids = sub(".rds.*","",files)


missing = filenames %>%
  mutate(id = as.character(id)) %>%
  mutate(miss = !(id %in% ids))



missing %>% filter(miss)


x = missing %>%
  filter(miss) %>%
  select(fold2) %>%
  unlist()

x %>% unique() %>% unname()
x %>% unique() %>% unname() %>% paste(., collapse = ",")

size = 5000
filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds_temporal.rds"))
x = ceiling(nrow(filenames) / size)
filenames = filenames %>%
  mutate(fold = rep(1:x, each = size)[1:nrow(filenames)])

fsize = ceiling(nrow(filenames)/1000)
x = ceiling(nrow(filenames)/fsize)
filenames = filenames %>%
  mutate(fold2 = rep(1:x, each = fsize)[1:nrow(filenames)])

files = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal", size))
ids = sub(".rds.*","",files)


missing = filenames %>%
  mutate(id = as.character(id)) %>%
  mutate(miss = !(id %in% ids))



missing %>% filter(miss)


x = missing %>%
  filter(miss) %>%
  select(fold2) %>%
  unlist()

x %>% unique() %>% unname()
x %>% unique() %>% unname() %>% paste(., collapse = ",")

