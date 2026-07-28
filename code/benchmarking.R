## fit model on subsets of n = 1000, predict on entire dataset

### average times
library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE




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

ifold = 1
size = 1000
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

f = 1
i = 1
if (!is.na(ifold)) {
  ids = filenames %>%
    filter(fold == f & fold2 == ifold) %>% pull(id)
}



ids_all =
  filenames %>%
  filter(fold == f) %>%
  pull(id) %>%
  as.character()

dat_nzv = read_rds(here::here(
  "data",
  "lily",
  "data",
  paste0("dat_nzv_train_", size, "_", f, ".rds")
)) %>%
  mutate(id = as.character(id)) %>%
  filter(id %in% ids_all)
dat_nzv_test = read_rds(here::here("data", "lily", "data", "dat_nzv_test.rds")) %>%
  mutate(id = as.character(id))

# make sure column names are same
names_train = names(dat_nzv)
names_test = names(dat_nzv_test)
keep = intersect(names_train, names_test)
keep_idx_test = which(names(dat_nzv_test) %in% keep)
dat_nzv_test = dat_nzv_test[, keep_idx_test]
keep_idx_train = which(names(dat_nzv) %in% keep)
dat_nzv = dat_nzv[, keep_idx_train]


times = c()
for (id in ids) {
  t = Sys.time()
  preds = fit_model_fast(subject = id,
                         train = dat_nzv,
                         test = dat_nzv_test)
  x = Sys.time() - t
  print(x)
  times = c(times, x)

}
print(mean(times))
# [1] 9.969268 # seconds

dat_nzv_train = read_rds(here::here("data", "lily", "data", "dat_nzv_train.rds")) %>%
  mutate(id = as.character(id))

times2 = c()
for (id in ids) {
  t = Sys.time()
  preds = fit_model_fast(subject = id,
                         train = dat_nzv_train,
                         test = dat_nzv_test)
  x = Sys.time() - t
  print(x)
  times2 = c(times2, x)

}

print(mean(times2))


## test how long SC takes

dat_nzv = read_rds(here::here(
  "data",
  "lily",
  "data",
  paste0("dat_nzv_train_sc", ".rds")
)) %>%
  mutate(id = as.character(id)) %>%
  filter(id %in% ids_all)


dat_nzv_test = read_rds(here::here("data", "lily", "data", "dat_nzv_test_sc.rds")) %>%
  mutate(id = as.character(id))

# make sure column names are same
names_train = names(dat_nzv)
names_test = names(dat_nzv_test)
keep = intersect(names_train, names_test)
keep_idx_test = which(names(dat_nzv_test) %in% keep)
dat_nzv_test = dat_nzv_test[, keep_idx_test]
keep_idx_train = which(names(dat_nzv) %in% keep)
dat_nzv = dat_nzv[, keep_idx_train]


times = c()
for (id in ids) {
  t = Sys.time()
  preds = fit_model_fast(subject = id,
                         train = dat_nzv,
                         test = dat_nzv_test)
  x = Sys.time() - t
  print(x)
  times = c(times, x)

}
print(mean(times))


## full w/ sc
dat_nzv_train = read_rds(here::here("data", "lily", "data", "dat_nzv_train_sc.rds")) %>%
  mutate(id = as.character(id))
dat_nzv_test = read_rds(here::here("data", "lily", "data", "dat_nzv_test_sc.rds")) %>%
  mutate(id = as.character(id))

times2 = c()
for (id in ids) {
  t = Sys.time()
  preds = fit_model_fast(subject = id,
                         train = dat_nzv_train,
                         test = dat_nzv_test)
  x = Sys.time() - t
  print(x)
  times2 = c(times2, x)

}
print(mean(times2))
