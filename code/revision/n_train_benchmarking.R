library(tidyverse)
library(tidymodels)
# source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE

# let's do this in a better way


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

filenames = read_rds(here::here("data", "lily", "data", "fingerprint_folds.rds"))

dat_nzv_test = read_rds(here::here("data", "lily", "data", "dat_nzv_test.rds")) %>%
  mutate(id = as.character(id))


dat_nzv_train = read_rds(here::here("data", "lily", "data", "dat_nzv_train.rds")) %>%
  mutate(id = as.character(id))



library(rbenchmark)

set.seed(42)
idx = sample(filenames$id, 1)
ids_to_sample = filenames %>%
  filter(id != idx) %>%
  pull(id) %>%
  as.character()

bench = benchmark("100" = {
    train_ids = sample(ids_to_sample, size = 99, replace = FALSE)
    train_temp = dat_nzv_train %>% filter(id %in% c(train_ids, idx))
    b = fit_model_fast(subject = idx,
                           train = train_temp,
                           test = dat_nzv_test)
  },
  "500" = {
    train_ids = sample(ids_to_sample, size = 499, replace = FALSE)
    train_temp = dat_nzv_train %>% filter(id %in% c(train_ids, idx))
    b = fit_model_fast(subject = idx,
                       train = train_temp,
                       test = dat_nzv_test)
  },
  "1000" = {
    train_ids = sample(ids_to_sample, size = 999, replace = FALSE)
    train_temp = dat_nzv_train %>% filter(id %in% c(train_ids, idx))
    b = fit_model_fast(subject = idx,
                       train = train_temp,
                       test = dat_nzv_test)
  },
  "2500" = {
    train_ids = sample(ids_to_sample, size = 2499, replace = FALSE)
    train_temp = dat_nzv_train %>% filter(id %in% c(train_ids, idx))
    b = fit_model_fast(subject = idx,
                       train = train_temp,
                       test = dat_nzv_test)
  },
  "5000" = {
    train_ids = sample(ids_to_sample, size = 4999, replace = FALSE)
    train_temp = dat_nzv_train %>% filter(id %in% c(train_ids, idx))
    b = fit_model_fast(subject = idx,
                       train = train_temp,
                       test = dat_nzv_test)
  },
  "7500" = {
    train_ids = sample(ids_to_sample, size = 7499, replace = FALSE)
    train_temp = dat_nzv_train %>% filter(id %in% c(train_ids, idx))
    b = fit_model_fast(subject = idx,
                       train = train_temp,
                       test = dat_nzv_test)
  },
  "10000" = {
    train_ids = sample(ids_to_sample, size = 9999, replace = FALSE)
    train_temp = dat_nzv_train %>% filter(id %in% c(train_ids, idx))
    b = fit_model_fast(subject = idx,
                       train = train_temp,
                       test = dat_nzv_test)
  },
  "N" = {
    train_ids = sample(ids_to_sample, size = 13366, replace = FALSE)
    train_temp = dat_nzv_train %>% filter(id %in% c(train_ids, idx))
    b = fit_model_fast(subject = idx,
                       train = train_temp,
                       test = dat_nzv_test)
  },
  replications = 50,
  columns = c("test", "replications", "elapsed",
              "relative", "user.self", "sys.self"))

bench

bench = tibble(
  test = bench$test,
  replications = bench$replications,
  elapsed = bench$elapsed,
  relative = bench$relative,
  user.self = bench$user.self,
  sys.self = bench$sys.self
)

write_rds(bench, here::here("data", "lily", "data", "bench_adept.rds"))

# test replications  elapsed relative user.self sys.self
# 1  100           50   33.774    1.000    29.969    3.501
# 3 1000           50  202.016    5.981   198.813    1.534
# 4 2500           50  492.843   14.592   485.703    3.077
# 2  500           50  107.229    3.175   104.481    1.831
# 5 5000           50 1024.732   30.341   997.698   19.082
# 6    N           50 3138.554   92.928  3034.587   78.754



filenames_sc = read_rds(here::here("data", "lily", "data", "fingerprint_folds_sc.rds"))

dat_nzv_test_sc = read_rds(here::here("data", "lily", "data", "dat_nzv_test_sc.rds")) %>%
  filter(id %in% filenames$id) %>%
  mutate(id = as.character(id))

dat_nzv_train_sc = read_rds(here::here("data", "lily", "data", "dat_nzv_train_sc.rds")) %>%
  filter(id %in% filenames$id) %>%
  mutate(id = as.character(id))


set.seed(42)
idx = sample(filenames$id, 1)
ids_to_sample = filenames %>%
  filter(id != idx) %>%
  pull(id) %>%
  as.character()

bench_sc = benchmark("adept" = {
  train_ids = sample(ids_to_sample, size = 499, replace = FALSE)
  train_temp = dat_nzv_train %>% filter(id %in% c(train_ids, idx))
  b = fit_model_fast(subject = idx,
                     train = train_temp,
                     test = dat_nzv_test)
},
"sc" = {
  train_ids = sample(ids_to_sample, size = 499, replace = FALSE)
  train_temp = dat_nzv_train_sc %>% filter(id %in% c(train_ids, idx))
  b = fit_model_fast(subject = idx,
                     train = train_temp,
                     test = dat_nzv_test_sc)
},
replications = 50,
columns = c("test", "replications", "elapsed",
            "relative", "user.self", "sys.self"))

bench_sc
write_rds(bench_sc, here::here("data", "lily", "data", "bench_sc_adept.rds"))

bench = benchmark("100" = {
  train_ids = sample(ids_to_sample, size = 99, replace = FALSE)
  train_temp = dat_nzv_train %>% filter(id %in% c(train_ids, idx))
  b = fit_model_fast(subject = idx,
                     train = train_temp,
                     test = dat_nzv_test)
},
"500" = {
  train_ids = sample(ids_to_sample, size = 499, replace = FALSE)
  train_temp = dat_nzv_train %>% filter(id %in% c(train_ids, idx))
  b = fit_model_fast(subject = idx,
                     train = train_temp,
                     test = dat_nzv_test)
},
"1000" = {
  train_ids = sample(ids_to_sample, size = 999, replace = FALSE)
  train_temp = dat_nzv_train %>% filter(id %in% c(train_ids, idx))
  b = fit_model_fast(subject = idx,
                     train = train_temp,
                     test = dat_nzv_test)
},
"2500" = {
  train_ids = sample(ids_to_sample, size = 2499, replace = FALSE)
  train_temp = dat_nzv_train %>% filter(id %in% c(train_ids, idx))
  b = fit_model_fast(subject = idx,
                     train = train_temp,
                     test = dat_nzv_test)
},
"5000" = {
  train_ids = sample(ids_to_sample, size = 4999, replace = FALSE)
  train_temp = dat_nzv_train %>% filter(id %in% c(train_ids, idx))
  b = fit_model_fast(subject = idx,
                     train = train_temp,
                     test = dat_nzv_test)
},
"N" = {
  train_ids = sample(ids_to_sample, size = 13366, replace = FALSE)
  train_temp = dat_nzv_train %>% filter(id %in% c(train_ids, idx))
  b = fit_model_fast(subject = idx,
                     train = train_temp,
                     test = dat_nzv_test)
},
replications = 50,
columns = c("test", "replications", "elapsed",
            "relative", "user.self", "sys.self"))

bench

bench = tibble(
  test = bench$test,
  replications = bench$replications,
  elapsed = bench$elapsed,
  relative = bench$relative,
  user.self = bench$user.self,
  sys.self = bench$sys.self
)

write_rds(bench, here::here("data", "lily", "data", "bench_adept.rds"))

bench_sc %>%
  mutate(across(-c(test, replications), ~.x / replications))

# fitting models w/ 30 min of data, 500 subs per training
# 61.20304 * 13367 / 60
# + 17 hrs for step 0

# 5.73960 * 13367 / 60 /60
