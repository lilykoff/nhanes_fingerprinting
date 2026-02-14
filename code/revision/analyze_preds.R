library(tidyverse)
library(tidymodels)

if (!dir.exists(here::here("data", "lily", "data", "pred_dfs"))) {
  dir.create(here::here("data", "lily", "data", "pred_dfs"), recursive = TRUE)
}
root = file.path(here::here("data", "lily", "data", "pred_dfs"))
filenames = read_rds(here::here("data", "lily", "data", "fingerprint_folds.rds"))
fnames = readRDS(here::here("data", "raw", "all_filenames.rds"))
preds = read_rds(here::here("data", "lily", "data", "subj_level_preds.rds"))
size = 100
poor_preds =
  preds %>%
  filter(rank5 == 0) %>%
  arrange(mean_pred) %>%
  slice(1:100) %>%
  pull(true_subject)

poor_preds

good_preds =
  preds %>%
  filter(rank1 == 1) %>%
  arrange(desc(mean_pred)) %>%
  slice(1:100) %>%
  pull(true_subject)

good_preds

# read in training and testing data
idx = poor_preds[1]
xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells.csv.gz"))

for(idx in c(good_preds, poor_preds)){
  ifold = filenames %>%
    filter(id == idx) %>%
    pull(fold)
  idf = fnames %>%
    filter(id == idx)
  if (!is.na(ifold) & !is.na(size)) {
    x = ceiling(nrow(filenames)/size)
    filenames = filenames %>%
      mutate(fold = rep(1:x, each = size)[1:nrow(filenames)])
    fdf = filenames %>%
      filter(fold == ifold)
  }

  df =
    xdf %>%
    filter(id %in% fdf$id) %>%
    group_by(id) %>%
    mutate(rn = row_number()) %>%
    ungroup()

  set.seed(123)
  initialsplit = initial_split(df, prop = 3/4, strata = id)

  data_train = training(initialsplit)
  data_test = testing(initialsplit)


  train_inds = data_train %>% filter(id == idx) %>% pull(rn)
  test_inds = data_test %>% filter(id == idx) %>% pull(rn)

  walking_df =
    readr::read_csv(here::here("data", "lily", "data", "fingerprint_data", idf$version, paste0(idf$id, ".csv.gz")))
  # get segments of consecutive walking with < 2 seconds between them that are at least 10 seconds long
  segments_10 = walking_df %>%
    select(second, day) %>%
    distinct() %>%
    mutate(timediff = as.numeric(difftime(second, dplyr::lag(second, n = 1), units = "secs")),
           ltwosec = (timediff <= 2)*1,
           rleid = data.table::rleid(ltwosec)) %>%
    filter(ltwosec == 1) %>%
    group_by(rleid, day) %>%
    summarize(n_seconds = n(),
              start = min(second),
              end = max(second)) %>%
    filter(n_seconds >= 10)

  # key of those times
  seconds_key =
    segments_10 %>%
    group_by(rleid, day) %>%
    tidyr::expand(second = seq(start, end, "sec"))

  df_small =
    walking_df %>%
    inner_join(seconds_key, by = c("second", "day"))

  # if there are at least 3 mins of data, sample 3 mins randomly (so that everyone has same amount of walking)

  set.seed(123)
  density =
    df_small %>%
    sample_n(size = 180, replace = FALSE) %>%
    mutate(rn = row_number()) %>%
    mutate(train = rn %in% train_inds) %>%
    select(second_id, train, everything())

  wdf = read_csv(here::here(
    "data",
    "lily",
    "data",
    "adept_walking_dfs",
    idf$version,
    paste0(idf$id, ".csv.gz")
  ))

  train_ids = density %>% filter(train) %>% pull(second_id)
  wdf_small =
    wdf %>%
    filter(second_id %in% density$second_id) %>%
    mutate(train = second_id %in% train_ids) %>%
    select(second_id, train, everything())

  rm(wdf)

  write_rds(wdf_small, here::here(root, paste0(idx, "_raw_vm.rds")))
  write_rds(density, here::here(root, paste0(idx, "_gcells.rds")))
}


key_df = tibble(preds = c(poor_preds, good_preds),
                type = c(rep("poor", length(poor_preds)), rep("good", length(good_preds))))

write_rds(key_df, here::here("data", "lily", "data", "pred_key_df.rds"))
