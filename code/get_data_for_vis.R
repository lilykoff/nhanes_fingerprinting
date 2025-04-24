# try fitting regression models on one fold (70 people)

library(tidyverse)
library(purrr)
library(tidymodels)
library(magrittr)
force = FALSE
tidymodels_prefer()
source(here::here("code", "R", "utils.R"))
ifold = get_fold()
options(digits.secs = 3)
fnames = readRDS(here::here("data", "raw", "all_filenames.rds"))

filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds.rds"))
filenames_t = readRDS(here::here("data", "lily", "data", "fingerprint_folds_temporal.rds"))


# df = fnames %>% filter(fold == 1)
options(dplyr.summarise.inform = FALSE)

get_density = function(subject, df){
  idf = df %>% filter(id == subject)
  x = try({
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
    if(nrow(df_small) >= 180) {
      set.seed(123)
      density =
        df_small %>%
        sample_n(size = 180, replace = FALSE) %>%
        mutate(id = subject) %>%
        select(second, id)
      walking_dat = read_csv(
        here::here(
          "data",
          "lily",
          "data",
          "adept_walking_dfs",
          idf$version,
          paste0(idf$id, ".csv.gz")
        )
      )
      fprint_seconds = walking_dat %>%
        filter(second %in% density$second) %>%
        mutate(id = subject)
      rm(density); rm(walking_dat)

    } else {
      fprint_seconds = NULL
    }
  fprint_seconds
  })
  x
}

set.seed(124)
sample_ids = sample_n(filenames, size = 100, replace = FALSE) %>% pull(id)

# get_density(sub, df = fnames)

walking_dat = map_dfr(.x = sample_ids,
                  .f = get_density,
                  df = fnames)
#
# walking_dat =
#   bind_rows(walking_dat)

write_rds(walking_dat,  here::here("data", "lily", "data", "fingerprint_data_sample.rds"))


get_density_temporal = function(subject, df){
  idf = df %>% filter(id == subject)
  x = try({
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

    day_df =
      df_small %>%
      group_by(day) %>%
      count() %>%
      ungroup()

    day_train =
      day_df %>%
      filter(n >= 120) %>%
      slice(1) %>%
      pull(day)

    day_test =
      day_df %>%
      filter(n >= 45 & day > day_train) %>%
      slice_sample(n = 1)  %>%
      pull(day)


    test =
      df_small %>%
      filter(day == day_test) %>%
      mutate(id = subject, data = "test")

    test_start = sample(x = 1:(nrow(test)-44), size = 1)

    test = test %>% slice(test_start:(test_start + 44))

    train =
      df_small %>%
      filter(day == day_train) %>%
      mutate(id = subject, data = "train")

    train_start = sample(x = 1:(nrow(train)-119), size = 1)

    train = train %>% slice(train_start:(train_start + 119))
    train = train %>% select(day, second, data)
    test = test %>% select(day, second, data)
    res = bind_rows(train, test)

    walking_dat = read_csv(
      here::here(
        "data",
        "lily",
        "data",
        "adept_walking_dfs",
        idf$version,
        paste0(idf$id, ".csv.gz")
      )
    )
    walking_dat %>%
      right_join(res, by = c("second")) %>%
      mutate(id = subject)
  })
  x
}


set.seed(124)
sample_ids = sample_n(filenames_t, size = 100, replace = FALSE) %>% pull(id)

# get_density(sub, df = fnames)

walking_dat = map_dfr(.x = sample_ids,
                  .f = get_density_temporal,
                  df = fnames)


write_rds(walking_dat,  here::here("data", "lily", "data", "fingerprint_data_sample_temporal.rds"))

