
library(tidyverse)
library(purrr)
library(tidymodels)
library(magrittr)
force = TRUE
tidymodels_prefer()
source(here::here("code", "R", "utils.R"))
ifold = get_fold()

fnames = readRDS(here::here("data", "raw", "all_filenames.rds"))
if(!is.null(ifold)){
  df = fnames %>% filter(fold == ifold)
}

if(!dir.exists(here::here("data", "lily", "data", "grid_cell_data_temporal2"))){
  dir.create(here::here("data", "lily", "data", "grid_cell_data_temporal2"))
}

# df = fnames %>% filter(fold == 1)
options(dplyr.summarise.inform = FALSE)

get_density = function(subject, df = df){
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
      filter(n >= 135) %>%
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
      mutate(id = subject, data = "test") %>%
      select(id, data, starts_with("("), starts_with("["))

    test_start = sample(x = 1:(nrow(test)-44), size = 1)

    test = test %>% slice(test_start:(test_start + 44))

    train =
      df_small %>%
      filter(day == day_train) %>%
      mutate(id = subject, data = "train") %>%
      select(id, data, starts_with("("), starts_with("["))

    train_start = sample(x = 1:(nrow(train)-134), size = 1)

    train = train %>% slice(train_start:(train_start + 134))


    res = bind_rows(train, test)
    res
  })
  x
}

outname = paste0("grid_data_fold_", ifold, ".csv.gz")
if(!file.exists(here::here("data", "lily", "data", "grid_cell_data_temporal2", outname)) | force){
  grid_data_list =
    map(.x = df$id, .f = get_density, df = df)

  grid_data_df =
    grid_data_list %>%
    keep(., ~ inherits(.x, "tbl_df")) %>%
    bind_rows() %>%
    janitor::clean_names()

  readr::write_csv(grid_data_df, here::here("data", "lily", "data", "grid_cell_data_temporal2", outname))

}
