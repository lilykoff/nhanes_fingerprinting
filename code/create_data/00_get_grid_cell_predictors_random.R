library(tidyverse)
# needed because
# https://github.com/OverLordGoldDragon/ssqueezepy#gpu--cpu-acceleration
Sys.setenv("SSQ_PARALLEL" = 0)
options(digits.secs = 3)
source(here::here("code", "R", "helper_functions.R"))
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))

df = readRDS(here::here("data", "raw", "all_filenames.rds"))
xdf = df

ifold = get_fold()

if (!is.na(ifold)) {
  df = df %>%
    dplyr::filter(fold %in% ifold)
}

max_n = nrow(df)
# max_n = 1
force = TRUE
index = 1

get_grid_data_lagsec = function(s, lag, data) {
  # filter to one second
  data %>% filter(second_id == s) %>%
    dplyr::select(vm) %>%
    mutate(lag_vm = dplyr::lag(vm, n = lag)) %>%   # for each second, calculate vm and lagged vm
    mutate(
      cut_sig = cut(
        vm,
        breaks = seq(0, max_vm, by = gcell_size),
        include.lowest = T
      ),
      cut_lagsig = cut(
        lag_vm,
        breaks = seq(0, max_vm, by = gcell_size),
        include.lowest = T
      )
    ) %>%
    drop_na() %>% # count # points in each "grid cell"
    count(cut_sig, cut_lagsig, .drop = FALSE) %>%
    mutate(
      lag = lag,
      second_id = s,
      cell = paste(cut_sig, cut_lagsig, lag, sep = "_")
    ) %>%
    dplyr::select(n, second_id, cell)
}

sample_rate = 80L
time_lags = c(12L, 24L, 36L)
gcell_size = 0.25
max_vm  = 3  # we set max vm to 3 based on EDA, but could take actual max vm


for (index in seq(max_n)) {
  # print(index)
  idf = df[index, ]
  print(paste0(index, " of ", max_n))
  print(idf$csv_file)

  files = c(idf$csv_file)

  outfiles = c(file.path(
    here::here(
      "data",
      "lily",
      "data",
      "random_dfs",
      idf$version,
      paste0(idf$id, ".csv.gz")
    )
  ), file.path(
    here::here(
      "data",
      "lily",
      "data",
      "random_fingerprint_data",
      idf$version,
      paste0(idf$id, ".csv.gz")
    )
  ))

  if (!dir.exists(dirname(outfiles[1])))
    dir.create(dirname(outfiles[1]), recursive = TRUE)
  if (!dir.exists(dirname(outfiles[2])))
    dir.create(dirname(outfiles[2]), recursive = TRUE)


  if (!all(file.exists(outfiles)) && all(file.exists(files)) || force) {
    x = try({
      # read in adept file (second level steps)
      adept_file = readr::read_csv(idf$adept_file)
      # filter to seconds where steps
      adept_file_walking = adept_file %>% filter(steps > 0)
      # read in csv file with raw data
      min_file = read_80hz(idf$csv_file, progress = FALSE)

      min_file

      seconds =
        min_file %>%
        mutate(second = floor_date(HEADER_TIMESTAMP, unit = "seconds")) %>%
        select(second) %>%
        distinct()

      ## take 30 mins randomly and 30 mins consecutively

      set.seed(123)
      seconds_random =
        seconds %>%
        slice_sample(n = 3 * 60)

      set.seed(123)
      # start_ind = sample(1:(nrow(seconds) - 3 * 60), 1)

      # seconds_consec =
      #   seconds %>%
      #   slice(start_ind:(start_ind + (3 * 60) - 1))
      #
      df_random =
        min_file %>%
        mutate(second = lubridate::floor_date(HEADER_TIMESTAMP, unit = "seconds")) %>%
        filter(second %in% seconds_random$second)

      # df_consec =
      #   min_file %>%
      #   mutate(second = lubridate::floor_date(HEADER_TIMESTAMP, unit = "seconds")) %>%
      #   filter(second %in% seconds_consec$second)
      #
      rm(min_file) # free up space

      # only keep full seconds for fingerprinting
      df_random =  df_random %>%
        group_by(second) %>%
        mutate(n = n()) %>%
        filter(n == sample_rate) %>%
        ungroup() %>%
        select(-n) %>%
        mutate(second_id = data.table::rleid(second),
               vm = sqrt(X ^ 2 + Y ^ 2 + Z ^ 2))

      # get days for rejoining
      second_key = df_random %>%
        select(second, second_id) %>%
        distinct() %>%
        mutate(day = floor_date(second, unit = "days"))

      write_csv_gz(df_random, outfiles[1], progress = FALSE) # save walking data

      # now do fingerprinting
      n_seconds = max(df_random$second_id) # no. of seconds for the subject
      seconds = rep(seq(1, n_seconds, 1), each = length(time_lags)) # vector of seconds and lags so that we can iterate over both
      lags = rep(time_lags, n_seconds)

      res = map2_dfr(
        .x = seconds,
        .y = lags,
        .f = get_grid_data_lagsec,
        data = df_random
      ) %>%
        pivot_wider(
          id_cols = second_id,
          names_from = cell,
          values_from = n
        ) %>%
        left_join(second_key, by = "second_id")

      write_csv_gz(res, outfiles[2], progress = FALSE) # save fingerprint data

      # df_consec =  df_consec %>%
      #   group_by(second) %>%
      #   mutate(n = n()) %>%
      #   filter(n == sample_rate) %>%
      #   ungroup() %>%
      #   select(-n) %>%
      #   mutate(second_id = data.table::rleid(second),
      #          vm = sqrt(X ^ 2 + Y ^ 2 + Z ^ 2))
      #
      # # get days for rejoining
      # second_key = df_consec %>%
      #   select(second, second_id) %>%
      #   distinct() %>%
      #   mutate(day = floor_date(second, unit = "days"))
      #
      # write_csv_gz(df_consec, outfiles[1], progress = FALSE) # save walking data
      #
      # # now do fingerprinting
      # n_seconds = max(df_consec$second_id) # no. of seconds for the subject
      # seconds = rep(seq(1, n_seconds, 1), each = length(time_lags)) # vector of seconds and lags so that we can iterate over both
      # lags = rep(time_lags, n_seconds)
      #
      # res = map2_dfr(
      #   .x = seconds,
      #   .y = lags,
      #   .f = get_grid_data_lagsec,
      #   data = df_consec
      # ) %>%
      #   pivot_wider(
      #     id_cols = second_id,
      #     names_from = cell,
      #     values_from = n
      #   ) %>%
      #   left_join(second_key, by = "second_id")
      #
      # write_csv_gz(res, outfiles[2], progress = FALSE) # save fingerprint data
      #
    })
    rm(x)

  }
}
