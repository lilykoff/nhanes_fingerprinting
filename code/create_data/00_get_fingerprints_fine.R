library(tidyverse)
# needed because
# https://github.com/OverLordGoldDragon/ssqueezepy#gpu--cpu-acceleration
Sys.setenv("SSQ_PARALLEL" = 0)
options(digits.secs = 3)
source(here::here("code", "R", "helper_functions.R"))
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))

# idea: run fingerprints w/ 0.125 grid cells

df = readRDS(here::here("data", "raw", "all_filenames.rds"))
xdf = df

ifold = get_fold()

if (!is.na(ifold)) {
  df = df %>%
    dplyr::filter(fold %in% ifold)
}

max_n = nrow(df)
# max_n = 1
force = FALSE
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
# time_lags = c(15L, 30L, 45L)
time_lags = c(12L, 24L, 36L)
gcell_size = 0.1
max_vm  = 3  # we set max vm to 3 based on EDA, but could take actual max vm


for (index in seq(max_n)) {
  # print(index)
  idf = df[index, ]
  print(paste0(index, " of ", max_n))
  print(idf$csv_file)

  files = c(idf$csv_file, idf$adept_file)

  walking_file = file.path(
    here::here(
      "data",
      "lily",
      "data",
      "adept_walking_dfs",
      idf$version,
      paste0(idf$id, ".csv.gz")
    )
  )
  f_file = file.path(
    here::here(
      "data",
      "lily",
      "data",
      "fingerprint_data_10",
      idf$version,
      paste0(idf$id, ".csv.gz")
    )
  )


  if (!dir.exists(dirname(f_file))) {
    dir.create(dirname(f_file), recursive = TRUE)
  }


  if (!file.exists(f_file) && all(file.exists(files)) | force) {
    x = try({
      # read in walking file
      walking_df = readr::read_csv(here::here(walking_file)) # save walking data

      # now do fingerprinting
      n_seconds = max(walking_df$second_id) # no. of seconds for the subject
      seconds = rep(seq(1, n_seconds, 1), each = length(time_lags)) # vector of seconds and lags so that we can iterate over both
      lags = rep(time_lags, n_seconds)
      second_key = walking_df %>%
        select(second, second_id) %>%
        distinct() %>%
        mutate(day = floor_date(second, unit = "days"))

      res = map2_dfr(
        .x = seconds,
        .y = lags,
        .f = get_grid_data_lagsec,
        data = walking_df
      ) %>%
        pivot_wider(
          id_cols = second_id,
          names_from = cell,
          values_from = n
        ) %>%
        left_join(second_key, by = "second_id")

      write_csv_gz(res, f_file, progress = FALSE) # save fingerprint data

    })
    rm(x)

  }
}
