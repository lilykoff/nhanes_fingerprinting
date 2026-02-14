library(tidyverse)
library(tidymodels)
force = FALSE
filenames = read_rds(here::here("data", "lily", "data", "fingerprint_folds.rds"))
fnames = read_rds(here::here("data", "raw", "all_filenames.rds"))
# idx = 75012
flags = read_csv(here::here("data", "lily", "data", "data_1440/v2", "nhanes_1440_PAXFLGSM.csv.xz"))
wear = read_csv(here::here("data", "lily", "data", "data_1440/v2", "nhanes_1440_PAXPREDM.csv.xz"))


get_real_times = function(idx){
  out_file = here::here("data", "lily", "data", "fprint_df_ts", paste0(idx, ".rds"))
  if(!dir.exists(dirname(out_file))) dir.create(dirname(out_file), recursive = TRUE)
  if (!file.exists(out_file) || force){
    x = try({
    idf = fnames %>%
      filter(id == idx)

    flagsx = flags %>%
      filter(SEQN == idx)

    wearx = wear %>%
      filter(SEQN == idx)

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
      select(second_id, second, day, rleid) %>%
      mutate(minute = floor_date(second, unit = "mins"))

    start_time = read_csv(here::here("data", "csv", idf$version, paste0(idf$id, ".csv.gz")),
                          n_max = 1) %>%
      pull(HEADER_TIMESTAMP)

    wearx =
      wearx %>%
      pivot_longer(cols = starts_with("min")) %>%
      mutate(day = floor_date(start_time, unit = "days") + as.period(PAXDAYM - 1, "days")) %>%
      mutate(mins = as.numeric(sub(".*\\_", "", name)),
             minute = day + as.period(mins - 1, "minutes")) %>%
      select(SEQN, day, minute, PAXDAYM, wear_pred = value)

    wear_small =
      wearx %>%
      right_join(density, by = c("minute", "day"))

    flagsx =
      flagsx %>%
      pivot_longer(cols = starts_with("min")) %>%
      mutate(day = floor_date(start_time, unit = "days") + as.period(PAXDAYM - 1, "days")) %>%
      mutate(mins = as.numeric(sub(".*\\_", "", name)),
             minute = day + as.period(mins - 1, "minutes")) %>%
      select(SEQN, day, minute, PAXDAYM, flag_ind = value)

    flag_small =
      flagsx %>%
      right_join(density, by = c("minute", "day"))

    final_df =
      wear_small %>%
      full_join(flag_small, by = c("SEQN", "day", "minute", "PAXDAYM", "second_id",
                                    "second", "rleid")) %>%
      select(SEQN, minute, wear_pred, flag_ind, everything())

    ### temporal
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
      mutate(data = "test") %>%
      select(second, day, rleid, data)

    test_start = sample(x = 1:(nrow(test)-44), size = 1)

    test = test %>% slice(test_start:(test_start + 44))

    train =
      df_small %>%
      filter(day == day_train) %>%
      mutate(data = "train")  %>%
      select(second, day, rleid, data)
    train_start = sample(x = 1:(nrow(train)-134), size = 1)
    train = train %>% slice(train_start:(train_start + 134))
    res = bind_rows(train, test) %>%
      mutate(minute = floor_date(second, unit = "mins"))

    if(length(day_train) == 0 || length(day_test) == 0) {
      fdf = final_df %>%
        select(SEQN, minute, PAXDAYM, wear_pred, flag_ind, second, rleid, second_id) %>%
        mutate(data = "random")
    } else {
      wear_small_temp =
        wearx %>%
        right_join(res, by = c("minute", "day"))

      flag_small_temp =
        flagsx %>%
        right_join(res, by = c("minute", "day"))

      final_df_temp =
        wear_small_temp %>%
        full_join(flag_small_temp, by = c("SEQN", "day", "minute", "PAXDAYM",
                                          "second", "rleid", "data")) %>%
        select(SEQN, minute, PAXDAYM, wear_pred, flag_ind, data, everything())
      fdf = final_df %>%
        select(SEQN, minute, PAXDAYM, wear_pred, flag_ind, second, rleid, second_id) %>%
        mutate(data = "random") %>%
        bind_rows(final_df_temp %>%
                    select(SEQN, minute, PAXDAYM, wear_pred, flag_ind, second, rleid, data) %>%
                    mutate(data = paste0("temporal_", data)))
    }

    write_rds(fdf, out_file)
  })
 rm(x)
  }
}

purrr::walk(.x = filenames$id,
            .f = get_real_times)

# files = list.files(here::here("data", "lily", "data", "fprint_df_ts"))
# ids = sub(".rds.*", "", files)
# ids[1]
# missing = setdiff(filenames$id, ids)
#
# idx = missing[1]
