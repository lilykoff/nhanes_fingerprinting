library(tidyverse)


files = list.files(here::here("data", "lily", "data",
                              "fingerprint_prediction_results"),
                   full.names = TRUE)

get_summarized_predictions = function(predictions, rank = FALSE, exp = FALSE) {
  # predictions is tibble
  # column j is the predictions from fitting the model where the "true subject" is subject j
  # each row is prediction for a given second
  # true subject is the final column

  if (rank) {
    # will return data frame with columns true subject, model, mean prediction, rank of correct prediction,
    # and rank1, rank5 which indicate whether predicted subject was in top 1 or top 5
    predictions %>%
      group_by(true_subject) %>%
      mutate(sec = row_number()) %>%
      pivot_longer(cols = -c("true_subject", "sec"), names_to = "name", values_to = "pred") %>%
      mutate(model = as.numeric(sub(".*x", "", name))) %>%
      select(-name) %>%
      # now we have the prediction for each second for each model / true subject combo
      mutate(pred = case_when(exp ~ exp(pred),
                              .default = pred)) %>% # exponentiate based on exp argument
      ungroup() %>%
      group_by(true_subject, model) %>%
      # get mean probability across seconds for each true subject / model combo
      summarize(mean_pred = mean(pred, na.rm = TRUE), .groups = "drop") %>%
      group_by(true_subject) %>%
      mutate(
        rank = rank(-mean_pred)
      ) %>% # get the rank for each prediction
      ungroup() %>%
      filter(model == true_subject) %>% # only keep the correct combos and get ranks
      mutate(
        rank1 = if_else(rank == 1, 1, 0),
        rank5 = if_else(rank <= 5, 1, 0)
      )
  } else {
    predictions %>%
      group_by(true_subject) %>%
      mutate(sec = row_number()) %>%
      pivot_longer(cols = -c("true_subject", "sec"), names_to = "name", values_to = "pred") %>%
      mutate(model = as.numeric(sub(".*x", "", name))) %>%
      select(-name) %>%
      # now we have the prediction for each second for each model / true subject combo
      mutate(pred = case_when(exp ~ exp(pred),
                              .default = pred)) %>% # exponentiate based on exp argument
      ungroup() %>%
      group_by(true_subject, model) %>%
      summarize(mean_pred = mean(pred, na.rm = TRUE), .groups = "drop") %>%
      group_by(true_subject) %>%
      summarize(
        maxprob = first(max(mean_pred)),
        predicted_sub = first(model[mean_pred == maxprob]),
        probsubj = first(mean_pred[true_subject == model])
      ) %>%
      mutate(correct = if_else(as.numeric(predicted_sub) == true_subject, 1, 0)) %>%
      ungroup()
  }
}


dirs = c("100", "250", "500" , "1000", "2500", "5000", "10000", "13367")
dir = dirs[1]

dirnum = as.numeric(dir)



all_preds = list.files(
  here::here("data", "lily", "data", "fingerprint_res", dir),
  recursive = TRUE,
  full.names = TRUE,
  pattern = "rds"
)

summary = map(
  .x = all_preds,
  .f = function(file) {
    x = readRDS(file)

    fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))
    n_target = as.numeric(sub(".*fingerprint_res\\/(.+)\\/fold.*", "\\1", file))

    res = get_summarized_predictions(x, rank = FALSE)
    rm(x)
    res
  }
) %>%
  bind_rows(.id = "id")

s1 = summary %>%
  filter(correct == 1) %>%
  group_by(id) %>%
  arrange(-maxprob) %>%
  ungroup() %>%
  slice(1:133)  %>%
  pull(true_subject)

dir = "500"
all_preds = list.files(
  here::here("data", "lily", "data", "fingerprint_res", dir),
  recursive = TRUE,
  full.names = TRUE,
  pattern = "rds"
)

summary_500 = map(
  .x = all_preds,
  .f = function(file) {
    x = readRDS(file)

    fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))
    n_target = as.numeric(sub(".*fingerprint_res\\/(.+)\\/fold.*", "\\1", file))

    res = get_summarized_predictions(x, rank = FALSE)
    rm(x)
    res
  }
) %>%
  bind_rows(.id = "id")

s5 = summary_500 %>%
  filter(correct == 1) %>%
  group_by(id) %>%
  arrange(-maxprob) %>%
  ungroup() %>%
  slice(1:26) %>%
  pull(true_subject)

intersect(s1, s5)

dir = "1000"
all_preds = list.files(
  here::here("data", "lily", "data", "fingerprint_res", dir),
  recursive = TRUE,
  full.names = TRUE,
  pattern = "rds"
)

summary_1000 = map(
  .x = all_preds,
  .f = function(file) {
    x = readRDS(file)

    fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))
    n_target = as.numeric(sub(".*fingerprint_res\\/(.+)\\/fold.*", "\\1", file))

    res = get_summarized_predictions(x, rank = FALSE)
    rm(x)
    res
  }
) %>%
  bind_rows(.id = "id")

s10 = summary_1000 %>%
  filter(correct == 1) %>%
  group_by(id) %>%
  arrange(-maxprob) %>%
  ungroup() %>%
  slice(1:13) %>%
  pull(true_subject)

ids_high = intersect(s1, s10) %>% intersect(., s5)

# 76565 75012 82367 78466 71996 66810 79177 79001 73380 68744 79351
s10 = summary_1000 %>%
  filter(correct == 0) %>%
  group_by(id) %>%
  arrange(probsubj) %>%
  ungroup() %>%
  slice(1:100) %>%
  pull(true_subject)
s5 = summary_500 %>%
  filter(correct == 0) %>%
  group_by(id) %>%
  arrange(probsubj) %>%
  ungroup() %>%
  slice(1:100) %>%
  pull(true_subject)
s1 = summary %>%
  filter(correct == 0) %>%
  group_by(id) %>%
  arrange(probsubj) %>%
  ungroup() %>%
  slice(1:100)  %>%
  pull(true_subject)
ids_low = intersect(s1, s10) %>% intersect(., s5)
ids_low

# 69878 72735 77715 79772 74392 65559 69606 81018

ids = c(ids_high, ids_low)
fnames = readRDS(here::here("data", "raw", "all_filenames.rds"))

for(subject in ids){
  outfile = here::here("data", "lily", "data", "fingerprint_exs", paste0(subject, ".csv.gz"))
  x = try({
    idf = fnames %>% filter(id == subject)
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
      mutate(id = subject) %>%
      select(id, second_id, day, second, rleid)

    rm(df_small) # free up space
    walking_raw = read_csv(here::here(
      "data",
      "lily",
      "data",
      "adept_walking_dfs",
      idf$version,
      paste0(idf$id, ".csv.gz"))) %>%
      filter(second_id %in% density$second_id) %>%
      mutate(id = subject)
    rm(density)
    write_csv(walking_raw, outfile)

  })

  rm(x)
  gc()
}

files = list.files(here::here("data", "lily", "data", "fingerprint_exs"),
                   full.names = TRUE)


all = map_dfr(files, read_csv)

all = all %>%
  mutate(good_pred = id %in% ids_high)

write_csv(all, here::here("data", "lily", "data", "fingerprint_exs", "all_data.csv.gz"))



