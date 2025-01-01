library(tidyverse)

pct_files = list.files(here::here("data", "lily", "data",
                                  "fingerprint_res"), pattern = "*pct*", full.names = TRUE, recursive = TRUE)

pct_files
# read in one file
x = read_rds(pct_files[1])
# list of 5 items corresp to different % oversampled: 10, 25, 50, 75, 90


key = tibble(var = as.character(1:5),
             pctov = c(10, 25, 50, 75, 90))
get_summarized_predictions = function(predictions, rank = FALSE) {
  if (rank) {
    predictions %>%
      group_by(true_subject) %>%
      mutate(sec = row_number()) %>%
      pivot_longer(cols = -c("true_subject", "sec")) %>%
      mutate(model = as.numeric(sub(".*x", "", name))) %>%
      rename(pred = value) %>%
      ungroup() %>%
      group_by(true_subject, model) %>%
      summarize(mean_pred = mean(pred, na.rm = TRUE)) %>%
      group_by(true_subject) %>%
      mutate(
        rank = rank(-mean_pred)
      ) %>%
      filter(model==true_subject) %>%
      mutate(
        rank1 = ifelse(rank == 1, 1, 0),
        rank5 = ifelse(rank <= 5, 1, 0)
      )
  } else {
    predictions %>%
      group_by(true_subject) %>%
      mutate(sec = row_number()) %>%
      pivot_longer(cols = -c("true_subject", "sec")) %>%
      mutate(model = as.numeric(sub(".*x", "", name))) %>%
      rename(pred = value) %>%
      ungroup() %>%
      group_by(true_subject, model) %>%
      summarize(mean_pred = mean(pred, na.rm = TRUE)) %>%
      group_by(true_subject) %>%
      summarize(
        maxprob = first(max(mean_pred)),
        predicted_sub = first(model[mean_pred == maxprob]),
        probsubj = first(mean_pred[true_subject == model])
      ) %>%
      mutate(correct = ifelse(as.numeric(predicted_sub) == true_subject, 1, 0)) %>%
      ungroup()
  }
}
# small =
#   x %>%
#   group_by(pct_oversample) %>%
#   sample_n(100) %>%
#   ungroup()


res_all = map(.x = pct_files,
              .f = function(x){
                res = read_rds(x) %>%
                  group_by(pct_oversample) %>%
                  group_map(~ get_summarized_predictions(.x, rank = TRUE) %>%
                              ungroup() %>%
                              summarize(n = n(),
                                        across(c("rank1", "rank5"), sum))) %>%
                  bind_rows(.id = "pct")
                rm(x)
                res
              }) %>%
  bind_rows(.id = "fold") %>%
  left_join(key, by = c("pct" = "var"))


saveRDS(res_all, here::here("data", "lily", "data", "res_pct.rds"))


# results = x %>%
#   group_by(pct_oversample) %>%
#   group_map(~ get_summarized_predictions(.x, rank = TRUE) %>%
#               ungroup() %>%
#               summarize(n = n(),
#                         across(c("rank1", "rank5"), sum))) %>%
#   bind_rows(.id = "pct")


# pct       n rank1 rank5 pctov
# <chr> <int> <dbl> <dbl> <dbl>
#   1 1      1000 0.716 0.888    10
# 2 2      1000 0.76  0.925    25
# 3 3      1000 0.634 0.868    50
# 4 4      1000 0.319 0.635    75
# 5 5      1000 0.13  0.353    90
