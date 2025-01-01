library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE
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
        rank1 = if_else(rank == 1, 1, 0),
        rank5 = if_else(rank <= 5, 1, 0)
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
      mutate(correct = if_else(as.numeric(predicted_sub) == true_subject, 1, 0)) %>%
      ungroup()
  }
}

all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res"),
                       # recursive = TRUE,
                       full.names = TRUE,
                       pattern = "rds")
# over = list.files(here::here("data", "lily", "data", "fingerprint_res"),
#                          recursive = TRUE,
#                          full.names = TRUE,
#                          pattern = "over.rds")
# all_preds = all_preds[!(all_preds %in% over)]

# all_res = map_dfr(.x = over,
#                   .f = function(file){
#                     x = readRDS(file)
#                     fold = sub(".*fold\\_(.+)\\_.*", "\\1", basename(file))
#                     get_summarized_predictions(x, rank = TRUE) %>%
#                       ungroup() %>%
#                       summarize(rank1 = sum(rank1),
#                                 rank5 = sum(rank5),
#                                 n = n()) %>%
#                       mutate(fold = fold)
#                   })

all_res = map_dfr(.x = all_preds,
              .f = function(file){
                x = readRDS(file)
                if(grepl("over", file)){
                  fold = sub("^[^_]*_([^_]*)_.*", "\\1", basename(file))
                  n_target = as.numeric(sub(".*\\_(.+)\\_over.rds.*", "\\1", basename(file)))
                  type = "over"
                } else {
                  fold = sub(".*fold\\_(.+)\\_.*", "\\1", basename(file))
                  n_target = as.numeric(sub(".*\\_(.+).rds.*", "\\1", basename(file)))
                  type = "regular"
                }

                get_summarized_predictions(x, rank = TRUE) %>%
                  ungroup() %>%
                  mutate(rank1pct = (rank <= n_target * 0.01) * 1,
                         rank5pct = (rank <= n_target * 0.05) * 1) %>%
                  select(-rank) %>%
                  summarize(across(contains("rank"), sum),
                            n = n()) %>%
                  mutate(fold = fold,
                         n_tar = n_target,
                         type = type) %>%
                  filter(n == n_tar)
              })
saveRDS(all_res, here::here("data", "lily", "data", "fingerprint_res_all.rds"))
all_res %>% mutate(across(contains("rank"), ~.x / n * 100)) %>% group_by(n) %>% summarize(across(contains("rank"), ~mean(.x)))

# n rank1 rank5
# <int> <dbl> <dbl>
# 1   100  77.5  95.5
# 2   200  65.8  89.2
# 3   267  59.2  84.3
# 4   300  58.5  84.2
# 5   500  49.2  76
# 6  1000  37.5  63.9

######### old
# all_res = readRDS(here::here("data", "lily", "data", "fingerprint_res_all.rds"))
# all_res = readRDS(here::here("data", "fingerprint_res_all.rds"))
# all_res %>% mutate(across(contains("rank"), ~.x / n * 100)) %>%
#   mutate(n = if_else(grepl("\\_", fold), paste0(n, "_boost"), paste0(n))) %>%
#   group_by(n) %>% summarize(across(contains("rank"), ~mean(.x)))

