library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE
options(dplyr.summarise.inform = FALSE)
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

all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res"),
                       # recursive = TRUE,
                       full.names = TRUE,
                       pattern = "rds")




calc_accuracy = function(preds, pct_test = 0.5, n_target, type, fold){
  n_test = floor((ncol(preds)-1) * pct_test)
  test = preds[, c(1:n_test, ncol(preds))]

  get_summarized_predictions(test, rank = TRUE) %>%
    ungroup() %>%
    summarize(rank1 = sum(rank1),
              rank5 = sum(rank5),
              n_test = n()) %>%
    mutate(fold = fold,
           n_train = n_target,
           type = type)
}



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

                    map_dfr(.x = seq(0.1, 0.9, by = 0.1),
                            .f = calc_accuracy,
                            preds = x,
                            n_target = n_target,
                            type = type,
                            fold = fold)
                  })
saveRDS(all_res, here::here("data", "lily", "data", "fingerprint_res_train_not_test.rds"))
# all_res %>% mutate(across(contains("rank"), ~.x / n * 100)) %>% group_by(n) %>% summarize(across(contains("rank"), ~mean(.x)))

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

