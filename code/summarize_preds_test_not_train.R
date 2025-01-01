library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE
options(dplyr.summarise.inform = FALSE)

all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res", "test_not_train"),
                       # recursive = TRUE,
                       full.names = TRUE,
                       pattern = "rds")


get_probs = function(df){
  df %>%
    group_by(proptrain, true_subject) %>%
    mutate(rank = rank(-mean_pred)) %>%
    summarize(
      maxprob = first(max(mean_pred)),
      predicted_sub = first(model[mean_pred == maxprob]),
      probsubj = first(mean_pred[true_subject == model]),
      ranksub = first(rank[true_subject == model])
    )
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
                    get_probs(x) %>%
                      mutate(fold = fold,
                             n_tar = n_target,
                             type = type,
                             n = n()) %>%
                      filter(n == n_tar)
                  })
saveRDS(all_res, here::here("data", "lily", "data", "fingerprint_res_test_not_train.rds"))
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

