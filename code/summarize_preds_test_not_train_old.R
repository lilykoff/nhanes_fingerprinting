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

# read in one file
file = all_preds[1]
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

# number of train subjects
train_subs = x %>% filter(proptrain == .1) %>% pull(model)
# number of test subjects
test_subs = x %>% filter(proptrain == .1) %>% pull(true_subject)
temp =
  x %>%
  filter(proptrain == 0.1)

temp %>%
  group_by(true_subject) %>%
  summarize(
    maxprob = first(max(mean_pred)),
    predicted_sub = first(model[mean_pred == maxprob]),
    probsubj = first(mean_pred[true_subject == model])
  ) %>%
  filter(true_subject %in% train_subs)

# this is right (I think)
temp %>%
  group_by(true_subject) %>%
  mutate(rank = rank(-mean_pred)) %>%
  summarize(
    maxprob = first(max(mean_pred)),
    predicted_sub = first(model[mean_pred == maxprob]),
    probsubj = first(mean_pred[true_subject == model]),
    ranksub = first(rank[true_subject == model])
  ) %>% print(n=100)
  # filter(!(true_subject %in% train_subs))

# look at distribution of max probbaility
# two people: one from training, one from test
# how confident are you in person from training vs impostor
# mean median max of probabilities assigned to top 5 impostors - look at that vs. overlap
# ideally: low probabilities from impostors
# calc accuracy on high confidence cases

# i,jth entry probability that patient 4 (test) is patient 1 in training data
# if there's not separability in distributions
# take top 5 probabilities from impostor
overlap_res =
  temp %>%
  filter(true_subject %in% train_subs) %>%
  group_by(true_subject) %>%
  mutate(
    rank = rank(-mean_pred)
  ) %>%
  filter(model==true_subject) %>%
  mutate(
    rank1 = ifelse(rank == 1, 1, 0),
    rank5 = ifelse(rank <= 5, 1, 0)
  )

impostor_res =
  temp %>%
  filter(!(true_subject %in% train_subs)) %>%
  group_by(true_subject) %>%
  summarize(max_pred = max(mean_pred))


ncol(x) -1 == n_target
# we want to calculate accuracy where test set is various subset of training data

calc_accuracy = function(preds, pct_test = 0.5, n_target, type){
  n_test = floor((ncol(preds)-1) * pct_test)
  test = preds[, c(1:n_test, ncol(preds))]
  get_summarized_predictions(test, rank = TRUE) %>%
    ungroup() %>%
    summarize(rank1 = sum(rank1),
              rank5 = sum(rank5),
              n = n()) %>%
    mutate(fold = fold,
           n_tar = n_target,
           type = type) %>%
    filter(n == n_tar)

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
                            type = type)
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

