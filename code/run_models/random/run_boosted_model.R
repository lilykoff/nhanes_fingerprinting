## model boosting

library(tidyverse)
source(here::here("code", "R", "utils.R"))
source(here::here("data", "lily", "code", "summary_fns.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE
ifold = get_fold()


filenames = read_rds(here::here("data", "lily", "data", "fingerprint_folds.rds"))
filenames =
  filenames %>%
  mutate(fold = rep(1:200, each = 67)[1:13367])

files = list.files(here::here("data", "lily", "data", "preds_13367"),
                   recursive = TRUE,
                   full.names = TRUE)

ids = filenames %>%
  filter(fold == ifold)  %>%
  pull(id) %>%
  as.character()

if(!file.exists(here::here("data", "lily", "data", "preds_13367.rds"))){
  all_preds =
    map_dfr(files, read_rds)
} else{
  all_preds = read_rds(here::here("data", "lily", "data", "preds_13367.rds"))
}

all_preds =
  all_preds %>%
  filter(true_subject %in% ids)
# all_preds =
#   map_dfr(files, read_rds)

# write_rds(all_preds, here::here("data", "lily", "data", "preds_13367.rds"))

if(!dir.exists(here::here("data", "lily", "data", "13367_boosted"))){
  dir.create(here::here("data", "lily", "data", "13367_boosted"),
             recursive = TRUE)
}

data_train = read_rds(here::here("data", "lily", "data", "dat_nzv_train.rds")) %>%
  mutate(id = as.character(id))
data_test = read_rds(here::here("data", "lily", "data", "dat_nzv_test.rds")) %>%
  mutate(id = as.character(id))

fit_model = function(subject, train, test) {
  train$class <- ifelse(train$id == subject, 1, 0)
  tmp <- train %>% dplyr::select(-id)
  tmp_test <- test %>% dplyr::select(-id)
  mod <-
    glm(class ~ ., data = tmp, family = binomial(link = "logit"))
  pred <- predict.glm(mod, newdata = tmp_test, type = "response")
  return(pred)
}


run_model_100 = function(id) {
  x = try({
    top_100 =
    all_preds %>%
    filter(true_subject == id) %>%
    arrange(desc(mean_pred)) %>%
    slice(1:133) %>%
    pull(model)

  train = data_train %>% filter(id %in% top_100)
  test = data_test %>% filter(id %in% top_100)
  ## now fit models, get predictions
  all_predictions =
    map_dfc(
      .x = top_100,
      .f = fit_model,
      train = train,
      test = test,
      .progress = T
    ) %>%
    janitor::clean_names()

  # column j is predicted probability that data in that row belong to subject j
  # normalize probabilities
  row_sums = rowSums(all_predictions)

  # normalize and add "true subject column"
  all_predictions =
    all_predictions %>%
    bind_cols(sum = row_sums) %>%
    rowwise() %>%
    mutate(across(starts_with("x"), ~ .x / sum)) %>%
    dplyr::select(-sum) %>%
    ungroup() %>%
    bind_cols(true_subject = test$id)

  colnames(all_predictions) =
    c(paste("x", top_100, sep = ""), "true_subject")

  res = get_summarized_predictions(all_predictions, rank = TRUE) %>%
    filter(true_subject == id)
  rm(all_predictions)
  rm(row_sums)
  rm(train)
  rm(test)
  rm(top_100)
  res
  })
  x
}

outfile = here::here("data", "lily", "data", "13367_boosted", paste0("fold_", ifold, ".rds"))
if(!file.exists(outfile) || force){
  fold_res =
    map(.x = ids,
        .f = run_model_100,
        .progress = TRUE)

  final_res =
    fold_res %>%
    keep(., is.data.frame) %>%
    bind_rows()

  write_rds(final_res, outfile)
}

#  x = list(df = tibble(seq(1:10)), x2 = "try error")
# x %>%
#   keep(., is.data.frame) %>%
#   bind_rows()


# test = read_rds(here::here("data", "lily", "data", "preds_13367", paste0(66080, ".rds")))

