# try fitting regression models on one fold (70 people)

library(tidyverse)
library(purrr)
library(tidymodels)
library(magrittr)
tidymodels_prefer()

ifold = get_fold()

fnames = readRDS(here::here("data", "raw", "all_filenames.rds"))
if(!is.null(ifold)){
  df = fnames %>% filter(fold == ifold)
}

# df = fnames %>% filter(fold == 1)
options(dplyr.summarise.inform = FALSE)

get_density = function(subject, df = df){
  idf = df %>% filter(id == subject)
  walking_df =
    readr::read_csv(here::here("data", "lily", "data", "fingerprint_data", idf$version, paste0(idf$id, ".csv.gz")))
  x = try({
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
    if(nrow(df_small) >= 180) {
      density =
        df_small %>%
        sample_n(size = 180, replace = FALSE) %>%
        mutate(id = subject) %>%
        select(id, starts_with("("), starts_with("["))
    } else {
      density = NULL
    }
  })
  x
}

grid_data_list =
  map(.x = df$id, .f = get_density, df = df)


grid_data = Filter(function(x) is_tibble(x), grid_data_list) %>%
  bind_rows()

set.seed(123)
initialsplit = initial_split(grid_data, prop = 3/4, strata = id)
ids = unique(grid_data$id)


data_train = training(initialsplit)
data_test = testing(initialsplit)
# function to fit ovr logistic regression models
fit_model <- function(subject, train, test) {
  train$class <- ifelse(train$id == subject, 1, 0)
  test$class <- ifelse(test$id == subject, 1, 0)
  tmp <- train %>% dplyr::select(-id)
  tmp_test <- test %>% dplyr::select(-id)
  mod <-
    glm(class ~ ., data = tmp, family = binomial(link = "logit"))
  pred <- predict.glm(mod, newdata = tmp_test, type = "response")
  return(pred)
}

# first we want to remove columns with near zero variance
nzv_trans <-
  recipe(id ~ ., data = data_train) %>%
  step_nzv(all_predictors())

nzv_estimates <- prep(nzv_trans)

nzv <- colnames(juice(nzv_estimates))
dat_nzv <- data_train %>% dplyr::select(id, all_of(nzv))
dat_nzv_test <- data_test %>% dplyr::select(id, all_of(nzv))

## now fit models, get predictions
t1 <- Sys.time()
all_predictions =
  map_dfc(
    .x = ids,
    .f = fit_model,
    train = dat_nzv,
    test = dat_nzv_test,
    .progress = T
  ) %>%
  janitor::clean_names()

Sys.time() - t1
# Time difference of 44.60639 secs


# column j is predicted probability that data in that row belong to subject j
# normalize probabilities
row_sums <- rowSums(all_predictions)

# normalize and add "true subject column"
all_predictions =
  all_predictions %>%
  bind_cols(sum = row_sums) %>%
  rowwise() %>%
  mutate(across(starts_with("x"), ~ .x / sum)) %>%
  dplyr::select(-sum) %>%
  ungroup() %>%
  bind_cols(true_subject = dat_nzv_test$id)

colnames(all_predictions) =
  c(paste("x", ids, sep = ""), "true_subject")
# print some results summaries

get_summarized_predictions <- function(predictions, long = FALSE) {
  if (long == T) {
    predictions %>%
      group_by(true_subject) %>%
      mutate(sec = row_number()) %>%
      pivot_longer(cols = -c("true_subject", "sec")) %>%
      mutate(model = as.numeric(sub(".*x", "", name))) %>%
      rename(pred = value) %>%
      ungroup() %>%
      group_by(true_subject, model) %>%
      summarize(mean_pred = mean(pred, na.rm = TRUE)) %>%
      mutate(correct = ifelse(true_subject == model, 1, 0))
  }
  else{
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
      mutate(correct = ifelse(as.numeric(predicted_sub) == true_subject, 1, 0))
  }
}

res = get_summarized_predictions(all_predictions)
res %>% print(n = Inf)
sum(res$correct) / nrow(res)

# fn takes number of seconds to average over as input, outputs classification stats
get_prediction_stats <- function(predictions, seconds) {
  predictions %>%
    group_by(true_subject) %>%
    mutate(sec = floor(row_number() / seconds)) %>%
    pivot_longer(cols = -c("true_subject", "sec")) %>%
    mutate(model = as.numeric(sub(".*x", "", name))) %>%
    rename(pred = value) %>%
    ungroup() %>%
    group_by(true_subject, model, sec) %>%
    summarize(mean_pred = mean(pred, na.rm = TRUE)) %>%
    group_by(true_subject, sec) %>%
    summarize(maxprob = max(mean_pred),
              predicted_subject = model[mean_pred == maxprob]) %>% ungroup() %>%
    dplyr::select(c(true_subject, predicted_subject)) %>%
    mutate(across(1:2, as.factor)) %>%
    yardstick::conf_mat(., truth = true_subject, estimate = predicted_subject) %>%
    summary() %>%
    mutate(s = seconds)
}

results_summarized <-
  map_dfr(
    .x = c(1, seq(10, 100, 10)),
    .f = get_prediction_stats,
    predictions = all_predictions_IU,
    .progress = T
  ) %>%
  filter(.metric != "detection_prevalence")
supp.labs <-
  c(
    "Accuracy",
    "Kappa",
    "Sensitivity",
    "Specificity",
    "PPV",
    "NVP",
    "MCC",
    "J Index",
    "Balanced Accuracy",
    "Precision",
    "Recall",
    "F1 Score"
  )
names(supp.labs) <- unique(results_summarized$.metric)

results_summarized %>%
  ggplot(aes(x = s, y = .estimate, col = .metric)) +
  geom_point() +
  geom_line() +
  theme_light() +
  facet_wrap(. ~ .metric,
             scales = "free_y",
             labeller = labeller(.metric = supp.labs)) +
  labs(x = "Number of Seconds", y = "Estimate") +
  scale_x_continuous(breaks = c(1, seq(10, 100, 10))) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(legend.position = "none")

## think about different train/test paradigms
# impostors etc.


