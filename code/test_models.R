library(tidyverse)
library(tidymodels)
library(corrr)
library(ggcorrplot)
library(factoextra)
df = readr::read_csv(here::here("data", "sample_grid_cells.csv.gz"))

nzv_trans =
  recipe(id ~ ., data = df) %>%
  step_nzv(all_predictors())


nzv_estimates = prep(nzv_trans)

nzv = colnames(juice(nzv_estimates))

df_nzv = df %>% select(id, all_of(nzv))
df_nzv_scale =
  df_nzv %>%
  select(-id) %>%
  mutate(across(starts_with("x"), ~ (.x - mean(.x)) / sd(.x)))

df_nzv_pca = princomp(df_nzv_scale)
summary(df_nzv_pca)
df_nzv_pca$loadings[, 1:2]

fviz_eig(df_nzv_pca, addlabels = TRUE)
fviz_pca_var(df_nzv_pca, col.var = "black")

fviz_pca_var(df_nzv_pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)
###### model fitting

set.seed(123)
initialsplit = initial_split(df, prop = 3/4, strata = id)
ids = unique(df$id)


data_train = training(initialsplit)
data_test = testing(initialsplit)
# function to fit ovr logistic regression models
fit_model = function(subject, train, test) {
  train$class <- ifelse(train$id == subject, 1, 0)
  test$class <- ifelse(test$id == subject, 1, 0)
  tmp = train %>% dplyr::select(-id)
  tmp_test = test %>% dplyr::select(-id)
  mod =
    glm(class ~ ., data = tmp, family = binomial(link = "logit"))
  pred <- predict.glm(mod, newdata = tmp_test, type = "response")
  return(pred)
}
ids_small = ids[1:100]
dat_nzv = data_train %>% dplyr::select(id, all_of(nzv)) %>%
  filter(id %in% ids_small)
dat_nzv_test = data_test %>% dplyr::select(id, all_of(nzv)) %>%
  filter(id %in% ids_small)

## now fit models, get predictions
t1 = Sys.time()
all_predictions =
  map_dfc(
    .x = ids_small,
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
row_sums = rowSums(all_predictions)

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
  c(paste("x", ids_small, sep = ""), "true_subject")
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

