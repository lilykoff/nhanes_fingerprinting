library(tidyverse)
library(tidymodels)
# df = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells.csv.gz"))
df = readr::read_csv(here::here("data", "sample_grid_cells.csv.gz"))

nzv_trans =
  recipe(id ~ ., data = df) %>%
  step_nzv(all_predictors())


nzv_estimates = prep(nzv_trans)

nzv = colnames(juice(nzv_estimates))

df_nzv = df %>% select(id, all_of(nzv))

# make X matrix

X = df_nzv %>% select(starts_with("x")) %>% as.matrix()

n = length(unique(df_nzv$id))
each = unique(df_nzv %>% group_by(id) %>% count() %>% pull(n))
m = nrow(X)


generate_matrix = function(n_rows, n_cols, block_size) {
  # Initialize an empty matrix of zeros
  mat = matrix(0, nrow = n_rows, ncol = n_cols)

  # Fill in the 1s in a block-diagonal pattern
  for (i in 1:n_cols) {
    start_row <- (i - 1) * block_size + 1
    end_row <- start_row + block_size - 1
    if (end_row <= n_rows) {
      mat[start_row:end_row, i] <- 1
    }
  }

  return(mat)
}

generate_matrix2 = function(n_rows, n_cols, block_size) {
  # Initialize an empty matrix of zeros
  mat = matrix(0, nrow = n_rows, ncol = n_cols)

  # Fill in the 1s in a block-diagonal pattern
  for (i in 1:n_cols) {
    start_row <- sum(block_size[0:(i-1)]) + 1
    end_row <- start_row + block_size[i] - 1
    if (end_row <= n_rows) {
      mat[start_row:end_row, i] <- 1
    }
  }

  return(mat)
}

# Example usage
n_rows <- m
n_cols <- n
block_size <- each
Y <- generate_matrix(n_rows, n_cols, block_size)

mod = lm(Y ~ X)

set.seed(123)
initialsplit = initial_split(df_nzv, prop = 3/4, strata = id)

data_train = training(initialsplit)
data_test = testing(initialsplit)
n = length(unique(data_train$id))
m = nrow(X_train)
each = data_train %>% group_by(id) %>% count() %>% pull(n)
X_train = data_train %>% select(starts_with("x")) %>% as.matrix()
n_rows = m
n_cols = n
block_size = each
Y_train = generate_matrix2(n_rows, n_cols, block_size)


X_test = data_test %>% select(starts_with("x")) %>% as.matrix()
each_test = data_test %>% group_by(id) %>% count() %>% pull(n)
m_test = nrow(X_test)
n_rows_test = m_test
n_cols = n
block_size = each_test
Y_test = generate_matrix2(n_rows, n_cols, block_size)


  # Example usage
mod = lm(Y_train ~ X_train)

coefs = coef(mod)

preds = cbind(rep(1, nrow(X_test)), X_test) %*% coefs

ids_key = tibble(model = ids,
                 num = seq(1:length(ids)),
                 name = paste0("V", num)) %>%
  select(-num)

sum_preds =
  preds %>%
  as_data_frame() %>%
  bind_cols(data_test %>% select(id)) %>%
  group_by(id) %>%
  mutate(sec = row_number()) %>%
  pivot_longer(cols = -c("id", "sec")) %>%
  left_join(ids_key, by = c("name" = "name")) %>%
  rename(pred = value) %>%
  ungroup() %>%
  group_by(id, model) %>%
  summarize(mean_pred = mean(pred, na.rm = TRUE)) %>%
  group_by(id) %>%
  summarize(
    maxprob = first(max(mean_pred)),
    predicted_sub = first(model[mean_pred == maxprob]),
    probsubj = first(mean_pred[id == model])
  ) %>%
  mutate(correct = predicted_sub == id)


###### model fitting

set.seed(123)
initialsplit = initial_split(df, prop = 3/4, strata = id)
ids = unique(df$id)


data_train = training(initialsplit)
data_test = testing(initialsplit)


train = data_train
test = data_test
subject = ids[1]


# function to fit ovr logistic regression models
fit_model = function(subject, train, test) {
  train$class = factor(ifelse(train$id == subject, 1, 0))
  test$class = factor(ifelse(test$id == subject, 1, 0))
  tmp = train %>% dplyr::select(-id)
  tmp_test = test %>% dplyr::select(-id)

  # mod = svm(class ~ ., data = tmp, probability = TRUE)

  # svm_mod = svm_linear()

  using_formula =
    svm_linear() %>%
    set_engine("kernlab") %>%
    set_mode("classification") %>%
    fit(class ~ ., data = tmp)

  x = augment(using_formula, new_data = tmp_test)


  pred = predict(using_formula, new_data = tmp_test, type = "raw")

  mod =
    glm(class ~ ., data = tmp, family = binomial(link = "logit"))
  pred = predict.glm(mod, newdata = tmp_test, type = "response")
  return(pred)
}


data_train = training(initialsplit)
data_test = testing(initialsplit)

# break into folds
fit_model(subject = ids[1],train = data_train, test = data_test)

all_predictions =
  map_dfc(
    .x = ids_small,
    .f = fit_model,
    train = dat_nzv,
    test = dat_nzv_test
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

