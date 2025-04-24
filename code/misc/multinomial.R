library(tidyverse)
library(tidymodels)
# install.packages("VGAM")
library(VGAM)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE

filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds.rds"))
folds_keep = filenames %>%
  group_by(fold) %>%
  count() %>%
  filter(n == 100) %>%
  pull(fold)

ifold = get_fold()
if (!is.na(ifold)) {
  fdf = filenames %>%
    filter(fold == ifold)
}

# 100 subs per fold
xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells.csv.gz"))

df =
  xdf %>%
  filter(id %in% fdf$id) %>%
  mutate(id = factor(id))

set.seed(123)
initialsplit = initial_split(df, prop = 3/4, strata = id)

data_train = training(initialsplit)
data_test = testing(initialsplit)

library(nnet)

nzv_trans =
  recipe(id ~ ., data = data_train) %>%
  step_nzv(all_predictors())

nzv_estimates = prep(nzv_trans)

nzv = colnames(juice(nzv_estimates))
dat_nzv = data_train %>% dplyr::select(id, all_of(nzv))
dat_nzv_test = data_test %>% dplyr::select(id, all_of(nzv))
# Fit multinomial logistic regression
# Assuming 'response' is a categorical variable with more than 2 levels
# and 'predictor1', 'predictor2', etc. are predictor variables in the data
# model = multinom(id ~ ., data = dat_nzv)
model = vglm(id ~ ., data = dat_nzv, family = multinomial)

# Summary of the model
summary(model)

# Predicted probabilities for each class
# predictions = predict(model, type = "probs", newdata = dat_nzv_test)
predictions = predict(model, type = "response", newdata = dat_nzv_test)



saveRDS(predictions, here::here("data", "lily", "data", "multinom_model"))
