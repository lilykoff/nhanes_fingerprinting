require(xgboost)
library(tidyverse)
library(tidymodels)
library(Matrix)

# https://juliasilge.com/blog/xgboost-tune-volleyball/

# source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE


df = read_csv(here::here("data", "sample_grid_cells.csv.gz"))
subject = df$id[1]
df = df %>%
  mutate(label = factor(if_else(id == subject, 1, 0)))

set.seed(123)
initialsplit = initial_split(df, prop = 3 / 4, strata = id)
data_train = training(initialsplit)
data_test = testing(initialsplit)

cv_folds = data_train %>%
  rsample::vfold_cv(v = 5, strata = id)

# model spec

xgb_spec = boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  ## first three: model complexity
  sample_size = tune(),
  mtry = tune(),
  ## randomness
  learn_rate = tune()) %>%                           ## step size)
    set_engine("xgboost") %>%
    set_mode("classification")

xgb_grid = dials::grid_space_filling(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), data_train),
  learn_rate(),
  size = 10 # CHANGE LATER
)


# workflow
xgb_wf = workflow() %>%
  add_variables(outcomes = label,
                predictors = starts_with("x")) %>%
  add_model(xgb_spec)

xgb_wf

doParallel::registerDoParallel()

set.seed(234)
xgb_res = tune_grid(
  xgb_wf,
  resamples = cv_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = FALSE)
)

collect_metrics(xgb_res)

# xgb_res %>%
#   collect_metrics() %>%
#   filter(.metric == "accuracy") %>%
#   select(mean, mtry:sample_size) %>%
#   pivot_longer(mtry:sample_size, values_to = "value", names_to = "parameter") %>%
#   ggplot(aes(value, mean, color = parameter)) +
#   geom_point(alpha = 0.8, show.legend = FALSE) +
#   facet_wrap( ~ parameter, scales = "free_x") +
#   labs(x = NULL, y = "AUC")

# show_best(xgb_res, metric = "accuracy")

best_acc = select_best(xgb_res, metric = "accuracy")

final_xgb = finalize_workflow(xgb_wf, best_acc)



final_res = last_fit(final_xgb, initialsplit)

preds = final_res %>%
  collect_predictions() %>%
  pull(.pred_1)

data_test %>%
  mutate(pred = preds) %>%
  group_by(label) %>%
  summarize(across(pred, mean))
