### pca

library(tidyverse)
library(Matrix)
library(Rfast)
library(tidymodels)

force = FALSE

outfiles = c(here::here("data", "lily", "data", "dat_train_pca.rds"),
             here::here("data", "lily", "data", "dat_test_pca.rds"))

if(!all(file.exists(outfiles)) || force) {
  xdf = read_csv(here::here("data", "lily", "data", "all_grid_cells.csv.gz"))
               # n_max = 10e4)

  set.seed(123)
  initialsplit = initial_split(xdf, prop = 3/4, strata = id)

  rm(xdf)

  data_train = training(initialsplit)
  data_test = testing(initialsplit)

  rm(initialsplit)

  train_ids = data_train$id
  test_ids = data_test$id

  train_mat = data_train %>% select(-id) %>% as.matrix()
  test_mat = data_test %>% select(-id) %>% as.matrix()

  rm(data_train, data_test)
  gc()

  v = colVars(train_mat)
  zero_var = which(v <= 2e-16)

  train_mat = train_mat[, -zero_var]
  test_mat = test_mat[, -zero_var]


  pca_fit = prcomp(train_mat,
                   center = TRUE,
                   scale = TRUE)

  rm(train_mat)

  cum_var = summary(pca_fit)$importance[3,]
  num_pcs = min(which(cum_var > .9))

  score_df = pca_fit$x[, 1:num_pcs] %>% as.data.frame()
  score_df$id = train_ids


  test_score_df = predict(pca_fit, newdata = test_mat)[, 1:num_pcs] %>% as.data.frame()
  test_score_df$id = test_ids

  # check dimensions
  # dim(new_scores)
  # dim(score_df)

  rm(test_mat, pca_fit, train_ids, test_ids)

  write_rds(score_df, outfiles[1], compress = "xz")
  write_rds(test_score_df, outfiles[2], compress = "xz")
  rm(score_df, test_score_df)
}


#### old
#
# X_mat = x %>% select(-id) %>% as.matrix()
# rm(x)
# # remove cols with 0 variance
# v = colVars(X_mat)
# zero_var = which(v <= 2e-16)
#
# X_mat = X_mat[, -zero_var]
# pca_fit = prcomp(X_mat,
#                  center = TRUE,
#                  scale = TRUE)
# screeplot(pca_fit, type = "lines")
# summary(pca_fit)
#
# # str(summary(pca_fit))
# # dim(summary(pca_fit)$importance)
# cum_var = summary(pca_fit)$importance[3,]; cum_var
# num_pcs = min(which(cum_var > .9))
#
#
# # find first index of cum_var greater than 0.99
#
#
#
# # take PCs that explain 99% of variance
# # save matrix, use in prediction
