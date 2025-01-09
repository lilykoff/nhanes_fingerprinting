library(tidyverse)
library(tidymodels)
library(future)
library(furrr)
# plan(multicore, workers = 8)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = TRUE
filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds.rds"))
filenames_temp = readRDS(here::here("data", "lily", "data", "fingerprint_folds_temporal.rds"))
if (!dir.exists(here::here("data", "lily", "data", "fingerprint_prediction_results"))) {
  dir.create(here::here(
    "data",
    "lily",
    "data",
    "fingerprint_prediction_results"
  ))
}

get_summarized_predictions = function(predictions, rank = FALSE, exp = FALSE) {
  # predictions is tibble
  # column j is the predictions from fitting the model where the "true subject" is subject j
  # each row is prediction for a given second
  # true subject is the final column

  if (rank) {
    # will return data frame with columns true subject, model, mean prediction, rank of correct prediction,
    # and rank1, rank5 which indicate whether predicted subject was in top 1 or top 5
    predictions %>%
      group_by(true_subject) %>%
      mutate(sec = row_number()) %>%
      pivot_longer(cols = -c("true_subject", "sec"), names_to = "name", values_to = "pred") %>%
      mutate(model = as.numeric(sub(".*x", "", name))) %>%
      select(-name) %>%
      # now we have the prediction for each second for each model / true subject combo
      mutate(pred = case_when(exp ~ exp(pred),
                              .default = pred)) %>% # exponentiate based on exp argument
      ungroup() %>%
      group_by(true_subject, model) %>%
      # get mean probability across seconds for each true subject / model combo
      summarize(mean_pred = mean(pred, na.rm = TRUE), .groups = "drop") %>%
      group_by(true_subject) %>%
      mutate(
        rank = rank(-mean_pred)
      ) %>% # get the rank for each prediction
      ungroup() %>%
      filter(model == true_subject) %>% # only keep the correct combos and get ranks
      mutate(
        rank1 = if_else(rank == 1, 1, 0),
        rank5 = if_else(rank <= 5, 1, 0)
      )
  } else {
    predictions %>%
      group_by(true_subject) %>%
      mutate(sec = row_number()) %>%
      pivot_longer(cols = -c("true_subject", "sec"), names_to = "name", values_to = "pred") %>%
      mutate(model = as.numeric(sub(".*x", "", name))) %>%
      select(-name) %>%
      # now we have the prediction for each second for each model / true subject combo
      mutate(pred = case_when(exp ~ exp(pred),
                              .default = pred)) %>% # exponentiate based on exp argument
      ungroup() %>%
      group_by(true_subject, model) %>%
      summarize(mean_pred = mean(pred, na.rm = TRUE), .groups = "drop") %>%
      group_by(true_subject) %>%
      summarize(
        maxprob = first(max(mean_pred)),
        predicted_sub = first(model[mean_pred == maxprob]),
        probsubj = first(mean_pred[true_subject == model])
      ) %>%
      mutate(correct = if_else(as.numeric(predicted_sub) == true_subject, 1, 0)) %>%
      ungroup()
  }
}

###########
## summarize predictions for "regular" models
dirs = c("100", "250", "500" ,"1000", "2500", "5000", "10000", "13367")
# dir = dirs[1]
# dir = dirs[5]
for (dir in dirs) {
  dirnum = as.numeric(dir)
  outfile = here::here("data", "lily", "data","fingerprint_prediction_results",
                       paste0("prediction_res_", dir, ".rds"))

  if(!file.exists(outfile) || force) {
    if(dirnum <= 1000){
      all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res", dir),
                             recursive = TRUE,
                             full.names = TRUE,
                             pattern = "rds")
      x = try({
        summary = map_dfr(.x = all_preds,
                          .f = function(file){
                            x = readRDS(file)

                            fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))
                            n_target = as.numeric(sub(".*fingerprint_res\\/(.+)\\/fold.*", "\\1", file))

                            res = get_summarized_predictions(x, rank = TRUE) %>%
                              mutate(rank1pct = (rank <= n_target * 0.01) * 1,
                                     rank5pct = (rank <= n_target * 0.05) * 1) %>%
                              select(-rank) %>%
                              summarize(across(contains("rank"), sum),
                                        n = n()) %>%
                              mutate(fold = fold,
                                     n_tar = n_target) %>%
                              filter(n == n_tar)
                            rm(x); rm(fold); rm(n_target)
                            res
                          })
        write_rds(summary, outfile, compress = "xz")
      })
      rm(x)
    } else { # in this scenario, we have one file for each model
      x = try({
        # figure out how many folds there were for this number of subjects
        x = ceiling(nrow(filenames) / dirnum)
        filenames = filenames %>%
          mutate(fold = rep(1:x, each = dirnum)[1:nrow(filenames)])

        folds = filenames %>%
          count(fold) %>%
          filter(n == dirnum) %>%
          pull(fold)

        # for each fold, need to read in files and put into one data frame to summarize predictions
        summary = map_dfr(
          .x = folds,
          .f = function(f) {
            ids = filenames %>% filter(fold == f) %>% pull(id) %>% as.character()
            files = file.path(here::here(
              "data",
              "lily",
              "data",
              "fingerprint_res",
              dir,
              paste(ids, ".rds", sep = "")
            ))
            # figure out which test data to use
            if(dirnum < 13367){
              dat_nzv_test = read_rds(here::here("data", "lily", "data", paste0("dat_nzv_test_", dir, "_", f, ".rds"))) %>%
              mutate(id = as.character(id)) %>%
              filter(id %in% ids) } else {
                dat_nzv_test = read_rds(here::here("data", "lily", "data", "dat_nzv_test.rds")) %>%
                  mutate(id = as.character(id)) %>%
                  filter(id %in% ids)
              }

            true_sub_vec = dat_nzv_test$id
            rm(dat_nzv_test) # save memory

            all_preds =
              map(
                .x = files,
                .f = function(x) {
                  read_rds(x) %>% as_tibble()
                }
              ) %>%
              bind_cols()
            # normalize probabilities
            row_sums = rowSums(all_preds)

            # normalize and add "true subject column"
            all_preds =
              all_preds %>%
              bind_cols(sum = row_sums) %>%
              rowwise() %>%
              mutate(across(-sum, ~ .x / sum)) %>%
              dplyr::select(-sum) %>%
              ungroup() %>%
              bind_cols(true_subject = true_sub_vec)

            colnames(all_preds) = c(ids, "true_subject")
            rm(row_sums)

            res = get_summarized_predictions(all_preds, rank = TRUE) %>%
              ungroup() %>%
              mutate(
                rank1pct = (rank <= dirnum * 0.01) * 1,
                rank5pct = (rank <= dirnum * 0.05) * 1
              ) %>%
              select(-rank) %>%
              summarize(across(contains("rank"), sum), n = n()) %>%
              mutate(fold = f, n_target = dirnum) %>%
              filter(n == dirnum)
            rm(all_preds)
            res
          }
        )
        write_rds(summary, outfile, compress = "xz")
      })

      rm(x)
    }

  }
}

############## summarize predictions for temporal models
dirs = c("100", "250", "500" ,"1000", "2500", "5000", "10000", "11225")
dir = dirs[1]
for (dir in dirs) {
  dirnum = as.numeric(dir)
  outfile = here::here("data", "lily", "data","fingerprint_prediction_results",
                       paste0("prediction_res_temporal_", paste0(dir, ".rds")))

  if(!file.exists(outfile) || force) {
    if(dirnum <= 1000){
      all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal", dir),
                             recursive = TRUE,
                             full.names = TRUE,
                             pattern = "rds")
      x = try({
        summary = map_dfr(.x = all_preds,
                          .f = function(file){
                            x = readRDS(file)

                            fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))
                            n_target = as.numeric(sub(".*fingerprint_res_temporal\\/(.+)\\/fold.*", "\\1", file))

                            res = get_summarized_predictions(x, rank = TRUE) %>%
                              ungroup() %>%
                              mutate(rank1pct = (rank <= n_target * 0.01) * 1,
                                     rank5pct = (rank <= n_target * 0.05) * 1) %>%
                              select(-rank) %>%
                              summarize(across(contains("rank"), sum),
                                        n = n()) %>%
                              mutate(fold = fold,
                                     n_tar = n_target) %>%
                              filter(n == n_tar)
                            rm(x); rm(fold); rm(n_target)
                            res
                          })
        write_rds(summary, outfile, compress = "xz")
      })
      rm(x)
    } else {
      x = try({
        x = ceiling(nrow(filenames_temp) / dirnum)
        filenames_temp = filenames_temp %>%
          mutate(fold = rep(1:x, each = dirnum)[1:nrow(filenames_temp)])

        folds = filenames_temp %>%
          count(fold) %>%
          filter(n == dirnum) %>%
          pull(fold)


        summary = map_dfr(
          .x = folds,
          .f = function(f) {
            ids = filenames_temp %>% filter(fold == f) %>% pull(id) %>% as.character()
            files = file.path(here::here(
              "data",
              "lily",
              "data",
              "fingerprint_res_temporal",
              dir,
              paste(ids, ".rds", sep = "")
            ))

            if(dirnum < 11225){
              dat_nzv_test = read_rds(here::here("data", "lily", "data", paste0("dat_nzv_test_temporal_", dir, "_", f, ".rds"))) %>%
              mutate(id = as.character(id)) %>%
              filter(id %in% ids)} else {
                dat_nzv_test = read_rds(here::here("data", "lily", "data", "dat_nzv_test_temporal.rds")) %>%
                  mutate(id = as.character(id)) %>%
                  filter(id %in% ids)
              }

            true_sub_vec = dat_nzv_test$id
            rm(dat_nzv_test)

            all_preds =
              map(
                .x = files,
                .f = function(x) {
                  read_rds(x) %>% as_tibble()
                }
              ) %>%
              bind_cols()
            # normalize probabilities
            row_sums = rowSums(all_preds)

            # normalize and add "true subject column"
            all_preds =
              all_preds %>%
              bind_cols(sum = row_sums) %>%
              rowwise() %>%
              mutate(across(-sum, ~ .x / sum)) %>%
              dplyr::select(-sum) %>%
              ungroup() %>%
              bind_cols(true_subject = true_sub_vec)

            colnames(all_preds) = c(ids, "true_subject")
            rm(row_sums)

            res = get_summarized_predictions(all_preds, rank = TRUE) %>%
              ungroup() %>%
              mutate(
                rank1pct = (rank <= dirnum * 0.01) * 1,
                rank5pct = (rank <= dirnum * 0.05) * 1
              ) %>%
              select(-rank) %>%
              summarize(across(contains("rank"), sum), n = n()) %>%
              mutate(fold = f, n_target = dirnum) %>%
              filter(n == dirnum)
            rm(all_preds)
            res
          }
        )
        write_rds(summary, outfile, compress = "xz")
      })

      rm(x)
    }

  }
}


### xgboost
dirs = c("100") # only one completed
dir = dirs[1]
for (dir in dirs) {
  dirnum = as.numeric(dir)
  outfile = here::here("data", "lily", "data","fingerprint_prediction_results",
                       paste0("prediction_res_", paste0(dir, "xgb", ".rds")))

  if(!file.exists(outfile) || force) {
    if(dirnum <= 100){
      all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res", paste0(dir, "xgb")),
                             recursive = TRUE,
                             full.names = TRUE,
                             pattern = "rds")
      x = try({
        summary = map_dfr(.x = all_preds,
                          .f = function(file){
                            x = readRDS(file)

                            fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))
                            n_target = as.numeric(sub(".*fingerprint_res\\/(.+)xgb.*", "\\1", file))

                            res = get_summarized_predictions(x, rank = TRUE) %>%
                              ungroup() %>%
                              mutate(rank1pct = (rank <= n_target * 0.01) * 1,
                                     rank5pct = (rank <= n_target * 0.05) * 1) %>%
                              select(-rank) %>%
                              summarize(across(contains("rank"), sum),
                                        n = n()) %>%
                              mutate(fold = fold,
                                     n_tar = n_target) %>%
                              filter(n == n_tar)
                            rm(x); rm(fold); rm(n_target)
                            res
                          })
        write_rds(summary, outfile, compress = "xz")
      })
      rm(x)
    } else {
      x = try({
        x = ceiling(nrow(filenames) / dirnum)
        filenames = filenames %>%
          mutate(fold = rep(1:x, each = dirnum)[1:nrow(filenames)])

        folds = filenames %>%
          count(fold) %>%
          filter(n == dirnum) %>%
          pull(fold)
        xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells.csv.gz"))


        summary = map_dfr(
          .x = folds,
          .f = function(f) {
            ids = filenames %>% filter(fold == f) %>% pull(id) %>% as.character()
            df =
              xdf %>%
              filter(id %in% ids)

            set.seed(123)
            is = initial_split(df, prop = 3/4, strata = id)
            test = testing(is)
            rm(df); rm(is)
            files = file.path(here::here(
              "data",
              "lily",
              "data",
              "fingerprint_res",
              paste0(dir, "xgb"),
              paste(ids, ".rds", sep = "")
            ))


            true_sub_vec = test$id

            all_preds =
              map(
                .x = files,
                .f = function(x) {
                  read_rds(x) %>% as_tibble()
                }
              ) %>%
              bind_cols()
            # normalize probabilities
            row_sums = rowSums(all_preds)

            # normalize and add "true subject column"
            all_preds =
              all_preds %>%
              bind_cols(sum = row_sums) %>%
              rowwise() %>%
              mutate(across(-sum, ~ .x / sum)) %>%
              dplyr::select(-sum) %>%
              ungroup() %>%
              bind_cols(true_subject = true_sub_vec)

            colnames(all_preds) = c(ids, "true_subject")
            rm(row_sums)

            res = get_summarized_predictions(all_preds, rank = TRUE) %>%
              ungroup() %>%
              mutate(
                rank1pct = (rank <= dirnum * 0.01) * 1,
                rank5pct = (rank <= dirnum * 0.05) * 1
              ) %>%
              select(-rank) %>%
              summarize(across(contains("rank"), sum), n = n()) %>%
              mutate(fold = f, n_target = dirnum) %>%
              filter(n == dirnum)
            rm(all_preds)
            rm(dat_nzv_test)
            res
          }
        )
        write_rds(summary, outfile, compress = "xz")
      })

      rm(x)
    }

  }
}


## xgboost temporal
dirs = c("100")
dir = dirs[1]
for (dir in dirs) {
  dirnum = as.numeric(dir)
  outfile = here::here("data", "lily", "data","fingerprint_prediction_results",
                       paste0("prediction_res_temporal_", paste0(dir, "xgb", ".rds")))

  if(!file.exists(outfile) || force) {
    if(dirnum <= 100){
      all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal", paste0(dir, "xgb")),
                             recursive = TRUE,
                             full.names = TRUE,
                             pattern = "rds")
      x = try({
        summary = map_dfr(.x = all_preds,
                          .f = function(file){
                            x = readRDS(file)

                            fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))
                            n_target = as.numeric(sub(".*fingerprint_res_temporal\\/(.+)xgb.*", "\\1", file))

                            res = get_summarized_predictions(x, rank = TRUE) %>%
                              ungroup() %>%
                              mutate(rank1pct = (rank <= n_target * 0.01) * 1,
                                     rank5pct = (rank <= n_target * 0.05) * 1) %>%
                              select(-rank) %>%
                              summarize(across(contains("rank"), sum),
                                        n = n()) %>%
                              mutate(fold = fold,
                                     n_tar = n_target) %>%
                              filter(n == n_tar)
                            rm(x); rm(fold); rm(n_target)
                            res
                          })
        write_rds(summary, outfile, compress = "xz")
      })
      rm(x)
    } else {
      x = try({
        x = ceiling(nrow(filenames_temp) / dirnum)
        filenames_temp = filenames_temp %>%
          mutate(fold = rep(1:x, each = dirnum)[1:nrow(filenames_temp)])

        folds = filenames_temp %>%
          count(fold) %>%
          filter(n == dirnum) %>%
          pull(fold)
        xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells_temporal.csv.gz"))


        summary = map_dfr(
          .x = folds,
          .f = function(f) {
            ids = filenames_temp %>% filter(fold == f) %>% pull(id) %>% as.character()
            df =
              xdf %>%
              filter(id %in% ids)

            df =
              xdf %>%
              filter(id %in% ids_all)

            test =
              df %>%
              filter(data == "test") %>%
              select(-data)
            rm(df)


            files = file.path(here::here(
              "data",
              "lily",
              "data",
              "fingerprint_res",
              paste0(dir, "xgb"),
              paste(ids, ".rds", sep = "")
            ))


            true_sub_vec = test$id

            all_preds =
              map(
                .x = files,
                .f = function(x) {
                  read_rds(x) %>% as_tibble()
                }
              ) %>%
              bind_cols()
            # normalize probabilities
            row_sums = rowSums(all_preds)

            # normalize and add "true subject column"
            all_preds =
              all_preds %>%
              bind_cols(sum = row_sums) %>%
              rowwise() %>%
              mutate(across(-sum, ~ .x / sum)) %>%
              dplyr::select(-sum) %>%
              ungroup() %>%
              bind_cols(true_subject = true_sub_vec)

            colnames(all_preds) = c(ids, "true_subject")
            rm(row_sums)

            res = get_summarized_predictions(all_preds, rank = TRUE) %>%
              ungroup() %>%
              mutate(
                rank1pct = (rank <= dirnum * 0.01) * 1,
                rank5pct = (rank <= dirnum * 0.05) * 1
              ) %>%
              select(-rank) %>%
              summarize(across(contains("rank"), sum), n = n()) %>%
              mutate(fold = f, n_target = dirnum) %>%
              filter(n == dirnum)
            rm(all_preds)
            rm(dat_nzv_test)
            res
          }
        )
        write_rds(summary, outfile, compress = "xz")
      })

      rm(x)
    }

  }
}

# random forest
dirs = c("100") # only one completed
dir = dirs[1]
for (dir in dirs) {
  dirnum = as.numeric(dir)
  outfile = here::here("data", "lily", "data","fingerprint_prediction_results",
                       paste0("prediction_res_", paste0(dir, "rf", ".rds")))

  if(!file.exists(outfile) || force) {
    if(dirnum <= 100){
      all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res", paste0(dir, "rf")),
                             recursive = TRUE,
                             full.names = TRUE,
                             pattern = "rds")
      x = try({
        summary = map_dfr(.x = all_preds,
                          .f = function(file){
                            x = readRDS(file)

                            fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))
                            n_target = as.numeric(sub(".*fingerprint_res\\/(.+)rf.*", "\\1", file))

                            res = get_summarized_predictions(x, rank = TRUE) %>%
                              ungroup() %>%
                              mutate(rank1pct = (rank <= n_target * 0.01) * 1,
                                     rank5pct = (rank <= n_target * 0.05) * 1) %>%
                              select(-rank) %>%
                              summarize(across(contains("rank"), sum),
                                        n = n()) %>%
                              mutate(fold = fold,
                                     n_tar = n_target) %>%
                              filter(n == n_tar)
                            rm(x); rm(fold); rm(n_target)
                            res
                          })
        write_rds(summary, outfile, compress = "xz")
      })
      rm(x)
    } else {
      x = try({
        x = ceiling(nrow(filenames) / dirnum)
        filenames = filenames %>%
          mutate(fold = rep(1:x, each = dirnum)[1:nrow(filenames)])

        folds = filenames %>%
          count(fold) %>%
          filter(n == dirnum) %>%
          pull(fold)
        xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells.csv.gz"))


        summary = map_dfr(
          .x = folds,
          .f = function(f) {
            ids = filenames %>% filter(fold == f) %>% pull(id) %>% as.character()
            df =
              xdf %>%
              filter(id %in% ids)

            set.seed(123)
            is = initial_split(df, prop = 3/4, strata = id)
            test = testing(is)
            rm(df); rm(is)
            files = file.path(here::here(
              "data",
              "lily",
              "data",
              "fingerprint_res",
              paste0(dir, "rf"),
              paste(ids, ".rds", sep = "")
            ))


            true_sub_vec = test$id

            all_preds =
              map(
                .x = files,
                .f = function(x) {
                  read_rds(x) %>% as_tibble()
                }
              ) %>%
              bind_cols()
            # normalize probabilities
            row_sums = rowSums(all_preds)

            # normalize and add "true subject column"
            all_preds =
              all_preds %>%
              bind_cols(sum = row_sums) %>%
              rowwise() %>%
              mutate(across(-sum, ~ .x / sum)) %>%
              dplyr::select(-sum) %>%
              ungroup() %>%
              bind_cols(true_subject = true_sub_vec)

            colnames(all_preds) = c(ids, "true_subject")
            rm(row_sums)

            res = get_summarized_predictions(all_preds, rank = TRUE) %>%
              ungroup() %>%
              mutate(
                rank1pct = (rank <= dirnum * 0.01) * 1,
                rank5pct = (rank <= dirnum * 0.05) * 1
              ) %>%
              select(-rank) %>%
              summarize(across(contains("rank"), sum), n = n()) %>%
              mutate(fold = f, n_target = dirnum) %>%
              filter(n == dirnum)
            rm(all_preds)
            rm(dat_nzv_test)
            res
          }
        )
        write_rds(summary, outfile, compress = "xz")
      })

      rm(x)
    }

  }
}


## temporal
dirs = c("100")
dir = dirs[1]
for (dir in dirs) {
  dirnum = as.numeric(dir)
  outfile = here::here("data", "lily", "data","fingerprint_prediction_results",
                       paste0("prediction_res_temporal_", paste0(dir, "rf", ".rds")))

  if(!file.exists(outfile) || force) {
    if(dirnum <= 100){
      all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal", paste0(dir, "rf")),
                             recursive = TRUE,
                             full.names = TRUE,
                             pattern = "rds")
      x = try({
        summary = map_dfr(.x = all_preds,
                          .f = function(file){
                            x = readRDS(file)

                            fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))
                            n_target = as.numeric(sub(".*fingerprint_res_temporal\\/(.+)rf.*", "\\1", file))

                            res = get_summarized_predictions(x, rank = TRUE) %>%
                              ungroup() %>%
                              mutate(rank1pct = (rank <= n_target * 0.01) * 1,
                                     rank5pct = (rank <= n_target * 0.05) * 1) %>%
                              select(-rank) %>%
                              summarize(across(contains("rank"), sum),
                                        n = n()) %>%
                              mutate(fold = fold,
                                     n_tar = n_target) %>%
                              filter(n == n_tar)
                            rm(x); rm(fold); rm(n_target)
                            res
                          })
        write_rds(summary, outfile, compress = "xz")
      })
      rm(x)
    } else {
      x = try({
        x = ceiling(nrow(filenames_temp) / dirnum)
        filenames_temp = filenames_temp %>%
          mutate(fold = rep(1:x, each = dirnum)[1:nrow(filenames_temp)])

        folds = filenames_temp %>%
          count(fold) %>%
          filter(n == dirnum) %>%
          pull(fold)
        xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells_temporal.csv.gz"))


        summary = map_dfr(
          .x = folds,
          .f = function(f) {
            ids = filenames_temp %>% filter(fold == f) %>% pull(id) %>% as.character()
            df =
              xdf %>%
              filter(id %in% ids)

            df =
              xdf %>%
              filter(id %in% ids_all)

            test =
              df %>%
              filter(data == "test") %>%
              select(-data)
            rm(df)


            files = file.path(here::here(
              "data",
              "lily",
              "data",
              "fingerprint_res",
              paste0(dir, "rf"),
              paste(ids, ".rds", sep = "")
            ))


            true_sub_vec = test$id

            all_preds =
              map(
                .x = files,
                .f = function(x) {
                  read_rds(x) %>% as_tibble()
                }
              ) %>%
              bind_cols()
            # normalize probabilities
            row_sums = rowSums(all_preds)

            # normalize and add "true subject column"
            all_preds =
              all_preds %>%
              bind_cols(sum = row_sums) %>%
              rowwise() %>%
              mutate(across(-sum, ~ .x / sum)) %>%
              dplyr::select(-sum) %>%
              ungroup() %>%
              bind_cols(true_subject = true_sub_vec)

            colnames(all_preds) = c(ids, "true_subject")
            rm(row_sums)

            res = get_summarized_predictions(all_preds, rank = TRUE) %>%
              ungroup() %>%
              mutate(
                rank1pct = (rank <= dirnum * 0.01) * 1,
                rank5pct = (rank <= dirnum * 0.05) * 1
              ) %>%
              select(-rank) %>%
              summarize(across(contains("rank"), sum), n = n()) %>%
              mutate(fold = f, n_target = dirnum) %>%
              filter(n == dirnum)
            rm(all_preds)
            rm(dat_nzv_test)
            res
          }
        )
        write_rds(summary, outfile, compress = "xz")
      })

      rm(x)
    }

  }
}


# functional, regular
dirs = c("100", "500", "1000")
for (dir in dirs) {
  dirnum = as.numeric(dir)
  outfile = here::here("data", "lily", "data","fingerprint_prediction_results",
                       paste0("prediction_res_", paste0(dir, "fnl", ".rds")))

  if(!file.exists(outfile) || force) {
    if(dirnum <= 500){
      all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res", paste0(dir, "fnl")),
                             recursive = TRUE,
                             full.names = TRUE,
                             pattern = "rds")
      x = try({
        summary = map_dfr(.x = all_preds,
                                 .f = function(file){
                                   x = readRDS(file)

                                   fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))
                                   n_target = as.numeric(sub(".*fingerprint_res\\/(.+)fnl.*", "\\1", file))

                                   res = get_summarized_predictions(x, rank = TRUE, exp = TRUE) %>%
                                     ungroup() %>%
                                     mutate(rank1pct = (rank <= n_target * 0.01) * 1,
                                            rank5pct = (rank <= n_target * 0.05) * 1) %>%
                                     select(-rank) %>%
                                     summarize(across(contains("rank"), sum),
                                               n = n()) %>%
                                     mutate(fold = fold,
                                            n_tar = n_target) %>%
                                     filter(n == n_tar)
                                   rm(x); rm(fold); rm(n_target)
                                   res
                                 })
        write_rds(summary, outfile, compress = "xz")
      })
      rm(x)
      outfile = here::here("data", "lily", "data","fingerprint_prediction_results",
                           paste0("prediction_res_", paste0(dir, "nlfnl", ".rds")))
      all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res", paste0(dir, "nlfnl")),
                             recursive = TRUE,
                             full.names = TRUE,
                             pattern = "rds")
      x = try({
        summary = map_dfr(.x = all_preds,
                          .f = function(file){
                            x = readRDS(file)

                            fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))
                            n_target = as.numeric(sub(".*fingerprint_res\\/(.+)nlfnl.*", "\\1", file))

                            res = get_summarized_predictions(x, rank = TRUE, exp = TRUE) %>%
                              ungroup() %>%
                              mutate(rank1pct = (rank <= n_target * 0.01) * 1,
                                     rank5pct = (rank <= n_target * 0.05) * 1) %>%
                              select(-rank) %>%
                              summarize(across(contains("rank"), sum),
                                        n = n()) %>%
                              mutate(fold = fold,
                                     n_tar = n_target) %>%
                              filter(n == n_tar)
                            rm(x); rm(fold); rm(n_target)
                            res
                          })
        write_rds(summary, outfile, compress = "xz")
      })
      rm(x)
    } else {
      x = try({
        x = ceiling(nrow(filenames) / dirnum)
        filenames = filenames %>%
          mutate(fold = rep(1:x, each = dirnum)[1:nrow(filenames)])

        folds = filenames %>%
          count(fold) %>%
          filter(n == dirnum) %>%
          pull(fold)
        xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells.csv.gz"))


        summary = map_dfr(
          .x = folds,
          .f = function(f) {
            ids = filenames %>% filter(fold == f) %>% pull(id) %>% as.character()
            df =
              xdf %>%
              filter(id %in% ids)

            set.seed(123)
            is = initial_split(df, prop = 3/4, strata = id)
            test = testing(is)
            rm(df); rm(is)
            files = file.path(here::here(
              "data",
              "lily",
              "data",
              "fingerprint_res",
              paste0(dir, "xgb"),
              paste(ids, ".rds", sep = "")
            ))


            true_sub_vec = test$id

            all_preds =
              map(
                .x = files,
                .f = function(x) {
                  read_rds(x) %>% as_tibble()
                }
              ) %>%
              bind_cols()
            # normalize probabilities
            row_sums = rowSums(all_preds)

            # normalize and add "true subject column"
            all_preds =
              all_preds %>%
              bind_cols(sum = row_sums) %>%
              rowwise() %>%
              mutate(across(-sum, ~ .x / sum)) %>%
              dplyr::select(-sum) %>%
              ungroup() %>%
              bind_cols(true_subject = true_sub_vec)

            colnames(all_preds) = c(ids, "true_subject")
            rm(row_sums)

            res = get_summarized_predictions(all_preds, rank = TRUE, exp = TRUE) %>%
              ungroup() %>%
              mutate(
                rank1pct = (rank <= dirnum * 0.01) * 1,
                rank5pct = (rank <= dirnum * 0.05) * 1
              ) %>%
              select(-rank) %>%
              summarize(across(contains("rank"), sum), n = n()) %>%
              mutate(fold = f, n_target = dirnum) %>%
              filter(n == dirnum)
            rm(all_preds)
            rm(dat_nzv_test)
            res
          }
        )
        write_rds(summary, outfile, compress = "xz")
      })

      rm(x)
    }

  }
}

# functional, temporal
dirs = c("100", "500")
dir = dirs[1]
for (dir in dirs) {
  dirnum = as.numeric(dir)
  outfile = here::here("data", "lily", "data","fingerprint_prediction_results",
                       paste0("prediction_res_temporal_", paste0(dir, "fnl", ".rds")))

  if(!file.exists(outfile) || force) {
    if(dirnum <= 500){
      all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal", paste0(dir, "fnl")),
                             recursive = TRUE,
                             full.names = TRUE,
                             pattern = "rds")
      x = try({
        summary = map_dfr(.x = all_preds,
                          .f = function(file){
                            x = readRDS(file)

                            fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))
                            n_target = as.numeric(sub(".*fingerprint_res_temporal\\/(.+)fnl.*", "\\1", file))

                            res = get_summarized_predictions(x, rank = TRUE, exp = TRUE) %>%
                              ungroup() %>%
                              mutate(rank1pct = (rank <= n_target * 0.01) * 1,
                                     rank5pct = (rank <= n_target * 0.05) * 1) %>%
                              select(-rank) %>%
                              summarize(across(contains("rank"), sum),
                                        n = n()) %>%
                              mutate(fold = fold,
                                     n_tar = n_target) %>%
                              filter(n == n_tar)
                            rm(x); rm(fold); rm(n_target)
                            res
                          })
        write_rds(summary, outfile, compress = "xz")
      })
      rm(x)
      outfile = here::here("data", "lily", "data","fingerprint_prediction_results",
                           paste0("prediction_res_", paste0(dir, "nlfnl", ".rds")))
      all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res", paste0(dir, "nlfnl")),
                             recursive = TRUE,
                             full.names = TRUE,
                             pattern = "rds")
      x = try({
        summary = map_dfr(.x = all_preds,
                          .f = function(file){
                            x = readRDS(file)

                            fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))
                            n_target = as.numeric(sub(".*fingerprint_res\\/(.+)nlfnl.*", "\\1", file))

                            res = get_summarized_predictions(x, rank = TRUE, exp = TRUE) %>%
                              ungroup() %>%
                              mutate(rank1pct = (rank <= n_target * 0.01) * 1,
                                     rank5pct = (rank <= n_target * 0.05) * 1) %>%
                              select(-rank) %>%
                              summarize(across(contains("rank"), sum),
                                        n = n()) %>%
                              mutate(fold = fold,
                                     n_tar = n_target) %>%
                              filter(n == n_tar)
                            rm(x); rm(fold); rm(n_target)
                            res
                          })
        write_rds(summary, outfile, compress = "xz")
      })
      rm(x)
    } else {
      x = try({
        x = ceiling(nrow(filenames) / dirnum)
        filenames = filenames %>%
          mutate(fold = rep(1:x, each = dirnum)[1:nrow(filenames)])

        folds = filenames %>%
          count(fold) %>%
          filter(n == dirnum) %>%
          pull(fold)
        xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells.csv.gz"))


        summary = map_dfr(
          .x = folds,
          .f = function(f) {
            ids = filenames %>% filter(fold == f) %>% pull(id) %>% as.character()
            df =
              xdf %>%
              filter(id %in% ids)

            set.seed(123)
            is = initial_split(df, prop = 3/4, strata = id)
            test = testing(is)
            rm(df); rm(is)
            files = file.path(here::here(
              "data",
              "lily",
              "data",
              "fingerprint_res",
              paste0(dir, "xgb"),
              paste(ids, ".rds", sep = "")
            ))


            true_sub_vec = test$id

            all_preds =
              map(
                .x = files,
                .f = function(x) {
                  read_rds(x) %>% as_tibble()
                }
              ) %>%
              bind_cols()
            # normalize probabilities
            row_sums = rowSums(all_preds)

            # normalize and add "true subject column"
            all_preds =
              all_preds %>%
              bind_cols(sum = row_sums) %>%
              rowwise() %>%
              mutate(across(-sum, ~ .x / sum)) %>%
              dplyr::select(-sum) %>%
              ungroup() %>%
              bind_cols(true_subject = true_sub_vec)

            colnames(all_preds) = c(ids, "true_subject")
            rm(row_sums)
            res = get_summarized_predictions(all_preds, rank = TRUE, exp = TRUE) %>%
              ungroup() %>%
              mutate(
                rank1pct = (rank <= dirnum * 0.01) * 1,
                rank5pct = (rank <= dirnum * 0.05) * 1
              ) %>%
              select(-rank) %>%
              summarize(across(contains("rank"), sum), n = n()) %>%
              mutate(fold = f, n_target = dirnum) %>%
              filter(n == dirnum)
            rm(all_preds)
            rm(dat_nzv_test)
            res
          }
        )
        write_rds(summary, outfile, compress = "xz")
      })

      rm(x)
    }

  }
}

