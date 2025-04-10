library(tidyverse)
library(tidymodels)
library(future)
library(furrr)
# plan(multicore, workers = 8)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE
filenames_temp = readRDS(here::here("data", "lily", "data", "fingerprint_folds_temporal2.rds"))



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


############## summarize predictions for temporal models
dirs = c("100", "500" ,"1000", "2500", "5000", "10770")
dir = dirs[1]
for (dir in dirs) {
  dirnum = as.numeric(dir)
  outfile = here::here("data", "lily", "data","fingerprint_prediction_results",
                       paste0("prediction_res_temporal2_", paste0(dir, ".rds")))

  if(!file.exists(outfile) || force) {
    if(dirnum <= 1000){
      all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal2", dir),
                             recursive = TRUE,
                             full.names = TRUE,
                             pattern = "rds")
      x = try({
        summary = map_dfr(.x = all_preds,
                          .f = function(file){
                            x = readRDS(file)

                            fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))
                            n_target = as.numeric(sub(".*fingerprint_res_temporal2\\/(.+)\\/fold.*", "\\1", file))

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
              "fingerprint_res_temporal2",
              dir,
              paste(ids, ".rds", sep = "")
            ))

            if(dirnum < 10770){
              dat_nzv_test = read_rds(here::here("data", "lily", "data", paste0("dat_nzv_test_temporal2_", dir, "_", f, ".rds"))) %>%
                mutate(id = as.character(id)) %>%
                filter(id %in% ids)} else {
                  dat_nzv_test = read_rds(here::here("data", "lily", "data", "dat_nzv_test_temporal2.rds")) %>%
                    mutate(id = as.character(id)) %>%
                    filter(id %in% ids)
                }

            true_sub_vec = dat_nzv_test$id
            rm(dat_nzv_test)

            all_preds =
              map_dfr(
                .x = files,
                .f = function(x, exp = FALSE) {
                  id_tmp = sub(".*\\/(.+).rds.*", "\\1", x)
                  tmp = read_rds(x) %>% as_tibble() %>%
                    mutate(true_subject = true_sub_vec) %>%
                    magrittr::set_colnames(c(id_tmp, "true_subject")) %>%
                    group_by(true_subject) %>%
                    mutate(sec = row_number()) %>%
                    pivot_longer(cols = -c("true_subject", "sec"), names_to = "name", values_to = "pred") %>%
                    mutate(model = as.character(sub(".*x", "", name))) %>%
                    select(-name) %>%
                    # now we have the prediction for each second for each model / true subject combo
                    mutate(pred = case_when(exp ~ exp(pred),
                                            .default = pred)) %>% # exponentiate based on exp argument
                    ungroup() %>%
                    group_by(true_subject, model) %>%
                    # get mean probability across seconds for each true subject / model combo
                    summarize(mean_pred = mean(pred, na.rm = TRUE), .groups = "drop")
                  rm(id_tmp)
                  tmp
                }
              )

            res =
              all_preds %>%
              group_by(true_subject) %>%
              mutate(
                rank = rank(-mean_pred)
              ) %>% # get the rank for each prediction
              ungroup() %>%
              filter(model == true_subject) %>% # only keep the correct combos and get ranks
              mutate(
                rank1 = if_else(rank == 1, 1, 0),
                rank5 = if_else(rank <= 5, 1, 0),
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




## xgboost temporal
dirs = c("100")
dir = dirs[1]
for (dir in dirs) {
  dirnum = as.numeric(dir)
  outfile = here::here("data", "lily", "data","fingerprint_prediction_results",
                       paste0("prediction_res_temporal2_", paste0(dir, "xgb", ".rds")))

  if(!file.exists(outfile) || force) {
    if(dirnum <= 100){
      all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal2", paste0(dir, "xgb")),
                             recursive = TRUE,
                             full.names = TRUE,
                             pattern = "rds")
      x = try({
        summary = map_dfr(.x = all_preds,
                          .f = function(file){
                            x = readRDS(file)

                            fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))
                            n_target = as.numeric(sub(".*fingerprint_res_temporal2\\/(.+)xgb.*", "\\1", file))

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
              "fingerprint_res_temporal",
              paste0(dir, "xgb"),
              paste(ids, ".rds", sep = "")
            ))


            true_sub_vec = test$id
            rm(test)

            all_preds =
              map_dfr(
                .x = files,
                .f = function(x, exp = FALSE) {
                  id_tmp = sub(".*\\/(.+).rds.*", "\\1", x)
                  tmp = read_rds(x) %>% as_tibble() %>%
                    mutate(true_subject = true_sub_vec) %>%
                    magrittr::set_colnames(c(id_tmp, "true_subject")) %>%
                    group_by(true_subject) %>%
                    mutate(sec = row_number()) %>%
                    pivot_longer(cols = -c("true_subject", "sec"), names_to = "name", values_to = "pred") %>%
                    mutate(model = as.character(sub(".*x", "", name))) %>%
                    select(-name) %>%
                    # now we have the prediction for each second for each model / true subject combo
                    mutate(pred = case_when(exp ~ exp(pred),
                                            .default = pred)) %>% # exponentiate based on exp argument
                    ungroup() %>%
                    group_by(true_subject, model) %>%
                    # get mean probability across seconds for each true subject / model combo
                    summarize(mean_pred = mean(pred, na.rm = TRUE), .groups = "drop")
                  rm(id_tmp)
                  tmp
                }
              )

            res =
              all_preds %>%
              group_by(true_subject) %>%
              mutate(
                rank = rank(-mean_pred)
              ) %>% # get the rank for each prediction
              ungroup() %>%
              filter(model == true_subject) %>% # only keep the correct combos and get ranks
              mutate(
                rank1 = if_else(rank == 1, 1, 0),
                rank5 = if_else(rank <= 5, 1, 0)
              ) %>%
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


## temporal
dirs = c("100")
dir = dirs[1]
for (dir in dirs) {
  dirnum = as.numeric(dir)
  outfile = here::here("data", "lily", "data","fingerprint_prediction_results",
                       paste0("prediction_res_temporal2_", paste0(dir, "rf", ".rds")))

  if(!file.exists(outfile) || force) {
    if(dirnum <= 100){
      all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal2", paste0(dir, "rf")),
                             recursive = TRUE,
                             full.names = TRUE,
                             pattern = "rds")
      x = try({
        summary = map_dfr(.x = all_preds,
                          .f = function(file){
                            x = readRDS(file)

                            fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))
                            n_target = as.numeric(sub(".*fingerprint_res_temporal2\\/(.+)rf.*", "\\1", file))

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
              "fingerprint_res_temporal",
              paste0(dir, "rf"),
              paste(ids, ".rds", sep = "")
            ))


            true_sub_vec = test$id
            rm(test)
            all_preds =
              map_dfr(
                .x = files,
                .f = function(x, exp = FALSE) {
                  id_tmp = sub(".*\\/(.+).rds.*", "\\1", x)
                  tmp = read_rds(x) %>% as_tibble() %>%
                    mutate(true_subject = true_sub_vec) %>%
                    magrittr::set_colnames(c(id_tmp, "true_subject")) %>%
                    group_by(true_subject) %>%
                    mutate(sec = row_number()) %>%
                    pivot_longer(cols = -c("true_subject", "sec"), names_to = "name", values_to = "pred") %>%
                    mutate(model = as.character(sub(".*x", "", name))) %>%
                    select(-name) %>%
                    # now we have the prediction for each second for each model / true subject combo
                    mutate(pred = case_when(exp ~ exp(pred),
                                            .default = pred)) %>% # exponentiate based on exp argument
                    ungroup() %>%
                    group_by(true_subject, model) %>%
                    # get mean probability across seconds for each true subject / model combo
                    summarize(mean_pred = mean(pred, na.rm = TRUE), .groups = "drop")
                  rm(id_tmp)
                  tmp
                }
              )

            res =
              all_preds %>%
              group_by(true_subject) %>%
              mutate(
                rank = rank(-mean_pred)
              ) %>% # get the rank for each prediction
              ungroup() %>%
              filter(model == true_subject) %>% # only keep the correct combos and get ranks
              mutate(
                rank1 = if_else(rank == 1, 1, 0),
                rank5 = if_else(rank <= 5, 1, 0)
              ) %>%
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



## lasso
############## summarize predictions for temporal models
dirs = c("100", "500")
dir = dirs[1]
for (dir in dirs) {
  dirnum = as.numeric(dir)
  outfile = here::here("data", "lily", "data","fingerprint_prediction_results",
                       paste0("prediction_res_temporal2_", paste0(dir, "lasso", ".rds")))

  if(!file.exists(outfile) || force) {
    if(dirnum <= 100){
      all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal2", paste0(dir, "lasso")),
                             recursive = TRUE,
                             full.names = TRUE,
                             pattern = "rds")
      x = try({
        summary = map_dfr(.x = all_preds,
                          .f = function(file){
                            x = readRDS(file)

                            fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))
                            n_target = as.numeric(sub(".*fingerprint_res_temporal2\\/(.+)lasso.*", "\\1", file))

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

            test =
              df %>%
              filter(data == "test") %>%
              select(-data)
            rm(df)


            files = file.path(here::here(
              "data",
              "lily",
              "data",
              "fingerprint_res_temporal",
              paste0(dir, "lasso"),
              paste(ids, ".rds", sep = "")
            ))


            true_sub_vec = test$id
            rm(test)

            all_preds =
              map_dfr(
                .x = files,
                .f = function(x, exp = FALSE) {
                  id_tmp = sub(".*\\/(.+).rds.*", "\\1", x)
                  tmp = read_rds(x) %>% as_tibble() %>%
                    mutate(true_subject = true_sub_vec) %>%
                    magrittr::set_colnames(c(id_tmp, "true_subject")) %>%
                    group_by(true_subject) %>%
                    mutate(sec = row_number()) %>%
                    pivot_longer(cols = -c("true_subject", "sec"), names_to = "name", values_to = "pred") %>%
                    mutate(model = as.character(sub(".*x", "", name))) %>%
                    select(-name) %>%
                    # now we have the prediction for each second for each model / true subject combo
                    mutate(pred = case_when(exp ~ exp(pred),
                                            .default = pred)) %>% # exponentiate based on exp argument
                    ungroup() %>%
                    group_by(true_subject, model) %>%
                    # get mean probability across seconds for each true subject / model combo
                    summarize(mean_pred = mean(pred, na.rm = TRUE), .groups = "drop")
                  rm(id_tmp)
                  tmp
                }
              )

            res =
              all_preds %>%
              group_by(true_subject) %>%
              mutate(
                rank = rank(-mean_pred)
              ) %>% # get the rank for each prediction
              ungroup() %>%
              filter(model == true_subject) %>% # only keep the correct combos and get ranks
              mutate(
                rank1 = if_else(rank == 1, 1, 0),
                rank5 = if_else(rank <= 5, 1, 0)
              ) %>%
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

