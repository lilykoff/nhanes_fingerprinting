library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE
filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds.rds"))
filenames_temp = readRDS(here::here("data", "lily", "data", "fingerprint_folds_temporal.rds"))
if (dir.exists(here::here("data", "lily", "data", "fingerprint_prediction_results"))) {
  dir.create(here::here(
    "data",
    "lily",
    "data",
    "fingerprint_prediction_results"
  ))
}

get_summarized_predictions = function(predictions, rank = FALSE) {
  if (rank) {
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
      mutate(
        rank = rank(-mean_pred)
      ) %>%
      filter(model==true_subject) %>%
      mutate(
        rank1 = if_else(rank == 1, 1, 0),
        rank5 = if_else(rank <= 5, 1, 0)
      )
  } else {
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
      mutate(correct = if_else(as.numeric(predicted_sub) == true_subject, 1, 0)) %>%
      ungroup()
  }
}

###########
## summarize predictions for "regular" models
dirs = c("100", "250", "500" ,"1000", "2500", "5000", "10000", "13367")
dir = dirs[5]
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

            dat_nzv_test = read_rds(here::here("data", "lily", "data", paste0("dat_nzv_test_", dir, "_", f, ".rds"))) %>%
              mutate(id = as.character(id)) %>%
              filter(id %in% ids)

            true_sub_vec = dat_nzv_test$id

            all_preds =
              map(
                .x = files,
                .f = function(x) {
                  read_rds(x) %>% as_tibble()
                }
              ) %>%
              bind_cols() %>%
              mutate(true_subject = true_sub_vec)

            colnames(all_preds) = c(ids, "true_subject")

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

############## summarize predictions for temporal models
dirs = c("100", "250", "500" ,"1000", "2500", "5000", "10000", "11225")
dir = dirs[1]
for (dir in dirs) {
  dirnum = as.numeric(dir)
  outfile = here::here("data", "lily", "data","fingerprint_prediction_results",
                       paste0("prediction_res_", paste0(dir, "temporal", ".rds")))

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

            dat_nzv_test = read_rds(here::here("data", "lily", "data", paste0("dat_nzv_test_temporal", dir, "_", f, ".rds"))) %>%
              mutate(id = as.character(id)) %>%
              filter(id %in% ids)

            true_sub_vec = dat_nzv_test$id

            all_preds =
              map(
                .x = files,
                .f = function(x) {
                  read_rds(x) %>% as_tibble()
                }
              ) %>%
              bind_cols() %>%
              mutate(true_subject = true_sub_vec)

            colnames(all_preds) = c(ids, "true_subject")

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


### xgboost
dirs = c("100", "500" ,"1000", "5000")
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
              bind_cols() %>%
              mutate(true_subject = true_sub_vec)

            colnames(all_preds) = c(ids, "true_subject")

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
dirs = c("100", "500" ,"1000", "5000")
dir = dirs[1]
for (dir in dirs) {
  dirnum = as.numeric(dir)
  outfile = here::here("data", "lily", "data","fingerprint_prediction_results_temporal",
                       paste0("prediction_res_", paste0(dir, "xgb", ".rds")))

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
              bind_cols() %>%
              mutate(true_subject = true_sub_vec)

            colnames(all_preds) = c(ids, "true_subject")

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



# OLD

dir = "100xgb"
outfile = here::here("data", "lily", "data","fingerprint_prediction_results",
                     paste0("prediction_res_", dir, ".rds"))

all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res", dir),
                       recursive = TRUE,
                       full.names = TRUE,
                       pattern = "rds")
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
rs = readRDS(here::here("data/lily/data", "fingerprint_prediction_results", "prediction_res_100.rds"))
rs

all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal", "100"),
                       recursive = TRUE,
                       full.names = TRUE,
                       pattern = "rds")
summary_temp = map_dfr(.x = all_preds,
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

all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal", "100xgb"),
                       recursive = TRUE,
                       full.names = TRUE,
                       pattern = "rds")
summary_tempx = map(.x = all_preds,
                       .f = function(file){
                         try({x = readRDS(file)

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
                         res})
                       })
stx = summary_tempx %>%
  keep(~!inherits(.x, "try-error")) %>%
  bind_rows()
mean(stx$rank1); mean(stx$rank5)
