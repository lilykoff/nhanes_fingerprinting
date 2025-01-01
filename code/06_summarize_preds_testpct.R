library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE
filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds.rds"))

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

calc_accuracy = function(preds, pct_test = 0.5, n_target, fold){
  n_test = floor((ncol(preds)-1) * pct_test)
  test = preds[, c(1:n_test, ncol(preds))]

  get_summarized_predictions(test, rank = TRUE) %>%
    ungroup() %>%
    mutate(
      rank1pct = (rank <= n_test * 0.01) * 1,
      rank5pct = (rank <= n_test * 0.05) * 1
    ) %>%
    select(-rank) %>%
    summarize(across(contains("rank"), sum), n_test = n()) %>%
    mutate(fold = fold, n_train = n_target)
}


dirs = c("100", "250", "500" ,"1000", "2500", "5000", "10000", "13367")
dir = dirs[1]
for (dir in dirs) {
  dirnum = as.numeric(dir)
  outfile = here::here("data", "lily", "data","fingerprint_prediction_results",
                       paste0("prediction_res_", dir, "testpct.rds"))

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

                           res = map_dfr(.x = seq(0.1, 0.9, by = 0.1),
                                         .f = calc_accuracy,
                                         preds = x,
                                         n_target = n_target,
                                         fold = fold) %>%
                             filter(n_train == n_target)
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

            res = map_dfr(.x = seq(0.1, 0.9, by = 0.1),
                          .f = calc_accuracy,
                          preds = x,
                          n_target = n_target,
                          fold = fold) %>%
              filter(n_train == dirnum)

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
