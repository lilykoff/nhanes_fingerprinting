library(tidyverse)
library(tidymodels)
library(future)
library(furrr)
# plan(multicore, workers = 8)
source(here::here("code", "R", "utils.R"))
source(here::here("data", "lily", "code", "summary_fns.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE






  exp = FALSE
  individual = TRUE
  outfile = "prediction_res_1000rf.rds"
  dirnum = 1000
  filenames_file = "fingerprint_folds.rds"
  pred_dir = here::here("data", "lily", "data", "fingerprint_res", "1000rf")
  force = FALSE
  n_max = 13367
  no_nzv_dat = FALSE
  testdata_name = "dat_nzv_test_1000"
  out_dir = here::here("data", "lily", "data", "fingerprint_prediction_results")

    out = here::here(out_dir, outfile)
   files = list.files(here::here(pred_dir), pattern = "*.rds", recursive = TRUE)
   file_ids = sub(".rds.*", "", files) %>% as.numeric()
    filenames = read_rds(here::here("data", "lily", "data", filenames_file))
        x = ceiling(nrow(filenames) / dirnum)
        filenames = filenames %>%
          mutate(fold = rep(1:x, each = dirnum)[1:nrow(filenames)])

        folds = filenames %>%
          count(fold) %>%
          filter(n == dirnum) %>%
          pull(fold)

        folds = filenames %>%
          mutate(complete = id %in% file_ids) %>%
          group_by(fold) %>%
          summarize(n = sum(complete), .groups = "drop") %>%
          filter(n == 1000) %>%
          pull(fold)
#
#         filenames %>%
#           mutate(complete = id %in% file_ids) %>%
#           filter(fold == 11 & !complete)

        summary = map_dfr(
          .x = folds,
          .f = function(f) {
            ids = filenames %>% filter(fold == f) %>% pull(id) %>% as.character()
            files = file.path(here::here(
              pred_dir,
              paste(ids, ".rds", sep = "")
            ))
            if (dirnum < n_max & !no_nzv_dat) {
              dat_nzv_test = read_rds(here::here("data", "lily", "data", paste0(testdata_name, "_", f, ".rds"))) %>%
                mutate(id = as.character(id)) %>%
                filter(id %in% ids)
            } else if (no_nzv_dat) {
              xdf = read_csv(here::here("data", "lily", "data", paste0(testdata_name)))
              df =
                xdf %>%
                mutate(id = as.character(id)) %>%
                filter(id %in% ids)
              rm(xdf)
              set.seed(123)
              is = initial_split(df, prop = 3 / 4, strata = id)
              dat_nzv_test = testing(is)
              rm(df)
              rm(is)
            } else {
              dat_nzv_test = read_rds(here::here("data", "lily", "data", paste0(testdata_name, ".rds"))) %>%
                mutate(id = as.character(id)) %>%
                filter(id %in% ids)
            }

            true_sub_vec = dat_nzv_test$id
            rm(dat_nzv_test)

            all_preds =
              map_dfr(
                .x = files,
                .f = function(x) {
                  id_tmp = sub(".*\\/(.+).rds.*", "\\1", x)
                  tmp = read_rds(x) %>% as_tibble() %>%
                    mutate(true_subject = true_sub_vec) %>%
                    magrittr::set_colnames(c(id_tmp, "true_subject")) %>%
                    group_by(true_subject) %>%
                    mutate(sec = row_number()) %>%
                    pivot_longer(
                      cols = -c("true_subject", "sec"),
                      names_to = "name",
                      values_to = "pred"
                    ) %>%
                    mutate(model = as.character(sub(".*x", "", name))) %>%
                    select(-name) %>%
                    # now we have the prediction for each second for each model / true subject combo
                    mutate(pred = case_when(exp ~ exp(pred), .default = pred)) %>% # exponentiate based on exp argument
                    ungroup() %>%
                    group_by(true_subject, model) %>%
                    # get mean probability across seconds for each true subject / model combo
                    summarize(mean_pred = mean(pred, na.rm = TRUE),
                              .groups = "drop")
                  rm(id_tmp)
                  tmp
                }
              )

            res =
              all_preds %>%
              group_by(true_subject) %>%
              mutate(rank = rank(-mean_pred)) %>% # get the rank for each prediction
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

        write_rds(summary, out, compress = "xz")




