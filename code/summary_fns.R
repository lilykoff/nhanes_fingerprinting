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

get_summarized_predictions_full = function(exp = FALSE,
                                           outfile,
                                           dirnum,
                                           pred_dir,
                                           filenames_file,
                                           individual = FALSE,
                                           testdata_name = NULL,
                                           out_dir = here::here("data", "lily", "data", "fingerprint_prediction_results"),
                                           n_max = NULL,
                                           force = FALSE,
                                           no_nzv_dat = FALSE){
  x = try({
    out = here::here(out_dir, outfile)
    print(out)
    if (!file.exists(out) || force) {
      filenames = read_rds(here::here("data", "lily", "data", filenames_file))
      if (!individual) {
        all_preds = list.files(
          pred_dir,
          recursive = TRUE,
          full.names = TRUE,
          pattern = "rds"
        )
        summary = map_dfr(
          .x = all_preds,
          .f = function(file) {
            x = read_rds(file)

            fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))
            n_target = dirnum
            res = get_summarized_predictions(x, rank = TRUE) %>%
              ungroup() %>%
              mutate(
                rank1pct = (rank <= n_target * 0.01) * 1,
                rank5pct = (rank <= n_target * 0.05) * 1
              ) %>%
              select(-rank) %>%
              summarize(across(contains("rank"), sum), n = n()) %>%
              mutate(fold = fold, n_tar = n_target) %>%
              filter(n == n_tar)
            rm(x)
            rm(fold)
            rm(n_target)
            res
          }
        )
        write_rds(summary, out, compress = "xz")
      } else {
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

      }
    }
  })
  rm(x)
}
