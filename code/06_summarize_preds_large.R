library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE
filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds.rds"))

# summarize preds large !
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

sizes = c("2000", "4000", "10000", "13367")
# s = sizes[4]
for (s in sizes) {
  x = try({
    s_num = as.numeric(s)
    x = ceiling(nrow(filenames) / s_num)
    filenames = filenames %>%
      mutate(fold = rep(1:x, each = s_num)[1:nrow(filenames)])

    folds = filenames %>%
      count(fold) %>%
      filter(n == s_num) %>%
      pull(fold)


    preds = map_dfr(
      .x = folds,
      .f = function(f) {
        ids = filenames %>% filter(fold == f) %>% pull(id) %>% as.character()
        files = file.path(here::here(
          "data",
          "lily",
          "data",
          "fingerprint_res",
          s,
          paste(ids, ".rds", sep = "")
        ))

        dat_nzv_test = read_rds(here::here("data", "lily", "data", "dat_nzv_test.rds")) %>%
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
        n_tar = as.numeric(s)

        get_summarized_predictions(all_preds, rank = TRUE) %>%
          ungroup() %>%
          mutate(
            rank1pct = (rank <= n_tar * 0.01) * 1,
            rank5pct = (rank <= n_tar * 0.05) * 1
          ) %>%
          select(-rank) %>%
          summarize(across(contains("rank"), sum), n = n()) %>%
          mutate(fold = f, n_target = n_tar) %>%
          filter(n == n_tar)
      }
    )

    if (dir.exists(here::here("data", "lily", "data", "fingerprint_prediction_results"))) {
      dir.create(here::here(
        "data",
        "lily",
        "data",
        "fingerprint_prediction_results"
      ))
    }
    fname = paste0("prediction_res_", s)
    saveRDS(
      preds,
      here::here(
        "data",
        "lily",
        "data",
        "fingerprint_prediction_results",
        fname
      )
    )

  })
  rm(x)
}


  all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res", s),
                         recursive = TRUE,
                         full.names = TRUE,
                         pattern = "rds")

  x = try({
    res = map_dfr(.x = all_preds,
                  .f = function(file){
                    x = readRDS(file)
                    n_tar = as.numeric(s)
                    fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))

                    get_summarized_predictions(x, rank = TRUE) %>%
                      ungroup() %>%
                      mutate(rank1pct = (rank <= n_tar * 0.01) * 1,
                             rank5pct = (rank <= n_tar * 0.05) * 1) %>%
                      select(-rank) %>%
                      summarize(across(contains("rank"), sum),
                                n = n()) %>%
                      mutate(fold = fold,
                             n_target = n_tar) %>%
                      filter(n == n_tar)
                  })
    if(dir.exists(here::here("data", "lily", "data", "fingerprint_prediction_results"))){
      dir.create(here::here("data", "lily", "data", "fingerprint_prediction_results"))
    }
    fname = paste0("prediction_res_", s)
    saveRDS(res, here::here("data", "lily", "data", "fingerprint_prediction_results", fname))
  })
  rm(x)

}

