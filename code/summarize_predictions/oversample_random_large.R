library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
source(here::here("data", "lily", "data", "summary_fns.R"))
fold = NULL
rm(list = c("fold"))
force = TRUE
filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds.rds"))
if (!dir.exists(here::here("data", "lily", "data", "fingerprint_prediction_results"))) {
  dir.create(here::here(
    "data",
    "lily",
    "data",
    "fingerprint_prediction_results"
  ))
}


dir = 10000
dirnum = as.numeric(dir)
outfile = here::here(
  "data",
  "lily",
  "data",
  "fingerprint_prediction_results",
  paste0("prediction_res_", paste0(dir, "over", ".rds"))
)

if(!file.exists(outfile) ||
   force) {
  x = try({
    x = ceiling(nrow(filenames) / dirnum)
    filenames = filenames %>%
      mutate(fold = rep(1:x, each = dirnum)[1:nrow(filenames)])

    folds = filenames %>%
      count(fold) %>%
      filter(n == dirnum) %>%
      pull(fold)

    f = folds[1]

    ids = filenames %>% filter(fold == f) %>% pull(id) %>% as.character()
    files = file.path(here::here(
      "data",
      "lily",
      "data",
      "fingerprint_res",
      paste0(dir, "over"),
      paste(ids, ".rds", sep = "")
    ))
    # figure out which test data to use
    if(dirnum < 13367) {
      dat_nzv_test = read_rds(here::here(
        "data",
        "lily",
        "data",
        paste0("dat_nzv_test_", dir, "_", f, ".rds")
      )) %>%
        mutate(id = as.character(id)) %>%
        filter(id %in% ids)
    } else {
      dat_nzv_test = read_rds(here::here("data", "lily", "data", "dat_nzv_test.rds")) %>%
        mutate(id = as.character(id)) %>%
        filter(id %in% ids)
    }

    true_sub_vec = dat_nzv_test$id
    rm(dat_nzv_test) # save memory

    all_preds =
      map_dfr(
        .x = files,
        .f = function(x, exp = FALSE) {
          id_tmp = sub(".*\\/(.+).rds.*", "\\1", x)
          tmp = read_rds(x)
          tmp = tmp %>%
            map(
              \(x) as_tibble(x) %>%
                mutate(true_subject = true_sub_vec) %>%
                magrittr::set_colnames(c(id_tmp, "true_subject")) %>%
                group_by(true_subject) %>%
                mutate(sec = row_number()) %>%
                pivot_longer(
                  cols = -c("true_subject", "sec"),
                  names_to = "name",
                  values_to = "pred"
                ) %>%
                mutate(model = as.character(sub(
                  ".*x", "", name
                ))) %>%
                select(-name) %>%
                # now we have the prediction for each second for each model / true subject combo
                mutate(pred = case_when(exp ~ exp(pred), .default = pred)) %>% # exponentiate based on exp argument
                ungroup() %>%
                group_by(true_subject, model) %>%
                # get mean probability across seconds for each true subject / model combo
                summarize(
                  mean_pred = mean(pred, na.rm = TRUE),
                  .groups = "drop"
                )
            )
          rm(id_tmp)
          tmp =
            tmp %>%
            bind_rows(.id = "factor") %>%
            mutate(
              factor2 = case_when(
                factor == "1" ~ 0.1,
                factor == "2" ~ 0.25,
                factor == "3" ~ 0.5,
                factor == "4" ~ 0.75,
                factor == "5" ~ 0.9,
                .default = NA_real_
              )
            )


          #
          # # fac = c(0.1, 0.25, 0.5, 0.75, 0.9)
          # fac = c(0.1, 0.25)
        }
      )
    all_preds = all_preds %>%
      filter(factor2 %in% c(0.1, 0.25))
    res_list = split(all_preds, all_preds$factor2)
    rm(all_preds)
    res =
      map(
        .x = res_list,
        .f = function(x) {
          x %>%
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
        }
      )

    rm(res_list)
    res
    write_rds(res, outfile, compress = "xz")
  })
  rm(x)
}






