library(tidyverse)


files = list.files(here::here("data", "lily", "data", "13367_boosted"),
                   recursive = TRUE,
                   full.names = TRUE)
filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds.rds"))

folds = sub(".*fold\\_(.+).rds.*", "\\1", files) %>% as.numeric()

x = read_rds(files[1])

res_df =
  map_dfr(files,
          .f = function(x){
            read_rds(x) %>%
            mutate(fold = sub(".*fold\\_(.+).rds.*", "\\1", x))}) %>%
  mutate(model = as.character(model))

sum(res_df$rank1) / 13367
sum(res_df$rank5) / 13367


missing_ids =
  filenames %>%
  mutate(id = as.character(id)) %>%
  anti_join(res_df,
            by = c("id" = "true_subject")) %>%
  mutate(model = id,
         mean_pred = NA_real_,
         rank = NA_real_,
         rank1 = 0,
         rank5 = 0,
         fold = as.character(fold))

result =
  res_df %>%
  bind_rows(missing_ids %>% select(-id))

result_summ =
  result %>%
  summarize(n = n(),
            across(c(rank1, rank5), ~sum(.x) / nrow(result))) %>%
  mutate(fold = 1,
         n_target = n)

write_rds(result_summ, here::here("data", "lily", "data", "fingerprint_prediction_results", "prediction_res_13367boost.rds"))


