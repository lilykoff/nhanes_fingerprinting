library(tidyverse)


files = list.files(here::here("data", "lily", "data", "10770_boosted"),
                   recursive = TRUE,
                   full.names = TRUE)
filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds_temporal2.rds"))

folds = sub(".*fold\\_(.+).rds.*", "\\1", files) %>% as.numeric()

x = read_rds(files[1])

res_df =
  map_dfr(files,
          .f = function(x){
            read_rds(x) %>%
              mutate(fold = sub(".*fold\\_(.+).rds.*", "\\1", x))}) %>%
  mutate(model = as.character(model))

sum(res_df$rank1) / 10770
sum(res_df$rank5) / 10770


missing_ids =
  filenames %>%
  mutate(id = as.character(id)) %>%
  anti_join(res_df,
            by = c("id" = "true_subject")) %>%
  mutate(model = id,
         mean_pred = NA_real_,
         rank = 10770,
         rank1 = 0,
         rank5 = 0)

result =
  res_df %>%
  bind_rows(missing_ids %>% select(-id))

result_summ =
  result %>%
  mutate(rank1pct = if_else(rank <= (10770 * .01), 1, 0),
         rank5pct = if_else(rank <= (10770 * .05), 1, 0)) %>%
  summarize(n = n(),
            across(starts_with("rank"), ~sum(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(fold = 1,
         n_target = n)




write_rds(result_summ, here::here("data", "lily", "data", "fingerprint_prediction_results", "prediction_res_temporal2_10770boost.rds"))


