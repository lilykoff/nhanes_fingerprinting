# 75012

library(tidyverse)

preds = read_rds(here::here("data", "lily", "data", "subj_level_preds.rds"))
dfs = list.files(here::here("data/lily/data/fprint_df_ts"), full.names = TRUE)
dfs
df_list = purrr::map_dfr(.x = dfs,
                     .f = read_rds)

summarized =
  df_list %>%
  group_by(SEQN, data) %>%
  summarize(flagged = sum(flag_ind),
            wear = sum(wear_pred == 1),
            sleep = sum(wear_pred == 2),
            nonwear = sum(wear_pred == 3),
            unknown = sum(wear_pred == 4))

summ_random =
  summarized %>%
  filter(data == "random") %>%
  left_join(preds %>%
              select(true_subject, mean_pred, rank1, rank),
            by = c("SEQN" = "true_subject"))

summ_random
summary(summ_random$nonwear)
summary(summ_random$flagged)
summary(summ_random$sleep)

summ_random %>%
  arrange(desc(sleep))
summ_random %>%
  mutate(flag_bin = flagged > 0) %>%
  group_by(flag_bin) %>%
  summarize(acc = mean(rank1, na.rm = TRUE))

summ_random %>%
  mutate(flag_bin = nonwear > 0) %>%
  group_by(flag_bin) %>%
  summarize(acc = mean(rank1, na.rm = TRUE))
