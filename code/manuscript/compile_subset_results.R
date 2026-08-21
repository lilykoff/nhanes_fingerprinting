## compile and save results to upload to github
library(tidyverse)




subsets100 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_100_v4.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 100)


subsets500 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_500_v3.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 500)
subsets1000 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_1000_v3.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 1000)


subsets2500 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_2500_v4.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 2500)


subsets5000 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_5000_v4.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 5000)

subsets7500 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_7500_v4.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 7500)

all = read_rds(here::here("results", "prediction_res_13367wtd.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 13367)

comb =
  subsets100 %>%
  bind_rows(subsets500, subsets1000, subsets2500, subsets5000, subsets7500, all) %>%
  select(-fold)


write_rds(comb, here::here("results", "subset_train_results.rds"))

