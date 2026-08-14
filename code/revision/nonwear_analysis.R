### figure out how many minutes that are classified as walking by ADEPT are also classified as nonwear

# 1 = wake wear, 2 = sleep wear, 3 = nonwear, and 4 = unknown
library(tidyverse)

adept = read_rds(here::here("..", "nhanes_steps_mortality", "data",
                            "accelerometry", "minute_level", "nhanes_1440_adeptsteps.rds"))

wear = read_rds(here::here("..", "nhanes_steps_mortality", "data",
                            "accelerometry", "minute_level", "nhanes_1440_PAXPREDM.rds"))


adept %>% head
wear %>% head

adept_walk_mat =
  adept %>%
  select(starts_with("min")) %>%
  mutate(across(.cols = everything(), ~if_else(is.na(.x) | .x < 1, FALSE, TRUE))) %>%
  unname() %>%
  as.matrix()

unk_mat =
  wear %>%
  select(starts_with("min")) %>%
  mutate(across(.cols = everything(), ~if_else(.x == 4, TRUE, FALSE))) %>%
  unname() %>%
  as.matrix()

nw_mat =
  wear %>%
  select(starts_with("min")) %>%
  mutate(across(.cols = everything(), ~if_else(.x == 3, TRUE, FALSE))) %>%
  unname() %>%
  as.matrix()


sleep_mat =
  wear %>%
  select(starts_with("min")) %>%
  mutate(across(.cols = everything(), ~if_else(.x == 2, TRUE, FALSE))) %>%
  unname() %>%
  as.matrix()

wake_mat =
  wear %>%
  select(starts_with("min")) %>%
  mutate(across(.cols = everything(), ~if_else(.x == 1, TRUE, FALSE))) %>%
  unname() %>%
  as.matrix()

# get the number of minutes that are classified as walking by ADEPT and nonwear by PAXPREDM

walk_nw = adept_walk_mat & nw_mat
sum(walk_nw, na.rm = TRUE) / nrow(wear)

walk_unk = adept_walk_mat & unk_mat
sum(walk_unk, na.rm = TRUE) / nrow(wear)

walk_sleep = adept_walk_mat & sleep_mat
sum(walk_sleep, na.rm = TRUE) / nrow(wear)

walk_wake = adept_walk_mat & wake_mat
sum(walk_wake, na.rm = TRUE) / nrow(wear)
