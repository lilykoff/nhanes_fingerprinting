# scalar on image regression
# outcome: sex, age, mortality
# predictor: function


library(tidyverse)
library(tidymodels)
library(refund)
library(mgcv)
library(tidyfun)

df = read_csv(here::here("data", "sample_grid_cells.csv.gz"))
# all_grid_cells_fine.csv.gz

df_summ =
  df %>%
  group_by(id) %>%
  mutate(n = n()) %>%
  group_by(id, n) %>%
  summarize(across(starts_with("x"), ~sum(.x))) %>%
  ungroup() %>%
  mutate(across(ends_with("12"), ~.x / (n * (80-12))),
         across(ends_with("24"), ~.x / (n * (80-24))),
         across(ends_with("36"), ~.x / (n * (80-36))))

demo = readRDS(here::here("data", "covariates_accel_mortality_df.rds"))

df_summ =
  df_summ %>%
  left_join(demo %>% select(id = SEQN, age = age_in_years_at_screening) %>% mutate(id = as.numeric(id)))


image_mat =
  df_summ %>% select(starts_with("x")) %>%
  as.matrix()

pfr_fit =
  pfr(
    age ~ lf(image_mat, argvals = seq(1, 432)),
    method = "REML", data = df_summ)

pfr_coef_df =
  coef(pfr_fit) %>%
  mutate(method = "refund::pfr()") %>%
  tf_nest(.id = method, .arg = image_mat.argvals) %>%
  rename(estimate = value)

pfr_coef_df %>%
  ggplot(aes(y = estimate)) +
  geom_spaghetti(alpha = 1, linewidth = 1.2) +
  labs(x = "Time of day (hours)", y = "Coefficient")
