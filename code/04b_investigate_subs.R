# get samples
library(tidyverse)
library(tidymodels)

# if(!file.exists(here::here("data", "lily", "data", "nzv_fine.rds"))){
if(!file.exists(here::here("data", "nzv_fine.rds"))){

  df = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells_fine.csv.gz"))
  # df = readr::read_csv(here::here("data", "all_grid_cells_fine.csv.gz"))
  #
  # df = read_csv(here::here("data", "sample_grid_cells_fine.csv.gz"))
  nzv_trans =
    recipe(id ~ ., data = df) %>%
    step_nzv(all_predictors())


  nzv_estimates = prep(nzv_trans)

  nzv = colnames(juice(nzv_estimates))

  df_nzv = df %>% select(id, all_of(nzv))

  df_nzv_summ =
    df_nzv %>%
    # df %>%
    group_by(id) %>%
    mutate(n = n()) %>%
    group_by(id, n) %>%
    summarize(across(starts_with("x"), ~ sum(.x))) %>%
    ungroup() %>%
    mutate(across(ends_with("12"), ~ .x / (n * (80 - 12))),
           across(ends_with("24"), ~ .x / (n * (80 - 24))),
           across(ends_with("36"), ~ .x / (n * (80 - 36))))
  # write_rds(df_nzv_summ, here::here("data", "nzv_fine.rds"))

  write_rds(df_nzv_summ, here::here("data", "lily", "data", "nzv_fine.rds"))
  rm(df)
}

df_nzv_summ = read_rds(here::here("data", "lily", "data", "nzv_fine.rds"))

age_sex_res = readRDS(here::here("data", "lily", "data", "covar_reg_fine.rds"))

age_res = age_sex_res %>%
  filter(term == "age") %>%
  mutate(lag = sub(".*_(.*)", "\\1", var),
         var = sub("(.*)_.*", "\\1", var))

sex_res = age_sex_res %>%
  filter(term == "genderMale") %>%
  mutate(lag = sub(".*_(.*)", "\\1", var),
         var = sub("(.*)_.*", "\\1", var))

mort_res = age_sex_res %>%
  filter(term == "mortstat1") %>%
  mutate(lag = sub(".*_(.*)", "\\1", var),
         var = sub("(.*)_.*", "\\1", var))

x_vars = seq(0, 3, 0.1)

df_fine = tibble(vm = seq(0, 3, 0.05)) %>%
  mutate(lag_vm = dplyr::lag(vm, n = 1)) %>%   # for each second, calculate vm and lagged vm
  mutate(
    cut_sig = cut(
      vm,
      breaks = seq(0, 3, by = 0.1),
      include.lowest = T
    ),
    cut_lagsig = cut(
      lag_vm,
      breaks = seq(0, 3, by = 0.1),
      include.lowest = T
    )
  ) %>%
  drop_na() %>% # count # points in each "grid cell"
  count(cut_sig, cut_lagsig, .drop = FALSE) %>%
  mutate(
    cell = paste(cut_sig, cut_lagsig, sep = "_"),
    num_x  = as.numeric(cut_sig),
    num_y = as.numeric(cut_lagsig)
  )

old_names = unique(df_fine$cell)

temp =
  tibble(x = old_names,
         y = seq(1:length(old_names))) %>%
  pivot_wider(names_from = x, values_from = y)
clean_names = janitor::clean_names(temp) %>%
  colnames()

key = tibble(old_names, clean_names)

df_fine = df_fine %>%
  full_join(key, by = c("cell" = "old_names"))


age_max =
  df_fine %>%
  left_join(age_res, by = c("clean_names" = "var")) %>%
  filter(!is.na(lag) & reg_type == "age_sex_mort") %>%
  arrange(desc(estimate)) %>%
  slice(1) %>%
  pull(clean_names)


a_max =
  df_nzv_summ %>%
  select(id, var = all_of(age_max)) %>%
  summarize(min_id = sample(id[var == min(var)], 1),
            max_id = sample(id[var == max(var)], 1))

a_max

age_min = df_fine %>%
  left_join(age_res, by = c("clean_names" = "var")) %>%
  filter(!is.na(lag) & reg_type == "age_sex_mort") %>%
  arrange(estimate) %>%
  slice(1) %>%
  pull(clean_names)

a_min =
  df_nzv_summ %>%
  select(id, var = all_of(age_min)) %>%
  summarize(min_id = sample(id[var == min(var)], 1),
            max_id = sample(id[var == max(var)], 1))
a_min

sex_max =
  df_fine %>%
  left_join(sex_res, by = c("clean_names" = "var")) %>%
  filter(!is.na(lag) & reg_type == "age_sex_mort") %>%
  arrange(desc(estimate)) %>%
  slice(1) %>%
  pull(clean_names)

s_max =
  df_nzv_summ %>%
  select(id, var = all_of(sex_max)) %>%
  summarize(min_id = sample(id[var == min(var)], 1),
            max_id = sample(id[var == max(var)], 1))

s_max

sex_min =
  df_fine %>%
  left_join(sex_res, by = c("clean_names" = "var")) %>%
  filter(!is.na(lag) & reg_type == "age_sex_mort") %>%
  arrange(estimate) %>%
  slice(1) %>%
  pull(clean_names)

s_min =
  df_nzv_summ %>%
  select(id, var = all_of(sex_min)) %>%
  summarize(min_id = sample(id[var == min(var)], 1),
            max_id = sample(id[var == max(var)], 1))
s_min

mort_max =
  df_fine %>%
  left_join(mort_res, by = c("clean_names" = "var")) %>%
  filter(!is.na(lag) & reg_type == "age_sex_mort") %>%
  arrange(desc(estimate)) %>%
  slice(1) %>%
  pull(clean_names)

m_max =
  df_nzv_summ %>%
  select(id, var = all_of(mort_max)) %>%
  summarize(min_id = sample(id[var == min(var)], 1),
            max_id = sample(id[var == max(var)], 1))

m_max

mort_min = df_fine %>%
  left_join(mort_res, by = c("clean_names" = "var")) %>%
  filter(!is.na(lag) & reg_type == "age_sex_mort") %>%
  arrange(estimate) %>%
  slice(1) %>%
  pull(clean_names)

m_min =
  df_nzv_summ %>%
  select(id, var = all_of(mort_min)) %>%
  summarize(min_id = sample(id[var == min(var)], 1),
            max_id = sample(id[var == max(var)], 1))
m_min
