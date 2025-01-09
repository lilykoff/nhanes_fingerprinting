# get samples
library(tidyverse)
library(tidymodels)

if(!file.exists(here::here("data", "lily", "data", "nzv_fine.rds"))){
# if(!file.exists(here::here("data", "nzv_fine.rds"))){

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

age_sex_res = read_rds(here::here("data", "lily", "data", "covar_reg_fine.rds"))

age_res = age_sex_res %>%
  filter(term == "age")

sex_res = age_sex_res %>%
  filter(term == "genderMale")

mort_res = age_sex_res %>%
  filter(term == "mortstat1")

age_max =
  age_res %>%
  filter(reg_type == "age_sex_mort") %>%
  arrange(desc(estimate)) %>%
  slice(1) %>%
  pull(var)


a_max =
  df_nzv_summ %>%
  select(id, var = all_of(age_max)) %>%
  summarize(min_id = sample(id[var == min(var)], 1),
            max_id = sample(id[var == max(var)], 1))

a_max

age_min =
  age_res %>%
  filter(reg_type == "age_sex_mort") %>%
  arrange(estimate) %>%
  slice(1) %>%
  pull(var)


a_min =
  df_nzv_summ %>%
  select(id, var = all_of(age_min)) %>%
  summarize(min_id = first(id[var == min(var)]),
            max_id = first(id[var == max(var)]))
a_min

sex_max =
  sex_res %>%
  filter(reg_type == "age_sex_mort") %>%
  arrange(desc(estimate)) %>%
  slice(1) %>%
  pull(var)


s_max =
  df_nzv_summ %>%
  select(id, var = all_of(sex_max)) %>%
  summarize(min_id = first(id[var == min(var)]),
            max_id = first(id[var == max(var)]))

s_max

sex_min =
  sex_max =
  sex_res %>%
  filter(reg_type == "age_sex_mort") %>%
  arrange(estimate) %>%
  slice(1) %>%
  pull(var)

s_min =
  df_nzv_summ %>%
  select(id, var = all_of(sex_min)) %>%
  summarize(min_id = first(id[var == min(var)]),
            max_id = first(id[var == max(var)]))
s_min

mort_max =
  mort_res %>%
  filter(reg_type == "age_sex_mort") %>%
  arrange(desc(estimate)) %>%
  slice(1) %>%
  pull(var)

m_max =
  df_nzv_summ %>%
  select(id, var = all_of(mort_max)) %>%
  summarize(min_id = first(id[var == min(var)]),
            max_id = first(id[var == max(var)]))

m_max

mort_min =
  mort_res %>%
  filter(reg_type == "age_sex_mort") %>%
  arrange(estimate) %>%
  slice(1) %>%
  pull(var)

m_min =
  df_nzv_summ %>%
  select(id, var = all_of(mort_min)) %>%
  summarize(min_id = first(id[var == min(var)]),
            max_id = first(id[var == max(var)]))
m_min

#####
> m_min
# A tibble: 1 × 2
min_id max_id
<dbl>  <int>
  1  70026  25254

> m_max
# A tibble: 1 × 2
min_id max_id
<int>  <int>
  1  25622  21582

> s_min
# A tibble: 1 × 2
min_id max_id
<int>  <int>
  1  75903  39628

> s_max
# A tibble: 1 × 2
min_id max_id
<dbl>  <int>
  1  83595  24302

> a_min
# A tibble: 1 × 2
min_id max_id
<dbl>  <int>
  1  76568  46745

m_min
# A tibble: 1 × 2
min_id max_id
<int>  <int>
  1  61434  19394


file = here::here("data", "lily", "data", "adept_walking_dfs", "pax_y", "61434.csv.gz")

x =read_csv(file)


