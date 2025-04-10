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

df_nzv_summ = read_rds(here::here("data", "lily", "data", "nzv_fine.rds")) %>%
  mutate(id = as.character(id))

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
            max_id = sample(id[var == max(var)], 1)) %>%
  mutate(varname = age_max)

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
  summarize(min_id = sample(id[var == min(var)], 1),
            max_id = sample(id[var == max(var)], 1)) %>%
  mutate(varname = age_min)

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
  summarize(min_id = sample(id[var == min(var)], 1),
            max_id = sample(id[var == max(var)], 1)) %>%
  mutate(varname = sex_max)

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
  summarize(min_id = sample(id[var == min(var)], 1),
            max_id = sample(id[var == max(var)], 1)) %>%
  mutate(varname = sex_min)
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
  summarize(min_id = sample(id[var == min(var)], 1),
            max_id = sample(id[var == max(var)], 1)) %>%
  mutate(varname = mort_max)

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
  summarize(min_id = sample(id[var == min(var)], 1),
            max_id = sample(id[var == max(var)], 1)) %>%
  mutate(varname = mort_min)
m_min


file = here::here("data", "lily", "data", "adept_walking_dfs", "pax_g", "62336.csv.gz")

filex = read_csv(file)

df =
  s_min %>% mutate(var = "sex_min") %>%
  bind_rows(s_max %>% mutate(var = "sex_max")) %>%
  bind_rows(m_min %>% mutate(var = "mort_min")) %>%
  bind_rows(m_max %>% mutate(var = "mort_max")) %>%
  bind_rows(a_min %>% mutate(var = "age_min")) %>%
  bind_rows(a_max %>% mutate(var = "age_max")) %>%
  pivot_longer(cols = contains("id"),
               names_to = "name",
               values_to = "id")

covars = readRDS(here::here("data", "lily", "data", "covar_data_accel_subset.rds"))

i = 1
result = list()
for(i in 1:nrow(df)){
  idf = df[i,]
  cyc = covars %>% filter(SEQN == as.numeric(idf$id)) %>% pull(data_release_cycle)
  cyc_char = if_else(cyc == 7, "pax_g", "pax_h")
  fpath = file.path(here::here("data", "lily",
                               "data", "adept_walking_dfs",
                               cyc_char,
                               paste0(idf$id, ".csv.gz")))
  x = read_csv(fpath) %>%
    mutate(wave = cyc_char,
           var = idf$var,
           varname = idf$varname,
           name = idf$name,
           id = idf$id) %>%
    select(id, wave, var, name, everything())
  result[[i]] = x
  rm(x)
}

res_final = bind_rows(result)
write_rds(res_final, here::here("data", "lily", "data", "sample_reg_subjects.rds"),
          compress = "xz")

