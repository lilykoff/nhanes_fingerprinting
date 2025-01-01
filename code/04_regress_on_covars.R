library(tidyverse)
library(tidymodels)
# df = readr::read_csv(here::here("data", "lily", "data", "sample_grid_cells.csv.gz"))
force = FALSE

if(!file.exists(here::here("data", "lily", "data", "covar_reg.rds")) || force) {
  df = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells.csv.gz"))
  # everyone has 180 sec of data

  nzv_trans =
    recipe(id ~ ., data = df) %>%
    step_nzv(all_predictors())


  nzv_estimates = prep(nzv_trans)

  nzv = colnames(juice(nzv_estimates))

  df_nzv = df %>% select(id, all_of(nzv))

  df_nzv_summ =
    df_nzv %>%
    group_by(id) %>%
    mutate(n = n()) %>%
    group_by(id, n) %>%
    summarize(across(starts_with("x"), ~sum(.x))) %>%
    ungroup() %>%
    mutate(across(ends_with("12"), ~.x / (n * (80-12))),
           across(ends_with("24"), ~.x / (n * (80-24))),
           across(ends_with("36"), ~.x / (n * (80-36))))

  covars = readRDS(here::here("data", "lily", "data", "covar_data_accel_subset.rds"))

  df_nzv_summ_covar =
    df_nzv_summ %>%
    left_join(covars %>% select(SEQN, age = age_in_years_at_screening, gender, mortstat), by = c("id" = "SEQN"))

  cols = df_nzv_summ_covar %>% select(starts_with("x")) %>% colnames()

  sex_coefs =
    map_dfr(.x = cols,
        .f = function(x){
         tmp =
           df_nzv_summ_covar %>%
            select(y = all_of(x), gender, age)
         smod = lm(y ~ gender, data = tmp)
         smod %>%
           broom::tidy() %>%
           slice(2) %>%
           mutate(var = x)
        }) %>%
    mutate(reg_type = "sex_uni")

  age_coefs =
    map_dfr(.x = cols,
            .f = function(x){
              tmp =
                df_nzv_summ_covar %>%
                select(y = all_of(x), gender, age)
              smod = lm(y ~ age, data = tmp)
              smod %>%
                broom::tidy() %>%
                slice(2) %>%
                mutate(var = x)
            }) %>%
    mutate(reg_type = "age_uni")

  mort_coefs =
    map_dfr(.x = cols,
            .f = function(x){
              tmp =
                df_nzv_summ_covar %>%
                select(y = all_of(x), gender, age, mortstat) %>%
                mutate(mortstat = factor(mortstat))
              smod = lm(y ~ mortstat, data = tmp)
              smod %>%
                broom::tidy() %>%
                slice(2) %>%
                mutate(var = x)
            }) %>%
    mutate(reg_type = "mort_uni")

  agesex_coefs =
    map_dfr(.x = cols,
            .f = function(x){
              tmp =
                df_nzv_summ_covar %>%
                select(y = all_of(x), gender, age, mortstat) %>%
                mutate(mortstat = factor(mortstat))
              smod = lm(y ~ gender + age, data = tmp)
              smod %>%
                broom::tidy() %>%
                slice(2:3) %>%
                mutate(var = x)
            }) %>%
    mutate(reg_type = "age_sex")

  agesexmort_coefs =
    map_dfr(.x = cols,
            .f = function(x){
              tmp =
                df_nzv_summ_covar %>%
                select(y = all_of(x), gender, age, mortstat) %>%
                mutate(mortstat = factor(mortstat))
              smod = lm(y ~ gender + age + mortstat, data = tmp)
              smod %>%
                broom::tidy() %>%
                slice(2:4) %>%
                mutate(var = x)
            }) %>%
    mutate(reg_type = "age_sex_mort")

  agemort_coefs =
    map_dfr(.x = cols,
            .f = function(x){
              tmp =
                df_nzv_summ_covar %>%
                select(y = all_of(x), gender, age, mortstat) %>%
                mutate(mortstat = factor(mortstat))
              smod = lm(y ~  age + mortstat, data = tmp)
              smod %>%
                broom::tidy() %>%
                slice(2:3) %>%
                mutate(var = x)
            }) %>%
    mutate(reg_type = "age_mort")

  # age_coefs %>% mutate(mag = abs(estimate)) %>% arrange(desc(mag))
  # sex_coefs %>% mutate(mag = abs(estimate)) %>% arrange(desc(mag))

  res =
    bind_rows(age_coefs, sex_coefs, mort_coefs, agesex_coefs,
              agesexmort_coefs, agemort_coefs)

  saveRDS(res, here::here("data", "lily", "data", "covar_reg.rds"))
}


if(!file.exists(here::here("data", "lily", "data", "covar_reg_fine.rds")) || force) {
  df = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells_fine.csv.gz"))
  # everyone has 180 sec of data

  nzv_trans =
    recipe(id ~ ., data = df) %>%
    step_nzv(all_predictors())


  nzv_estimates = prep(nzv_trans)

  nzv = colnames(juice(nzv_estimates))

  df_nzv = df %>% select(id, all_of(nzv))

  df_nzv_summ =
    df_nzv %>%
    group_by(id) %>%
    mutate(n = n()) %>%
    group_by(id, n) %>%
    summarize(across(starts_with("x"), ~sum(.x))) %>%
    ungroup() %>%
    mutate(across(ends_with("12"), ~.x / (n * (80-12))),
           across(ends_with("24"), ~.x / (n * (80-24))),
           across(ends_with("36"), ~.x / (n * (80-36))))

  covars = readRDS(here::here("data", "lily", "data", "covar_data_accel_subset.rds"))

  df_nzv_summ_covar =
    df_nzv_summ %>%
    left_join(covars %>% select(SEQN, age = age_in_years_at_screening, gender, mortstat), by = c("id" = "SEQN"))

  cols = df_nzv_summ_covar %>% select(starts_with("x")) %>% colnames()

  sex_coefs =
    map_dfr(.x = cols,
            .f = function(x){
              tmp =
                df_nzv_summ_covar %>%
                select(y = all_of(x), gender, age)
              smod = lm(y ~ gender, data = tmp)
              smod %>%
                broom::tidy() %>%
                slice(2) %>%
                mutate(var = x)
            }) %>%
    mutate(reg_type = "sex_uni")

  age_coefs =
    map_dfr(.x = cols,
            .f = function(x){
              tmp =
                df_nzv_summ_covar %>%
                select(y = all_of(x), gender, age)
              smod = lm(y ~ age, data = tmp)
              smod %>%
                broom::tidy() %>%
                slice(2) %>%
                mutate(var = x)
            }) %>%
    mutate(reg_type = "age_uni")

  mort_coefs =
    map_dfr(.x = cols,
            .f = function(x){
              tmp =
                df_nzv_summ_covar %>%
                select(y = all_of(x), gender, age, mortstat) %>%
                mutate(mortstat = factor(mortstat))
              smod = lm(y ~ mortstat, data = tmp)
              smod %>%
                broom::tidy() %>%
                slice(2) %>%
                mutate(var = x)
            }) %>%
    mutate(reg_type = "mort_uni")

  agesex_coefs =
    map_dfr(.x = cols,
            .f = function(x){
              tmp =
                df_nzv_summ_covar %>%
                select(y = all_of(x), gender, age, mortstat) %>%
                mutate(mortstat = factor(mortstat))
              smod = lm(y ~ gender + age, data = tmp)
              smod %>%
                broom::tidy() %>%
                slice(2:3) %>%
                mutate(var = x)
            }) %>%
    mutate(reg_type = "age_sex")

  agesexmort_coefs =
    map_dfr(.x = cols,
            .f = function(x){
              tmp =
                df_nzv_summ_covar %>%
                select(y = all_of(x), gender, age, mortstat) %>%
                mutate(mortstat = factor(mortstat))
              smod = lm(y ~ gender + age + mortstat, data = tmp)
              smod %>%
                broom::tidy() %>%
                slice(2:4) %>%
                mutate(var = x)
            }) %>%
    mutate(reg_type = "age_sex_mort")

  agemort_coefs =
    map_dfr(.x = cols,
            .f = function(x){
              tmp =
                df_nzv_summ_covar %>%
                select(y = all_of(x), gender, age, mortstat) %>%
                mutate(mortstat = factor(mortstat))
              smod = lm(y ~  age + mortstat, data = tmp)
              smod %>%
                broom::tidy() %>%
                slice(2:3) %>%
                mutate(var = x)
            }) %>%
    mutate(reg_type = "age_mort")

  age_coefs %>% mutate(mag = abs(estimate)) %>% arrange(desc(mag))
  sex_coefs %>% mutate(mag = abs(estimate)) %>% arrange(desc(mag))

  res =
    bind_rows(age_coefs, sex_coefs, mort_coefs, agesex_coefs,
              agesexmort_coefs, agemort_coefs)

  saveRDS(res, here::here("data", "lily", "data", "covar_reg_fine.rds"))
}
