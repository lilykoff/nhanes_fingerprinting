library(tidyverse)

# age_sex_res = readRDS(here::here("data", "age_sex_reg.rds"))
age_sex_res = readRDS(here::here("data", "covar_reg_fine.rds"))
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


unique(age_res$var)

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


### get largest coefficient for age res
df_fine %>%
  left_join(age_res, by = c("clean_names" = "var")) %>%
  filter(!is.na(lag) & reg_type == "age_sex_mort") %>%
  arrange(desc(estimate)) %>%
  slice(1) %>%
  pull(clean_names)

df_fine %>%
  left_join(age_res, by = c("clean_names" = "var")) %>%
  filter(!is.na(lag) & reg_type == "age_sex_mort") %>%
  arrange(estimate) %>%
  slice(1) %>%
  pull(clean_names)



df_fine %>%
  left_join(age_res, by = c("clean_names" = "var")) %>%
  filter(!is.na(lag)) %>%
  mutate(sig =
           case_when(p.value < 0.001 ~ "***",
                     p.value < 0.01 ~ "**",
                     p.value < 0.05 ~ "*",
                     TRUE ~ "")) %>%
  ggplot(aes(x =cut_sig, y = cut_lagsig, fill = estimate)) +
  # scale_fill_viridis() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  # facet_grid(reg_type~lag) +
  geom_tile(col = "black")+
  theme_classic() +
  geom_text(aes(x = cut_sig, y = cut_lagsig, label = sig))+
  labs(x = "Signal", y = "Lag Signal", title = "Effect of Increasing Age on Grid Cells")


df_fine %>%
  left_join(age_res, by = c("clean_names" = "var")) %>%
  filter(!is.na(lag)) %>%
  filter(p.value < 0.001) %>%
  ggplot(aes(x =cut_sig, y = cut_lagsig, fill = estimate)) +
  # scale_fill_viridis() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  # facet_wrap(.~lag) +
  facet_grid(reg_type~lag) +
  geom_tile()+
  theme_grey() +
  geom_raster(interpolate=TRUE) +
  # geom_text(aes(x = cut_sig, y = cut_lagsig, label = sig))+
  labs(x = "Signal", y = "Lag Signal", title = "Effect of Increasing Age on Grid Cells")


df_fine %>%
  left_join(sex_res, by = c("clean_names" = "var")) %>%
  filter(!is.na(lag)) %>%
  filter(p.value < 0.001) %>%
  ggplot(aes(x =cut_sig, y = cut_lagsig, fill = estimate)) +
  # scale_fill_viridis() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  # facet_wrap(.~lag) +
  facet_grid(reg_type~lag) +
  geom_tile()+
  theme_grey() +
  geom_raster(interpolate=TRUE) +
  # geom_text(aes(x = cut_sig, y = cut_lagsig, label = sig))+
  labs(x = "Signal", y = "Lag Signal", title = "Effect of Male on Grid Cells")


df_fine %>%
  left_join(mort_res, by = c("clean_names" = "var")) %>%
  filter(!is.na(lag)) %>%
  filter(p.value < 0.001) %>%
  ggplot(aes(x =cut_sig, y = cut_lagsig, fill = estimate)) +
  # scale_fill_viridis() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  # facet_wrap(.~lag) +
  facet_grid(reg_type~lag) +
  geom_tile()+
  theme_grey() +
  geom_raster(interpolate=TRUE) +
  # geom_text(aes(x = cut_sig, y = cut_lagsig, label = sig))+
  labs(x = "Signal", y = "Lag Signal", title = "Effect of Mortality on Grid Cells")







xdf = df_fine %>%
  left_join(age_res, by = c("clean_names" = "var"))  %>%
  filter(lag == 12)
library(mgcv)
m <- gam(estimate ~ te(num_x, num_y), data = xdf, method = "REML")
# get predictions
xdf$pred <- predict(m, newdata = xdf, type = "response")

xdf %>%
  pivot_longer(cols = c(estimate, pred)) %>%
  ggplot(aes(x =cut_sig, y = cut_lagsig, fill = value)) +
  # scale_fill_viridis() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  facet_wrap(.~name) +
  geom_tile()+
  theme_grey() +
  geom_raster(interpolate=TRUE) +
  # geom_text(aes(x = cut_sig, y = cut_lagsig, label = sig))+
  labs(x = "Signal", y = "Lag Signal", title = "Effect of Increasing Age on Grid Cells")


m <- gam(scored ~ te(x, y), data = myDF, family = binomial, method = "REML")


m <- matrix(rnorm(20),5,dimnames=list(x=1:5,y=1:4))

plot2d_1 <- reshape2::melt(m,value.name="z")
ggplot(plot2d_1, aes(x,y, fill=z))


df %>%
  left_join(sex_res, by = c("clean_names" = "var")) %>%
  filter(!is.na(lag)) %>%
  mutate(sig =
           case_when(p.value < 0.001 ~ "***",
                     p.value < 0.01 ~ "**",
                     p.value < 0.05 ~ "*",
                     TRUE ~ "")) %>%
  ggplot(aes(x =cut_sig, y = cut_lagsig, fill = estimate)) +
  theme_classic()+
  # scale_fill_viridis() +
  facet_wrap(.~lag) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_tile(col = "black")+
  geom_text(aes(x = cut_sig, y = cut_lagsig, label = sig))+
  labs(x = "Signal", y = "Lag Signal", title = "Effect of Gender: Male on Grid Cells")

df %>%
  left_join(mort_res, by = c("clean_names" = "var")) %>%
  filter(!is.na(lag)) %>%
  mutate(sig =
           case_when(p.value < 0.001 ~ "***",
                     p.value < 0.01 ~ "**",
                     p.value < 0.05 ~ "*",
                     TRUE ~ "")) %>%
  ggplot(aes(x =cut_sig, y = cut_lagsig, fill = estimate)) +
  theme_classic()+
  # scale_fill_viridis() +
  facet_wrap(.~lag) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_tile(col = "black")+
  geom_text(aes(x = cut_sig, y = cut_lagsig, label = sig))+
  labs(x = "Signal", y = "Lag Signal", title = "Effect of Mortality on Grid Cells")



##### example subs
options(digits.secs = 3)
examples = read_rds(here::here("data", "sample_reg_subjects.rds"))

sub = examples$id[1]

ex_small =
  map_dfr(.x = unique(examples$id),
          .f = function(sub){
            walking_df = examples %>%
              filter(id == sub) %>%
              select(HEADER_TIMESTAMP, X, Y, Z, second_id) %>%
              mutate(second = floor_date(HEADER_TIMESTAMP, unit = "secs"),
                     day = floor_date(HEADER_TIMESTAMP, unit = "days"))
            segments_10 = walking_df %>%
              select(second, day) %>%
              distinct() %>%
              mutate(timediff = as.numeric(difftime(second, dplyr::lag(second, n = 1), units = "secs")),
                     ltwosec = (timediff <= 2)*1,
                     rleid = data.table::rleid(ltwosec)) %>%
              filter(ltwosec == 1) %>%
              group_by(rleid, day) %>%
              summarize(n_seconds = n(),
                        start = min(second),
                        end = max(second)) %>%
              filter(n_seconds >= 10)

            seconds_key =
              segments_10 %>%
              group_by(rleid, day) %>%
              tidyr::expand(second = seq(start, end, "sec"))

            df_small =
              walking_df %>%
              inner_join(seconds_key, by = c("second", "day"))
            set.seed(123)
            density =
              df_small %>%
              sample_n(size = 180, replace = FALSE) %>%
              select(second, day, second_id)

            examples %>%
              filter(id == sub) %>%
              filter(second_id %in% density$second_id)

          })

ex_small %>%
  filter(var == "sex_min") %>%
  mutate(vm = sqrt(X^2+ Y^2 + Z^2)) %>%
  # filter(second_id <= 180) %>%
  group_by(id) %>%
  mutate(r = row_number()) %>%
  ggplot(aes(x = r, y = vm)) +
  geom_line() +
  facet_grid(.~name)

ex_small %>%
  filter(var == "sex_min") %>%
  mutate(vm = sqrt(X^2+ Y^2 + Z^2)) %>%
  group_by(id) %>%
  mutate(r = row_number()) %>%
  filter(r <= 80 * 10) %>%
  pivot_longer(cols= X:Z, names_to = "axis", values_to = "value") %>%
  ggplot(aes(x = r, y = value, color = axis)) +
  facet_grid(.~id) +
  geom_line()

ex_small %>%
  filter(var == "sex_min") %>%
  mutate(vm = sqrt(X^2+ Y^2 + Z^2)) %>%
  group_by(id) %>%
  mutate(r = row_number()) %>%
  filter(r <= 80 * 10) %>%
  # pivot_longer(cols= X:Z, names_to = "axis", values_to = "value") %>%
  ggplot(aes(x = r, y = vm)) +
  facet_grid(.~id) +
  geom_line()


ex_small %>%
  filter(var == "sex_max") %>%
  mutate(vm = sqrt(X^2+ Y^2 + Z^2)) %>%
  # filter(second_id <= 180) %>%
  group_by(id) %>%
  mutate(r = row_number()) %>%
  ggplot(aes(x = r, y = vm)) +
  geom_line() +
  facet_grid(.~name)

ex_small %>%
  filter(var == "sex_max") %>%
  mutate(vm = sqrt(X^2+ Y^2 + Z^2)) %>%
  group_by(id) %>%
  mutate(r = row_number()) %>%
  filter(r <= 80 * 10) %>%
  pivot_longer(cols= X:Z, names_to = "axis", values_to = "value") %>%
  ggplot(aes(x = r, y = value, color = axis)) +
  facet_grid(.~id) +
  geom_line()

ex_small %>%
  filter(var == "sex_max") %>%
  mutate(vm = sqrt(X^2+ Y^2 + Z^2)) %>%
  group_by(id) %>%
  mutate(r = row_number()) %>%
  # filter(r <= 80 * 10) %>%
  filter(between(r, 40*80, 50*80)) %>%
  # pivot_longer(cols= X:Z, names_to = "axis", values_to = "value") %>%
  ggplot(aes(x = r, y = vm)) +
  facet_grid(.~id) +
  geom_line()
ex_small %>%
  filter(var == "sex_max") %>%
  mutate(vm = sqrt(X^2+ Y^2 + Z^2)) %>%
  group_by(id) %>%
  mutate(r = row_number()) %>%
  # filter(r <= 80 * 10) %>%
  filter(between(r, 40*80, 41*80)) %>%
  # pivot_longer(cols= X:Z, names_to = "axis", values_to = "value") %>%
  ggplot(aes(x = r, y = vm)) +
  facet_grid(.~id) +
  geom_line()



ex_small %>%
  filter(var == "age_min") %>%
  mutate(vm = sqrt(X^2+ Y^2 + Z^2)) %>%
  # filter(second_id <= 180) %>%
  group_by(id) %>%
  mutate(r = row_number()) %>%
  ggplot(aes(x = r, y = vm)) +
  geom_line() +
  facet_grid(.~name)


ex_small %>%
  filter(var == "age_max") %>%
  mutate(vm = sqrt(X^2+ Y^2 + Z^2)) %>%
  # filter(second_id <= 180) %>%
  group_by(id) %>%
  mutate(r = row_number()) %>%
  ggplot(aes(x = r, y = vm)) +
  geom_line() +
  facet_grid(.~name)

ex_small %>%
  filter(var == "age_max") %>%
  mutate(vm = sqrt(X^2+ Y^2 + Z^2)) %>%
  group_by(id) %>%
  mutate(r = row_number()) %>%
  # filter(r <= 80 * 10) %>%
  filter(between(r, 40*80, 50*80)) %>%
  # pivot_longer(cols= X:Z, names_to = "axis", values_to = "value") %>%
  ggplot(aes(x = r, y = vm)) +
  facet_grid(.~id) +
  geom_line()


ex_small %>%
  filter(var == "mort_min") %>%
  mutate(vm = sqrt(X^2+ Y^2 + Z^2)) %>%
  # filter(second_id <= 180) %>%
  group_by(id) %>%
  mutate(r = row_number()) %>%
  ggplot(aes(x = r, y = vm)) +
  geom_line() +
  facet_grid(.~name)


ex_small %>%
  filter(var == "mort_max") %>%
  mutate(vm = sqrt(X^2+ Y^2 + Z^2)) %>%
  # filter(second_id <= 180) %>%
  group_by(id) %>%
  mutate(r = row_number()) %>%
  ggplot(aes(x = r, y = vm)) +
  geom_line() +
  facet_grid(.~id)

ex_small %>%
  filter(var == "mort_max" & name == "max_id") %>%
  mutate(vm = sqrt(X^2+ Y^2 + Z^2)) %>%
  group_by(id) %>%
  mutate(r = row_number()) %>%
  filter(r <= 800) %>%
  ggplot(aes(x = r, y = vm)) +
  geom_line() +
  # facet_grid(.~name) +
  scale_x_continuous(breaks=seq(0,800,80),
                     labels =seq(0,10,1))+
  labs(x = "Seconds", y = "VM") +
  theme_bw()

ex_small %>%
  filter(var == "mort_max" & name == "max_id") %>%
  # mutate(vm = sqrt(X^2+ Y^2 + Z^2)) %>%
  mutate(r = row_number()) %>%
  filter(r <= 800) %>%
  group_by(id) %>%
  rename(vname = name) %>%
  pivot_longer(cols = X:Z) %>%
  ggplot(aes(x = r, y = value, color = name)) +
  geom_line() +
  # theme(legend.position = "none") +
  # facet_grid(.~name) +
  scale_x_continuous(breaks=seq(0,800,80),
                     labels =seq(0,10,1))+
  labs(x = "Seconds", y = "VM") +
  theme_bw() +
  scale_color_discrete(name = "")

####### old
examples %>%
  filter(var == "sex_min") %>%
  pull(varname) %>%
  unique()

df_fine %>%
  filter(clean_names == "x0_8_0_9_0_8_0_9")
x =
  examples %>%
  filter(var == "sex_min") %>%
  mutate(vm = sqrt(X^2+ Y^2 + Z^2)) %>%
  select(id,second_id) %>%
  distinct() %>%
  group_by(id) %>%
  summarize(n = n(),
            across(second_id, list(min, max)))
examples %>%
  filter(var == "sex_min") %>%
  mutate(vm = sqrt(X^2+ Y^2 + Z^2)) %>%
  # filter(second_id <= 180) %>%
  group_by(id) %>%
  mutate(r = row_number()) %>%
  ggplot(aes(x = r, y = vm)) +
  geom_line() +
  facet_grid(.~name)

examples %>%
  filter(var == "sex_min") %>%
  mutate(vm = sqrt(X^2+ Y^2 + Z^2)) %>%
  # filter(second_id <= 180) %>%
  group_by(id) %>%
  mutate(r = row_number()) %>%
  filter(between(r, 50000, 55000)) %>%
  ggplot(aes(x = r, y = vm)) +
  geom_line() +
  facet_grid(.~name)
examples$varname


examples %>%
  filter(var == "sex_max") %>%
  mutate(vm = sqrt(X^2+ Y^2 + Z^2)) %>%
  # filter(second_id <= 180) %>%
  group_by(id) %>%
  mutate(r = row_number()) %>%
  ggplot(aes(x = r, y = vm)) +
  geom_line() +
  facet_grid(.~name)

examples %>%
  filter(var == "sex_max") %>%
  mutate(vm = sqrt(X^2+ Y^2 + Z^2)) %>%
  # filter(second_id <= 180) %>%
  group_by(id) %>%
  mutate(r = row_number()) %>%
  filter(between(r, 5000, 7000)) %>%
  ggplot(aes(x = r, y = vm)) +
  geom_line() +
  facet_grid(.~name)
