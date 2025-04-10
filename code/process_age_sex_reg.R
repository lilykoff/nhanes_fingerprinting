library(tidyverse)

# age_sex_res = readRDS(here::here("data", "age_sex_reg.rds"))
age_sex_res = readRDS(here::here("data", "covar_reg.rds"))
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

x_vars = seq(0, 3, 0.25)

df = tibble(vm = seq(0, 3, 0.125)) %>%
    mutate(lag_vm = dplyr::lag(vm, n = 1)) %>%   # for each second, calculate vm and lagged vm
    mutate(
      cut_sig = cut(
        vm,
        breaks = seq(0, 3, by = 0.25),
        include.lowest = T
      ),
      cut_lagsig = cut(
        lag_vm,
        breaks = seq(0, 3, by = 0.25),
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

old_names = unique(df$cell)
# clean_names = janitor::clean_names(old_names)

temp =
  tibble(x = old_names,
         y = seq(1:length(old_names))) %>%
  pivot_wider(names_from = x, values_from = y)
clean_names = janitor::clean_names(temp) %>%
  colnames()

key = tibble(old_names, clean_names)

df = df %>%
  full_join(key, by = c("cell" = "old_names"))

df %>%
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
  facet_wrap(.~lag) +
  geom_tile(col = "black")+
  theme_classic() +
  geom_text(aes(x = cut_sig, y = cut_lagsig, label = sig))+
  labs(x = "Signal", y = "Lag Signal", title = "Effect of Increasing Age on Grid Cells")


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
