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


df_mat =
  df %>% select(starts_with("x")) %>%
  as.matrix()
xdf =
  df %>%
  mutate(outcome = if_else(id == id[1], 1, 0)) %>%
  select(-starts_with("x")) %>%
  mutate(mat = df_mat)

pfr_fit =
  pfr(
    outcome ~ lf(mat, argvals = seq(1, 432)),
    family = binomial(link = "logit"),
    method = "REML", data = xdf)

predicted_values = predict(pfr_fit, newdata = xdf, type = "link")
predicted_values2 = predicted_values %>% as.numeric()
bind_cols(predicted_values %>% as.numeric, predicted_values2) %>% janitor::clean_names()
pfr_fit_nonlinear =
  pfr(
    outcome ~ af(mat, argvals = seq(1, 432)),
    family = binomial(link = "logit"),
    method = "REML", data = xdf)
pv = predict(pfr_fit_nonlinear, newdata = xdf, type = "link")


gpfr_coef_df =
  coef(pfr_fit) %>%
  mutate(method = "refund::pfr()") %>%
  tf_nest(.id = method, .arg = mat.argvals) %>%
  rename(estimate = value)

pfr_coef_df %>%
  ggplot(aes(y = estimate)) +
  geom_spaghetti(alpha = 1, linewidth = 1.2) +
  labs(x = "Time of day (hours)", y = "Coefficient")

# try with GAM
fit =
    gam(
      outcome ~ s(mat),
      method = "REML",
      data = xdf,
      family = quasibinomial()
    )
lp = predict.gam(fit, newdata = xdf, type = "link")

lp
