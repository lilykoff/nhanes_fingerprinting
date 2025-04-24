library(tidyverse)
library(tidymodels)
library(viridis)
library(patchwork)
options(digits.secs = 3)
all = read_csv(here::here("data", "all_data.csv.gz"))

## need to plot train and test

get_density = function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

split = initial_split(all, prop = 1/2, strata = id)
train = training(split)
test = testing(split)
df =
  train %>% mutate(cat = "train") %>%
  bind_rows(test %>% mutate(cat = "test"))

plotdf =
  df %>%
  group_by(second_id, id, cat, good_pred) %>%
  mutate(lagvm = lag(vm, n = 12)) %>%
  drop_na() %>%
  filter(n() > 10) %>%
  group_modify(~ .x %>% mutate(dens = get_density(.x$vm, .x$lagvm))) %>%

  # group_modify(~ .x %>% mutate(dens = get_density(.x$vm, .x$lagvm, n = 68))) %>%
  ungroup()
gp = unique(plotdf$id[plotdf$good_pred == TRUE])
pp = unique(plotdf$id[plotdf$good_pred != TRUE])

plotdf %>%
  filter(good_pred, cat == "train") %>%
  filter(id == gp[1]) %>%
  ggplot(aes(x = vm, y = lagvm, color = dens))+
  geom_point(size = .1, alpha = .5) +
  scale_color_viridis() +
  scale_x_continuous(limits=c(0,3))+
  scale_y_continuous(limits=c(0,3))

all %>%
  filter(id == gp[2]) %>%
  group_by(second_id) %>%
  mutate(x = row_number()) %>%
  ungroup() %>%
  slice(1:(10*80)) %>%
  ggplot(aes(x= x, y = vm))+
  geom_line() +
  facet_wrap(~second_id, scales = "free_x")

plotdf %>%
  filter(good_pred, cat == "test") %>%
  filter(id == gp[1]) %>%
  ggplot(aes(x = vm, y = lagvm, color = dens))+
  geom_point(size = .1, alpha = .5) +
  scale_color_viridis() +
  scale_x_continuous(limits=c(0,3))+
  scale_y_continuous(limits=c(0,3))

for(sub in gp){
  p = plotdf %>%
    # filter(good_pred) %>%
    mutate(cat = factor(cat, levels = c("train", "test"))) %>%
    filter(id == sub) %>%
    ggplot(aes(x = vm, y = lagvm, color = dens))+
    geom_point(size = .5, alpha = .5) +
    scale_color_viridis(name = "Density") +
    scale_x_continuous(limits=c(0,3))+
    scale_y_continuous(limits=c(0,3)) +
    facet_grid(. ~ cat) +
    labs(x = "Acceleration (g)", y = "Lag Acceleration (g)")
  p2 =
    all %>%
    filter(id == sub) %>%
    group_by(second_id) %>%
    mutate(x = row_number()) %>%
    ungroup() %>%
    slice(1:(10*80)) %>%
    ggplot(aes(x= x, y = vm))+
    geom_line() +
    facet_wrap(~second_id, scales = "free_x")
  print(p + p2)
}

for(sub in pp){
  p = plotdf %>%
    # filter(good_pred) %>%
    mutate(cat = factor(cat, levels = c("train", "test"))) %>%
    filter(id == sub) %>%
    ggplot(aes(x = vm, y = lagvm, color = dens))+
    geom_point(size = .5, alpha = .5) +
    scale_color_viridis(name = "Density") +
    scale_x_continuous(limits=c(0,3))+
    scale_y_continuous(limits=c(0,3)) +
    facet_grid(. ~ cat) +
    labs(x = "Acceleration (g)", y = "Lag Acceleration (g)")
  # print(p)
  p2 =
    all %>%
    filter(id == sub) %>%
    group_by(second_id) %>%
    mutate(x = row_number()) %>%
    ungroup() %>%
    slice(1:(10*80)) %>%
    ggplot(aes(x= x, y = vm))+
    geom_line() +
    facet_wrap(~second_id, scales = "free_x")
  print(p + p2)
}

pdf =
  plotdf %>%
  select(second_id, id, cat) %>%
  distinct()
all %>%
  filter(id == gp[1]) %>%
  left_join(pdf, by = join_by("second_id", "id")) %>%
  group_by(second_id, id, cat) %>%
  mutate(x = row_number()) %>%
  ungroup() %>%
  group_by(cat, x) %>%
  summarize(vm = mean(vm)) %>%
  ungroup() %>%
  ggplot(aes(x= x, y = vm, color = cat, group = cat))+
  geom_line(alpha = .8) +
  facet_wrap(.~cat)
