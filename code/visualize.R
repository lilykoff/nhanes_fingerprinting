library(tidyverse)
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

ids = unique(sample_dat$id)



# dens_df$density = get_density(dens_df$vm, dens_df$lag_vm, n = 100)
sample_dat = read_rds(here::here("data", "fingerprint_data_sample.rds"))
dens_df =
  sample_dat %>%
  group_by(id, second) %>%
  mutate(lag_vm = lag(vm, n = 12)) %>%
  ungroup() %>%
  drop_na()
dens_df =
  dens_df %>%
  ungroup() %>%
  group_by(id) %>%
  group_modify(~ .x %>% mutate(density = get_density(vm, lag_vm, n = 80)))

dens_df %>%
  filter(id %in% ids[1:10]) %>%
  ggplot(aes(x = vm, y = lag_vm, color = density)) +
  geom_point() +
  scale_color_viridis() +
  facet_wrap(.~id)

sample_dat2 = read_rds(here::here("data", "fingerprint_data_sample_temporal.rds"))
ids = unique(sample_dat2$id)

dens_df =
  sample_dat2 %>%
  group_by(id, second) %>%
  mutate(lag_vm = lag(vm, n = 12)) %>%
  ungroup() %>%
  drop_na()
dens_df =
  dens_df %>%
  ungroup() %>%
  group_by(id, data) %>%
  group_modify(~ .x %>% mutate(density = get_density(vm, lag_vm, n = 80)))

purrr::walk(seq(1, 96, 5), \(x){
  p = dens_df %>%
    filter(id %in% ids[x:(x+4)]) %>%
    ggplot(aes(x = vm, y = lag_vm, color = density)) +
    geom_point() +
    scale_color_viridis() +
    facet_grid(data~id)
  print(p)
})
dens_df %>%
  filter(id %in% ids[20:25]) %>%
  ggplot(aes(x = vm, y = lag_vm, color = density)) +
  geom_point() +
  scale_color_viridis() +
  facet_grid(data~id)


p1 = dens_df %>%
  filter(id == 78898) %>%
  mutate(data = factor(data, levels = c("train", "test"), labels = c("Training data", "Testing data"))) %>%
  ggplot(aes(x = vm, y = lag_vm, color = density)) +
  geom_point(size = .85) +
  scale_color_viridis() +
  facet_grid(data~.) +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)") +
  scale_x_continuous(limits=c(0,3), breaks=seq(0, 3, 0.5)) +
  scale_y_continuous(limits=c(0,3), breaks=seq(0, 3, 0.5)) +
  theme(legend.position = c(0.9, 0.7))
p1
p2 = sample_dat2 %>%
  filter(id == 78898) %>%
  mutate(data = factor(data, levels = c("train", "test"), labels = c("Training data", "Testing data"))) %>%
  group_by(data) %>%
  mutate(row = row_number()) %>%
  filter(row <= 800) %>%
  ungroup() %>%
  ggplot(aes(x = row, y = vm)) +
  geom_line() +
  facet_grid(data~.) +
  scale_x_continuous(breaks=seq(0, 800, 80), labels = seq(0, 10, 1)) +
  labs(x = "Time (sec)", y = "Acceleration (g)")

library(patchwork)

p_1 = p2 + p1 + plot_annotation(title = "Subject 1")

# other subs to investigate: 68561, 70844, 67407, 68022, 82860, 69972, 81349, 78898

p3 = dens_df %>%
  filter(id == 68561) %>%
  mutate(data = factor(data, levels = c("train", "test"), labels = c("Training data", "Testing data"))) %>%
  ggplot(aes(x = vm, y = lag_vm, color = density)) +
  geom_point(size = .85) +
  scale_color_viridis() +
  facet_grid(data~.) +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)") +
  scale_x_continuous(limits=c(0,3), breaks=seq(0, 3, 0.5)) +
  scale_y_continuous(limits=c(0,3), breaks=seq(0, 3, 0.5)) +
  theme(legend.position = c(0.9, 0.7))

p4 = sample_dat2 %>%
  filter(id == 68561) %>%
  mutate(data = factor(data, levels = c("train", "test"), labels = c("Training data", "Testing data"))) %>%
  group_by(data) %>%
  mutate(row = row_number()) %>%
  filter(row <= 800) %>%
  ungroup() %>%
  ggplot(aes(x = row, y = vm)) +
  geom_line() +
  facet_grid(data~.) +
  scale_x_continuous(breaks=seq(0, 800, 80), labels = seq(0, 10, 1)) +
  labs(x = "Time (sec)", y = "Acceleration (g)")

library(patchwork)
p_2 = (p4 + p3) + plot_annotation(title = "Subject 2")
png(here::here("manuscript", "figs", "fingerprint_data.png"), width = 10, height = 8, units = "in",
    res = 500)
p_1 / p_2 + plot_annotation(tag_levels = "A")
dev.off()

(p2 + p1) / (p4 + p3)
