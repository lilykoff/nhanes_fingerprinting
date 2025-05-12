library(tidyverse)
theme_set(theme_light(base_size = 14))

preds = list.files(here::here("data", "lily", "data","fingerprint_res", "1000"),
                   full.names = TRUE)
x = read_rds(preds[1])
summary =
  x %>%
  group_by(true_Participant) %>%
  mutate(sec = row_number()) %>%
  pivot_longer(cols = -c("true_Participant", "sec"), names_to = "name", values_to = "pred") %>%
  mutate(model = as.numeric(sub(".*x", "", name))) %>%
  select(-name) %>%
  # now we have the prediction for each second for each model / true Participant combo
  ungroup() %>%
  group_by(true_Participant, model) %>%
  # get mean probability across seconds for each true Participant / model combo
  summarize(mean_pred = mean(pred, na.rm = TRUE), .groups = "drop") %>%
  group_by(true_Participant) %>%
  mutate(
    rank = rank(-mean_pred)
  ) %>% # get the rank for each prediction
  ungroup() %>%
  filter(model == true_Participant) %>% # only keep the correct combos and get ranks
  mutate(
    rank1 = if_else(rank == 1, 1, 0),
    rank5 = if_else(rank <= 5, 1, 0)
  )

summary %>%
  filter(rank1 == 1) %>%
  arrange(desc(mean_pred))

# 6        67363 67363    0.109      1     1     1
# 7        77035 77035    0.101      1     1     1
# 8        67779 67779    0.101      1     1     1
# 9        73754 73754    0.0957     1     1     1

x %>%
  filter(true_Participant == 77035) %>%
  mutate(sec = row_number()) %>%
  pivot_longer(cols = -c("true_Participant", "sec"), names_to = "name", values_to = "pred") %>%
  mutate(model = as.numeric(sub(".*x", "", name))) %>%
  select(-name) %>%
  # now we have the prediction for each second for each model / true Participant combo
  ungroup() %>%
  group_by(true_Participant, model) %>%
  # get mean probability across seconds for each true Participant / model combo
  summarize(mean_pred = mean(pred, na.rm = TRUE), .groups = "drop")  %>%
  arrange(desc(mean_pred)) %>%
  slice(1:10)


# true_Participant model mean_pred
# <dbl> <dbl>     <dbl>
#   1        77035 77035   0.101
# 2        77035 64929   0.00763
# 3        77035 70412   0.00701
# 4        77035 63753   0.00680
# 5        77035 82860   0.00647
# 6        77035 78675   0.00643
# 7        77035 67468   0.00573
# 8        77035 63449   0.00562
# 9        77035 77705   0.00528
# 10        77035 83399   0.00470

## get fingerprints for those subs
fnames = readRDS(here::here("data", "raw", "all_filenames.rds"))

ids = x %>%
  filter(true_Participant == 77035) %>%
  mutate(sec = row_number()) %>%
  pivot_longer(cols = -c("true_Participant", "sec"), names_to = "name", values_to = "pred") %>%
  mutate(model = as.numeric(sub(".*x", "", name))) %>%
  select(-name) %>%
  # now we have the prediction for each second for each model / true Participant combo
  ungroup() %>%
  group_by(true_Participant, model) %>%
  # get mean probability across seconds for each true Participant / model combo
  summarize(mean_pred = mean(pred, na.rm = TRUE), .groups = "drop")  %>%
  arrange(desc(mean_pred)) %>%
  slice(1:10) %>%
  pull(model)

get_density = function(Participant, df){
  idf = df %>% filter(id == Participant)
  x = try({
    walking_df =
      readr::read_csv(here::here("data", "lily", "data", "fingerprint_data", idf$version, paste0(idf$id, ".csv.gz")))
    # get segments of consecutive walking with < 2 seconds between them that are at least 10 seconds long
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

    # key of those times
    seconds_key =
      segments_10 %>%
      group_by(rleid, day) %>%
      tidyr::expand(second = seq(start, end, "sec"))

    df_small =
      walking_df %>%
      inner_join(seconds_key, by = c("second", "day"))

    # if there are at least 3 mins of data, sample 3 mins randomly (so that everyone has same amount of walking)
    if(nrow(df_small) >= 180) {
      set.seed(123)
      density =
        df_small %>%
        sample_n(size = 180, replace = FALSE) %>%
        mutate(id = Participant) %>%
        select(second, id)
      walking_dat = read_csv(
        here::here(
          "data",
          "lily",
          "data",
          "adept_walking_dfs",
          idf$version,
          paste0(idf$id, ".csv.gz")
        )
      )
      fprint_seconds = walking_dat %>%
        filter(second %in% density$second) %>%
        mutate(id = Participant)
      rm(density); rm(walking_dat)

    } else {
      fprint_seconds = NULL
    }
    fprint_seconds
  })
  x
}



walking_dat = map_dfr(.x = ids,
                      .f = get_density,
                      df = fnames)
#
# walking_dat =
#   bind_rows(walking_dat)

write_rds(walking_dat,  here::here("data", "lily", "data", "fingerprint_data_example.rds"))


### now locally
walking_dat = read_rds(here::here("data", "fingerprint_data_example.rds"))
ids = unique(walking_dat$id)
ids = ids[1:5]
walking_dat = walking_dat %>% filter(id %in% ids)
walking_dat$id = factor(walking_dat$id)

library(tidymodels)

second_df =
  walking_dat %>%
  select(id, second_id) %>%
  distinct()
splt = initial_split(second_df, strata = id, prop = 3/4)
train_secs = training(splt)
test_secs = testing(splt)


train =
  walking_dat %>%
  inner_join(train_secs, by = c("id", "second_id")) %>%
  mutate(data = "train")

test =
  walking_dat %>%
  inner_join(test_secs, by = c("id", "second_id")) %>%
  mutate(data = "test")


get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

library(ggnewscale)

dens_df =
  train %>%
  group_by(id, second_id, data) %>%
  mutate(lag_vm = lag(vm, n = 12)) %>%
  ungroup() %>%
  drop_na()
dens_df_12 =
  dens_df %>%
  ungroup() %>%
  group_by(id, data) %>%
  group_modify(~ .x %>% mutate(density = get_density(vm, lag_vm, n = 80))) %>%
  ungroup()

dens_df =
  train %>%
  group_by(id, second_id, data) %>%
  mutate(lag_vm = lag(vm, n = 24)) %>%
  ungroup() %>%
  drop_na()

dens_df_24 =
  dens_df %>%
  ungroup() %>%
  group_by(id, data) %>%
  group_modify(~ .x %>% mutate(density = get_density(vm, lag_vm, n = 80))) %>%
  ungroup()

dens_df =
  train %>%
  group_by(id, second_id, data) %>%
  mutate(lag_vm = lag(vm, n = 36)) %>%
  ungroup() %>%
  drop_na()

dens_df_36 =
  dens_df %>%
  ungroup() %>%
  group_by(id, data) %>%
  group_modify(~ .x %>% mutate(density = get_density(vm, lag_vm, n = 80))) %>%
  ungroup()

# dens_df %>%
#   ggplot(aes(x = vm, y = lag_vm, color = density)) +
#   geom_point(size = .5) +
#   scale_color_viridis() +
#   facet_grid(data~id)

true_sub12 = dens_df_12 %>%
  filter(id == 77035 & data == "train") %>%
  ggplot(aes(x = vm, y = lag_vm, color = density)) +
  geom_point(size = 1) +
  scale_color_viridis_b(option = "C") +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)") +
  scale_x_continuous(limits=c(0,3)) +
  scale_y_continuous(limits=c(0,3))

true_sub24 = dens_df_24 %>%
  filter(id == 77035 & data == "train") %>%
  ggplot(aes(x = vm, y = lag_vm, color = density)) +
  geom_point(size = 1) +
  scale_color_viridis_b(option = "C") +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  theme(legend.position = "none") +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)")  +
  scale_x_continuous(limits=c(0,3)) +
  scale_y_continuous(limits=c(0,3))
true_sub36 = dens_df_36 %>%
  filter(id == 77035) %>%
  ggplot(aes(x = vm, y = lag_vm, color = density)) +
  geom_point(size = 1) +
  scale_color_viridis_b(option = "C") +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)") +
  scale_x_continuous(limits=c(0,3)) +
  scale_y_continuous(limits=c(0,3))

true_sub12 + true_sub24 + true_sub36

all_dens=
  dens_df_12 %>%
  mutate(lag = "12") %>%
  bind_rows(dens_df_24 %>% mutate(lag = "24")) %>%
  bind_rows(dens_df_36 %>% mutate(lag = "36")) %>%
  filter(id == "77035") %>%
  mutate(lag = factor(lag, labels = c("Lag = 0.15s", "Lag = 0.30s", "Lag = 0.45s")),
         id = "Participant 1")


p <- ggplot()

# Get unique facet combinations
facet_levels <- all_dens %>% distinct(id, lag)

# Loop over each facet and add a layer
for (i in seq_len(nrow(facet_levels))) {
  this_id <- facet_levels$id[i]
  this_data <- facet_levels$lag[i]

  # Filter data for this facet
  this_df <- all_dens %>% filter(id == this_id, lag == this_data)

  # Add the layer
  p <- p +
    geom_point(
      data = this_df,
      aes(x = vm, y = lag_vm, color = density),
      size = 1
    ) +
    scale_color_viridis_b(option = "C") +
    new_scale_color()
}

# Add faceting after all layers
p1 = p + facet_grid(lag ~ id) +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)", title = "Training Data") +
  scale_x_continuous(limits=c(0,3)) +
  scale_y_continuous(limits=c(0,3))

####

dens_df =
  test %>%
  group_by(id, second_id, data) %>%
  mutate(lag_vm = lag(vm, n = 12)) %>%
  ungroup() %>%
  drop_na()
dens_df_12a =
  dens_df %>%
  ungroup() %>%
  group_by(id, data) %>%
  group_modify(~ .x %>% mutate(density = get_density(vm, lag_vm, n = 80))) %>%
  ungroup()

dens_df =
  test %>%
  group_by(id, second_id, data) %>%
  mutate(lag_vm = lag(vm, n = 24)) %>%
  ungroup() %>%
  drop_na()

dens_df_24a =
  dens_df %>%
  ungroup() %>%
  group_by(id, data) %>%
  group_modify(~ .x %>% mutate(density = get_density(vm, lag_vm, n = 80))) %>%
  ungroup()

dens_df =
  test %>%
  group_by(id, second_id, data) %>%
  mutate(lag_vm = lag(vm, n = 36)) %>%
  ungroup() %>%
  drop_na()

dens_df_36a =
  dens_df %>%
  ungroup() %>%
  group_by(id, data) %>%
  group_modify(~ .x %>% mutate(density = get_density(vm, lag_vm, n = 80))) %>%
  ungroup()

all_densa =
  dens_df_12a %>%
  mutate(lag = "12") %>%
  bind_rows(dens_df_24a %>% mutate(lag = "24")) %>%
  bind_rows(dens_df_36a %>% mutate(lag = "36")) %>%
  mutate(id = fct_relevel(id, "77035", after = 0L)) %>%
  mutate(id = paste0("Participant ", as.factor(as.numeric(id)))) %>%
  mutate(id = case_when(
    id == "Participant 1" ~ "Participant A",
    id == "Participant 2" ~ "Participant B",
    id == "Participant 3" ~ "Participant C",
    id == "Participant 4" ~ "Participant D",
    .default =  "Participant E"
  )) %>%
  mutate(lag = factor(lag, labels = c("Lag = 0.15s", "Lag = 0.30s", "Lag = 0.45s")))

p <- ggplot()

# Get unique facet combinations
facet_levels <- all_densa %>% distinct(id, lag)

# Loop over each facet and add a layer
for (i in seq_len(nrow(facet_levels))) {
  this_id <- facet_levels$id[i]
  this_data <- facet_levels$lag[i]

  # Filter data for this facet
  this_df <- all_densa %>% filter(id == this_id, lag == this_data)

  # Add the layer
  p <- p +
    geom_point(
      data = this_df,
      aes(x = vm, y = lag_vm, color = density),
      size = 1
    ) +
    scale_color_viridis_b(option= "C") +
    new_scale_color()
}

preds = tibble(
  id = paste("Participant ", c("A", "B", "C", "D", "E"), sep = ""),
  probs = c(0.101, 0.008,  0.007, 0.0070, .006),
  lag = rep("Lag = 0.15s", 5)
)
# Add faceting after all layers
p2 = p + facet_grid(lag ~ id) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)", title = "Testing Data") +
  scale_x_continuous(limits=c(0,3)) +
  scale_y_continuous(limits=c(0,3)) +
  geom_label(data = preds,
             aes(x = 2, y = 2.6, label = paste0("p = ", probs)), size = 3)


p_final = p1 + p2 + plot_layout(widths = c(1, 6), axes = "collect")
png(here::here("manuscript", "figs_final", "fingerprint_predictions.png"), width = 10, height = 6, units = "in",
    res = 500)
p1 + p2 + plot_layout(widths = c(1, 6), axes = "collect")
dev.off()


####
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
ids = unique(sample_dat$id)

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

# purrr::walk(seq(1, 96, 5), \(x){
#   p = dens_df %>%
#     filter(id %in% ids[x:(x+4)]) %>%
#     ggplot(aes(x = vm, y = lag_vm, color = density)) +
#     geom_point() +
#     scale_color_viridis() +
#     facet_grid(data~id)
#   print(p)
# })
# dens_df %>%
#   filter(id %in% ids[20:25]) %>%
#   ggplot(aes(x = vm, y = lag_vm, color = density)) +
#   geom_point() +
#   scale_color_viridis() +
#   facet_grid(data~id)


p1 = dens_df %>%
  filter(id == 78898) %>%
  mutate(data = factor(data, levels = c("train", "test"), labels = c("Training data", "Testing data"))) %>%
  ggplot(aes(x = vm, y = lag_vm, color = density)) +
  geom_point(size = .85) +
  scale_color_viridis_b(name = "# points", option = "C") +
  facet_grid(data~.) +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)") +
  scale_x_continuous(limits=c(0,3), breaks=seq(0, 3, 0.5)) +
  scale_y_continuous(limits=c(0,3), breaks=seq(0, 3, 0.5)) +
  theme(legend.position = c(0.9, 0.7),
        panel.grid = element_blank())
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
  scale_y_continuous(limits=c(0, 2.5)) +
  labs(x = "Time (sec)", y = "Acceleration (g)") +
  theme(panel.grid = element_blank())

library(patchwork)

p_1 = p2 + p1 + plot_annotation(title = "Subject 1")

# other subs to investigate: 68561, 70844, 67407, 68022, 82860, 69972, 81349, 78898

p3 = dens_df %>%
  filter(id == 68561) %>%
  mutate(data = factor(data, levels = c("train", "test"), labels = c("Training data", "Testing data"))) %>%
  ggplot(aes(x = vm, y = lag_vm, color = density)) +
  geom_point(size = .85) +
  scale_color_viridis_b(option = "C", name = "# points") +
  facet_grid(data~.) +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)") +
  scale_x_continuous(limits=c(0,3), breaks=seq(0, 3, 0.5)) +
  scale_y_continuous(limits=c(0,3), breaks=seq(0, 3, 0.5)) +
  theme(legend.position = c(0.9, 0.7),
        panel.grid = element_blank())

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
  scale_y_continuous(limits=c(0,2.5)) +
  labs(x = "Time (sec)", y = "Acceleration (g)") +
  theme(panel.grid = element_blank())

library(patchwork)
p_2 = (p4 + p3) + plot_annotation(title = "Subject 2")
png(here::here("manuscript", "figs_final", "fingerprint_data.png"), width = 10, height = 8, units = "in",
    res = 500)
p_1 / p_2 + plot_annotation(tag_levels = "A")
dev.off()

(p2 + p1) / (p4 + p3)

