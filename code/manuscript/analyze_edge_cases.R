library(tidyverse)
key_df = read_rds(here::here("results", "pred_key_df.rds"))
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}
library(patchwork)

pred_dfs = list.files(here::here("results", "pred_dfs"), recursive = TRUE)


pdf(here::here("results/test_plots.pdf"))
for(row in 1:nrow(key_df)){
  temp = key_df %>% slice(row)
  idx = temp$preds
  t = temp$type
  wdf = read_rds(here::here("results", "pred_dfs", paste0(idx, "_raw_vm.rds"))) %>%
    mutate(train = if_else(train == TRUE, "train", "test"))
  density = read_rds(here::here("results", "pred_dfs", paste0(idx, "_gcells.rds"))) %>%
    mutate(train = if_else(train == TRUE, "train", "test"))

  # plot fingerprint
  dens_df =
    wdf %>%
    group_by(train, second_id) %>%
    mutate(lag_vm = lag(vm, n = 12)) %>%
    ungroup() %>%
    drop_na()
  dens_df =
    dens_df %>%
    mutate(density = get_density(vm, lag_vm, n = 80))

  p = dens_df %>%
    ggplot(aes(x = vm, y = lag_vm, color = density)) +
    geom_point() +
    facet_wrap(.~train) +
    viridis::scale_color_viridis(option = "B") +
    labs(title = paste0("Subject ID: ", idx),
         subtitle = paste0(t)) +
    scale_y_continuous(limits=c(0, 3)) +
    scale_x_continuous(limits=c(0, 3))



  p2 = wdf %>%
    # filter(second_id == second_id[100]) %>%
    group_by(train) %>%
    mutate(rn = row_number()) %>%
    slice(1:(20 * 80)) %>%
    ungroup() %>%
    ggplot(aes(x = rn, y = vm)) +
    geom_line(linewidth = 0.5) +
    facet_wrap(.~train, scales = "free_x") +
    scale_y_continuous(limits=c(0, 3))
  print(p / p2)
}

dev.off()

### make a figure

## viridis option C
idx = 82367

wdf = read_rds(here::here("results", "pred_dfs", paste0(idx, "_raw_vm.rds"))) %>%
  mutate(train = if_else(train == TRUE, "train", "test"))

# plot fingerprint
theme_set(theme_light(base_size = 14))

make_plot = function(id){
  wdf = read_rds(here::here("results", "pred_dfs", paste0(id, "_raw_vm.rds"))) %>%
    mutate(train = if_else(train == TRUE, "train", "test"))

  dens_df =
    map_dfr(.x = c(12, 24, 36),
            .f = function(df, lg){
              df %>%
                select(second_id, train, vm) %>%
                group_by(train, second_id) %>%
                mutate(lag_vm = lag(vm, n = lg)) %>%
                ungroup() %>%
                drop_na() %>%
                mutate(lag = lg)
            },
            df = wdf)
  dens_df =
    dens_df %>%
    # group_by(lag, second_id, train) %>%
    group_by(lag, train) %>%
    mutate(density = get_density(vm, lag_vm, n = 80)) %>%
    ungroup()


  p = dens_df %>%
    mutate(train = factor(train, levels = c("train", "test"), labels = c("Training data","Testing data")),
           lag = case_when(lag == 12 ~ "Lag = 0.15s",
                           lag == 24 ~ "Lag = 0.30s",
                           .default = "Lag = 0.45s")) %>%
    ggplot(aes(x = vm, y = lag_vm, color = density)) +
    geom_point(size= 1, alpha= .9) +
    facet_grid(lag~train) +
    viridis::scale_color_viridis(option = "C") +
    labs(title = paste0("Participant ", id)) +
    scale_y_continuous(limits=c(0, 3)) +
    scale_x_continuous(limits=c(0, 3)) +
    theme(legend.position = "none", panel.grid = element_blank()) +
    labs(x = "Acceleration (g)", y = "Lag Acceleration (g)")

    return(p)
}

plot_list = map(.x = c(82367, 78466, 66810, 79001, 79235), .f = make_plot)

library(patchwork)

p1 = plot_list[[1]] + plot_list[[2]] + plot_list[[3]]  +
  plot_layout(nrow = 1, axes = 'collect') + plot_annotation(title = "A) Well-Predicted Participants")

plot_list2 = map(.x = c(69878, 72735, 65584, 73266, 72232), .f = make_plot)

p2 = plot_list2[[1]] + plot_list2[[2]] + plot_list2[[3]]  +
  plot_layout(nrow = 1, axes = 'collect') + plot_annotation(title = "B) Poorly-Predicted Participants")


png(here::here("manuscript", "figs_final", "prediction_summary.png"), width = 15, height = 10, res = 350, units = "in")
cowplot::plot_grid(p1, p2, ncol = 1)
dev.off()


res = make_plot(82367)
print(res)

dens_df_12 =
  wdf %>%
  group_by(train, second_id) %>%
  mutate(lag_vm = lag(vm, n = 12)) %>%
  ungroup() %>%
  drop_na()


dens_df =
  dens_df %>%
  mutate(density = get_density(vm, lag_vm, n = 80))

p = dens_df %>%
  ggplot(aes(x = vm, y = lag_vm, color = density)) +
  geom_point() +
  facet_wrap(.~train) +
  viridis::scale_color_viridis(option = "B") +
  labs(title = paste0("Subject ID: ", idx),
       subtitle = paste0(t)) +
  scale_y_continuous(limits=c(0, 3)) +
  scale_x_continuous(limits=c(0, 3))



