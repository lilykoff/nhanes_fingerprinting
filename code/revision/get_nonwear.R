library(tidyverse)
nhanes_1440_PAXPREDM = readRDS(here::here("../", "nhanes_steps_mortality/data/accelerometry/minute_level/nhanes_1440_PAXPREDM.rds"))
# 3 is nonwear

min_mat =
  nhanes_1440_PAXPREDM %>%
  select(starts_with("min")) %>%
  as.matrix()

nonwear = min_mat == 3
nonwear_day = rowSums(nonwear, na.rm = TRUE)


nonwear_df =
  nhanes_1440_PAXPREDM %>%
  select(-starts_with("min")) %>%
  bind_cols(nonwear_min = nonwear_day)

total_nonwear =
  nonwear_df %>%
  group_by(SEQN) %>%
  summarize(nw = sum(nonwear_min))


preds = read_rds(here::here("results", "subj_level_preds.rds")) %>%
  mutate(true_subject = as.character(true_subject))

joined = total_nonwear %>%
  left_join(preds, by = c("SEQN" = "true_subject"))
joined

joined %>%
  drop_na() %>%
  ggplot(aes(x = nw / 60, y = mean_pred, color = factor(rank1))) +
  geom_point(alpha = 0.1, size = 0.75) +
  scale_x_continuous(breaks=seq(0,24*8, 24)) +
  labs(x = "Total nonwear hours", y = "Probability from correct model",
       color = "Participant correctly predicted") +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))  +
  scale_color_brewer(palette = "Dark2", direction = -1)


deciles = quantile(joined$nw, seq(0, 1, 0.1), na.rm = TRUE)
joined_summary =
  joined %>%
  drop_na() %>%
  mutate(nw_bin = cut(nw, breaks = deciles, include.lowest = TRUE,
         labels = paste("Decile", seq(1:10)))) %>%
  group_by(nw_bin) %>%
  summarize(acc = mean(rank1),
            sd = sd(rank1),
            se = sd / sqrt(n()),
            n = n(),
            .groups = "drop")

theme_set(theme_light(base_size = 14))
png(here::here("manuscript", "nonwear_accuracy.png"), height = 8, width = 12, units ="in", res= 350)
joined_summary %>%
  ggplot(aes(x = nw_bin)) +
  geom_point(aes(y = acc), size = 2) +
  geom_errorbar(aes(ymin = acc - 1.96 * se, ymax = acc + 1.96 * se, x = nw_bin), width = 0.2, linewidth = 1.1) +
  scale_y_continuous(limits = c(0.5, 1)) +
  labs(x = "Nonwear hours (deciles)", y = "Mean Rank-1 Accuracy", title = "Accuracy vs. nonwear in subgroups of n=100")
  # theme_light()
dev.off()


