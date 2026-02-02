library(tidyverse)

files = list.files(here::here("data", "lily", "data", "grid_cell_data_temporal_days"), recursive = TRUE,
                   full.names = TRUE)
length(files)
# nums = sub(".*fold\\_(.+).csv.gz.*", "\\1", files)
# nums = as.numeric(nums)
# nums %>% sort()
# seq(1:200)[!(seq(1:200) %in% nums)]

if(!file.exists(here::here("data", "lily", "data", "train_test_temporal_days.rds"))) {
  pred_dist =
    files %>%
    map(., .f = function(x){
      readr::read_csv(x) %>%
        group_by(id, data, day) %>%
        count()
    }) %>%
    bind_rows()
  write_rds(pred_dist, here::here("data", "lily", "data", "train_test_temporal_days.rds"))
}

all_preds =
  files %>%
  map(., .f = function(x){
    file = readr::read_csv(x)
    sample = file %>%
      group_by(id, data, day) %>%
      count()

    keep =
      sample %>%
      ungroup() %>%
      select(id, data, n) %>%
      group_by(data, id) %>%
      summarize(n = sum(n), .groups = "drop") %>%
      pivot_wider(values_from = n, names_from = data) %>%
      filter(test >= 60 & train >= 180) %>%
      pull(id)
    # if there's more than 10 min on a day, take random sample of 10 min

    start_inds =
      sample %>%
      filter(id %in% keep) %>%
      mutate(start = if_else(n > (10 * 60), sample(1:(n - 600), 1), 1),
             end = if_else(n > (10 * 60), start + 600 - 1, n))

    preds = file %>%
      filter(id %in% keep) %>%
      group_by(id, day, data) %>%
      mutate(rn = row_number()) %>%
      left_join(start_inds, by = c("id", "data", "day")) %>%
      filter(rn >= start & rn <= end) %>%
      ungroup() %>%
      select(id, data, starts_with("x"))
    rm(file)
    preds
  }) %>%
  bind_rows()


readr::write_csv(all_preds, here::here("data", "lily", "data", "all_grid_cells_temporal_days.csv.gz"))

ids = unique(all_preds$id)
set.seed(123)
filename_df =
  tibble(id = sample(ids, size = length(ids), replace = FALSE))

write_rds(filename_df, here::here("data", "lily", "data", "fingerprint_folds_temporal_days.rds"))



