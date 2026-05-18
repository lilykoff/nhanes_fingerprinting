library(tidyverse)

files = list.files(here::here("data", "lily", "data", "random_grid_cell_data"), recursive = TRUE,
                   full.names = TRUE)
length(files)
# nums = sub(".*fold\\_(.+).csv.gz.*", "\\1", files)
# nums = as.numeric(nums)
# nums %>% sort()
# seq(1:200)[!(seq(1:200) %in% nums)]


all_preds =
  files %>%
  map(readr::read_csv) %>%
  bind_rows()

readr::write_csv(all_preds, here::here("data", "lily", "data", "all_grid_cells_random.csv.gz"))
ids = unique(all_preds$id)


set.seed(123)
filename_df =
  tibble(id = sample(ids, size = length(ids), replace = FALSE))


write_rds(filename_df, here::here("data", "lily", "data", "fingerprint_folds_random.rds"))

rm(list = ls())


files = list.files(here::here("data", "lily", "data", "mixed_grid_cell_data"), recursive = TRUE,
                   full.names = TRUE)


all_preds =
  files %>%
  map(readr::read_csv) %>%
  bind_rows()

readr::write_csv(all_preds, here::here("data", "lily", "data", "all_grid_cells_mixed.csv.gz"))
ids = unique(all_preds$id)
set.seed(123)

filename_df =
  tibble(id = sample(ids, size = length(ids), replace = FALSE))


write_rds(filename_df, here::here("data", "lily", "data", "fingerprint_folds_mixed.rds"))
