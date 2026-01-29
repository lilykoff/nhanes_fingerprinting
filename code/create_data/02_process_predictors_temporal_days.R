library(tidyverse)

files = list.files(here::here("data", "lily", "data", "grid_cell_data_temporal_days"), recursive = TRUE,
                   full.names = TRUE)
length(files)
# nums = sub(".*fold\\_(.+).csv.gz.*", "\\1", files)
# nums = as.numeric(nums)
# nums %>% sort()
# seq(1:200)[!(seq(1:200) %in% nums)]


all_preds =
  files %>%
  map(., .f = function(x){
    file = readr::read_csv(x)
    keep = file %>% group_by(id, data) %>%
      count() %>%
      pivot_wider(values_from = n, names_from =data) %>%
      filter(train >= 180*.75, test >= 180*.25) %>%
      pull(id)
    file %>%
      filter(id %in% keep)
  }) %>%
  bind_rows()

readr::write_csv(all_preds, here::here("data", "lily", "data", "all_grid_cells_temporal_days.csv.gz"))

ids = unique(all_preds$id)
set.seed(123)
filename_df =
  tibble(id = sample(ids, size = length(ids), replace = FALSE))

write_rds(filename_df, here::here("data", "lily", "data", "fingerprint_folds_temporal_days.rds"))



