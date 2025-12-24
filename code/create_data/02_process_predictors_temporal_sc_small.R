library(tidyverse)

files = list.files(here::here("data", "lily", "data", "grid_cell_data_sc_temporal_small"), recursive = TRUE,
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

readr::write_csv(all_preds, here::here("data", "lily", "data", "all_grid_cells_sc_temporal_small.csv.gz"))


