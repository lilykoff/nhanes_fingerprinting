library(tidyverse)

files = list.files(here::here("data", "lily", "data", "grid_cell_data_big"), recursive = TRUE,
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

readr::write_csv(all_preds, here::here("data", "lily", "data", "all_grid_cells_long.csv.gz"))

ids = unique(all_preds$id)
set.seed(123)
filename_df =
  tibble(id = sample(ids, size = length(ids), replace = FALSE))


saveRDS(filename_df, here::here("data", "lily", "data", "fingerprint_folds_long.rds"))



# small sample

all_preds_small =
  files[1:10] %>%
  map(readr::read_csv) %>%
  bind_rows()

readr::write_csv(all_preds_small, here::here("data", "lily", "data", "sample_grid_cells_long.csv.gz"))

