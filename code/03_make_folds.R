library(tidyverse)
library(tidymodels)
df = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells.csv.gz"))

ids = unique(df$id)
set.seed(123)
filename_df =
  tibble(id = sample(ids, size = length(ids), replace = FALSE))

fold_vec = rep(seq(1:ceiling(nrow(filename_df))), each = 100)

filename_df$fold =
  fold_vec[1:nrow(filename_df)]

saveRDS(filename_df, here::here("data", "lily", "data", "fingerprint_folds.rds"))

