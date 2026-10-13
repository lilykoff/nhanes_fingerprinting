library(tidyverse)

x = read_csv(here::here("data", "lily", "data", "all_grid_cells_temporal_days.csv.gz"))

train = x |> filter(data == "train")
test = x |> filter(data == "test")

write_csv(train, here::here("data", "lily", "data", "all_grid_cells_temporal_days_train.csv.gz"))
write_csv(test, here::here("data", "lily", "data", "all_grid_cells_temporal_days_test.csv.gz"))

pred_dist = read_rds(here::here("data", "lily", "data", "train_test_temporal_days.rds"))


