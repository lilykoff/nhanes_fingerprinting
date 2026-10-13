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
    set.seed(21231)
    keep =
      sample %>%
      group_by(id) |> 
      arrange(day, .by_group = TRUE) |> 
      filter(n >= 60) |> 
      mutate(n_days = n()) |> 
      filter(n_days >= 3) |> 
      ungroup() |> 
      rowwise() |> 
      mutate(start = if_else(n > 60, sample(1:(n-60), 1), 1),
             end = if_else(n > 60, start + 60 - 1, n)) |> 
      ungroup() 

             
    
    preds = file %>%
      filter(id %in% keep$id) %>%
      group_by(id, day, data) %>%
      mutate(rn = row_number()) %>%
      left_join(keep, by = c("id", "data", "day")) %>%
      filter(rn >= start & rn <= end) %>%
      ungroup() %>%
      select(id, data, second, starts_with("x"))
    rm(file)
    preds
  }) %>%
  bind_rows()


readr::write_csv(all_preds, here::here("data", "lily", "data", "all_grid_cells_temporal_days_thresh.csv.gz"))

train = all_preds |> filter(data == "train")
readr::write_csv(train, here::here("data", "lily", "data", "all_grid_cells_temporal_days_thresh_train.csv.gz"))

test = all_preds |> filter(data == "test")
readr::write_csv(test, here::here("data", "lily", "data", "all_grid_cells_temporal_days_thresh_test.csv.gz"))


# x = read_csv(here::here("data", "lily", "data", "all_grid_cells_temporal_days.csv.gz"))

