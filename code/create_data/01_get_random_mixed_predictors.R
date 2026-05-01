library(tidyverse)
library(purrr)
library(tidymodels)
library(magrittr)
force = FALSE
tidymodels_prefer()
source(here::here("code", "R", "utils.R"))
ifold = get_fold()

fnames = readRDS(here::here("data", "raw", "all_filenames.rds"))
if(!is.null(ifold)){
  df = fnames %>% filter(fold == ifold)
}



# df = fnames %>% filter(fold == 1)
options(dplyr.summarise.inform = FALSE)

get_density = function(subject, df = df){
  idf = df %>% filter(id == subject)
  x = try({
    readr::read_csv(here::here("data", "lily", "data", "random_fingerprint_data", idf$version, paste0(idf$id, ".csv.gz"))) %>%
        mutate(id = subject) %>%
        select(id, starts_with("("), starts_with("["))
  })
  x
}

if (!dir.exists(here::here("data", "lily", "data", "random_grid_cell_data"))) {
  dir.create(here::here("data", "lily", "data", "random_grid_cell_data"), recursive = TRUE)
}

if (!dir.exists(here::here("data", "lily", "data", "mixed_grid_cell_data"))) {
  dir.create(here::here("data", "lily", "data", "mixed_grid_cell_data"), recursive = TRUE)
}
outname = paste0("grid_data_fold_", ifold, ".csv.gz")
if(!file.exists(here::here(outname)) || force){
  grid_data_list =
    map(.x = df$id, .f = get_density, df = df)

  grid_data_df =
    grid_data_list %>%
    keep(., ~ inherits(.x, "tbl_df")) %>%
    bind_rows() %>%
    janitor::clean_names()

  outname = paste0("grid_data_fold_", ifold, ".csv.gz")
  readr::write_csv(grid_data_df, here::here("data", "lily", "data", "random_grid_cell_data", outname))

  ## make mixed df

  walking_df = read_csv(here::here("data", "lily", "data", "grid_cell_data", outname))

  walking_df_small =
    walking_df %>%
    group_by(id) %>%
    slice_sample(n = 90) %>%
    ungroup() %>%
    mutate(id = as.character(id))


  random_df_small =
    grid_data_df %>%
    filter(id %in% walking_df$id) %>%
    group_by(id) %>%
    slice_sample(n = 90) %>%
    ungroup()

  mixed =
    walking_df_small %>%
    bind_rows(random_df_small) %>%
    arrange(id)

  readr::write_csv(mixed, here::here("data", "lily", "data", "mixed_grid_cell_data", outname))

}
