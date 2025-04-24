# try fitting regression models on one fold (70 people)

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
    walking_df =
      readr::read_csv(here::here("data", "lily", "data", "fingerprint_data", idf$version, paste0(idf$id, ".csv.gz")))
    # get segments of consecutive walking with < 2 seconds between them that are at least 10 seconds long
    segments_10 = walking_df %>%
      select(second, day) %>%
      distinct() %>%
      mutate(timediff = as.numeric(difftime(second, dplyr::lag(second, n = 1), units = "secs")),
             ltwosec = (timediff <= 2)*1,
             rleid = data.table::rleid(ltwosec)) %>%
      filter(ltwosec == 1) %>%
      group_by(rleid, day) %>%
      summarize(n_seconds = n(),
                start = min(second),
                end = max(second)) %>%
      filter(n_seconds >= 10)

    # key of those times
    seconds_key =
      segments_10 %>%
      group_by(rleid, day) %>%
      tidyr::expand(second = seq(start, end, "sec"))

    df_small =
      walking_df %>%
      inner_join(seconds_key, by = c("second", "day"))

    # if there are at least 3 mins of data, sample 3 mins randomly (so that everyone has same amount of walking)
    if(nrow(df_small) >= 180) {
      set.seed(123)
      density =
        df_small %>%
        sample_n(size = 180, replace = FALSE) %>%
        mutate(id = subject) %>%
        select(id, starts_with("("), starts_with("["))
    } else {
      density = NULL
    }
  })
  x
}

outname = paste0("grid_data_fold_", ifold, ".csv.gz")
if(!file.exists(here::here(outname)) | force){
  grid_data_list =
    map(.x = df$id, .f = get_density, df = df)

  grid_data_df =
    grid_data_list %>%
    keep(., ~ inherits(.x, "tbl_df")) %>%
    bind_rows() %>%
    janitor::clean_names()

  outname = paste0("grid_data_fold_", ifold, ".csv.gz")
  readr::write_csv(grid_data_df, here::here("data", "lily", "data", "grid_cell_data", outname))

}
