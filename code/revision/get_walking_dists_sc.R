# investigate boutsof walking

library(tidyverse)
library(purrr)
library(tidymodels)
library(magrittr)
library(furrr)
library(future)
force = FALSE
tidymodels_prefer()
source(here::here("code", "R", "utils.R"))


fnames = readRDS(here::here("data", "raw", "all_filenames.rds"))


df = fnames

# df = fnames %>% filter(fold == 1)
options(dplyr.summarise.inform = FALSE)

get_density = function(subject, df = df){
  idf = df %>% filter(id == subject)
  x = try({
    walking_df =
      readr::read_csv(here::here("data", "lily", "data", "fingerprint_data_sc", idf$version, paste0(idf$id, ".csv.gz")))
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

    rm(walking_df)
    # key of those times
    segments_10 %>%
      select(rleid, day, n_seconds) %>%
      mutate(id = subject)
  })
  x
}

# plan(multisession)
# grid_data_list =
#   furrr::future_map(.x = df$id, .f = get_density, df = df)
grid_data_list =
  map(.x = df$id, .f = get_density, df = df)
grid_data_df =
  grid_data_list %>%
  keep(., ~ inherits(.x, "tbl_df")) %>%
  bind_rows()

readr::write_csv(grid_data_df,
                 here::here("data", "lily", "data", "walking_segments_sc.csv.gz"))


dists = read_csv(here::here("data", "walking_segments_sc.csv.gz"))

byday =
  dists %>%
  group_by(id, day) %>%
  summarize(min = sum(n_seconds) / 60)

byday %>%
  filter(min <= 60) %>%
  ggplot(aes(x = min)) +
  geom_histogram(color = "black") +
  # scale_x_continuous(breaks = seq(0, 600, 60), labels = seq(0, 600/60, 1)) +
  labs(x = "Daily Walking Hours", y = "Count")

byday %>%
  mutate(lt5 = min < 5) %>%
  group_by(id) %>%
  summarize(nowalk = all(lt5)) %>%
  filter(nowalk)

dists %>%
  filter(n_seconds <= 1000) %>%
  ggplot(aes(x = n_seconds)) +
  geom_histogram()

dists %>%
  group_by(id, day) %>%
  summarize(min = sum(n_seconds) / 60) %>%
  ggplot(aes(x = min)) +
  geom_histogram(color = "black", binwidth = 30) +
  scale_x_continuous(breaks = seq(0, 600, 60), labels = seq(0, 600/60, 1)) +
  labs(x = "Daily Walking Hours", y = "Count")
# plan(sequential)
