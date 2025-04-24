# aggregate fingerprints

library(readr)
library(ggplot2)
library(tidyverse)
library(tidymodels)
library(future)
library(furrr)
options(mc.cores = availableCores())

fingerprint_g = list.files(here::here("data", "lily", "data", "fingerprint_data", "pax_g"),
                               recursive = TRUE, full.names = TRUE)
fingerprint_h = list.files(here::here("data", "lily", "data", "fingerprint_data", "pax_h"),
                           recursive = TRUE, full.names = TRUE)
get_summary = function(fname){
  id = sub(".*\\/(.+).csv.gz.*", "\\1", fname)
  version = sub(".*data\\/(.+)\\/.*", "\\1", fname)
  df = read_csv(fname)

  day_summary =
    df %>%
    group_by(day) %>%
    summarize(n_seconds = n(),
              across(-c(second_id, second), ~sum(.x)))
  rm(df)
  day_summary %>%
    mutate(id = id,
           version = version)
}
plan(cluster)
res_g = furrr::future_map_dfr(.f = get_summary, .x = fingerprint_g)

write_csv(res_g,
          here::here("data", "lily", "data", "summarized_fingerprints_pax_g.csv.gz"))

rm(res_g)

res_h = furrr::future_map_dfr(.f = get_summary, .x = fingerprint_h)

write_csv(res_h,
          here::here("data", "lily", "data", "summarized_fingerprints_pax_h.csv.gz"))
