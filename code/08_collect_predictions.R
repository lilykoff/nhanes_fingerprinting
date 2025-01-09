library(tidyverse)

files = list.files(here::here("data", "lily", "data",
                              "fingerprint_prediction_results"),
                   full.names = TRUE)
files = files[!grepl("over", files)]

ovfiles = list.files(here::here("data", "lily", "data",
                              "fingerprint_prediction_results"),
                   full.names = TRUE,
                   pattern = "over")

testpctfiles = list.files(here::here("data", "lily", "data",
                                "fingerprint_prediction_results"),
                     full.names = TRUE,
                     pattern = "testpct")
files = files[!(grepl("over", files) | grepl("testpct", files))]

x = read_rds(files[1])

all =
  map_dfr(.x = files,
          .f = function(x){
      read_rds(x) %>%
      mutate(across(contains("rank"), ~.x / n * 100)) %>%
      summarize(n_sub = first(n),
                n_folds = n(),
                across(contains("rank"),
                       list(mean = ~mean(.x, na.rm = TRUE),
                            min = ~min(.x, na.rm = TRUE),
                            max = ~max(.x, na.rm = TRUE),
                            median = ~median(.x, na.rm = TRUE)))) %>%
      mutate(name = sub(".*res\\_(.+).rds.*", "\\1", x))
  })

saveRDS(all, here::here("data", "lily", "data", "all_fprint_res.rds"))
# all %>%
#   select(name, n_sub, contains("mean"), everything()) %>%
#   arrange(n_sub)
# y = read_rds(files[4])

all_ov =
  map_dfr(.x = ovfiles,
          .f = function(x){
            read_rds(x) %>%
              group_by(factor) %>%
              mutate(across(contains("rank"), ~.x / n * 100)) %>%
              summarize(n_sub = first(n),
                        n_folds = n(),
                        across(contains("rank"),
                               list(mean = ~mean(.x, na.rm = TRUE),
                                    min = ~min(.x, na.rm = TRUE),
                                    max = ~max(.x, na.rm = TRUE),
                                    median = ~median(.x, na.rm = TRUE)))) %>%
              mutate(name = sub(".*res\\_(.+).rds.*", "\\1", x)) %>%
              ungroup()
          })

saveRDS(all_ov, here::here("data", "lily", "data", "all_fprint_res_ov.rds"))


all_ov %>%
  select(name, factor, n_sub, contains("mean"), everything()) %>%
  group_by(n_sub) %>%
  arrange(desc(rank1_mean), .by_group = TRUE) %>%
  print(n=100)

all_tp =
  map_dfr(.x = testpctfiles,
          .f = function(x){
            read_rds(x) %>%
              group_by(n_test, n_train) %>%
              mutate(across(contains("rank"), ~.x / n_test * 100)) %>%
              summarize(n_folds = n(),
                        across(contains("rank"),
                               list(mean = ~mean(.x, na.rm = TRUE),
                                    min = ~min(.x, na.rm = TRUE),
                                    max = ~max(.x, na.rm = TRUE),
                                    median = ~median(.x, na.rm = TRUE))), .groups = "drop") %>%
              mutate(name = sub(".*res\\_(.+).rds.*", "\\1", x))
          })

saveRDS(all_tp, here::here("data", "lily", "data", "all_fprint_res_testpct.rds"))
