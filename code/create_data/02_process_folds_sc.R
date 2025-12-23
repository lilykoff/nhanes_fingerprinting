library(tidyverse)

files = list.files(here::here("data", "lily", "data", "grid_cell_data_temporal_sc"), recursive = TRUE,
                   full.names = TRUE)
length(files)
# nums = sub(".*fold\\_(.+).csv.gz.*", "\\1", files)
# nums = as.numeric(nums)
# nums %>% sort()
# seq(1:200)[!(seq(1:200) %in% nums)]


sub_df = map(files, function(f){
  x = read_csv(f)
  ids = unique(x$id)
  tibble(id = ids,
         fold = sub(".*fold\\_(.+).csv.gz.*", "\\1", f))
})

all_subs = bind_rows(sub_df)
set.seed(123)
all_subs_random = all_subs %>% slice_sample(n = nrow(all_subs),
                                            replace = FALSE)


write_rds(all_subs_random, here::here("data", "lily", "data", "fingerprint_folds_temporal_sc.rds"))


# library(tidyverse)

files = list.files(here::here("data", "lily", "data", "grid_cell_data_sc"), recursive = TRUE,
                   full.names = TRUE)
length(files)
# nums = sub(".*fold\\_(.+).csv.gz.*", "\\1", files)
# nums = as.numeric(nums)
# nums %>% sort()
# seq(1:200)[!(seq(1:200) %in% nums)]


sub_df = map(files, function(f){
  x = read_csv(f)
  ids = unique(x$id)
  tibble(id = ids,
         fold = sub(".*fold\\_(.+).csv.gz.*", "\\1", f))
})

all_subs = bind_rows(sub_df)
set.seed(123)
all_subs_random = all_subs %>% slice_sample(n = nrow(all_subs),
                                            replace = FALSE)

write_rds(all_subs_random, here::here("data", "lily", "data", "fingerprint_folds_sc.rds"))

filenames2 = read_rds(here::here("data", "lily", "data", "fingerprint_folds_temporal_sc.rds"))
filenames = read_rds(here::here("data", "lily", "data", "fingerprint_folds_sc.rds"))

reg_ids= unique(filenames$id)
temp_ids= unique(filenames2$id)

together = intersect(reg_ids, temp_ids)

filenames_all =
  filenames %>%
  inner_join(filenames2) %>%
  rename(fold_orig = fold)

write_rds(filenames_all, here::here("data", "lily", "data", "folds_sc.rds"))
