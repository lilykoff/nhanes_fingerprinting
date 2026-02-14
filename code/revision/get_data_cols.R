## how many predictors were included after variable screening
library(tidyverse)

files = list.files(here::here("data", "lily", "data"), full.names = TRUE)
files_nzv = files[grepl("nzv", basename(files))]
files_nzv = files_nzv[grepl("train", basename(files_nzv))]
file=files_nzv[1]
for(file in files_nzv){
  x = read_rds(file)
  print(basename(file))
  print(ncol(x) - 1)
}

file_amts = map_dfr(.x = files_nzv,
                    .f = function(file){
                      x = read_rds(file)
                      r = tibble(f = basename(file),
                             cols = ncol(x)-1)
                      rm(x)
                      r
                    })

write_rds(file_amts, here::here("data", "lily", "data", "training_df_size.rds"))
file_amts %>%
  arrange(desc(cols))

file_amts %>%
  arrange(cols)

mean(file_amts$cols)
median(file_amts$cols)

## do PCA
all_cells = read_csv(here::here("data", "lily", "data","sample_grid_cells.csv.gz"))

library(tidymodels)
set.seed(123)
initialsplit = initial_split(all_cells, prop = 3/4, strata = id)

train_df = training(initialsplit)
train_df ## 432 cols

# norm_df = train_df %>% select(-id) %>%
#   scale()

pca_res = princomp(train_df %>% select(-id))
summary(pca_res)
