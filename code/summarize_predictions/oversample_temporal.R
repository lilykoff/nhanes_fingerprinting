library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE
source(here::here("data", "lily", "code", "summary_fns.R"))
if (!dir.exists(here::here("data", "lily", "data", "fingerprint_prediction_results"))) {
  dir.create(here::here(
    "data",
    "lily",
    "data",
    "fingerprint_prediction_results"
  ))
}


###########
## small sample size
dirs = c("100", "500" ,"1000")
for (dir in dirs) {
  dirnum = as.numeric(dir)
  outfile = here::here("data", "lily", "data","fingerprint_prediction_results",
                       paste0("prediction_res_temporal2_", paste0(dir, "over", ".rds")))

  if(!file.exists(outfile) || force) {
    all_preds = list.files(here::here("data", "lily", "data", "fingerprint_res_temporal2", paste0(dir, "over")),
                           recursive = TRUE,
                           full.names = TRUE,
                           pattern = "rds")
    x = try({
      summary = map_dfr(.x = all_preds,
                        .f = function(file){
                          x = readRDS(file)

                          fold = sub(".*fold\\_(.+)\\.rds.*", "\\1", basename(file))
                          n_target = as.numeric(sub(".*res_temporal2\\/(.+)over.*", "\\1", file))

                          res =
                            x %>%
                            group_by(factor) %>%
                            group_modify(~ get_summarized_predictions(.x, rank = TRUE) %>%
                                           ungroup() %>%
                                           mutate(rank1pct = (rank <= n_target * 0.01) * 1,
                                                  rank5pct = (rank <= n_target * 0.05) * 1) %>%
                                           select(-rank) %>%
                                           summarize(across(contains("rank"), sum),
                                                     n = n()) %>%
                                           mutate(fold = fold,
                                                  n_tar = n_target) %>%
                                           filter(n == n_tar))

                          rm(x); rm(fold); rm(n_target)
                          res
                        })
      write_rds(summary, outfile, compress = "xz")
    })
    rm(x)
  }}
