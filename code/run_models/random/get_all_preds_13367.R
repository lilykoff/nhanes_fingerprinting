## model boosting

library(tidyverse)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE
ifold = get_fold()


filenames = read_rds(here::here("data", "lily", "data", "fingerprint_folds.rds"))
filenames =
  filenames %>%
  mutate(fold = rep(1:200, each = 67)[1:13367])

if(!is.na(ifold)){
  filenames = filenames %>% filter(fold == ifold)
}


ids = filenames %>% pull(id) %>% as.character()
dat_nzv_test = read_rds(here::here("data", "lily", "data", "dat_nzv_test.rds")) %>%
  mutate(id = as.character(id))

true_sub_vec = dat_nzv_test$id
rm(dat_nzv_test)

if(!dir.exists(here::here("data", "lily", "data", "preds_13367"))){
  dir.create(here::here("data", "lily", "data", "preds_13367"), recursive = TRUE)
}
for(id in ids){
  print(id)
  file = file.path(here::here("data", "lily", "data", "fingerprint_res", "13367", paste(id, ".rds", sep = "")))

  outfile = file.path(here::here("data", "lily", "data", "preds_13367", paste0(id, ".rds")))

  if(file.exists(file) & (!file.exists(outfile) || force)) {
    x = try({
      id_tmp = sub(".*\\/(.+).rds.*", "\\1", file)
      tmp = read_rds(file) %>% as_tibble() %>%
        mutate(true_subject = true_sub_vec) %>%
        magrittr::set_colnames(c(id_tmp, "true_subject")) %>%
        group_by(true_subject) %>%
        mutate(sec = row_number()) %>%
        pivot_longer(
          cols = -c("true_subject", "sec"),
          names_to = "name",
          values_to = "pred"
        ) %>%
        mutate(model = as.character(sub(".*x", "", name))) %>%
        select(-name) %>%
        # now we have the prediction for each second for each model / true subject combo
        ungroup() %>%
        group_by(true_subject, model) %>%
        # get mean probability across seconds for each true subject / model combo
        summarize(mean_pred = mean(pred, na.rm = TRUE), .groups = "drop")
      rm(id_tmp)
      write_rds(tmp, outfile, compress = "xz")
    })
    rm(x)
  }

}

# test = read_rds(here::here("data", "lily", "data", "preds_13367", paste0(66080, ".rds")))

