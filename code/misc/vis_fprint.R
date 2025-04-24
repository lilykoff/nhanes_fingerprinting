library(tidyverse)
## generate some fingerprint visualization
get_summarized_predictions = function(predictions, rank = FALSE, exp = FALSE) {
  # predictions is tibble
  # column j is the predictions from fitting the model where the "true subject" is subject j
  # each row is prediction for a given second
  # true subject is the final column

  if (rank) {
    # will return data frame with columns true subject, model, mean prediction, rank of correct prediction,
    # and rank1, rank5 which indicate whether predicted subject was in top 1 or top 5
    predictions %>%
      group_by(true_subject) %>%
      mutate(sec = row_number()) %>%
      pivot_longer(cols = -c("true_subject", "sec"), names_to = "name", values_to = "pred") %>%
      mutate(model = as.numeric(sub(".*x", "", name))) %>%
      select(-name) %>%
      # now we have the prediction for each second for each model / true subject combo
      mutate(pred = case_when(exp ~ exp(pred),
                              .default = pred)) %>% # exponentiate based on exp argument
      ungroup() %>%
      group_by(true_subject, model) %>%
      # get mean probability across seconds for each true subject / model combo
      summarize(mean_pred = mean(pred, na.rm = TRUE), .groups = "drop") %>%
      group_by(true_subject) %>%
      mutate(
        rank = rank(-mean_pred)
      ) %>% # get the rank for each prediction
      ungroup() %>%
      filter(model == true_subject) %>% # only keep the correct combos and get ranks
      mutate(
        rank1 = if_else(rank == 1, 1, 0),
        rank5 = if_else(rank <= 5, 1, 0)
      )
  } else {
    predictions %>%
      group_by(true_subject) %>%
      mutate(sec = row_number()) %>%
      pivot_longer(cols = -c("true_subject", "sec"), names_to = "name", values_to = "pred") %>%
      mutate(model = as.numeric(sub(".*x", "", name))) %>%
      select(-name) %>%
      # now we have the prediction for each second for each model / true subject combo
      mutate(pred = case_when(exp ~ exp(pred),
                              .default = pred)) %>% # exponentiate based on exp argument
      ungroup() %>%
      group_by(true_subject, model) %>%
      summarize(mean_pred = mean(pred, na.rm = TRUE), .groups = "drop") %>%
      group_by(true_subject) %>%
      summarize(
        maxprob = first(max(mean_pred)),
        predicted_sub = first(model[mean_pred == maxprob]),
        probsubj = first(mean_pred[true_subject == model])
      ) %>%
      mutate(correct = if_else(as.numeric(predicted_sub) == true_subject, 1, 0)) %>%
      ungroup()
  }
}

result_folds  = read_rds(here::here("data", "all_fprint_folds.rds"))
result_folds %>%
  mutate(temporal = if_else(grepl("temporal", name), "Temporal", "Random"),
         type = case_when(
           sub(".*\\d", "", name) == "xgb" ~ "XGBoost",
           sub(".*\\d", "", name) == "long" ~ "Long",
           sub(".*\\d", "", name) == "lasso" ~ "Lasso",
           sub(".*\\d", "", name) == "rf" ~ "Random Forest",
           sub(".*\\d", "", name) == "fnl" ~ "Linear SoFR",
           sub(".*\\d", "", name) == "nlfnl" ~ "Nonlinear SoFR",
           .default = "Logistic")) %>%
  filter(n == 100 & type != "Long") %>%
  group_by(fold, temporal) %>%
  summarize(n = n(),
            across(rank1, mean)) %>%
  arrange(desc(rank1)) %>%
  filter(temporal =="Temporal")
## folds 48, 105 - random
## folds 69, 18 - temporal

f1 = read_rds(here::here("data", "lily", "data", "fingerprint_folds.rds"))
f2 = read_rds(here::here("data", "lily", "data", "fingerprint_folds_temporal.rds"))


ifold = 48
size = 100



if (!is.na(ifold) & !is.na(size)) {
  x = ceiling(nrow(f1)/size)
  f1 = f1 %>%
    mutate(fold = rep(1:x, each = size)[1:nrow(f1)])
  fdf = f1 %>%
    filter(fold == ifold)
}

preds = read_rds(here::here("data", "lily", "data", "fingerprint_res", "100", "fold_48.rds"))
preds = read_rds(here::here("data", "lily", "data", "fingerprint_res_temporal", "100", "fold_48.rds"))

res = get_summarized_predictions(preds, rank = TRUE)
res %>%
  filter(rank1 == 1) %>%
  arrange(desc(mean_pred))

# 73773 66045

res %>%
  filter(rank1 == 1) %>%
  arrange(mean_pred)
#
# 1        69165 69165     0.346     1     1     1
# 2        81886 81886     0.318     1     1     1
# 3        66321 66321     0.192     1     1     1
# 4        75256 75256     0.187     1     1     1

res %>%
  filter(rank1 == 0 & rank5 == 0) %>%
  arrange(desc(mean_pred))

# true_subject model mean_pred  rank rank1 rank5
# <dbl> <dbl>     <dbl> <dbl> <dbl> <dbl>
# 1        74833 74833    0.0212     6     0     0
# 2        65182 65182    0.0197     7     0     0
# 3        74364 74364    0.0175     7     0     0

# need raw acceleration data

ids = c("73773", "66045", "75135", "68265")
df = readRDS(here::here("data", "raw", "all_filenames.rds"))

fprints =
  map(.x = ids,
      .f = function(id_tmp){
        idf = df %>% filter(id == id_tmp)
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

        day_df =
          df_small %>%
          group_by(day) %>%
          count() %>%
          ungroup()

        day_train =
          day_df %>%
          filter(n >= 120) %>%
          slice(1) %>%
          pull(day)

        day_test =
          day_df %>%
          filter(n >= 45 & day > day_train) %>%
          slice_sample(n = 1)  %>%
          pull(day)


        test =
          df_small %>%
          filter(day == day_test) %>%
          mutate(id = id_tmp, data = "test")

        test_start = sample(x = 1:(nrow(test)-44), size = 1)

        test = test %>% slice(test_start:(test_start + 44)) %>%
          select(second_id, id, second, day)

        train =
          df_small %>%
          filter(day == day_train) %>%
          mutate(id = id_tmp, data = "test")

        train_start = sample(x = 1:(nrow(train)-119), size = 1)

        train = train %>% slice(train_start:(train_start + 119)) %>%
          select(second_id, id, second, day)


        rm(walking_df)
        # get raw data
        adept_df =
          readr::read_csv(here::here("data", "lily", "data", "adept_walking_dfs", idf$version, paste0(idf$id, ".csv.gz")))

        adept_df =
          adept_df %>%
          filter(second_id %in% unique(c(train$second_id, test$second_id))) %>%
          mutate(category = if_else(second_id %in% train$second_id, "train", "test")) %>%
          select(HEADER_TIMESTAMP, vm, second_id, category) %>%
          mutate(id = id_tmp)

        adept_df

      }) %>%
  list_rbind()
write_rds(fprints, here::here("data", "lily", "data", "fprint_test.rds"),
          compress = "xz")


test = read_rds(here::here("data", "fprint_test.rds"))


get_density = function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}



plotdf =
  test %>%
  group_by(second_id, id, category) %>%
  mutate(lagvm = lag(vm, n = 12)) %>%
  drop_na() %>%
  filter(n() > 10) %>%
  group_modify(~ .x %>% mutate(dens = get_density(.x$vm, .x$lagvm, n = 68))) %>%
  ungroup()

plotdf %>%
  ggplot(aes(x = vm, y = lagvm, color = dens))+
  geom_point(size = .5) +
  scale_color_viridis() +
  scale_x_continuous(limits=c(0,3))+
  scale_y_continuous(limits=c(0,3))+
  facet_grid(id ~ category)

plotdf =
  test %>%
  group_by(second_id, id, category) %>%
  mutate(lagvm = lag(vm, n = 24)) %>%
  drop_na() %>%
  filter(n() > 10) %>%
  group_modify(~ .x %>% mutate(dens = get_density(.x$vm, .x$lagvm, n = 56))) %>%
  ungroup()

plotdf %>%
  ggplot(aes(x = vm, y = lagvm, color = dens))+
  geom_point() +
  scale_color_viridis() +
  scale_x_continuous(limits=c(0,3))+
  scale_y_continuous(limits=c(0,3))+
  facet_grid(id ~ category)

plotdf =
  test %>%
  group_by(second_id, id, category) %>%
  mutate(lagvm = lag(vm, n = 36)) %>%
  drop_na() %>%
  filter(n() > 10) %>%
  group_modify(~ .x %>% mutate(dens = get_density(.x$vm, .x$lagvm, n = 44))) %>%
  ungroup()

plotdf %>%
  ggplot(aes(x = vm, y = lagvm, color = dens))+
  geom_point() +
  scale_color_viridis() +
  scale_x_continuous(limits=c(0,3))+
  scale_y_continuous(limits=c(0,3))+
  facet_grid(id ~ category)
