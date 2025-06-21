library(tidyverse)
library(tidymodels)
library(future)
library(furrr)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE
n_cores = parallel::detectCores()
n_cores_inner = floor(n_cores / 2)
n_cores_outer = 2
# each one takes about 10 min and 20G (30 to be safe)
# 1024 gb per user = 34 jobs at once
# 13367 total = 92 days / 34 = ~ 3 days

get_input = function(default = NA_real_){
  input = as.numeric(Sys.getenv("INPUT", unset = as.character(default)))
  print(paste0("input is: ", input))
  input
}

rf_spec = rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 1000
) %>%
  set_engine("ranger", num.threads = 1) %>%
  set_mode("classification")


fit_model = function(subject, train, test) {
  full_data = bind_rows(train, test)  # Combine the datasets
  full_data = full_data %>%
    mutate(label = factor(if_else(id == subject, 1, 0)))

  train_indices = 1:nrow(train)           # Indices for the training set
  test_indices = (nrow(train) + 1):nrow(full_data)
  # Manually create the initial_split object
  initialsplit = make_splits(
    x = list(analysis = train_indices, assessment = test_indices),
    data = full_data
  )
  train = training(initialsplit)

  # nzv_trans =
  #   recipe(id ~ ., data = train) %>%
  #   step_nzv(all_predictors())
  #
  # nzv_estimates = prep(nzv_trans)
  #
  # nzv = colnames(juice(nzv_estimates))
  # train = train %>% dplyr::select(label, id, all_of(nzv))

  cv_folds = train %>%
    rsample::vfold_cv(v = 5, strata = id)
  # cv_folds = train %>%  rsample::vfold_cv(v = 5)
  rf_grid = grid_space_filling(
    finalize(mtry(), train),
    min_n(),
    size = 30
  )

  rf_wf = workflow() %>%
    add_variables(outcomes = label,
                  predictors = starts_with("x")) %>%
    add_model(rf_spec)

  doParallel::registerDoParallel(cores = n_cores_inner)
  set.seed(234)
  rf_res = tune_grid(
    rf_wf,
    resamples = cv_folds,
    grid = rf_grid,
    control = control_grid(save_pred = FALSE, parallel_over = "everything")
  )

  best_acc = select_best(rf_res, metric = "roc_auc")

  final_rf = finalize_workflow(rf_wf, best_acc)

  final_res = last_fit(final_rf, initialsplit)

  preds = final_res %>%
    collect_predictions() %>%
    pull(.pred_1)
  rm(final_res); rm(rf_res); rm(final_rf)
  return(preds)
}

ifold = get_fold()
size = get_input()
filenames = read_rds(here::here("data", "lily", "data", "fingerprint_folds_temporal2.rds"))

# filenames = read_rds(here::here("data", "lily", "data", "fingerprint_folds_temporal_rf_missing.rds"))

#
if (!is.na(size)) {
  x = ceiling(nrow(filenames)/size)
  filenames = filenames %>%
    mutate(fold = rep(1:x, each = size)[1:nrow(filenames)])
}

#
fsize = ceiling(nrow(filenames)/1000)
x = ceiling(nrow(filenames)/fsize)
filenames = filenames %>%
  mutate(fold2 = rep(1:x, each = fsize)[1:nrow(filenames)])

# max(filenames$fold2)

# folds = seq(1:21)

# f = folds$fold[1]
folds = filenames %>%
  count(fold) %>%
  filter(n == size)

for(f in folds$fold){
  i = 1
  if (!is.na(ifold)) {
    ids = filenames %>%
      filter(fold == f & fold2 == ifold) %>% pull(id)
  }
  if(length(ids) > 0){
    ids_all =
      filenames %>%
      filter(fold == f) %>%
      pull(id) %>%
      as.character()

    if(size == 10770){
      dat_nzv = read_rds(here::here("data", "lily", "data", "dat_nzv_train_temporal2.rds")) %>%
        mutate(id = as.character(id)) %>%
        filter(id %in% ids_all)
      dat_nzv_test = read_rds(here::here("data", "lily", "data", "dat_nzv_test_temporal2.rds")) %>%
        mutate(id = as.character(id)) %>%
        filter(id %in% ids_all)
    } else if(size > 100) {
      dat_nzv = read_rds(here::here("data", "lily", "data", paste0("dat_nzv_train_temporal2_", size, "_", f, ".rds"))) %>%
        mutate(id = as.character(id)) %>%
        filter(id %in% ids_all)
      dat_nzv_test = read_rds(here::here("data", "lily", "data", paste0("dat_nzv_test_temporal2_", size, "_", f, ".rds"))) %>%
        mutate(id = as.character(id)) %>%
        filter(id %in% ids_all)
    } else {
      xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells_temporal2.csv.gz"))

      df =
        xdf %>%
        filter(id %in% ids_all)
      rm(xdf)
      data_train =
        df %>%
        filter(data == "train") %>%
        select(-data)

      data_test =
        df %>%
        filter(data == "test") %>%
        select(-data)

      nzv_trans =
        recipe(id ~ ., data = data_train) %>%
        step_nzv(all_predictors())

      nzv_estimates = prep(nzv_trans)

      nzv = colnames(juice(nzv_estimates))
      dat_nzv = data_train %>% dplyr::select(id, all_of(nzv))
      dat_nzv_test = data_test %>% dplyr::select(id, all_of(nzv))
      rm(data_train); rm(data_test)
    }
    options(parallelly.availableCores.methods = "custom", parallelly.availableCores.custom = n_cores)
    plan(multisession, workers = n_cores_outer)


    process_id = function(id) {
      print(paste0("id = ", id))
      outfile = here::here("data", "lily", "data", "fingerprint_res_temporal2", paste0(size, "rf"), paste0(id, ".rds"))
      if(file.exists(outfile)){
        out = read_rds(outfile)
        if(length(out) != nrow(dat_nzv_test)){
          x = try({
            preds = fit_model(subject = id, train = dat_nzv, test = dat_nzv_test)
            write_rds(preds, outfile, compress = "xz")
            rm(preds)
          })
          rm(x)
        }
        rm(out)
      } else if(!file.exists(outfile) | force) {
        x = try({
          preds = fit_model(subject = id, train = dat_nzv, test = dat_nzv_test)
          write_rds(preds, outfile, compress = "xz")
          rm(preds)
        })
        rm(x)
      }
    }
    # instead of for(id in ids)
    future_map(ids, process_id, .options = furrr_options(seed = TRUE),
               .progress = TRUE)
    plan(sequential)


  }
}

# find folds

# filenames %>%
#   filter(!(fold %in% c(2,5,6,10,11,20))) %>%
#   pull(fold2) %>%
#   unique() %>%
#   paste(., collapse = ",")

#1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,409,410,501,502,503,504,505,506,507,508,509,510,511,512,513,514,515,516,517,518,519,520,521,522,523,524,525,526,527,528,529,530,531,532,533,534,535,536,537,538,539,540,541,542,543,544,545,546,547,548,549,550,551,552,553,554,555,556,557,558,559,560,561,562,563,564,565,566,567,568,569,570,571,572,573,574,575,576,577,578,579,580,581,582,583,584,585,586,587,588,589,590,591,592,593,594,595,596,597,598,599,600,601,602,603,604,605,606,607,608,609,610,611,612,613,614,615,616,617,618,619,620,621,622,623,624,625,626,627,628,629,630,631,632,633,634,635,636,637,638,639,640,641,642,643,644,645,646,647,648,649,650,651,652,653,654,655,656,657,658,659,660,661,662,663,664,665,666,667,668,669,670,671,672,673,674,675,676,677,678,679,680,681,682,683,684,685,686,687,688,689,690,691,692,693,694,695,696,697,698,699,700,701,702,703,704,705,706,707,708,709,710,711,712,713,714,715,716,717,718,719,720,721,722,723,724,725,726,727,728,729,730,731,732,733,734,735,736,737,738,739,740,741,742,743,744,745,746,747,748,749,750,751,752,753,754,755,756,757,758,759,760,761,762,763,764,765,766,767,768,769,770,771,772,773,774,775,776,777,778,779,780,781,782,783,784,785,786,787,788,789,790,791,792,793,794,795,796,797,798,799,800,801,802,803,804,805,806,807,808,809,810,811,812,813,814,815,816,817,818,819,820,821,822,823,824,825,826,827,828,829,830,831,832,833,834,835,836,837,838,839,840,841,842,843,844,845,846,847,848,849,850,851,852,853,854,855,856,857,858,859,860,861,862,863,864,910,911,912,913,914,915,916,917,918,919,920,921,922,923,924,925,926,927,928,929,930,931,932,933,934,935,936,937,938,939,940,941,942,943,944,945,946,947,948,949,950,951,952,953,954,955,956,957,958,959,960,961,962,963,964,965,966,967,968,969,970,971,972,973,974,975,976,977,978,979,980"

