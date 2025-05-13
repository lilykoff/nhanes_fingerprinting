# get the fingerprints (walking)
Rnosave 00_get_fingerprints.R -J FINGERPRINTNEW --mem=15G --array=1-200 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

# get the grid cell predictors - regular, fine, and temporal
Rnosave 01_get_grid_cell_predictors.R -J GCPREDS --mem=15G --array=1-200 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 01_get_grid_cell_predictors_fine.R -J GCPREDS_FINE --mem=15G --array=1-200 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 01_get_grid_cell_predictors_temporal.R -J GCPREDS_TEMP --mem=15G --array=1-200 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 01_get_grid_cell_predictors_long.R -J GCPREDS_LONG --mem=15G --array=1-200 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 01_get_grid_cell_predictors_long_temporal.R -J GCPREDS_LONG_TEMP --mem=15G --array=1-200 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 01_get_grid_cell_predictors_temporal2.R -J GCPREDS_TEMP2 --mem=15G --array=1-200 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err


# process the predictors to make one file w/ all predictors
Rnosave 02_process_predictors_fine.R -J PROCPREDFINE --mem=70G -o eofiles/%x_%A.out -e eofiles/%x_%A.err
Rnosave 02_process_predictors_temporal.R -J PROCPREDTEMP --mem=50G -o eofiles/%x_%A.out -e eofiles/%x_%A.err
Rnosave 02_process_predictors.R -J PROCPRED --mem=70G -o eofiles/%x_%A.out -e eofiles/%x_%A.err
Rnosave 02_process_predictors_long.R -J PROCPREDLONG --mem=70G -o eofiles/%x_%A.out -e eofiles/%x_%A.err
Rnosave 02_process_predictors_temporal2.R -J PROCPREDTEMP --mem=50G -o eofiles/%x_%A.out -e eofiles/%x_%A.err
Rnosave 02_process_predictors_long_temporal.R -J PROCPREDL --mem=50G -o eofiles/%x_%A.out -e eofiles/%x_%A.err


# make train/test data and folds
Rnosave 03_make_folds.R -J MAKEFOLDS --mem=50G -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 03_make_data_allmodels.R -J MAKEDAT --mem=20G -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu


# regress on covariates (outcome is grid cell, predictor is covariate)
Rnosave 04_regress_on_covars.R -J REGRESSF3 --mem=120G --cpus-per-task=8 --ntasks=1 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

## run models - random

# logistic
Rnosave run.R -J FP100 --export=INPUT=100 --mem=40G --array=1-133 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave run.R -J FP250 --export=INPUT=250 --mem=60G --array=1-53 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave run.R -J FP500 --export=INPUT=250 --mem=60G --array=1-26 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave run.R -J FP1000 --export=INPUT=1000 --mem=100G --array=1-13 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
# larger sample size
Rnosave run_large.R -J FP2500 --export=INPUT=2500 --mem=30G --array=1-1000 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave run_large.R -J FP5000 --export=INPUT=5000 --mem=30G --array=1-1000 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave run_large.R -J FP10000 --export=INPUT=10000 --mem=40G --array=1-1000 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave run_large.R -J FPALL --export=INPUT=13367 --mem=50G --array=1-1000 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave run_large_weighted.R -J FPWT --export=INPUT=13367 --mem=15G --array=1 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err


# lasso
Rnosave run_lasso.R -J LASSO100 --export=INPUT=100 --mem=30G --array=1-133 --cpus-per-task=8 --ntasks=1 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_lasso_large.R -J LASSO500 --export=INPUT=500 --mem=30G --array=1-1000 --cpus-per-task=8 --ntasks=1 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

# xgb
Rnosave run_xgb.R -J XGB100 --export=INPUT=100 --mem=30G --array=1-133 --cpus-per-task=8 --ntasks=1 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_xgb_large.R -J XGB500 --export=INPUT=500 --mem=20G --array=1-1000 --cpus-per-task=8 --ntasks=1 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

Rnosave run_xgb_large.R -J XGB500 --export=INPUT=500 --mem=30G --array=53,70,72,73,74,75,76,78,79,80,81,82,167,169,426,627,629,861,927,928 --cpus-per-task=8 --ntasks=1 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

# rf
Rnosave run_rf.R -J RF100 --export=INPUT=100 --mem=50G --array=1-133 --cpus-per-task=8 --ntasks=1 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_rf_large.R -J RF500 --export=INPUT=500 --mem=20G --array=1-1000 --cpus-per-task=8 --ntasks=1 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

Rnosave run_rf_large.R -J RF500 --export=INPUT=500 --mem=12G --array=6,9,42,43,45,46,50,55,56,58,60,61,62,63,65,66,69,70,71,72,81,83,88,89,91,92,93,95,98,99,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,409,412,413,433,445,448,450,453,454,455,456,457,458,459,460,461,462,463,464,465,466,467,468,469,470,471,472,473,474,475,476,477,478,479,480,481,482,483,484,485,486,487,488,489,490,491,492,493,494,495,496,497,498,499,500,512,513,514,515,516,522,523,524,541,546,559,560,561,562,563,593,600,601,602,605,606,613,614,622,623,625,628,629,630,631,633,634,635,636,638,639,640,641,642,643,644,645,646,647,648,649,650,651,652,653,654,655,656,657,658,659,660,661,662,663,664,665,666,667,668,669,670,671,672,673,674,675,676,677,678,679,680,681,682,683,684,685,686,687,688,689,690,691,692,693,694,695,696,697,698,699,700,701,702,703,704,705,706,707,708,709,710,711,712,713,714,715,716,717,718,719,720,721,722,723,724,725,726,727,728,729,730,731,732,733,734,735,736,737,738,739,740,741,742,743,744,745,746,747,748,749,750,751,752,753,754,755,756,757,758,759,760,761,762,763,764,765,766,767,768,769,770,771,772,773,774,775,776,777,778,779,780,781,782,783,784,785,786,787,788,789,790,791,792,793,794,795,796,797,798,799,800,801,802,803,804,805,806,807,808,809,810,811,812,813,814,815,816,817,818,819,820,821,822,823,824,825,826,827,828,829,830,831,832,833,834,835,836,837,838,839,840,841,842,843,844,845,846,847,848,849,850,851,852,853,854,855,856,857,858,859,860,861,862,863,864,865,866,867,868,869,870,871,872,873,874,875,876,877,878,879,880,881,882,883,884,885,886,887,888,889,890,891,892,893,894,895,896,897,898,899,900,901,902,903,904,905,906,907,908,909,910,911,912,913,914,915,916,917,918,919,920,921,922,923,924,925,926,927,928,929 --cpus-per-task=8 --ntasks=1 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

# sofr
Rnosave run_sofr.R -J SOFR100 --export=INPUT=100 --mem=30G --array=1-133 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_sofr.R -J SOFR500 --export=INPUT=500 --mem=30G --array=1-1000 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_sofr.R -J SOFR1000 --export=INPUT=1000 --mem=30G --array=1-1000 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

# long sensitivity analysis
Rnosave run_long.R -J FPL100 --export=INPUT=100 --mem=40G --array=1-101 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave run_long_large.R -J FP2500L --export=INPUT=2500 --mem=30G --array=1-1000 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave run_long_large.R -J FP5000L --export=INPUT=5000 --mem=30G --array=1-921 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave run_long_large.R -J FP10000L --export=INPUT=10129 --mem=40G --array=1-921 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave run_large_longsubset.R -J FP10129 --export=INPUT=10129 --mem=15G --array=1-921 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

# oversampling
Rnosave run_oversamp.R -J PCT100 --export=INPUT=100 --mem=35G --array=1-133 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_oversamp.R -J PCT500  --export=INPUT=500 --mem=35G --array=1-22 --time 10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_oversamp_large.R -J PCT10000 --export=INPUT=10000 --mem=30G --array=1-1000 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

Rnosave run_oversamp_large.R -J PCTALL --export=INPUT=13367 --mem=20G --array=1-1000 --time=2-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

# run models - temporal

# logistic
Rnosave run_temporal.R -J FP100T --export=INPUT=100 --mem=40G --array=1-107 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave run_temporal.R -J FP500T --export=INPUT=250 --mem=60G --array=1-21 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave run_temporal.R -J FP1000T --export=INPUT=1000 --mem=100G --array=1-10 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
# larger sample size
Rnosave run_temporal_large.R -J FP2500T --export=INPUT=2500 --mem=30G --array=1-1000 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave run_temporal_large.R -J FP5000T --export=INPUT=5000 --mem=30G --array=1-1000 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave run_temporal_large.R -J FP10000T --export=INPUT=10770 --mem=40G --array=1-1000 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err


# lasso
Rnosave run_temporal_lasso.R -J LASSO100T --export=INPUT=100 --mem=30G --array=1-107 --cpus-per-task=8 --ntasks=1 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_temporal_lasso_large.R -J LASSO500T --export=INPUT=500 --mem=30G --array=1-1000 --cpus-per-task=8 --ntasks=1 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

# xgb
Rnosave run_temporal_xgb.R -J XGB100T --export=INPUT=100 --mem=30G --array=1-107 --cpus-per-task=8 --ntasks=1 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_temporal_xgb_large.R -J XGB500T --export=INPUT=500 --mem=12G --array=2-955 --cpus-per-task=12 --ntasks=1 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu


Rnosave run_xgbt_rfish.R -J XGB500T --export=INPUT=500 --mem=6G --array=500 --cpus-per-task=12 --ntasks=1 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

# rf
Rnosave run_temporal_rf.R -J RF100T --export=INPUT=100 --mem=50G --array=1-107 --cpus-per-task=8 --ntasks=1 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_temporal_rf_large.R -J RF500T --export=INPUT=500 --mem=50G --array=1-1000 --cpus-per-task=8 --ntasks=1 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

# sofr
Rnosave run_temporal_sofr.R -J SOFR100T --export=INPUT=100 --mem=30G --array=1-107 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_temporal_sofr.R -J SOFR500T --export=INPUT=500 --mem=30G --array=1-1000 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_temporal_sofr.R -J SOFR1000T --export=INPUT=1000 --mem=30G --array=1-1000 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

# long sensitivity analysis
Rnosave run_temporal_long.R -J FPL100T --export=INPUT=100 --mem=40G --array=1-80 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_temporal_long.R -J FPL1000T --export=INPUT=100 --mem=40G --array=1-8 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

Rnosave run_temporal_long_large.R -J FP2500LT --export=INPUT=2500 --mem=20G --array=1-1000 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_temporal_long_large.R -J FP5000LT --export=INPUT=5000 --mem=30G --array=1-1000 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_temporal_long_large.R -J FP10000LT --export=INPUT=8018 --mem=40G --array=1-1000 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

# susbet analysis
Rnosave run_temporal_longsubset.R -J FP8018 --export=INPUT=8018 --mem=15G --array=1-891 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_longsubset.R -J FP10129 --export=INPUT=10129 --mem=20G --array=1-921 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu


# oversampling
Rnosave run_temporal_oversamp.R -J PCT100T --export=INPUT=100 --mem=35G --array=1-107 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_temporal_oversamp.R -J PCT500T  --export=INPUT=500 --mem=35G --array=1-21 --time 10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_temporal_oversamp.R -J PCT1000T  --export=INPUT=1000 --mem=35G --array=1-10 --time 10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_temporal_oversamp_large.R -J PCT10000 --export=INPUT=10000 --mem=30G --array=1-1000 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

# weighted
Rnosave run_large_weighted.R -J FPWT --export=INPUT=13367 --mem=15G --array=1-1000 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave run_temporal_weighted_large.R -J FPWT_TEMP --export=INPUT=10770 --mem=15G --array=1-1000 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

## summarizing

Rnosave lasso.R -J SUMM_LASSO --mem=50G --time=3-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave logistic_random.R -J SUMM_LOG_R --mem=20G --time=3-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave logistic_temporal.R -J SUMM_TEMP_T --mem=50G --time=3-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave long_random.R -J SUMM_LONG_R --mem=50G --time=3-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave long_temporal.R -J SUMM_LONG_T --mem=50G --time=3-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave oversample_random_large.R -J SUMM_OV_R_LG --mem=50G --time=3-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave oversample_temporal_large.R -J SUMM_OV_T_LG --mem=50G --time=3-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave oversample_random.R -J SUMM_OV_R --mem=50G --time=3-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave oversample_temporal.R -J SUMM_OV_T --mem=50G --time=3-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave sofr.R -J SUMM_SOFR --mem=50G --time=3-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave xgb_rf.R -J SUMM_ML --mem=10G --time=3-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave long_temporal_subset.R -J SUMM_LONG_TS --mem=50G --time=3-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

## boosting

Rnosave get_all_preds_13367.R -J GET_PREDS --mem=5G --array=1-200 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave get_all_preds_10770.R -J GET_PREDS_TEMP --mem=5G --array=1-200 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

Rnosave run_boosted_model.R -J RUN_BOOSTED --mem=10G --array=1-200 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_temporal_boosted_model.R -J RUN_BOOSTED_TEMP --mem=10G --array=1-200 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

