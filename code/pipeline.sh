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
Rnosave run_xgb_large.R -J XGB500 --export=INPUT=500 --mem=50G --array=1-1000 --cpus-per-task=8 --ntasks=1 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

# rf
Rnosave run_rf.R -J RF100 --export=INPUT=100 --mem=50G --array=1-133 --cpus-per-task=8 --ntasks=1 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_rf_large.R -J RF500 --export=INPUT=500 --mem=50G --array=1-1000 --cpus-per-task=8 --ntasks=1 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu


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
Rnosave run_temporal_xgb_large.R -J XGB500T --export=INPUT=500 --mem=50G --array=1-955 --cpus-per-task=12 --ntasks=1 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu


# rf
Rnosave run_temporal_rf.R -J RF100T --export=INPUT=100 --mem=50G --array=1-107 --cpus-per-task=8 --ntasks=1 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_temporal_rf_large.R -J RF500T --export=INPUT=500 --mem=50G --array=1-1000 --cpus-per-task=8 --ntasks=1 --time=10-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

# sofr
Rnosave run_temporal_sofr.R -J SOFR100T --export=INPUT=100 --mem=30G --array=1-107 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
qRnosave run_temporal_sofr.R -J SOFR500T --export=INPUT=500 --mem=30G --array=1-1000 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
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
Rnosave xgb_rf.R -J SUMM_ML --mem=40G --time=3-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave long_temporal_subset.R -J SUMM_LONG_TS --mem=50G --time=3-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu


## boosting

Rnosave get_all_preds_13367.R -J GET_PREDS --mem=5G --array=1-200 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave get_all_preds_10770.R -J GET_PREDS_TEMP --mem=5G --array=1-200 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

Rnosave run_boosted_model.R -J RUN_BOOSTED --mem=10G --array=1-200 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave run_temporal_boosted_model.R -J RUN_BOOSTED_TEMP --mem=10G --array=1-200 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

