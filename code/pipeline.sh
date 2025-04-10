# get the fingerprints (walking)
Rnosave 00_get_fingerprints.R -J FINGERPRINTNEW --mem=15G --array=1-200 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

# get the grid cell predictors - regular, fine, and temporal
Rnosave 01_get_grid_cell_predictors.R -J GCPREDS --mem=15G --array=1-200 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 01_get_grid_cell_predictors_fine.R -J GCPREDS_FINE --mem=15G --array=1-200 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 01_get_grid_cell_predictors_temporal.R -J GCPREDS_TEMP --mem=15G --array=1-200 --time=3-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

# process the predictors to make one file w/ all predictors
Rnosave 02_process_predictors_fine.R -J PROCPREDFINE --mem=70G -o eofiles/%x_%A.out -e eofiles/%x_%A.err
Rnosave 02_process_predictors_temporal.R -J PROCPREDTEMP --mem=50G -o eofiles/%x_%A.out -e eofiles/%x_%A.err
Rnosave 02_process_predictors.R -J PROCPRED --mem=70G -o eofiles/%x_%A.out -e eofiles/%x_%A.err


# make train/test data and folds
Rnosave 03_make_folds.R -J MAKEFOLDS --mem=50G -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 03_make_data_allmodels.R -J MAKEDAT --mem=35G -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu


# regress on covariates (outcome is grid cell, predictor is covariate)
Rnosave 04_regress_on_covars.R -J REGRESSF3 --mem=120G --cpus-per-task=8 --ntasks=1 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu


# logistic regression fingerprints for different size folds: 100 through 1000
Rnosave 05_run_fingerprints_generic.R -J FP100 --export=INPUT=100 --mem=40G --array=1-133 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic.R -J FP250 --export=INPUT=250 --mem=60G --array=1-53 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic.R -J FP500 --export=INPUT=250 --mem=60G --array=1-26 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic.R -J FP1000 --export=INPUT=1000 --mem=100G --array=1-13 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
# 2500 thru all
Rnosave 05_run_fingerprints_generic_large.R -J FP2500 --export=INPUT=2500 --mem=30G --array=2-1000 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic_large.R -J FP5000 --export=INPUT=5000 --mem=30G --array=1-1000 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic_large.R -J FP10000 --export=INPUT=10000 --mem=40G --array=1-1000 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic_large.R -J FPALL --export=INPUT=13367 --mem=50G --array=1-1000 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

# temporal logistic regression fingerprints
Rnosave 05_run_fingerprints_generic_temporal.R -J FPT100 --export=INPUT=100 --mem=40G --array=1-112 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic_temporal.R -J FPT250 --export=INPUT=250 --mem=60G --array=1-44 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic_temporal.R -J FPT500 --export=INPUT=500 --mem=60G --array=1-22 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic_temporal.R -J FPT1000 --export=INPUT=1000 --mem=100G --array=1-11 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic_large_temporal.R -J FP2500T --export=INPUT=2500 --mem=15G --array=1-1000 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic_large_temporal.R -J FP5000T --export=INPUT=5000 --mem=15G --array=1-1000 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic_large_temporal.R -J FP10000T --export=INPUT=10000 --mem=15G --array=1-1000 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic_large_temporal.R -J FPALLT --export=INPUT=11225 --mem=30G --array=1-1000 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err


# xgboost models
Rnosave 05_run_fingerprints_generic_xgb.R -J XGB100 --export=INPUT=100 --mem=30G --array=1-133 --cpus-per-task=8 --ntasks=1 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_temporal_xgb.R -J XGBT100 --export=INPUT=100 --mem=30G --array=1-112 --cpus-per-task=8 --ntasks=1 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_large_xgb.R -J XGB500 --export=INPUT=500 --mem=30G --array=1-1000 --cpus-per-task=8 --ntasks=1 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_large_temporal_xgb.R -J XGBT500 --export=INPUT=500 --mem=30G --array=1-1000 --cpus-per-task=8 --ntasks=1 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

# random forest models
Rnosave 05_run_fingerprints_generic_rf.R -J RF100 --export=INPUT=100 --mem=30G --array=1-133 --cpus-per-task=8 --ntasks=1 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_temporal_rf.R -J RFT100 --export=INPUT=100 --mem=30G --array=1-112 --cpus-per-task=8 --ntasks=1 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_large_rf.R -J RF500 --export=INPUT=500 --mem=30G --array=1-1000 --cpus-per-task=8 --ntasks=1 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_large_temporal_rf.R -J RFT500 --export=INPUT=500 --mem=30G --array=1-1000 --cpus-per-task=8 --ntasks=1 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

## oversampling models
Rnosave 05_run_fingerprints_generic_pctov.R -J PCT100 --export=INPUT=100 --mem=35G --array=1-133 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_pctov_tmp.R -J PCT100T --export=INPUT=100 --mem=35G --array=1-112 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_pctov.R -J PCT500  --export=INPUT=500 --mem=35G --array=1-22 --time 30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_pctov_tmp.R -J PCT500T  --export=INPUT=500 --mem=35G --array=1-22 --time 30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_large_over.R -J PCT10000 --export=INPUT=10000 --mem=30G --array=1-1000 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_large_temporal_over.R -J PCT10000T --export=INPUT=10000 --mem=30G --array=1-1000 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

# scalar on function regression
Rnosave 06_fit_sofr.R -J SOFR100 --export=INPUT=100 --mem=30G --array=1-133 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 06_fit_sofr_temporal.R -J TSOFR100 --export=INPUT=100 --mem=30G --array=1-112 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 06_fit_sofr.R -J SOFR500 --export=INPUT=500 --mem=40G --array=1-1000 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 06_fit_sofr_temporal.R -J TSOFR500 --export=INPUT=500 --mem=40G --array=1-1000 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 06_fit_sofr.R -J SOFR1000 --export=INPUT=1000 --mem=50G --array=1-1000 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

# test not train
Rnosave 05_run_fingerprints_generic_tnt.R -J TNT100 --export=INPUT=100 --mem=35G --array=1-133 -t 30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_tnt.R -J TNT500 --export=INPUT=500 --mem=35G --array=1-26 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu


# summarizing predictions
Rnosave 07_summarize_predictions.R -J SUMMPA2 --mem=200G --time=30-00 --cpus-per-task=8 --ntasks=1 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 07_summarize_oversample.R -J PCTPROC --mem=100G --time=30-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 07_summarize_preds_testpct.R -J TNTPROC --mem=120G --time=30-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu


# investigation
Rnosave 04b_investigate_subs.R -J INVSUB --mem=50G --time=30-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
