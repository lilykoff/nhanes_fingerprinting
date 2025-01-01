Rnosave test_fingerperint.R -J testmem --mem=15G --array=1 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave get_fingerprints.R -J FINGERPRINT --mem=15G --array=1-200 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave redo_fingerprints.R -J FINGERPRINT2 --mem=8G --array=1-200 --time=3-00:00:00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave 00_get_fingerprints.R -J FINGERPRINTNEW --mem=15G --array=1-200 --time=3-00:00:00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 01_get_grid_cell_predictors.R -J GCPREDS --mem=15G --array=1-200 --time=3-00:00:00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave get_day_fingerprints.R -J GCDAYPREDS --mem=15G --array=2-200 --time=3-00:00:00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave get_icc_byfold.R -J ICCFOLD --mem=70G --time=3-00:00:00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err


Rnosave 01_get_grid_cell_predictors_fine.R -J GCPREDS_FINE --mem=15G --array=1-200 --time=3-00:00:00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave 01_get_grid_cell_predictors_temporal.R -J GCPREDS_TEMP --mem=15G --array=1-200 --time=3-00:00:00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err


Rnosave make_folds.R -J MAKEFOLDS --mem=50G

Rnosave 05_run_fingerprints_300.R -J FPREDS_300 --mem=50G --array=1-44 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave 05_run_fingerprints_100.R -J FPREDS_100 --mem=40G --array=1-133 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_200.R -J FPREDS_200 --mem=40G --array=1-66 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 04_regress_on_covars.R -J REGRESSF3 --mem=90G -o eofiles/%x_%A.out -e eofiles/%x_%A.err
Rnosave 05_run_fingerprints_1000.R -J FPREDS_1000 --mem=100G --array=1-13 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave 05_run_fingerprints_1000_xgb.R -J FPREDS_1000XGB --time=30-00 --mem=150G --array=1-13 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu


Rnosave 05_run_fingerprints_1000_over.R -J FPREDS_1000OV --mem=100G --array=1-13 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_2000_over.R -J FPREDS_2000OV --mem=100G --array=1-6 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
 Rnosave 05_run_fingerprints_1000_over_pct.R -J FPREDS_1000OVPCT --mem=100G --array=1-13 --time=7-00:00:00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave 05_run_fingerprints_500.R -J FPREDS_500 --mem=70G --array=1-26 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_2000.R -J FPREDS_2000 --mem=100G --array=1-6 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_4000.R -J FPREDS_4000 --mem=400G --array=1-3 --time=7-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_4000_over.R -J FPREDS_4000_OV --mem=400G --array=1-3 --time=7-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_5000_over.R -J FPREDS_5000_OV --mem=400G --array=1-2 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_all_over.R -J FPREDS_ALL_OV --mem=400G --time=7-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err

# start here
Rnosave 05_run_fingerprints_5000.R -J FPREDS_5000 --mem=400G --array=1-2 --time=30-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_all.R -J FPREDS_ALL --mem=400G --time=30-00 -o eofiles/%x_%j.out -e eofiles/%x_%j.err


Rnosave 00_get_fingerprints_fine.R -J FINGERPRINTFINE --mem=15G --array=1-200 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 00_get_fingerprints_fine.R -J FINGERPRINTFINE --mem=15G --array=1-200 --time=2-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave multinomial.R -J MNOM --array=1 --mem=100G -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave summarize_preds.R -J SUMMP --mem=100G -o eofiles/%x_%A.out -e eofiles/%x_%A.err
Rnosave summarize_preds_train_not_test.R -J SUMMPT --mem=100G -o eofiles/%x_%A.out -e eofiles/%x_%A.err
Rnosave summarize_preds_test_not_train.R -J SUMMPTT --mem=100G -o eofiles/%x_%A.out -e eofiles/%x_%A.err

Rnosave get_walking_dists.R -J WALKDIST --mem=30G -o eofiles/%x_%A.out -e eofiles/%x_%A.err

Rnosave 02_process_predictors_fine.R -J PROCPREDFINE --mem=70G -o eofiles/%x_%A.out -e eofiles/%x_%A.err
Rnosave 02_process_predictors_temporal.R -J PROCPREDTEMP --mem=50G -o eofiles/%x_%A.out -e eofiles/%x_%A.err

Rnosave 05_run_fingerprints_100.R -J FPREDS_100 --mem=40G --array=1-133 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

##train not test
Rnosave train_not_test.R -J FPREDS_TNT1000 --mem=100G --array=1-13 --time=7-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave test_not_train.R -J FPREDS_TNT21000 --mem=100G --array=1-13 --time=7-00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err



Rnosave distribution_plots.R -J DPLOT --mem=50G -o eofiles/%x_%A.out -e eofiles/%x_%A.err


Rnosave 05_run_fingerprints_100_xgboost.R -J FPREDS_100XGB --mem=40G --array=1 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_100_xgboost.R -J FPREDS_100XGB --mem=40G --array=1-50 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave make_data_allmodels.R -J MAKEDAT --mem=35G -o eofiles/%x_%A.out -e eofiles/%x_%A.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

Rnosave 05_run_fingerprints_all_folds.R -J INDIVID --mem=30G --array=2-1000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_all_folds.R -J INDIVID --mem=30G --array=1001-2000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_all_folds.R -J INDIVID --mem=30G --array=2001-3000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_all_folds.R -J INDIVID --mem=30G --array=3001-4000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_all_folds.R -J INDIVID --mem=30G --array=4001-5000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_all_folds.R -J INDIVID --mem=30G --array=5001-6000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_all_folds.R -J INDIVID --mem=30G --array=6001-7000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_all_folds.R -J INDIVID --mem=30G --array=7001-8000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_all_folds.R -J INDIVID --mem=30G --array=8001-9000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_all_folds.R -J INDIVID --mem=30G --array=9001-10000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_all_folds_x.R -J INDIVID --mem=30G --array=1-1000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_all_folds_x.R -J INDIVID --mem=30G --array=1001-2000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_all_folds_x.R -J INDIVID --mem=30G --array=2001-3000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_all_folds_x.R -J INDIVID --mem=30G --array=3001-3367 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err


Rnosave 05_run_fingerprints_generic.R -J FP100 --export=INPUT=100 --mem=40G --array=1-133 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic.R -J FP250 --export=INPUT=250 --mem=60G --array=1-53 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic.R -J FP500 --export=INPUT=250 --mem=60G --array=1-26 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic.R -J FP1000 --export=INPUT=1000 --mem=100G --array=1-13 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave 05_run_fingerprints_generic_temporal.R -J FPT100 --export=INPUT=100 --mem=40G --array=1-112 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic_temporal.R -J FPT250 --export=INPUT=250 --mem=60G --array=1-44 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

# run
Rnosave 05_run_fingerprints_generic_temporal.R -J FPT500 --export=INPUT=500 --mem=60G --array=1-22 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic_temporal.R -J FPT1000 --export=INPUT=1000 --mem=100G --array=1-11 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic_large.R -J FP2500 --export=INPUT=2500 --mem=30G --array=2-1000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic_large.R -J FP5000 --export=INPUT=5000 --mem=30G --array=1-1000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic_large.R -J FP10000 --export=INPUT=10000 --mem=40G --array=1-1000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave 05_run_fingerprints_generic_large.R -J FPALL --export=INPUT=13367 --mem=50G --array=1-1000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave 05_run_fingerprints_generic_large_temporal.R -J FP2500T --export=INPUT=2500 --mem=15G --array=1-1000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic_large_temporal.R -J FP5000T --export=INPUT=5000 --mem=15G --array=1-1000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic_large_temporal.R -J FP10000T --export=INPUT=10000 --mem=15G --array=1-1000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic_large_temporal.R -J FPALLT --export=INPUT=11225 --mem=30G --array=1-1000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave 06_summarize_preds.R -J SUMMP2 --mem=60G -o eofiles/%x_%A.out -e eofiles/%x_%A.err
Rnosave 06_summarize_preds_all.R -J SUMMP3 --mem=60G -o eofiles/%x_%A.out -e eofiles/%x_%A.err

Rnosave 05_run_fingerprints_generic_large.R -J FPALLFIN --export=INPUT=13367 --mem=100G --array=26,74,77,78 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

Rnosave 05_run_fingerprints_generic_xgb.R -J XGB100 --export=INPUT=100 --mem=30G --array=1-133 --cpus-per-task=8 --ntasks=1 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_temporal_xgb.R -J XGBT100 --export=INPUT=100 --mem=30G --array=1-133 --cpus-per-task=8 --ntasks=1 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

Rnosave 05_run_fingerprints_generic_rf.R -J RF100 --export=INPUT=100 --mem=30G --array=1-133 --cpus-per-task=8 --ntasks=1 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_temporal_rf.R -J RFT100 --export=INPUT=100 --mem=30G --array=1-133 --cpus-per-task=8 --ntasks=1 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu


Rnosave 05_run_fingerprints_generic_large_xgb2.R -J XBG500TEST --export=INPUT=500 --mem=50G --array=1 --cpus-per-task=8 --ntasks=1 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_large_xgb2.R -J XGB500 --export=INPUT=500 --mem=30G --array=1-1000 --cpus-per-task=8 --ntasks=1 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_large_temporal_xgb2.R -J XGBT500 --export=INPUT=500 --mem=30G --array=1-1000 --cpus-per-task=8 --ntasks=1 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

Rnosave 05_run_fingerprints_generic_large_xgb2.R -J XGB5000 --export=INPUT=5000 --mem=40G --array=1-1000 --cpus-per-task=8 --ntasks=1 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_large_temporal_xgb2.R -J XGBT5000 --export=INPUT=5000 --mem=40G --array=1-1000 --cpus-per-task=8 --ntasks=1 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu





Rnosave 05_run_fingerprints_generic_xgb.R -J XGB500 --export=INPUT=500 --mem=50G --array=1-26 --cpus-per-task=8 --ntasks=1 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_temporal_xgb.R -J XGBT500 --export=INPUT=500 --mem=50G --array=1-26 --cpus-per-task=8 --ntasks=1 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

Rnosave 05_run_fingerprints_generic_xgb.R -J XGB1000 --export=INPUT=1000 --mem=50G --array=1-13 --cpus-per-task=8 --ntasks=1 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_temporal_xgb.R -J XGBT1000 --export=INPUT=1000 --mem=50G --array=1-13 --cpus-per-task=8 --ntasks=1 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

Rnosave 05_run_fingerprints_generic_large_temporal_xgb.R -J XGB10000T --export=INPUT=10000 --mem=20G --cpus-per-task=8 --ntasks=1 --array=1-1000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 05_run_fingerprints_generic_large_xgb.R -J XGB10000 --export=INPUT=10000 --mem=20G --cpus-per-task=8 --ntasks=1 --array=1-1000 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

# run these 2 script next
Rnosave 05_run_fingerprints_generic_tnt.R -J TNT100 --export=INPUT=100 --mem=30G --array=1-133 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_pctov.R -J PCT100 --export=INPUT=100 --mem=30G --array=1-133 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu


Rnosave 05_run_fingerprints_generic_temporal_rf.R -J RFT100 --export=INPUT=100 --mem=40G --array=1-133 --cpus-per-task=8 --ntasks=1 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_rf.R -J RF100 --export=INPUT=100 --mem=40G --array=1-133 --cpus-per-task=8 --ntasks=1 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu

Rnosave 05_run_fingerprints_generic_large_rf.R -J RF500 --export=INPUT=500 --mem=30G --array=1-1000 --cpus-per-task=8 --ntasks=1 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
Rnosave 05_run_fingerprints_generic_large_temporal_rf.R -J RFT500 --export=INPUT=500 --mem=30G --array=1-1000 --cpus-per-task=8 --ntasks=1 -t `timeleft` -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu
