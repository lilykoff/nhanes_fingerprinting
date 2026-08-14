## create_data 
This folder has steps for creating grid cell data for models from the ADEPT-segmented walking 
+ `00_get_fingerprints.R`
  + Read in ADEPT file 
  + Get 80Hz data from seconds identified as having steps in ADEPT file 
  + Save 80Hz data during walking to `data/lily/data/adept_walking_dfs/<version>/<id>.csv.gz`
  + Get fingerprints (grid cell data) from walking seconds and save to `data/lily/data/fingerprint_data/<version>/<id>.csv.gz`, using lags of 0.15, 0.30, 0.45 seconds and grid cell size of 0.25g
+ `00_get_fingerprints_fine.R`
  + Same as above but with grid cell size of 0.1g, saved to `data/lily/data/fingerprint_data_10/<version>/<id>.csv.gz`
+ `00_get_fingerprints_sc.R`
  + Get fingerprints for stepcount walking segments (instead of ADEPT) 
  + Read in stepcount file 
  + Get 80Hz data from seconds identified as having steps in stepcount file 
  + Save 80Hz data during walking to `data/lily/data/sc_walking_dfs/<version>/<id>.csv.gz`
  + Get fingerprints (grid cell data) from walking seconds and save to `data/lily/data/fingerprint_data_sc/<version>/<id>.csv.gz`, using lags of 0.15, 0.30, 0.45 seconds and grid cell size of 0.25g
+ `00_get_grid_cell_predictors_random.R`
  + For sensitivity analysis: get ``random'' fingerprints from any data (not walking) 
  + Save 80Hz data to `data/lily/data/random_dfs/<version>/<id>.csv.gz`
  + Save fingerprints to `data/lily/data/random_fingerprint_data/<version>/<id>.csv.gz`
+ `01_get_random_mixed_predictors.R`
  + For sensitivity analysis: create grid cell predictors that are mixed walking data and "random" data
  + Save data data to `data/lily/data/mixed_grid_cell_data/grid_data_fold_<fold>.csv.gz`

+ `01_get_grid_cell_predictors.R`
  + For folds 1-200: 
  + For each subject in fold, read in `fingerprint_data` file and find bouts of walking
  + If there are at least 180 seconds of walking across valid bouts, randomly sample 180 seconds 
  + Save data from all subjects in fold to `data/lily/data/grid_cell_data/grid_data_fold_<fold>.csv.gz`
+ `01_get_grid_cell_predictors_fine.R`
  + Same as above but with the 0.1g predictors 
  + Save data from all subjects in fold to `data/lily/data/grid_cell_data_fine/grid_data_fold_<fold>.csv.gz`
+ `01_get_grid_cell_predictors_long.R`
  + Same as above but require 360 seconds of walking across valid bouts
  + Save data from all subjects in fold to `data/lily/data/grid_cell_data_big/grid_data_fold_<fold>.csv.gz`
+ `01_get_grid_cell_predictors_30min.R`
  + Same as above but require 30 min seconds of walking across valid bouts
  + Save data from all subjects in fold to `data/lily/data/grid_cell_data_30/grid_data_fold_<fold>.csv.gz`
+ `01_get_grid_cell_predictors_temporal2.R`
  + Same as above but require 135 seconds on one day, 45 seconds on a later day
  + Save data from all subjects in fold to `data/lily/data/grid_cell_data_temporal2/grid_data_fold_<fold>.csv.gz`
+ `01_get_grid_cell_predictors_long_temporal.R`
  + Same as above but require 270 seconds on one day, 90 seconds on a later day
  + Save data from all subjects in fold to `data/lily/data/grid_cell_data_long_temporal/grid_data_fold_xx.csv.gz`
+ `01_get_grid_cell_predictors_temporal_sc_small.R`
  + Require 135 seconds on one day, 45 seconds on a later day using stepcount not ADEPT
  + Save data from all subjects in fold to `data/lily/data/grid_cell_data_temporal_sc_small/grid_data_fold_<fold>.csv.gz`
+ `01_get_grid_cell_predictors_temporal_sc.R`
  + 30 minutes total of data identified by stepcount
  + Save data from all subjects in fold to `data/lily/data/grid_cell_data_temporal_sc/grid_data_fold_<fold>.csv.gz`
+ `01_get_grid_cell_predictors_sc_small.R`
  + Require 3 minutes of walking identified by stepcount 
  + Save data from all subjects in fold to `data/lily/data/grid_cell_data_sc_small/grid_data_fold_<fold>.csv.gz`
+ `01_get_grid_cell_predictors_sc.R`
  + Require 30 minutes of walking identified by stepcount 
  + Save data from all subjects in fold to `data/lily/data/grid_cell_data_sc/grid_data_fold_<fold>.csv.gz`
+ `01_get_grid_cell_predictors_temporal_30min.R`
  + Require 30 minutes of data, temporal paradigm
  + Save data from all subjects in fold to `data/lily/data/grid_cell_data_temporal_30min/grid_data_fold_<fold>.csv.gz`
+ `01_get_grid_cell_predictors_temporal_days.R`
  + Train on first day, test on last day 
  + Save data from all subjects in fold to `data/lily/data/grid_cell_data_temporal_days/grid_data_fold_<fold>.csv.gz`
+ `01_get_random_

+ `02_process_predictors.R` 
  + Read in all folds from `data/lily/data/grid_cell_data/grid_data_fold_xx.csv.gz` and write to one csv: `data/lily/data/all_grid_cells.csv.gz`; also save filenames/folds file `fingerprint_folds.rds`
All other `02_` files do the same thing for long and temporal data, respectively 

+ `03_make_data_allmodels.R`
  + Makes train/test data for all models (removing near zero variance columns). Data has the naming convention: 
  `dat_nzv_<train/test>_<model type>_<sample size>_<fold>.rds` and is saved to `data/lily/data`
+ `03_make_data_sc.R`: 
  + Make train/test data for stepcount data 
+ `03_make_data_sc_30min.R`
  + Make train/test for 30 min stepcount data
+ `03_make_sc_temporal_30min.R`
  + Make train/test for 30 min temporal stepcount data
+ `03_make_temporal_days.R`
  + Make train/test for temporal "days" paradigm
+ `03_make_pca_data.R`
  + Make train/test data using PCA for variable selection

## run models 
### random 
Run models in the random paradigm 
+ `run.R`: fit one vs. rest logistic regression models for sample sizes of n=100, 500, 1000. See `pipeline.sh` for how to run, result is file in `data/lily/data/fingerprint_res/<n>/fold_<fold>.rds`
+ `run_large.R`: fit one vs. rest logistic regression models for sample sizes of n=2500,5000,10000,13367. See `pipeline.sh` for how to run, result is file in `data/lily/data/fingerprint_res/<n>/<id>.rds`
+ `run_boosted_model.R`: runs the two-stage model sensitivity analysis 

Rest of files follow similar format with `run` and `run_large`, but for models other than logistic regression (lasso, xgboost, scalar on function regression, random forest). The `oversamp` models are for the oversampling sensitivity analysis, `weighted` are for weighted logistic regression, `mixed`, `random`, and `pca` are for sensitivity analysis with induced error and PCA dimension reduction, respectively. `stepwise` is attempt to run stepwise model. 

### temporal
Run models in the temporal paradigm 
+ `run_temporal.R`: fit one vs. rest logistic regression models for sample sizes of n=100, 500, 1000. See `pipeline.sh` for how to run, result is file in `data/lily/data/fingerprint_res_temporal2/<n>/fold_<fold>.rds`
+ `run_temporal_large.R`: fit one vs. rest logistic regression models for sample sizes of n=2500,5000,10000,13367. See `pipeline.sh` for how to run, result is file in `data/lily/data/fingerprint_res_temporal2/<n>/<id>.rds`
Rest of files follow similar format with `run` and `run_large`, but for models other than logistic regression (lasso, xgboost, scalar on function regression, random forest). The `oversamp` models are for the oversampling sensitivity analysis.

## summarize predictions
Files to summarize the results of the models. For each script, result is file in `data/lily/data/fingerprint_prediction_results` with format: `prediction_res_<temporal>_<sample size><model type>_.rds`. Relies on `summary_fns.R` file. 

## manuscript
Code to generate figures and tables for manuscript 

Other: 
+ `pipeline.sh`: bash code to run scripts on JHPCE cluster 
+ `get_walking_dists.R`: code to get distribution of walking bouts across population
+ `summary_fns.R`: helper functions for summarize predictions files 
+ `nonwear_analysis.R`: checking overlap of nonwear and walking in data

## revision 
Code for additions for revisions
+ `n_train_benchmarking.R`: comparing time for models with different number of people in training data 


