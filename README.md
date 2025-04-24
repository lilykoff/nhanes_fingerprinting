# nhanes_fingerprinting

## code 
- `00_get_fingerprints.R`: read in ADEPT file to find seconds with walking, read in raw csv file and get seconds where ADEPT identifies walking, save in `adept_walking_dfs` folder, then calculate fingerprints and save to `fingerprint_data` folder
  - Default fingerprint settings are lags 12, 24, 36 samples and grid cell size of 0.25g
  - `00_get_fingerprints_fine.R`: read in `adept_walking_dfs` file, then run fingerprints with grid cell size of 0.10g, save to `fingerprint_data_10` folder
- `01_get_grid_cell_predictors.R`: read in `fingerprint_data` files, get segments of consecutive walking with < 2 seconds between them that are at least 10 seconds long. If there are at least 3 minutes of data, sample 3 minutes randomly (so that everyone has same amount of data). Save to `grid_cell_data` folder (in folds)
  - `01_get_grid_cell_predictors_fine.R`: read in `fingerprint_data_10` files, get segments of consecutive walking with < 2 seconds between them that are at least 10 seconds long. If there are at least 3 minutes of data, sample 3 minutes randomly (so that everyone has same amount of data). Save to `grid_cell_data_fine` folder (in folds)
- `02_process_predictors.R`: bind files from `grid_cell_data` into one file: `all_grid_cell_data.csv` 
- `04_regress_on_covars.R` 
- `make_folds.R`: make folds for fold-based regression
- `run_fingerprints_<n>.R`: run fingerprinting logistic models on groups with `n` subjects each 
- `scripts.sh`: scripts to run on cluster 
- `summarize_preds.R`: process results of run fingerprints models
