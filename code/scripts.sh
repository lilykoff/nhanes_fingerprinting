Rnosave test_fingerperint.R -J testmem --mem=15G --array=1 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave get_fingerprints.R -J FINGERPRINT --mem=15G --array=1-200 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave redo_fingerprints.R -J FINGERPRINT2 --mem=8G --array=1-200 --time=3-00:00:00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave 00_get_fingerprints.R -J FINGERPRINTNEW --mem=15G --array=1-200 --time=3-00:00:00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 01_get_grid_cell_predictors.R -J GCPREDS --mem=15G --array=1-200 --time=3-00:00:00 -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave make_folds.R -J MAKEFOLDS --mem=50G
