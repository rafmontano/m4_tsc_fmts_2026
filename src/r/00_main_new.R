# ==============================================================================
# 00_main_new.R
#
# Purpose: Run the R data-preparation, modelling, and evaluation pipeline.
# Inputs:  M4comp2018 data, shared configuration, and R source scripts.
# Outputs: Generated datasets, fitted models, and evaluation results.
# Run from: Repository root with Rscript src/r/00_main_new.R
# ==============================================================================

# Dependencies ----------------------------------------------------------------

library(future)
library(future.apply)
library(forecast)
library(M4comp2018)

source("src/r/utils.R")
source("src/r/parallel_util.R")
source("src/r/util_metrics.R")

# Configuration ---------------------------------------------------------------

cat("\n====================\n")
cat("   M4-XGB Pipeline\n")
cat("====================\n\n")

source("src/r/0_common_new.R")

# Maximum size for globals passed to future workers.

options(future.globals.maxSize = 4 * 1024^3)

cat("Parameters:\n")
cat("  periods           =", paste(periods, collapse = ", "), "\n")
cat("  RUN_PARALLEL      =", RUN_PARALLEL, "\n")
cat("  FORCE_RERUN       =", FORCE_RERUN, "\n")
cat("  RUN_CLEAN         =", RUN_CLEAN, "\n")
cat("  SPLIT_STRATEGY_ID =", SPLIT_STRATEGY_ID, "\n")
cat("  SPLIT_SEED        =", SPLIT_SEED, "\n")
cat("  TRAIN_FRAC        =", TRAIN_FRAC, "\n")
cat("  DTW_WINDOW_FRAC   =", DTW_WINDOW_FRAC, "\n")

# Main execution --------------------------------------------------------------

# 01: Load M4 subset ----------------------------------------------------------

if (!file.exists(subset_file) || FORCE_RERUN) {
  cat("\n[01] Loading M4 subset...\n")
  source("src/r/01_load_m4_subset.R")
} else {
  cat("\n[01] Skipped (already exists): ", subset_file, "\n", sep = "")
}
cleanup_step()

# 02: Clean M4 ----------------------------------------------------------------

set_parallel_plan(FALSE)

if (!file.exists(subset_clean_file) || FORCE_RERUN) {
  cat("\n[02] Cleaning M4 dataset...\n")
  source("src/r/02_clean_m4.R")
} else {
  cat("\n[02] Skipped (already exists): ", subset_clean_file, "\n", sep = "")
}

cleanup_step()
set_parallel_plan(FALSE)

# 03: Create rolling windows --------------------------------------------------

set_parallel_plan(FALSE)

if (FORCE_RERUN || any(!file.exists(windows_raw_files))) {
  cat("\n[03] Creating rolling windows...\n")
  source("src/r/03_rolling_windows.R")
} else {
  cat("\n[03] Skipped (all rolling window files exist)\n")
}

cleanup_step()

# 04: Apply transformations ---------------------------------------------------

set_parallel_plan(TRUE)

if (FORCE_RERUN || any(!file.exists(windows_std_files))) {
  cat("\n[04] Applying transformations...\n")
  source("src/r/04_transformations.R")
} else {
  cat("\n[04] Skipped (all transformed files exist)\n")
}

cleanup_step()
set_parallel_plan(FALSE)

# 06: Create directional labels -----------------------------------------------

set_parallel_plan(TRUE)

if (FORCE_RERUN || any(!file.exists(labeled_files))) {
  cat("\n[06] Creating labels...\n")
  source("src/r/06_labels.R")
} else {
  cat("\n[06] Skipped (all labeled files exist)\n")
}

cleanup_step()
set_parallel_plan(FALSE)

# 07: Compute time-series features --------------------------------------------

set_parallel_plan(FALSE)

if (FORCE_RERUN || any(!file.exists(features_files))) {
  cat("\n[07] Computing tsfeatures...\n")
  source("src/r/07a_ts_features.R")
} else {
  cat("\n[07] Skipped (all feature files exist)\n")
}

cleanup_step()
set_parallel_plan(FALSE)

# 08: Create split indices ----------------------------------------------------

set_parallel_plan(FALSE)

if (FORCE_RERUN || any(!file.exists(split_index_files))) {
  cat("\n[08] Creating split indices...\n")
  source("src/r/08_split_index.R")
} else {
  cat("\n[08] Skipped (all split index files exist)\n")
}

cleanup_step()

# 09: Train XGBoost by horizon ------------------------------------------------

cat("\n[09] Training XGBoost models...\n")
source("src/r/09_hyper_xgb_new.R")
cleanup_step()

# 10: Evaluate XGBoost --------------------------------------------------------

cat("\n[10] Evaluating XGBoost...\n")
set_parallel_plan(FALSE)
set_parallel_plan(TRUE)
source("src/r/10a_build_real_eval_parallel.R")
set_parallel_plan(FALSE)
source("src/r/10b_eval_xgb2_from_real_rds.R")
cleanup_step()
set_parallel_plan(FALSE)

# 12: Evaluate SMYL and FFORMA ------------------------------------------------

cat("\n[12] Evaluating Smyl and FFORMA baselines...\n")
source("src/r/12_baseline_fforma_smyl.R")
cleanup_step()

# 13: Evaluate DTW ------------------------------------------------------------

cat("\n[13] Evaluating DTW baseline...\n")
set_parallel_plan(RUN_PARALLEL)
source("src/r/13_baseline_dtw_real.R")
cleanup_step()
set_parallel_plan(FALSE)

# 15: Evaluate Euclidean distance ---------------------------------------------

cat("\n[15] Evaluating Euclidean baseline...\n")
source("src/r/15_baseline_euclidean.R")
cleanup_step()

# Completion ------------------------------------------------------------------

cat("\n=====================\n")
cat("  PIPELINE COMPLETE\n")
cat("=====================\n")
