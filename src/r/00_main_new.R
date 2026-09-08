# =====================================================================
# 00_main_new.R
# Master script for the new pipeline
# =====================================================================

library(future)
library(future.apply)
library(forecast)
library(M4comp2018)

source("src/r/utils.R")
source("src/r/parallel_util.R")
source("src/r/util_metrics.R")

cat("\n====================\n")
cat("   M4-XGB Pipeline\n")
cat("====================\n\n")

# ---------------------------------------------------------------------
# Load shared configuration
# ---------------------------------------------------------------------

source("src/r/0_common_new.R")

# ---------------------------------------------------------------------
# Global future configuration
# ---------------------------------------------------------------------


options(future.globals.maxSize = 4 * 1024^4)

cat("Parameters:\n")
cat("  periods           =", paste(periods, collapse = ", "), "\n")
cat("  RUN_PARALLEL      =", RUN_PARALLEL, "\n")
cat("  FORCE_RERUN       =", FORCE_RERUN, "\n")
cat("  SYNTHETIC         =", SYNTHETIC, "\n")
cat("  RUN_CLEAN         =", RUN_CLEAN, "\n")
cat("  SPLIT_STRATEGY_ID =", SPLIT_STRATEGY_ID, "\n")
cat("  SPLIT_SEED        =", SPLIT_SEED, "\n")
cat("  TRAIN_FRAC        =", TRAIN_FRAC, "\n")
cat("  DTW_WINDOW_FRAC   =", DTW_WINDOW_FRAC, "\n")

# ---------------------------------------------------------------------
# 01: Load M4 subset
# ---------------------------------------------------------------------


if (!file.exists(subset_file) || FORCE_RERUN) {
  cat("\n[01] Loading M4 subset...\n")
  source("src/r/01_load_m4_subset.R")
} else {
  cat("\n[01] Skipped (already exists): ", subset_file, "\n", sep = "")
}
cleanup_step()

# ---------------------------------------------------------------------
# 02: Clean M4
# ---------------------------------------------------------------------

set_parallel_plan(TRUE)
if (!file.exists(subset_clean_file) || FORCE_RERUN) {
  cat("\n[02] Cleaning M4 dataset...\n")
  source("src/r/02_clean_m4.R")
} else {
  cat("\n[02] Skipped (already exists): ", subset_clean_file, "\n", sep = "")
}
cleanup_step()
set_parallel_plan(FALSE)

# ---------------------------------------------------------------------
# 03: Rolling windows
# ---------------------------------------------------------------------
set_parallel_plan(FALSE)
if (FORCE_RERUN || any(!file.exists(windows_raw_files))) {
  cat("\n[03] Creating rolling windows...\n")
  source("src/r/03_rolling_windows.R")
} else {
  cat("\n[03] Skipped (all rolling window files exist)\n")
}
cleanup_step()

# ---------------------------------------------------------------------
# 04: Transformations
# ---------------------------------------------------------------------
set_parallel_plan(TRUE)
if (FORCE_RERUN || any(!file.exists(windows_std_files))) {
  cat("\n[04] Applying transformations...\n")
  source("src/r/04_transformations.R")
} else {
  cat("\n[04] Skipped (all transformed files exist)\n")
}
cleanup_step()
set_parallel_plan(FALSE)

# ---------------------------------------------------------------------
# 06: Labels
# ---------------------------------------------------------------------
set_parallel_plan(TRUE)
if (FORCE_RERUN || any(!file.exists(labeled_files))) {
  cat("\n[06] Creating labels...\n")
  source("src/r/06_labels.R")
} else {
  cat("\n[06] Skipped (all labeled files exist)\n")
}
cleanup_step()
set_parallel_plan(FALSE)
# ---------------------------------------------------------------------
# 07: TS features
# ---------------------------------------------------------------------

#set_parallel_plan(RUN_PARALLEL)
set_parallel_plan(TRUE)
if (FORCE_RERUN || any(!file.exists(features_files))) {
  cat("\n[07] Computing tsfeatures...\n")
  source("src/r/07a_ts_features.R")
} else {
  cat("\n[07] Skipped (all feature files exist)\n")
}
cleanup_step()
set_parallel_plan(FALSE)

# ---------------------------------------------------------------------
# 08: Split index
# ---------------------------------------------------------------------

set_parallel_plan(FALSE)
if (FORCE_RERUN || any(!file.exists(split_index_files))) {
  cat("\n[08] Creating split indices...\n")
  source("src/r/08_split_index.R")
} else {
  cat("\n[08] Skipped (all split index files exist)\n")
}
cleanup_step()

# ---------------------------------------------------------------------
# 09: XGBoost per horizon
# ---------------------------------------------------------------------
#set_parallel_plan(RUN_PARALLEL)
cat("\n[09] Training XGBoost models...\n")
source("src/r/09_hyper_xgb_new.R")
cleanup_step()

# ---------------------------------------------------------------------
# 10: XGBoost evaluation
# ---------------------------------------------------------------------

cat("\n[10] Evaluating XGBoost...\n")
set_parallel_plan(FALSE)
set_parallel_plan(TRUE)
source("src/r/10a_build_real_eval_parallel.R")
set_parallel_plan(FALSE)
source("src/r/10b_eval_xgb2_from_real_rds.R")
cleanup_step()
set_parallel_plan(FALSE)

# ---------------------------------------------------------------------
# 12: Baseline Smyl and FFORMA
# ---------------------------------------------------------------------

cat("\n[12] Evaluating Smyl and FFORMA baselines...\n")
source("src/r/12_baseline_fforma_smyl.R")
cleanup_step()

# ---------------------------------------------------------------------
# 13: Baseline DTW REAL
# ---------------------------------------------------------------------

cat("\n[13] Evaluating DTW baseline...\n")
source("src/r/13_baseline_dtw_real.R")
cleanup_step()

# ---------------------------------------------------------------------
# 15: Baseline Euclidean REAL
# ---------------------------------------------------------------------

cat("\n[15] Evaluating Euclidean baseline...\n")
source("src/r/15_baseline_euclidean.R")
cleanup_step()

# ---------------------------------------------------------------------
# Done
# ---------------------------------------------------------------------

cat("\n=====================\n")
cat("  PIPELINE COMPLETE\n")
cat("=====================\n")