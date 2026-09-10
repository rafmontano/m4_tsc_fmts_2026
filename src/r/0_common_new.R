# ==============================================================================
# 0_common_new.R
#
# Purpose: Define shared configuration, artifact paths, and cleanup behaviour.
# Inputs:  Shared utility functions, including freq_tag().
# Outputs: Pipeline configuration, artifact paths, and cleanup_step().
# Run from: Repository root; sourced after src/r/utils.R.
# ==============================================================================

# Configuration ---------------------------------------------------------------

# Frequencies -----------------------------------------------------------------

periods <- c("Weekly", "Hourly", "Daily", "Yearly", "Quarterly", "Monthly")

# M4 series counts: Weekly 359; Hourly 414; Daily 4,227; Yearly 23,000;
# Quarterly 24,000; Monthly 48,000.

# Run controls ----------------------------------------------------------------

RUN_PARALLEL <- TRUE
FORCE_RERUN <-  FALSE
RUN_CLEAN <- FALSE
USE_TEST_SUBSET <- FALSE
TEST_SERIES_PER_PERIOD <- 10L

FEATURE_ENGINE <- "da"

WINDOW_MODE <- "default"
# window_modes <- c("small", "default", "large", "full")
# window_tags  <- c("s", "d", "l", "f")
window_modes <- "default"
window_tags <- "d"

# Data split ------------------------------------------------------------------

SPLIT_STRATEGY_ID <- "S1"
SPLIT_SEED <- 123
TRAIN_FRAC <- 0.80

# DTW -------------------------------------------------------------------------

DTW_WINDOW_FRAC <- 0.10

# Directories -----------------------------------------------------------------

data_dir <- "data"
models_dir <- "models"
results_dir <- "results"

models_xgb_dir <- file.path(models_dir, "xgb")

results_xgb_dir <- file.path(results_dir, "xgb")
results_smyl_dir <- file.path(results_dir, "smyl")
results_fforma_dir <- file.path(results_dir, "fforma")
results_dtw_dir <- file.path(results_dir, "dtw")
results_euclidean_dir <- file.path(results_dir, "euclidean")

# Artifact paths --------------------------------------------------------------

period_tags <- vapply(periods, freq_tag, character(1))

# Window-dependent files use <base>_<period_tag>_<window_tag>.rds.
# Evaluation files remain grouped by frequency.

subset_file <- file.path(data_dir, "M4_subset.rds")
subset_clean_file <- file.path(data_dir, "M4_subset_clean.rds")

windows_raw_files <- as.vector(outer(
  period_tags,
  window_tags,
  FUN = function(pt, wt) {
    file.path(
      data_dir,
      paste0("all_windows_raw_", pt, "_", wt, ".rds")
    )
  }
))

windows_std_files <- as.vector(outer(
  period_tags,
  window_tags,
  FUN = function(pt, wt) {
    file.path(
      data_dir,
      paste0("all_windows_std_", pt, "_", wt, ".rds")
    )
  }
))

labeled_files <- as.vector(outer(
  period_tags,
  window_tags,
  FUN = function(pt, wt) {
    file.path(
      data_dir,
      paste0("all_windows_labeled_", pt, "_", wt, ".rds")
    )
  }
))

features_files <- as.vector(outer(
  period_tags,
  window_tags,
  FUN = function(pt, wt) {
    file.path(
      data_dir,
      paste0("all_windows_with_features_", pt, "_", wt, ".rds")
    )
  }
))

split_index_files <- as.vector(outer(
  period_tags,
  window_tags,
  FUN = function(pt, wt) {
    file.path(
      data_dir,
      paste0("split_index_", pt, "_", wt, ".rds")
    )
  }
))

real_eval_files <- as.vector(outer(
  period_tags,
  window_tags,
  FUN = function(pt, wt) {
    file.path(
      data_dir,
      paste0("real_eval_with_features_", pt, "_", wt, ".rds")
    )
  }
))

xgb_eval_files <- file.path(
  results_xgb_dir,
  paste0("xgb_eval_", period_tags, ".rds")
)

smyl_eval_files <- file.path(
  results_smyl_dir,
  paste0("smyl_eval_", period_tags, ".rds")
)

fforma_eval_files <- file.path(
  results_fforma_dir,
  paste0("fforma_eval_", period_tags, ".rds")
)

dtw_eval_files <- file.path(
  results_dtw_dir,
  paste0("dtw_eval_", period_tags, ".rds")
)

euclidean_eval_files <- file.path(
  results_euclidean_dir,
  paste0("euclidean_eval_", period_tags, ".rds")
)

# Training caps ---------------------------------------------------------------

# NULL means no cap; a numeric value sets the maximum number of training rows.

CAP_POLICY <- list(
  xgboost       = NULL,
  euclidean     = NULL,
  rocket        = NULL,
  rotf          = NULL,
  inceptiontime = 200000,
  dtw           = 10000,
  hivecotev2    = 10000
)

# Helper functions ------------------------------------------------------------

# Remove non-function objects created by a pipeline step while retaining shared
# configuration and paths.

cleanup_step <- function() {
  keep_explicit <- c(
    "autodetect_num_workers",
    "build_consolidated_eval_object",
    "build_real_eval_dataset",
    "calculate_chunk_size",
    "calc_features",
    "CAP_POLICY",
    "chunk_xapply",
    "cleanup_step",
    "compute_binary_eval",
    "compute_c",
    "compute_label_vector",
    "compute_z",
    "compute_z_all",
    "compute_z_generic",
    "data_dir",
    "DTW_WINDOW_FRAC",
    "dtw_eval_files",
    "euclidean_eval_files",
    "FEATURE_ENGINE",
    "features_files",
    "fforma_eval_files",
    "FORCE_RERUN",
    "freq_tag",
    "get_m4_horizon",
    "get_model_split",
    "get_predictor_cols_base",
    "get_predictor_cols_train_safe",
    "get_window_size_from_h",
    "infer_frequency",
    "init_eval_containers",
    "label_from_z_int",
    "labeled_files",
    "make_empty_eval_summary_row",
    "minmax_vec",
    "models_dir",
    "models_xgb_dir",
    "period_to_freq",
    "period_tags",
    "periods",
    "real_eval_files",
    "results_dir",
    "results_dtw_dir",
    "results_euclidean_dir",
    "results_fforma_dir",
    "results_smyl_dir",
    "results_xgb_dir",
    "RUN_CLEAN",
    "RUN_PARALLEL",
    "run_step_parallel",
    "save_consolidated_eval_object",
    "scale_pair_minmax_std",
    "scale_pair_std",
    "set_parallel_plan",
    "smyl_eval_files",
    "SPLIT_SEED",
    "SPLIT_STRATEGY_ID",
    "split_index_files",
    "standardise_vec",
    "store_real_eval_result",
    "store_test_eval_result",
    "subset_clean_file",
    "subset_file",
    "TEST_SERIES_PER_PERIOD",
    "TRAIN_FRAC",
    "USE_TEST_SUBSET",
    "WINDOW_MODE",
    "window_modes",
    "window_tag",
    "window_tags",
    "windows_raw_files",
    "windows_std_files",
    "xgb_eval_files"
  )

  all_objs <- ls(envir = .GlobalEnv)

  is_fun <- vapply(
    all_objs,
    function(x) is.function(get(x, envir = .GlobalEnv)),
    logical(1)
  )

  non_fun_objs <- all_objs[!is_fun]
  to_remove <- setdiff(non_fun_objs, keep_explicit)

  if (length(to_remove) > 0L) {
    rm(list = to_remove, envir = .GlobalEnv)
  }

  gc()
}
