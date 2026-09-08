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
FORCE_RERUN <- FALSE
RUN_CLEAN <- TRUE
USE_TEST_SUBSET <- TRUE
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
    "periods",
    "period_tags",
    "RUN_PARALLEL",
    "FORCE_RERUN",
    "SPLIT_STRATEGY_ID",
    "SPLIT_SEED",
    "TRAIN_FRAC",
    "DTW_WINDOW_FRAC",
    "RUN_CLEAN",
    "FEATURE_ENGINE",
    "WINDOW_MODE",
    "window_modes",
    "window_tags",
    "data_dir",
    "models_dir",
    "results_dir",
    "models_xgb_dir",
    "results_xgb_dir",
    "results_smyl_dir",
    "results_fforma_dir",
    "results_dtw_dir",
    "results_euclidean_dir",
    "subset_file",
    "subset_clean_file",
    "windows_raw_files",
    "windows_std_files",
    "labeled_files",
    "features_files",
    "split_index_files",
    "real_eval_files",
    "xgb_eval_files",
    "smyl_eval_files",
    "fforma_eval_files",
    "dtw_eval_files",
    "euclidean_eval_files",
    "CAP_POLICY",
    "cleanup_step",
    "infer_frequency",
    "get_m4_horizon",
    "get_window_size_from_h",
    "window_tag",
    "minmax_vec",
    "standardise_vec",
    "scale_pair_minmax_std",
    "freq_tag",
    "period_to_freq",
    "compute_c",
    "label_from_z_int",
    "compute_z",
    "compute_z_generic",
    "compute_z_all",
    "scale_pair_std",
    "compute_label_vector",
    "autodetect_num_workers",
    "calculate_chunk_size",
    "chunk_xapply",
    "run_step_parallel",
    "set_parallel_plan",
    "get_model_split",
    "make_empty_eval_summary_row",
    "compute_binary_eval",
    "get_predictor_cols_base",
    "get_predictor_cols_train_safe",
    "build_real_eval_dataset",
    "init_eval_containers",
    "store_test_eval_result",
    "store_real_eval_result",
    "build_consolidated_eval_object",
    "save_consolidated_eval_object",
    "calc_features",
    "RUN_CLEAN",
    "USE_TEST_SUBSET",
    "TEST_SERIES_PER_PERIOD",
    "FEATURE_ENGINE",
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
