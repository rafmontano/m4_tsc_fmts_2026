# ==============================================================================
# 02_clean_m4.R
#
# Purpose: Clean the historical values in each M4 series while retaining the
#          future values unchanged.
# Inputs:  Global subset_file, subset_clean_file, data_dir, RUN_CLEAN, and
#          RUN_PARALLEL values; shared parallel and frequency helpers.
# Outputs: The cleaned RDS file specified by subset_clean_file.
# Run from: Repository root; sourced by src/r/00_main_new.R.
# ==============================================================================

# Load dataset ----------------------------------------------------------------

M4_subset <- readRDS(subset_file)
cat("Loaded dataset with", length(M4_subset), "series.\n")

# Helper functions ------------------------------------------------------------

clean_one_series <- function(s) {
  if (!RUN_CLEAN) {
    return(s)
  }

  freq <- infer_frequency(s$period)
  x_ts <- ts(as.numeric(s$x), frequency = freq)
  s$x <- as.numeric(forecast::tsclean(x_ts))
  s
}

# Clean dataset ---------------------------------------------------------------

if (!RUN_CLEAN) {
  M4_subset_clean <- M4_subset
  cat("RUN_CLEAN = FALSE. Dataset left unchanged.\n")
} else if (RUN_PARALLEL) {
  cat("Cleaning dataset in parallel...\n")

  cache_dir_clean <- file.path(
    data_dir,
    paste0("cache_clean")
  )

  M4_subset_clean <- run_step_parallel(
    dataset = M4_subset,
    step_fun = clean_one_series,
    chunk_size = NULL,
    save_foldername = cache_dir_clean,
    step_name = "clean_m4"
  )
} else {
  cat("Cleaning dataset sequentially...\n")

  M4_subset_clean <- lapply(M4_subset, clean_one_series)
}

cat("Processed", length(M4_subset_clean), "series.\n")

# Save dataset ----------------------------------------------------------------

saveRDS(M4_subset_clean, file = subset_clean_file)
cat("Saved cleaned dataset to:", subset_clean_file, "\n")
