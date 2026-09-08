# =====================================================================
# 02_clean_m4.R
# Clean only the training part (x) of the M4 dataset using forecast::tsclean()
# Leaves future part (xx) unchanged.
#
# Inputs (globals): subset_file, subset_clean_file, RUN_CLEAN, RUN_PARALLEL
# Requires: run_step_parallel() from parallel_util.R
# Output: subset_clean_file (RDS)
# =====================================================================

# ---------------------------------------------------------------------
# 1. Load dataset created by 01_load_m4_subset.R
# ---------------------------------------------------------------------

M4_subset <- readRDS(subset_file)
cat("Loaded dataset with", length(M4_subset), "series.\n")

# ---------------------------------------------------------------------
# 2. Define one-series cleaning function
# ---------------------------------------------------------------------

clean_one_series <- function(s) {
  if (!RUN_CLEAN) {
    return(s)
  }
  
  freq <- infer_frequency(s$period)
  x_ts <- ts(as.numeric(s$x), frequency = freq)
  s$x <- as.numeric(forecast::tsclean(x_ts))
  s
}

# ---------------------------------------------------------------------
# 3. Clean dataset
# ---------------------------------------------------------------------

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

# ---------------------------------------------------------------------
# 4. Save cleaned dataset
# ---------------------------------------------------------------------

saveRDS(M4_subset_clean, file = subset_clean_file)
cat("Saved cleaned dataset to:", subset_clean_file, "\n")