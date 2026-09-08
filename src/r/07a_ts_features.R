# =====================================================================
# 07a_ts_features.R
# Extract features for each window, by frequency and window mode
#
# Same input/output contract as 07_ts_features.R
# Difference:
#   - builds a lean working object for feature calculation
#   - releases the full input before parallel work
#   - re-reads the original input only for final recombination
#
# Supports:
#   - FEATURE_ENGINE = "fforma"
#   - FEATURE_ENGINE = "da"
# =====================================================================

source("src/r/features.R")
source("src/r/forecast_methods3.R")

for (period_i in periods) {
  
  TAG_i <- freq_tag(period_i)
  
  for (window_mode_i in window_modes) {
    
    WINDOW_TAG_i <- window_tag(window_mode_i)
    
    labeled_i <- file.path(
      data_dir,
      paste0("all_windows_labeled_", TAG_i, "_", WINDOW_TAG_i, ".rds")
    )
    
    features_i <- file.path(
      data_dir,
      paste0("all_windows_with_features_", TAG_i, "_", WINDOW_TAG_i, ".rds")
    )
    
    features_cache_dir <- file.path(
      data_dir,
      paste0("cache_features_", FEATURE_ENGINE, "_", TAG_i, "_", WINDOW_TAG_i)
    )
    
    if (!file.exists(labeled_i)) {
      cat("[07a] Skip", period_i, window_mode_i, "- missing input:", labeled_i, "\n")
      next
    }
    
    # ---------------------------------------------------------------
    # Read full input once, derive lean working object, then release
    # ---------------------------------------------------------------
    
    all_windows_labeled <- readRDS(labeled_i)
    n_windows <- nrow(all_windows_labeled)
    
    cat(
      "\n[07a] Period =", period_i,
      "| window_mode =", window_mode_i,
      "| windows =", n_windows,
      "| feature_engine =", FEATURE_ENGINE, "\n"
    )
    
    x_list  <- all_windows_labeled$x
    xx_list <- all_windows_labeled$xx
    
    working_dataset <- Map(
      function(x, xx) list(x = x, xx = xx),
      x_list,
      xx_list
    )
    
    rm(all_windows_labeled, x_list, xx_list)
    gc()
    
    # ---------------------------------------------------------------
    # Compute features on lean working object
    # ---------------------------------------------------------------
    
    compute_one_feature_row <- function(obj) {
      x <- obj$x
      
      if (FEATURE_ENGINE == "fforma") {
        
        calc_features(list(x = x))$features
        
      } else if (FEATURE_ENGINE == "da") {
        
        xx <- obj$xx
        
        calc_features_with_da(
          seriesentry = list(x = x, xx = xx),
          period = period_i
        )$features
        
      } else {
        stop("Unknown FEATURE_ENGINE: ", FEATURE_ENGINE)
      }
    }
    
    if (RUN_PARALLEL) {
      set_parallel_plan(FALSE)
      set_parallel_plan(TRUE)
      
      feature_rows <- run_step_parallel(
        dataset = working_dataset,
        step_fun = compute_one_feature_row,
        chunk_size = 5000L,
        save_foldername = features_cache_dir,
        step_name = paste0("features_", FEATURE_ENGINE, "_", TAG_i, "_", WINDOW_TAG_i)
      )
      
    } else {
      
      feature_rows <- lapply(working_dataset, compute_one_feature_row)
    }
    
    rm(working_dataset)
    gc()
    
    # ---------------------------------------------------------------
    # Assemble feature matrix
    # ---------------------------------------------------------------
    
    feature_matrix <- dplyr::bind_rows(feature_rows)
    
    if (nrow(feature_matrix) != n_windows) {
      stop("Feature rows mismatch: got ", nrow(feature_matrix), " expected ", n_windows)
    }
    
    rm(feature_rows)
    gc()
    
    # ---------------------------------------------------------------
    # Re-read full input and recombine to preserve same final artifact
    # ---------------------------------------------------------------
    
    all_windows_labeled <- readRDS(labeled_i)
    
    all_windows_with_features <- dplyr::bind_cols(
      all_windows_labeled,
      feature_matrix
    )
    
    cat(
      "Final dataset:",
      nrow(all_windows_with_features),
      "rows x",
      ncol(all_windows_with_features),
      "cols\n"
    )
    
    saveRDS(all_windows_with_features, features_i)
    cat("Saved →", features_i, "\n")
    
    rm(
      all_windows_labeled,
      feature_matrix,
      all_windows_with_features
    )
    gc()
  }
}