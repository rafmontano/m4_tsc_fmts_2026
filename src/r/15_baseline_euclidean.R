# =====================================================================
# 15_baseline_euclidean.R
# Euclidean 1-NN baseline on REAL evaluation data
#
# Design:
#   - REAL-only evaluation
#   - no CSVs
#   - no LABEL_ID
#   - no model saving
#   - uses canonical split_index
#   - uses canonical REAL dataset from Script 10a
#   - saves one consolidated RDS per frequency under:
#       small / default / large
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(RANN)
})

source("src/r/utils.R")
source("src/r/util_metrics.R")

results_euclidean_dir <- file.path(results_dir, "euclidean")
if (!dir.exists(results_euclidean_dir)) dir.create(results_euclidean_dir, recursive = TRUE)

# ---------------------------------------------------------------------
# Helper: pad or truncate list of numeric series to a common width
# ---------------------------------------------------------------------

pad_series_matrix_left <- function(series_list, target_len = NULL) {
  series_list <- lapply(series_list, function(v) as.numeric(unlist(v, use.names = FALSE)))
  
  if (is.null(target_len)) {
    target_len <- max(vapply(series_list, length, integer(1)))
  }
  
  out <- t(vapply(series_list, function(v) {
    n <- length(v)
    
    if (n == target_len) {
      return(v)
    }
    
    if (n > target_len) {
      return(v[(n - target_len + 1L):n])
    }
    
    c(rep(0, target_len - n), v)
  }, numeric(target_len)))
  
  out[!is.finite(out)] <- NA_real_
  out
}

# ---------------------------------------------------------------------
# Main loop by frequency
# ---------------------------------------------------------------------

for (period_i in periods) {
  
  TAG_i <- freq_tag(period_i)
  eval_i <- file.path(results_euclidean_dir, paste0("euclidean_eval_", TAG_i, ".rds"))
  
  for (window_mode_i in window_modes) {
    
    WINDOW_TAG_i <- window_tag(window_mode_i)
    
    labeled_i <- file.path(
      data_dir,
      paste0("all_windows_labeled_", TAG_i, "_", WINDOW_TAG_i, ".rds")
    )
    
    split_i <- file.path(
      data_dir,
      paste0("split_index_", TAG_i, "_", WINDOW_TAG_i, ".rds")
    )
    
    real_eval_i <- file.path(
      data_dir,
      paste0("real_eval_with_features_", TAG_i, "_", WINDOW_TAG_i, ".rds")
    )
    
    if (!file.exists(labeled_i)) {
      cat("[15] Skip", period_i, window_mode_i, "- missing labeled file:", labeled_i, "\n")
      next
    }
    
    if (!file.exists(split_i)) {
      cat("[15] Skip", period_i, window_mode_i, "- missing split file:", split_i, "\n")
      next
    }
    
    if (!file.exists(real_eval_i)) {
      cat("[15] Skip", period_i, window_mode_i, "- missing REAL dataset:", real_eval_i, "\n")
      next
    }
    
    df <- readRDS(labeled_i)
    split_index <- readRDS(split_i)
    real_df <- readRDS(real_eval_i)
    
    split_use <- get_model_split(split_index, model_id = "euclidean")
    
    train_idx <- split_use$train
    H_i <- length(df$labels[[1]])
    
    cat("\n[15] Period =", period_i,
        "| window_mode =", window_mode_i,
        "| train =", length(train_idx),
        "| real =", nrow(real_df),
        "| horizons =", H_i, "\n")
    
    train_df <- df[train_idx, , drop = FALSE]
    
    summary_rows <- vector("list", H_i)
    detail_real  <- vector("list", H_i)
    
    for (h in seq_len(H_i)) {
      
      y_train <- vapply(train_df$labels, function(v) as.integer(v[h]), integer(1))
      y_real  <- vapply(real_df$labels,  function(v) as.integer(v[h]), integer(1))
      
      if (length(unique(y_train)) < 2L) {
        summary_rows[[h]] <- make_empty_eval_summary_row(
          period_i  = period_i,
          tag_i     = TAG_i,
          eval_type = "real",
          horizon_i = h,
          n_rows    = nrow(real_df),
          status    = "train_one_class"
        )
        summary_rows[[h]]$method <- "euclidean"
        detail_real[[h]] <- NULL
        next
      }
      
      target_len <- max(
        max(vapply(train_df$x, length, integer(1))),
        max(vapply(real_df$x,  length, integer(1)))
      )
      
      X_train <- pad_series_matrix_left(train_df$x, target_len = target_len)
      X_real  <- pad_series_matrix_left(real_df$x,  target_len = target_len)
      
      nn <- RANN::nn2(
        data  = X_train,
        query = X_real,
        k     = 1
      )
      
      pred_real <- as.integer(y_train[nn$nn.idx[, 1]])
      
      res_real <- compute_binary_eval(
        y_true     = y_real,
        pred_class = pred_real,
        period_i   = period_i,
        tag_i      = TAG_i,
        horizon_i  = h,
        eval_type  = "real"
      )
      
      res_real$summary$method <- "euclidean"
      
      summary_rows[[h]] <- res_real$summary
      detail_real[[h]] <- c(
        res_real$detail,
        list(series_name = real_df$series_name)
      )
      
      cat("[15] Period:", period_i,
          "| mode:", window_mode_i,
          "| horizon:", h,
          "| real acc:", round(res_real$summary$accuracy, 4), "\n")
      
      rm(y_train, y_real, target_len, X_train, X_real, nn, pred_real, res_real)
      gc()
    }
    
    euclidean_eval <- list(
      real = list(
        summary = bind_rows(summary_rows),
        detail  = detail_real,
        dataset = real_df
      )
    )
    
    save_consolidated_eval_object(euclidean_eval, eval_i, window_mode_i)
    cat("[15] Saved:", eval_i, "| mode:", window_mode_i, "\n")
    
    rm(df, split_index, split_use, train_idx, train_df,
       real_df, H_i, summary_rows, detail_real, euclidean_eval)
    gc()
  }
}