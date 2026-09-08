# =====================================================================
# 08_split_index.R
# Create one split-index object per frequency and window mode
#
# Inputs (globals):
#   data_dir, periods, SPLIT_STRATEGY_ID, SPLIT_SEED, TRAIN_FRAC, CAP_POLICY
#
# Output: one RDS per frequency + window mode:
#   data/split_index_y_s.rds
#   data/split_index_y_d.rds
#   data/split_index_y_l.rds
#   etc.
#
# Notes:
#   - uncap$train / uncap$test always exist
#   - capped models get their own train/test indices
#   - for S2, stratification uses labels[[1]][1] as a simple proxy
# =====================================================================

# ---------------------------------------------------------------------
# Helper: cap training indices stratified on horizon 1 label
# ---------------------------------------------------------------------

cap_train_indices_stratified <- function(df, train_idx, max_rows, seed) {
  if (is.null(max_rows) || !is.numeric(max_rows) || max_rows <= 0) {
    return(train_idx)
  }
  
  if (length(train_idx) <= max_rows) {
    return(train_idx)
  }
  
  set.seed(seed)
  
  y <- vapply(df$labels[train_idx], function(v) as.integer(v[1]), integer(1))
  levs <- sort(unique(y))
  counts <- as.integer(table(factor(y, levels = levs)))
  
  quotas <- floor((counts / sum(counts)) * max_rows)
  quotas <- pmax(quotas, 1L)
  
  diff <- max_rows - sum(quotas)
  if (diff != 0L) {
    order_idx <- order(counts, decreasing = TRUE)
    i <- 1L
    while (diff != 0L) {
      k <- order_idx[((i - 1L) %% length(order_idx)) + 1L]
      if (diff > 0L) {
        quotas[k] <- quotas[k] + 1L
        diff <- diff - 1L
      } else {
        if (quotas[k] > 1L) {
          quotas[k] <- quotas[k] - 1L
          diff <- diff + 1L
        }
      }
      i <- i + 1L
    }
  }
  
  idx_keep <- integer(0)
  
  for (i in seq_along(levs)) {
    cls <- levs[i]
    q <- quotas[i]
    cls_idx_local <- which(y == cls)
    
    if (length(cls_idx_local) <= q) {
      idx_keep <- c(idx_keep, cls_idx_local)
    } else {
      idx_keep <- c(idx_keep, sample(cls_idx_local, size = q, replace = FALSE))
    }
  }
  
  idx_keep <- sample(idx_keep, length(idx_keep))
  train_idx[idx_keep]
}

# ---------------------------------------------------------------------
# Main loop by frequency
# ---------------------------------------------------------------------

for (period_i in periods) {
  
  TAG_i <- freq_tag(period_i)
  
  for (window_mode_i in window_modes) {
    
    WINDOW_TAG_i <- window_tag(window_mode_i)
    
    windows_labeled_i <- file.path(
      data_dir,
      paste0("all_windows_labeled_", TAG_i, "_", WINDOW_TAG_i, ".rds")
    )
    
    split_index_i <- file.path(
      data_dir,
      paste0("split_index_", TAG_i, "_", WINDOW_TAG_i, ".rds")
    )
    
    if (!file.exists(windows_labeled_i)) {
      cat("[08] Skip", period_i, window_mode_i, "- missing input:", windows_labeled_i, "\n")
      next
    }
    
    df <- readRDS(windows_labeled_i)
    
    cat(
      "\n[08] Period =", period_i,
      "| window_mode =", window_mode_i,
      "| loaded", nrow(df),
      "rows from", windows_labeled_i, "\n"
    )
    
    if (nrow(df) == 0L) {
      cat("[08] Skip", period_i, window_mode_i, "- empty dataset\n")
      rm(df)
      gc()
      next
    }
    
    set.seed(SPLIT_SEED)
    
    # -----------------------------------------------------------------
    # Canonical split
    # -----------------------------------------------------------------
    
    if (SPLIT_STRATEGY_ID %in% c("S1", "S3")) {
      
      if (!("series_id" %in% names(df))) {
        stop("[08] ", SPLIT_STRATEGY_ID, " requires column 'series_id'.")
      }
      
      ids <- sort(unique(df$series_id))
      n_test_ids <- max(1L, floor((1 - TRAIN_FRAC) * length(ids)))
      test_ids <- sample(ids, size = n_test_ids, replace = FALSE)
      
      train_idx <- which(!(df$series_id %in% test_ids))
      test_idx  <- which(df$series_id %in% test_ids)
      
    } else if (SPLIT_STRATEGY_ID == "S2") {
      
      y_split <- vapply(df$labels, function(v) as.integer(v[1]), integer(1))
      
      train_idx <- caret::createDataPartition(
        y = y_split,
        p = TRAIN_FRAC,
        list = FALSE
      )
      
      train_idx <- as.integer(train_idx[, 1])
      test_idx  <- setdiff(seq_len(nrow(df)), train_idx)
      
    } else {
      stop("[08] Unknown SPLIT_STRATEGY_ID: ", SPLIT_STRATEGY_ID)
    }
    
    cat("[08] Uncapped split | train rows:", length(train_idx), "| test rows:", length(test_idx), "\n")
    
    # -----------------------------------------------------------------
    # Build split object
    # -----------------------------------------------------------------
    
    split_index <- list()
    
    split_index$uncap <- list(
      train = train_idx,
      test  = test_idx
    )
    
    # -----------------------------------------------------------------
    # Optional capped variants
    # -----------------------------------------------------------------
    
    cap_models <- names(CAP_POLICY)[!vapply(CAP_POLICY, is.null, logical(1))]
    
    if (length(cap_models) > 0L) {
      for (model_id in cap_models) {
        
        max_train <- CAP_POLICY[[model_id]]
        
        train_cap_idx <- cap_train_indices_stratified(
          df        = df,
          train_idx = train_idx,
          max_rows  = max_train,
          seed      = SPLIT_SEED
        )
        
        split_index[[model_id]] <- list(
          train = train_cap_idx,
          test  = test_idx
        )
        
        cat("[08] Cap for", model_id,
            "| train:", length(train_cap_idx),
            "| test:", length(test_idx), "\n")
      }
    }
    
    # -----------------------------------------------------------------
    # Save
    # -----------------------------------------------------------------
    
    saveRDS(split_index, split_index_i)
    cat("[08] Saved split index to:", split_index_i, "\n")
    
    rm(df, train_idx, test_idx, split_index, cap_models)
    gc()
  }
}