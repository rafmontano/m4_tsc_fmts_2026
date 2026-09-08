# ==============================================================================
# 10b_eval_xgb2_from_real_rds.R
#
# Purpose:
#   Evaluate saved XGBoost models on the internal test split and real series.
# Inputs:
#   Feature, split-index, real-evaluation, and model files for each frequency
#   and window mode.
# Outputs:
#   One consolidated XGBoost evaluation RDS file per frequency.
# Run from:
#   Project root, through 00_main_new.R after scripts 09 and 10a.
# ==============================================================================

# Dependencies ----------------------------------------------------------------

library(xgboost)
library(caret)
library(dplyr)
library(tibble)

source("src/r/utils.R")
source("src/r/features.R")
source("src/r/util_metrics.R")
source("src/r/forecast_methods3.R")

# Main execution --------------------------------------------------------------

if (!file.exists(subset_clean_file)) {
  message("Missing cleaned dataset: ", subset_clean_file)
}

for (period_i in periods) {
  TAG_i <- freq_tag(period_i)
  eval_rds_i <- file.path(results_xgb_dir, paste0("xgb_eval_", TAG_i, ".rds"))

  for (window_mode_i in window_modes) {
    WINDOW_TAG_i <- window_tag(window_mode_i)

    features_i <- file.path(
      data_dir,
      paste0("all_windows_with_features_", TAG_i, "_", WINDOW_TAG_i, ".rds")
    )

    split_i <- file.path(
      data_dir,
      paste0("split_index_", TAG_i, "_", WINDOW_TAG_i, ".rds")
    )

    real_eval_file_i <- file.path(
      data_dir,
      paste0("real_eval_with_features_", TAG_i, "_", WINDOW_TAG_i, ".rds")
    )

    model_dir_i <- file.path(models_dir, "xgb", paste0(TAG_i, "_", WINDOW_TAG_i))

    if (!file.exists(features_i)) {
      cat("[10b] Skip", period_i, window_mode_i, "- missing features file:", features_i, "\n")
      next
    }

    if (!file.exists(split_i)) {
      cat("[10b] Skip", period_i, window_mode_i, "- missing split file:", split_i, "\n")
      next
    }

    if (!file.exists(real_eval_file_i)) {
      cat("[10b] Skip", period_i, window_mode_i, "- missing REAL dataset:", real_eval_file_i, "\n")
      next
    }

    if (!dir.exists(model_dir_i)) {
      cat("[10b] Skip", period_i, window_mode_i, "- missing model directory:", model_dir_i, "\n")
      next
    }

    df <- readRDS(features_i)
    split_index <- readRDS(split_i)
    real_eval_df <- readRDS(real_eval_file_i)

    split_use <- get_model_split(split_index, model_id = "xgboost")

    train_idx <- split_use$train
    test_idx <- split_use$test

    H_i <- length(df$labels[[1]])

    cat(
      "\n[10b] Period =", period_i,
      "| window_mode =", window_mode_i,
      "| rows =", nrow(df),
      "| train =", length(train_idx),
      "| test =", length(test_idx),
      "| horizons =", H_i,
      "| real rows =", nrow(real_eval_df), "\n"
    )

    predictor_cols_base <- get_predictor_cols_base(df)

    if (length(predictor_cols_base) == 0L) {
      message("[10b] No predictor columns found for period: ", period_i)
    }

    if (nrow(real_eval_df) == 0L) {
      cat("[10b] Skip", period_i, window_mode_i, "- REAL evaluation dataset is empty\n")
      rm(df, split_index, split_use, train_idx, test_idx, predictor_cols_base, H_i, real_eval_df)
      gc()
      next
    }

    containers <- init_eval_containers(H_i)

    for (h in seq_len(H_i)) {
      model_path_i <- file.path(model_dir_i, sprintf("model_h%02d.ubj", h))

      if (!file.exists(model_path_i)) {
        cat("[10b] Missing model for", period_i, window_mode_i, "horizon", h, "- skipping\n")

        containers$summary_test_rows[[h]] <-
          make_empty_eval_summary_row(period_i, TAG_i, "test", h, length(test_idx), "missing_model")

        containers$summary_real_rows[[h]] <-
          make_empty_eval_summary_row(period_i, TAG_i, "real", h, nrow(real_eval_df), "missing_model")

        next
      }

      model_xgb <- xgb.load(model_path_i)

      y_all <- vapply(df$labels, function(v) as.integer(v[h]), integer(1))

      train_df <- df[train_idx, , drop = FALSE]
      test_df <- df[test_idx, , drop = FALSE]

      predictor_cols_h <- get_predictor_cols_train_safe(train_df, predictor_cols_base)

      if (length(predictor_cols_h) == 0L) {
        cat("[10b] No usable predictors for", period_i, window_mode_i, "horizon", h, "- skipping\n")

        containers$summary_test_rows[[h]] <-
          make_empty_eval_summary_row(period_i, TAG_i, "test", h, length(test_idx), "no_predictors")

        containers$summary_real_rows[[h]] <-
          make_empty_eval_summary_row(period_i, TAG_i, "real", h, nrow(real_eval_df), "no_predictors")

        next
      }

      # Internal test evaluation

      X_test <- as.matrix(test_df[, predictor_cols_h, drop = FALSE])
      X_test[!is.finite(X_test)] <- NA_real_
      y_test <- y_all[test_idx]

      dtest <- xgb.DMatrix(X_test)
      pred_prob_test <- predict(model_xgb, dtest)
      pred_class_test <- ifelse(pred_prob_test >= 0.5, 1L, 0L)

      res_test <- compute_binary_eval(
        y_true     = y_test,
        pred_class = pred_class_test,
        period_i   = period_i,
        tag_i      = TAG_i,
        horizon_i  = h,
        eval_type  = "test"
      )

      containers <- store_test_eval_result(containers, h, res_test)

      # Real-series evaluation

      missing_real_cols <- setdiff(predictor_cols_h, names(real_eval_df))
      if (length(missing_real_cols) > 0L) {
        for (nm in missing_real_cols) real_eval_df[[nm]] <- 0
      }

      X_real <- as.matrix(real_eval_df[, predictor_cols_h, drop = FALSE])
      X_real[!is.finite(X_real)] <- NA_real_
      y_real <- vapply(real_eval_df$labels, function(v) as.integer(v[h]), integer(1))

      dreal <- xgb.DMatrix(X_real)
      pred_prob_real <- predict(model_xgb, dreal)
      pred_class_real <- ifelse(pred_prob_real >= 0.5, 1L, 0L)

      res_real <- compute_binary_eval(
        y_true     = y_real,
        pred_class = pred_class_real,
        period_i   = period_i,
        tag_i      = TAG_i,
        horizon_i  = h,
        eval_type  = "real"
      )

      containers <- store_real_eval_result(
        containers = containers,
        h = h,
        res_real = res_real,
        series_name = real_eval_df$series_name
      )

      cat(
        "[10b] Period:", period_i,
        "| mode:", window_mode_i,
        "| horizon:", h,
        "| test acc:", round(res_test$summary$accuracy, 4),
        "| real acc:", round(res_real$summary$accuracy, 4), "\n"
      )

      rm(
        model_xgb, y_all, train_df, test_df, predictor_cols_h,
        X_test, y_test, dtest, pred_prob_test, pred_class_test, res_test,
        X_real, y_real, dreal, pred_prob_real, pred_class_real, res_real
      )
      gc()
    }

    xgb_eval <- build_consolidated_eval_object(
      containers = containers,
      real_eval_df = real_eval_df
    )

    save_consolidated_eval_object(xgb_eval, eval_rds_i, window_mode_i)
    cat("[10b] Saved consolidated evaluation object:", eval_rds_i, "| mode:", window_mode_i, "\n")

    rm(
      df, split_index, split_use, train_idx, test_idx,
      predictor_cols_base, H_i, real_eval_df,
      containers, xgb_eval
    )
    gc()
  }
}
