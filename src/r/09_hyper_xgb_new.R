# ==============================================================================
# 09_hyper_xgb_new.R
#
# Purpose:
#   Train one binary XGBoost model per horizon, frequency, and window mode.
# Inputs:
#   Global configuration and feature/split RDS files under data_dir.
# Outputs:
#   XGBoost model files under models/xgb/<frequency>_<window_mode>/.
# Run from:
#   Project root, through 00_main_new.R.
# ==============================================================================

# Dependencies ----------------------------------------------------------------

library(xgboost)
library(rBayesianOptimization)
library(parallel)

# Helper functions ------------------------------------------------------------

get_model_split <- function(split_index, model_id = "xgboost") {
  if (model_id %in% names(split_index)) {
    return(split_index[[model_id]])
  }
  split_index$uncap
}

get_xgb_nthread <- function() {
  sys <- Sys.info()[["sysname"]]

  if (is.null(sys)) {
    return(8L)
  }

  sys <- tolower(sys)

  if (sys == "darwin") {
    return(10L)
  }

  if (sys == "linux") {
    return(32L)
  }

  return(8L)
}

xgb_hyperparameter_search_binary <- function(
  X, y,
  nfold = 5,
  init_points = 8,
  n_iter = 20,
  nrounds_max = 1500,
  nthread = get_xgb_nthread()
) {
  dtrain <- xgb.DMatrix(data = X, label = y)

  xgb_cv_bayes <- function(max_depth, eta, gamma, min_child_weight,
                           subsample, colsample_bytree, lambda, alpha) {
    params <- list(
      booster          = "gbtree",
      objective        = "binary:logistic",
      eval_metric      = "logloss",
      max_depth        = as.integer(max_depth),
      eta              = eta,
      gamma            = gamma,
      min_child_weight = min_child_weight,
      subsample        = subsample,
      colsample_bytree = colsample_bytree,
      lambda           = lambda,
      alpha            = alpha,
      nthread          = nthread
    )

    cv <- tryCatch(
      xgb.cv(
        params                = params,
        data                  = dtrain,
        nrounds               = nrounds_max,
        nfold                 = nfold,
        showsd                = TRUE,
        verbose               = FALSE,
        early_stopping_rounds = 15
      ),
      error = function(e) NULL
    )

    if (is.null(cv) || is.null(cv$evaluation_log) || nrow(cv$evaluation_log) == 0L) {
      return(list(Score = -1e6 + runif(1, -1e-3, 1e-3), Pred = 1L))
    }

    j <- if (!is.null(cv$best_iteration) && is.finite(cv$best_iteration)) {
      cv$best_iteration
    } else {
      which.min(cv$evaluation_log$test_logloss_mean)
    }

    val <- cv$evaluation_log$test_logloss_mean[j]

    if (!is.finite(val) || length(val) == 0L) {
      return(list(Score = -1e6 + runif(1, -1e-3, 1e-3), Pred = 1L))
    }

    list(Score = -val, Pred = as.integer(j))
  }

  bounds <- list(
    max_depth        = c(3L, 8L),
    eta              = c(0.02, 0.2),
    gamma            = c(0, 10),
    min_child_weight = c(1, 20),
    subsample        = c(0.5, 1.0),
    colsample_bytree = c(0.4, 1.0),
    lambda           = c(0, 10),
    alpha            = c(0, 10)
  )

  opt_res <- tryCatch(
    BayesianOptimization(
      FUN         = xgb_cv_bayes,
      bounds      = bounds,
      init_points = init_points,
      n_iter      = n_iter,
      acq         = "ei",
      verbose     = TRUE
    ),
    error = function(e) {
      message("[09] BayesianOptimization failed: ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(opt_res) && !is.null(opt_res$Best_Par)) {
    return(opt_res)
  }

  message("[09] Falling back to fixed default XGBoost parameters.")

  list(
    Best_Par = c(
      max_depth = 6,
      eta = 0.10,
      gamma = 0,
      min_child_weight = 1,
      subsample = 0.80,
      colsample_bytree = 0.80,
      lambda = 1,
      alpha = 0
    )
  )
}

# Main execution --------------------------------------------------------------

for (period_i in periods) {
  TAG_i <- freq_tag(period_i)

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

    out_dir_i <- file.path(models_dir, "xgb", paste0(TAG_i, "_", WINDOW_TAG_i))
    if (!dir.exists(out_dir_i)) dir.create(out_dir_i, recursive = TRUE)

    if (!file.exists(features_i)) {
      cat("[09] Skip", period_i, window_mode_i, "- missing features file:", features_i, "\n")
      next
    }

    if (!file.exists(split_i)) {
      cat("[09] Skip", period_i, window_mode_i, "- missing split file:", split_i, "\n")
      next
    }

    df <- readRDS(features_i)
    split_index <- readRDS(split_i)
    split_use <- get_model_split(split_index, model_id = "xgboost")

    train_idx <- split_use$train
    test_idx <- split_use$test

    H_i <- length(df$labels[[1]])

    cat(
      "\n[09] Period =", period_i,
      "| window_mode =", window_mode_i,
      "| rows =", nrow(df),
      "| train =", length(train_idx),
      "| test =", length(test_idx),
      "| horizons =", H_i, "\n"
    )

    predictor_cols <- setdiff(
      names(df),
      c("x", "xx", "labels", "series_id", "series_name")
    )

    if (length(predictor_cols) == 0L) {
      message("[09] No predictor columns found for period: ", period_i)
    }

    for (h in seq_len(H_i)) {
      model_path_i <- file.path(out_dir_i, sprintf("model_h%02d.ubj", h))

      if (file.exists(model_path_i) && !isTRUE(FORCE_RERUN)) {
        cat("[09] Skip existing model:", model_path_i, "\n")
        next
      }

      cat("[09] Training horizon", h, "for", period_i, "mode", window_mode_i, "\n")

      y_all <- vapply(df$labels, function(v) as.integer(v[h]), integer(1))

      train_df <- df[train_idx, , drop = FALSE]
      test_df <- df[test_idx, , drop = FALSE]

      y_train <- y_all[train_idx]
      y_test <- y_all[test_idx]

      if (length(unique(y_train)) < 2L) {
        cat("[09] Horizon", h, "has < 2 classes in train set. Skipping.\n")
        next
      }

      feature_df_train <- train_df[, predictor_cols, drop = FALSE]
      feature_vars <- sapply(feature_df_train, function(col) var(col, na.rm = TRUE))
      constant_cols <- names(feature_vars[feature_vars == 0 | is.na(feature_vars)])

      predictor_cols_h <- setdiff(predictor_cols, constant_cols)

      X_train <- as.matrix(train_df[, predictor_cols_h, drop = FALSE])
      X_test <- as.matrix(test_df[, predictor_cols_h, drop = FALSE])

      X_train[!is.finite(X_train)] <- NA_real_
      X_test[!is.finite(X_test)] <- NA_real_

      opt_res <- xgb_hyperparameter_search_binary(
        X = X_train,
        y = y_train,
        nfold = 5,
        init_points = 8,
        n_iter = 20,
        nrounds_max = 1500,
        nthread = get_xgb_nthread()
      )

      best_params <- opt_res$Best_Par

      dtrain_final <- xgb.DMatrix(X_train, label = y_train)

      params_final <- list(
        booster          = "gbtree",
        objective        = "binary:logistic",
        eval_metric      = "logloss",
        max_depth        = as.integer(best_params["max_depth"]),
        eta              = best_params["eta"],
        gamma            = best_params["gamma"],
        min_child_weight = best_params["min_child_weight"],
        subsample        = best_params["subsample"],
        colsample_bytree = best_params["colsample_bytree"],
        lambda           = best_params["lambda"],
        alpha            = best_params["alpha"],
        nthread          = get_xgb_nthread()
      )

      cv_final <- xgb.cv(
        params                = params_final,
        data                  = dtrain_final,
        nrounds               = 1500,
        nfold                 = 5,
        verbose               = FALSE,
        early_stopping_rounds = 15
      )

      best_nrounds <- cv_final$best_iteration
      if (is.null(best_nrounds) || !is.finite(best_nrounds)) {
        best_nrounds <- which.min(cv_final$evaluation_log$test_logloss_mean)
      }

      best_nrounds <- as.integer(best_nrounds)

      final_model <- xgb.train(
        params  = params_final,
        data    = dtrain_final,
        nrounds = best_nrounds,
        verbose = 0
      )

      dtest <- xgb.DMatrix(X_test)
      pred_prob <- predict(final_model, dtest)
      pred_class <- ifelse(pred_prob >= 0.5, 1L, 0L)

      conf_mat <- table(Pred = pred_class, True = y_test)
      accuracy <- sum(diag(conf_mat)) / sum(conf_mat)

      cat("[09] Horizon", h, "| held-out accuracy:", round(accuracy, 4), "\n")

      xgb.save(final_model, model_path_i)
      cat("[09] Saved model:", model_path_i, "\n")

      rm(
        y_all, train_df, test_df, y_train, y_test,
        feature_df_train, feature_vars, constant_cols, predictor_cols_h,
        X_train, X_test, dtrain_final, params_final, cv_final,
        best_nrounds, final_model, dtest, pred_prob, pred_class, conf_mat, accuracy,
        best_params, opt_res
      )
      gc()
    }

    rm(df, split_index, split_use, train_idx, test_idx, predictor_cols, H_i)
    gc()
  }
}
