# ==============================================================================
# 13_baseline_dtw_real.R
#
# Purpose:
#   Evaluate the DTW 1-nearest-neighbour baseline on real M4 series, reusing
#   each series' nearest training neighbour across forecast horizons.
# Inputs:
#   Global configuration and labeled, split-index, and real-evaluation RDS files.
# Outputs:
#   One consolidated DTW evaluation RDS file per frequency.
# Run from:
#   Project root, through 00_main_new.R after script 10a.
# ==============================================================================

# Dependencies ----------------------------------------------------------------

library(dtw)
library(dplyr)
library(tibble)

source("src/r/utils.R")
source("src/r/util_metrics.R")
source("src/r/parallel_util.R")

# Helper functions ------------------------------------------------------------

results_dtw_dir <- file.path(results_dir, "dtw")
if (!dir.exists(results_dtw_dir)) {
  dir.create(results_dtw_dir, recursive = TRUE)
}

dtw_distance_safe <- function(a, b, w_base) {
  la <- length(a)
  lb <- length(b)

  if (la < 2L || lb < 2L) {
    return(Inf)
  }

  w_cap <- min(w_base, la - 1L, lb - 1L)
  if (w_cap < 1L) {
    w_cap <- 1L
  }

  tryCatch(
    dtw::dtw(
      a,
      b,
      distance.only = TRUE,
      step.pattern = dtw::symmetric2,
      window.type = "sakoechiba",
      window.size = as.integer(w_cap)
    )$distance,
    error = function(e) Inf
  )
}

find_nearest_dtw_index <- function(x_target, train_series, w_base) {
  dists <- vapply(
    train_series,
    function(x_train) {
      dtw_distance_safe(x_train, x_target, w_base)
    },
    numeric(1)
  )

  if (all(!is.finite(dists))) {
    return(NA_integer_)
  }

  as.integer(which.min(dists))
}

# Main execution --------------------------------------------------------------

for (period_i in periods) {
  TAG_i <- freq_tag(period_i)
  dtw_eval_i <- file.path(results_dtw_dir, paste0("dtw_eval_", TAG_i, ".rds"))

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
      cat("[13] Skip", period_i, window_mode_i, "- missing labeled file:", labeled_i, "\n")
      next
    }

    if (!file.exists(split_i)) {
      cat("[13] Skip", period_i, window_mode_i, "- missing split file:", split_i, "\n")
      next
    }

    if (!file.exists(real_eval_i)) {
      cat("[13] Skip", period_i, window_mode_i, "- missing REAL dataset:", real_eval_i, "\n")
      next
    }

    train_all <- readRDS(labeled_i)
    split_index <- readRDS(split_i)
    real_df <- readRDS(real_eval_i)

    split_use <- get_model_split(split_index, model_id = "dtw")
    train_idx <- split_use$train
    train_df <- train_all[train_idx, , drop = FALSE]

    H_i <- length(real_df$labels[[1]])

    cat(
      "\n[13] Period =", period_i,
      "| window_mode =", window_mode_i,
      "| train =", nrow(train_df),
      "| real =", nrow(real_df),
      "| horizons =", H_i,
      "\n"
    )

    train_series <- train_df$x
    real_series <- real_df$x

    base_len <- max(
      max(vapply(train_series, length, integer(1))),
      max(vapply(real_series, length, integer(1)))
    )

    dtw_window_frac <- if (exists("DTW_WINDOW_FRAC")) DTW_WINDOW_FRAC else 0.10
    dtw_window_base <- max(1L, as.integer(round(base_len * dtw_window_frac)))

    cat(
      "[13] DTW_WINDOW_FRAC =", dtw_window_frac,
      "| base_len =", base_len,
      "| dtw_window_base =", dtw_window_base,
      "\n"
    )

    # Compute the nearest neighbour once per real series.

    nn_cache_dir <- file.path(
      results_dtw_dir,
      "cache",
      paste0("dtw_nn_", TAG_i, "_", WINDOW_TAG_i)
    )

    nearest_train_idx <- tryCatch(
      {
        preds <- run_step_parallel(
          dataset = real_series,
          step_fun = function(
            x_target,
            train_series,
            w_base
          ) {
            find_nearest_dtw_index(
              x_target = x_target,
              train_series = train_series,
              w_base = w_base
            )
          },
          train_series = train_series,
          w_base = dtw_window_base,
          chunk_size = NULL,
          save_foldername = nn_cache_dir,
          step_name = paste0("dtw_nn_", TAG_i, "_", WINDOW_TAG_i)
        )

        as.integer(unlist(preds, use.names = FALSE))
      },
      error = function(e) {
        cat(
          "[13] Parallel nearest-neighbour DTW failed; using sequential fallback. Reason:",
          conditionMessage(e),
          "\n"
        )

        vapply(
          real_series,
          function(x_target) {
            find_nearest_dtw_index(
              x_target = x_target,
              train_series = train_series,
              w_base = dtw_window_base
            )
          },
          integer(1)
        )
      }
    )

    if (length(nearest_train_idx) != length(real_series)) {
      cat("[13] Skip", period_i, window_mode_i, "- nearest neighbour length mismatch.\n")
      next
    }

    if (anyNA(nearest_train_idx)) {
      cat("[13] Skip", period_i, window_mode_i, "- nearest neighbour prediction failed.\n")
      next
    }

    summary_real_rows <- list()
    detail_real <- vector("list", H_i)

    # Reuse nearest-neighbour labels across horizons.

    for (h in seq_len(H_i)) {
      y_train <- vapply(
        train_df$labels,
        function(v) as.integer(v[h]),
        integer(1)
      )

      y_real <- vapply(
        real_df$labels,
        function(v) as.integer(v[h]),
        integer(1)
      )

      if (length(unique(y_train)) < 2L) {
        summary_real_rows[[length(summary_real_rows) + 1L]] <-
          make_empty_eval_summary_row(
            period_i,
            TAG_i,
            "real",
            h,
            nrow(real_df),
            "single_class_train"
          )
        next
      }

      pred_class <- y_train[nearest_train_idx]

      if (anyNA(pred_class)) {
        summary_real_rows[[length(summary_real_rows) + 1L]] <-
          make_empty_eval_summary_row(
            period_i,
            TAG_i,
            "real",
            h,
            nrow(real_df),
            "prediction_failed"
          )
        next
      }

      res_real <- compute_binary_eval(
        y_true = y_real,
        pred_class = pred_class,
        period_i = period_i,
        tag_i = TAG_i,
        horizon_i = h,
        eval_type = "real"
      )

      summary_real_rows[[length(summary_real_rows) + 1L]] <- res_real$summary

      detail_real[[h]] <- c(
        res_real$detail,
        list(
          series_name = real_df$series_name,
          nearest_train_idx = nearest_train_idx
        )
      )

      cat(
        "[13] Period:", period_i,
        "| mode:", window_mode_i,
        "| horizon:", h,
        "| real acc:", round(res_real$summary$accuracy, 4),
        "\n"
      )

      rm(y_train, y_real, pred_class, res_real)
      gc()
    }

    dtw_eval <- list(
      real = list(
        summary = bind_rows(summary_real_rows),
        detail = detail_real,
        dataset = real_df
      )
    )

    save_consolidated_eval_object(dtw_eval, dtw_eval_i, window_mode_i)

    cat("[13] Saved:", dtw_eval_i, "| mode:", window_mode_i, "\n")

    rm(
      train_all,
      split_index,
      split_use,
      train_idx,
      train_df,
      real_df,
      train_series,
      real_series,
      nearest_train_idx,
      H_i,
      summary_real_rows,
      detail_real,
      dtw_eval
    )
    gc()
  }
}
