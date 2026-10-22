# ==============================================================================
# 12_baseline_majority_feedback.R
# Adapted from src/r/12_baseline_fforma_smyl.R.
# Evaluate a training-selected majority direction at each frequency and horizon.
# Inputs: saved training rows, splits and real-evaluation rows in data/feedback.
# Outputs: results/feedback/majority/majority_eval_<tag>.rds.
# Run from the project root. No model fitting and no changes to baseline inputs.
# Existing feedback majority outputs (including the temporary DTW copy) are replaced
# for the default window mode when this script is run.
# ==============================================================================

library(dplyr)
library(tibble)
library(caret)

source("src/r/utils.R")
source("src/r/util_metrics.R")

# Allow a single frequency to be selected before sourcing for a staged run.
if (!exists("feedback_periods", inherits = FALSE)) {
  feedback_periods <- c("Hourly", "Daily", "Weekly", "Monthly", "Quarterly", "Yearly")
}
input_dir <- file.path("data", "feedback")
results_majority_dir <- file.path("results", "feedback", "majority")
window_mode_i <- "default"
window_tag_i <- "d"

# Keep training labels unchanged, but evaluate every method against original M4.
original_file <- file.path(input_dir, "M4_subset.rds")
original_m4 <- readRDS(original_file)
original_ids <- vapply(original_m4, function(s) as.character(s$st), character(1))
stopifnot(!anyDuplicated(original_ids))

# Fail before producing any outputs if required input files are absent.
for (period_i in feedback_periods) {
  TAG_i <- freq_tag(period_i)
  required <- file.path(input_dir, c(
    paste0("all_windows_with_features_", TAG_i, "_", window_tag_i, ".rds"),
    paste0("split_index_", TAG_i, "_", window_tag_i, ".rds"),
    paste0("real_eval_with_features_", TAG_i, "_", window_tag_i, ".rds")
  ))
  if (any(!file.exists(required))) {
    stop("Missing feedback inputs: ", paste(required[!file.exists(required)], collapse = ", "))
  }
}

dir.create(results_majority_dir, recursive = TRUE, showWarnings = FALSE)

for (period_i in feedback_periods) {
  TAG_i <- freq_tag(period_i)
  training_file <- file.path(input_dir, paste0("all_windows_with_features_", TAG_i, "_d.rds"))
  split_file <- file.path(input_dir, paste0("split_index_", TAG_i, "_d.rds"))
  evaluation_file <- file.path(input_dir, paste0("real_eval_with_features_", TAG_i, "_d.rds"))
  majority_eval_i <- file.path(results_majority_dir, paste0("majority_eval_", TAG_i, ".rds"))

  training_df <- readRDS(training_file)
  split_index <- readRDS(split_file)
  baseline_df <- readRDS(evaluation_file)
  # Match the MANTIS split selection in r_bridge_sktime.py.
  split_use <- get_model_split(split_index, model_id = "MANTIS")
  train_idx <- split_use$train
  test_idx <- split_use$test
  if (is.null(train_idx) || !length(train_idx) || anyNA(train_idx) ||
      any(train_idx != as.integer(train_idx)) || anyDuplicated(train_idx) ||
      any(train_idx < 1L | train_idx > nrow(training_df))) {
    stop("Invalid saved training indices: ", split_file)
  }
  if (length(intersect(train_idx, test_idx))) stop("Training/test rows overlap: ", split_file)
  if ("series_id" %in% names(training_df) && length(test_idx)) {
    if (anyNA(test_idx) || any(test_idx < 1L | test_idx > nrow(training_df))) {
      stop("Invalid saved test indices: ", split_file)
    }
    if (length(intersect(training_df$series_id[train_idx], training_df$series_id[test_idx]))) {
      stop("Training/test series overlap: ", split_file)
    }
  }
  if (!"labels" %in% names(training_df) || !"labels" %in% names(baseline_df) ||
      !nrow(baseline_df)) stop("Missing or empty directional labels for ", period_i)
  train_labels <- training_df$labels[train_idx]
  H_i <- length(baseline_df$labels[[1]])
  if (!H_i || any(lengths(train_labels) != H_i) ||
      any(lengths(baseline_df$labels) != H_i)) stop("Horizon mismatch for ", period_i)

  original_idx <- match(baseline_df$series_name, original_ids)
  if (length(original_idx) != nrow(baseline_df) || anyNA(original_idx) ||
      anyDuplicated(original_idx)) stop("Original M4 series mismatch for ", period_i)
  input_labels <- baseline_df$labels
  baseline_df$labels <- lapply(original_m4[original_idx], function(s) {
    as.integer(as.numeric(s$xx) > tail(as.numeric(s$x), 1))
  })
  stopifnot(all(lengths(baseline_df$labels) == H_i))

  cat("\n[12 feedback] Period =", period_i, "| training windows =", length(train_idx),
      "| evaluation series =", nrow(baseline_df), "| horizons =", H_i, "\n")
  summary_majority_rows <- vector("list", H_i)
  detail_majority <- vector("list", H_i)

  for (h in seq_len(H_i)) {
    y_train <- vapply(train_labels, function(v) as.integer(v[h]), integer(1))
    y_true <- vapply(baseline_df$labels, function(v) as.integer(v[h]), integer(1))
    if (anyNA(y_train) || anyNA(y_true) ||
        any(!y_train %in% c(0L, 1L)) || any(!y_true %in% c(0L, 1L))) {
      stop("Non-binary or missing labels for ", period_i, ", horizon ", h)
    }
    # Count training WINDOWS only, as used by Mantis. Exact ties select 0
    # (non-increase); evaluation outcomes never select the prediction.
    train_up_proportion <- mean(y_train == 1L)
    majority_class <- as.integer(train_up_proportion > 0.5)
    y_majority <- rep.int(majority_class, length(y_true))

    res_majority <- compute_binary_eval(
      y_true = y_true,
      pred_class = y_majority,
      period_i = period_i,
      tag_i = TAG_i,
      horizon_i = h,
      eval_type = "real"
    )
    res_majority$summary$method <- "majority"
    res_majority$summary$n_train <- length(y_train)
    res_majority$summary$n_train_up <- sum(y_train == 1L)
    res_majority$summary$train_up_proportion <- train_up_proportion
    res_majority$summary$majority_class <- majority_class
    res_majority$summary$eval_up_proportion <- mean(y_true == 1L)
    stopifnot(isTRUE(all.equal(res_majority$summary$accuracy, mean(y_true == majority_class))))
    summary_majority_rows[[h]] <- res_majority$summary
    detail_majority[[h]] <- c(res_majority$detail, list(
      series_name = baseline_df$series_name,
      input_y_true = vapply(input_labels, function(v) as.integer(v[h]), integer(1))
    ))

    cat("[12 feedback] Horizon:", h, "| training Up:", round(train_up_proportion, 4),
        "| prediction:", majority_class, "| accuracy:", round(res_majority$summary$accuracy, 4), "\n")
  }

  majority_eval <- list(
    real = list(
      summary = bind_rows(summary_majority_rows),
      detail = detail_majority
    ),
    selection = list(
      training_file = training_file,
      split_file = split_file,
      evaluation_file = evaluation_file,
      split_name = if ("MANTIS" %in% names(split_index)) "MANTIS" else "uncap",
      weighting = "one vote per saved training window",
      tie_rule = "non-increase (0)",
      evaluation_definition = "original M4 future value > original last observation",
      original_file = original_file
    )
  )
  save_consolidated_eval_object(majority_eval, majority_eval_i, window_mode_i)
  cat("[12 feedback] Saved:", majority_eval_i, "| mode:", window_mode_i, "\n")

  rm(training_df, split_index, split_use, train_idx, test_idx, baseline_df,
     train_labels, summary_majority_rows, detail_majority, majority_eval,
     y_train, y_true, y_majority, res_majority)
  gc()
}
