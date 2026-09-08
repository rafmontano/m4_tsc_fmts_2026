# ==============================================================================
# 04b_table_accuracy_macroF1_by_frequency.R
#
# Purpose:
#   Build accuracy and macro-F1 tables by model, frequency, and window mode.
# Inputs:
#   Consolidated model-evaluation RDS files under results.
# Outputs:
#   Paper CSV tables under results/paper/tables.
# Run from:
#   Project root.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(readr)
})

# Configuration --------------------------------------------------------------

results_dir <- "results"

# window_modes <- c("small", "default", "large", "full")
window_modes <- "default"
benchmark_models <- c("fforma", "smyl")
benchmark_window_mode <- "default"

freq_levels <- c("Yearly", "Quarterly", "Monthly", "Weekly", "Daily", "Hourly")

freq_map <- tibble::tribble(
  ~dataset,     ~tag,
  "Yearly",     "y",
  "Quarterly",  "q",
  "Monthly",    "m",
  "Weekly",     "w",
  "Daily",      "d",
  "Hourly",     "h"
)

model_map <- tibble::tribble(
  ~model_id,        ~model,
  "dtw",            "Benchmark (1NN-DTW)",
  "euclidean",      "EUCLIDEAN",
  "fforma",         "FFORMA",
  "inceptiontime",  "InceptionTime",
  "rocket",         "ROCKET",
  "rotf",           "Rotation Forest",
  "smyl",           "SMYL",
  "mantis",         "MANTIS",
  "xgb",            "XGBoost",
  "chronos",        "CHRONOS"
)

model_levels_preferred <- c(
  "Benchmark (1NN-DTW)",
  "EUCLIDEAN",
  "Rotation Forest",
  "XGBoost",
  "InceptionTime",
  "ROCKET",
  "MANTIS",
  "CHRONOS",
  "SMYL",
  "FFORMA"
)

# Output paths ---------------------------------------------------------------

out_dir <- file.path("results", "paper", "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Helper functions -----------------------------------------------------------

safe_read_rds <- function(path) {
  tryCatch(readRDS(path), error = function(e) NULL)
}

fmt3 <- function(x) {
  ifelse(is.na(x), "", sprintf("%.3f", x))
}

read_one_eval_summary <- function(model_id, model, tag, dataset, window_mode) {
  path <- file.path(results_dir, model_id, sprintf("%s_eval_%s.rds", model_id, tag))

  if (!file.exists(path)) {
    return(tibble())
  }

  obj <- safe_read_rds(path)

  if (is.null(obj)) {
    warning("Failed to read RDS: ", path)
    return(tibble())
  }

  effective_window_mode <- ifelse(
    model_id %in% benchmark_models,
    benchmark_window_mode,
    window_mode
  )

  if (!(effective_window_mode %in% names(obj))) {
    warning("Window mode '", effective_window_mode, "' not found in: ", path)
    return(tibble())
  }

  if (is.null(obj[[effective_window_mode]]$real) ||
    is.null(obj[[effective_window_mode]]$real$summary)) {
    warning("Missing obj[[effective_window_mode]]$real$summary in: ", path)
    return(tibble())
  }

  summ <- obj[[effective_window_mode]]$real$summary

  if (!is.data.frame(summ) || nrow(summ) == 0L) {
    warning("Empty REAL summary in: ", path, " | window_mode=", window_mode)
    return(tibble())
  }

  if (!("accuracy" %in% names(summ))) {
    warning("Column 'accuracy' not found in: ", path, " | window_mode=", window_mode)
    return(tibble())
  }

  if (!("macro_f1" %in% names(summ))) {
    warning("Column 'macro_f1' not found in: ", path, " | window_mode=", window_mode)
    return(tibble())
  }

  tibble(
    window_mode = window_mode,
    source_window_mode = effective_window_mode,
    dataset = dataset,
    tag = tag,
    model_id = model_id,
    model = model,
    accuracy = mean(as.numeric(summ$accuracy), na.rm = TRUE),
    macro_f1 = mean(as.numeric(summ$macro_f1), na.rm = TRUE),
    n_rows = if ("n_rows" %in% names(summ)) sum(as.numeric(summ$n_rows), na.rm = TRUE) else NA_real_,
    file = path
  ) %>%
    mutate(
      accuracy = ifelse(is.nan(accuracy), NA_real_, accuracy),
      macro_f1 = ifelse(is.nan(macro_f1), NA_real_, macro_f1),
      n_rows   = ifelse(is.nan(n_rows), NA_real_, n_rows)
    )
}

build_window_rows <- function(window_mode) {
  purrr::pmap_dfr(freq_map, function(dataset, tag) {
    purrr::pmap_dfr(model_map, function(model_id, model) {
      read_one_eval_summary(
        model_id    = model_id,
        model       = model,
        tag         = tag,
        dataset     = dataset,
        window_mode = window_mode
      )
    })
  }) %>%
    mutate(
      dataset  = factor(dataset, levels = freq_levels),
      model    = as.character(model),
      accuracy = as.numeric(accuracy),
      macro_f1 = as.numeric(macro_f1),
      n_rows   = as.numeric(n_rows)
    ) %>%
    filter(!is.na(dataset), !is.na(model)) %>%
    arrange(model, dataset) %>%
    mutate(dataset = as.character(dataset))
}

build_weighted_averages <- function(rows) {
  freq_sizes_from_results <- rows %>%
    group_by(dataset) %>%
    summarise(
      n = suppressWarnings(max(n_rows, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(n = ifelse(is.infinite(n) | is.na(n) | n <= 0, NA_real_, n))

  freq_sizes_default <- tibble::tribble(
    ~dataset, ~n_default,
    "Yearly", 23000,
    "Quarterly", 24000,
    "Monthly", 48000,
    "Weekly", 359,
    "Daily", 4000,
    "Hourly", 414
  )

  freq_sizes <- freq_sizes_from_results %>%
    left_join(freq_sizes_default, by = "dataset") %>%
    mutate(n = ifelse(is.na(n), n_default, n)) %>%
    select(dataset, n)

  message("Frequency sizes used for W.Avg:")
  print(freq_sizes)

  rows %>%
    left_join(freq_sizes, by = "dataset") %>%
    group_by(model) %>%
    summarise(
      dataset  = "W. Avg.",
      accuracy = sum(accuracy * n, na.rm = TRUE) / sum(n[!is.na(accuracy)]),
      macro_f1 = sum(macro_f1 * n, na.rm = TRUE) / sum(n[!is.na(macro_f1)]),
      .groups  = "drop"
    )
}

write_window_table <- function(window_mode, rows) {
  out_window_dir <- file.path(out_dir, window_mode)
  dir.create(out_window_dir, recursive = TRUE, showWarnings = FALSE)

  out_csv <- file.path(
    out_window_dir,
    "model_accuracy_macroF1_by_frequency.csv"
  )

  out_csv_display <- file.path(
    out_window_dir,
    "model_accuracy_macroF1_by_frequency_display.csv"
  )

  out_coverage_csv <- file.path(
    out_window_dir,
    "model_accuracy_macroF1_coverage.csv"
  )

  rows_w <- build_weighted_averages(rows)

  rows_all <- bind_rows(rows, rows_w) %>%
    mutate(dataset = factor(dataset, levels = c(freq_levels, "W. Avg.")))

  wide_acc <- rows_all %>%
    select(model, dataset, accuracy) %>%
    tidyr::pivot_wider(names_from = dataset, values_from = accuracy) %>%
    rename_with(~ paste0(.x, "_acc"), -model)

  wide_f1 <- rows_all %>%
    select(model, dataset, macro_f1) %>%
    tidyr::pivot_wider(names_from = dataset, values_from = macro_f1) %>%
    rename_with(~ paste0(.x, "_f1"), -model)

  tbl_wide <- wide_acc %>%
    left_join(wide_f1, by = "model")

  models_found <- unique(tbl_wide$model)

  model_levels <- unique(c(
    model_levels_preferred,
    sort(setdiff(models_found, model_levels_preferred))
  ))

  tbl_wide <- tbl_wide %>%
    mutate(model = factor(model, levels = model_levels)) %>%
    arrange(model) %>%
    mutate(model = as.character(model))

  readr::write_csv(tbl_wide, out_csv)

  tbl_display <- tbl_wide

  for (j in seq_along(tbl_display)) {
    if (is.numeric(tbl_display[[j]])) {
      tbl_display[[j]] <- fmt3(tbl_display[[j]])
    }
  }

  readr::write_csv(tbl_display, out_csv_display)

  coverage <- rows %>%
    count(model, dataset, name = "present") %>%
    tidyr::pivot_wider(
      names_from = dataset,
      values_from = present,
      values_fill = 0
    ) %>%
    arrange(model)

  readr::write_csv(coverage, out_coverage_csv)

  message("Saved numeric table to:  ", out_csv)
  message("Saved display table to:  ", out_csv_display)
  message("Saved coverage table to: ", out_coverage_csv)

  print(tbl_display)

  message("Coverage (1=present, 0=missing):")
  print(coverage)

  invisible(tbl_wide)
}

# Build one table per window mode --------------------------------------------

for (window_mode in window_modes) {
  message("------------------------------------------------------------")
  message("Processing window_mode: ", window_mode)
  message("------------------------------------------------------------")

  rows <- build_window_rows(window_mode)

  if (nrow(rows) == 0L) {
    warning("No rows found for window_mode=", window_mode, ". Skipping.")
    next
  }

  write_window_table(window_mode, rows)
}

message("Finished accuracy / Macro-F1 table generation for all window modes.")
