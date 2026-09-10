# ==============================================================================
# 03_rolling_windows.R
#
# Purpose: Create rolling or full-history input and horizon pairs from cleaned
#          M4 series.
# Inputs:  Global subset_clean_file, data_dir, periods, window_modes,
#          SPLIT_STRATEGY_ID, and RUN_PARALLEL values; shared helper functions.
# Outputs: data/all_windows_raw_<frequency>_<window>.rds files.
# Run from: Repository root; sourced by src/r/00_main_new.R.
# ==============================================================================

# Load dataset ----------------------------------------------------------------

M4_subset_clean <- readRDS(subset_clean_file)
cat(
  "Loaded cleaned M4 dataset with",
  length(M4_subset_clean),
  "series.\n"
)

# Helper functions ------------------------------------------------------------

make_rolling_windows <- function(
  x,
  window_size,
  horizon,
  stride,
  split_strategy_id,
  window_mode
) {
  x <- as.numeric(x)
  n <- length(x)

  # Full-history mode uses x as model history and xx as the final holdout.

  if (window_mode == "full") {
    if (n <= horizon) {
      return(NULL)
    }

    xx <- x[(n - horizon + 1L):n]
    x_new <- x[1L:(n - horizon)]

    return(tibble::tibble(
      x = list(x_new),
      xx = list(xx)
    ))
  }

  # S3 creates one history and holdout pair per eligible series.

  if (split_strategy_id == "S3") {
    if (n <= horizon) {
      return(NULL)
    }

    xx <- x[(n - horizon + 1L):n]
    x_new <- x[1L:(n - horizon)]

    return(tibble::tibble(
      x = list(x_new),
      xx = list(xx)
    ))
  }

  # Other strategies create fixed-length rolling windows.

  max_start <- n - window_size - horizon + 1L

  if (max_start < 1L) {
    return(NULL)
  }

  starts <- seq.int(
    1L,
    max_start,
    by = as.integer(stride)
  )

  windows <- vector("list", length(starts))
  horizons <- vector("list", length(starts))

  for (k in seq_along(starts)) {
    i <- starts[k]

    windows[[k]] <- x[
      i:(i + window_size - 1L)
    ]

    horizons[[k]] <- x[
      (i + window_size):(i + window_size + horizon - 1L)
    ]
  }

  tibble::tibble(
    x = windows,
    xx = horizons
  )
}

# Main execution --------------------------------------------------------------

for (period_i in periods) {
  HORIZON_i <- get_m4_horizon(period_i)
  TAG_i <- freq_tag(period_i)

  M4_period <- Filter(
    function(s) as.character(s$period) == period_i,
    M4_subset_clean
  )

  n_series <- length(M4_period)

  for (window_mode_i in window_modes) {
    WINDOW_TAG_i <- window_tag(window_mode_i)

    WINDOW_SIZE_i <- if (window_mode_i == "full") {
      Inf
    } else {
      get_window_size_from_h(period_i, window_mode_i)
    }

    STRIDE_i <- if (window_mode_i == "full") {
      NA_integer_
    } else {
      switch(SPLIT_STRATEGY_ID,
        "S1" = as.integer(WINDOW_SIZE_i + HORIZON_i),
        "S2" = as.integer(WINDOW_SIZE_i + HORIZON_i),
        "S3" = NA_integer_,
        message(
          "Unknown SPLIT_STRATEGY_ID: ",
          SPLIT_STRATEGY_ID
        )
      )
    }

    windows_raw_i <- file.path(
      data_dir,
      paste0(
        "all_windows_raw_",
        TAG_i,
        "_",
        WINDOW_TAG_i,
        ".rds"
      )
    )

    cat(
      "\n[03] Period =", period_i,
      "| window_mode =", window_mode_i,
      "| window_tag =", WINDOW_TAG_i,
      "| series =", n_series,
      "| window_size =", WINDOW_SIZE_i,
      "| horizon =", HORIZON_i,
      "| stride =", STRIDE_i, "\n"
    )

    create_series_windows <- function(i) {
      s <- M4_period[[i]]

      tbl <- make_rolling_windows(
        x = s$x,
        window_size = WINDOW_SIZE_i,
        horizon = HORIZON_i,
        stride = STRIDE_i,
        split_strategy_id = SPLIT_STRATEGY_ID,
        window_mode = window_mode_i
      )

      if (is.null(tbl)) {
        return(NULL)
      }

      series_name <- if (!is.null(s$st)) {
        as.character(s$st)
      } else {
        NA_character_
      }

      dplyr::mutate(
        tbl,
        series_id = i,
        series_name = series_name
      )
    }

    if (RUN_PARALLEL) {
      rows <- run_step_parallel(
        dataset = seq_len(n_series),
        step_fun = create_series_windows,
        chunk_size = 250L,
        save_foldername = file.path(data_dir, paste0("cache_windows_", 
                                                     TAG_i, "_", WINDOW_TAG_i)),
        step_name = paste0(
          "rolling_windows_",
          TAG_i,
          "_",
          WINDOW_TAG_i
        )
      )
    } else {
      rows <- lapply(
        seq_len(n_series),
        create_series_windows
      )
    }

    all_windows <- dplyr::bind_rows(rows)

    cat(
      "Created",
      nrow(all_windows),
      "windows for",
      period_i,
      "mode",
      window_mode_i,
      ".\n"
    )

    if (SPLIT_STRATEGY_ID == "S3") {
      cat(
        "[03] S3 active: created one (x,xx) pair per eligible series.\n"
      )
    }

    if (window_mode_i == "full") {
      cat(
        "[03] FULL active: created one full-history holdout pair per eligible series.\n"
      )
    }

    saveRDS(all_windows, windows_raw_i)
    cat("Saved rolling windows to", windows_raw_i, "\n")

    rm(rows, all_windows)
    gc()
  }

  rm(M4_period)
  gc()
}
