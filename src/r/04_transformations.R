# ==============================================================================
# 04_transformations.R
#
# Purpose: Standardise the historical and future values in each M4 window.
# Inputs:  Global periods, window_modes, data_dir, and RUN_PARALLEL values;
#          shared scaling, tagging, and parallel-processing helpers.
# Outputs: data/all_windows_std_<frequency>_<window>.rds files.
# Run from: Repository root; sourced by src/r/00_main_new.R.
# ==============================================================================

# Main execution --------------------------------------------------------------

for (period_i in periods) {
  TAG_i <- freq_tag(period_i)

  for (window_mode_i in window_modes) {
    WINDOW_TAG_i <- window_tag(window_mode_i)

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

    windows_std_i <- file.path(
      data_dir,
      paste0(
        "all_windows_std_",
        TAG_i,
        "_",
        WINDOW_TAG_i,
        ".rds"
      )
    )

    if (!file.exists(windows_raw_i)) {
      cat(
        "[04] Skip",
        period_i,
        window_mode_i,
        "- missing input:",
        windows_raw_i,
        "\n"
      )
      next
    }

    all_windows <- readRDS(windows_raw_i)
    n_windows <- nrow(all_windows)

    cat(
      "\n[04] Period =", period_i,
      "| window_mode =", window_mode_i,
      "| loaded", n_windows,
      "windows from", windows_raw_i, "\n"
    )

    transform_one_window <- function(i) {
      scaled <- scale_pair_minmax_std(
        x = all_windows$x[[i]],
        xx = all_windows$xx[[i]]
      )

      list(
        x = scaled$x_std,
        xx = scaled$xx_std
      )
    }

    if (RUN_PARALLEL) {
      cache_dir_transf <- file.path(
        data_dir,
        paste0(
          "cache_transf_",
          TAG_i,
          "_",
          WINDOW_TAG_i
        )
      )

      rows <- run_step_parallel(
        dataset = seq_len(n_windows),
        step_fun = transform_one_window,
        chunk_size = 5000L,
        save_foldername = cache_dir_transf,
        step_name = paste0(
          "transform_windows_",
          TAG_i,
          "_",
          WINDOW_TAG_i
        )
      )
    } else {
      rows <- lapply(
        seq_len(n_windows),
        transform_one_window
      )
    }

    all_windows_std <- all_windows
    all_windows_std$x <- lapply(rows, `[[`, "x")
    all_windows_std$xx <- lapply(rows, `[[`, "xx")

    cat(
      "Applied standardisation to",
      nrow(all_windows_std),
      "windows for",
      period_i,
      "mode",
      window_mode_i,
      ".\n"
    )

    saveRDS(
      all_windows_std,
      file = windows_std_i
    )

    cat(
      "Saved transformed windows to:",
      windows_std_i,
      "\n"
    )

    rm(all_windows, rows, all_windows_std)
    gc()
  }
}
