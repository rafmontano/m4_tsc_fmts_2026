# ==============================================================================
# 06_labels.R
#
# Purpose: Compute the directional label vector for every standardised window.
# Inputs:  Global data_dir, periods, window_modes, and RUN_PARALLEL values;
#          shared label, tagging, and parallel-processing helpers.
# Outputs: data/all_windows_labeled_<frequency>_<window>.rds files.
# Run from: Repository root; sourced by src/r/00_main_new.R.
# ==============================================================================

# Main execution --------------------------------------------------------------

for (period_i in periods) {
  TAG_i <- freq_tag(period_i)

  for (window_mode_i in window_modes) {
    WINDOW_TAG_i <- window_tag(window_mode_i)

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

    labeled_i <- file.path(
      data_dir,
      paste0(
        "all_windows_labeled_",
        TAG_i,
        "_",
        WINDOW_TAG_i,
        ".rds"
      )
    )

    if (!file.exists(windows_std_i)) {
      cat(
        "[06] Skip",
        period_i,
        window_mode_i,
        "- missing input:",
        windows_std_i,
        "\n"
      )
      next
    }

    all_windows_std <- readRDS(windows_std_i)
    n_windows <- nrow(all_windows_std)

    if (n_windows == 0L) {
      cat(
        "[06] Skip",
        period_i,
        window_mode_i,
        "- empty input:",
        windows_std_i,
        "\n"
      )
      next
    }

    cat(
      "\n[06] Period =", period_i,
      "| window_mode =", window_mode_i,
      "| window_tag =", WINDOW_TAG_i,
      "| loaded", n_windows,
      "windows from", windows_std_i, "\n"
    )

    add_labels_one_window <- function(i) {
      compute_label_vector(
        x = all_windows_std$x[[i]],
        xx = all_windows_std$xx[[i]]
      )
    }

    if (RUN_PARALLEL) {
      cache_dir_label <- file.path(
        data_dir,
        paste0(
          "cache_label_",
          TAG_i,
          "_",
          WINDOW_TAG_i
        )
      )

      rows <- run_step_parallel(
        dataset = seq_len(n_windows),
        step_fun = add_labels_one_window,
        chunk_size = 5000L,
        save_foldername = cache_dir_label,
        step_name = paste0(
          "labels_",
          TAG_i,
          "_",
          WINDOW_TAG_i
        )
      )
    } else {
      rows <- lapply(
        seq_len(n_windows),
        add_labels_one_window
      )
    }

    all_windows_labeled <- all_windows_std
    all_windows_labeled$labels <- rows

    cat(
      "Added labels vector to",
      nrow(all_windows_labeled),
      "windows for",
      period_i,
      "mode",
      window_mode_i,
      ".\n"
    )

    saveRDS(all_windows_labeled, labeled_i)
    cat("Saved labeled windows to", labeled_i, "\n")

    rm(all_windows_std, rows, all_windows_labeled)
    gc()
  }
}
