# ==============================================================================
# 07a_ts_features.R
#
# Purpose: Extract FFORMA-compatible or directional features for each labelled
#          window while limiting memory use during parallel processing.
# Inputs:  Global periods, window_modes, data_dir, FEATURE_ENGINE, and
#          RUN_PARALLEL values; labelled-window RDS files and shared helpers.
# Outputs: data/all_windows_with_features_<frequency>_<window>.rds files.
# Run from: Repository root; sourced by src/r/00_main_new.R.
# ==============================================================================

# Dependencies ----------------------------------------------------------------

library(parallel)
library(foreach)
library(doParallel)

source("src/r/features.R")
source("src/r/forecast_methods3.R")

# Main execution --------------------------------------------------------------

for (period_i in periods) {
  TAG_i <- freq_tag(period_i)

  for (window_mode_i in window_modes) {
    WINDOW_TAG_i <- window_tag(window_mode_i)

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

    features_i <- file.path(
      data_dir,
      paste0(
        "all_windows_with_features_",
        TAG_i,
        "_",
        WINDOW_TAG_i,
        ".rds"
      )
    )

    features_cache_dir <- file.path(
      data_dir,
      paste0(
        "cache_features_",
        FEATURE_ENGINE,
        "_",
        TAG_i,
        "_",
        WINDOW_TAG_i
      )
    )

    if (!file.exists(labeled_i)) {
      cat(
        "[07a] Skip",
        period_i,
        window_mode_i,
        "- missing input:",
        labeled_i,
        "\n"
      )
      next
    }

    # Retain only the columns required during feature calculation.

    all_windows_labeled <- readRDS(labeled_i)
    n_windows <- nrow(all_windows_labeled)

    cat(
      "\n[07a] Period =", period_i,
      "| window_mode =", window_mode_i,
      "| windows =", n_windows,
      "| feature_engine =", FEATURE_ENGINE, "\n"
    )

    x_list <- all_windows_labeled$x
    xx_list <- all_windows_labeled$xx

    working_dataset <- Map(
      function(x, xx) list(x = x, xx = xx),
      x_list,
      xx_list
    )

    rm(all_windows_labeled, x_list, xx_list)
    gc()

    # Compute features.

    compute_one_feature_row <- function(obj) {
      x <- obj$x

      if (FEATURE_ENGINE == "fforma") {
        calc_features(list(x = x))$features
      } else if (FEATURE_ENGINE == "da") {
        xx <- obj$xx

        calc_features_with_da(
          seriesentry = list(x = x, xx = xx),
          period = period_i
        )$features
      } else {
        message("Unknown FEATURE_ENGINE: ", FEATURE_ENGINE)
      }
    }

    if (isTRUE(RUN_PARALLEL)) {
      chunk_size <- 5000L
      all_indices <- seq_len(n_windows)
      chunk_ids <- ceiling(all_indices / chunk_size)
      n_chunks <- max(chunk_ids)

      if (!dir.exists(features_cache_dir)) {
        dir.create(
          features_cache_dir,
          recursive = TRUE,
          showWarnings = FALSE
        )
      }

      chunk_feature_files <- file.path(
        features_cache_dir,
        sprintf("chunk_%03d.rds", seq_len(n_chunks))
      )

      n_workers <- autodetect_num_workers()

      cat(
        "[07a] Using PSOCK cluster with",
        n_workers,
        "workers and",
        n_chunks,
        "chunks\n"
      )

      cl <- parallel::makeCluster(
        n_workers,
        type = "PSOCK"
      )
      doParallel::registerDoParallel(cl)

      feature_rows <- tryCatch(
        {
          parallel::clusterEvalQ(
            cl,
            {
              source("src/r/utils.R")
              source("src/r/features.R")
              source("src/r/forecast_methods3.R")
              NULL
            }
          )

          for (chunk_id in seq_len(n_chunks)) {
            chunk_file <- chunk_feature_files[chunk_id]

            if (file.exists(chunk_file)) {
              cat(
                "[07a] Chunk",
                chunk_id,
                "of",
                n_chunks,
                "already exists; skipping\n"
              )
              next
            }

            idx <- which(chunk_ids == chunk_id)

            cat(
              "[07a] Processing chunk",
              chunk_id,
              "of",
              n_chunks,
              "with",
              length(idx),
              "windows\n"
            )

            chunk_rows <- foreach::foreach(
              obj = working_dataset[idx],
              .inorder = TRUE,
              .packages = c(
                "tsfeatures",
                "forecast",
                "tibble"
              )
            ) %dopar% {
              compute_one_feature_row(obj)
            }

            saveRDS(chunk_rows, chunk_file)

            cat(
              "[07a] Saved chunk",
              chunk_id,
              "of",
              n_chunks,
              "→",
              chunk_file,
              "\n"
            )

            rm(chunk_rows)
            gc()
          }

          chunk_rows <- lapply(
            chunk_feature_files,
            readRDS
          )

          do.call(c, chunk_rows)
        },
        finally = {
          parallel::stopCluster(cl)
          foreach::registerDoSEQ()
        }
      )
    } else {
      feature_rows <- lapply(
        working_dataset,
        compute_one_feature_row
      )
    }

    rm(working_dataset)
    gc()

    # Assemble and validate the feature matrix.

    feature_matrix <- dplyr::bind_rows(feature_rows)

    if (nrow(feature_matrix) != n_windows) {
      message(
        "Feature rows mismatch: got ",
        nrow(feature_matrix),
        " expected ",
        n_windows
      )
    }

    rm(feature_rows)
    gc()

    # Re-read the labelled windows and append the feature matrix.

    all_windows_labeled <- readRDS(labeled_i)

    all_windows_with_features <- dplyr::bind_cols(
      all_windows_labeled,
      feature_matrix
    )

    cat(
      "Final dataset:",
      nrow(all_windows_with_features),
      "rows x",
      ncol(all_windows_with_features),
      "cols\n"
    )

    saveRDS(all_windows_with_features, features_i)
    cat("Saved →", features_i, "\n")

    rm(
      all_windows_labeled,
      feature_matrix,
      all_windows_with_features
    )
    gc()
  }
}
