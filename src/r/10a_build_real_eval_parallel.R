# ==============================================================================
# 10a_build_real_eval_parallel.R
#
# Purpose:
#   Build the real-series evaluation dataset using parallel feature computation.
# Inputs:
#   Global configuration and the cleaned M4 dataset in subset_clean_file.
# Outputs:
#   One real_eval_with_features_<frequency>_<window_mode>.rds file per run.
# Run from:
#   Project root, through 00_main_new.R.
# ==============================================================================

# Dependencies ----------------------------------------------------------------

source("src/r/features.R")
source("src/r/forecast_methods3.R")
source("src/r/utils.R")

# Main execution --------------------------------------------------------------

if (!file.exists(subset_clean_file)) {
  message("Missing cleaned dataset: ", subset_clean_file)
}

M4_clean_all <- readRDS(subset_clean_file)

for (period_i in periods) {
  TAG_i <- freq_tag(period_i)

  for (window_mode_i in window_modes) {
    WINDOW_TAG_i <- window_tag(window_mode_i)

    real_eval_file_i <- file.path(
      data_dir,
      paste0("real_eval_with_features_", TAG_i, "_", WINDOW_TAG_i, ".rds")
    )

    cat("\n[10a] Period =", period_i, "| window_mode =", window_mode_i, "\n")

    M4_period <- Filter(function(s) as.character(s$period) == period_i, M4_clean_all)
    n_series <- length(M4_period)

    cat("[10a] Series =", n_series, "\n")

    # Build lightweight base rows without exporting M4_period to each worker.

    build_base_row <- function(i) {
      s <- M4_period[[i]]

      x_full <- as.numeric(unlist(s$x, use.names = FALSE))
      xx_full <- as.numeric(unlist(s$xx, use.names = FALSE))

      if (length(x_full) < 1L) {
        return(NULL)
      }
      if (length(xx_full) < 1L) {
        return(NULL)
      }

      if (window_mode_i == "full") {
        x_win <- x_full
      } else {
        window_size_i <- get_window_size_from_h(period_i, window_mode_i)

        if (length(x_full) >= window_size_i) {
          x_win <- tail(x_full, window_size_i)
        } else {
          pad_n <- window_size_i - length(x_full)
          x_win <- c(rep(x_full[1], pad_n), x_full)
        }
      }

      scaled <- scale_pair_std(x_win, xx_full)

      list(
        series_id   = i,
        series_name = if (!is.null(s$st)) as.character(s$st) else NA_character_,
        x           = as.numeric(scaled$x_std),
        xx          = as.numeric(scaled$xx_std),
        labels      = compute_label_vector(scaled$x_std, scaled$xx_std)
      )
    }

    base_rows <- lapply(seq_len(n_series), build_base_row)
    base_rows <- base_rows[!vapply(base_rows, is.null, logical(1))]

    rm(M4_period)
    gc()

    cat("[10a] Valid base rows:", length(base_rows), "\n")

    if (length(base_rows) == 0L) {
      empty_df <- tibble::tibble(
        series_id = integer(),
        series_name = character(),
        x = list(),
        xx = list(),
        labels = list()
      )

      saveRDS(empty_df, real_eval_file_i)
      cat("[10a] Saved empty REAL dataset →", real_eval_file_i, "\n")

      rm(empty_df, base_rows)
      gc()
      next
    }

    # Compute features and assemble the final rows in parallel.

    compute_one_row <- function(row) {
      x_i <- as.numeric(row$x)
      xx_i <- as.numeric(row$xx)

      featrow <- tryCatch(
        {
          fr <- calc_features(
            list(x = x_i)
          )$features

          if (FEATURE_ENGINE == "da") {
            h <- length(xx_i)
            n <- length(x_i)

            if (n > h) {
              x_hist <- head(x_i, n - h)
              xx_hist <- tail(x_i, h)

              fr <- cbind(
                fr,
                da_features(
                  x = x_hist,
                  xx = xx_hist,
                  h = h,
                  period = period_i
                )
              )
            } else {
              fr <- cbind(
                fr,
                data.frame(
                  da_mda_arima = 0.5,
                  da_mdv_arima = 0,
                  da_mdpv_arima = 0,
                  da_pt_pvalue_arima = 1,
                  da_mda_ets = 0.5,
                  da_mdv_ets = 0,
                  da_mdpv_ets = 0,
                  da_pt_pvalue_ets = 1
                )
              )
            }
          }

          fr
        },
        error = function(e) {
          return(NULL)
        }
      )

      if (is.null(featrow)) {
        return(NULL)
      }

      tibble::tibble(
        series_id   = row$series_id,
        series_name = row$series_name,
        x           = list(x_i),
        xx          = list(xx_i),
        labels      = list(as.integer(row$labels))
      ) |>
        dplyr::bind_cols(featrow)
    }

    rows <- run_step_parallel(
      dataset = base_rows,
      step_fun = compute_one_row,
      chunk_size = 2000L,
      save_foldername = file.path(data_dir, paste0("cache_real_", TAG_i, "_", WINDOW_TAG_i)),
      step_name = paste0("real_features_", TAG_i, "_", WINDOW_TAG_i)
    )

    real_eval_df <- dplyr::bind_rows(rows)

    cat("[10a] Final REAL dataset:", nrow(real_eval_df), "rows\n")

    saveRDS(real_eval_df, real_eval_file_i)
    cat("[10a] Saved →", real_eval_file_i, "\n")

    rm(base_rows, rows, real_eval_df)
    gc()
  }
}
