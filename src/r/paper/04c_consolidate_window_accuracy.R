# ==============================================================================
# 04c_consolidate_window_accuracy.R
#
# Purpose:
#   Consolidate accuracy tables across window modes.
# Inputs:
#   Window-specific model-accuracy CSV files under results/paper/tables.
# Outputs:
#   Consolidated CSV and RDS tables under results/paper/tables/total.
# Run from:
#   Project root.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
  library(readr)
})

# Configuration --------------------------------------------------------------

base_dir <- file.path("results", "paper", "tables")

# window_modes <- c("small", "default", "large", "full")
window_modes <- "default"

benchmark_models <- c("FFORMA", "SMYL")

input_file <- "model_accuracy_by_frequency.csv"

out_dir <- file.path(base_dir, "total")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_csv <- file.path(out_dir, "model_accuracy_by_frequency.csv")
out_rds <- file.path(out_dir, "model_accuracy_by_frequency.rds")

# Read one window mode -------------------------------------------------------

read_window_file <- function(window_mode) {
  path <- file.path(base_dir, window_mode, input_file)

  if (!file.exists(path)) {
    warning("Missing file: ", path)
    return(tibble())
  }

  df <- readr::read_csv(path, show_col_types = FALSE)

  if (nrow(df) == 0L) {
    return(tibble())
  }

  df %>%
    mutate(
      window_mode = window_mode,
      source_file = path
    )
}

# Read all window modes ------------------------------------------------------

df_all <- purrr::map_dfr(window_modes, read_window_file)

if (nrow(df_all) == 0L) {
  warning("No rows found across window modes.")
  readr::write_csv(tibble(), out_csv)
  saveRDS(tibble(), out_rds)
  quit(save = "no")
}

# Remove benchmark duplicates ------------------------------------------------
# FFORMA and SMYL are fixed forecasting benchmarks.
# Keep only their default rows, because values are repeated across
# small/default/large/full.

df_total <- df_all %>%
  filter(
    !(model %in% benchmark_models & window_mode != "default")
  ) %>%
  distinct()

# Save consolidated table ----------------------------------------------------

readr::write_csv(df_total, out_csv)
saveRDS(df_total, out_rds)

message("Saved consolidated CSV to: ", out_csv)
message("Saved consolidated RDS to: ", out_rds)

print(df_total)
