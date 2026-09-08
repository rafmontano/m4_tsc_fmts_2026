# ==============================================================================
# 05_pca.R
#
# Purpose:
#   Run PCA on training feature tables and plot the leading variables.
# Inputs:
#   Training feature CSV files under data/export.
# Outputs:
#   PCA figures under results/paper/figures.
# Run from:
#   Project root.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(purrr)
  library(ggplot2)
})

if (!requireNamespace("factoextra", quietly = TRUE)) {
  stop(
    "Package 'factoextra' is required for PCA plots.\n",
    "Install it with: install.packages('factoextra')"
  )
}
suppressPackageStartupMessages(library(factoextra))

source("src/r/utils.R")

# Label ID (prefer global) ---------------------------------------------------

LABEL_ID <- 5

# Frequencies ----------------------------------------------------------------

target_periods <- c("Yearly", "Quarterly", "Monthly", "Weekly", "Daily", "Hourly")

top_n <- 10

# Output directory -----------------------------------------------------------

fig_dir <- file.path("results", "paper", "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

fn <- function(suffix, freq_tag) {
  file.path(fig_dir, sprintf("pca_%s_l%d_%s.pdf", suffix, LABEL_ID, freq_tag))
}

# Helper functions -----------------------------------------------------------

drop_label_cols <- function(df, label_id) {
  candidates <- c(
    "label",
    "true_label",
    paste0("l", label_id)
  )
  keep <- setdiff(names(df), candidates)
  df[, keep, drop = FALSE]
}

get_numeric_predictors <- function(df) {
  df %>% dplyr::select(where(is.numeric))
}

# Preferred then fallback input paths
get_input_path <- function(label_id, tag) {
  preferred <- file.path("data", "export", sprintf("train_l%d_%s.csv", label_id, tag))
  fallback <- file.path("data", "export", sprintf("windows_tsc_train_l%d_%s.csv", label_id, tag))

  if (file.exists(preferred)) {
    return(preferred)
  }
  if (file.exists(fallback)) {
    return(fallback)
  }
  NA_character_
}

# Loop across frequencies ----------------------------------------------------

for (tp in target_periods) {
  tag <- freq_tag(tp)

  input_csv <- get_input_path(LABEL_ID, tag)

  if (is.na(input_csv)) {
    message("[05] Skipping ", tp, " (no input found for tag=", tag, ")")
    next
  }

  message("\n[05] PCA for ", tp, " (", tag, ")")
  message("     Input: ", input_csv)

  df <- readr::read_csv(input_csv, show_col_types = FALSE)

  df <- drop_label_cols(df, LABEL_ID)
  df_numeric <- get_numeric_predictors(df)

  if (ncol(df_numeric) < 2) {
    message("[05] Skipping ", tp, " (not enough numeric columns for PCA): ", input_csv)
    next
  }

  # Remove columns with zero variance (prcomp will fail otherwise)
  nzv <- vapply(df_numeric, function(x) stats::sd(x, na.rm = TRUE) > 0, logical(1))
  df_numeric <- df_numeric[, nzv, drop = FALSE]

  if (ncol(df_numeric) < 2) {
    message("[05] Skipping ", tp, " (all numeric cols zero-variance after filtering): ", input_csv)
    next
  }

  pca <- stats::prcomp(df_numeric, center = TRUE, scale. = TRUE)

  # Scree plot
  grDevices::pdf(fn("scree", tag), width = 12, height = 8)
  print(
    factoextra::fviz_eig(pca, addlabels = TRUE) +
      theme_minimal(base_size = 11)
  )
  grDevices::dev.off()

  # Top variable contributions
  grDevices::pdf(fn("contrib_pc1_top", tag), width = 12, height = 8)
  print(
    factoextra::fviz_contrib(pca, choice = "var", axes = 1, top = top_n) +
      theme_minimal(base_size = 11)
  )
  grDevices::dev.off()

  grDevices::pdf(fn("contrib_pc2_top", tag), width = 12, height = 8)
  print(
    factoextra::fviz_contrib(pca, choice = "var", axes = 2, top = top_n) +
      theme_minimal(base_size = 11)
  )
  grDevices::dev.off()

  message("[05] Completed ", tp, " (", tag, "). Figures saved to: ", fig_dir)
}

message("\n[05] All PCA runs completed.")
