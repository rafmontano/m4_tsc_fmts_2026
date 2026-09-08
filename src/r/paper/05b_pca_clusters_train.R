# =====================================================================
# 05b_pca_clusters_train_l3.R
# Purpose:
#   PCA on training exports with k-means clustering in PC1–PC2,
#   looping across six frequencies (y, q, m, w, d, h).
#
# Preferred input (per frequency):
#   data/export/train_l{LABEL_ID}_{freq_tag}.csv
#
# Fallback input (per frequency):
#   data/export/windows_tsc_train_l{LABEL_ID}_{freq_tag}.csv
#
# Output (per frequency):
#   results/paper/figures/pca_pc1_pc2_clusters_train_l{LABEL_ID}_{freq_tag}.pdf
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(ggplot2)
})

source("src/r/utils.R")

# ---------------------------------------------------------------------
# 0) Label ID (prefer global)
# ---------------------------------------------------------------------

if (!exists("LABEL_ID")) {
  LABEL_ID <- 5
}
LABEL_ID <- as.integer(LABEL_ID)

# ---------------------------------------------------------------------
# 1) Frequencies
# ---------------------------------------------------------------------

target_periods <- c("Yearly", "Quarterly", "Monthly", "Weekly", "Daily", "Hourly")

# ---------------------------------------------------------------------
# 2) Clustering + plotting parameters
# ---------------------------------------------------------------------

set.seed(123)
k_clusters <- 3
max_points <- 5000

# ---------------------------------------------------------------------
# 3) Output directory
# ---------------------------------------------------------------------

fig_dir <- file.path("results", "paper", "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------
# 4) Helpers
# ---------------------------------------------------------------------

get_input_path <- function(label_id, tag) {
  preferred <- file.path("data", "export", sprintf("train_l%d_%s.csv", label_id, tag))
  fallback  <- file.path("data", "export", sprintf("windows_tsc_train_l%d_%s.csv", label_id, tag))
  
  if (file.exists(preferred)) return(preferred)
  if (file.exists(fallback))  return(fallback)
  NA_character_
}

drop_label_cols <- function(df, label_id) {
  candidates <- c("label", "true_label", paste0("l", label_id))
  keep <- setdiff(names(df), candidates)
  df[, keep, drop = FALSE]
}

remove_zero_variance <- function(df_num) {
  if (ncol(df_num) == 0) return(df_num)
  nzv <- vapply(df_num, function(x) stats::sd(x, na.rm = TRUE) > 0, logical(1))
  df_num[, nzv, drop = FALSE]
}

# ---------------------------------------------------------------------
# 5) Loop across frequencies
# ---------------------------------------------------------------------

for (tp in target_periods) {
  
  tag <- freq_tag(tp)
  
  input_csv <- get_input_path(LABEL_ID, tag)
  
  if (is.na(input_csv)) {
    message("[05b] Skipping ", tp, " (no input found for tag=", tag, ")")
    next
  }
  
  message("\n[05b] PCA clusters for ", tp, " (", tag, ")")
  message("      Input: ", input_csv)
  
  df <- readr::read_csv(input_csv, show_col_types = FALSE)
  
  df <- drop_label_cols(df, LABEL_ID)
  df_num <- df %>% dplyr::select(where(is.numeric))
  df_num <- remove_zero_variance(df_num)
  
  if (ncol(df_num) < 2) {
    message("[05b] Skipping ", tp, " (not enough numeric columns for PCA): ", input_csv)
    next
  }
  
  # -------------------------------------------------------------------
  # PCA
  # -------------------------------------------------------------------
  
  pca <- stats::prcomp(df_num, center = TRUE, scale. = TRUE)
  
  scores <- as_tibble(pca$x[, 1:2, drop = FALSE])
  colnames(scores) <- c("PC1", "PC2")
  
  # -------------------------------------------------------------------
  # k-means clustering on PC1–PC2
  # -------------------------------------------------------------------
  
  set.seed(123)
  km <- stats::kmeans(scores, centers = k_clusters, nstart = 20)
  
  scores <- scores %>% mutate(cluster = factor(km$cluster))
  
  # -------------------------------------------------------------------
  # Optional downsampling for plotting
  # -------------------------------------------------------------------
  
  set.seed(123)
  scores_plot <- if (nrow(scores) > max_points) {
    dplyr::sample_n(scores, max_points)
  } else {
    scores
  }
  
  # -------------------------------------------------------------------
  # Output file
  # -------------------------------------------------------------------
  
  fig_path <- file.path(
    fig_dir,
    sprintf("pca_pc1_pc2_clusters_train_l%d_%s.pdf", LABEL_ID, tag)
  )
  
  # -------------------------------------------------------------------
  # Plot PCA clusters + density contours
  # -------------------------------------------------------------------
  
  p <- ggplot(scores_plot, aes(PC1, PC2)) +
    stat_density_2d(
      colour    = "grey60",
      linewidth = 0.3,
      bins      = 6
    ) +
    geom_point(
      aes(colour = cluster),
      size  = 0.7,
      alpha = 0.7
    ) +
    scale_colour_brewer(
      palette = "Dark2",
      name    = "Cluster"
    ) +
    labs(x = "PC1", y = "PC2") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title      = element_blank(),
      legend.position = "right"
    )
  
  ggsave(fig_path, p, width = 7.0, height = 5.0)
  
  message("[05b] Clustered PCA plot saved to: ", fig_path)
}

message("\n[05b] All PCA cluster runs completed.")
