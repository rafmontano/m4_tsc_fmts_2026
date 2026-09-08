# ==============================================================================
# 04d_best_window_last_horizon_cd.R
#
# Purpose:
#   Select best-window last-horizon results and build a critical-difference diagram.
# Inputs:
#   The consolidated window-mode accuracy table.
# Outputs:
#   Best-window tables and a critical-difference figure.
# Run from:
#   Project root.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(readr)
})

if (!requireNamespace("scmamp", quietly = TRUE)) {
  message("Package 'scmamp' is required. Install with install.packages('scmamp').")
}
suppressPackageStartupMessages(library(scmamp))

# Configuration --------------------------------------------------------------

base_dir <- file.path("results", "paper", "tables", "total")
fig_dir <- file.path("results", "paper", "figures", "total")

dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

input_csv <- file.path(base_dir, "model_accuracy_by_frequency.csv")

out_best_csv <- file.path(
  base_dir,
  "model_accuracy_best_window_last_horizon.csv"
)

out_best_rds <- file.path(
  base_dir,
  "model_accuracy_best_window_last_horizon.rds"
)

out_cd_csv <- file.path(
  base_dir,
  "model_accuracy_best_window_last_horizon_cd.csv"
)

fig_path <- file.path(
  fig_dir,
  "cd_diagram_best_window_last_horizon.pdf"
)

benchmark_models <- c("FFORMA", "SMYL")

frequency_levels <- c(
  "Hourly",
  "Daily",
  "Weekly",
  "Monthly",
  "Quarterly",
  "Yearly"
)

preferred_model_order <- c(
  "FFORMA",
  "SMYL",
  "XGBoost",
  "DTW_1NN",
  "EUCLIDEAN",
  "RotF",
  "ROCKET",
  "MANTIS",
  "InceptionTime",
  "CHRONOS"
)

# Read consolidated results --------------------------------------------------

if (!file.exists(input_csv)) {
  message("Input file not found: ", input_csv)
}

df <- readr::read_csv(input_csv, show_col_types = FALSE)

if (nrow(df) == 0L) {
  message("Input file is empty: ", input_csv)
}

required_cols <- c("window_mode", "frequency", "model", "horizon", "accuracy")

missing_cols <- setdiff(required_cols, names(df))

if (length(missing_cols) > 0L) {
  message(
    "Missing required columns: ",
    paste(missing_cols, collapse = ", ")
  )
}

# Keep last horizon per frequency/model/window mode --------------------------

df_last <- df %>%
  mutate(
    frequency = as.character(frequency),
    model     = as.character(model),
    horizon   = as.integer(horizon),
    accuracy  = as.numeric(accuracy)
  ) %>%
  filter(!is.na(frequency), !is.na(model), !is.na(horizon), !is.na(accuracy)) %>%
  group_by(frequency, model, window_mode) %>%
  filter(horizon == max(horizon, na.rm = TRUE)) %>%
  ungroup()

# Select best window mode per frequency/model --------------------------------
# For FFORMA and SMYL:
#   Keep default only because they are fixed forecasting benchmarks.
#
# For all other models:
#   Keep the row with the highest last-horizon accuracy across window modes.

df_best <- df_last %>%
  filter(
    !(model %in% benchmark_models & window_mode != "default")
  ) %>%
  group_by(frequency, model) %>%
  arrange(desc(accuracy), window_mode, .by_group = TRUE) %>%
  slice(1L) %>%
  ungroup() %>%
  mutate(
    frequency = factor(frequency, levels = frequency_levels),
    model = factor(
      model,
      levels = unique(c(
        preferred_model_order,
        sort(setdiff(unique(as.character(model)), preferred_model_order))
      ))
    )
  ) %>%
  arrange(frequency, model) %>%
  mutate(
    frequency = as.character(frequency),
    model = as.character(model)
  )

readr::write_csv(df_best, out_best_csv)
saveRDS(df_best, out_best_rds)

message("Saved best-window last-horizon table to: ", out_best_csv)
print(df_best)

# Build CD input table: frequency × model ------------------------------------

models_complete <- df_best %>%
  distinct(frequency, model) %>%
  group_by(model) %>%
  summarise(n = n_distinct(frequency), .groups = "drop") %>%
  filter(n == length(unique(df_best$frequency))) %>%
  pull(model)

if (length(models_complete) == 0L) {
  warning("No complete models available for CD diagram.")
  readr::write_csv(tibble(frequency = unique(df_best$frequency)), out_cd_csv)
  quit(save = "no")
}

model_levels <- unique(c(
  preferred_model_order,
  sort(setdiff(models_complete, preferred_model_order))
))

df_cd <- df_best %>%
  filter(model %in% models_complete) %>%
  mutate(
    frequency = factor(frequency, levels = frequency_levels),
    model = factor(model, levels = model_levels)
  ) %>%
  select(frequency, model, accuracy)

wide_cd <- df_cd %>%
  tidyr::pivot_wider(
    names_from = model,
    values_from = accuracy
  ) %>%
  arrange(frequency) %>%
  mutate(frequency = as.character(frequency))

readr::write_csv(wide_cd, out_cd_csv)

message("Saved CD input table to: ", out_cd_csv)
print(wide_cd)

# Produce CD diagram ---------------------------------------------------------

if (length(models_complete) < 2L) {
  warning("Only one complete model available. CD plot requires >= 2 models.")
  quit(save = "no")
}

mat <- as.data.frame(wide_cd)
rn <- mat$frequency
mat <- as.matrix(mat[, -1, drop = FALSE])
rownames(mat) <- as.character(rn)

pdf(fig_path, width = 8, height = 4)
scmamp::plotCD(
  results.matrix = mat,
  alpha = 0.05,
  cex = 0.75,
  reverse = TRUE
)
dev.off()

message("Saved CD diagram to: ", fig_path)
