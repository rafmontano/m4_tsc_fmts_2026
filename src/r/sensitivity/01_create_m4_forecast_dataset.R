# ==============================================================================
# 01_create_m4_forecast_dataset.R
#
# Purpose:
#   Create one M4 sensitivity dataset per configured frequency.
# Inputs:
#   The M4 dataset and shared sensitivity configuration.
# Outputs:
#   Frequency-specific sensitivity RDS files.
# Run from:
#   Project root, directly or through 99_sensitivity_run_all.R.
# ==============================================================================

source("src/r/sensitivity/00_sensitivity_common.R")

if (USE_TEST_SUBSET) {
  M4 <- readRDS("data/M4_subset_clean.rds")
} else {
  M4 <- M4comp2018::M4
}

for (period_use in PERIODS_TO_RUN) {
  set_sensitivity_frequency(period_use)

  global_idx <- which(vapply(M4, function(s) s$period == PERIOD_USE, logical(1)))
  dataset <- M4[global_idx]
  dataset <- add_series_ids(dataset, global_idx)

  attr(dataset, "period") <- PERIOD_USE
  attr(dataset, "freq_tag") <- FREQ_TAG
  attr(dataset, "global_idx") <- global_idx

  write_sensitivity(dataset)

  cat("[01]", PERIOD_USE, "saved:", sensitivity_rds, "\n")
  cat("[01]", PERIOD_USE, "series:", length(dataset), "\n")
}
