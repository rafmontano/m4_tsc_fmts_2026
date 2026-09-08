# =====================================================================
# 01_create_m4_forecast_dataset.R
# Create one M4-style sensitivity dataset per frequency
# =====================================================================

source("src/r/sensitivity/00_sensitivity_common.R")

library(M4comp2018)
data(M4)

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
