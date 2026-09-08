# =====================================================================
# 02_add_smyl_forecasts.R
# Add SMYL, FFORMA and Naive2 forecasts inside each M4-style series object
# =====================================================================

source("src/r/sensitivity/00_sensitivity_common.R")
source("src/r/fct/01_fct_methods.R")

for (period_use in PERIODS_TO_RUN) {
  
  set_sensitivity_frequency(period_use)
  dataset <- read_sensitivity()
  
  dataset <- lapply(dataset, function(s) {
    if (is.null(s$fct)) s$fct <- list()
    
    s$fct$smyl <- as.numeric(s$pt_ff[SMYL_RANK, ])
    s$fct$fforma <- as.numeric(s$pt_ff[FFORMA_RANK, ])
    s$fct$naive2 <- as.numeric(
      naive2_forec(
        x = s$x,
        h = as.integer(s$h)
      )
    )
    
    s
  })
  
  write_sensitivity(dataset)
  
  cat("[02]", PERIOD_USE, "SMYL, FFORMA and Naive2 added\n")
}
