# =====================================================================
# 03_add_chronos_forecasts.R
# Add Chronos forecasts inside each M4-style series object
# =====================================================================

source("src/r/sensitivity/00_sensitivity_common.R")

chronos_add_one <- function(s) {
  if (is.null(s$fct)) s$fct <- list()
  
  s$fct$chronos <- as.numeric(
    chronos_forec(
      x = s$x,
      h = as.integer(s$h)
    )
  )
  
  s
}

for (period_use in PERIODS_TO_RUN) {
  
  set_sensitivity_frequency(period_use)
  dataset <- read_sensitivity()
  
  cat("[03]", PERIOD_USE, "Chronos series:", length(dataset), "\n")
  
  if (RUN_PARALLEL) {
    
    cl <- parallel::makeCluster(CHRONOS_WORKERS)
    
    parallel::clusterEvalQ(cl, {
      source("src/r/sensitivity/configure_chronos_python.R")
      configure_chronos_python()
      source("src/r/fct/01_fct_methods.R")
      NULL
    })
    
    parallel::clusterExport(
      cl,
      varlist = "chronos_add_one",
      envir = environment()
    )
    
    dataset <- parallel::parLapplyLB(cl, dataset, chronos_add_one)
    
    parallel::stopCluster(cl)
    
  } else {
    
    source("src/r/sensitivity/configure_chronos_python.R")
    configure_chronos_python()
    source("src/r/fct/01_fct_methods.R")
    
    dataset <- lapply(dataset, chronos_add_one)
  }
  
  write_sensitivity(dataset)
  
  cat("[03]", PERIOD_USE, "Chronos added and saved:", sensitivity_rds, "\n")
  
  rm(dataset)
  gc()
}

