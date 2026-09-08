# =====================================================================
# 99_sensitivity_run_all.R
# Run complete sensitivity-analysis and paper-output pipeline
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("[99] Starting complete sensitivity pipeline\n")
cat("============================================================\n")


# ---------------------------------------------------------------------
# Chronos-2
#
# The model is already cached locally on this workstation.
# Keep Hugging Face offline during the experiment so forecasting does
# not perform unnecessary Hub requests or trigger rate limiting.
# ---------------------------------------------------------------------

Sys.setenv(
  HF_HUB_OFFLINE = "1"
)


# ---------------------------------------------------------------------
# Complete pipeline
#
# 01   Create M4 datasets
# 02   Add SMYL, FFORMA and Naive2
# 03   Add Chronos-2
# 04   Add Mantis directional predictions
# 04a  Add SMYL Oracle
# 05   Run lambda sensitivity
# 05a  Build Tables 1 and 2
# 06   Build sensitivity summary
# 07_8 Run all 07_1--07_7 robustness/sensitivity paper outputs
# 08   Identify Daily case-study candidates
# 09   Build Daily case-study figures
# 09a  Build up/down case-study candidates and final Figure 1
# ---------------------------------------------------------------------

pipeline_scripts <- c(
  "src/r/sensitivity/01_create_m4_forecast_dataset.R",
  "src/r/sensitivity/02_add_smyl_forecasts.R",
  "src/r/sensitivity/03_add_chronos_forecasts.R",
  "src/r/sensitivity/04_add_mantis_direction.R",
  "src/r/sensitivity/04a_add_smyl_oracle.R",
  "src/r/sensitivity/05_run_lambda_sensitivity.R",
  "src/r/sensitivity/05a_build_tables1_2_from_05.R",
  "src/r/sensitivity/06_build_sensitivity_summary.R",
  "src/r/sensitivity/07_8_run_all_paper_outputs.R",
  "src/r/sensitivity/08_identify_daily_case_studies.R",
  "src/r/sensitivity/09_daily_case_study_figures.R",
  "src/r/sensitivity/09a_up_down_case_study_figures.R"
)


# ---------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------

for (script_path in pipeline_scripts) {
  
  # Script 05 uses cached parallel lambda jobs.
  # Remove them immediately before the sensitivity grid is rerun so
  # results are always rebuilt from the current forecasts.
  if (identical(
    script_path,
    "src/r/sensitivity/05_run_lambda_sensitivity.R"
  )) {
    
    cat(
      "\n[99] Clearing previous sensitivity cache\n"
    )
    
    unlink(
      "data/cache/sensitivity",
      recursive = TRUE,
      force = TRUE
    )
  }
  
  
  cat(
    "\n------------------------------------------------------------\n"
  )
  
  cat(
    "[99] Running:",
    script_path,
    "\n"
  )
  
  cat(
    "------------------------------------------------------------\n"
  )
  
  
  source(
    script_path
  )
  
  
  cat(
    "[99] Completed:",
    script_path,
    "\n"
  )
  
  
  gc()
}


# ---------------------------------------------------------------------
# Completed
# ---------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("[99] Complete sensitivity pipeline completed\n")
cat("============================================================\n")


# ---------------------------------------------------------------------
# Main Tables 1 and 2
# ---------------------------------------------------------------------

cat("\n[99] Tables 1 and 2:\n")
cat("  results/paper/tables\n")

# ---------------------------------------------------------------------
# Sensitivity summary
# ---------------------------------------------------------------------

cat("\n[99] Sensitivity summary:\n")
cat("  results/sensitivity/sensitivity_summary_all.rds\n")
cat("  results/sensitivity/sensitivity_summary_all.csv\n")


# ---------------------------------------------------------------------
# Sensitivity paper outputs
# ---------------------------------------------------------------------

cat("\n[99] Sensitivity paper tables:\n")
cat("  results/sensitivity/paper/tables\n")

cat("\n[99] Sensitivity paper figures:\n")
cat("  results/sensitivity/paper/figures\n")


# ---------------------------------------------------------------------
# Case-study identification
# ---------------------------------------------------------------------

cat("\n[99] Case-study rankings:\n")
cat("  results/sensitivity/paper/case_studies\n")


# ---------------------------------------------------------------------
# Main paper case-study figures
# ---------------------------------------------------------------------

cat("\n[99] Main paper figures:\n")
cat("  results/paper/figures\n")


cat("\n[99] End-to-end run complete.\n")