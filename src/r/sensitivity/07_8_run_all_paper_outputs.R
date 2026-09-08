# ==============================================================================
# 07_8_run_all_paper_outputs.R
#
# Purpose:
#   Run all sensitivity paper-output scripts sequentially.
# Inputs:
#   Completed sensitivity-analysis result files.
# Outputs:
#   Sensitivity paper tables and figures.
# Run from:
#   Project root, directly or through 99_sensitivity_run_all.R.
# ==============================================================================

cat("\n[07_8] Starting sensitivity paper-output pipeline\n")

paper_scripts <- c(
  "src/r/sensitivity/07_1_build_robustness_table.R",
  "src/r/sensitivity/07_2_build_robustness_latex_table.R",
  "src/r/sensitivity/07_3_build_surface_long_dataset.R",
  "src/r/sensitivity/07_4_plot_heatmaps_all_model_frequency.R",
  "src/r/sensitivity/07_5_plot_daily_two_panel_heatmap.R",
  "src/r/sensitivity/07_6_plot_daily_contour.R",
  "src/r/sensitivity/07_7_plot_best_improvement_by_frequency.R"
)

for (paper_script_path in paper_scripts) {
  cat("\n[07_8] Running:", paper_script_path, "\n")
  source(paper_script_path)
  cat("[07_8] Completed:", paper_script_path, "\n")
  gc()
}

cat("\n[07_8] Sensitivity paper-output pipeline completed\n")
cat("[07_8] Outputs saved under:\n")
cat("  results/sensitivity/paper/tables\n")
cat("  results/sensitivity/paper/figures\n")
