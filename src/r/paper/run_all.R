# ==============================================================================
# run_all.R
#
# Purpose:
#   Generate the tables and figures used by the current paper.
# Inputs:
#   Completed model-evaluation results and the M4 dataset.
# Outputs:
#   Paper tables and figures under results/paper.
# Run from:
#   Project root.
# ==============================================================================

paper_scripts <- c(
  "src/r/paper/01_m4_pivot_period_type.R",
  "src/r/paper/04_cd_diagram_models.R",
  "src/r/paper/05_accuracy_by_horizon_plots.R",
  "src/r/paper/06b_class_imbalance_bars_binary.R"
)

for (paper_script in paper_scripts) {
  cat("\nRunning:", paper_script, "\n")
  source(paper_script)
}

cat("\nPaper-output scripts executed successfully.\n")
