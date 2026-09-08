# ==============================================================================
# 07_2_build_robustness_latex_table.R
#
# Purpose:
#   Render the sensitivity robustness summary as a LaTeX table.
# Inputs:
#   The sensitivity robustness RDS table.
# Outputs:
#   A LaTeX table under results/sensitivity/paper/tables.
# Run from:
#   Project root, directly or through 07_8_run_all_paper_outputs.R.
# ==============================================================================

source("src/r/sensitivity/00_sensitivity_common.R")

table_dir <- file.path("results", "sensitivity", "paper", "tables")

robustness_rds <- file.path(table_dir, "sensitivity_robustness_table.rds")
robustness_table <- readRDS(robustness_rds)

latex_df <- robustness_table |>
  dplyr::transmute(
    Frequency = period,
    Base = base_model_label,
    Adjusted = adjusted_model_label,
    `Base OWA` = sprintf("%.3f", base_owa),
    `Best OWA` = sprintf("%.3f", best_owa),
    `$\\lambda_{Up}$` = sprintf("%.3f", best_lambda_up),
    `$\\lambda_{Down}$` = sprintf("%.3f", best_lambda_down),
    `Best imp. (\\%)` = sprintf("%.2f", best_improvement_pct),
    `Surface imp. (\\%)` = sprintf("%.2f", surface_improving_pct),
    `$\\lambda_{Up}$ range` = lambda_up_within_1pct_best,
    `$\\lambda_{Down}$ range` = lambda_down_within_1pct_best
  )

latex_lines <- c(
  "\\begin{table*}[t]",
  "\\centering",
  "\\caption{Sensitivity of directional adjustment to $\\lambda_{Up}$ and $\\lambda_{Down}$.}",
  "\\label{tab:sensitivity_robustness}",
  "\\small",
  "\\begin{tabular}{lllrrrrrrll}",
  "\\toprule",
  "Frequency & Base & Adjusted & Base OWA & Best OWA & $\\lambda_{Up}$ & $\\lambda_{Down}$ & Best imp. (\\%) & Surface imp. (\\%) & $\\lambda_{Up}$ range & $\\lambda_{Down}$ range \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(latex_df))) {
  latex_lines <- c(
    latex_lines,
    paste(
      latex_df$Frequency[i],
      latex_df$Base[i],
      latex_df$Adjusted[i],
      latex_df$`Base OWA`[i],
      latex_df$`Best OWA`[i],
      latex_df$`$\\lambda_{Up}$`[i],
      latex_df$`$\\lambda_{Down}$`[i],
      latex_df$`Best imp. (\\%)`[i],
      latex_df$`Surface imp. (\\%)`[i],
      latex_df$`$\\lambda_{Up}$ range`[i],
      latex_df$`$\\lambda_{Down}$ range`[i],
      sep = " & "
    ) |>
      paste0(" \\\\")
  )
}

latex_lines <- c(
  latex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table*}"
)

out_tex <- file.path(table_dir, "sensitivity_robustness_table.tex")

writeLines(latex_lines, out_tex)

cat("[07_2] Saved:", out_tex, "\n")
