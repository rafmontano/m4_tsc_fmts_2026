# Inspect the stored inputs for Figure 2's Daily focused plot.
# Read-only: creates no data, models, tables or figures. Base R only.
# Run from the cleaned project root, or set project_root below.
project_root <- "/Users/monta/Documents/Projects/m4_tsc_fmts_2026"

# These are RESULT files, not fitted models.
input_files <- c(
  Mantis = "results/mantis/mantis_eval_d.rds",
  `Chronos-2` = "results/chronos/chronos_eval_d.rds",
  `1-NN DTW` = "results/dtw/dtw_eval_d.rds",
  SMYL = "results/smyl/smyl_eval_d.rds",
  `Majority baseline` = "results/feedback/majority/majority_eval_d.rds"
)

# Each RDS is a container. The plot reads this table inside it:
# object$default$real$summary
# It needs horizon_id (or horizon) and accuracy, stored as 0--1.
# Example SHAPE ONLY for the future baseline output (do not save NA values):
# baseline_summary <- data.frame(horizon_id = 1:14, accuracy = NA_real_)
# baseline_object <- list(default = list(real = list(summary = baseline_summary)))
# Once REAL baseline accuracies have been calculated, their producer will save:
# saveRDS(baseline_object,
#         file.path(project_root, input_files[["Majority baseline"]]))
# This inspection script deliberately does not calculate or save that baseline.

plot_columns <- data.frame(horizon = integer())

for (model in names(input_files)) {
  path <- file.path(project_root, input_files[[model]])
  cat("\n", model, "\nFile: ", path, "\n", sep = "")
  if (!file.exists(path)) {
    cat("NOT PRESENT: ",
        if (model == "Majority baseline") "this is the NEW result file still to produce.\n"
        else "this existing result file is required for this plot.\n", sep = "")
    next
  }
  object <- readRDS(path)
  cat("Top-level sections: ", paste(names(object), collapse = ", "), "\n", sep = "")
  summary <- object$default$real$summary
  if (!is.data.frame(summary)) stop("Missing default$real$summary: ", path)
  cat("Summary columns: ", paste(names(summary), collapse = ", "), "\n", sep = "")
  horizon_col <- intersect(c("horizon_id", "horizon"), names(summary))[1]
  if (is.na(horizon_col) || !"accuracy" %in% names(summary)) {
    stop("Required columns missing: ", path)
  }
  rows <- data.frame(horizon = as.integer(summary[[horizon_col]]))
  rows[[model]] <- as.numeric(summary$accuracy)
  if (anyDuplicated(rows$horizon) || anyNA(rows) ||
      any(!is.finite(rows[[model]])) || any(rows[[model]] < 0 | rows[[model]] > 1) ||
      !setequal(rows$horizon, 1:14)) stop("Invalid Daily horizon/accuracy data: ", path)
  print(rows, row.names = FALSE)
  plot_columns <- merge(plot_columns, rows, by = "horizon", all = TRUE)
  rm(object)
}

cat("\nCOMBINED VIEW: the numbers the Daily focused plot uses\n")
print(plot_columns, row.names = FALSE)
cat("\nWHERE TO ADD THE NEW DATA\n",
    "Add a separate Majority baseline result file at the path printed above.\n",
    "Do not add baseline rows to Mantis, Chronos-2, DTW or SMYL files.\n",
    "The updated plotting script already recognises that separate file.\n",
    "The copied files in data/feedback are upstream inputs for calculating it;\n",
    "the plotting script does not read those training/evaluation datasets.\n",
    "No files were written by this inspection.\n", sep = "")
