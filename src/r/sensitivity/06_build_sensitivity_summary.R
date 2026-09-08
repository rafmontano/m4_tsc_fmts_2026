# ==============================================================================
# 06_build_sensitivity_summary.R
#
# Purpose:
#   Summarise lambda-sensitivity results across adjusted models.
# Inputs:
#   Frequency-specific lambda-sensitivity result files.
# Outputs:
#   Sensitivity summary files.
# Run from:
#   Project root, directly or through 99_sensitivity_run_all.R.
# ==============================================================================

source("src/r/sensitivity/00_sensitivity_common.R")

summarise_one_adjusted_model <- function(x, base_model, adjusted_model) {
  base_row <- x |>
    dplyr::filter(model_id == base_model)

  adjusted <- x |>
    dplyr::filter(model_id == adjusted_model)

  reference_row <- adjusted |>
    dplyr::filter(
      abs(lambda_up - reference_lambda_up) < 1e-9,
      abs(lambda_down - reference_lambda_down) < 1e-9
    )

  best_row <- adjusted |>
    dplyr::slice_min(owa_vs_naive2, n = 1, with_ties = FALSE)

  adjusted |>
    dplyr::summarise(
      base_model = base_model,
      adjusted_model = adjusted_model,
      base_owa = base_row$owa_vs_naive2,
      reference_lambda_up = reference_lambda_up,
      reference_lambda_down = reference_lambda_down,
      reference_owa = reference_row$owa_vs_naive2,
      reference_da = reference_row$da,
      reference_improvement_pct =
        100 * (base_row$owa_vs_naive2 - reference_row$owa_vs_naive2) /
          base_row$owa_vs_naive2,
      best_lambda_up = best_row$lambda_up,
      best_lambda_down = best_row$lambda_down,
      best_owa = best_row$owa_vs_naive2,
      best_da = best_row$da,
      best_improvement_pct =
        100 * (base_row$owa_vs_naive2 - best_row$owa_vs_naive2) /
          base_row$owa_vs_naive2,
      median_owa = median(owa_vs_naive2, na.rm = TRUE),
      median_improvement_pct =
        100 * (base_row$owa_vs_naive2 - median(owa_vs_naive2, na.rm = TRUE)) /
          base_row$owa_vs_naive2,
      pct_surface_improves =
        mean(owa_vs_naive2 < base_row$owa_vs_naive2, na.rm = TRUE) * 100,
      n_lambda_pairs = dplyr::n(),
      .groups = "drop"
    )
}

summary_all <- purrr::map_dfr(PERIODS_TO_RUN, function(period_use) {
  set_sensitivity_frequency(period_use)

  sensitivity_results_rds <- file.path(
    results_dir,
    paste0("lambda_sensitivity_", FREQ_TAG, ".rds")
  )

  x <- readRDS(sensitivity_results_rds)

  dplyr::bind_rows(
    summarise_one_adjusted_model(x, "smyl", "smyl_mantis"),
    summarise_one_adjusted_model(x, "chronos", "chronos_mantis")
  ) |>
    dplyr::mutate(
      period = PERIOD_USE,
      freq_tag = FREQ_TAG,
      .before = 1
    )
})

out_rds <- file.path("results", "sensitivity", "sensitivity_summary_all.rds")
out_csv <- file.path("results", "sensitivity", "sensitivity_summary_all.csv")

saveRDS(summary_all, out_rds)
readr::write_csv(summary_all, out_csv)

cat("[06] Saved:", out_rds, "\n")
cat("[06] Saved:", out_csv, "\n")
cat("[06] Rows:", nrow(summary_all), "\n")
