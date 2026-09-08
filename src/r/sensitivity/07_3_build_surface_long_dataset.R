# ==============================================================================
# 07_3_build_surface_long_dataset.R
#
# Purpose:
#   Build the long-form lambda-sensitivity surface used by paper figures.
# Inputs:
#   Frequency-specific lambda-sensitivity results.
# Outputs:
#   A long-form sensitivity-surface RDS table.
# Run from:
#   Project root, directly or through 07_8_run_all_paper_outputs.R.
# ==============================================================================

source("src/r/sensitivity/00_sensitivity_common.R")

# Output folder --------------------------------------------------------------

table_dir <- file.path("results", "sensitivity", "paper", "tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

# This paper figure always uses all six M4 frequencies. Keep this list
# independent of PERIODS_TO_RUN, which controls potentially expensive
# sensitivity-analysis runs in 00_sensitivity_common.R.
SURFACE_PERIODS <- c(
  "Hourly", "Daily", "Weekly", "Monthly", "Quarterly", "Yearly"
)

# Helper functions -----------------------------------------------------------

pretty_model <- function(x) {
  dplyr::recode(
    x,
    "smyl" = "SMYL",
    "chronos" = "Chronos",
    "smyl_mantis" = "SMYL-MANTIS",
    "chronos_mantis" = "Chronos-MANTIS"
  )
}

model_base <- function(model_id) {
  dplyr::case_when(
    model_id == "smyl_mantis" ~ "smyl",
    model_id == "chronos_mantis" ~ "chronos",
    TRUE ~ NA_character_
  )
}

read_sensitivity_results <- function(period_use) {
  set_sensitivity_frequency(period_use)

  path <- file.path(
    results_dir,
    paste0("lambda_sensitivity_", FREQ_TAG, ".rds")
  )

  readRDS(path) |>
    dplyr::mutate(
      period = PERIOD_USE,
      freq_tag = FREQ_TAG,
      .before = 1
    )
}

add_improvement <- function(x) {
  base_rows <- x |>
    dplyr::filter(model_id %in% c("smyl", "chronos")) |>
    dplyr::select(
      period,
      freq_tag,
      base_model = model_id,
      base_owa = owa_vs_naive2
    )

  x |>
    dplyr::filter(model_id %in% c("smyl_mantis", "chronos_mantis")) |>
    dplyr::mutate(base_model = model_base(model_id)) |>
    dplyr::left_join(
      base_rows,
      by = c("period", "freq_tag", "base_model")
    ) |>
    dplyr::mutate(
      improvement_pct = 100 * (base_owa - owa_vs_naive2) / base_owa,
      model_label = pretty_model(model_id),
      base_model_label = pretty_model(base_model),
      period = factor(
        period,
        levels = c("Hourly", "Daily", "Weekly", "Monthly", "Quarterly", "Yearly")
      )
    ) |>
    dplyr::arrange(period, model_label, lambda_up, lambda_down) |>
    dplyr::mutate(period = as.character(period))
}

# Build long-form plotting surface -------------------------------------------

surface <- purrr::map_dfr(
  SURFACE_PERIODS,
  read_sensitivity_results
) |>
  add_improvement()

# Save -----------------------------------------------------------------------

out_csv <- file.path(table_dir, "sensitivity_surface_long.csv")
out_rds <- file.path(table_dir, "sensitivity_surface_long.rds")

readr::write_csv(surface, out_csv)
saveRDS(surface, out_rds)

cat("[07_3] Saved:", out_csv, "\n")
cat("[07_3] Saved:", out_rds, "\n")
cat("[07_3] Rows:", nrow(surface), "\n")
cat("[07_3] Frequencies:", paste(unique(surface$period), collapse = ", "), "\n")
