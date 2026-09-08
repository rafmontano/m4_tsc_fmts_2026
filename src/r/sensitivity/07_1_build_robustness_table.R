# =====================================================================
# 07_1_build_robustness_table.R
# Build paper-facing robustness table for lambda sensitivity analysis
# =====================================================================

source("src/r/sensitivity/00_sensitivity_common.R")

# ---------------------------------------------------------------------
# Output folders
# ---------------------------------------------------------------------

paper_dir <- file.path("results", "sensitivity", "paper")
table_dir <- file.path(paper_dir, "tables")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------

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

lambda_range_label <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return("--")
  }
  
  paste0(
    sprintf("%.3f", min(x, na.rm = TRUE)),
    "--",
    sprintf("%.3f", max(x, na.rm = TRUE))
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
      base_model_label = pretty_model(base_model)
    )
}

# ---------------------------------------------------------------------
# Load sensitivity surfaces
# ---------------------------------------------------------------------

all_results <- purrr::map_dfr(
  PERIODS_TO_RUN,
  read_sensitivity_results
)

surface <- add_improvement(all_results)

# ---------------------------------------------------------------------
# Build robustness table
# ---------------------------------------------------------------------

robustness_table <- surface |>
  dplyr::group_by(period, freq_tag, base_model, model_id) |>
  dplyr::group_modify(function(.x, .y) {
    
    best_row <- .x |>
      dplyr::slice_max(improvement_pct, n = 1, with_ties = FALSE)
    
    reference_row <- .x |>
      dplyr::filter(
        abs(lambda_up - reference_lambda_up) < 1e-9,
        abs(lambda_down - reference_lambda_down) < 1e-9
      )
    
    within_1pct_best <- .x |>
      dplyr::filter(owa_vs_naive2 <= best_row$owa_vs_naive2 * 1.01)
    
    tibble::tibble(
      base_model_label = pretty_model(.y$base_model),
      adjusted_model_label = pretty_model(.y$model_id),
      base_owa = best_row$base_owa,
      best_owa = best_row$owa_vs_naive2,
      best_lambda_up = best_row$lambda_up,
      best_lambda_down = best_row$lambda_down,
      best_improvement_pct = best_row$improvement_pct,
      reference_lambda_up = reference_lambda_up,
      reference_lambda_down = reference_lambda_down,
      reference_owa = reference_row$owa_vs_naive2,
      reference_improvement_pct = reference_row$improvement_pct,
      surface_improving_pct = mean(.x$improvement_pct > 0, na.rm = TRUE) * 100,
      lambda_up_within_1pct_best = lambda_range_label(within_1pct_best$lambda_up),
      lambda_down_within_1pct_best = lambda_range_label(within_1pct_best$lambda_down),
      n_lambda_pairs = nrow(.x)
    )
  }) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    period = factor(
      period,
      levels = c("Hourly", "Daily", "Weekly", "Monthly", "Quarterly", "Yearly")
    )
  ) |>
  dplyr::arrange(period, base_model_label) |>
  dplyr::mutate(period = as.character(period))

# ---------------------------------------------------------------------
# Save CSV and RDS
# ---------------------------------------------------------------------

out_csv <- file.path(table_dir, "sensitivity_robustness_table.csv")
out_rds <- file.path(table_dir, "sensitivity_robustness_table.rds")

readr::write_csv(robustness_table, out_csv)
saveRDS(robustness_table, out_rds)

cat("[07_1] Saved:", out_csv, "\n")
cat("[07_1] Saved:", out_rds, "\n")
cat("[07_1] Rows:", nrow(robustness_table), "\n")