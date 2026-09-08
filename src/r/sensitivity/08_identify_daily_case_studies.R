# ==============================================================================
# 08_identify_daily_case_studies.R
#
# Purpose:
#   Identify Daily case studies for the adjusted Chronos and SMYL forecasts.
# Inputs:
#   The Daily sensitivity dataset and reference adjustment parameters.
# Outputs:
#   Selected Daily case-study data under results/sensitivity.
# Run from:
#   Project root, directly or through 99_sensitivity_run_all.R.
# ==============================================================================

source("src/r/sensitivity/00_sensitivity_common.R")

set_sensitivity_frequency("Daily")
dataset <- read_sensitivity()

# Local evaluation helpers ---------------------------------------------------
#
# These reproduce the exact metric and adjustment logic used in Script 05
# without rerunning the lambda-sensitivity analysis.

calc_smape <- function(actual, forecast) {
  mean(
    200 * abs(actual - forecast) /
      (abs(actual) + abs(forecast)),
    na.rm = TRUE
  )
}

calc_mase <- function(x, actual, forecast, mase_freq = 1L) {
  denom <- mean(
    abs(
      x[(mase_freq + 1):length(x)] -
        x[1:(length(x) - mase_freq)]
    ),
    na.rm = TRUE
  )

  mean(
    abs(actual - forecast) / denom,
    na.rm = TRUE
  )
}

calc_da <- function(actual, forecast, last_x) {
  actual_direction <- as.integer(
    tail(actual, 1) > last_x
  )

  forecast_direction <- as.integer(
    tail(forecast, 1) > last_x
  )

  as.numeric(
    actual_direction == forecast_direction
  )
}

get_m4_mase_freq <- function(period) {
  dplyr::case_when(
    period == "Monthly" ~ 12L,
    period == "Quarterly" ~ 4L,
    period == "Hourly" ~ 24L,
    TRUE ~ 1L
  )
}

calc_series_metrics <- function(s, forecast) {
  x <- as.numeric(s$x)
  xx <- as.numeric(s$xx)
  last_x <- tail(x, 1)

  tibble::tibble(
    smape = calc_smape(
      xx,
      forecast
    ),
    mase = calc_mase(
      x = x,
      actual = xx,
      forecast = forecast,
      mase_freq = get_m4_mase_freq(s$period)
    ),
    da = calc_da(
      xx,
      forecast,
      last_x
    )
  )
}

adjust_forecast <- function(
  base_forecast,
  last_x,
  mantis_final,
  lambda_up,
  lambda_down
) {
  forecast_final_direction <- direction_label(
    tail(base_forecast, 1),
    last_x
  )

  gamma <- dplyr::case_when(
    forecast_final_direction == mantis_final ~ 1,
    forecast_final_direction != mantis_final &
      mantis_final == 1 ~ lambda_up,
    forecast_final_direction != mantis_final &
      mantis_final == 0 ~ lambda_down
  )

  gamma * base_forecast
}

# Best aggregate lambdas from sensitivity surface ----------------------------

surface <- readRDS(file.path(results_dir, "lambda_sensitivity_d.rds"))

get_best_lambda <- function(target_model_id) {
  surface |>
    dplyr::filter(model_id == target_model_id) |>
    dplyr::slice_min(owa_vs_naive2, n = 1, with_ties = FALSE) |>
    dplyr::select(lambda_up, lambda_down, owa_vs_naive2)
}

best_smyl <- get_best_lambda("smyl_mantis")
best_chronos <- get_best_lambda("chronos_mantis")

print(best_smyl)
print(best_chronos)

# Per-series diagnostic ------------------------------------------------------

score_one_series <- function(s, model_name, lambda_up, lambda_down) {
  x <- as.numeric(s$x)
  xx <- as.numeric(s$xx)
  last_x <- tail(x, 1)

  base_forecast <- as.numeric(s$fct[[model_name]])
  mantis_final <- tail(as.integer(s$direction$mantis), 1)
  base_final <- direction_label(tail(base_forecast, 1), last_x)
  actual_final <- direction_label(tail(xx, 1), last_x)

  adjusted <- adjust_forecast(
    base_forecast = base_forecast,
    last_x = last_x,
    mantis_final = mantis_final,
    lambda_up = lambda_up,
    lambda_down = lambda_down
  )

  base_metrics <- calc_series_metrics(s, base_forecast)
  adj_metrics <- calc_series_metrics(s, adjusted)

  tibble::tibble(
    st = as.character(s$st),
    local_id = s$local_id,
    global_id = s$global_id,
    period = s$period,
    model = model_name,
    lambda_up = lambda_up,
    lambda_down = lambda_down,
    base_final_direction = base_final,
    mantis_final_direction = mantis_final,
    actual_final_direction = actual_final,
    correction_type = dplyr::case_when(
      base_final == mantis_final ~ "none",
      base_final != mantis_final & mantis_final == 1 ~ "up",
      base_final != mantis_final & mantis_final == 0 ~ "down"
    ),
    base_smape = base_metrics$smape,
    adjusted_smape = adj_metrics$smape,
    smape_gain = base_metrics$smape - adj_metrics$smape,
    smape_gain_pct = 100 * (base_metrics$smape - adj_metrics$smape) / base_metrics$smape,
    base_mase = base_metrics$mase,
    adjusted_mase = adj_metrics$mase,
    mase_gain = base_metrics$mase - adj_metrics$mase,
    mase_gain_pct = 100 * (base_metrics$mase - adj_metrics$mase) / base_metrics$mase,
    base_da = base_metrics$da,
    adjusted_da = adj_metrics$da
  )
}

score_model <- function(model_name, best_lambda) {
  purrr::map_dfr(
    dataset,
    score_one_series,
    model_name = model_name,
    lambda_up = best_lambda$lambda_up,
    lambda_down = best_lambda$lambda_down
  )
}

case_scores <- dplyr::bind_rows(
  score_model("smyl", best_smyl),
  score_model("chronos", best_chronos)
)

# Save full per-series ranking -----------------------------------------------

out_dir <- file.path("results", "sensitivity", "paper", "case_studies")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(
  case_scores,
  file.path(out_dir, "daily_case_study_scores_all.csv")
)

# Recommended candidates -----------------------------------------------------

best_overall <- case_scores |>
  dplyr::filter(correction_type != "none") |>
  dplyr::group_by(model) |>
  dplyr::slice_max(mase_gain_pct, n = 20, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::arrange(model, dplyr::desc(mase_gain_pct))

best_up <- case_scores |>
  dplyr::filter(correction_type == "up") |>
  dplyr::group_by(model) |>
  dplyr::slice_max(mase_gain_pct, n = 20, with_ties = FALSE) |>
  dplyr::ungroup()

best_down <- case_scores |>
  dplyr::filter(correction_type == "down") |>
  dplyr::group_by(model) |>
  dplyr::slice_max(mase_gain_pct, n = 20, with_ties = FALSE) |>
  dplyr::ungroup()

readr::write_csv(best_overall, file.path(out_dir, "daily_case_study_best_overall.csv"))
readr::write_csv(best_up, file.path(out_dir, "daily_case_study_best_up.csv"))
readr::write_csv(best_down, file.path(out_dir, "daily_case_study_best_down.csv"))

cat("\nBest overall candidates:\n")
print(best_overall |> dplyr::select(model, st, local_id, correction_type, mase_gain_pct, smape_gain_pct, base_da, adjusted_da) |> head(20))

cat("\nBest upward candidates:\n")
print(best_up |> dplyr::select(model, st, local_id, correction_type, mase_gain_pct, smape_gain_pct, base_da, adjusted_da) |> head(20))

cat("\nBest downward candidates:\n")
print(best_down |> dplyr::select(model, st, local_id, correction_type, mase_gain_pct, smape_gain_pct, base_da, adjusted_da) |> head(20))
