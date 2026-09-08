# ==============================================================================
# 05_run_lambda_sensitivity.R
#
# Purpose:
#   Evaluate terminal-horizon directional-adjustment sensitivity.
# Inputs:
#   Frequency-specific sensitivity datasets with forecasts and directions.
# Outputs:
#   Lambda-sensitivity result files for each frequency.
# Run from:
#   Project root, directly or through 99_sensitivity_run_all.R.
# ==============================================================================

source("src/r/sensitivity/00_sensitivity_common.R")

if (!exists("run_step_parallel")) {
  source("src/r/parallel_util.R")
}

# Accuracy functions ---------------------------------------------------------

calc_smape <- function(actual, forecast) {
  mean(
    200 * abs(actual - forecast) / (abs(actual) + abs(forecast)),
    na.rm = TRUE
  )
}

calc_mase <- function(x, actual, forecast, mase_freq = 1L) {
  denom <- mean(
    abs(x[(mase_freq + 1):length(x)] - x[1:(length(x) - mase_freq)]),
    na.rm = TRUE
  )

  mean(abs(actual - forecast) / denom, na.rm = TRUE)
}

calc_da <- function(actual, forecast, last_x) {
  actual_direction <- as.integer(tail(actual, 1) > last_x)
  forecast_direction <- as.integer(tail(forecast, 1) > last_x)

  as.numeric(actual_direction == forecast_direction)
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
    smape = calc_smape(xx, forecast),
    mase = calc_mase(
      x = x,
      actual = xx,
      forecast = forecast,
      mase_freq = get_m4_mase_freq(s$period)
    ),
    da = calc_da(xx, forecast, last_x)
  )
}

# Final-horizon adjustment ---------------------------------------------------

adjust_forecast <- function(base_forecast,
                            last_x,
                            mantis_final,
                            lambda_up,
                            lambda_down) {
  forecast_final_direction <- direction_label(tail(base_forecast, 1), last_x)

  gamma <- dplyr::case_when(
    forecast_final_direction == mantis_final ~ 1,
    forecast_final_direction != mantis_final & mantis_final == 1 ~ lambda_up,
    forecast_final_direction != mantis_final & mantis_final == 0 ~ lambda_down
  )

  gamma * base_forecast
}

# Baseline metrics -----------------------------------------------------------

evaluate_base_model <- function(dataset, model_name) {
  purrr::map_dfr(dataset, function(s) {
    calc_series_metrics(
      s = s,
      forecast = as.numeric(s$fct[[model_name]])
    )
  }) |>
    dplyr::summarise(
      model_id = model_name,
      lambda_up = NA_real_,
      lambda_down = NA_real_,
      smape = mean(smape, na.rm = TRUE),
      mase = mean(mase, na.rm = TRUE),
      da = mean(da, na.rm = TRUE),
      .groups = "drop"
    )
}

# Lambda sensitivity worker --------------------------------------------------

evaluate_lambda_pair <- function(lambda_row) {
  lambda_up <- lambda_row$lambda_up
  lambda_down <- lambda_row$lambda_down

  metrics_j <- purrr::map_dfr(SENS_DATASET, function(s) {
    x <- as.numeric(s$x)
    last_x <- tail(x, 1)

    base_forecast <- as.numeric(s$fct[[SENS_MODEL]])
    mantis_final <- tail(as.integer(s$direction$mantis), 1)

    adjusted <- adjust_forecast(
      base_forecast = base_forecast,
      last_x = last_x,
      mantis_final = mantis_final,
      lambda_up = lambda_up,
      lambda_down = lambda_down
    )

    calc_series_metrics(s, adjusted)
  })

  dplyr::summarise(
    metrics_j,
    model_id = paste0(SENS_MODEL, "_mantis"),
    lambda_up = lambda_up,
    lambda_down = lambda_down,
    smape = mean(smape, na.rm = TRUE),
    mase = mean(mase, na.rm = TRUE),
    da = mean(da, na.rm = TRUE),
    .groups = "drop"
  )
}

evaluate_sensitivity_model <- function(dataset, model_name) {
  jobs <- split(lambda_surface, seq_len(nrow(lambda_surface)))

  SENS_DATASET <<- dataset
  SENS_MODEL <<- model_name

  if (RUN_PARALLEL) {
    set_parallel_plan(TRUE)

    out <- run_step_parallel(
      dataset = jobs,
      step_fun = evaluate_lambda_pair,
      save_foldername = file.path(
        "data",
        "cache",
        "sensitivity",
        FREQ_TAG,
        model_name
      ),
      step_name = paste0("lambda_", FREQ_TAG, "_", model_name)
    )

    future::plan(future::sequential)
    result <- dplyr::bind_rows(out)
  } else {
    result <- purrr::map_dfr(jobs, evaluate_lambda_pair)
  }

  rm(SENS_DATASET, SENS_MODEL, envir = .GlobalEnv)
  gc()

  result
}

# OWA ------------------------------------------------------------------------

add_owa <- function(results) {
  naive2 <- results |> dplyr::filter(model_id == "naive2")

  results |>
    dplyr::mutate(
      relative_smape_vs_naive2 = smape / naive2$smape,
      relative_mase_vs_naive2 = mase / naive2$mase,
      owa_vs_naive2 = 0.5 * (
        relative_smape_vs_naive2 +
          relative_mase_vs_naive2
      )
    )
}

####
make_sensitivity_eval_dataset <- function(dataset) {
  lapply(dataset, function(s) {
    list(
      x = as.numeric(s$x),
      xx = as.numeric(s$xx),
      period = as.character(s$period),
      fct = list(
        naive2 = as.numeric(s$fct$naive2),
        fforma = as.numeric(s$fct$fforma),
        smyl = as.numeric(s$fct$smyl),
        chronos = as.numeric(s$fct$chronos),
        smyl_oracle = as.numeric(s$fct$smyl_oracle)
      ),
      direction = list(
        mantis = as.integer(s$direction$mantis)
      )
    )
  })
}

# Run all frequencies --------------------------------------------------------

for (period_use in PERIODS_TO_RUN) {
  set_sensitivity_frequency(period_use)
  dataset_raw <- read_sensitivity()
  dataset <- make_sensitivity_eval_dataset(dataset_raw)

  cat(
    "[05]", PERIOD_USE, "raw size:",
    format(object.size(dataset_raw), units = "auto"), "\n"
  )
  cat(
    "[05]", PERIOD_USE, "eval size:",
    format(object.size(dataset), units = "auto"), "\n"
  )

  rm(dataset_raw)
  gc()

  out_path <- file.path(
    results_dir,
    paste0("lambda_sensitivity_", FREQ_TAG, ".rds")
  )

  cat("[05]", PERIOD_USE, "base metrics\n")

  base_metrics <- dplyr::bind_rows(
    evaluate_base_model(dataset, "naive2"),
    evaluate_base_model(dataset, "fforma"),
    evaluate_base_model(dataset, "smyl"),
    evaluate_base_model(dataset, "chronos"),
    evaluate_base_model(dataset, "smyl_oracle")
  )

  cat("[05]", PERIOD_USE, "SMYL sensitivity\n")
  smyl_surface <- evaluate_sensitivity_model(dataset, "smyl")

  cat("[05]", PERIOD_USE, "Chronos sensitivity\n")
  chronos_surface <- evaluate_sensitivity_model(dataset, "chronos")

  sensitivity_results <- dplyr::bind_rows(
    base_metrics,
    smyl_surface,
    chronos_surface
  ) |>
    add_owa()

  saveRDS(sensitivity_results, out_path)

  cat("[05]", PERIOD_USE, "saved:", out_path, "\n")
  cat("[05]", PERIOD_USE, "rows:", nrow(sensitivity_results), "\n")

  rm(dataset, base_metrics, smyl_surface, chronos_surface, sensitivity_results)
  gc()
}
