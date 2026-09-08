# =====================================================================
# 04a_add_smyl_oracle.R
#
# Add the infeasible ex-post SMYL Oracle forecast used as a diagnostic
# upper-bound / headroom benchmark.
#
# For each M4 series independently, search:
#
#   m = 0.500, 0.501, ..., 1.500
#
# and select the multiplier that minimises realised SMYL sMAPE over
# the complete official forecast horizon.
#
# IMPORTANT:
#   - The Oracle does NOT use Mantis.
#   - The Oracle does NOT use directional disagreement.
#   - One multiplier is selected independently for each series.
#   - The same selected multiplier scales all official horizons of
#     that series.
#   - This is an infeasible ex-post diagnostic, not a forecasting model.
# =====================================================================


source("src/r/sensitivity/00_sensitivity_common.R")


# ---------------------------------------------------------------------
# 1. Configuration
# ---------------------------------------------------------------------

oracle_grid <- seq(
  0.500,
  1.500,
  by = 0.001
)

EXPECTED_ORACLE_GRID_SIZE <- 1001L

stopifnot(
  length(oracle_grid) == EXPECTED_ORACLE_GRID_SIZE
)


# ---------------------------------------------------------------------
# 2. sMAPE
# ---------------------------------------------------------------------

calc_smape_oracle <- function(actual, forecast) {
  
  mean(
    200 *
      abs(actual - forecast) /
      (abs(actual) + abs(forecast)),
    na.rm = TRUE
  )
}


# ---------------------------------------------------------------------
# 3. Oracle for one series
# ---------------------------------------------------------------------

add_smyl_oracle_one <- function(s) {
  
  if (is.null(s$fct$smyl)) {
    stop(
      "SMYL forecast missing for series ",
      s$st,
      ". Run 02_add_smyl_forecasts.R first."
    )
  }
  
  
  actual <- as.numeric(s$xx)
  smyl_fc <- as.numeric(s$fct$smyl)
  
  
  if (length(actual) != length(smyl_fc)) {
    stop(
      "Forecast-horizon mismatch for series ",
      s$st,
      ": actual length = ",
      length(actual),
      ", SMYL length = ",
      length(smyl_fc),
      "."
    )
  }
  
  
  if (
    any(!is.finite(actual)) ||
    any(!is.finite(smyl_fc))
  ) {
    stop(
      "Non-finite actual or SMYL forecast for series ",
      s$st,
      "."
    )
  }
  
  
  # ---------------------------------------------------------------
  # Evaluate all ex-post scaling multipliers
  # ---------------------------------------------------------------
  
  oracle_errors <- vapply(
    oracle_grid,
    function(multiplier) {
      
      calc_smape_oracle(
        actual = actual,
        forecast = multiplier * smyl_fc
      )
    },
    numeric(1)
  )
  
  
  if (any(!is.finite(oracle_errors))) {
    stop(
      "Non-finite Oracle sMAPE for series ",
      s$st,
      "."
    )
  }
  
  
  # ---------------------------------------------------------------
  # Select minimum-sMAPE multiplier
  #
  # which.min() deterministically chooses the first minimum if an
  # exact tie occurs. We record whether a tie occurred for auditing.
  # ---------------------------------------------------------------
  
  best_idx <- which.min(oracle_errors)
  
  best_multiplier <- oracle_grid[best_idx]
  
  best_error <- oracle_errors[best_idx]
  
  
  n_best <- sum(
    abs(
      oracle_errors - best_error
    ) <= .Machine$double.eps^0.5
  )
  
  
  # ---------------------------------------------------------------
  # Store Oracle forecast and audit information
  # ---------------------------------------------------------------
  
  if (is.null(s$fct)) {
    s$fct <- list()
  }
  
  
  s$fct$smyl_oracle <-
    best_multiplier * smyl_fc
  
  
  s$smyl_oracle_multiplier <-
    best_multiplier
  
  s$smyl_oracle_smape <-
    best_error
  
  s$smyl_oracle_n_tied_minima <-
    n_best
  
  
  s
}


# ---------------------------------------------------------------------
# 4. Run all frequencies
# ---------------------------------------------------------------------

for (period_use in PERIODS_TO_RUN) {
  
  set_sensitivity_frequency(period_use)
  
  dataset <- read_sensitivity()
  
  
  cat(
    "[04a]",
    period_use,
    "Oracle series:",
    length(dataset),
    "\n"
  )
  
  
  # This operation is computationally light relative to Chronos.
  # Sequential execution is deliberately used for reproducibility
  # and simplicity.
  dataset <- lapply(
    dataset,
    add_smyl_oracle_one
  )
  
  
  # ---------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------
  
  n_with_oracle <- sum(
    vapply(
      dataset,
      function(s) {
        
        !is.null(s$fct$smyl_oracle) &&
          length(s$fct$smyl_oracle) ==
          as.integer(s$h)
        
      },
      logical(1)
    )
  )
  
  
  if (n_with_oracle != length(dataset)) {
    stop(
      "[04a] ",
      period_use,
      ": incomplete Oracle coverage: ",
      n_with_oracle,
      "/",
      length(dataset),
      "."
    )
  }
  
  
  oracle_multipliers <- vapply(
    dataset,
    function(s) {
      as.numeric(
        s$smyl_oracle_multiplier
      )
    },
    numeric(1)
  )
  
  
  if (
    any(
      oracle_multipliers < 0.5 |
      oracle_multipliers > 1.5
    )
  ) {
    stop(
      "[04a] ",
      period_use,
      ": Oracle multiplier outside [0.5, 1.5]."
    )
  }
  
  
  # ---------------------------------------------------------------
  # Save
  # ---------------------------------------------------------------
  
  write_sensitivity(dataset)
  
  
  cat(
    "[04a]",
    period_use,
    "SMYL Oracle added:",
    n_with_oracle,
    "/",
    length(dataset),
    "\n"
  )
  
  
  cat(
    "[04a]",
    period_use,
    "multiplier range:",
    sprintf("%.3f", min(oracle_multipliers)),
    "to",
    sprintf("%.3f", max(oracle_multipliers)),
    "\n"
  )
  
  
  cat(
    "[04a]",
    period_use,
    "saved:",
    sensitivity_rds,
    "\n"
  )
  
  
  rm(
    dataset,
    oracle_multipliers
  )
  
  gc()
}


cat(
  "\n[04a] SMYL Oracle completed successfully.\n"
)