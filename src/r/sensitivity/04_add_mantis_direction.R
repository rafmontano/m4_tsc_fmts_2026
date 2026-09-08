# ==============================================================================
# 04_add_mantis_direction.R
#
# Purpose:
#   Add Mantis terminal-horizon directional predictions.
# Inputs:
#   Sensitivity RDS files and the classifier Python environment.
# Outputs:
#   Updated sensitivity RDS files containing Mantis directions.
# Run from:
#   Project root, directly or through 99_sensitivity_run_all.R.
# ==============================================================================

source("src/r/sensitivity/00_sensitivity_common.R")

mantis_csv <- "data/fct/mantis_da_horizon.csv"
WINDOW_MODE_USE <- "default"

mantis_all <- readr::read_csv(mantis_csv, show_col_types = FALSE)

add_mantis_direction_one <- function(s, mantis_df) {
  h_i <- as.integer(s$h)
  y_T <- as.numeric(tail(s$x, 1))

  m_i <- mantis_df |>
    dplyr::filter(
      st == as.character(s$st),
      horizon_id %in% seq_len(h_i)
    ) |>
    dplyr::arrange(horizon_id)

  if (nrow(m_i) != h_i) {
    message(
      "Incomplete MANTIS coverage for series ", s$st,
      ": expected ", h_i, " horizons, found ", nrow(m_i), "."
    )
  }

  if (any(!m_i$mantis_da %in% c(0L, 1L))) {
    message("Non-binary MANTIS prediction for series ", s$st, ".")
  }

  if (is.null(s$direction)) {
    s$direction <- list()
  }

  s$direction$mantis <- as.integer(m_i$mantis_da)
  s$direction$actual <- as.integer(as.numeric(s$xx) > y_T)
  s$direction$smyl <- as.integer(as.numeric(s$fct$smyl) > y_T)

  if (!is.null(s$fct$chronos)) {
    s$direction$chronos <- as.integer(as.numeric(s$fct$chronos) > y_T)
  }

  s
}

for (period_use in PERIODS_TO_RUN) {
  set_sensitivity_frequency(period_use)
  dataset <- read_sensitivity()

  mantis_df <- mantis_all |>
    dplyr::filter(
      freq_tag == FREQ_TAG,
      window_mode == WINDOW_MODE_USE
    )

  dataset <- lapply(
    dataset,
    add_mantis_direction_one,
    mantis_df = mantis_df
  )

  write_sensitivity(dataset)

  n_with_mantis <- sum(vapply(
    dataset,
    function(s) length(s$direction$mantis) == as.integer(s$h),
    logical(1)
  ))

  cat(
    "[04]", PERIOD_USE,
    "MANTIS direction added:",
    n_with_mantis, "/", length(dataset),
    "\n"
  )

  rm(dataset, mantis_df)
  gc()
}
