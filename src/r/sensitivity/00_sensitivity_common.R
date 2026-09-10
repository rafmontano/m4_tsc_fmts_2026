# ==============================================================================
# 00_sensitivity_common.R
#
# Purpose:
#   Define shared configuration, paths, and helpers for sensitivity analysis.
# Inputs:
#   Project configuration and M4 frequency metadata.
# Outputs:
#   Sensitivity globals and helper functions.
# Run from:
#   Project root; sourced by sensitivity scripts.
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(M4comp2018)

PERIODS_TO_RUN <- c(
  "Hourly",
  "Daily",
  "Weekly",
  "Monthly",
  "Quarterly",
  "Yearly"
)

# Test subset -----------------------------------------------------------------

USE_TEST_SUBSET <- FALSE
TEST_SERIES_PER_PERIOD <- 10L

# Official M4 frequency metadata.
# Used for validation, terminal-horizon DA, and the series-weighted
# "All" column in Tables 1 and 2.
M4_FREQUENCY_INFO <- tibble::tribble(
  ~period, ~freq_tag, ~n_series, ~horizon,
  "Hourly", "h", 414L, 48L,
  "Daily", "d", 4227L, 14L,
  "Weekly", "w", 359L, 13L,
  "Monthly", "m", 48000L, 18L,
  "Quarterly", "q", 24000L, 8L,
  "Yearly", "y", 23000L, 6L
)

# Parallel execution ---------------------------------------------------------

RUN_PARALLEL <- TRUE
MAX_WORKERS <- 8L

available_cores <- parallel::detectCores(logical = FALSE)

if (length(available_cores) != 1L || is.na(available_cores)) {
  available_cores <- 1L
}

# Each Chronos worker runs in a separate R/Python process and loads its
# own Chronos-2 model instance. Since all workers share one GPU, use a
# conservative cap rather than scaling workers with all CPU cores.

CHRONOS_WORKERS <- 1L

options(
  future.globals.maxSize = 4 * 1024^3
)

# Forecast configuration -----------------------------------------------------

SMYL_RANK <- 1L
FFORMA_RANK <- 2L

# Sensitivity grid -----------------------------------------------------------

lambda_up_values <- seq(
  1.00,
  1.12,
  by = 0.005
)

lambda_down_values <- seq(
  1.00,
  0.90,
  by = -0.005
)

lambda_surface <- tidyr::crossing(
  lambda_up = lambda_up_values,
  lambda_down = lambda_down_values
)

reference_lambda_up <- 1.05
reference_lambda_down <- 0.95

# Frequency helpers ----------------------------------------------------------

period_to_tag <- function(period) {
  dplyr::recode(
    as.character(period),
    "Yearly" = "y",
    "Quarterly" = "q",
    "Monthly" = "m",
    "Weekly" = "w",
    "Daily" = "d",
    "Hourly" = "h"
  )
}

set_sensitivity_frequency <- function(period_use) {
  PERIOD_USE <<- period_use
  FREQ_TAG <<- period_to_tag(period_use)

  data_dir <<- file.path(
    "data",
    "sensitivity",
    FREQ_TAG
  )

  results_dir <<- file.path(
    "results",
    "sensitivity",
    FREQ_TAG
  )

  dir.create(
    data_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  dir.create(
    results_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  sensitivity_rds <<- file.path(
    data_dir,
    paste0(
      "sensitivity_",
      FREQ_TAG,
      ".rds"
    )
  )

  sensitivity_summary_rds <<- file.path(
    results_dir,
    paste0(
      "sensitivity_summary_",
      FREQ_TAG,
      ".rds"
    )
  )
}

read_sensitivity <- function() {
  readRDS(
    sensitivity_rds
  )
}

write_sensitivity <- function(dataset) {
  saveRDS(
    dataset,
    sensitivity_rds
  )
}

direction_label <- function(value, origin) {
  as.integer(
    value > origin
  )
}

add_series_ids <- function(dataset, global_idx) {
  purrr::imap(
    dataset,
    function(s, i) {
      s$local_id <- i
      s$global_id <- global_idx[i]
      s$freq_tag <- period_to_tag(
        s$period
      )

      s
    }
  )
}

# Initialise first frequency -------------------------------------------------

set_sensitivity_frequency(
  PERIODS_TO_RUN[1]
)

cat(
  "[00] Sensitivity setup loaded\n"
)
