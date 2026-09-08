# ==============================================================================
# 05a_build_tables1_2_from_05.R
#
# Purpose:
#   Build the paper's primary result tables from sensitivity outputs.
# Inputs:
#   Lambda-sensitivity results and standalone Mantis directional accuracy.
# Outputs:
#   Paper-ready CSV, RDS, and LaTeX tables.
# Run from:
#   Project root, directly or through 99_sensitivity_run_all.R.
# ==============================================================================

source("src/r/sensitivity/00_sensitivity_common.R")

library(readr)

# Configuration --------------------------------------------------------------

frequency_info <- M4_FREQUENCY_INFO

period_order <- c(
  "Hourly",
  "Daily",
  "Weekly",
  "Monthly",
  "Quarterly",
  "Yearly",
  "All"
)

method_labels <- c(
  "naive2"         = "Naive2",
  "chronos"        = "Chronos-2",
  "fforma"         = "FFORMA",
  "smyl"           = "SMYL",
  "smyl_mantis"    = "SMYL-Mantis",
  "chronos_mantis" = "Chronos-2-Mantis",
  "mantis"         = "Mantis",
  "smyl_oracle"    = "SMYL-Oracle"
)

# Table 1 contains point forecasts only.
table1_order <- c(
  "naive2",
  "chronos",
  "fforma",
  "smyl",
  "smyl_mantis",
  "chronos_mantis",
  "smyl_oracle"
)

# Table 2 Panel A additionally contains standalone Mantis.
panel_a_order <- c(
  "naive2",
  "chronos",
  "fforma",
  "smyl",
  "smyl_mantis",
  "chronos_mantis",
  "mantis",
  "smyl_oracle"
)

# Panels B and C contain point forecasts only.
point_method_order <- table1_order

table_dir <- file.path(
  "results",
  "paper",
  "tables"
)

dir.create(
  table_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Read Script-05 results -----------------------------------------------------

read_frequency_results <- function(
  period,
  freq_tag,
  n_series,
  horizon
) {
  input_file <- file.path(
    "results",
    "sensitivity",
    freq_tag,
    paste0(
      "lambda_sensitivity_",
      freq_tag,
      ".rds"
    )
  )

  x <- readRDS(input_file)

  x |>
    dplyr::mutate(
      period = period,
      freq_tag = freq_tag,
      n_series = n_series,
      horizon = horizon,
      source_file = input_file
    )
}

results_05 <- purrr::pmap_dfr(
  frequency_info,
  read_frequency_results
)

# Select best lambda pair for each adjusted method ---------------------------
#
# Minimum full-horizon OWA is the selection criterion.
#
# In the unlikely event of an exact tie, prefer the parameter pair
# closest to no adjustment (1,1).

select_best_owa <- function(df) {
  df |>
    dplyr::arrange(
      owa_vs_naive2,
      abs(lambda_up - 1) +
        abs(lambda_down - 1),
      abs(lambda_up - 1),
      abs(lambda_down - 1)
    ) |>
    dplyr::slice(1)
}

adjusted_selected <- results_05 |>
  dplyr::filter(
    model_id %in% c(
      "smyl_mantis",
      "chronos_mantis"
    )
  ) |>
  dplyr::group_split(
    period,
    model_id
  ) |>
  purrr::map_dfr(
    select_best_owa
  )

# Select point-forecast baseline rows ----------------------------------------

baseline_selected <- results_05 |>
  dplyr::filter(
    model_id %in% c(
      "naive2",
      "chronos",
      "fforma",
      "smyl",
      "smyl_oracle"
    )
  )

# Standalone Mantis terminal-horizon DA --------------------------------------
#
# Mantis has directional predictions but no point forecast,
# therefore sMAPE, MASE and OWA are not applicable.

read_mantis_result <- function(
  period,
  freq_tag,
  n_series,
  horizon
) {
  input_file <- file.path(
    "data",
    "sensitivity",
    freq_tag,
    paste0(
      "sensitivity_",
      freq_tag,
      ".rds"
    )
  )

  dataset <- readRDS(input_file)

  mantis_da <- mean(
    vapply(
      dataset,
      function(s) {
        last_x <- tail(
          as.numeric(s$x),
          1
        )

        actual_final <- as.integer(
          tail(
            as.numeric(s$xx),
            1
          ) > last_x
        )

        mantis_final <- tail(
          as.integer(
            s$direction$mantis
          ),
          1
        )

        as.numeric(
          actual_final ==
            mantis_final
        )
      },
      numeric(1)
    ),
    na.rm = TRUE
  )

  tibble::tibble(
    model_id = "mantis",
    lambda_up = NA_real_,
    lambda_down = NA_real_,
    smape = NA_real_,
    mase = NA_real_,
    da = mantis_da,
    relative_smape_vs_naive2 = NA_real_,
    relative_mase_vs_naive2 = NA_real_,
    owa_vs_naive2 = NA_real_,
    period = period,
    freq_tag = freq_tag,
    n_series = n_series,
    horizon = horizon,
    source_file = input_file
  )
}

mantis_results <- purrr::pmap_dfr(
  frequency_info,
  read_mantis_result
)

# Selected results for the six M4 frequencies --------------------------------

selected_frequency_results <- dplyr::bind_rows(
  baseline_selected,
  adjusted_selected,
  mantis_results
) |>
  dplyr::arrange(
    factor(
      period,
      levels = period_order
    ),
    factor(
      model_id,
      levels = panel_a_order
    )
  )

# Calculate the "All" column -------------------------------------------------
#
# sMAPE, MASE and DA are weighted by number of series.
#
# OWA is then recalculated from the aggregated sMAPE and MASE
# relative to aggregated Naive2.

all_point_components <- selected_frequency_results |>
  dplyr::filter(
    model_id != "mantis"
  ) |>
  dplyr::group_by(
    model_id
  ) |>
  dplyr::summarise(
    smape = weighted.mean(
      smape,
      w = n_series
    ),
    mase = weighted.mean(
      mase,
      w = n_series
    ),
    da = weighted.mean(
      da,
      w = n_series
    ),
    .groups = "drop"
  )

all_naive2 <- all_point_components |>
  dplyr::filter(
    model_id == "naive2"
  )

all_point_results <- all_point_components |>
  dplyr::mutate(
    lambda_up = NA_real_,
    lambda_down = NA_real_,
    relative_smape_vs_naive2 =
      smape /
        all_naive2$smape,
    relative_mase_vs_naive2 =
      mase /
        all_naive2$mase,
    owa_vs_naive2 =
      0.5 * (
        relative_smape_vs_naive2 +
          relative_mase_vs_naive2
      ),
    period = "All",
    freq_tag = "all",
    n_series = sum(
      frequency_info$n_series
    ),
    horizon = NA_integer_,
    source_file =
      "Aggregated from Script-05 frequency results"
  ) |>
  dplyr::select(
    names(
      selected_frequency_results
    )
  )

all_mantis_result <- selected_frequency_results |>
  dplyr::filter(
    model_id == "mantis"
  ) |>
  dplyr::summarise(
    model_id = "mantis",
    lambda_up = NA_real_,
    lambda_down = NA_real_,
    smape = NA_real_,
    mase = NA_real_,
    da = weighted.mean(
      da,
      w = n_series
    ),
    relative_smape_vs_naive2 =
      NA_real_,
    relative_mase_vs_naive2 =
      NA_real_,
    owa_vs_naive2 =
      NA_real_,
    period = "All",
    freq_tag = "all",
    n_series = sum(
      frequency_info$n_series
    ),
    horizon = NA_integer_,
    source_file =
      "Aggregated from Script-04 Mantis directions"
  ) |>
  dplyr::select(
    names(
      selected_frequency_results
    )
  )

all_results <- dplyr::bind_rows(
  all_point_results,
  all_mantis_result
)

# Complete selected-results dataset ------------------------------------------

selected_results <- dplyr::bind_rows(
  selected_frequency_results,
  all_results
)

# TABLE 2 - PANEL A ----------------------------------------------------------
#    Terminal-horizon directional accuracy

panel_a <- selected_results |>
  dplyr::filter(
    model_id %in%
      panel_a_order
  ) |>
  dplyr::mutate(
    model_id = factor(
      model_id,
      levels = panel_a_order
    ),
    period = factor(
      period,
      levels = period_order
    ),
    Method =
      unname(
        method_labels[
          as.character(model_id)
        ]
      )
  ) |>
  dplyr::arrange(
    model_id,
    period
  ) |>
  dplyr::select(
    model_id,
    Method,
    period,
    DA = da
  ) |>
  tidyr::pivot_wider(
    names_from = period,
    values_from = DA
  ) |>
  dplyr::arrange(
    model_id
  ) |>
  dplyr::select(
    -model_id,
    dplyr::all_of(
      c(
        "Method",
        period_order
      )
    )
  )

# TABLE 2 - PANEL B ----------------------------------------------------------
#     Full-horizon OWA

panel_b <- selected_results |>
  dplyr::filter(
    model_id %in%
      point_method_order
  ) |>
  dplyr::mutate(
    model_id = factor(
      model_id,
      levels = point_method_order
    ),
    period = factor(
      period,
      levels = period_order
    ),
    Method =
      unname(
        method_labels[
          as.character(model_id)
        ]
      )
  ) |>
  dplyr::arrange(
    model_id,
    period
  ) |>
  dplyr::select(
    model_id,
    Method,
    period,
    OWA = owa_vs_naive2
  ) |>
  tidyr::pivot_wider(
    names_from = period,
    values_from = OWA
  ) |>
  dplyr::arrange(
    model_id
  ) |>
  dplyr::select(
    -model_id,
    dplyr::all_of(
      c(
        "Method",
        period_order
      )
    )
  )

# TABLE 2 - PANEL C ----------------------------------------------------------
#     OWA improvement relative to SMYL

smyl_reference <- selected_results |>
  dplyr::filter(
    model_id == "smyl"
  ) |>
  dplyr::select(
    period,
    smyl_owa = owa_vs_naive2
  )

panel_c <- selected_results |>
  dplyr::filter(
    model_id %in%
      point_method_order
  ) |>
  dplyr::left_join(
    smyl_reference,
    by = "period"
  ) |>
  dplyr::mutate(
    Improvement =
      100 * (
        1 -
          owa_vs_naive2 /
            smyl_owa
      ),
    model_id = factor(
      model_id,
      levels = point_method_order
    ),
    period = factor(
      period,
      levels = period_order
    ),
    Method =
      unname(
        method_labels[
          as.character(model_id)
        ]
      )
  ) |>
  dplyr::arrange(
    model_id,
    period
  ) |>
  dplyr::select(
    model_id,
    Method,
    period,
    Improvement
  ) |>
  tidyr::pivot_wider(
    names_from = period,
    values_from = Improvement
  ) |>
  dplyr::arrange(
    model_id
  ) |>
  dplyr::select(
    -model_id,
    dplyr::all_of(
      c(
        "Method",
        period_order
      )
    )
  )

# TABLE 1 - Daily results ----------------------------------------------------

daily_selected <- selected_results |>
  dplyr::filter(
    period == "Daily",
    model_id %in%
      table1_order
  )

daily_smyl <- daily_selected |>
  dplyr::filter(
    model_id == "smyl"
  )

table1_daily_full <- daily_selected |>
  dplyr::mutate(
    Method =
      unname(
        method_labels[
          model_id
        ]
      ),
    improvement_smape_vs_smyl_pct =
      100 * (
        1 -
          smape /
            daily_smyl$smape
      ),
    improvement_mase_vs_smyl_pct =
      100 * (
        1 -
          mase /
            daily_smyl$mase
      ),
    improvement_owa_vs_smyl_pct =
      100 * (
        1 -
          owa_vs_naive2 /
            daily_smyl$owa_vs_naive2
      ),
    model_order = factor(
      model_id,
      levels = table1_order
    )
  ) |>
  dplyr::arrange(
    model_order
  ) |>
  dplyr::select(
    model_id,
    Method,
    lambda_up,
    lambda_down,
    smape,
    mase,
    da,
    owa_vs_naive2,
    improvement_smape_vs_smyl_pct,
    improvement_mase_vs_smyl_pct,
    improvement_owa_vs_smyl_pct,
    source_file
  )

# Audit table ----------------------------------------------------------------

audit_table <- selected_results |>
  dplyr::mutate(
    Method =
      unname(
        method_labels[
          model_id
        ]
      ),
    model_order = factor(
      model_id,
      levels = panel_a_order
    ),
    period_order_internal = factor(
      period,
      levels = period_order
    )
  ) |>
  dplyr::arrange(
    period_order_internal,
    model_order
  ) |>
  dplyr::select(
    period,
    freq_tag,
    n_series,
    horizon,
    model_id,
    Method,
    lambda_up,
    lambda_down,
    smape,
    mase,
    da,
    relative_smape_vs_naive2,
    relative_mase_vs_naive2,
    owa_vs_naive2,
    source_file
  )

# Publication formatting -----------------------------------------------------

panel_a_print <- panel_a |>
  dplyr::mutate(
    dplyr::across(
      -Method,
      ~ round(.x, 3)
    )
  )

panel_b_print <- panel_b |>
  dplyr::mutate(
    dplyr::across(
      -Method,
      ~ round(.x, 3)
    )
  )

panel_c_print <- panel_c |>
  dplyr::mutate(
    dplyr::across(
      -Method,
      ~ round(.x, 2)
    )
  )

table1_daily_print <- table1_daily_full |>
  dplyr::transmute(
    Method,
    sMAPE =
      round(
        smape,
        2
      ),
    MASE =
      round(
        mase,
        2
      ),
    DA =
      round(
        da,
        3
      ),
    OWA =
      round(
        owa_vs_naive2,
        3
      ),
    `sMAPE improvement vs SMYL (%)` =
      round(
        improvement_smape_vs_smyl_pct,
        2
      ),
    `MASE improvement vs SMYL (%)` =
      round(
        improvement_mase_vs_smyl_pct,
        2
      ),
    `OWA improvement vs SMYL (%)` =
      round(
        improvement_owa_vs_smyl_pct,
        2
      )
  )

# Save Table 1 ---------------------------------------------------------------

readr::write_csv(
  table1_daily_full,
  file.path(
    table_dir,
    "table1_daily_full_precision.csv"
  )
)

readr::write_csv(
  table1_daily_print,
  file.path(
    table_dir,
    "table1_daily.csv"
  )
)

# Save Table 2 ---------------------------------------------------------------

readr::write_csv(
  panel_a,
  file.path(
    table_dir,
    "table2_panel_a_full_precision.csv"
  )
)

readr::write_csv(
  panel_b,
  file.path(
    table_dir,
    "table2_panel_b_full_precision.csv"
  )
)

readr::write_csv(
  panel_c,
  file.path(
    table_dir,
    "table2_panel_c_full_precision.csv"
  )
)

readr::write_csv(
  panel_a_print,
  file.path(
    table_dir,
    "table2_panel_a.csv"
  )
)

readr::write_csv(
  panel_b_print,
  file.path(
    table_dir,
    "table2_panel_b.csv"
  )
)

readr::write_csv(
  panel_c_print,
  file.path(
    table_dir,
    "table2_panel_c.csv"
  )
)

# Save full audit source -----------------------------------------------------

readr::write_csv(
  audit_table,
  file.path(
    table_dir,
    "table1_table2_audit.csv"
  )
)

# Console output -------------------------------------------------------------

cat(
  "\n",
  "============================================================\n",
  "TABLE 1 - DAILY RESULTS\n",
  "============================================================\n\n",
  sep = ""
)

print(
  table1_daily_print,
  width = Inf
)

cat(
  "\n",
  "============================================================\n",
  "TABLE 2 - PANEL A: TERMINAL-HORIZON DA\n",
  "============================================================\n\n",
  sep = ""
)

print(
  panel_a_print,
  width = Inf
)

cat(
  "\n",
  "============================================================\n",
  "TABLE 2 - PANEL B: OWA\n",
  "============================================================\n\n",
  sep = ""
)

print(
  panel_b_print,
  width = Inf
)

cat(
  "\n",
  "============================================================\n",
  "TABLE 2 - PANEL C: OWA IMPROVEMENT RELATIVE TO SMYL (%)\n",
  "============================================================\n\n",
  sep = ""
)

print(
  panel_c_print,
  width = Inf
)

cat(
  "\nFiles written to: ",
  table_dir,
  "\n",
  sep = ""
)

cat(
  "\n[05a] Tables 1 and 2 completed successfully.\n"
)
