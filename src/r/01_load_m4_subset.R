# ==============================================================================
# 01_load_m4_subset.R
#
# Purpose: Load the M4 dataset and save the local working copy.
# Inputs:  Global periods, USE_TEST_SUBSET, TEST_SERIES_PER_PERIOD, and
#          subset_file values; M4comp2018::M4.
# Outputs: The RDS file specified by subset_file.
# Run from: Repository root; sourced by src/r/00_main_new.R.
# ==============================================================================

# Load dataset ----------------------------------------------------------------

M4 <- M4comp2018::M4
cat("M4 dataset loaded.\n")

if (USE_TEST_SUBSET) {
  required_series <- c(
    "D2715",
    "D1602",
    "W39",
    "W82"
  )

  subset_indices <- unlist(
    lapply(periods, function(period_i) {
      period_indices <- which(vapply(
        M4,
        function(series) as.character(series$period) == period_i,
        logical(1)
      ))

      utils::head(
        period_indices,
        TEST_SERIES_PER_PERIOD
      )
    }),
    use.names = FALSE
  )

  series_names <- vapply(
    M4,
    function(series) as.character(series$st),
    character(1)
  )

  missing_required_series <- setdiff(
    required_series,
    series_names
  )

  if (length(missing_required_series) > 0L) {
    message(
      "Required test series not found: ",
      paste(missing_required_series, collapse = ", ")
    )
  }

  required_indices <- which(
    series_names %in% required_series
  )

  subset_indices <- sort(unique(c(
    subset_indices,
    required_indices
  )))

  M4 <- M4[subset_indices]

  cat(
    "Test subset enabled:",
    TEST_SERIES_PER_PERIOD,
    "series per configured period plus",
    length(required_indices),
    "required paper case-study series.\n"
  )
}

cat("Total series loaded:", length(M4), "\n")

# Save dataset ----------------------------------------------------------------

saveRDS(M4, file = subset_file)
cat("Saved dataset into:", subset_file, "\n")
