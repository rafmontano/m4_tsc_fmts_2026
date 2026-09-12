#!/usr/bin/env bash

# ==============================================================================
# run_paper_results_from_step3.sh
#
# Purpose:
#   Resume the pipeline from Step 3.
#
# Assumes:
#   Step 1 - R pipeline completed.
#   Step 2 - Python classifiers completed.
# ==============================================================================

set -euo pipefail

cd "$(dirname "$0")"

mkdir -p results

TIMING_FILE="results/runtime_from_step3_$(date -u +%Y%m%dT%H%M%SZ).tsv"
TOTAL_START=$SECONDS

printf "phase\tseconds\n" > "$TIMING_FILE"

run_phase() {
  local phase="$1"
  shift

  local phase_start=$SECONDS

  echo
  echo "============================================================"
  echo "$phase"
  echo "============================================================"

  "$@"

  local phase_seconds=$((SECONDS - phase_start))

  printf "%s\t%s\n" "$phase" "$phase_seconds" |
    tee -a "$TIMING_FILE"
}

# ------------------------------------------------------------------------------
# CUDA libraries for the foundation-model environment
# ------------------------------------------------------------------------------

FOUNDATION_SITE_PACKAGES="$(
  conda run -n m4_fmts_foundation \
    python -c 'import site; print(site.getsitepackages()[0])'
)"

FOUNDATION_NVIDIA_LIBS="$(
  find "$FOUNDATION_SITE_PACKAGES/nvidia" \
    -type d -name lib \
    -print 2>/dev/null |
    paste -sd:
)"

# ------------------------------------------------------------------------------
# Step 3: Mantis
# ------------------------------------------------------------------------------

run_phase "[3/6] Mantis" \
  env LD_LIBRARY_PATH="$FOUNDATION_NVIDIA_LIBS" \
  conda run --no-capture-output \
  -n m4_fmts_foundation \
  python -m src.python.run_mantis_experiment

# ------------------------------------------------------------------------------
# Step 4: Chronos-2
# ------------------------------------------------------------------------------

run_phase "[4/6] Chronos-2" \
  env LD_LIBRARY_PATH="$FOUNDATION_NVIDIA_LIBS" \
  conda run --no-capture-output \
  -n m4_fmts_foundation \
  python -m src.python.run_tsc_chronos2

# ------------------------------------------------------------------------------
# Step 5: Sensitivity analysis
# ------------------------------------------------------------------------------

run_phase "[5/6] Sensitivity analysis" \
  Rscript src/r/sensitivity/99_sensitivity_run_all.R

# ------------------------------------------------------------------------------
# Step 6: Paper tables and figures
# ------------------------------------------------------------------------------

run_phase "[6/6] Paper tables and figures" \
  Rscript src/r/paper/run_all.R

TOTAL_SECONDS=$((SECONDS - TOTAL_START))

printf "Total pipeline from Step 3\t%s\n" "$TOTAL_SECONDS" |
  tee -a "$TIMING_FILE"

echo
echo "Pipeline from Step 3 completed successfully."
echo "Runtime information saved to: $TIMING_FILE"