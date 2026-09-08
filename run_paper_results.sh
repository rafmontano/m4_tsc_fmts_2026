#!/usr/bin/env bash

# ==============================================================================
# run_paper_results.sh
#
# Purpose:
#   Run the complete experimental and paper-output pipeline.
# Inputs:
#   Installed R and Python environments plus the M4 dataset.
# Outputs:
#   Trained models, evaluation results, sensitivity results, tables, and figures.
# Run from:
#   Any location; the script changes to the project root automatically.
# ==============================================================================

set -e

cd "$(dirname "$0")"

mkdir -p results

TIMING_FILE="results/runtime_$(date -u +%Y%m%dT%H%M%SZ).tsv"
TOTAL_START=$SECONDS

printf "phase\tseconds\n" > "$TIMING_FILE"

run_phase() {
  local phase="$1"
  shift

  local phase_start=$SECONDS

  echo "$phase"
  "$@"

  local phase_seconds=$((SECONDS - phase_start))

  printf "%s\t%s\n" "$phase" "$phase_seconds" |
    tee -a "$TIMING_FILE"
}

run_phase "[1/6] R pipeline" \
  Rscript src/r/00_main_new.R

run_phase "[2/6] Python classifiers" \
  conda run --no-capture-output \
  -n m4_fmts_classifiers \
  python -m src.python.run_tsc_experiment

run_phase "[3/6] Mantis" \
  conda run --no-capture-output \
  -n m4_fmts_foundation \
  python -m src.python.run_mantis_experiment

run_phase "[4/6] Chronos-2" \
  conda run --no-capture-output \
  -n m4_fmts_foundation \
  python -m src.python.run_tsc_chronos2

run_phase "[5/6] Sensitivity analysis" \
  Rscript src/r/sensitivity/99_sensitivity_run_all.R

run_phase "[6/6] Paper tables and figures" \
  Rscript src/r/paper/run_all.R

TOTAL_SECONDS=$((SECONDS - TOTAL_START))

printf "Total pipeline\t%s\n" "$TOTAL_SECONDS" |
  tee -a "$TIMING_FILE"

echo "Paper-result pipeline completed successfully."
echo "Runtime information saved to: $TIMING_FILE"
