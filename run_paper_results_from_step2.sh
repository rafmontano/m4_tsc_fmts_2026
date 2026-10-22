#!/usr/bin/env bash

# ==============================================================================
# run_paper_results_from_step2.sh
#
# Purpose:
#   Resume the pipeline from Step 2.
#
# Assumes:
#   Step 1 - R pipeline completed successfully.
#
# Runs:
#   Step 2 - Python classifiers
#   Step 3 - Mantis
#   Step 4 - Chronos-2
#   Step 5 - Sensitivity analysis
#   Step 6 - Paper tables and figures
# ==============================================================================

set -euo pipefail

cd "$(dirname "$0")"

mkdir -p results

TIMING_FILE="results/runtime_from_step2_$(date -u +%Y%m%dT%H%M%SZ).tsv"
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
# Conda / CUDA helper
# ------------------------------------------------------------------------------

get_conda_nvidia_libs() {
  local environment="$1"

  conda run \
    -n "$environment" \
    python -c '
import glob
import site

site_packages = site.getsitepackages()[0]
paths = sorted(glob.glob(site_packages + "/nvidia/*/lib"))

print(":".join(paths))
'
}

# ------------------------------------------------------------------------------
# Step 2: Python classifiers
# ------------------------------------------------------------------------------

CLASSIFIER_NVIDIA_LIBS="$(
  get_conda_nvidia_libs "m4_fmts_classifiers"
)"

run_phase "[2/6] Python classifiers" \
  env LD_LIBRARY_PATH="$CLASSIFIER_NVIDIA_LIBS" \
  conda run --no-capture-output \
  -n m4_fmts_classifiers \
  python -m src.python.run_tsc_experiment

# ------------------------------------------------------------------------------
# Steps 3 and 4: Foundation models
# ------------------------------------------------------------------------------

FOUNDATION_NVIDIA_LIBS="$(
  get_conda_nvidia_libs "m4_fmts_foundation"
)"

run_phase "[3/6] Mantis" \
  env LD_LIBRARY_PATH="$FOUNDATION_NVIDIA_LIBS" \
  conda run --no-capture-output \
  -n m4_fmts_foundation \
  python -m src.python.run_mantis_experiment

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

# ------------------------------------------------------------------------------
# Completion
# ------------------------------------------------------------------------------

TOTAL_SECONDS=$((SECONDS - TOTAL_START))

printf "Total pipeline from Step 2\t%s\n" "$TOTAL_SECONDS" |
  tee -a "$TIMING_FILE"

echo
echo "============================================================"
echo "Pipeline from Step 2 completed successfully."
echo "Runtime information saved to: $TIMING_FILE"
echo "============================================================"