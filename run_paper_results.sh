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

echo "[1/6] Running R pipeline..."
Rscript src/r/00_main_new.R

echo "[2/6] Running Python classifiers..."
conda run --no-capture-output \
  -n m4_fmts_classifiers \
  python -m src.python.run_tsc_experiment

echo "[3/6] Running Mantis..."
conda run --no-capture-output \
  -n m4_fmts_foundation \
  python -m src.python.run_mantis_experiment

echo "[4/6] Running Chronos-2..."
conda run --no-capture-output \
  -n m4_fmts_foundation \
  python -m src.python.run_tsc_chronos2

echo "[5/6] Running sensitivity analysis..."
Rscript src/r/sensitivity/99_sensitivity_run_all.R

echo "[6/6] Generating paper tables and figures..."
Rscript src/r/paper/run_all.R

echo "Paper-result pipeline completed successfully."