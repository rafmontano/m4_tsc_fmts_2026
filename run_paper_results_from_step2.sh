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

exec bash "$(dirname "$0")/run_paper_results.sh" --from-step2 "$@"
