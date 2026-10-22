#!/usr/bin/env bash

# ==============================================================================
# run_paper_results.sh
#
# Purpose:
#   Run the complete experimental and paper-output pipeline.
#
# Inputs:
#   Installed R and Python environments plus the M4 dataset.
#
# Outputs:
#   Trained models, evaluation results, sensitivity results, tables, and figures.
#
# Run from:
#   Any location; the script changes to the project root automatically.
#
# Python environments:
#   m4_fmts_classifiers  - conventional classifiers / TensorFlow / InceptionTime
#   m4_fmts_foundation   - MANTIS / Chronos-2 / PyTorch
#
# CUDA handling:
#   NVIDIA libraries installed inside each Conda environment are exposed only
#   to processes launched from that environment. This avoids mixing CUDA
#   libraries between TensorFlow and PyTorch environments.
# ==============================================================================

set -euo pipefail

# ------------------------------------------------------------------------------
# Project root
# ------------------------------------------------------------------------------

cd "$(dirname "$0")"

mkdir -p results

# ------------------------------------------------------------------------------
# Runtime logging
# ------------------------------------------------------------------------------

TIMING_FILE="results/runtime_$(date -u +%Y%m%dT%H%M%SZ).tsv"
TOTAL_START=$SECONDS

printf "phase\tseconds\n" > "$TIMING_FILE"

# ------------------------------------------------------------------------------
# Generic phase runner
# ------------------------------------------------------------------------------

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
# Conda / CUDA helpers
# ------------------------------------------------------------------------------

get_conda_nvidia_libs() {
  local environment="$1"

  conda run \
    --no-capture-output \
    -n "$environment" \
    python -c '
import glob
import site

site_packages = site.getsitepackages()[0]

paths = sorted(
    glob.glob(site_packages + "/nvidia/*/lib")
)

print(":".join(paths))
'
}

get_conda_nvidia_bins() {
  local environment="$1"

  conda run \
    --no-capture-output \
    -n "$environment" \
    python -c '
import glob
import site

site_packages = site.getsitepackages()[0]

paths = sorted(
    glob.glob(site_packages + "/nvidia/*/bin")
)

print(":".join(paths))
'
}

run_conda_python_module() {
  local environment="$1"
  local module="$2"

  local nvidia_libs
  local nvidia_bins
  local runtime_path

  nvidia_libs="$(get_conda_nvidia_libs "$environment")"
  nvidia_bins="$(get_conda_nvidia_bins "$environment")"

  echo "[Python] Environment: $environment"
  echo "[Python] Module     : $module"

  if [[ -n "$nvidia_libs" ]]; then
    echo "[CUDA] NVIDIA libraries found inside Conda environment."
  else
    echo "[CUDA] No pip-installed NVIDIA library directories found."
  fi

  if [[ -n "$nvidia_bins" ]]; then
    echo "[CUDA] NVIDIA executable directories found inside Conda environment."
  fi

  runtime_path="$PATH"

  if [[ -n "$nvidia_bins" ]]; then
    runtime_path="$nvidia_bins:$runtime_path"
  fi

  if [[ -n "$nvidia_libs" ]]; then
    env \
      LD_LIBRARY_PATH="$nvidia_libs" \
      PATH="$runtime_path" \
      conda run \
        --no-capture-output \
        -n "$environment" \
        python -m "$module"
  else
    env \
      PATH="$runtime_path" \
      conda run \
        --no-capture-output \
        -n "$environment" \
        python -m "$module"
  fi
}

# ------------------------------------------------------------------------------
# CUDA pre-flight checks
# ------------------------------------------------------------------------------

check_tensorflow_gpu() {
  local environment="m4_fmts_classifiers"

  local nvidia_libs
  local nvidia_bins
  local runtime_path

  nvidia_libs="$(get_conda_nvidia_libs "$environment")"
  nvidia_bins="$(get_conda_nvidia_bins "$environment")"

  runtime_path="$PATH"

  if [[ -n "$nvidia_bins" ]]; then
    runtime_path="$nvidia_bins:$runtime_path"
  fi

  echo "[Check] TensorFlow GPU access..."

  env \
    LD_LIBRARY_PATH="$nvidia_libs" \
    PATH="$runtime_path" \
    conda run \
      --no-capture-output \
      -n "$environment" \
      python -c '
import tensorflow as tf

print("TensorFlow:", tf.__version__)
print("Built with CUDA:", tf.test.is_built_with_cuda())

gpus = tf.config.list_physical_devices("GPU")
print("GPUs:", gpus)

if not gpus:
    raise RuntimeError(
        "TensorFlow cannot access a GPU. "
        "Aborting before classifier experiments."
    )
'

  echo "[Check] TensorFlow GPU access OK."
}

check_pytorch_gpu() {
  local environment="m4_fmts_foundation"

  local nvidia_libs
  local nvidia_bins
  local runtime_path

  nvidia_libs="$(get_conda_nvidia_libs "$environment")"
  nvidia_bins="$(get_conda_nvidia_bins "$environment")"

  runtime_path="$PATH"

  if [[ -n "$nvidia_bins" ]]; then
    runtime_path="$nvidia_bins:$runtime_path"
  fi

  echo "[Check] PyTorch GPU access..."

  if [[ -n "$nvidia_libs" ]]; then
    env \
      LD_LIBRARY_PATH="$nvidia_libs" \
      PATH="$runtime_path" \
      conda run \
        --no-capture-output \
        -n "$environment" \
        python -c '
import torch

print("PyTorch:", torch.__version__)
print("CUDA available:", torch.cuda.is_available())
print("PyTorch CUDA:", torch.version.cuda)

if not torch.cuda.is_available():
    raise RuntimeError(
        "PyTorch cannot access a GPU. "
        "Aborting before foundation-model experiments."
    )

print("GPU:", torch.cuda.get_device_name(0))
'
  else
    env \
      PATH="$runtime_path" \
      conda run \
        --no-capture-output \
        -n "$environment" \
        python -c '
import torch

print("PyTorch:", torch.__version__)
print("CUDA available:", torch.cuda.is_available())
print("PyTorch CUDA:", torch.version.cuda)

if not torch.cuda.is_available():
    raise RuntimeError(
        "PyTorch cannot access a GPU. "
        "Aborting before foundation-model experiments."
    )

print("GPU:", torch.cuda.get_device_name(0))
'
  fi

  echo "[Check] PyTorch GPU access OK."
}

# ==============================================================================
# Pipeline
# ==============================================================================

# ------------------------------------------------------------------------------
# Step 1: R pipeline
# ------------------------------------------------------------------------------

run_phase "[1/6] R pipeline" \
  Rscript src/r/00_main_new.R

# ------------------------------------------------------------------------------
# Step 2: Python classifiers
#
# TensorFlow requires access to the CUDA libraries installed by
# tensorflow[and-cuda] inside m4_fmts_classifiers.
# ------------------------------------------------------------------------------

check_tensorflow_gpu

run_phase "[2/6] Python classifiers" \
  run_conda_python_module \
    "m4_fmts_classifiers" \
    "src.python.run_tsc_experiment"

# ------------------------------------------------------------------------------
# Steps 3-4: Foundation models
#
# Use the CUDA libraries belonging to m4_fmts_foundation rather than those
# belonging to the TensorFlow classifier environment.
# ------------------------------------------------------------------------------

check_pytorch_gpu

run_phase "[3/6] Mantis" \
  run_conda_python_module \
    "m4_fmts_foundation" \
    "src.python.run_mantis_experiment"

run_phase "[4/6] Chronos-2" \
  run_conda_python_module \
    "m4_fmts_foundation" \
    "src.python.run_tsc_chronos2"

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

printf "Total pipeline\t%s\n" "$TOTAL_SECONDS" |
  tee -a "$TIMING_FILE"

echo
echo "============================================================"
echo "Paper-result pipeline completed successfully."
echo "Runtime information saved to: $TIMING_FILE"
echo "============================================================"