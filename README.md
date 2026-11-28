# A Step in the Right Direction

## Bridging Forecasting and Classification for Temporal Foundation Models

Research code accompanying the FMTS \@ NeurIPS 2026 submission **A Step in the Right Direction: Bridging Forecasting and Classification for Temporal Foundation Models**.

## Authors

Rafael Montano, Pablo Montero-Manso, and Andrey Vasnev.

The University of Sydney Business School.

![Directional Prediction Framework](docs/assets/directional-prediction-framework.png)

## Overview

Point forecasts estimate future values, but many operational and financial decisions depend on whether a quantity increases or does not increase. Directional accuracy captures this decision-relevant property, yet it is commonly treated as a by-product of point forecasting rather than as a prediction task in its own right.

This project introduces a **Directional Prediction Framework** that reformulates forecasting datasets as binary time-series classification tasks. Directional accuracy provides a common evaluation criterion for comparing point forecasting methods with time-series classifiers, including foundation-model-based approaches.

## Framework

At each forecast origin and horizon, the framework:

1.  Obtains a classifier-predicted directional label.
2.  Derives a directional label implied by the point forecast.
3.  Selects a multiplicative adjustment when the two labels disagree.
4.  Applies the selected adjustment to the point forecast.

An increase is the positive class; a zero or negative change is the non-increase class. Both the observed and predicted changes are measured relative to the forecast origin.

The adjustment is a post hoc exploratory proof of concept. It is not an optimal adjustment rule or an estimate of held-out performance.

## Experimental study

The study evaluates the framework on the M4 dataset, which contains 100,000 time series across six sampling frequencies and multiple forecast horizons.

The comparison covers:

- Point forecasting methods: SMYL, FFORMA, and Chronos-2.
- Time-series classifiers: 1-NN DTW, 1-NN Euclidean distance, Rotation Forest, XGBoost, ROCKET, InceptionTime, and Mantis.
- Evaluation across forecast horizons using directional accuracy.
- Exploratory directional adjustment evaluated with standard point-forecast error measures.

## Main findings

- Directional accuracy does not necessarily follow the same horizon-dependent pattern as point-forecast accuracy.
- On the M4 Daily subset, Mantis achieves higher directional accuracy at some longer forecast horizons.
- In the exploratory Daily analysis, Mantis-based directional adjustment reduces the OWA of SMYL and Chronos-2.
- The observed gains vary across sampling frequencies and forecasting settings.

The evidence is descriptive, conditional on the reported experimental design, and limited to the M4 dataset and evaluated methods.

### Directional accuracy across horizons

Figure 2 highlights the paper's central empirical observation on the M4 Daily subset. Mantis separates from the point-forecast-implied and conventional classification baselines at several longer horizons, while the accompanying critical-difference diagram summarises descriptive average ranks across the Daily forecast horizon.

<p align="center">

<img src="docs/assets/figure-2a-directional-accuracy-daily.png" alt="Directional accuracy by forecast horizon on the M4 Daily subset" width="55%"/> <img src="docs/assets/figure-2b-daily-average-ranks.png" alt="Descriptive average ranks across M4 Daily forecast horizons" width="42%"/>

</p>

<p align="center">

<strong>Figure 2.</strong> Directional prediction results for the M4 Daily subset. <strong>Left:</strong> directional accuracy by forecast horizon. <strong>Right:</strong> descriptive average ranks across Daily forecast horizons.

</p>

### Directional adjustment in practice

Figure 3 illustrates how a learned directional signal complements an existing point forecast. When the classifier and point forecast imply different directions, the framework applies the corresponding upward or downward adjustment. The examples communicate the mechanism; they do not establish a general performance guarantee.

![Examples of upward and downward directional adjustment](docs/assets/figure-3-directional-adjustment-examples.png)

<p align="center">

<strong>Figure 3.</strong> Examples of directional adjustment. Mantis adjusts the SMYL forecast upward for M4 Daily series D1602 (left) and the Chronos-2 forecast downward for M4 Weekly series W39 (right).

</p>

## Installation

The project requires R, Conda, Git, and an internet connection for the initial package and model downloads. The published lockfile records R 4.6.2 and includes a package requiring R 4.5 or later. Run all commands from a terminal.

The reference configuration is Ubuntu 24.04 with an AMD CPU (16 cores / 32 threads), 128 GB RAM, and an NVIDIA RTX 5090 with CUDA. The second target is Apple-silicon macOS with 128 GB unified memory. On macOS, PyTorch uses MPS and TensorFlow uses the tensorflow-metal plugin for Metal acceleration.

Before running setup, install the native build prerequisites:

- Ubuntu: install `libuv1-dev` and `libx11-dev` for the R package dependencies.
- macOS: provide a Fortran compiler compatible with the installed R build and ensure that OpenSSL is discoverable by `pkg-config`. If necessary, set `PKG_CONFIG_PATH` to include the OpenSSL installation's `lib/pkgconfig` directory.

Clone the repository and run the single setup script:

``` bash
git clone https://github.com/rafmontano/m4_tsc_fmts_2026.git m4_tsc_fmts_2026
cd m4_tsc_fmts_2026
Rscript --vanilla setup.R
```

The setup script creates the local `data/`, `models/`, and `results/` directories, prepares the isolated R library, installs the required R and Python packages, creates the Conda environments, and checks access to the Chronos-2 and Mantis checkpoints. Generated data, models, and results remain local and are not committed to the repository.

The first installation can take considerable time. The setup output reports five progress milestones. A successful installation ends with:

``` text
[100%] Completed step 5 of 5: Installation checks passed
Setup completed successfully.
```

## Environment selection

Manual `conda activate` commands are not required. Conda may display activation instructions while creating an environment, but those instructions are optional for this project.

| Workload | Environment selection |
|------------------------------------|------------------------------------|
| R data preparation, modelling, and reporting | R loads the project-local `renv` library. |
| Chronos-2 calls made from R | `reticulate` selects `m4_fmts_foundation` automatically. |
| Chronos-2 and Mantis Python runners | The command uses `conda run -n m4_fmts_foundation`. |
| Rotation Forest, ROCKET, and InceptionTime | The command uses `conda run -n m4_fmts_classifiers`. |

This explicit selection keeps the environments separate without requiring the reader to switch an active terminal environment.

## Running the project

Run the complete experiment and generate the paper results from the repository root:

``` bash
./run_paper_results.sh
```

The script runs six phases in sequence:

1.  R data preparation, modelling, and evaluation.
2.  Rotation Forest, ROCKET, and InceptionTime.
3.  Mantis.
4.  Chronos-2.
5.  Sensitivity analysis.
6.  Paper tables and figures.

Keep these phases sequential; parallelism is within stages. The existing R worker caps are:

| Stage | Worker cap |
|-------|------------|
| Transformations and directional labels | 8 R workers |
| Feature extraction, REAL evaluation preparation, DTW, and the lambda sensitivity grid | 16 R workers |
| Chronos sensitivity forecasting | 1 R/Python GPU worker |

CPU worker counts may be lower when fewer cores are available or parallel execution is disabled. The single Chronos worker avoids loading multiple model instances onto the same GPU. These are the existing caps, not changes to the scientific settings.

The Python settings also cap ROCKET, Rotation Forest, and the Mantis random
forest at 16 jobs; ROCKET's BLAS and Numba thread limits are also 16. These
are concurrency limits, not a guarantee that a run will fit the CPU or memory
resources of every machine.

The full experiment is computationally intensive. The script displays the active phase and records the duration of each phase and the complete pipeline in `results/runtime_<timestamp>.tsv`. Generated data, fitted models, results, and runtime records remain local and are excluded from version control.

After phase 1 has completed successfully, use `./run_paper_results.sh --from-step2` to run phases 2–6 only. This option skips phase 1 and reruns phases 2–6. It assumes the phase 1 outputs are available. The existing `run_paper_results_from_step2.sh` delegates to this same pipeline, including its GPU checks; skipped phase 1 is not recorded as completed. Resume timings are saved in `results/runtime_from_step2_<timestamp>.tsv`.

PyTorch GPU checks accept CUDA or Apple MPS; TensorFlow uses CUDA or Apple Metal. Both launch modes use `amazon/chronos-2` through `src.python.run_tsc_chronos2`. The obsolete Chronos-T5 runner has been removed.

## Reproducibility scope

The repository covers the reproducibility path from environment creation and data preparation through model execution, evaluation, and generation of tables and figures.

The repository contains source code, scripts, configuration, environment and package definitions, and paper PDFs and poster assets. It does not version datasets, model weights, trained checkpoints, logs, caches, generated results, or generated result figures.

## Paper and poster

**A Step in the Right Direction: Bridging Forecasting and Classification for Temporal Foundation Models**\
Foundation Models for Temporal Systems (FMTS), NeurIPS 2026

- [A Step in the Right Direction](paper/a-step-in-the-right-direction.pdf)
- [Workshop poster (PDF)](paper/a-step-in-the-right-direction-poster.pdf)

[![Workshop poster preview](paper/a-step-in-the-right-direction-poster.png)](paper/a-step-in-the-right-direction-poster.pdf)

Citation information is available in [`CITATION.cff`](CITATION.cff).
