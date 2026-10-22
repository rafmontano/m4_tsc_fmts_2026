# Conference-feedback workspace

This is a selective working copy, not a full regeneration environment.
The original project directories are read-only historical sources.

## Provenance

- Starting code: `rafmontano/m4_tsc_fmts_2026`, revision
  `a2f8730e74105975aa7647de16d3def756e8ad7b`.
- Experimental inputs: `/home/rafmontano/Documents/PhD/2026/projects/old/m4_tsc_experiment`.
- Per-file source paths, sizes, modification times, and SHA-256 checksums:
  `results/feedback/input_manifest.csv`.
- Exact submitted manuscript: `paper/submitted_15Sept2026.pdf`.
- Published reference tables: `results/published_reference/`.
- Fitted models, Python environments, caches, and non-default window experiments
  were not copied: neither feedback script requires them.

## R environment

Only the reporting dependencies are installed in `renv/feedback-library`.
Compatible packages were copied from the historical project's installed R library;
missing dependencies were installed separately into this new library. No existing
Conda environment or historical project library is updated.
Package versions are recorded in `environments/feedback-renv.lock`.

Run from this directory, selecting this library explicitly:

```bash
export R_LIBS_USER="$PWD/renv/feedback-library"
export R_LIBS_SITE=""
Rscript --vanilla src/r/feedback/12_baseline_majority_feedback.R
Rscript --vanilla src/r/feedback/05_accuracy_by_horizon_plots_feedback.R
```

Do not run `setup.R` or `run_paper_results.sh` for this feedback-only task.

## Evaluation definition

The training-selected majority direction is unchanged: one vote per saved training
window at each horizon; exact ties choose non-increase. Evaluation uses original
M4 future values compared with the original last historical observation, matched
by series ID from `data/feedback/M4_subset.rds`.

The plotting script recalculates accuracy from saved predictions against these
same evaluation labels for all methods. It does not change any historical RDS
input. The output long table includes `original_accuracy` and
`evaluation_label_changes`, making differences from the historical summaries
explicit. All six frequencies and eleven methods must be complete.

Historical Chronos evaluation files retain their reported "Chronos-2" label in
the plots. Their checkpoint identity has not been independently established;
the research project also contains a legacy Chronos-T5 runner. Do not describe
these imported predictions as a freshly verified Chronos-2 run.

## Outputs

- Majority evaluation objects: `results/feedback/majority/`.
- Re-scored comparison tables: `results/feedback/tables/default/`.
- PDF figures: `results/feedback/figures/default/`.
- Execution logs and validation report: `results/feedback/`.

Both feedback scripts completed. Independent validation checked all 1,177
method/horizon accuracy values across six frequencies and eleven methods against
original M4 data, as well as the training-selected majority predictions. The
outputs include 24 PDF figures. Focused plot axes expand when necessary to show
the full majority-baseline curve; full-comparison legends use three rows.

At the final horizon, the majority baseline exceeds Mantis accuracy for Daily,
Weekly, and Quarterly data under the common original-M4 evaluation labels.
These feedback comparisons are not a claim that the paper used those same labels.

Re-running the two scripts replaces feedback outputs only. No models are trained
and no forecasts are regenerated. Source changes in this workspace remain local
unless explicitly committed and published later.

This working directory is not an independent safe backup. Data, results, and the
local R library are Git-ignored; committing or cloning this repository alone will
not preserve them. A complete archival backup would need those files and the
manifest, and the original research project remains necessary for fitted models
and full regeneration.
