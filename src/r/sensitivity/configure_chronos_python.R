# ==============================================================================
# configure_chronos_python.R
#
# Purpose:
#   Select the foundation-model Python environment for reticulate.
# Inputs:
#   RETICULATE_PYTHON or the m4_fmts_foundation Conda environment.
# Outputs:
#   The configured reticulate Python session.
# Run from:
#   Project root; sourced by the sensitivity pipeline.
# ==============================================================================

configure_chronos_python <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The R package 'reticulate' is required.")
  }

  python <- Sys.getenv("RETICULATE_PYTHON", unset = "")

  if (nzchar(python)) {
    reticulate::use_python(python, required = TRUE)
  } else {
    conda <- unname(Sys.which("conda"))

    if (!nzchar(conda)) {
      stop("Conda is required and must be available on PATH.")
    }

    reticulate::use_condaenv(
      "m4_fmts_foundation",
      conda = conda,
      required = TRUE
    )
  }

  invisible(reticulate::py_config())
}
