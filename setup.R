# ==============================================================================
# setup.R
#
# Purpose: Prepare and validate the project R and Python environments.
# Inputs:  Environment definitions in environments/ and project source files.
# Outputs: Project directories, a project-local renv library and lockfile, Conda
#          environments, and cached foundation-model files.
# Run from: Repository root with Rscript --vanilla setup.R
# ==============================================================================

# Configuration ---------------------------------------------------------------

if (!file.exists("m4-tsc-fmts-2026.Rproj")) {
  stop("Run setup.R from the repository root.")
}

total_steps <- 5L

# Helper functions ------------------------------------------------------------

start_step <- function(step, label) {
  percent <- (step - 1L) * 100L / total_steps
  message(sprintf(
    "\n[%d%%] Starting step %d of %d: %s",
    percent, step, total_steps, label
  ))
}

complete_step <- function(step, label) {
  percent <- step * 100L / total_steps
  message(sprintf(
    "[%d%%] Completed step %d of %d: %s",
    percent, step, total_steps, label
  ))
}

# Main execution --------------------------------------------------------------

message("Setup has five steps. Percentages show completed steps, not elapsed time.")

# Step 1: Project folders ------------------------------------------------------

start_step(1L, "Prepare project folders")

project_directories <- c(
  "data",
  "models",
  "results",
  file.path("results", "xgb")
)

invisible(lapply(
  project_directories,
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))

complete_step(1L, "Project folders are ready")

# Step 2: R environment -------------------------------------------------------

start_step(2L, "Install and record R packages")

download_timeout <- 7200L

options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  timeout = max(download_timeout, getOption("timeout", 60L)),
  renv.config.cache.symlinks = FALSE
)

Sys.setenv(
  HF_HUB_DOWNLOAD_TIMEOUT = as.character(download_timeout),
  HF_HUB_ETAG_TIMEOUT = as.character(download_timeout)
)

if (!requireNamespace("renv", quietly = TRUE)) {
  message("[R] Installing renv...")
  install.packages("renv")
}

if (!file.exists(file.path("renv", "activate.R"))) {
  message("[R] Creating the project R environment...")
  renv::init(bare = TRUE, restart = FALSE)
}

renv::load()
renv::settings$use.cache(FALSE)

if (file.exists("renv.lock")) {
  message("[R] Restoring packages recorded in renv.lock. This can take time...")
  renv::restore(prompt = FALSE)
}

r_packages <- c(
  "cachem", "caret", "dplyr", "dtw", "factoextra", "forecast",
  "future", "future.apply", "ggplot2", "ggpubr", "memoise", "purrr",
  "RANN", "ragg", "readr", "reticulate", "rBayesianOptimization",
  "scales", "stringr", "svglite", "tibble", "tidyr", "tidyverse",
  "tsfeatures", "xgboost"
)

missing_r_packages <- r_packages[
  !vapply(r_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_r_packages) > 0L) {
  message(
    "[R] Installing ",
    length(missing_r_packages),
    " missing CRAN packages. This can take time..."
  )
  renv::install(missing_r_packages, prompt = FALSE)
} else {
  message("[R] Required CRAN packages are already installed.")
}

if (!requireNamespace("M4comp2018", quietly = TRUE)) {
  m4_url <- paste0(
    "https://github.com/carlanetto/M4comp2018/releases/download/0.2.0/",
    "M4comp2018_0.2.0.tar.gz"
  )
  m4_tarball <- tempfile(fileext = ".tar.gz")
  m4_downloaded <- FALSE

  for (attempt in seq_len(3L)) {
    message("[R] Downloading M4comp2018 (attempt ", attempt, " of 3)...")
    unlink(m4_tarball)

    result <- try(
      download.file(
        m4_url,
        m4_tarball,
        method = "libcurl",
        mode = "wb",
        quiet = FALSE
      ),
      silent = TRUE
    )

    archive_contents <- if (
      !inherits(result, "try-error") &&
        file.exists(m4_tarball) &&
        file.info(m4_tarball)$size > 0L
    ) {
      try(utils::untar(m4_tarball, list = TRUE), silent = TRUE)
    } else {
      character()
    }

    m4_downloaded <- !inherits(archive_contents, "try-error") &&
      any(grepl("(^|/)DESCRIPTION$", archive_contents))

    if (m4_downloaded) {
      break
    }

    if (attempt < 3L) {
      Sys.sleep(5L)
    }
  }

  if (!m4_downloaded) {
    stop("Could not download M4comp2018 after three attempts.")
  }

  renv::install(m4_tarball, prompt = FALSE)
  unlink(m4_tarball)
}

if (!requireNamespace("scmamp", quietly = TRUE)) {
  message("[R] Installing scmamp from GitHub...")
  renv::install("b0rxa/scmamp", prompt = FALSE)
}

message("[R] Recording the installed R packages in renv.lock...")
renv::snapshot(type = "implicit", prompt = FALSE)

complete_step(2L, "R environment is ready")

# Steps 3 and 4: Python environments ------------------------------------------

foundation_env <- "m4_fmts_foundation"
classifiers_env <- "m4_fmts_classifiers"
foundation_file <- file.path("environments", "foundation-models.yml")
classifiers_file <- if (Sys.info()[["sysname"]] == "Darwin") {
  file.path("environments", "classifiers-macos.yml")
} else {
  file.path("environments", "classifiers-ubuntu.yml")
}

conda <- unname(Sys.which("conda"))

if (!nzchar(conda)) {
  stop("Conda is required and must be available on PATH.")
}

install_environment <- function(name, definition) {
  if (!file.exists(definition)) {
    stop("Missing environment file: ", definition)
  }

  exists <- name %in% reticulate::conda_list(conda = conda)$name
  action <- if (exists) "update" else "create"

  message(
    "[Python] Conda environment '",
    name,
    "': ",
    action,
    " in progress. This can take time..."
  )

  arguments <- c(
    "env", action, "--name", name,
    "--file", shQuote(normalizePath(definition, winslash = "/"))
  )

  if (exists) {
    arguments <- c(arguments, "--prune")
  }

  status <- system2(conda, arguments)

  if (!identical(status, 0L)) {
    stop("Could not install Conda environment: ", name)
  }
}

start_step(3L, "Install the foundation-model Python environment")
install_environment(foundation_env, foundation_file)
complete_step(3L, "Foundation-model Python environment is ready")

start_step(4L, "Install the classifier Python environment")
install_environment(classifiers_env, classifiers_file)
complete_step(4L, "Classifier Python environment is ready")

# Step 5: Installation check --------------------------------------------------

start_step(5L, "Check R, Python, and model access")

check_python <- function(environment, code) {
  status <- system2(
    conda,
    c("run", "--name", environment, "python", "-c", shQuote(code))
  )

  if (!identical(status, 0L)) {
    stop("Python check failed: ", environment)
  }
}

message(
  "[Check] Testing foundation-model packages and model access. ",
  "The first run may download model files..."
)

check_python(
  foundation_env,
  paste(
    paste(
      "import chronos, joblib, mantis, numpy, pandas, rdata,",
      "rpy2, sklearn, sktime, torch"
    ),
    "from chronos import Chronos2Pipeline",
    paste(
      "Chronos2Pipeline.from_pretrained(",
      "'amazon/chronos-2', device_map='cpu')"
    ),
    "from mantis.architecture import Mantis8M",
    "Mantis8M(device='cpu').from_pretrained('paris-noah/Mantis-8M')",
    sep = "; "
  )
)

message("[Check] Testing classifier packages...")

check_python(
  classifiers_env,
  paste(
    paste(
      "import joblib, numba, numpy, pandas, rdata, rpy2,",
      "sklearn, sktime, tensorflow"
    ),
    paste(
      "from sktime.classification.distance_based import",
      "KNeighborsTimeSeriesClassifier"
    ),
    "from sktime.classification.sklearn import RotationForest",
    "from sktime.classification.kernel_based import RocketClassifier",
    paste(
      "from sktime.classification.deep_learning import",
      "InceptionTimeClassifier"
    ),
    "from sktime.classification.hybrid import HIVECOTEV2",
    sep = "; "
  )
)

source("src/r/sensitivity/configure_chronos_python.R")
configure_chronos_python()

complete_step(5L, "Installation checks passed")
message("\nSetup completed successfully.")
