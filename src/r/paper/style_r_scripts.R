# ==============================================================================
# style_r_scripts.R
#
# Purpose:
#   Format all project R scripts using styler.
# Inputs:
#   setup.R and R scripts under src/r.
# Outputs:
#   Formatted R source files.
# Run from:
#   Project root with Rscript src/r/style_all.R.
# ==============================================================================

if (!requireNamespace("styler", quietly = TRUE)) {
  message(
    "Package 'styler' is required. Install it with: ",
    "install.packages('styler')"
  )
}

r_files <- c(
  "setup.R",
  list.files(
    "src/r",
    pattern = "\\.[Rr]$",
    recursive = TRUE,
    full.names = TRUE
  )
)

r_files <- r_files[file.exists(r_files)]

cat("Formatting", length(r_files), "R scripts...\n")

for (file in r_files) {
  cat("Formatting:", file, "\n")
  styler::style_file(file)
}

cat("R formatting completed successfully.\n")