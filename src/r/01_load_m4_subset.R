# =====================================================================
# 01_load_m4_subset.R
# Load full dataset (M4 or synthetic) and save it
# Inputs (globals): SYNTHETIC, qa_dir, subset_file
# Output: subset_file (RDS)
# =====================================================================

# ---------------------------------------------------------------------
# 1. Load full dataset
# ---------------------------------------------------------------------

if (SYNTHETIC) {
  m4_qa_path <- file.path(qa_dir, "M4_qa.rds")
  M4 <- readRDS(m4_qa_path)
  cat("Synthetic QA dataset loaded.\n")
} else {
  M4 <- M4comp2018::M4
  cat("M4 dataset loaded.\n")
}

cat("Total series loaded:", length(M4), "\n")

# ---------------------------------------------------------------------
# 2. Save output
# ---------------------------------------------------------------------

saveRDS(M4, file = subset_file)
cat("Saved dataset into:", subset_file, "\n")