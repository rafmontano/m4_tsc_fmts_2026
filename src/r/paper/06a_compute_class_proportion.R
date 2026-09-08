# ==============================================================================
# 06a_compute_class_proportion.R
#
# Purpose:
#   Compute class counts and proportions by M4 frequency.
# Inputs:
#   Real-evaluation label exports under data/export.
# Outputs:
#   data/export/class_proportion.csv.
# Run from:
#   Project root.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
})

source("src/r/utils.R") # freq_tag()

# Label ID (prefer global) ---------------------------------------------------

if (!exists("LABEL_ID")) {
  LABEL_ID <- 6
}
LABEL_ID <- as.integer(LABEL_ID)

# Frequencies ----------------------------------------------------------------

freq_list <- c("Yearly", "Quarterly", "Monthly", "Weekly", "Daily", "Hourly")

# Helper functions -----------------------------------------------------------

pick_first_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))[1]
  if (is.na(hit)) {
    return(NA_character_)
  }
  hit
}

find_real_eval_file <- function(label_id, tag, export_dir = file.path("data", "export")) {
  candidates <- c(
    file.path(export_dir, sprintf("real_eval_tsc_l%d_%s.csv", label_id, tag)), # current contract
    file.path(export_dir, sprintf("real_eval_l%d_%s.csv", label_id, tag)), # common variant
    file.path(export_dir, sprintf("real_eval_l%d_data_%s.csv", label_id, tag)),
    file.path(export_dir, sprintf("real_eval_l%d_%s_data.csv", label_id, tag)),
    file.path(export_dir, sprintf("real_eval_%s_l%d_data.csv", tag, label_id)),
    file.path(export_dir, sprintf("real_eval_%s_data_l%d.csv", tag, label_id))
  )

  existing <- candidates[file.exists(candidates)]
  if (length(existing) > 0) {
    return(existing[1])
  }

  if (!dir.exists(export_dir)) {
    return(NA_character_)
  }

  files <- list.files(export_dir, pattern = "\\.csv$", full.names = TRUE)

  tag_re <- sprintf("(^|[_\\-])%s([_\\-\\.]|$)", tag)
  lbl_re <- sprintf("l%d", label_id)

  hits <- files[
    str_detect(basename(files), regex("real_eval", ignore_case = TRUE)) &
      str_detect(basename(files), fixed(lbl_re)) &
      str_detect(basename(files), regex(tag_re, ignore_case = TRUE))
  ]

  if (length(hits) > 0) {
    return(hits[1])
  }

  NA_character_
}

map_class_label <- function(vals) {
  u <- sort(unique(vals))
  u <- u[!is.na(u)]

  # Binary
  if (length(u) <= 2 && all(u %in% c(0, 1))) {
    return(function(x) ifelse(x == 0, "Down", "Up"))
  }

  # Ternary
  if (length(u) <= 3 && all(u %in% c(0, 1, 2))) {
    return(function(x) {
      dplyr::case_when(
        x == 0 ~ "Down",
        x == 1 ~ "Neutral",
        x == 2 ~ "Up",
        TRUE ~ paste0("Class_", x)
      )
    })
  }

  # Unknown / other encodings
  return(function(x) paste0("Class_", x))
}

read_class_counts <- function(period) {
  tag <- freq_tag(period)
  export_dir <- file.path("data", "export")

  path <- find_real_eval_file(LABEL_ID, tag, export_dir)
  if (is.na(path) || !file.exists(path)) {
    message("[06a] WARNING: Missing REAL eval file for ", period, " (tag=", tag, ")")
    return(NULL)
  }

  message("[06a] Using REAL eval file for ", period, ": ", path)

  df <- readr::read_csv(path, show_col_types = FALSE)

  label_col <- pick_first_col(df, c("true_label", "label", paste0("l", LABEL_ID)))
  if (is.na(label_col)) {
    message(
      "[06a] WARNING: No label column found in: ", path,
      " (expected true_label / label / l", LABEL_ID, "). Skipping."
    )
    return(NULL)
  }

  y <- df[[label_col]]
  y <- suppressWarnings(as.integer(y))

  lab_fn <- map_class_label(y)

  counts <- tibble(true_label = y) %>%
    filter(!is.na(true_label)) %>%
    mutate(class_label = lab_fn(true_label)) %>%
    count(class_label, name = "n") %>%
    mutate(frequency = period) %>%
    group_by(frequency) %>%
    mutate(p = n / sum(n)) %>%
    ungroup() %>%
    select(frequency, class_label, n, p)

  counts
}

# Compute --------------------------------------------------------------------

all_counts <- purrr::map_dfr(freq_list, read_class_counts)

if (nrow(all_counts) == 0) {
  message("[06a] No class counts produced. Check REAL eval CSVs exist in data/export/.")
}

# Output (overwrite every run) -----------------------------------------------

out_dir <- file.path("data", "export")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_csv <- file.path(out_dir, "class_proportion.csv")
readr::write_csv(all_counts, out_csv)

cat("[06a] Class proportion CSV saved to:\n  ", out_csv, "\n", sep = "")
print(all_counts)
