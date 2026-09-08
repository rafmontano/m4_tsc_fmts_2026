# =====================================================================
# 12_baseline_fforma_smyl.R
# Evaluate Smyl and FFORMA baselines on REAL M4 data
#
# Design:
#   - REAL-only evaluation
#   - no train/test split
#   - no LABEL_ID
#   - no threshold c
#   - no scaling
#   - directly uses x, xx, and pt_ff from M4 object
#   - one consolidated RDS per method per frequency
#
# Inputs (globals):
#   results_dir, periods, RUN_PARALLEL
#
# Requires:
#   - src/r/utils.R
#   - src/r/parallel_util.R
#   - src/r/util_metrics.R
#   - M4comp2018
# =====================================================================

library(dplyr)
library(tibble)
library(caret)
library(future)
library(future.apply)
library(M4comp2018)

source("src/r/utils.R")
source("src/r/parallel_util.R")
source("src/r/util_metrics.R")

results_smyl_dir   <- file.path(results_dir, "smyl")
results_fforma_dir <- file.path(results_dir, "fforma")

if (!dir.exists(results_smyl_dir))   dir.create(results_smyl_dir, recursive = TRUE)
if (!dir.exists(results_fforma_dir)) dir.create(results_fforma_dir, recursive = TRUE)

M4_all <- M4comp2018::M4

for (period_i in periods) {
  
  TAG_i <- freq_tag(period_i)
  
  smyl_eval_i   <- file.path(results_smyl_dir,   paste0("smyl_eval_", TAG_i, ".rds"))
  fforma_eval_i <- file.path(results_fforma_dir, paste0("fforma_eval_", TAG_i, ".rds"))
  
  M4_period <- Filter(function(s) as.character(s$period) == period_i, M4_all)
  
  cat("\n[12] Period =", period_i, "| series =", length(M4_period), "\n")
  
  if (length(M4_period) == 0L) {
    cat("[12] Skip", period_i, "- empty period subset\n")
    next
  }
  
  process_one_series <- function(s) {
    x_hist  <- as.numeric(unlist(s$x,  use.names = FALSE))
    xx_true <- as.numeric(unlist(s$xx, use.names = FALSE))
    
    if (length(x_hist) == 0L) return(NULL)
    if (length(xx_true) == 0L) return(NULL)
    if (any(!is.finite(xx_true))) return(NULL)
    if (is.null(s$pt_ff)) return(NULL)
    
    pt_ff <- s$pt_ff
    
    if (is.matrix(pt_ff)) {
      if (nrow(pt_ff) < 2L) return(NULL)
      if (ncol(pt_ff) != length(xx_true)) return(NULL)
      xx_smyl   <- as.numeric(pt_ff[1, ])
      xx_fforma <- as.numeric(pt_ff[2, ])
    } else {
      pt_flat <- as.numeric(unlist(pt_ff, use.names = FALSE))
      H <- length(xx_true)
      if (length(pt_flat) < 2L * H) return(NULL)
      xx_smyl   <- pt_flat[1:H]
      xx_fforma <- pt_flat[(H + 1L):(2L * H)]
    }
    
    if (any(!is.finite(xx_smyl))) return(NULL)
    if (any(!is.finite(xx_fforma))) return(NULL)
    
    tibble(
      series_name   = if (!is.null(s$st)) as.character(s$st) else NA_character_,
      labels_true   = list(compute_label_vector(x_hist, xx_true)),
      labels_smyl   = list(compute_label_vector(x_hist, xx_smyl)),
      labels_fforma = list(compute_label_vector(x_hist, xx_fforma))
    )
  }
  
  rows <- run_step_parallel(
    dataset = M4_period,
    step_fun = process_one_series,
    chunk_size = NULL,
    save_foldername = NULL,
    step_name = paste0("baseline_", TAG_i)
  )
  
  rows <- rows[!vapply(rows, is.null, logical(1))]
  baseline_df <- bind_rows(rows)
  
  if (nrow(baseline_df) == 0L) {
    cat("[12] Skip", period_i, "- empty baseline dataset\n")
    next
  }
  
  H_i <- length(baseline_df$labels_true[[1]])
  
  cat("[12] Built baseline dataset | rows =", nrow(baseline_df), "| horizons =", H_i, "\n")
  
  summary_smyl_rows   <- vector("list", H_i)
  summary_fforma_rows <- vector("list", H_i)
  
  detail_smyl   <- vector("list", H_i)
  detail_fforma <- vector("list", H_i)
  
  for (h in seq_len(H_i)) {
    
    y_true   <- vapply(baseline_df$labels_true,   function(v) as.integer(v[h]), integer(1))
    y_smyl   <- vapply(baseline_df$labels_smyl,   function(v) as.integer(v[h]), integer(1))
    y_fforma <- vapply(baseline_df$labels_fforma, function(v) as.integer(v[h]), integer(1))
    
    res_smyl <- compute_binary_eval(
      y_true     = y_true,
      pred_class = y_smyl,
      period_i   = period_i,
      tag_i      = TAG_i,
      horizon_i  = h,
      eval_type  = "real"
    )
    
    res_fforma <- compute_binary_eval(
      y_true     = y_true,
      pred_class = y_fforma,
      period_i   = period_i,
      tag_i      = TAG_i,
      horizon_i  = h,
      eval_type  = "real"
    )
    
    res_smyl$summary$method   <- "smyl"
    res_fforma$summary$method <- "fforma"
    
    summary_smyl_rows[[h]]   <- res_smyl$summary
    summary_fforma_rows[[h]] <- res_fforma$summary
    
    detail_smyl[[h]] <- c(
      res_smyl$detail,
      list(series_name = baseline_df$series_name)
    )
    
    detail_fforma[[h]] <- c(
      res_fforma$detail,
      list(series_name = baseline_df$series_name)
    )
    
    cat("[12] Period:", period_i,
        "| horizon:", h,
        "| smyl acc:", round(res_smyl$summary$accuracy, 4),
        "| fforma acc:", round(res_fforma$summary$accuracy, 4), "\n")
    
    rm(y_true, y_smyl, y_fforma, res_smyl, res_fforma)
    gc()
  }
  
  smyl_eval <- list(
    real = list(
      summary = bind_rows(summary_smyl_rows),
      detail  = detail_smyl,
      dataset = baseline_df
    )
  )
  
  fforma_eval <- list(
    real = list(
      summary = bind_rows(summary_fforma_rows),
      detail  = detail_fforma,
      dataset = baseline_df
    )
  )
  
  save_consolidated_eval_object(smyl_eval, smyl_eval_i, "default")
  save_consolidated_eval_object(fforma_eval, fforma_eval_i, "default")
  
  cat("[12] Saved:", smyl_eval_i, "| mode: default\n")
  cat("[12] Saved:", fforma_eval_i, "| mode: default\n")
  
  rm(
    M4_period, rows, baseline_df, H_i,
    summary_smyl_rows, summary_fforma_rows,
    detail_smyl, detail_fforma,
    smyl_eval, fforma_eval
  )
  gc()
}