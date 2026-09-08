# =====================================================================
# File: src/r/paper/04_cd_diagram_models.R
# Purpose:
#   1) Read consolidated evaluation RDS files from:
#        results/{model}/{model}_eval_{tag}.rds
#   2) Extract REAL accuracy by window mode, dataset, model, and horizon
#      from:
#        obj[[window_mode]]$real$summary
#   3) Write one long table per window mode:
#        dataset, model, accuracy
#      where dataset = {frequency}_{horizon}
#   4) Build one wide CD input table per window mode
#   5) Produce one CD diagram per window mode if >= 2 models have
#      complete coverage
#
# Window modes:
#   small, default, large, full
#
# Notes:
#   - Reads ONLY consolidated RDS files
#   - Uses ONLY REAL evaluation summaries
#   - Does NOT aggregate across horizons
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(readr)
})

if (!requireNamespace("scmamp", quietly = TRUE)) {
  stop("Package 'scmamp' is required. Install with install.packages('scmamp').")
}
suppressPackageStartupMessages(library(scmamp))

# ---------------------------------------------------------------------
# 0) Config
# ---------------------------------------------------------------------

results_dir <- "results"

window_modes <- c("small", "default", "large", "full")

benchmark_models <- c("fforma", "smyl")
benchmark_window_mode <- "default"

freq_map <- tibble::tribble(
  ~frequency,    ~tag,
  "Hourly",     "h",
  "Daily",      "d",
  "Weekly",     "w",
  "Monthly",    "m",
  "Quarterly",  "q",
  "Yearly",     "y"
)

frequency_levels <- freq_map$frequency

model_map <- tibble::tribble(
  ~model_id,        ~model,
  "dtw",            "1-NN DTW",
  "euclidean",      "1-NN ED",
  "fforma",         "FFORMA",
  "inceptiontime",  "InceptionTime",
  "rocket",         "ROCKET",
  "rotf",           "RotF",
  "smyl",           "SMYL",
  "mantis",         "Mantis",
  "xgb",            "XGBoost",
  "chronos",        "Chronos-2"
)

preferred_model_order <- c(
  "FFORMA", "SMYL",
  "XGBoost",
  "1-NN DTW",
  "1-NN ED",
  "RotF",
  "ROCKET",
  "Mantis",
  "InceptionTime",
  "Chronos-2"
)

# ---------------------------------------------------------------------
# 1) Output paths
# ---------------------------------------------------------------------

tables_dir <- file.path("results", "paper", "tables")
fig_dir    <- file.path("results", "paper", "figures")

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------
# 2) Helpers
# ---------------------------------------------------------------------

safe_read_rds <- function(path) {
  tryCatch(readRDS(path), error = function(e) NULL)
}

read_one_eval_summary <- function(model_id, model, tag, frequency, window_mode) {
  
  path <- file.path(results_dir, model_id, sprintf("%s_eval_%s.rds", model_id, tag))
  
  if (!file.exists(path)) {
    return(tibble())
  }
  
  obj <- safe_read_rds(path)
  
  if (is.null(obj)) {
    warning("Failed to read RDS: ", path)
    return(tibble())
  }
  
  effective_window_mode <- ifelse(
    model_id %in% benchmark_models,
    benchmark_window_mode,
    window_mode
  )
  
  if (!(effective_window_mode %in% names(obj))) {
    warning("Window mode '", effective_window_mode, "' not found in: ", path)
    return(tibble())
  }
  
  if (is.null(obj[[effective_window_mode]]$real) ||
      is.null(obj[[effective_window_mode]]$real$summary)) {
    warning("Missing obj[[effective_window_mode]]$real$summary in: ", path)
    return(tibble())
  }
  
  summ <- obj[[effective_window_mode]]$real$summary
  
  if (!is.data.frame(summ) || nrow(summ) == 0L) {
    warning("Empty or invalid REAL summary in: ", path, " | window_mode=", window_mode)
    return(tibble())
  }
  
  if (!("accuracy" %in% names(summ))) {
    warning("Column 'accuracy' not found in: ", path, " | window_mode=", window_mode)
    return(tibble())
  }
  
  horizon_col <- dplyr::case_when(
    "horizon_id" %in% names(summ) ~ "horizon_id",
    "horizon"    %in% names(summ) ~ "horizon",
    TRUE                          ~ NA_character_
  )
  
  if (is.na(horizon_col)) {
    warning("No horizon column found in: ", path, " | window_mode=", window_mode)
    return(tibble())
  }
  
  status_col <- if ("status" %in% names(summ)) "status" else NA_character_
  
  tibble(
    window_mode = window_mode,
    source_window_mode = effective_window_mode,
    frequency   = frequency,
    tag         = tag,
    model_id    = model_id,
    model       = model,
    horizon     = as.integer(summ[[horizon_col]]),
    dataset     = paste0(frequency, "_", as.integer(summ[[horizon_col]])),
    accuracy    = as.numeric(summ[["accuracy"]]),
    status      = if (!is.na(status_col)) as.character(summ[[status_col]]) else NA_character_,
    file        = path
  ) %>%
    filter(!is.na(horizon), !is.na(accuracy))
}

build_window_long_table <- function(window_mode) {
  
  purrr::pmap_dfr(freq_map, function(frequency, tag) {
    purrr::pmap_dfr(model_map, function(model_id, model) {
      read_one_eval_summary(
        model_id    = model_id,
        model       = model,
        tag         = tag,
        frequency   = frequency,
        window_mode = window_mode
      )
    })
  }) %>%
    mutate(
      frequency = factor(frequency, levels = frequency_levels),
      model     = factor(model, levels = preferred_model_order)
    ) %>%
    arrange(frequency, horizon, model) %>%
    mutate(
      frequency = as.character(frequency),
      model     = as.character(model)
    ) %>%
    select(
      window_mode,
      source_window_mode,
      dataset,
      model,
      accuracy,
      frequency,
      horizon,
      tag,
      model_id,
      status,
      file
    )
}

write_frequency_cd_diagrams <- function(window_mode, df_long) {
  
  tables_window_dir <- file.path(tables_dir, window_mode)
  fig_window_dir    <- file.path(fig_dir, window_mode)
  
  for (freq_i in frequency_levels) {
    
    tag_i <- freq_map |>
      filter(frequency == freq_i) |>
      pull(tag)
    
    df_freq <- df_long |>
      filter(frequency == freq_i)
    
    if (nrow(df_freq) == 0L) {
      warning("No rows for frequency=", freq_i, " | window_mode=", window_mode)
      next
    }
    
    dataset_units <- df_freq |>
      distinct(dataset) |>
      arrange(dataset) |>
      pull(dataset)
    
    n_total <- length(dataset_units)
    
    models_complete <- df_freq |>
      distinct(dataset, model) |>
      group_by(model) |>
      summarise(n = n_distinct(dataset), .groups = "drop") |>
      filter(n == n_total) |>
      pull(model)
    
    out_wide_csv <- file.path(
      tables_window_dir,
      paste0("model_accuracy_cd_", tag_i, ".csv")
    )
    
    fig_path <- file.path(
      fig_window_dir,
      paste0("cd_diagram_models_", tag_i, ".pdf")
    )
    
    if (length(models_complete) < 2L) {
      warning(
        "Fewer than two complete models for frequency=", freq_i,
        " | window_mode=", window_mode,
        ". Skipping CD plot."
      )
      next
    }
    
    model_levels <- unique(c(
      preferred_model_order,
      sort(setdiff(models_complete, preferred_model_order))
    ))
    
    wide_freq <- df_freq |>
      filter(model %in% models_complete) |>
      mutate(model = factor(model, levels = model_levels)) |>
      select(dataset, model, accuracy) |>
      distinct() |>
      arrange(dataset, model) |>
      tidyr::pivot_wider(
        names_from = model,
        values_from = accuracy
      )
    
    readr::write_csv(wide_freq, out_wide_csv)
    
    mat <- as.data.frame(wide_freq)
    rn <- mat$dataset
    mat <- as.matrix(mat[, -1, drop = FALSE])
    rownames(mat) <- as.character(rn)
    
    pdf(fig_path, width = 8, height = 4.8, useDingbats = FALSE)
    par(mar = c(1.8, 2.3, 0.8, 2.3), xpd = NA)
    scmamp::plotCD(
      results.matrix = mat,
      alpha = 0.05,
      cex = 0.75,
      reverse = TRUE
    )
    dev.off()
    
    message("Wrote frequency CD table: ", out_wide_csv)
    message("Wrote frequency CD diagram: ", fig_path)
  }
  
  invisible(NULL)
}




write_window_outputs <- function(window_mode, df_long) {
  
  tables_window_dir <- file.path(tables_dir, window_mode)
  fig_window_dir    <- file.path(fig_dir, window_mode)
  
  dir.create(tables_window_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(fig_window_dir, recursive = TRUE, showWarnings = FALSE)
  
  out_long_csv <- file.path(
    tables_window_dir,
    "model_accuracy_by_frequency.csv"
  )
  
  out_long_rds <- file.path(
    tables_window_dir,
    "model_accuracy_by_frequency.rds"
  )
  
  out_coverage_csv <- file.path(
    tables_window_dir,
    "model_accuracy_coverage.csv"
  )
  
  out_wide_csv <- file.path(
    tables_window_dir,
    "model_accuracy_by_frequency_table_cd.csv"
  )
  
  fig_path <- file.path(
    fig_window_dir,
    "cd_diagram_models.pdf"
  )
  
  out_long_last_csv <- file.path(
    tables_window_dir,
    "model_accuracy_by_frequency_last_horizon.csv"
  )
  
  out_wide_last_csv <- file.path(
    tables_window_dir,
    "model_accuracy_by_frequency_table_cd_last_horizon.csv"
  )
  
  fig_last_path <- file.path(
    fig_window_dir,
    "cd_diagram_models_last_horizon.pdf"
  )
  
  readr::write_csv(df_long, out_long_csv)
  saveRDS(df_long, out_long_rds)
  
  message("Wrote long table: ", out_long_csv)
  print(df_long)
  
  coverage <- df_long %>%
    distinct(dataset, model) %>%
    mutate(present = 1L) %>%
    tidyr::pivot_wider(
      names_from  = dataset,
      values_from = present,
      values_fill = 0
    ) %>%
    arrange(model)
  
  readr::write_csv(coverage, out_coverage_csv)
  
  message("Coverage for window_mode=", window_mode, " (1=present, 0=missing):")
  print(coverage)
  
  dataset_units <- df_long %>%
    distinct(dataset) %>%
    arrange(dataset) %>%
    pull(dataset)
  
  n_total <- length(dataset_units)
  
  models_complete <- df_long %>%
    distinct(dataset, model) %>%
    group_by(model) %>%
    summarise(n = n_distinct(dataset), .groups = "drop") %>%
    filter(n == n_total) %>%
    pull(model)
  
  if (length(models_complete) == 0L) {
    wide_placeholder <- tibble(dataset = dataset_units)
    readr::write_csv(wide_placeholder, out_wide_csv)
    warning(
      "No complete models for all-horizon CD, window_mode=", window_mode,
      "; wrote placeholder wide table and skipped all-horizon CD plot."
    )
  } else {
    
    model_levels <- unique(c(
      preferred_model_order,
      sort(setdiff(models_complete, preferred_model_order))
    ))
    
    wide <- df_long %>%
      filter(model %in% models_complete) %>%
      mutate(model = factor(model, levels = model_levels)) %>%
      select(dataset, model, accuracy) %>%
      distinct() %>%
      arrange(dataset, model) %>%
      tidyr::pivot_wider(
        names_from  = model,
        values_from = accuracy
      )
    
    readr::write_csv(wide, out_wide_csv)
    
    message("Wrote wide CD table: ", out_wide_csv)
    print(wide)
    
    if (length(models_complete) < 2L) {
      warning(
        "Only one complete model for all-horizon CD, window_mode=", window_mode,
        "; CD plot requires >= 2. Skipping all-horizon CD plot."
      )
    } else {
      
      mat <- as.data.frame(wide)
      rn  <- mat$dataset
      mat <- as.matrix(mat[, -1, drop = FALSE])
      rownames(mat) <- as.character(rn)
      
      pdf(fig_path, width = 8, height = 4.8, useDingbats = FALSE)
      par(mar = c(1.8, 2.3, 0.8, 2.3), xpd = NA)
      scmamp::plotCD(
        results.matrix = mat,
        alpha = 0.05,
        cex = 0.75,
        reverse = TRUE
      )
      dev.off()
      
      message("Wrote CD diagram: ", fig_path)
    }
  }
  
  # -------------------------------------------------------------------
  # Last-horizon-only CD diagram
  # -------------------------------------------------------------------
  
  df_last <- df_long %>%
    group_by(frequency, model) %>%
    filter(horizon == max(horizon, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(dataset = frequency)
  
  readr::write_csv(df_last, out_long_last_csv)
  
  message("Wrote last-horizon long table: ", out_long_last_csv)
  print(df_last)
  
  last_dataset_units <- df_last %>%
    distinct(dataset) %>%
    arrange(dataset) %>%
    pull(dataset)
  
  last_n_total <- length(last_dataset_units)
  
  last_models_complete <- df_last %>%
    distinct(dataset, model) %>%
    group_by(model) %>%
    summarise(n = n_distinct(dataset), .groups = "drop") %>%
    filter(n == last_n_total) %>%
    pull(model)
  
  if (length(last_models_complete) == 0L) {
    
    last_wide_placeholder <- tibble(dataset = last_dataset_units)
    readr::write_csv(last_wide_placeholder, out_wide_last_csv)
    
    warning(
      "No complete models for last-horizon CD, window_mode=", window_mode,
      "; wrote placeholder wide table and skipped last-horizon CD plot."
    )
    
  } else {
    
    last_model_levels <- unique(c(
      preferred_model_order,
      sort(setdiff(last_models_complete, preferred_model_order))
    ))
    
    wide_last <- df_last %>%
      filter(model %in% last_models_complete) %>%
      mutate(model = factor(model, levels = last_model_levels)) %>%
      select(dataset, model, accuracy) %>%
      distinct() %>%
      arrange(dataset, model) %>%
      tidyr::pivot_wider(
        names_from  = model,
        values_from = accuracy
      )
    
    readr::write_csv(wide_last, out_wide_last_csv)
    
    message("Wrote last-horizon wide CD table: ", out_wide_last_csv)
    print(wide_last)
    
    if (length(last_models_complete) < 2L) {
      
      warning(
        "Only one complete model for last-horizon CD, window_mode=", window_mode,
        "; CD plot requires >= 2. Skipping last-horizon CD plot."
      )
      
    } else {
      
      mat_last <- as.data.frame(wide_last)
      rn_last  <- mat_last$dataset
      mat_last <- as.matrix(mat_last[, -1, drop = FALSE])
      rownames(mat_last) <- as.character(rn_last)
      
      pdf(fig_last_path, width = 8, height = 4.8, useDingbats = FALSE)
      par(mar = c(1.8, 2.3, 0.8, 2.3), xpd = NA)
      scmamp::plotCD(
        results.matrix = mat_last,
        alpha = 0.05,
        cex = 0.75,
        reverse = TRUE
      )
      dev.off()
      
      message("Wrote last-horizon CD diagram: ", fig_last_path)
    }
  }
  
  write_frequency_cd_diagrams(window_mode, df_long)
  
  invisible(NULL)
}

# ---------------------------------------------------------------------
# 3) Build CD diagrams by window mode
# ---------------------------------------------------------------------

for (window_mode in window_modes) {
  
  message("------------------------------------------------------------")
  message("Processing window_mode: ", window_mode)
  message("------------------------------------------------------------")
  
  df_long <- build_window_long_table(window_mode)
  
  if (nrow(df_long) == 0L) {
    warning("No rows found for window_mode=", window_mode, ". Skipping.")
    next
  }
  
  write_window_outputs(window_mode, df_long)
}

message("Finished CD diagram generation for all window modes.")
