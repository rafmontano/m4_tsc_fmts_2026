# =====================================================================
# File: src/r/paper/05_accuracy_by_horizon_plots.R
# Purpose:
#   1) Read consolidated evaluation RDS files from:
#        results/{model}/{model}_eval_{tag}.rds
#   2) Extract REAL accuracy by window mode, frequency, horizon, and model
#      from:
#        obj[[window_mode]]$real$summary
#   3) Save long and wide tables per window mode
#   4) Produce one line plot per frequency per window mode
#   5) Produce one boxplot per frequency per window mode
#
# Benchmark handling:
#   - FFORMA and SMYL exist only under default.
#   - They are fixed benchmarks and are repeated across all window modes.
#
# Outputs:
#   results/paper/tables/{window_mode}/model_accuracy_horizon_long.csv
#   results/paper/tables/{window_mode}/model_accuracy_horizon_long.rds
#   results/paper/tables/{window_mode}/model_accuracy_horizon_wide.csv
#   results/paper/figures/{window_mode}/accuracy_horizon_{frequency}.pdf
#   results/paper/figures/{window_mode}/accuracy_horizon_focused_{frequency}.pdf
#   results/paper/tables/{window_mode}/model_accuracy_horizon_focused_with_chronos_{frequency}.csv
#   results/paper/figures/{window_mode}/accuracy_horizon_focused_with_chronos_{frequency}.pdf
#   results/paper/figures/{window_mode}/accuracy_box_{frequency}.pdf
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(readr)
  library(ggplot2)
  library(ggpubr)
  library(scales)
})

# ---------------------------------------------------------------------
# 0) Config
# ---------------------------------------------------------------------

results_dir <- "results"

window_modes <- c("small", "default", "large", "full")

benchmark_models <- c("fforma", "smyl")
benchmark_window_mode <- "default"

freq_map <- tibble::tribble(
  ~frequency,   ~tag, ~file_stub,
  "Hourly",     "h",  "hourly",
  "Daily",      "d",  "daily",
  "Weekly",     "w",  "weekly",
  "Monthly",    "m",  "monthly",
  "Quarterly",  "q",  "quarterly",
  "Yearly",     "y",  "yearly"
)

frequency_levels <- freq_map$frequency

model_map <- tibble::tribble(
  ~model_id,        ~model,
  "chronos",        "Chronos-2",
  "dtw",            "1-NN DTW",
  "euclidean",      "1-NN ED",
  "fforma",         "FFORMA",
  # "hivecotev2",   "HIVE-COTE 2.0",
  "inceptiontime",  "InceptionTime",
  "rocket",         "ROCKET",
  "rotf",           "Rotation Forest",
  "smyl",           "SMYL",
  "mantis",         "Mantis",
  "xgb",            "XGBoost"
)

preferred_model_order <- c(
  "FFORMA",
  "SMYL",
  "XGBoost",
  "1-NN DTW",
  "1-NN ED",
  "Rotation Forest",
  "ROCKET",
  "InceptionTime",
  "Mantis",
  "Chronos-2"
  # "HIVE-COTE 2.0"
)

focused_models <- c(
  "Mantis",
  "1-NN DTW",
  "SMYL"
)

focused_model_palette <- c(
  "Mantis"  = "#D7191C",
  "1-NN DTW" = "#2C7BB6",
  "SMYL"    = "#333333"
)

focused_model_linetype <- c(
  "Mantis"  = "solid",
  "1-NN DTW" = "dashed",
  "SMYL"    = "dotdash"
)

focused_with_chronos_models <- c(
  "Mantis",
  "Chronos-2",
  "1-NN DTW",
  "SMYL"
)

focused_with_chronos_palette <- c(
  "Mantis"  = "#D7191C",
  "Chronos-2" = "#7B61FF",
  "1-NN DTW" = "#2C7BB6",
  "SMYL"    = "#333333"
)

focused_with_chronos_linetype <- c(
  "Mantis"  = "solid",
  "Chronos-2" = "longdash",
  "1-NN DTW" = "dashed",
  "SMYL"    = "dotdash"
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

get_horizon_col <- function(df) {
  if ("horizon_id" %in% names(df)) return("horizon_id")
  if ("horizon" %in% names(df)) return("horizon")
  NA_character_
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
  
  horizon_col <- get_horizon_col(summ)
  
  if (is.na(horizon_col)) {
    warning("No horizon column found in: ", path, " | window_mode=", window_mode)
    return(tibble())
  }
  
  status_col <- if ("status" %in% names(summ)) "status" else NA_character_
  
  tibble(
    window_mode        = window_mode,
    source_window_mode = effective_window_mode,
    frequency          = frequency,
    tag                = tag,
    model_id           = model_id,
    model              = model,
    horizon            = as.integer(summ[[horizon_col]]),
    accuracy           = as.numeric(summ[["accuracy"]]),
    status             = if (!is.na(status_col)) as.character(summ[[status_col]]) else NA_character_,
    file               = path
  ) %>%
    filter(!is.na(horizon), !is.na(accuracy))
}

theme_paper <- function() {
  ggpubr::theme_pubr(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.title = element_text(face = "bold"),
      panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
}

model_palette <- c(
  "FFORMA"          = "black",
  "SMYL"            = "grey30",
  "XGBoost"         = "#F564E3",
  "1-NN DTW"         = "#F8766D",
  "1-NN ED"  = "#C49A00",
  "Rotation Forest" = "#00B0F6",
  "ROCKET"          = "#00BFC4",
  "InceptionTime"   = "#53B400",
  # "HIVE-COTE 2.0" = "#00A08A",
  "Mantis"          = "#FF0000",
  "Chronos-2"         = "#7B61FF"
)

model_linetype <- c(
  "FFORMA"          = "dotted",
  "SMYL"            = "solid",
  "XGBoost"         = "solid",
  "1-NN DTW"         = "solid",
  "1-NN ED"  = "solid",
  "Rotation Forest" = "solid",
  "ROCKET"          = "solid",
  "InceptionTime"   = "solid",
  # "HIVE-COTE 2.0" = "solid",
  "Mantis"          = "solid",
  "Chronos-2"         = "solid"
)

build_line_plot <- function(df_plot, frequency_i) {
  
  max_h <- max(df_plot$horizon, na.rm = TRUE)
  x_breaks <- if (max_h > 24) seq(1, max_h, by = 4) else sort(unique(df_plot$horizon))
  show_points <- max_h <= 18
  
  p <- ggplot(
    df_plot,
    aes(
      x = horizon,
      y = accuracy,
      group = model,
      colour = model,
      linetype = model
    )
  ) +
    geom_line(linewidth = 1) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(labels = number_format(accuracy = 0.01), limits = c(0, 1)) +
    scale_colour_manual(values = model_palette) +
    scale_linetype_manual(values = model_linetype) +
    labs(
      title = frequency_i,
      x = "Horizon",
      y = "Directional Accuracy"
    ) +
    guides(
      colour = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    ) +
    theme_paper()
  
  if (show_points) {
    p <- p + geom_point(size = 1.5)
  }
  
  p
}

build_box_plot <- function(df_plot, frequency_i, model_levels) {
  
  med_tbl <- df_plot %>%
    group_by(model) %>%
    summarise(
      median_acc = median(accuracy, na.rm = TRUE),
      .groups = "drop"
    )
  
  ggplot(df_plot, aes(x = model, y = accuracy)) +
    geom_boxplot(
      fill = "grey80",
      colour = "grey20",
      outlier.size = 0.8,
      outlier.alpha = 0.7
    ) +
    geom_text(
      data = med_tbl,
      aes(
        x = model,
        y = median_acc,
        label = sprintf("%.3f", median_acc)
      ),
      vjust = -0.6,
      size = 3,
      inherit.aes = FALSE
    ) +
    scale_x_discrete(limits = model_levels) +
    scale_y_continuous(
      labels = number_format(accuracy = 0.01),
      limits = c(0, min(1, max(df_plot$accuracy, na.rm = TRUE) + 0.08))
    ) +
    labs(
      title = paste0(frequency_i, ": Directional Accuracy Distribution Across Horizons"),
      x = NULL,
      y = "Directional Accuracy"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 25, hjust = 1)
    )
}

build_window_long_table <- function(window_mode) {
  
  purrr::pmap_dfr(freq_map, function(frequency, tag, file_stub) {
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
      model     = factor(
        model,
        levels = unique(c(
          preferred_model_order,
          sort(setdiff(unique(model), preferred_model_order))
        ))
      )
    ) %>%
    arrange(frequency, horizon, model) %>%
    mutate(
      frequency = as.character(frequency),
      model     = as.character(model)
    )
}

focused_y_limits <- c(0.40, 1.00)
focused_y_breaks <- seq(0.40, 1.00, by = 0.10)

build_focused_line_plot <- function(
  df_plot,
  frequency_i,
  models = focused_models,
  palette = focused_model_palette,
  linetypes = focused_model_linetype
) {
  
  df_plot <- df_plot %>%
    filter(model %in% models) %>%
    mutate(model = factor(model, levels = models))
  
  max_h <- max(df_plot$horizon, na.rm = TRUE)
  
  x_breaks <- if (max_h > 24) {
    sort(unique(c(seq(1, max_h, by = 4), max_h)))
  } else {
    sort(unique(df_plot$horizon))
  }
  
  ggline(
    df_plot,
    x = "horizon",
    y = "accuracy",
    color = "model",
    linetype = "model",
    size = 1.1,
    add = "point",
    point.size = 2.2,
    palette = palette
  ) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(
      labels = scales::number_format(accuracy = 0.01),
      breaks = focused_y_breaks
    ) +
    coord_cartesian(ylim = focused_y_limits) +
    scale_linetype_manual(values = linetypes) +
    labs(
      title = paste0(frequency_i, ": Directional Accuracy by Horizon"),
      x = "Forecast Horizon",
      y = "Directional Accuracy"
    ) +
    theme_paper()
}


write_window_outputs <- function(window_mode, df_long) {
  
  tables_window_dir <- file.path(tables_dir, window_mode)
  fig_window_dir    <- file.path(fig_dir, window_mode)
  
  dir.create(tables_window_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(fig_window_dir, recursive = TRUE, showWarnings = FALSE)
  
  out_long_csv <- file.path(tables_window_dir, "model_accuracy_horizon_long.csv")
  out_long_rds <- file.path(tables_window_dir, "model_accuracy_horizon_long.rds")
  out_wide_csv <- file.path(tables_window_dir, "model_accuracy_horizon_wide.csv")
  
  readr::write_csv(df_long, out_long_csv)
  saveRDS(df_long, out_long_rds)
  
  message("Wrote long table: ", out_long_csv)
  print(df_long)
  
  model_levels_final <- unique(c(
    preferred_model_order,
    sort(setdiff(unique(df_long$model), preferred_model_order))
  ))
  
  plot_wide <- df_long %>%
    select(frequency, horizon, model, accuracy) %>%
    distinct() %>%
    mutate(
      frequency = factor(frequency, levels = frequency_levels),
      model     = factor(model, levels = model_levels_final)
    ) %>%
    arrange(frequency, horizon, model) %>%
    tidyr::pivot_wider(
      names_from  = model,
      values_from = accuracy
    ) %>%
    mutate(frequency = as.character(frequency))
  
  readr::write_csv(plot_wide, out_wide_csv)
  
  message("Wrote wide table: ", out_wide_csv)
  print(plot_wide)
  
  # -------------------------------------------------------------------
  # Full model line plots by frequency
  # -------------------------------------------------------------------
  
  for (i in seq_len(nrow(freq_map))) {
    
    frequency_i <- freq_map$frequency[i]
    file_stub_i <- freq_map$file_stub[i]
    
    df_plot_i <- df_long %>%
      filter(frequency == frequency_i) %>%
      select(frequency, horizon, model, accuracy)
    
    if (nrow(df_plot_i) == 0L) {
      message("Skip line plot for ", frequency_i, " - no rows available.")
      next
    }
    
    fig_path_i <- file.path(
      fig_window_dir,
      paste0("accuracy_horizon_", file_stub_i, ".pdf")
    )
    
    p_i <- build_line_plot(df_plot_i, frequency_i)
    
    ggsave(
      filename = fig_path_i,
      plot     = p_i,
      width    = 7.2,
      height   = 4.4
    )
    
    message("Wrote line plot: ", fig_path_i)
  }
  
  # -------------------------------------------------------------------
  # Focused line plots by frequency: MANTIS vs DTW_1NN vs SMYL
  # -------------------------------------------------------------------
  
  for (i in seq_len(nrow(freq_map))) {
    
    frequency_i <- freq_map$frequency[i]
    file_stub_i <- freq_map$file_stub[i]
    
    df_focus_i <- df_long %>%
      filter(frequency == frequency_i) %>%
      filter(model %in% focused_models) %>%
      select(frequency, horizon, model, accuracy)
    
    if (nrow(df_focus_i) == 0L) {
      message("Skip focused line plot for ", frequency_i, " - no rows available.")
      next
    }
    
    missing_models_i <- setdiff(focused_models, unique(df_focus_i$model))
    
    if (length(missing_models_i) > 0L) {
      warning(
        "Generating focused plot for ", frequency_i,
        " (window_mode = ", window_mode, ") without: ",
        paste(missing_models_i, collapse = ", "),
        call. = FALSE
      )
    }
    
    out_focus_csv_i <- file.path(
      tables_window_dir,
      paste0("model_accuracy_horizon_focused_", file_stub_i, ".csv")
    )
    
    fig_focus_i <- file.path(
      fig_window_dir,
      paste0("accuracy_horizon_focused_", file_stub_i, ".pdf")
    )
    
    readr::write_csv(df_focus_i, out_focus_csv_i)
    
    p_focus_i <- build_focused_line_plot(df_focus_i, frequency_i)
    
    ggsave(
      filename = fig_focus_i,
      plot     = p_focus_i,
      width    = 7.2,
      height   = 4.4
    )
    
    message("Wrote focused table: ", out_focus_csv_i)
    message("Wrote focused line plot: ", fig_focus_i)
  }

  # -------------------------------------------------------------------
  # Focused line plots with Chronos:
  # MANTIS vs CHRONOS vs DTW_1NN vs SMYL
  # -------------------------------------------------------------------

  for (i in seq_len(nrow(freq_map))) {

    frequency_i <- freq_map$frequency[i]
    file_stub_i <- freq_map$file_stub[i]

    df_focus_chronos_i <- df_long %>%
      filter(frequency == frequency_i) %>%
      filter(model %in% focused_with_chronos_models) %>%
      select(frequency, horizon, model, accuracy)

    if (nrow(df_focus_chronos_i) == 0L) {
      message(
        "Skip focused-with-Chronos line plot for ",
        frequency_i,
        " - no rows available."
      )
      next
    }

    missing_models_i <- setdiff(
      focused_with_chronos_models,
      unique(df_focus_chronos_i$model)
    )

    if (length(missing_models_i) > 0L) {
      warning(
        "Generating focused-with-Chronos plot for ", frequency_i,
        " (window_mode = ", window_mode, ") without: ",
        paste(missing_models_i, collapse = ", "),
        call. = FALSE
      )
    }

    out_focus_chronos_csv_i <- file.path(
      tables_window_dir,
      paste0(
        "model_accuracy_horizon_focused_with_chronos_",
        file_stub_i,
        ".csv"
      )
    )

    fig_focus_chronos_i <- file.path(
      fig_window_dir,
      paste0(
        "accuracy_horizon_focused_with_chronos_",
        file_stub_i,
        ".pdf"
      )
    )

    readr::write_csv(df_focus_chronos_i, out_focus_chronos_csv_i)

    p_focus_chronos_i <- build_focused_line_plot(
      df_plot = df_focus_chronos_i,
      frequency_i = frequency_i,
      models = focused_with_chronos_models,
      palette = focused_with_chronos_palette,
      linetypes = focused_with_chronos_linetype
    )

    ggsave(
      filename = fig_focus_chronos_i,
      plot     = p_focus_chronos_i,
      width    = 7.2,
      height   = 4.4
    )

    message("Wrote focused-with-Chronos table: ", out_focus_chronos_csv_i)
    message("Wrote focused-with-Chronos line plot: ", fig_focus_chronos_i)
  }
  
  # -------------------------------------------------------------------
  # Box plots by frequency
  # -------------------------------------------------------------------
  
  for (i in seq_len(nrow(freq_map))) {
    
    frequency_i <- freq_map$frequency[i]
    file_stub_i <- freq_map$file_stub[i]
    
    df_box_i <- df_long %>%
      filter(frequency == frequency_i) %>%
      select(frequency, horizon, model, accuracy) %>%
      mutate(model = factor(model, levels = model_levels_final))
    
    if (nrow(df_box_i) == 0L) {
      message("Skip box plot for ", frequency_i, " - no rows available.")
      next
    }
    
    fig_box_i <- file.path(
      fig_window_dir,
      paste0("accuracy_box_", file_stub_i, ".pdf")
    )
    
    p_box_i <- build_box_plot(df_box_i, frequency_i, model_levels_final)
    
    ggsave(
      filename = fig_box_i,
      plot     = p_box_i,
      width    = 7.2,
      height   = 4.4
    )
    
    message("Wrote box plot: ", fig_box_i)
  }
  
  invisible(NULL)
}

# ---------------------------------------------------------------------
# 3) Build one set of plots per window mode
# ---------------------------------------------------------------------

for (window_mode in window_modes) {
  
  message("------------------------------------------------------------")
  message("Processing window_mode: ", window_mode)
  message("------------------------------------------------------------")
  
  df_long <- build_window_long_table(window_mode)
  
  if (nrow(df_long) == 0L) {
    warning("No usable evaluation rows found for window_mode=", window_mode, ". Skipping.")
    next
  }
  
  write_window_outputs(window_mode, df_long)
}

message("Finished accuracy-by-horizon plots for all window modes.")
