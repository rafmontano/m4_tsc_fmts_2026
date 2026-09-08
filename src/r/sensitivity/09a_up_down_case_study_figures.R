# ==============================================================================
# 09a_up_down_case_study_figures.R
#
# Purpose:
#   Create upward- and downward-adjustment case-study figures.
# Inputs:
#   Configured case-study series and frequency-specific sensitivity datasets.
# Outputs:
#   Case-study figures under results/sensitivity/paper/figures.
# Run from:
#   Project root, directly or through 99_sensitivity_run_all.R.
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)

source("src/r/sensitivity/00_sensitivity_common.R")

# Configuration --------------------------------------------------------------

case_studies <- tibble::tribble(
  ~period, ~st, ~base_model, ~panel_title, ~expected_adjustment, ~lambda_up, ~lambda_down,
  "Daily", "D2715", "chronos", "Chronos-2-Mantis: D2715", "up", 1.015, 1.000,
  "Daily", "D1602", "smyl", "SMYL-Mantis: D1602", "up", 1.015, 1.000,
  "Weekly", "W39", "chronos", "Chronos-2-Mantis: W39", "down", 1.015, 0.990,
  "Weekly", "W82", "smyl", "SMYL-Mantis: W82", "down", 1.015, 0.995
)

recommended_panels <- c(
  "SMYL-Mantis: D1602",
  "Chronos-2-Mantis: W39"
)

recommended_panel_labels <- c(
  "SMYL-Mantis: D1602" = "(a) Upward adjustment",
  "Chronos-2-Mantis: W39" = "(b) Downward adjustment"
)

history_multiple <- 2L

out_dir <- Sys.getenv(
  "CASE_STUDY_OUT_DIR",
  unset = file.path("results", "paper", "figures")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

plot_series_levels <- c(
  "Observed data",
  "Base point forecast",
  "Upward adjusted point forecast",
  "Downward adjusted point forecast"
)

plot_colours <- c(
  "Observed data" = "#000000",
  "Base point forecast" = "#6F6F6F",
  "Upward adjusted point forecast" = "#009E73",
  "Downward adjusted point forecast" = "#D55E00"
)

plot_linetypes <- c(
  "Observed data" = "solid",
  "Base point forecast" = "dashed",
  "Upward adjusted point forecast" = "solid",
  "Downward adjusted point forecast" = "solid"
)

plot_linewidths <- c(
  "Observed data" = 0.80,
  "Base point forecast" = 0.80,
  "Upward adjusted point forecast" = 1.05,
  "Downward adjusted point forecast" = 1.05
)

plot_shapes <- c(
  "Observed data" = NA,
  "Base point forecast" = 1,
  "Upward adjusted point forecast" = 16,
  "Downward adjusted point forecast" = 16
)

plot_series_labels <- c(
  "Observed data" = "Observed",
  "Base point forecast" = "Original point forecast",
  "Upward adjusted point forecast" = "Upward adjustment",
  "Downward adjusted point forecast" = "Downward adjustment"
)

forecast_origin_colour <- "#B3B3B3"
secondary_colour <- "#D9D9D9"

# Helper functions -----------------------------------------------------------

direction_label_local <- function(value, last_x) {
  as.integer(value > last_x)
}

calc_smape <- function(actual, forecast) {
  mean(
    200 * abs(actual - forecast) / (abs(actual) + abs(forecast)),
    na.rm = TRUE
  )
}

get_m4_mase_frequency <- function(period) {
  dplyr::case_when(
    period == "Monthly" ~ 12L,
    period == "Quarterly" ~ 4L,
    period == "Hourly" ~ 24L,
    TRUE ~ 1L
  )
}

calc_mase <- function(x, actual, forecast, period) {
  mase_frequency <- get_m4_mase_frequency(period)
  denominator <- mean(
    abs(
      x[(mase_frequency + 1L):length(x)] -
        x[1L:(length(x) - mase_frequency)]
    ),
    na.rm = TRUE
  )
  mean(abs(actual - forecast) / denominator, na.rm = TRUE)
}

adjust_forecast_local <- function(base_forecast,
                                  last_x,
                                  mantis_final,
                                  lambda_up,
                                  lambda_down) {
  base_final <- direction_label_local(tail(base_forecast, 1L), last_x)

  gamma <- dplyr::case_when(
    base_final == mantis_final ~ 1,
    mantis_final == 1L ~ lambda_up,
    TRUE ~ lambda_down
  )

  list(
    adjusted_forecast = gamma * base_forecast,
    gamma = gamma,
    base_final = base_final,
    adjustment = dplyr::case_when(
      base_final == mantis_final ~ "none",
      mantis_final == 1L ~ "up",
      TRUE ~ "down"
    )
  )
}

read_period_dataset <- function(period) {
  set_sensitivity_frequency(period)
  read_sensitivity()
}

publication_style <- function() {
  legend_guide <- guide_legend(nrow = 1, byrow = TRUE)

  list(
    scale_colour_manual(
      values = plot_colours,
      breaks = plot_series_levels,
      labels = plot_series_labels,
      drop = TRUE,
      name = NULL,
      guide = legend_guide
    ),
    scale_linetype_manual(
      values = plot_linetypes,
      breaks = plot_series_levels,
      labels = plot_series_labels,
      drop = TRUE,
      name = NULL,
      guide = "none"
    ),
    scale_linewidth_manual(
      values = plot_linewidths,
      breaks = plot_series_levels,
      drop = TRUE,
      guide = "none"
    ),
    scale_shape_manual(
      values = plot_shapes,
      breaks = plot_series_levels,
      labels = plot_series_labels,
      drop = TRUE,
      name = NULL,
      guide = "none"
    ),
    theme_minimal(base_size = 10.5, base_family = "sans"),
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_rect(
        fill = NA,
        colour = secondary_colour,
        linewidth = 0.45
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.key.width = grid::unit(1.2, "cm"),
      legend.key.height = grid::unit(0.38, "cm"),
      legend.text = element_text(size = 8.5, colour = "#333333"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        colour = secondary_colour,
        linewidth = 0.30
      ),
      axis.title = element_text(size = 9.5, colour = "#222222"),
      axis.text = element_text(size = 8.5, colour = "#444444"),
      plot.title = element_text(
        face = "bold",
        size = 11,
        hjust = 0.5,
        colour = "#111111"
      ),
      strip.background = element_blank(),
      strip.text = element_text(
        face = "bold",
        size = 10.5,
        colour = "#111111"
      ),
      plot.margin = margin(7, 8, 5, 7)
    )
  )
}

build_case_data <- function(dataset,
                            period,
                            st,
                            base_model,
                            panel_title,
                            expected_adjustment,
                            lambda_up,
                            lambda_down,
                            history_multiple = 2L) {
  series_index <- which(
    vapply(dataset, function(z) as.character(z$st), character(1)) == st
  )

  if (length(series_index) != 1L) {
    stop("Expected one series for ", st, "; found ", length(series_index), ".")
  }

  s <- dataset[[series_index]]
  y_history <- as.numeric(s$x)
  y_test <- as.numeric(s$xx)
  horizon <- length(y_test)
  last_x <- tail(y_history, 1L)

  base_forecast <- as.numeric(s$fct[[base_model]])
  mantis_final <- tail(as.integer(s$direction$mantis), 1L)
  actual_final <- direction_label_local(tail(y_test, 1L), last_x)

  adjustment_result <- adjust_forecast_local(
    base_forecast = base_forecast,
    last_x = last_x,
    mantis_final = mantis_final,
    lambda_up = lambda_up,
    lambda_down = lambda_down
  )

  if (adjustment_result$adjustment != expected_adjustment) {
    stop(
      panel_title,
      " is a ", adjustment_result$adjustment,
      " adjustment, not ", expected_adjustment, "."
    )
  }

  if (adjustment_result$gamma == 1) {
    stop(panel_title, " has no numerical adjustment (gamma = 1).")
  }

  adjusted_forecast <- adjustment_result$adjusted_forecast
  history_length <- min(length(y_history), history_multiple * horizon)
  plotted_history <- tail(y_history, history_length)

  observed_df <- tibble::tibble(
    panel = panel_title,
    time = c(seq.int(-history_length + 1L, 0L), seq_len(horizon)),
    value = c(plotted_history, y_test),
    series = "Observed data"
  )

  adjusted_series <- if (adjustment_result$adjustment == "up") {
    "Upward adjusted point forecast"
  } else {
    "Downward adjusted point forecast"
  }

  forecast_df <- bind_rows(
    tibble::tibble(
      panel = panel_title,
      time = seq_len(horizon),
      value = base_forecast,
      series = "Base point forecast"
    ),
    tibble::tibble(
      panel = panel_title,
      time = seq_len(horizon),
      value = adjusted_forecast,
      series = adjusted_series
    )
  )

  arrow_index <- ceiling(horizon / 2)
  arrow_df <- tibble::tibble(
    panel = panel_title,
    x = arrow_index,
    xend = arrow_index,
    y = base_forecast[arrow_index],
    yend = adjusted_forecast[arrow_index],
    series = adjusted_series
  )

  base_smape <- calc_smape(y_test, base_forecast)
  adjusted_smape <- calc_smape(y_test, adjusted_forecast)
  base_mase <- calc_mase(y_history, y_test, base_forecast, period)
  adjusted_mase <- calc_mase(y_history, y_test, adjusted_forecast, period)

  summary_df <- tibble::tibble(
    period = period,
    series = st,
    base_model = base_model,
    adjustment = adjustment_result$adjustment,
    lambda_up = lambda_up,
    lambda_down = lambda_down,
    gamma = adjustment_result$gamma,
    base_final_direction = adjustment_result$base_final,
    mantis_final_direction = mantis_final,
    actual_final_direction = actual_final,
    base_smape = base_smape,
    adjusted_smape = adjusted_smape,
    smape_improvement_pct = 100 * (base_smape - adjusted_smape) / base_smape,
    base_mase = base_mase,
    adjusted_mase = adjusted_mase,
    mase_improvement_pct = 100 * (base_mase - adjusted_mase) / base_mase
  )

  list(
    lines = bind_rows(observed_df, forecast_df) |>
      mutate(
        panel = factor(panel, levels = case_studies$panel_title),
        series = factor(series, levels = plot_series_levels)
      ),
    arrows = arrow_df |>
      mutate(
        panel = factor(panel, levels = case_studies$panel_title),
        series = factor(series, levels = plot_series_levels)
      ),
    summary = summary_df
  )
}

plot_one_case <- function(case_data, panel_title) {
  ggplot(
    case_data$lines,
    aes(
      x = time,
      y = value,
      colour = series,
      linetype = series,
      linewidth = series
    )
  ) +
    geom_line(lineend = "round") +
    geom_point(
      data = ~ dplyr::filter(.x, series != "Observed data"),
      aes(shape = series),
      size = 1.65,
      stroke = 0.55
    ) +
    geom_vline(
      xintercept = 0.5,
      colour = forecast_origin_colour,
      linetype = "dashed",
      linewidth = 0.40
    ) +
    geom_segment(
      data = case_data$arrows,
      aes(
        x = x,
        xend = xend,
        y = y,
        yend = yend,
        colour = series
      ),
      inherit.aes = FALSE,
      linewidth = 1.00,
      show.legend = FALSE,
      arrow = grid::arrow(
        ends = "last",
        type = "closed",
        length = grid::unit(0.11, "inches")
      )
    ) +
    labs(
      title = panel_title,
      x = "Time relative to forecast origin",
      y = "Value"
    ) +
    publication_style()
}

save_publication_plot <- function(plot, filename_stem, width, height) {
  pdf_file <- file.path(out_dir, paste0(filename_stem, ".pdf"))
  svg_file <- file.path(out_dir, paste0(filename_stem, ".svg"))
  png_file <- file.path(out_dir, paste0(filename_stem, ".png"))

  ggsave(
    filename = pdf_file,
    plot = plot,
    device = grDevices::pdf,
    width = width,
    height = height,
    units = "in",
    bg = "white",
    useDingbats = FALSE
  )

  svg_written <- FALSE
  if (requireNamespace("svglite", quietly = TRUE)) {
    ggsave(
      filename = svg_file,
      plot = plot,
      device = svglite::svglite,
      width = width,
      height = height,
      units = "in",
      bg = "white"
    )
    svg_written <- TRUE
  } else {
    warning(
      "Package 'svglite' is not installed; PDF and PNG were written, ",
      "but SVG export was skipped."
    )
  }

  ggsave(
    filename = png_file,
    plot = plot,
    device = if (requireNamespace("ragg", quietly = TRUE)) {
      ragg::agg_png
    } else {
      "png"
    },
    width = width,
    height = height,
    units = "in",
    dpi = 600,
    bg = "white"
  )

  output_files <- c(pdf = pdf_file, png = png_file)
  if (svg_written) {
    output_files <- c(output_files, svg = svg_file)
  }

  invisible(output_files)
}

# Build and validate all four cases ------------------------------------------

datasets <- setNames(
  lapply(unique(case_studies$period), read_period_dataset),
  unique(case_studies$period)
)

case_data <- purrr::pmap(
  case_studies,
  function(period,
           st,
           base_model,
           panel_title,
           expected_adjustment,
           lambda_up,
           lambda_down) {
    build_case_data(
      dataset = datasets[[period]],
      period = period,
      st = st,
      base_model = base_model,
      panel_title = panel_title,
      expected_adjustment = expected_adjustment,
      lambda_up = lambda_up,
      lambda_down = lambda_down,
      history_multiple = history_multiple
    )
  }
)

names(case_data) <- case_studies$panel_title

case_summary <- bind_rows(lapply(case_data, `[[`, "summary"))
readr::write_csv(
  case_summary,
  file.path(out_dir, "figure1_case_study_summary.csv")
)

print(case_summary)

# Save the four individual candidates ----------------------------------------

for (i in seq_len(nrow(case_studies))) {
  case_row <- case_studies[i, ]
  panel_data <- case_data[[case_row$panel_title]]

  panel_plot <- plot_one_case(
    case_data = panel_data,
    panel_title = if (case_row$expected_adjustment == "up") {
      "Upward adjustment"
    } else {
      "Downward adjustment"
    }
  )

  filename_stem <- paste(
    "figure1_candidate",
    case_row$expected_adjustment,
    if (case_row$base_model == "chronos") "chronos2_mantis" else "smyl_mantis",
    case_row$st,
    sep = "_"
  )

  save_publication_plot(
    plot = panel_plot,
    filename_stem = filename_stem,
    width = 8.0,
    height = 4.8
  )
}

# Save recommended up/down two-panel Figure 1 --------------------------------

recommended_data <- case_data[recommended_panels]
combined_lines <- bind_rows(lapply(recommended_data, `[[`, "lines")) |>
  mutate(panel = factor(panel, levels = recommended_panels))
combined_arrows <- bind_rows(lapply(recommended_data, `[[`, "arrows")) |>
  mutate(panel = factor(panel, levels = recommended_panels))

combined_plot <-
  ggplot(
    combined_lines,
    aes(
      x = time,
      y = value,
      colour = series,
      linetype = series,
      linewidth = series
    )
  ) +
  geom_line(lineend = "round") +
  geom_point(
    data = ~ dplyr::filter(.x, series != "Observed data"),
    aes(shape = series),
    size = 1.55,
    stroke = 0.55
  ) +
  geom_vline(
    xintercept = 0.5,
    colour = forecast_origin_colour,
    linetype = "dashed",
    linewidth = 0.40
  ) +
  geom_segment(
    data = combined_arrows,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend,
      colour = series
    ),
    inherit.aes = FALSE,
    linewidth = 1.00,
    show.legend = FALSE,
    arrow = grid::arrow(
      ends = "last",
      type = "closed",
      length = grid::unit(0.11, "inches")
    )
  ) +
  facet_wrap(
    ~panel,
    scales = "free_y",
    nrow = 1,
    labeller = labeller(panel = as_labeller(recommended_panel_labels))
  ) +
  labs(
    x = "Time relative to forecast origin",
    y = "Value"
  ) +
  publication_style()

save_publication_plot(
  plot = combined_plot,
  filename_stem = "figure1_recommended_up_down",
  width = 10,
  height = 4.5
)

if (interactive()) {
  print(combined_plot)
}
