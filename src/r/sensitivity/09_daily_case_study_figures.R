# ==============================================================================
# 09_daily_case_study_figures.R
#
# Purpose:
#   Create Daily case-study figures for adjusted Chronos and SMYL forecasts.
# Inputs:
#   Selected case-study series and the Daily sensitivity dataset.
# Outputs:
#   Case-study figures under results/sensitivity/paper/figures.
# Run from:
#   Project root, directly or through 99_sensitivity_run_all.R.
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

source("src/r/sensitivity/00_sensitivity_common.R")

# Configuration --------------------------------------------------------------

set_sensitivity_frequency("Daily")

case_studies <- tibble::tribble(
  ~st,      ~base_model, ~panel_title,        ~lambda_up, ~lambda_down,
  "D2715",  "chronos",   "Chronos-MANTIS",    1.015,      1.000,
  "D1602",  "smyl",      "SMYL-MANTIS",       1.015,      1.000
)

history_multiple <- 2

# Publication-style visual system. These mappings are shared by the
# individual panels and the combined figure so the outputs cannot drift.
plot_series_levels <- c(
  "Training data",
  "Test data",
  "Base forecast",
  "MANTIS-adjusted forecast"
)

plot_colours <- c(
  "Training data" = "#111111",
  "Test data" = "#D55E00",
  "Base forecast" = "#8A8A8A",
  "MANTIS-adjusted forecast" = "#0072B2"
)

plot_linetypes <- c(
  "Training data" = "solid",
  "Test data" = "dotted",
  "Base forecast" = "solid",
  "MANTIS-adjusted forecast" = "solid"
)

plot_linewidths <- c(
  "Training data" = 0.80,
  "Test data" = 0.80,
  "Base forecast" = 0.85,
  "MANTIS-adjusted forecast" = 1.10
)

forecast_origin_colour <- "#B3B3B3"
adjustment_colour <- unname(plot_colours[["MANTIS-adjusted forecast"]])

out_dir <- "results/paper/figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Helper functions -----------------------------------------------------------

m4_daily_start_date <- function(x) {
  st <- start(x)
  freq <- frequency(x)

  if (length(st) >= 2 && freq > 1) {
    as.Date(paste0(st[1], "-01-01")) + st[2] - 1
  } else {
    as.Date("2000-01-01")
  }
}

direction_label_local <- function(value, last_x) {
  as.integer(value > last_x)
}

adjust_forecast_local <- function(base_forecast,
                                  last_x,
                                  mantis_final,
                                  lambda_up,
                                  lambda_down) {
  base_final <- direction_label_local(tail(base_forecast, 1), last_x)

  gamma <- dplyr::case_when(
    base_final == mantis_final ~ 1,
    base_final != mantis_final & mantis_final == 1 ~ lambda_up,
    base_final != mantis_final & mantis_final == 0 ~ lambda_down
  )

  gamma * base_forecast
}

publication_style <- function(legend_labels = plot_series_levels) {
  legend_guide <- guide_legend(
    nrow = 1,
    byrow = TRUE
  )

  list(
    scale_colour_manual(
      values = plot_colours,
      breaks = plot_series_levels,
      labels = legend_labels,
      drop = FALSE,
      name = NULL,
      guide = legend_guide
    ),
    scale_linetype_manual(
      values = plot_linetypes,
      breaks = plot_series_levels,
      labels = legend_labels,
      drop = FALSE,
      name = NULL,
      guide = legend_guide
    ),
    scale_linewidth_manual(
      values = plot_linewidths,
      breaks = plot_series_levels,
      drop = FALSE,
      guide = "none"
    ),
    scale_x_date(
      date_breaks = "1 week",
      date_labels = "%d %b"
    ),
    theme_minimal(base_size = 12),
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.key.width = grid::unit(1.4, "cm"),
      legend.key.height = grid::unit(0.45, "cm"),
      legend.text = element_text(size = 9.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        colour = "#E6E6E6",
        linewidth = 0.35
      ),
      axis.text = element_text(colour = "#444444"),
      plot.title = element_text(face = "bold", size = 12),
      strip.text = element_text(face = "bold", size = 11)
    )
  )
}

build_case_data <- function(dataset, st, base_model, panel_title,
                            lambda_up, lambda_down,
                            history_multiple = 2) {
  s <- dataset[[which(vapply(dataset, function(z) z$st, character(1)) == st)]]

  y_hist <- as.numeric(s$x)
  y_test <- as.numeric(s$xx)
  h <- length(y_test)
  last_x <- tail(y_hist, 1)

  base_fc <- as.numeric(s$fct[[base_model]])
  mantis_final <- tail(as.integer(s$direction$mantis), 1)

  adjusted_fc <- adjust_forecast_local(
    base_forecast = base_fc,
    last_x = last_x,
    mantis_final = mantis_final,
    lambda_up = lambda_up,
    lambda_down = lambda_down
  )

  start_date <- m4_daily_start_date(s$x)
  hist_dates <- seq.Date(from = start_date, by = "day", length.out = length(y_hist))
  test_dates <- seq.Date(from = max(hist_dates) + 1, by = "day", length.out = h)

  plot_start_date <- max(hist_dates) - history_multiple * h + 1

  observed_df <- tibble::tibble(
    panel = panel_title,
    date = hist_dates,
    value = y_hist,
    series = "Training data"
  ) |>
    filter(date >= plot_start_date)

  future_df <- tibble::tibble(
    panel = panel_title,
    date = test_dates,
    value = y_test,
    series = "Test data"
  )

  fc_df <- tibble::tibble(
    panel = panel_title,
    date = test_dates,
    Base = base_fc,
    Adjusted = adjusted_fc
  ) |>
    pivot_longer(
      cols = c(Base, Adjusted),
      names_to = "series",
      values_to = "value"
    ) |>
    mutate(
      series = recode(
        series,
        "Base" = "Base forecast",
        "Adjusted" = "MANTIS-adjusted forecast"
      )
    )

  # Use the same relative position in every panel. The arrow runs from the
  # base forecast to the adjusted forecast and has one head at its destination.
  arrow_i <- ceiling(h / 2)

  arrow_df <- tibble::tibble(
    panel = panel_title,
    x = test_dates[arrow_i],
    xend = test_dates[arrow_i],
    y = base_fc[arrow_i],
    yend = adjusted_fc[arrow_i]
  )

  list(
    lines = bind_rows(observed_df, future_df, fc_df) |>
      mutate(
        series = factor(series, levels = plot_series_levels)
      ),
    arrows = arrow_df
  )
}

plot_one_case <- function(
  case_data,
  panel_title,
  legend_labels = plot_series_levels
) {
  line_df <- case_data$lines
  arrow_df <- case_data$arrows

  ggplot(
    data = line_df,
    aes(
      x = date,
      y = value,
      colour = series,
      linetype = series,
      linewidth = series
    )
  ) +
    geom_line(
      lineend = "round"
    ) +
    geom_vline(
      xintercept = min(filter(line_df, series == "Test data")$date),
      colour = forecast_origin_colour,
      linetype = "dashed",
      linewidth = 0.40
    ) +
    geom_segment(
      data = arrow_df,
      aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      colour = adjustment_colour,
      linewidth = 1.00,
      alpha = 1.00,
      arrow = grid::arrow(
        ends = "last",
        type = "closed",
        length = grid::unit(0.11, "inches")
      )
    ) +
    labs(
      title = panel_title,
      x = NULL,
      y = "Value"
    ) +
    publication_style(legend_labels = legend_labels)
}

# Load sensitivity dataset ---------------------------------------------------

dataset <- read_sensitivity()

case_data <- purrr::pmap(
  case_studies,
  ~ build_case_data(
    dataset = dataset,
    st = ..1,
    base_model = ..2,
    panel_title = ..3,
    lambda_up = ..4,
    lambda_down = ..5,
    history_multiple = history_multiple
  )
)

names(case_data) <- case_studies$panel_title

chronos_plot <- plot_one_case(
  case_data = case_data[["Chronos-MANTIS"]],
  panel_title = "Chronos-MANTIS: D2715",
  legend_labels = c(
    "Training data",
    "Test data",
    "Chronos",
    "Chronos-MANTIS"
  )
)

smyl_plot <- plot_one_case(
  case_data = case_data[["SMYL-MANTIS"]],
  panel_title = "SMYL-MANTIS: D1602",
  legend_labels = c(
    "Training data",
    "Test data",
    "SMYL",
    "SMYL-MANTIS"
  )
)

# Save PNG and vector-PDF versions -------------------------------------------

save_publication_plot <- function(plot, filename_stem, width, height) {
  png_file <- file.path(out_dir, paste0(filename_stem, ".png"))
  pdf_file <- file.path(out_dir, paste0(filename_stem, ".pdf"))

  ggsave(
    filename = png_file,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    bg = "white"
  )

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

  invisible(c(png = png_file, pdf = pdf_file))
}

save_publication_plot(
  plot = chronos_plot,
  filename_stem = "daily_case_chronos_mantis_D2715",
  width = 8.5,
  height = 5.5
)

save_publication_plot(
  plot = smyl_plot,
  filename_stem = "daily_case_smyl_mantis_D1602",
  width = 8.5,
  height = 5.5
)

# Combined two-panel figure without extra packages ---------------------------

combined_lines <- bind_rows(lapply(case_data, `[[`, "lines"))
combined_arrows <- bind_rows(lapply(case_data, `[[`, "arrows"))

combined_plot <-
  ggplot(
    data = combined_lines,
    aes(
      x = date,
      y = value,
      colour = series,
      linetype = series,
      linewidth = series
    )
  ) +
  geom_line(
    lineend = "round"
  ) +
  geom_vline(
    data = combined_lines |>
      filter(series == "Test data") |>
      group_by(panel) |>
      summarise(xintercept = min(date), .groups = "drop"),
    aes(xintercept = xintercept),
    inherit.aes = FALSE,
    colour = forecast_origin_colour,
    linetype = "dashed",
    linewidth = 0.40
  ) +
  geom_segment(
    data = combined_arrows,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    colour = adjustment_colour,
    linewidth = 1.00,
    alpha = 1.00,
    arrow = grid::arrow(
      ends = "last",
      type = "closed",
      length = grid::unit(0.11, "inches")
    )
  ) +
  facet_wrap(~panel, scales = "free_y", nrow = 1) +
  labs(
    x = NULL,
    y = "Value"
  ) +
  publication_style(
    legend_labels = c(
      "Training data",
      "Test data",
      "Unadjusted forecast",
      "MANTIS-adjusted forecast"
    )
  )

save_publication_plot(
  plot = combined_plot,
  filename_stem = "daily_case_chronos_smyl_mantis_two_panel",
  width = 10,
  height = 4.8
)

if (interactive()) {
  print(chronos_plot)
  print(smyl_plot)
  print(combined_plot)
}
