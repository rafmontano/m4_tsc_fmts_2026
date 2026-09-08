# ==============================================================================
# 07_6_plot_daily_contour.R
#
# Purpose:
#   Plot the two-panel Daily sensitivity contour figure.
# Inputs:
#   The long-form sensitivity-surface RDS table.
# Outputs:
#   A Daily contour figure under results/sensitivity/paper/figures.
# Run from:
#   Project root, directly or through 07_8_run_all_paper_outputs.R.
# ==============================================================================

source("src/r/sensitivity/00_sensitivity_common.R")

library(ggplot2)

# Input / output -------------------------------------------------------------

table_dir <- file.path("results", "sensitivity", "paper", "tables")
figure_dir <- file.path("results", "sensitivity", "paper", "figures")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

surface <- readRDS(file.path(table_dir, "sensitivity_surface_long.rds"))

daily_surface <- surface |>
  dplyr::filter(period == "Daily") |>
  dplyr::mutate(
    model_label = factor(
      model_label,
      levels = c("SMYL-MANTIS", "Chronos-MANTIS")
    )
  )

# Plot markers ---------------------------------------------------------------

best_points <- daily_surface |>
  dplyr::group_by(model_label) |>
  dplyr::slice_max(improvement_pct, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()

no_adjustment_points <- daily_surface |>
  dplyr::distinct(model_label) |>
  dplyr::mutate(
    lambda_up = 1,
    lambda_down = 1
  )

reference_points <- daily_surface |>
  dplyr::distinct(model_label) |>
  dplyr::mutate(
    lambda_up = reference_lambda_up,
    lambda_down = reference_lambda_down
  )

# Plot -----------------------------------------------------------------------

p <- ggplot(
  daily_surface,
  aes(
    x = lambda_up,
    y = lambda_down,
    z = improvement_pct
  )
) +
  geom_contour_filled(bins = 12) +
  geom_contour(linewidth = 0.25, colour = "grey35", bins = 12) +
  geom_point(
    data = best_points,
    aes(x = lambda_up, y = lambda_down),
    inherit.aes = FALSE,
    shape = 4,
    size = 3,
    stroke = 0.9
  ) +
  geom_point(
    data = no_adjustment_points,
    aes(x = lambda_up, y = lambda_down),
    inherit.aes = FALSE,
    shape = 1,
    size = 2.6,
    stroke = 0.8
  ) +
  geom_point(
    data = reference_points,
    aes(x = lambda_up, y = lambda_down),
    inherit.aes = FALSE,
    shape = 3,
    size = 2.6,
    stroke = 0.8
  ) +
  facet_wrap(~model_label, nrow = 1) +
  scale_x_continuous(
    breaks = c(1.00, 1.04, 1.08, 1.12),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    title = "Daily sensitivity contours",
    subtitle = "Cross: best point. Circle: no adjustment. Plus: reference lambda.",
    x = expression(lambda[Up]),
    y = expression(lambda[Down]),
    fill = "OWA improvement (%)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    panel.spacing.x = grid::unit(1.5, "lines"),
    plot.subtitle = element_text(size = 9)
  )

# Save -----------------------------------------------------------------------

out_pdf <- file.path(figure_dir, "03_contour_daily_two_panel.pdf")
out_png <- file.path(figure_dir, "03_contour_daily_two_panel.png")

ggsave(out_pdf, p, width = 8, height = 4)
ggsave(out_png, p, width = 8, height = 4, dpi = 300)

cat("[07_6] Saved:", out_pdf, "\n")
cat("[07_6] Saved:", out_png, "\n")
