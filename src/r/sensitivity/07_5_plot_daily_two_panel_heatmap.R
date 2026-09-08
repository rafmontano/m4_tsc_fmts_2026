# ==============================================================================
# 07_5_plot_daily_two_panel_heatmap.R
#
# Purpose:
#   Plot the two-panel Daily sensitivity heatmap.
# Inputs:
#   The long-form sensitivity-surface RDS table.
# Outputs:
#   A Daily heatmap under results/sensitivity/paper/figures.
# Run from:
#   Project root, directly or through 07_8_run_all_paper_outputs.R.
# ==============================================================================

source("src/r/sensitivity/00_sensitivity_common.R")

library(ggplot2)

# Paper-wide display standards
model_levels <- c("SMYL-Mantis", "Chronos-2-Mantis")
colour_negative <- "#D55E00"
colour_neutral <- "#FFFFFF"
colour_positive <- "#009E73"
colour_ink <- "#1A1A1A"

# Input / output -------------------------------------------------------------

table_dir <- file.path("results", "sensitivity", "paper", "tables")
figure_dir <- file.path("results", "sensitivity", "paper", "figures")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

surface <- readRDS(file.path(table_dir, "sensitivity_surface_long.rds"))

daily_surface <- surface |>
  dplyr::filter(period == "Daily") |>
  dplyr::mutate(
    model_label = dplyr::recode(
      as.character(model_label),
      "SMYL-MANTIS" = "SMYL-Mantis",
      "Chronos-MANTIS" = "Chronos-2-Mantis"
    ),
    model_label = factor(
      model_label,
      levels = model_levels
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

# Plot -----------------------------------------------------------------------

p <- ggplot(
  daily_surface,
  aes(
    x = lambda_up,
    y = lambda_down,
    fill = improvement_pct
  )
) +
  geom_tile() +
  geom_point(
    data = best_points,
    aes(x = lambda_up, y = lambda_down),
    inherit.aes = FALSE,
    shape = 4,
    size = 3.4,
    stroke = 1.1,
    colour = colour_ink
  ) +
  geom_point(
    data = no_adjustment_points,
    aes(x = lambda_up, y = lambda_down),
    inherit.aes = FALSE,
    shape = 1,
    size = 3.0,
    stroke = 1.0,
    colour = colour_ink
  ) +
  facet_wrap(~model_label, nrow = 1) +
  scale_fill_gradient2(
    name = "OWA improvement (%)",
    low = colour_negative,
    mid = colour_neutral,
    high = colour_positive,
    midpoint = 0,
    na.value = "#E6E6E6"
  ) +
  labs(
    x = expression(lambda[Up]),
    y = expression(lambda[Down])
  ) +
  guides(
    fill = guide_colourbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = grid::unit(45, "mm"),
      barheight = grid::unit(3, "mm")
    )
  ) +
  theme_minimal(base_size = 10, base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 9, colour = colour_ink),
    legend.text = element_text(size = 8, colour = colour_ink),
    strip.text = element_text(face = "bold", colour = colour_ink),
    axis.title = element_text(colour = colour_ink),
    axis.text = element_text(colour = colour_ink)
  )

# Save -----------------------------------------------------------------------

out_pdf <- file.path(figure_dir, "02_heatmap_daily_two_panel.pdf")
out_png <- file.path(figure_dir, "02_heatmap_daily_two_panel.png")
out_svg <- file.path(figure_dir, "02_heatmap_daily_two_panel.svg")

save_vector_pdf <- function(filename, plot, width, height) {
  temporary_pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(temporary_pdf), add = TRUE)

  ggsave(
    temporary_pdf,
    plot,
    width = width,
    height = height,
    device = "pdf",
    bg = "white"
  )

  if (!file.copy(temporary_pdf, filename, overwrite = TRUE)) {
    stop("Could not replace PDF output: ", filename)
  }

  if (!file.exists(filename) || file.info(filename)$size <= 0) {
    stop("PDF output was not created correctly: ", filename)
  }
}

save_vector_pdf(out_pdf, p, width = 8, height = 4)
ggsave(out_png, p, width = 8, height = 4, dpi = 600, bg = "white")

if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(
    out_svg,
    p,
    width = 8,
    height = 4,
    device = svglite::svglite,
    bg = "white"
  )
}

cat("[07_5] Saved:", out_pdf, "\n")
cat("[07_5] Saved:", out_png, "\n")
if (file.exists(out_svg)) cat("[07_5] Saved:", out_svg, "\n")
