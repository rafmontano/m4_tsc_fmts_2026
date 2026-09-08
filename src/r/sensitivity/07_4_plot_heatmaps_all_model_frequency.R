# =====================================================================
# 07_4_plot_heatmaps_all_model_frequency.R
# Plot all sensitivity heatmaps by frequency and adjusted model
# =====================================================================

source("src/r/sensitivity/00_sensitivity_common.R")

library(ggplot2)

# Paper-wide display standards
model_levels <- c("SMYL-Mantis", "Chronos-2-Mantis")
colour_negative <- "#D55E00"
colour_neutral <- "#FFFFFF"
colour_positive <- "#009E73"
colour_ink <- "#1A1A1A"

# ---------------------------------------------------------------------
# Input / output
# ---------------------------------------------------------------------

table_dir <- file.path("results", "sensitivity", "paper", "tables")
figure_dir <- file.path("results", "sensitivity", "paper", "figures")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

surface <- readRDS(file.path(table_dir, "sensitivity_surface_long.rds"))

expected_periods <- c(
  "Hourly", "Daily", "Weekly", "Monthly", "Quarterly", "Yearly"
)

missing_periods <- setdiff(
  expected_periods,
  unique(as.character(surface$period))
)

if (length(missing_periods) > 0) {
  stop(
    "Missing frequencies in sensitivity_surface_long.rds: ",
    paste(missing_periods, collapse = ", "),
    ". Run 07_3_build_surface_long_dataset.R before this script."
  )
}

surface <- surface |>
  dplyr::mutate(
    period = factor(
      period,
      levels = expected_periods
    ),
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

# ---------------------------------------------------------------------
# Plot markers
# ---------------------------------------------------------------------

best_points <- surface |>
  dplyr::group_by(period, model_label) |>
  dplyr::slice_max(improvement_pct, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()

no_adjustment_points <- surface |>
  dplyr::distinct(period, model_label) |>
  dplyr::mutate(
    lambda_up = 1,
    lambda_down = 1
  )


# ---------------------------------------------------------------------
# Plot
# ---------------------------------------------------------------------

p <- ggplot(
  surface,
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
    size = 2.3,
    stroke = 0.9,
    colour = colour_ink
  ) +
  geom_point(
    data = no_adjustment_points,
    aes(x = lambda_up, y = lambda_down),
    inherit.aes = FALSE,
    shape = 1,
    size = 2.1,
    stroke = 0.8,
    colour = colour_ink
  ) +
  facet_grid(
    rows = vars(model_label),
    cols = vars(period)
  ) +
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
    strip.text.x = element_text(face = "bold", colour = colour_ink),
    strip.text.y = element_text(
      face = "bold",
      angle = 0,
      size = 9,
      colour = colour_ink
    ),
    axis.title = element_text(colour = colour_ink),
    axis.text = element_text(colour = colour_ink)
  )

# ---------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------

out_pdf <- file.path(figure_dir, "01_heatmaps_all_model_frequency.pdf")
out_png <- file.path(figure_dir, "01_heatmaps_all_model_frequency.png")
out_svg <- file.path(figure_dir, "01_heatmaps_all_model_frequency.svg")

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

save_vector_pdf(out_pdf, p, width = 13, height = 5.5)
ggsave(out_png, p, width = 13, height = 5.5, dpi = 600, bg = "white")

if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(
    out_svg,
    p,
    width = 13,
    height = 5.5,
    device = svglite::svglite,
    bg = "white"
  )
}

cat("[07_4] Saved:", out_pdf, "\n")
cat("[07_4] Saved:", out_png, "\n")
if (file.exists(out_svg)) cat("[07_4] Saved:", out_svg, "\n")
