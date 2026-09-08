# =====================================================================
# 07_7_plot_best_improvement_by_frequency.R
# Plot best directional-adjustment improvement by frequency
# =====================================================================

source("src/r/sensitivity/00_sensitivity_common.R")

library(ggplot2)

# ---------------------------------------------------------------------
# Input / output
# ---------------------------------------------------------------------

table_dir <- file.path("results", "sensitivity", "paper", "tables")
figure_dir <- file.path("results", "sensitivity", "paper", "figures")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

surface <- readRDS(file.path(table_dir, "sensitivity_surface_long.rds"))

# ---------------------------------------------------------------------
# Best improvement per frequency and adjusted model
# ---------------------------------------------------------------------

best_by_frequency <- surface |>
  dplyr::group_by(period, model_label) |>
  dplyr::slice_max(improvement_pct, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    period = factor(
      period,
      levels = c("Hourly", "Daily", "Weekly", "Monthly", "Quarterly", "Yearly")
    ),
    model_label = factor(
      model_label,
      levels = c("SMYL-MANTIS", "Chronos-MANTIS")
    )
  ) |>
  dplyr::arrange(period, model_label)

# ---------------------------------------------------------------------
# Save plotting dataset
# ---------------------------------------------------------------------

out_csv <- file.path(table_dir, "sensitivity_best_by_frequency.csv")
out_rds <- file.path(table_dir, "sensitivity_best_by_frequency.rds")

readr::write_csv(best_by_frequency, out_csv)
saveRDS(best_by_frequency, out_rds)

# ---------------------------------------------------------------------
# Plot
# ---------------------------------------------------------------------

p <- ggplot(
  best_by_frequency,
  aes(
    x = period,
    y = improvement_pct,
    fill = model_label
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  labs(
    title = "Best directional-adjustment improvement by frequency",
    subtitle = "Best OWA improvement over each unadjusted base forecast across the sensitivity surface.",
    x = NULL,
    y = "Best OWA improvement (%)",
    fill = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# ---------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------

out_pdf <- file.path(figure_dir, "04_best_improvement_by_frequency.pdf")
out_png <- file.path(figure_dir, "04_best_improvement_by_frequency.png")

ggsave(out_pdf, p, width = 8, height = 4)
ggsave(out_png, p, width = 8, height = 4, dpi = 300)

cat("[07_7] Saved:", out_csv, "\n")
cat("[07_7] Saved:", out_rds, "\n")
cat("[07_7] Saved:", out_pdf, "\n")
cat("[07_7] Saved:", out_png, "\n")