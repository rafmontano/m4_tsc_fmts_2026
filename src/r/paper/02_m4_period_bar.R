# ==============================================================================
# 02_m4_period_bar.R
#
# Purpose:
#   Plot the number of M4 series in each frequency.
# Inputs:
#   The M4 dataset from M4comp2018.
# Outputs:
#   PDF, PNG, and SVG figures under results/paper/figures.
# Run from:
#   Project root.
# ==============================================================================

suppressPackageStartupMessages({
  library(M4comp2018)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(scales)
})

# Paper-wide display standards
colour_bar <- "#0072B2"
colour_ink <- "#1A1A1A"
colour_lightgray <- "#D9D9D9"

# Output directory
fig_dir <- file.path("results", "paper", "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

out_pdf <- file.path(fig_dir, "m4_period_bar.pdf")
out_png <- file.path(fig_dir, "m4_period_bar.png")
out_svg <- file.path(fig_dir, "m4_period_bar.svg")

# Load M4 metadata
data("M4")
m4_meta <- purrr::map_dfr(M4, function(s) tibble(period = s$period))

# Desired order (low -> high frequency)
period_levels <- c("Hourly", "Daily", "Weekly", "Monthly", "Quarterly", "Yearly")

# Validate periods (helps catch subset / unexpected labels)
observed_periods <- sort(unique(m4_meta$period))
missing_expected <- setdiff(period_levels, observed_periods)
unexpected <- setdiff(observed_periods, period_levels)

if (length(missing_expected) > 0) {
  warning("Missing expected periods: ", paste(missing_expected, collapse = ", "))
}
if (length(unexpected) > 0) {
  warning("Unexpected periods found: ", paste(unexpected, collapse = ", "))
  period_levels <- c(period_levels, unexpected)
}

# Counts & percentages
period_tbl <- m4_meta %>%
  count(period, name = "n_series") %>%
  mutate(
    total_series = sum(n_series),
    pct          = n_series / total_series,
    n_fmt        = format(n_series, big.mark = ",", trim = TRUE),
    pct_label    = sprintf("%.1f%%", 100 * pct),
    label_text   = paste0(n_fmt, " (", pct_label, ")"),
    period       = factor(period, levels = period_levels)
  ) %>%
  arrange(period)

# Plot
p <- ggplot(period_tbl, aes(x = period, y = n_series)) +
  geom_col(width = 0.64, fill = colour_bar) +
  coord_flip() +
  geom_text(
    aes(label = label_text),
    hjust = -0.08,
    size = 3.4,
    family = "sans",
    colour = colour_ink
  ) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ",", accuracy = 1),
    breaks = c(0, 20000, 40000),
    expand = expansion(mult = c(0, 0.34))
  ) +
  labs(x = "Frequency", y = "Number of series") +
  theme_minimal(base_size = 12, base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.title = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      colour = colour_lightgray,
      linewidth = 0.35
    ),
    axis.title = element_text(colour = colour_ink),
    axis.title.y = element_text(margin = margin(r = 6)),
    axis.title.x = element_text(margin = margin(t = 5)),
    axis.text = element_text(size = 10, colour = colour_ink),
    plot.margin = margin(5.5, 12, 5.5, 5.5)
  )

# Save
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

save_vector_pdf(out_pdf, p, width = 5.0, height = 3.0)
ggsave(out_png, p, width = 5.0, height = 3.0, dpi = 600, bg = "white")

if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(
    out_svg,
    p,
    width = 5.0,
    height = 3.0,
    device = svglite::svglite,
    bg = "white"
  )
}

message("Saved: ", out_pdf)
message("Saved: ", out_png)
if (file.exists(out_svg)) message("Saved: ", out_svg)
