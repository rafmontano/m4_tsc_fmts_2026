# ==============================================================================
# 06b_class_imbalance_bars_binary.R
#
# Purpose:
#   Compute and plot binary class balance by M4 frequency and horizon.
# Inputs:
#   The M4 dataset from M4comp2018.
# Outputs:
#   Binary class-proportion tables and figures.
# Run from:
#   Project root.
# ==============================================================================

suppressPackageStartupMessages({
  library(M4comp2018)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(scales)
})

# Configuration --------------------------------------------------------------

export_dir <- file.path("data", "export")
fig_dir <- file.path("results", "paper", "figures")

# Paper-wide display standards
colour_increase <- "#009E73"
colour_nonincrease <- "#D55E00"
colour_ink <- "#1A1A1A"
colour_lightgray <- "#D9D9D9"

dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

output_csv_freq <- file.path(export_dir, "class_proportion_binary.csv")
output_csv_horizon <- file.path(export_dir, "class_proportion_binary_horizon.csv")

out_pdf <- file.path(fig_dir, "m4_class_imbalance_by_frequency_bar_binary.pdf")
out_png <- file.path(fig_dir, "m4_class_imbalance_by_frequency_bar_binary.png")
out_svg <- file.path(fig_dir, "m4_class_imbalance_by_frequency_bar_binary.svg")

freq_levels <- c("Hourly", "Daily", "Weekly", "Monthly", "Quarterly", "Yearly")
class_levels <- c("Up", "Down")

# Load M4 data ---------------------------------------------------------------

data(M4)

# Build binary directional labels --------------------------------------------

labels <- bind_rows(lapply(seq_along(M4), function(i) {
  s <- M4[[i]]

  frequency <- s$period
  x <- as.numeric(s$x)
  xx <- as.numeric(s$xx)

  if (length(x) == 0 || length(xx) == 0) {
    return(tibble())
  }

  y_T <- tail(x, 1)

  tibble(
    series_id   = i,
    frequency   = frequency,
    horizon     = seq_along(xx),
    y_T         = y_T,
    y_T_h       = xx,
    class_label = if_else(xx > y_T, "Up", "Down")
  )
}))

# Frequency-level class counts -----------------------------------------------

df_counts_freq <- labels %>%
  count(frequency, class_label, name = "n") %>%
  complete(
    frequency = freq_levels,
    class_label = class_levels,
    fill = list(n = 0)
  ) %>%
  mutate(
    frequency = factor(frequency, levels = freq_levels),
    class_label = factor(class_label, levels = class_levels)
  ) %>%
  arrange(frequency, class_label)

readr::write_csv(df_counts_freq, output_csv_freq)

# Horizon-level class counts -------------------------------------------------

df_counts_horizon <- labels %>%
  count(frequency, horizon, class_label, name = "n") %>%
  complete(
    frequency = freq_levels,
    horizon = sort(unique(labels$horizon)),
    class_label = class_levels,
    fill = list(n = 0)
  ) %>%
  mutate(
    frequency = factor(frequency, levels = freq_levels),
    class_label = factor(class_label, levels = class_levels)
  ) %>%
  arrange(frequency, horizon, class_label)

readr::write_csv(df_counts_horizon, output_csv_horizon)

# Release the instance-level data before rendering the publication figure.
rm(labels)
invisible(gc())

# Plot frequency-level class proportions -------------------------------------

df_props <- df_counts_freq %>%
  group_by(frequency) %>%
  mutate(
    total_n = sum(n),
    prop = if_else(total_n > 0, n / total_n, 0),
    pct_lab = if_else(prop >= 0.02, scales::percent(prop, accuracy = 1), ""),
    class_display = dplyr::recode(
      as.character(class_label),
      "Up" = "Increase",
      "Down" = "Non-increase"
    ),
    class_display = factor(
      class_display,
      levels = c("Increase", "Non-increase")
    )
  ) %>%
  ungroup()

p <- ggplot(df_props, aes(x = frequency, y = prop, fill = class_display)) +
  geom_col(width = 0.64, colour = "white", linewidth = 0.4) +
  geom_text(
    aes(label = pct_lab),
    position = position_stack(vjust = 0.5),
    size = 3.4,
    family = "sans",
    colour = "white",
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "Increase" = colour_increase,
      "Non-increase" = colour_nonincrease
    ),
    breaks = c("Increase", "Non-increase"),
    name = NULL
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = c(0, 0.25, 0.50, 0.75, 1),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(x = "Frequency", y = "Class proportion") +
  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(colour = NA)
    )
  ) +
  theme_minimal(base_size = 12, base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      colour = colour_lightgray,
      linewidth = 0.35
    ),
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.text = element_text(size = 10, colour = colour_ink),
    legend.margin = margin(0, 0, 2, 0),
    legend.box.spacing = grid::unit(1, "pt"),
    axis.title = element_text(colour = colour_ink),
    axis.title.y = element_text(margin = margin(r = 6)),
    axis.title.x = element_text(margin = margin(t = 5)),
    axis.text = element_text(size = 10, colour = colour_ink),
    plot.margin = margin(5.5, 12, 5.5, 5.5)
  )

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

message("Wrote: ", output_csv_freq)
message("Wrote: ", output_csv_horizon)
message("Saved: ", out_pdf)
message("Saved: ", out_png)
if (file.exists(out_svg)) message("Saved: ", out_svg)
