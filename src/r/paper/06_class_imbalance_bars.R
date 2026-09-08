# ==============================================================================
# 06_class_imbalance_bars.R
#
# Purpose:
#   Plot class distributions by M4 frequency.
# Inputs:
#   data/export/class_proportion.csv.
# Outputs:
#   A class-imbalance figure under results/paper/figures.
# Run from:
#   Project root.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(scales)
})

input_csv <- file.path("data", "export", "class_proportion.csv")
if (!file.exists(input_csv)) {
  message("Missing input file: ", input_csv, "\nRun 06a_compute_class_proportion.R first.")
}

df_counts <- readr::read_csv(input_csv, show_col_types = FALSE)

required_cols <- c("frequency", "class_label", "n")
missing_cols <- setdiff(required_cols, names(df_counts))
if (length(missing_cols) > 0) {
  message("Missing required columns in input CSV: ", paste(missing_cols, collapse = ", "))
}

# Frequency order (low -> high frequency after flip)
freq_levels <- c("Hourly", "Daily", "Weekly", "Monthly", "Quarterly", "Yearly")

# Prefer this class order if present; otherwise keep observed order
preferred_class_levels <- c("Up", "Neutral", "Down")

df_counts <- df_counts %>%
  mutate(
    frequency = as.character(frequency),
    class_label = as.character(class_label),
    class_label = case_when(
      tolower(class_label) == "up" ~ "Up",
      tolower(class_label) == "neutral" ~ "Neutral",
      tolower(class_label) == "down" ~ "Down",
      TRUE ~ class_label
    )
  )

observed_classes <- unique(df_counts$class_label)
class_levels <- c(
  preferred_class_levels[preferred_class_levels %in% observed_classes],
  setdiff(observed_classes, preferred_class_levels)
)

df_counts <- df_counts %>%
  tidyr::complete(
    frequency   = freq_levels,
    class_label = class_levels,
    fill        = list(n = 0)
  ) %>%
  mutate(
    frequency   = factor(frequency, levels = freq_levels),
    class_label = factor(class_label, levels = class_levels)
  )

df_props <- df_counts %>%
  group_by(frequency) %>%
  mutate(
    total_n = sum(n),
    prop    = ifelse(total_n > 0, n / total_n, 0),
    pct_lab = ifelse(prop >= 0.02, scales::percent(prop, accuracy = 1), "")
  ) %>%
  ungroup()

fig_dir <- file.path("results", "paper", "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
fig_path <- file.path(fig_dir, "class_imbalance_by_frequency_bar.pdf")

p <- ggplot(df_props, aes(x = frequency, y = prop, fill = class_label)) +
  geom_col(color = "grey20", linewidth = 0.2) +
  geom_text(
    aes(label = pct_lab),
    position = position_stack(vjust = 0.5),
    size = 3,
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_grey(start = 0.8, end = 0.2, name = "Class") +
  labs(x = "Frequency", y = "Class proportion", title = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(fig_path, p, width = 6.3, height = 3.8)
message("Class imbalance bar chart saved to: ", fig_path)
