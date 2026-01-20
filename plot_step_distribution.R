# =============================================================================
# QUICK PLOT: Step Distribution by BMI Class with Median Lines
# =============================================================================
# Run this after analysis1_enhanced.R to create density plot
# Assumes analysis1_data exists in memory
# =============================================================================

library(tidyverse)

# Check if data exists
if (!exists("analysis1_data")) {
  stop("Error: analysis1_data not found. Please run analysis1_enhanced.R first.")
}

cat("Creating step distribution plot with medians...\n")

# Calculate medians and N per class
class_stats <- analysis1_data %>%
  group_by(bmi_class) %>%
  summarize(
    median_steps = median(avg_steps, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    label = sprintf("%s (n=%s)", bmi_class, format(n, big.mark = ",")),
    median_label = sprintf("%.0f", median_steps)
  )

# Define distinct RGB colors for each BMI class
bmi_colors <- c(
  "18.5-25" = "#0066CC",  # Blue
  "25-30"   = "#00AA00",  # Green
  "30-35"   = "#FF8800",  # Orange
  "35-40"   = "#CC0099",  # Magenta
  "≥40"     = "#DD0000"   # Red
)

# Create median labels with white background boxes for readability
class_stats <- class_stats %>%
  mutate(
    label_x = median_steps,
    label_y = 102  # Just above 100% line
  )

# Create overlapping density plot with median lines (scaled to percentage)
p_density <- ggplot(analysis1_data, aes(x = avg_steps, fill = bmi_class, color = bmi_class)) +
  # Density curves (scaled to 0-100%)
  geom_density(aes(y = after_stat(scaled) * 100), alpha = 0.3, linewidth = 1.2) +

  # Vertical median lines for each BMI class
  geom_vline(data = class_stats,
             aes(xintercept = median_steps, color = bmi_class),
             linetype = "dashed", linewidth = 1, show.legend = FALSE) +

  # White background boxes for median labels
  geom_label(data = class_stats,
             aes(x = median_steps, y = label_y, label = median_label,
                 color = bmi_class, fill = bmi_class),
             fontface = "bold", size = 4,
             label.padding = unit(0.3, "lines"),
             label.size = 0.5,
             alpha = 0.9,
             show.legend = FALSE) +

  # Color scales - distinct RGB colors
  scale_fill_manual(values = bmi_colors,
                    labels = class_stats$label,
                    name = "BMI Class (N)") +
  scale_color_manual(values = bmi_colors, guide = "none") +

  # Axis formatting
  scale_x_continuous(labels = scales::comma,
                     limits = c(0, 20000),
                     breaks = seq(0, 20000, 2500)) +
  scale_y_continuous(limits = c(0, 110),  # Extended to 110 for labels above
                     breaks = seq(0, 100, 20),
                     labels = function(x) paste0(x, "%")) +

  # Labels
  labs(
    title = "Distribution of Daily Steps by BMI Class",
    subtitle = "Dashed lines show median steps per class (values labeled at top). BMI < 18.5 excluded.",
    x = "Average Daily Steps",
    y = "Participants (%, scaled within BMI class)"
  ) +

  # Theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    plot.margin = margin(10, 10, 10, 10)
  )

# Save plot
ggsave("step_distribution_with_medians.png", p_density,
       width = 12, height = 7, dpi = 300, bg = "white")
ggsave("step_distribution_with_medians.pdf", p_density,
       width = 12, height = 7)

cat("✓ Saved: step_distribution_with_medians.png/pdf\n")

# Print median values for reference
cat("\nMedian steps by BMI class:\n")
print(class_stats %>% select(bmi_class, n, median_steps))

cat("\nDone!\n")
