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

# Create overlapping density plot with median lines
p_density <- ggplot(analysis1_data, aes(x = avg_steps, fill = bmi_class, color = bmi_class)) +
  # Density curves
  geom_density(alpha = 0.3, linewidth = 1) +

  # Vertical median lines for each BMI class
  geom_vline(data = class_stats,
             aes(xintercept = median_steps, color = bmi_class),
             linetype = "dashed", linewidth = 0.8, show.legend = FALSE) +

  # Median value annotations at top of plot
  geom_text(data = class_stats,
            aes(x = median_steps, label = median_label, color = bmi_class),
            y = Inf, vjust = 1.5, hjust = 0.5, size = 3.5, fontface = "bold",
            show.legend = FALSE) +

  # Color scales
  scale_fill_brewer(palette = "RdYlBu", direction = -1,
                    labels = class_stats$label,
                    name = "BMI Class (N)") +
  scale_color_brewer(palette = "RdYlBu", direction = -1, guide = "none") +

  # Axis formatting
  scale_x_continuous(labels = scales::comma,
                     limits = c(0, 20000),
                     breaks = seq(0, 20000, 2500)) +
  scale_y_continuous(labels = scales::scientific) +

  # Labels
  labs(
    title = "Distribution of Daily Steps by BMI Class",
    subtitle = "Dashed lines show median steps per class. BMI < 18.5 excluded.",
    x = "Average Daily Steps",
    y = "Density"
  ) +

  # Theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30")
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
