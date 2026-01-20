# =============================================================================
# QUICK PLOT: Step Distribution by BMI Class with Median Lines
# =============================================================================
# Run this after analysis1_enhanced.R to create density plot
# Assumes analysis1_data exists in memory
# Publication-ready figure matching template
# =============================================================================

library(tidyverse)

# Check if data exists
if (!exists("analysis1_data")) {
  stop("Error: analysis1_data not found. Please run analysis1_enhanced.R first.")
}

cat("Creating publication-ready step distribution plot with medians...\n")

# Calculate medians and N per class
class_stats <- analysis1_data %>%
  group_by(bmi_class) %>%
  summarize(
    median_steps = median(avg_steps, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    label_n = sprintf("%s (n=%s)", bmi_class, format(n, big.mark = ","))
  )

# Define distinct RGB colors (blue, green, orange, magenta, red)
bmi_colors <- c(
  "18.5-25" = "#0066CC",  # Blue
  "25-30"   = "#00AA00",  # Green
  "30-35"   = "#FF8800",  # Orange
  "35-40"   = "#CC0099",  # Magenta
  "≥40"     = "#DD0000"   # Red
)

# Create publication-ready density plot
p <- ggplot(analysis1_data, aes(x = avg_steps, fill = bmi_class, color = bmi_class)) +

  # Density curves scaled to percentage (0-100%)
  geom_density(aes(y = after_stat(scaled) * 100),
               alpha = 0.4, linewidth = 1.2) +

  # THICK median lines - very visible
  geom_vline(data = class_stats,
             aes(xintercept = median_steps, color = bmi_class),
             linetype = "dashed", linewidth = 1.5, alpha = 0.9,
             show.legend = FALSE) +

  # Median labels at top - black text, no boxes
  geom_text(data = class_stats,
            aes(x = median_steps, y = 105,
                label = format(round(median_steps), big.mark = ",")),
            color = "black", size = 4.5, fontface = "bold",
            angle = 0, vjust = 0, show.legend = FALSE) +

  # Color scales - distinct RGB colors
  scale_fill_manual(values = bmi_colors,
                    labels = class_stats$label_n,
                    name = "BMI Class (N)") +
  scale_color_manual(values = bmi_colors,
                     labels = class_stats$label_n,
                     name = "BMI Class (N)") +

  # X-axis formatting
  scale_x_continuous(
    limits = c(0, 20000),
    breaks = seq(0, 20000, 2500),
    labels = scales::comma,
    expand = c(0, 0)
  ) +

  # Y-axis as percentage (0-110% to fit labels)
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 110),
    breaks = seq(0, 100, 20),
    expand = c(0, 0)
  ) +

  # Labels
  labs(
    title = "Distribution of Daily Steps by BMI Class",
    subtitle = "Dashed lines show median steps per class (values labeled above). BMI < 18.5 excluded.",
    x = "Average Daily Steps",
    y = "Participants (%, scaled within BMI class)"
  ) +

  # Theme - publication ready
  theme_minimal(base_size = 14) +
  theme(
    # Text
    plot.title = element_text(size = 18, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, color = "gray30", hjust = 0),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "black"),

    # Legend - positioned in upper right
    legend.position = c(0.85, 0.65),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.key.size = unit(1, "cm"),

    # Grid
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),

    # Background
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 10, 10, 10)
  )

# Save high-resolution versions
ggsave("step_distribution_with_medians.png", p,
       width = 12, height = 7, dpi = 300, bg = "white")
ggsave("step_distribution_with_medians.pdf", p,
       width = 12, height = 7)

cat("✓ Saved: step_distribution_with_medians.png (300 DPI)\n")
cat("✓ Saved: step_distribution_with_medians.pdf (vector)\n\n")

# Print median values for reference
cat("Median steps by BMI class:\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
print(class_stats %>% select(bmi_class, n, median_steps), n = Inf)

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  PUBLICATION-READY FIGURE CREATED                            ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

cat("Key features:\n")
cat("  ✓ Distinct RGB colors (Blue, Green, Orange, Magenta, Red)\n")
cat("  ✓ Thick median lines (1.5pt dashed)\n")
cat("  ✓ Median values labeled at top (black text)\n")
cat("  ✓ Y-axis scaled to percentages within BMI class\n")
cat("  ✓ 300 DPI PNG + vector PDF for journals\n")
cat("  ✓ N per class shown in legend\n")
cat("  ✓ Professional theme with panel border\n\n")

cat("Done!\n")
