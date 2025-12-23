# =============================================================================
# Period Analysis Visualizations
# Creates publication-quality figures for period-based GLP-1 analysis
# =============================================================================

library(tidyverse)
library(patchwork)

# Auto-install patchwork if needed
if (!require(patchwork, quietly = TRUE)) {
  message("Installing patchwork package...")
  install.packages("patchwork")
  library(patchwork)
}

# Load results
load("period_analysis_results.RData")

cat("=============================================================================\n")
cat("PERIOD ANALYSIS VISUALIZATIONS\n")
cat("=============================================================================\n\n")

# =============================================================================
# Figure 1: Weight and Steps Trajectories Over Time Periods
# =============================================================================

cat("Creating Figure 1: Weight and Steps Trajectories...\n")

# Prepare data for plotting
period_order <- c("Baseline", "1-30d", "31-60d", "61-90d", "91-180d", "181-365d", "1-45d", "46-90d")

trajectory_data <- period_summary_table %>%
  mutate(period = factor(period, levels = period_order)) %>%
  arrange(period)

# Weight trajectory
p1 <- ggplot(trajectory_data, aes(x = period, y = mean_weight, group = 1)) +
  geom_line(linewidth = 1, color = "#2E86AB") +
  geom_point(size = 3, color = "#2E86AB") +
  geom_errorbar(aes(ymin = mean_weight - sd_weight,
                    ymax = mean_weight + sd_weight),
                width = 0.2, color = "#2E86AB", alpha = 0.6) +
  geom_text(aes(label = sprintf("n=%d", n_patients)),
            vjust = -1.5, size = 3, color = "gray40") +
  labs(title = "Weight Trajectory",
       x = "Period",
       y = "Weight (kg)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.minor = element_blank())

# Steps trajectory
p2 <- ggplot(trajectory_data, aes(x = period, y = mean_steps, group = 1)) +
  geom_line(linewidth = 1, color = "#A23B72") +
  geom_point(size = 3, color = "#A23B72") +
  geom_errorbar(aes(ymin = mean_steps - sd_steps,
                    ymax = mean_steps + sd_steps),
                width = 0.2, color = "#A23B72", alpha = 0.6) +
  geom_text(aes(label = sprintf("n=%d", n_patients)),
            vjust = -1.5, size = 3, color = "gray40") +
  labs(title = "Daily Steps Trajectory",
       x = "Period",
       y = "Steps per Day") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.minor = element_blank())

# Combine
fig1 <- p1 / p2 +
  plot_annotation(
    title = "Weight and Activity Trajectories Across Time Periods",
    subtitle = "Mean ± SD shown with error bars | Sample sizes shown above points",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 10, hjust = 0.5))
  )

ggsave("period_figure1_trajectories.png", fig1, width = 12, height = 10, dpi = 300)
cat("  Saved: period_figure1_trajectories.png\n\n")

# =============================================================================
# Figure 2: Activity Composition Over Time
# =============================================================================

cat("Creating Figure 2: Activity Composition...\n")

# Prepare long format for stacked plot
activity_composition <- trajectory_data %>%
  select(period, n_patients,
         mean_sedentary = mean_sedentary_min,
         mean_light = mean_light_min,
         mean_fairly = mean_fairly_min,
         mean_very = mean_very_min) %>%
  pivot_longer(cols = starts_with("mean_"),
               names_to = "activity_type",
               values_to = "minutes") %>%
  mutate(
    activity_type = factor(activity_type,
                          levels = c("mean_sedentary", "mean_light",
                                   "mean_fairly", "mean_very"),
                          labels = c("Sedentary", "Lightly Active",
                                   "Fairly Active", "Very Active"))
  )

# Stacked bar chart
p_stacked <- ggplot(activity_composition, aes(x = period, y = minutes, fill = activity_type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Sedentary" = "#E8E8E8",
                               "Lightly Active" = "#A8DADC",
                               "Fairly Active" = "#457B9D",
                               "Very Active" = "#1D3557")) +
  geom_text(data = trajectory_data,
            aes(x = period, y = 1400, label = sprintf("n=%d", n_patients)),
            inherit.aes = FALSE, size = 3, color = "black") +
  labs(title = "Activity Composition Across Time Periods",
       subtitle = "Daily minutes by activity intensity",
       x = "Period",
       y = "Minutes per Day",
       fill = "Activity Level") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

ggsave("period_figure2_activity_composition.png", p_stacked, width = 12, height = 8, dpi = 300)
cat("  Saved: period_figure2_activity_composition.png\n\n")

# =============================================================================
# Figure 3: Change from Baseline (4-panel)
# =============================================================================

cat("Creating Figure 3: Change from Baseline...\n")

# Calculate changes from baseline
baseline_values <- trajectory_data %>%
  filter(period == "Baseline") %>%
  select(baseline_weight = mean_weight,
         baseline_steps = mean_steps,
         baseline_sedentary = mean_sedentary_min,
         baseline_very = mean_very_min)

change_data <- trajectory_data %>%
  filter(period != "Baseline") %>%
  mutate(
    weight_change = mean_weight - baseline_values$baseline_weight,
    steps_change = mean_steps - baseline_values$baseline_steps,
    sedentary_change = mean_sedentary_min - baseline_values$baseline_sedentary,
    very_active_change = mean_very_min - baseline_values$baseline_very
  )

# Weight change
p_wt <- ggplot(change_data, aes(x = period, y = weight_change, group = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1, color = "#2E86AB") +
  geom_point(size = 3, color = "#2E86AB") +
  labs(title = "Weight Change",
       x = NULL,
       y = "Δ Weight (kg)") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

# Steps change
p_steps <- ggplot(change_data, aes(x = period, y = steps_change, group = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1, color = "#A23B72") +
  geom_point(size = 3, color = "#A23B72") +
  labs(title = "Steps Change",
       x = NULL,
       y = "Δ Steps/day") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

# Sedentary change
p_sed <- ggplot(change_data, aes(x = period, y = sedentary_change, group = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1, color = "#E63946") +
  geom_point(size = 3, color = "#E63946") +
  labs(title = "Sedentary Minutes Change",
       x = "Period",
       y = "Δ Sedentary (min/day)") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

# Very active change
p_very <- ggplot(change_data, aes(x = period, y = very_active_change, group = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1, color = "#06A77D") +
  geom_point(size = 3, color = "#06A77D") +
  labs(title = "Very Active Minutes Change",
       x = "Period",
       y = "Δ Very Active (min/day)") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

# Combine in 2x2 grid
fig3 <- (p_wt | p_steps) / (p_sed | p_very) +
  plot_annotation(
    title = "Change from Baseline Across Time Periods",
    subtitle = "Dashed line at zero indicates no change",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 10, hjust = 0.5))
  )

ggsave("period_figure3_change_from_baseline.png", fig3, width = 12, height = 10, dpi = 300)
cat("  Saved: period_figure3_change_from_baseline.png\n\n")

# =============================================================================
# Figure 4: Sample Size and Dropout Visualization
# =============================================================================

cat("Creating Figure 4: Sample Sizes and Dropout...\n")

# Sample size over time
p_n <- ggplot(trajectory_data, aes(x = period, y = n_patients, group = 1)) +
  geom_line(linewidth = 1.2, color = "#457B9D") +
  geom_point(size = 4, color = "#457B9D") +
  geom_text(aes(label = n_patients), vjust = -1, size = 4, fontface = "bold") +
  labs(title = "Sample Size Across Time Periods",
       subtitle = "Number of patients with data at each period",
       x = "Period",
       y = "Number of Patients") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.minor = element_blank()) +
  expand_limits(y = c(0, max(trajectory_data$n_patients) * 1.1))

# Calculate retention rate
retention_data <- trajectory_data %>%
  filter(period != "Baseline") %>%
  mutate(retention_pct = 100 * n_patients / first(trajectory_data$n_patients))

p_retention <- ggplot(retention_data, aes(x = period, y = retention_pct, group = 1)) +
  geom_line(linewidth = 1.2, color = "#E63946") +
  geom_point(size = 4, color = "#E63946") +
  geom_text(aes(label = sprintf("%.0f%%", retention_pct)),
            vjust = -1, size = 4, fontface = "bold") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray50", alpha = 0.7) +
  labs(title = "Retention Rate",
       subtitle = "Percentage of baseline cohort with data",
       x = "Period",
       y = "Retention (%)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.minor = element_blank()) +
  expand_limits(y = c(0, 105))

# Combine
fig4 <- p_n / p_retention +
  plot_annotation(
    title = "Data Availability and Patient Retention",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

ggsave("period_figure4_sample_sizes.png", fig4, width = 12, height = 10, dpi = 300)
cat("  Saved: period_figure4_sample_sizes.png\n\n")

# =============================================================================
# Figure 5: Percent Change Heatmap
# =============================================================================

cat("Creating Figure 5: Percent Change from Baseline...\n")

# Calculate percent changes
pct_change_data <- change_data %>%
  mutate(
    weight_pct = 100 * weight_change / baseline_values$baseline_weight,
    steps_pct = 100 * steps_change / baseline_values$baseline_steps,
    sedentary_pct = 100 * sedentary_change / baseline_values$baseline_sedentary,
    light_pct = 100 * (mean_light_min - first(trajectory_data$mean_light_min)) /
                      first(trajectory_data$mean_light_min),
    fairly_pct = 100 * (mean_fairly_min - first(trajectory_data$mean_fairly_min)) /
                       first(trajectory_data$mean_fairly_min),
    very_pct = 100 * very_active_change / baseline_values$baseline_very
  ) %>%
  select(period, weight_pct, steps_pct, sedentary_pct,
         light_pct, fairly_pct, very_pct) %>%
  pivot_longer(cols = -period, names_to = "metric", values_to = "pct_change") %>%
  mutate(
    metric = factor(metric,
                   levels = c("weight_pct", "steps_pct", "sedentary_pct",
                            "light_pct", "fairly_pct", "very_pct"),
                   labels = c("Weight", "Steps", "Sedentary",
                            "Light Active", "Fairly Active", "Very Active"))
  )

p_heatmap <- ggplot(pct_change_data, aes(x = period, y = metric, fill = pct_change)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.1f%%", pct_change)),
            color = "white", fontface = "bold", size = 4) +
  scale_fill_gradient2(low = "#2E86AB", mid = "white", high = "#E63946",
                       midpoint = 0, limits = c(-15, 15),
                       oob = scales::squish,
                       name = "% Change\nfrom Baseline") +
  labs(title = "Percent Change from Baseline - All Metrics",
       subtitle = "Blue = decrease | Red = increase",
       x = "Period",
       y = "Metric") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        legend.position = "right")

ggsave("period_figure5_percent_change.png", p_heatmap, width = 12, height = 8, dpi = 300)
cat("  Saved: period_figure5_percent_change.png\n\n")

# =============================================================================
# Summary
# =============================================================================

cat("=============================================================================\n")
cat("VISUALIZATION COMPLETE\n")
cat("=============================================================================\n\n")

cat("Generated figures:\n")
cat("1. period_figure1_trajectories.png - Weight and steps over time\n")
cat("2. period_figure2_activity_composition.png - Activity intensity breakdown\n")
cat("3. period_figure3_change_from_baseline.png - 4-panel change analysis\n")
cat("4. period_figure4_sample_sizes.png - Sample sizes and retention\n")
cat("5. period_figure5_percent_change.png - Percent change heatmap\n\n")

cat("All figures saved in current directory.\n")
cat("=============================================================================\n")
