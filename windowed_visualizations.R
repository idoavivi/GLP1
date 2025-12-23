# =============================================================================
# Windowed Analysis Visualizations
# Creates publication-quality figures for windowed GLP-1 analysis
# =============================================================================

library(tidyverse)
library(ggplot2)
library(scales)

# Optional packages - install if needed
if (!require(patchwork, quietly = TRUE)) {
  message("Installing patchwork package...")
  install.packages("patchwork")
  library(patchwork)
}

# Load windowed analysis results
load("windowed_analysis_results.RData")

cat("Creating visualizations for windowed analysis...\n\n")

# =============================================================================
# FIGURE 1: Weight and Activity Trajectories Over Time
# =============================================================================

# Prepare data for trajectory plot
trajectory_data <- windowed_analysis_results$combined_summary %>%
  mutate(
    timepoint_num = case_when(
      timepoint == "Baseline" ~ 0,
      str_detect(timepoint, "Day") ~ as.numeric(str_extract(timepoint, "\\d+")),
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(timepoint_num)) %>%
  arrange(timepoint_num)

# Weight trajectory
p1 <- ggplot(trajectory_data, aes(x = timepoint_num, y = mean_weight)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(aes(ymin = mean_weight - sd_weight,
                    ymax = mean_weight + sd_weight),
                width = 10, color = "steelblue", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "Weight Trajectory",
    x = "Days from GLP-1 Initiation",
    y = "Weight (kg, Mean ± SD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  )

# Steps trajectory
p2 <- ggplot(trajectory_data, aes(x = timepoint_num, y = mean_steps)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(size = 3, color = "darkgreen") +
  geom_errorbar(aes(ymin = mean_steps - sd_steps,
                    ymax = mean_steps + sd_steps),
                width = 10, color = "darkgreen", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "Daily Steps Trajectory",
    x = "Days from GLP-1 Initiation",
    y = "Steps (Mean ± SD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  )

# Combine plots
fig1 <- p1 / p2 +
  plot_annotation(
    title = "Weight and Activity Trajectories After GLP-1 Initiation",
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )

print(fig1)
ggsave("figure_trajectories.png", fig1, width = 10, height = 8, dpi = 300)

# =============================================================================
# FIGURE 2: Activity Composition at Each Timepoint
# =============================================================================

# Prepare stacked activity data
activity_composition <- trajectory_data %>%
  select(timepoint, timepoint_num,
         Sedentary = mean_sedentary_min,
         `Lightly Active` = mean_lightly_active_min,
         `Fairly Active` = mean_fairly_active_min,
         `Very Active` = mean_very_active_min) %>%
  pivot_longer(cols = c(Sedentary, `Lightly Active`, `Fairly Active`, `Very Active`),
               names_to = "activity_type",
               values_to = "minutes") %>%
  mutate(
    activity_type = factor(activity_type,
                          levels = c("Very Active", "Fairly Active",
                                    "Lightly Active", "Sedentary"))
  )

p3 <- ggplot(activity_composition, aes(x = timepoint_num, y = minutes, fill = activity_type)) +
  geom_area(alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  scale_fill_manual(
    values = c("Very Active" = "#2E7D32",
               "Fairly Active" = "#66BB6A",
               "Lightly Active" = "#AED581",
               "Sedentary" = "#BDBDBD")
  ) +
  labs(
    title = "Activity Composition Over Time",
    x = "Days from GLP-1 Initiation",
    y = "Minutes per Day",
    fill = "Activity Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p3)
ggsave("figure_activity_composition.png", p3, width = 10, height = 6, dpi = 300)

# =============================================================================
# FIGURE 3: Activity Calories Over Time
# =============================================================================

p4 <- ggplot(trajectory_data, aes(x = timepoint_num, y = mean_activity_calories)) +
  geom_line(color = "darkorange", size = 1.2) +
  geom_point(size = 3, color = "darkorange") +
  geom_errorbar(aes(ymin = mean_activity_calories - sd_activity_calories,
                    ymax = mean_activity_calories + sd_activity_calories),
                width = 10, color = "darkorange", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "Activity Calories Over Time",
    x = "Days from GLP-1 Initiation",
    y = "Activity Calories (Mean ± SD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  )

print(p4)
ggsave("figure_activity_calories.png", p4, width = 10, height = 6, dpi = 300)

# =============================================================================
# FIGURE 4: Change from Baseline for All Metrics
# =============================================================================

# Calculate changes from baseline
baseline_values <- trajectory_data %>%
  filter(timepoint_num == 0) %>%
  select(baseline_weight = mean_weight,
         baseline_steps = mean_steps,
         baseline_sedentary = mean_sedentary_min,
         baseline_very_active = mean_very_active_min,
         baseline_activity_cal = mean_activity_calories)

change_data <- trajectory_data %>%
  filter(timepoint_num > 0) %>%
  mutate(
    weight_change = mean_weight - baseline_values$baseline_weight,
    steps_change = mean_steps - baseline_values$baseline_steps,
    sedentary_change = mean_sedentary_min - baseline_values$baseline_sedentary,
    very_active_change = mean_very_active_min - baseline_values$baseline_very_active,
    activity_cal_change = mean_activity_calories - baseline_values$baseline_activity_cal,
    # Percent changes
    weight_pct = 100 * weight_change / baseline_values$baseline_weight,
    steps_pct = 100 * steps_change / baseline_values$baseline_steps,
    sedentary_pct = 100 * sedentary_change / baseline_values$baseline_sedentary,
    very_active_pct = 100 * very_active_change / baseline_values$baseline_very_active,
    activity_cal_pct = 100 * activity_cal_change / baseline_values$baseline_activity_cal
  )

# Panel A: Weight change
p5a <- ggplot(change_data, aes(x = timepoint_num, y = weight_change)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray50") +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(size = 3, color = "steelblue") +
  labs(
    title = "A. Weight Change",
    x = NULL,
    y = "Change (kg)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11))

# Panel B: Steps change
p5b <- ggplot(change_data, aes(x = timepoint_num, y = steps_change)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray50") +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(size = 3, color = "darkgreen") +
  labs(
    title = "B. Steps Change",
    x = NULL,
    y = "Change (steps/day)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11))

# Panel C: Sedentary change
p5c <- ggplot(change_data, aes(x = timepoint_num, y = sedentary_change)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray50") +
  geom_line(color = "gray40", size = 1.2) +
  geom_point(size = 3, color = "gray40") +
  labs(
    title = "C. Sedentary Minutes Change",
    x = "Days from GLP-1 Initiation",
    y = "Change (min/day)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11))

# Panel D: Very active change
p5d <- ggplot(change_data, aes(x = timepoint_num, y = very_active_change)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray50") +
  geom_line(color = "#2E7D32", size = 1.2) +
  geom_point(size = 3, color = "#2E7D32") +
  labs(
    title = "D. Very Active Minutes Change",
    x = "Days from GLP-1 Initiation",
    y = "Change (min/day)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11))

# Combine change panels
fig4 <- (p5a | p5b) / (p5c | p5d) +
  plot_annotation(
    title = "Change from Baseline Across Follow-up Timepoints",
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )

print(fig4)
ggsave("figure_change_from_baseline.png", fig4, width = 12, height = 10, dpi = 300)

# =============================================================================
# FIGURE 5: Baseline Window Comparison
# =============================================================================

if (!is.null(windowed_analysis_results$baseline_winners)) {
  baseline_comparison <- windowed_analysis_results$baseline_winners %>%
    mutate(
      min_days_label = paste0(min_days, " days"),
      window_label = paste0(window_name, "\n(", min_days, "d min)")
    )

  p6 <- ggplot(baseline_comparison, aes(x = window_label, y = mean_steps)) +
    geom_col(aes(fill = factor(min_days)), alpha = 0.7) +
    geom_text(aes(label = sprintf("N=%d", n_patients)),
              vjust = -0.5, size = 3.5) +
    scale_fill_brewer(palette = "Set2", name = "Min Days") +
    labs(
      title = "Baseline Window Comparison",
      subtitle = "Recommended windows by minimum Fitbit days requirement",
      x = "Baseline Window",
      y = "Mean Daily Steps"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "bottom"
    )

  print(p6)
  ggsave("figure_baseline_windows.png", p6, width = 10, height = 6, dpi = 300)
}

# =============================================================================
# FIGURE 6: Nadir Analysis (if available)
# =============================================================================

if (!is.null(windowed_analysis_results$nadir_individual)) {
  nadir_data <- windowed_analysis_results$nadir_individual

  # Distribution of days to nadir
  p7a <- ggplot(nadir_data, aes(x = days_from_initiation)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "black") +
    geom_vline(aes(xintercept = mean(days_from_initiation)),
               linetype = "dashed", color = "red", size = 1) +
    labs(
      title = "Distribution of Time to Nadir Weight",
      x = "Days from GLP-1 Initiation to Nadir",
      y = "Number of Patients"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 12))

  # Nadir weight vs steps
  p7b <- ggplot(nadir_data, aes(x = nadir_weight, y = mean_steps)) +
    geom_point(alpha = 0.6, size = 3, color = "darkgreen") +
    geom_smooth(method = "lm", color = "red", fill = "pink") +
    labs(
      title = "Activity at Nadir Weight",
      x = "Nadir Weight (kg)",
      y = "Mean Daily Steps"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 12))

  fig6 <- p7a / p7b +
    plot_annotation(
      title = "Nadir Weight Analysis",
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )

  print(fig6)
  ggsave("figure_nadir_analysis.png", fig6, width = 10, height = 8, dpi = 300)
}

# =============================================================================
# FIGURE 7: Sample Size Flow Chart Data
# =============================================================================

# Create sample size table
if (!is.null(windowed_analysis_results$followup_summary)) {
  sample_sizes <- windowed_analysis_results$followup_summary %>%
    select(timepoint_days, n_patients) %>%
    arrange(timepoint_days)

  # Add baseline
  baseline_n <- windowed_analysis_results$recommended_baseline$n_patients
  sample_sizes <- bind_rows(
    tibble(timepoint_days = 0, n_patients = baseline_n),
    sample_sizes
  )

  # Add nadir if available
  if (!is.null(windowed_analysis_results$nadir_summary)) {
    nadir_n <- windowed_analysis_results$nadir_summary$n_patients
    max_day <- max(sample_sizes$timepoint_days)
    sample_sizes <- bind_rows(
      sample_sizes,
      tibble(timepoint_days = max_day + 30, n_patients = nadir_n)
    )
  }

  p8 <- ggplot(sample_sizes, aes(x = factor(timepoint_days), y = n_patients)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    geom_text(aes(label = n_patients), vjust = -0.5, size = 4, fontface = "bold") +
    labs(
      title = "Sample Size at Each Timepoint",
      subtitle = "Number of patients meeting eligibility criteria",
      x = "Days from GLP-1 Initiation",
      y = "Number of Patients"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  print(p8)
  ggsave("figure_sample_sizes.png", p8, width = 10, height = 6, dpi = 300)
}

# =============================================================================
# FIGURE 8: Percent Change from Baseline
# =============================================================================

# Prepare percent change data
pct_change_long <- change_data %>%
  select(timepoint_num,
         Weight = weight_pct,
         Steps = steps_pct,
         `Sedentary Min` = sedentary_pct,
         `Very Active Min` = very_active_pct,
         `Activity Cal` = activity_cal_pct) %>%
  pivot_longer(cols = -timepoint_num,
               names_to = "metric",
               values_to = "pct_change")

p9 <- ggplot(pct_change_long, aes(x = timepoint_num, y = pct_change, color = metric)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray50") +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set1", name = "Metric") +
  labs(
    title = "Percent Change from Baseline",
    x = "Days from GLP-1 Initiation",
    y = "% Change from Baseline"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p9)
ggsave("figure_percent_change.png", p9, width = 10, height = 6, dpi = 300)

# =============================================================================
# SAVE SUMMARY
# =============================================================================

cat("\n=== Visualizations Created ===\n\n")
cat("Figures saved:\n")
cat("  1. figure_trajectories.png - Weight and steps over time\n")
cat("  2. figure_activity_composition.png - Stacked area chart of activity levels\n")
cat("  3. figure_activity_calories.png - Activity calories trajectory\n")
cat("  4. figure_change_from_baseline.png - 4-panel change analysis\n")
cat("  5. figure_baseline_windows.png - Baseline window comparison\n")
if (!is.null(windowed_analysis_results$nadir_individual)) {
  cat("  6. figure_nadir_analysis.png - Nadir weight distribution and activity\n")
}
cat("  7. figure_sample_sizes.png - Sample size at each timepoint\n")
cat("  8. figure_percent_change.png - Percent change for all metrics\n")

cat("\nAll visualizations complete!\n")
